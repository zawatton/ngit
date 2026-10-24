#!/usr/bin/env python3
"""Compare literal nested history paths with native Git, including read state."""
import argparse
import base64
import hashlib
import json
import os
from pathlib import Path
import shlex
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[1]
ENV = dict(os.environ, LC_ALL="C", TZ="UTC", GIT_OPTIONAL_LOCKS="0",
           GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL=os.devnull,
           GIT_AUTHOR_NAME="Nested History", GIT_AUTHOR_EMAIL="history@example.test",
           GIT_COMMITTER_NAME="Nested History", GIT_COMMITTER_EMAIL="history@example.test",
           GIT_AUTHOR_DATE="2001-02-03T04:05:06+0000",
           GIT_COMMITTER_DATE="2001-02-03T04:05:06+0000")


def run(command, cwd):
    result = subprocess.run(command, cwd=cwd, env=ENV, capture_output=True, timeout=35)
    return result.returncode, result.stdout, result.stderr


def git(repo, *args):
    result = run(["git", *args], repo)
    if result[0]:
        raise RuntimeError(f"fixture Git failed: {args!r}: {result!r}")
    return result


def fixture(repo):
    repo.mkdir()
    git(repo, "init", "-q", "-b", "main")
    paths = {"file.txt": "root\n", "sibling.txt": "sibling\n",
             "space dir/file.txt": "nested\n", "space dir/space name.txt": "space\n",
             "space dir/日本語.txt": "unicode\n", "space dir/深い/deep.txt": "deep\n"}
    for name, content in paths.items():
        path = repo / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
    git(repo, "add", ".")
    git(repo, "commit", "-qm", "initial")
    for name, content, message in [("space dir/file.txt", "nested two\n", "nested change"),
                                   ("sibling.txt", "sibling two\n", "sibling change"),
                                   ("space dir/日本語.txt", "unicode two\n", "unicode change")]:
        (repo / name).write_text(content)
        git(repo, "add", name)
        git(repo, "commit", "-qm", message)
    (repo / "space dir/colon:name").write_text("colon path\n")
    (repo / "space dir/HEAD:missing").write_text("missing tree path filename\n")
    # A valid revision:path and existing filename require disambiguation.
    collision = repo / "space dir/HEAD:space dir/file.txt"
    collision.parent.mkdir(parents=True)
    collision.write_text("filename shadow\n")
    git(repo, "add", "space dir/colon:name")
    git(repo, "add", "space dir/HEAD:missing")
    git(repo, "add", "space dir/HEAD:space dir/file.txt")
    oid = git(repo, "rev-parse", "HEAD")[1].decode().strip()
    git(repo, "update-index", "--add", "--cacheinfo", "160000", oid, "space dir/module")
    git(repo, "commit", "-qm", "gitlink and colon path")


def state(repo):
    # Hash all repository files, including index, refs, objects and worktree.
    files = {}
    for path in sorted(repo.rglob("*")):
        if path.is_file():
            files[path.relative_to(repo).as_posix()] = [path.stat().st_mode,
                                                      hashlib.sha256(path.read_bytes()).hexdigest()]
    return {"files": files, "fsck": packed(run(["git", "fsck", "--full"], repo))}


def packed(result):
    return {"exit": result[0], "stdout": base64.b64encode(result[1]).decode(),
            "stderr": base64.b64encode(result[2]).decode()}


def cases(mode, repo):
    sub = repo / "space dir"
    deep = sub / "深い"
    if mode == "selection":
        for command in ("log", "show"):
            options = ["--format=%s"] if command == "log" else ["--format=%s", "--name-only"]
            for path in ("file.txt", "./file.txt", "../space dir/file.txt", str(sub / "file.txt"),
                         "space name.txt", "日本語.txt", "../sibling.txt", "深い/", "file.txt/", "module/", ".", "../"):
                if command == "show" and path == "module/":
                    yield sub, ["show", "--format=", "--name-only", "HEAD", "--", path]
                else:
                    yield sub, [command, *options, "--", path]
        yield sub, ["log", "--format=%s", "--", "*.txt"]
        yield repo, ["log", "--format=%s", "--", "space dir/*.txt"]
        yield repo, ["log", "--format=%s", "--", ":/space dir/file.txt"]
        yield sub, ["log", "--format=%s", "--", "module/*"]
    elif mode == "boundaries":
        for command in ("log", "show"):
            options = ["--format=%s"] if command == "log" else ["--format=%s", "--name-only"]
            yield sub, [command, *options, "file.txt"]
            yield sub, [command, *options, "HEAD~1", "file.txt"]
            yield sub, [command, *options, "HEAD~2..HEAD", "--", "file.txt"]
            yield sub, [command, *options, "file.txt", "HEAD"]
            yield sub, [command, *options, "--", "HEAD"]
            yield sub, [command, *options, "HEAD", "--", "missing.txt"]
        for name in ("HEAD:./file.txt", "HEAD:../sibling.txt", "HEAD:./日本語.txt",
                     "HEAD:space dir/file.txt", "HEAD:./深い/"):
            yield sub, ["show", name]
        yield deep, ["show", "HEAD:../file.txt"]
        yield sub, ["show", "--format=", "--name-only", "colon:name"]
        yield sub, ["show", "--format=", "--name-only", "HEAD:missing"]
        yield sub, ["show", "--format=", "--name-only", "--", "colon:name"]
    elif mode == "e2e":
        for command in ("log", "show"):
            yield repo, ["-C", "space dir", "-C", "深い", command, "--format=%s", "-p", "HEAD", "--", "../file.txt"]
            yield repo, ["-C", "space dir", "-C", "深い", command, "--format=%s", "--name-only", "--", "../../sibling.txt"]
            yield sub, [command, "--oneline", "--", "../space dir/file.txt"]
            yield repo, [command, "--format=%s", "--", "space dir/file.txt"]
        yield repo, ["-C", "space dir", "-C", "深い", "show", "HEAD:../file.txt"]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("mode", choices=("selection", "boundaries", "e2e"))
    args = parser.parse_args()
    report = {"mode": args.mode, "comparisons": 0, "failures": 0, "unexpected_errors": 0, "cases": []}
    candidate = shlex.split(os.environ.get("NGIT_NESTED_COMMAND", str(ROOT / "bin/ngit")))
    try:
        base = os.environ.get("NGIT_FIXTURE_DIR")
        if base:
            Path(base).mkdir(parents=True, exist_ok=True)
        with tempfile.TemporaryDirectory(prefix="nested-history-", dir=base) as directory:
            repo = Path(directory) / "repo"
            fixture(repo)
            for cwd, argv in cases(args.mode, repo):
                before = state(repo)
                expected = run(["git", *argv], cwd)
                oracle_after = state(repo)
                actual = run([*candidate, *argv], cwd)
                after = state(repo)
                passed = expected == actual and before == oracle_after == after
                report["comparisons"] += 1
                report["failures"] += int(not passed)
                report["cases"].append({"cwd": str(cwd.relative_to(repo)), "args": argv,
                                         "pass": passed, "git": packed(expected), "ngit": packed(actual),
                                         "state_equal": before == oracle_after == after})
    except Exception as error:
        report["unexpected_errors"] += 1
        report["error"] = f"{type(error).__name__}: {error}"
    output = os.environ.get("NGIT_NESTED_REPORT")
    if output:
        Path(output).write_text(json.dumps(report, indent=2) + "\n")
    summary = {key: value for key, value in report.items() if key != "cases"}
    print(json.dumps(summary, sort_keys=True))
    return 2 if report["unexpected_errors"] else int(bool(report["failures"] or not report["comparisons"]))


if __name__ == "__main__":
    raise SystemExit(main())
