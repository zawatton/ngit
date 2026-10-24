#!/usr/bin/env python3
"""Exact native-Git comparisons for nested grep and blame invocation paths."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
ENV = dict(os.environ, LC_ALL="C", GIT_CONFIG_NOSYSTEM="1",
           GIT_AUTHOR_NAME="Nested Author", GIT_AUTHOR_EMAIL="nested@example.com",
           GIT_COMMITTER_NAME="Nested Author", GIT_COMMITTER_EMAIL="nested@example.com",
           GIT_AUTHOR_DATE="1700000000 +0000", GIT_COMMITTER_DATE="1700000000 +0000",
           GIT_OPTIONAL_LOCKS="0")


def run(command, cwd):
    p = subprocess.run(command, cwd=cwd, env=ENV, capture_output=True, timeout=60)
    return p.returncode, p.stdout, p.stderr


def git(repo, *args):
    result = run(["git", *args], repo)
    if result[0]:
        raise RuntimeError((args, result))
    return result


def snapshot(repo):
    files = {}
    for path in sorted(repo.rglob("*")):
        if path.is_file() and ".git" not in path.relative_to(repo).parts:
            files[str(path.relative_to(repo))] = hashlib.sha256(path.read_bytes()).hexdigest()
    return (hashlib.sha256((repo / ".git/index").read_bytes()).hexdigest(),
            git(repo, "show-ref"), files, git(repo, "fsck", "--full", "--no-reflogs"))


def fixture(repo):
    repo.mkdir()
    git(repo, "init", "-q", "-b", "main")
    git(repo, "config", "gc.auto", "0")
    for name in ["outer.txt", "sub/inner.txt", "sub/HEAD", "sub/space name.txt",
                 "sub/日本語.txt", "sub/deep/leaf.txt", "sub/dir"]:
        path = repo / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("needle one\nneedle two\nother three\n", encoding="utf-8")
    git(repo, "add", ".")
    git(repo, "commit", "-qm", "initial")
    git(repo, "mv", "sub/inner.txt", "sub/renamed.txt")
    (repo / "sub/renamed.txt").write_text("needle one\nchanged two\nother three\n")
    git(repo, "add", ".")
    ENV.update(GIT_AUTHOR_DATE="1700000010 +0000", GIT_COMMITTER_DATE="1700000010 +0000")
    git(repo, "commit", "-qm", "rename and edit")
    # Cached and work-tree searches must read different final images.
    (repo / "sub/deep/leaf.txt").write_text("needle cached\n")
    git(repo, "add", "sub/deep/leaf.txt")
    (repo / "sub/deep/leaf.txt").write_text("needle worktree\n")


def cases(mode):
    if mode == "grep":
        for options in [[], ["-l"], ["-n"], ["-c"], ["-z", "-l"], ["-h"],
                        ["-F", "-n"], ["-E", "-n"], ["-v", "-n"], ["-q"]]:
            yield "sub", [], ["grep", *options, "needle"]
        for source in [[], ["--cached"], ["HEAD"]]:
            for paths in [[], ["."], ["../outer.txt"], ["deep/"], ["space name.txt"],
                          ["日本語.txt"], ["../"], ["dir/"], ["HEAD"]]:
                opts = ["--cached"] if source == ["--cached"] else []
                trees = source if source == ["HEAD"] else []
                yield "sub", [], ["grep", "-n", *opts, "needle", *trees, "--", *paths]
        yield "sub", [], ["grep", "-l", "needle", "--", "HEAD"]
        yield "sub", [], ["grep", "--", "needle", "renamed.txt"]
        yield "sub", [], ["grep", "--", "-needle", "renamed.txt"]
        yield "sub", [], ["grep", "--", "needle", "--", "HEAD"]
        yield ".", ["-C", "sub", "-C", "deep"], ["grep", "-n", "needle", "HEAD", "--", "../../outer.txt"]
        yield "sub/deep", [], ["grep", "-n", "needle", "--", "../../"]
        yield ".", [], ["grep", "-n", "needle", "HEAD", "--", "sub/"]
    elif mode == "blame":
        for options in [[], ["-s"], ["-e"], ["-w"], ["--porcelain"],
                        ["--porcelain", "-w"], ["-L", "1,2"], ["--abbrev=12"]]:
            for name in ["renamed.txt", "space name.txt", "日本語.txt", "../outer.txt"]:
                yield "sub", [], ["blame", *options, "HEAD", "--", name]
        yield "sub", [], ["blame", "renamed.txt"]
        yield "sub", [], ["blame", "--porcelain", "--", "HEAD"]
        yield ".", ["-C", "sub", "-C", "deep"], ["blame", "--porcelain", "HEAD", "--", "../renamed.txt"]
    else:
        yield from cases("grep")
        yield from cases("blame")


def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else "e2e"
    if mode not in {"grep", "blame", "e2e"}:
        raise ValueError("expected grep, blame or e2e")
    result = dict(comparisons=0, failures=0, unexpected_errors=0)
    base = os.environ.get("NGIT_FIXTURE_DIR")
    if base:
        Path(base).mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="nested-search-", dir=base) as tmp:
        repo = Path(tmp) / "repo"
        try:
            fixture(repo)
            initial = snapshot(repo)
            executable = os.environ.get("NGIT_NESTED_COMMAND", str(ROOT / "bin/ngit"))
            for directory, global_args, args in cases(mode):
                cwd = repo / directory
                expected = run(["git", *global_args, *args], cwd)
                actual = run([executable, *global_args, *args], cwd)
                result["comparisons"] += 1
                if expected != actual:
                    result["failures"] += 1
                    print("MISMATCH", directory, global_args, args,
                          "git=", repr(expected), "ngit=", repr(actual))
            result["comparisons"] += 1
            if snapshot(repo) != initial:
                result["failures"] += 1
                print("MISMATCH repository index/refs/worktree/full-fsck state changed")
        except Exception as exc:
            result["unexpected_errors"] += 1
            print("UNEXPECTED", repr(exc))
    print(json.dumps(result, sort_keys=True))
    report = os.environ.get("NGIT_NESTED_REPORT")
    if report:
        Path(report).write_text(json.dumps(result, sort_keys=True) + "\n")
    return int(bool(result["failures"] or result["unexpected_errors"]))


if __name__ == "__main__":
    sys.exit(main())
