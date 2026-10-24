#!/usr/bin/env python3
"""Exact Git oracle comparisons for shared pathspec magic."""
import base64
import json
import os
from pathlib import Path
import shlex
import shutil
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
COMMAND = shlex.split(os.environ.get("NGIT_COMPAT_COMMAND", str(ROOT / "bin/ngit")))
ENV = dict(os.environ, GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL="/dev/null",
           LC_ALL="C", GIT_AUTHOR_NAME="A", GIT_AUTHOR_EMAIL="a@example.test",
           GIT_COMMITTER_NAME="A", GIT_COMMITTER_EMAIL="a@example.test",
           GIT_AUTHOR_DATE="2001-01-01T00:00:00+0000",
           GIT_COMMITTER_DATE="2001-01-01T00:00:00+0000")
for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR"):
    ENV.pop(key, None)
CASES = []
ERRORS = 0


def run(command, cwd):
    return subprocess.run(command, cwd=cwd, env=ENV, capture_output=True, timeout=40)


def tuple_data(result):
    return {"exit": result.returncode, "stdout": base64.b64encode(result.stdout).decode(),
            "stderr": base64.b64encode(result.stderr).decode()}


def runtime_failed(result):
    "Keep runtime/provider failures separate from semantic mismatches."
    global ERRORS
    failed = result.returncode < 0 or any(marker in result.stderr for marker in
        (b"void-function", b"void-variable", b"wrong-type-argument", b"Symbol's function definition is void",
         b"Cannot open load file", b"memory limit", b"panicked at"))
    if failed:
        ERRORS += 1
    return failed


def pair(name, left_root, right_root, args, cwd="."):
    global ERRORS
    if os.environ.get("NGIT_COMPAT_MATCH", "") not in name:
        return
    try:
        left, right = run(["git", *args], left_root / cwd), run([*COMMAND, *args], right_root / cwd)
        runtime_failed(right)
        CASES.append({"name": name, "args": args, "cwd": cwd, "expected": tuple_data(left),
                      "actual": tuple_data(right), "pass": tuple_data(left) == tuple_data(right)})
        for check in [["diff", "--cached", "--binary"], ["diff", "--binary"],
                      ["status", "--porcelain"], ["ls-files", "--stage"],
                      ["for-each-ref", "--format=%(refname) %(objectname)"],
                      ["reflog", "--format=%H %gs"], ["fsck", "--full"]]:
            a, b = run(["git", *check], left_root), run(["git", *check], right_root)
            CASES.append({"name": f"{name}-{'-'.join(check)}", "args": check,
                          "expected": tuple_data(a), "actual": tuple_data(b),
                          "pass": tuple_data(a) == tuple_data(b)})
        def files(root):
            return {str(p.relative_to(root)): base64.b64encode(p.read_bytes()).decode()
                    for p in root.rglob("*") if p.is_file() and ".git" not in p.relative_to(root).parts}
        a, b = files(left_root), files(right_root)
        CASES.append({"name": name + "-worktree-bytes", "expected": a, "actual": b, "pass": a == b})
    except (OSError, subprocess.TimeoutExpired) as error:
        ERRORS += 1
        CASES.append({"name": name, "error": str(error), "pass": False})


def compare(name, cwd, args):
    global ERRORS
    if os.environ.get("NGIT_COMPAT_MATCH", "") not in name:
        return
    try:
        expected = run(["git", *args], cwd)
        actual = run([*COMMAND, *args], cwd)
        runtime_failed(actual)
        good = (expected.returncode, expected.stdout, expected.stderr) == (actual.returncode, actual.stdout, actual.stderr)
        CASES.append({"name": name, "args": args, "expected": tuple_data(expected),
                      "actual": tuple_data(actual), "pass": good})
        print(("PASS " if good else "FAIL ") + name, flush=True)
    except (OSError, subprocess.TimeoutExpired) as error:
        ERRORS += 1
        CASES.append({"name": name, "error": str(error), "pass": False})


def fixture(root):
    root.mkdir()
    run(["git", "init", "-q", "-b", "main"], root).check_returncode()
    for name in ["a.txt", "A.TXT", "literal*.txt", ":(literal)foo", "dir/b.txt", "dir/deep/c.txt", "dir/keep.md", "other/z.txt", "unicodé.txt"]:
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("needle " + name + "\n")
    run(["git", "add", "."], root).check_returncode()
    run(["git", "commit", "-qm", "base"], root).check_returncode()
    for path in root.rglob("*.txt"):
        if ".git" not in path.parts:
            path.write_bytes(path.read_bytes() + b"change\n")


def collision_fixture(root):
    for name in ("Box/file.txt", "box/file.txt"):
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(b"base " + name.encode() + b"\n")
    run(["git", "add", "--", "Box/file.txt", "box/file.txt"], root).check_returncode()
    run(["git", "commit", "-qm", "case-sensitive prefixes"], root).check_returncode()
    for name in ("Box/file.txt", "box/file.txt"):
        (root / name).write_bytes(b"dirty " + name.encode() + b"\n")


def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else "read"
    with tempfile.TemporaryDirectory(prefix="ngit-pathspec-") as tmp:
        base = Path(tmp) / "base"
        fixture(base)
        if mode == "read":
            specs = [[":(top)a.txt"], [":/a.txt"], [":(literal)literal*.txt"],
                     [":(glob)*.txt"], [":(glob)**/*.txt"], [":(icase)a.txt"],
                     [":(top,glob)dir/*.txt", ":(exclude)dir/b.txt"],
                     [":(exclude)dir"], [":!dir"], [":^dir"], [":(top,icase)A.txt"],
                     [":(literal)a.txt/"], [":(literal)dir/"], [":(glob)dir/**/"]]
            for i, spec in enumerate(specs):
                for command in [["status", "--porcelain"], ["diff", "--name-only"],
                                ["log", "--format=%s"], ["show", "--format=", "--name-only"],
                                ["grep", "-l", "needle"]]:
                    compare(f"{command[0]}-{i}", base, [*command, "--", *spec])
            for command in [["diff", "--name-only"], ["log", "--format=%s"], ["grep", "-l", "needle"]]:
                compare(f"nested-{command[0]}", base / "dir", [*command, "--", ":(top)a.txt"])
                compare(f"nested-exclude-{command[0]}", base / "dir", [*command, "--", ":(exclude)b.txt"])
            compare("nested-exclude-status", base / "dir", ["status", "--porcelain", "--", ":(exclude)b.txt"])
            compare("grep-tree", base, ["grep", "-l", "needle", "HEAD", "--", ":(glob)dir/*.txt"])
            compare("unicode-literal", base, ["diff", "--name-only", "--", ":(literal)unicodé.txt"])
            compare("blame-magic-filename", base, ["blame", "--", ":(literal)foo"])
            compare("show-magic-filename", base, ["show", "HEAD:./:(literal)foo"])
            for name in ["loose/a.txt", "loose/deep/b.txt", "loose/read.md"]:
                path = base / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("needle\n")
            for i, spec in enumerate([[":(top)loose"], [":(glob)loose/*.txt"],
                                      [":(glob)**/*.txt"], [":(exclude)dir"],
                                      [":(top)loose", ":(exclude)loose/a.txt"]]):
                compare(f"untracked-{i}", base, ["status", "--porcelain", "--", *spec])
            head = run(["git", "rev-parse", "HEAD"], base).stdout.decode().strip()
            run(["git", "update-index", "--add", "--cacheinfo", f"160000,{head},module"], base).check_returncode()
            run(["git", "commit", "-qm", "gitlink"], base).check_returncode()
            for command in [["status", "--porcelain"], ["log", "--format=%s"],
                            ["show", "--format=", "--name-only"], ["ls-tree", "-r", "--name-only", "HEAD"]]:
                compare("gitlink-slash-" + command[0], base, [*command, "--", ":(literal)module/"])
            collision_fixture(base)
            for i, spec in enumerate([":(icase)file.txt", ":(top,icase)box/file.txt", ":(icase)../box/file.txt"]):
                for command in [["status", "--porcelain"], ["diff", "--name-only"], ["grep", "-l", "dirty"]]:
                    compare(f"prefix-read-{i}-{command[0]}", base / "box", [*command, "--", spec])
        elif mode == "boundaries":
            for spec in [":(bogus)a", ":(glob,literal)a", ":(literal,glob)a", ":(top,unknown)a"]:
                compare(spec, base, ["diff", "--name-only", "--", spec])
            for spec in [":(top)a.txt", ":/a.txt", ":(literal)a.txt", ":(glob)*.txt", ":(exclude)a.txt", ":(icase)a.txt"]:
                compare("ls-tree-" + spec, base, ["ls-tree", "-r", "--name-only", "HEAD", "--", spec])
            compare("literal-head", base, ["diff", "--name-only", "--", ":(literal)HEAD"])
            compare("plain-log-glob", base, ["log", "--format=%s", "--", "dir/*.txt"])
            for command in ("add", "restore"):
                for i, spec in enumerate([":(bogus)a", ":(glob,literal)a", ":(literal)missing"]):
                    left, right = Path(tmp) / f"left-{command}-{i}", Path(tmp) / f"right-{command}-{i}"
                    shutil.copytree(base, left)
                    shutil.copytree(base, right)
                    pair(f"atomic-{command}-{i}", left, right, [command, "--", "a.txt", spec])
        elif mode == "write":
            for i, spec in enumerate([[":(glob)dir/*.txt"], [":(literal)literal*.txt"],
                                      [":(icase)a.txt"], [":(exclude)dir"],
                                      [":(top,glob)**/*.txt", ":(exclude)dir/b.txt"]]):
                for command in ["add", "restore"]:
                    expected = Path(tmp) / f"git-{i}-{command}"
                    actual = Path(tmp) / f"ngit-{i}-{command}"
                    shutil.copytree(base, expected)
                    shutil.copytree(base, actual)
                    pair(f"{command}-{i}", expected, actual, [command, "--", *spec])
            for command in ("add", "restore"):
                left, right = Path(tmp) / f"prefix-left-{command}", Path(tmp) / f"prefix-right-{command}"
                shutil.copytree(base, left)
                shutil.copytree(base, right)
                for repo in (left, right):
                    collision_fixture(repo)
                pair(f"prefix-icase-{command}", left, right, [command, "--", ":(icase)file.txt"], cwd="box")
            for name, args in [("ignored-explicit", ["add", "--", ":(literal)hidden.txt"]),
                               ("ignored-forced", ["add", "-f", "--", ":(literal)hidden.txt"]),
                               ("ignored-glob", ["add", "--", ":(glob)*.txt"]),
                               ("ignored-glob-forced", ["add", "-f", "--", ":(glob)*.txt"])]:
                left, right = Path(tmp) / (name + "-left"), Path(tmp) / (name + "-right")
                shutil.copytree(base, left)
                shutil.copytree(base, right)
                for repo in (left, right):
                    (repo / ".gitignore").write_text("hidden.txt\n")
                    (repo / "hidden.txt").write_bytes(b"ignored\n")
                    (repo / "visible.txt").write_bytes(b"visible\n")
                pair(name, left, right, args)
        elif mode == "e2e":
            left, right = Path(tmp) / "left", Path(tmp) / "right"
            shutil.copytree(base, left)
            shutil.copytree(base, right)
            for repo in (left, right):
                (repo / "dir/new.txt").write_bytes(b"binary\x00needle\xff\n")
                (repo / "dir/b.txt").unlink()
            specs = [":(top,glob)dir/**/*.txt", ":(exclude)dir/deep"]
            pair("e2e-add", left, right, ["add", "--", *specs])
            pair("e2e-commit", left, right, ["commit", "-q", "-m", "selected"])
            pair("e2e-tree-restore", left, right, ["restore", "--staged", "--worktree", "--source=HEAD~", "--", *specs])
            left, right = Path(tmp) / "nested-left", Path(tmp) / "nested-right"
            shutil.copytree(base, left)
            shutil.copytree(base, right)
            for repo in (left, right):
                collision_fixture(repo)
            pair("nested-e2e-add", left, right, ["add", "--", ":(icase)file.txt"], cwd="box")
            pair("nested-e2e-commit", left, right, ["commit", "-q", "-m", "nested selection"], cwd="box")
            for repo in (left, right):
                (repo / "box/file.txt").write_bytes(b"second lower edit\n")
            pair("nested-e2e-restore", left, right,
                 ["restore", "--source=HEAD~", "--staged", "--worktree", "--", ":(icase)file.txt"], cwd="box")
        else:
            raise SystemExit("unknown mode")
    report = {"comparisons": len(CASES) - ERRORS, "failures": sum(not c["pass"] for c in CASES) - ERRORS,
              "unexpected_errors": ERRORS, "cases": CASES}
    path = os.environ.get("NGIT_COMPAT_REPORT")
    if path:
        Path(path).write_text(json.dumps(report, indent=2) + "\n")
    print(f"{report['comparisons']} comparisons, {report['failures']} failures, {ERRORS} unexpected errors")
    return bool(report["failures"] or ERRORS or not report["comparisons"])


if __name__ == "__main__":
    sys.exit(main())
