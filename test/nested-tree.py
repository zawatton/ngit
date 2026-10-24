#!/usr/bin/env python3
"""Compare literal ls-tree selection, formatting and read-only repository state."""
import hashlib
import json
import os
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
MODE = sys.argv[1] if len(sys.argv) == 2 else ""
if MODE not in ("selection", "format", "e2e"):
    sys.exit("usage: nested-tree.sh selection|format|e2e")
BASE = Path(os.environ.get("NGIT_FIXTURE_DIR", ROOT / "target/nested-tree-fixtures"))
BASE.mkdir(parents=True, exist_ok=True)
REPO = Path(tempfile.mkdtemp(prefix="tree-" + MODE + "-", dir=BASE.resolve()))
LOG = Path(os.environ.get("NGIT_NESTED_LOG_DIR", ROOT / "target/nested-tree-logs")) / MODE
LOG.mkdir(parents=True, exist_ok=True)
COMMAND = shlex.split(os.environ.get("NGIT_NESTED_COMMAND", str(ROOT / "bin/ngit")))
ENV = dict(os.environ)
for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR"):
    ENV.pop(key, None)
ENV.update(GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL="/dev/null", GIT_OPTIONAL_LOCKS="0",
           LC_ALL="C", TZ="UTC", GIT_AUTHOR_NAME="Nested Tree", GIT_AUTHOR_EMAIL="tree@example.com",
           GIT_COMMITTER_NAME="Nested Tree", GIT_COMMITTER_EMAIL="tree@example.com",
           GIT_AUTHOR_DATE="1700600000 +0000", GIT_COMMITTER_DATE="1700600000 +0000")
REPORT = {"comparisons": 0, "failures": 0, "unexpected_errors": 0, "cases": []}


def run(command, cwd=REPO, check=False):
    result = subprocess.run(command, cwd=cwd, env=ENV, capture_output=True, timeout=45)
    if check and result.returncode:
        raise RuntimeError(f"fixture command failed: {command}: {result.stderr!r}")
    return result


def git(*args):
    return run(["git", *args], check=True).stdout


def snapshot():
    return {str(p.relative_to(REPO)): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(REPO.rglob("*")) if p.is_file()}


def compare(name, args, cwd="sub", global_args=None):
    prefix = global_args if global_args is not None else ["-C", str(REPO / cwd)]
    before = snapshot()
    oracle = run(["git", *prefix, "ls-tree", *args])
    actual = run([*COMMAND, *prefix, "ls-tree", *args])
    fsck = run(["git", "fsck", "--full", "--no-reflogs"])
    got = (actual.returncode, actual.stdout, actual.stderr)
    want = (oracle.returncode, oracle.stdout, oracle.stderr)
    ok = got == want and before == snapshot() and fsck.returncode == 0
    REPORT["comparisons"] += 1
    REPORT["failures"] += not ok
    REPORT["cases"].append({"name": name, "ok": ok, "args": args,
                            "git_exit": oracle.returncode, "ngit_exit": actual.returncode,
                            "state_unchanged": before == snapshot(), "fsck_exit": fsck.returncode})
    for label, result in (("git", oracle), ("ngit", actual), ("fsck", fsck)):
        for stream in ("stdout", "stderr"):
            (LOG / f"{name}.{label}.{stream}").write_bytes(getattr(result, stream))
    if not ok:
        print("FAIL", name, repr(want), repr(got))


try:
    git("init", "-q", "-b", "main")
    for name in ("root.txt", "sub/file.txt", "sub/deep/leaf.txt", "sub/deep/more/end.txt",
                 "sibling/other.txt", "sub/sp ace.txt", "sub/日本語.txt", "sub/-dash.txt"):
        path = REPO / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(name + "\n", encoding="utf-8")
    git("add", ".")
    git("commit", "-qm", "base")
    git("branch", "old")
    (REPO / "sub/file.txt").write_text("new version\n")
    git("add", ".")
    git("commit", "-qm", "next")
    oid = git("rev-parse", "HEAD").decode().strip()
    git("update-index", "--add", "--cacheinfo", "160000", oid, "sub/module")
    git("commit", "-qm", "gitlink")
    if MODE == "selection":
        for index, path in enumerate(("file.txt", "deep", "deep/", "deep/leaf.txt", "../root.txt",
                                      "../sibling", "../sibling/", ".", "..", "../", "missing",
                                      "file.txt/", "sp ace.txt", "日本語.txt", "./file.txt")):
            compare(f"literal-{index}", ["HEAD", path])
        compare("union", ["HEAD", "file.txt", "deep/leaf.txt", "../root.txt"])
        compare("double-c", ["HEAD", "../file.txt"], global_args=["-C", str(REPO), "-C", "sub/deep"])
        compare("old", ["old", "file.txt"])
        compare("tree-path", ["HEAD:sub", "deep/"], cwd="")
        compare("tree-path-nested", ["HEAD:sub", "file.txt"])
        compare("separator", ["HEAD", "--", "-dash.txt"])
        compare("separator-before-rev", ["--", "HEAD"])
        compare("separator-before-rev-path", ["--", "HEAD", "file.txt"])
        compare("gitlink-directory", ["HEAD", "module/"])
        compare("gitlink-directory-recursive", ["-r", "HEAD", "module/"])
    elif MODE == "format":
        for index, options in enumerate(([], ["-r"], ["-t"], ["-r", "-t"], ["--name-only"],
                                         ["-r", "--name-only"], ["--name-status"], ["--full-name"],
                                         ["--full-tree"], ["--full-tree", "-r", "-t", "--name-only"])):
            compare(f"default-{index}", [*options, "HEAD"])
            compare(f"directory-{index}", [*options, "HEAD", "deep/"])
        compare("full-tree-root-path", ["--full-tree", "HEAD", "sub/deep/"])
        compare("full-name-parent", ["--full-name", "HEAD", "../sibling/"])
        compare("tree-path-full", ["--full-tree", "HEAD:sub", "deep/"])
        compare("missing-leaf-ancestors", ["-t", "HEAD", "deep/missing"])
    else:
        for index, args in enumerate((["HEAD"], ["-r", "HEAD"], ["-r", "-t", "HEAD"],
                                     ["--name-only", "-r", "HEAD"], ["old"],
                                     ["-r", "--full-name", "HEAD", "../sibling/"],
                                     ["-t", "HEAD", "deep/more/end.txt"],
                                     ["--full-tree", "HEAD:sub", "deep/"],
                                     ["HEAD", "sp ace.txt", "日本語.txt"])):
            compare(f"workflow-{index}", args, cwd="" if index < 5 else "sub")
except Exception as error:
    REPORT["unexpected_errors"] += 1
    REPORT["error"] = repr(error)
    print("ERROR", repr(error))
finally:
    text = json.dumps(REPORT, indent=2) + "\n"
    (LOG / "report.json").write_text(text)
    if os.environ.get("NGIT_NESTED_REPORT"):
        Path(os.environ["NGIT_NESTED_REPORT"]).write_text(text)
print(f"{MODE}: {REPORT['comparisons']} comparisons, {REPORT['failures']} failures, "
      f"{REPORT['unexpected_errors']} unexpected errors")
sys.exit(1 if REPORT["failures"] else 2 if REPORT["unexpected_errors"] else 0)
