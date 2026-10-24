#!/usr/bin/env python3
"""Compare status/diff rename detection and complete repository state with Git."""
import base64
import hashlib
import json
import os
from pathlib import Path
import re
import resource
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
COMMAND = shlex.split(os.environ.get("NGIT_COMPAT_COMMAND", str(ROOT / "bin/ngit")))
ENV = dict(os.environ, LC_ALL="C", TZ="UTC", GIT_OPTIONAL_LOCKS="0",
           GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL=os.devnull,
           GIT_AUTHOR_NAME="Similarity Author", GIT_AUTHOR_EMAIL="similarity@example.test",
           GIT_COMMITTER_NAME="Similarity Author", GIT_COMMITTER_EMAIL="similarity@example.test",
           GIT_AUTHOR_DATE="1700000000 +0000", GIT_COMMITTER_DATE="1700000000 +0000",
           GIT_EDITOR="true", GIT_MERGE_AUTOEDIT="no")
for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR"):
    ENV.pop(key, None)
notes = Path(os.environ["NOTES_ROOT"]) if os.environ.get("NOTES_ROOT") else next(
    (path for path in ROOT.parents if (path / "bin/heavy-run.sh").is_file()), None)
if notes:
    ENV.setdefault("NGIT_NELISP_ROOT", str(notes / "dev/nelisp"))
REPORT = dict(comparisons=0, failures=0, unexpected_errors=0, assertions=0, cases=[])
DEADLINE = time.monotonic() + 540
BODY = b"".join(("line %02d stable content\n" % n).encode() for n in range(20))
EDIT = BODY.replace(b"line 07 stable content", b"line 07 edited content")


def require(value, message):
    REPORT["assertions"] += 1
    if not value:
        raise RuntimeError(message)


def run(repo, args, native=True, check=False):
    budget = min(60, DEADLINE - time.monotonic())
    require(budget > 0, "Overall timeout")
    process = subprocess.Popen((["git"] if native else COMMAND) + ["-C", str(repo), *args],
                               env=ENV, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               start_new_session=True)
    try:
        out, err = process.communicate(timeout=budget)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGKILL)
        process.communicate()
        raise RuntimeError("Command timeout: " + repr(args))
    result = (process.returncode, out, err)
    require(process.returncode >= 0 and process.returncode not in (126, 127, 134, 139)
            and not re.search(rb"void-function|void-variable|wrong-type-argument|nelisp:.*(?:error|signal)|"
                              rb"backtrace \(innermost|error while loading|out of memory|resource exhausted", err),
            "Runtime failure: " + repr(result))
    if check:
        require(result[0] == 0, "Fixture command failed: " + repr((args, result)))
    return result


def put(repo, name, body):
    path = repo / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(body)


def fixture(repo, kind="edited"):
    repo.mkdir()
    run(repo, ["init", "-qb", "main"], check=True)
    run(repo, ["config", "gc.auto", "0"], check=True)
    put(repo, ".gitignore", b"ignored/\n")
    binary = b"\x00" + BODY
    if kind in ("ambiguous", "repeat-exact"):
        for name in ("old/a", "old/b"):
            put(repo, name, BODY)
    elif kind == "limit":
        put(repo, "old/a", BODY)
        put(repo, "old/b", BODY.replace(b"stable", b"second"))
    else:
        put(repo, "old/sub/file.txt", binary if kind == "binary" else BODY)
    run(repo, ["add", "-A"], check=True)
    run(repo, ["commit", "-qm", "base"], check=True)
    if kind in ("ambiguous", "repeat-exact", "limit"):
        for old, new in (("old/a", "new/c"), ("old/b", "new/d")):
            data = (repo / old).read_bytes()
            if kind != "repeat-exact":
                data = data.replace(b"line 07", b"edit 07")
            (repo / old).unlink()
            put(repo, new, data)
    else:
        (repo / "old/sub/file.txt").unlink()
        if kind == "reordered-basename":
            put(repo, "new/file.txt", b"".join(reversed(BODY.splitlines(keepends=True))))
        else:
            put(repo, "new/deep/moved.txt", binary.replace(b"line 07", b"edit 07") if kind == "binary"
                else BODY if kind == "exact" else EDIT)
        if kind == "low":
            put(repo, "new/deep/moved.txt", b"".join(
                ("line %02d changed string\n" % n).encode() if n < 12
                else ("line %02d stable content\n" % n).encode() for n in range(20)))
        if kind == "mode":
            (repo / "new/deep/moved.txt").chmod(0o755)
    put(repo, "ignored/noise", b"ignored\x00noise")
    run(repo, ["add", "-A"], check=True)
    if kind == "unstaged":
        run(repo, ["reset", "-q", "HEAD"], check=True)
        run(repo, ["add", "-N", "new/deep/moved.txt"], check=True)


def packed(value):
    return dict(exit=value[0], stdout_b64=base64.b64encode(value[1]).decode(),
                stderr_b64=base64.b64encode(value[2]).decode())


def compare(name, left, right, args):
    expected, actual = run(left, args), run(right, args, native=False)
    REPORT["comparisons"] += 1
    failed = expected != actual
    REPORT["failures"] += int(failed)
    REPORT["cases"].append(dict(name=name, args=args, expected=packed(expected), actual=packed(actual)))
    if failed:
        print("FAIL", name, repr(expected), repr(actual))


def state(repo):
    # Compare logical storage and every remaining byte, including operation
    # files, symbolic HEAD, configuration and full reflogs.
    files = {}
    for path in sorted(repo.rglob("*")):
        name = path.relative_to(repo).as_posix()
        if name in (".git/index", ".git/packed-refs") or name.startswith((".git/objects/", ".git/refs/")):
            continue
        if path.is_symlink():
            files[name] = ("symlink", os.readlink(path))
        elif path.is_file():
            files[name] = (path.stat().st_mode & 0o777, hashlib.sha256(path.read_bytes()).hexdigest())
    return dict(files=files, metadata_directories=sorted(str(path.relative_to(repo))
                for path in (repo / ".git").rglob("*") if path.is_dir()
                and not str(path.relative_to(repo)).startswith((".git/objects", ".git/refs"))),
                refs=packed(run(repo, ["for-each-ref", "--format=%(refname)%00%(objectname)%00%(symref)"])),
                index=packed(run(repo, ["ls-files", "-v", "--stage", "-z"])),
                fsck=packed(run(repo, ["fsck", "--full"])))


def objects(repo):
    return {str(path.relative_to(repo)): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in (repo / ".git/objects").rglob("*") if path.is_file()}


def selected(name):
    return os.environ.get("NGIT_COMPAT_MATCH", "") in name


def main(mode, area):
    if mode == "baseline-semantic-red":
        cases = [("similarity-status-edited-baseline-semantic-red", "edited", ["status", "--porcelain"])]
    elif mode == "status":
        cases = [("status-" + name, kind, args) for name, kind, args in (
            ("porcelain", "edited", ["status", "--porcelain"]),
            ("short", "edited", ["status", "--short"]),
            ("long", "edited", ["status", "--long"]),
            ("exact", "exact", ["status", "--porcelain"]),
            ("unstaged-porcelain", "unstaged", ["status", "--porcelain"]),
            ("unstaged-short", "unstaged", ["status", "--short"]),
            ("unstaged-long", "unstaged", ["status", "--long"]),
            ("unstaged-disabled", "unstaged", ["status", "--porcelain", "--no-renames"]),
            ("disabled", "edited", ["status", "--porcelain", "--no-renames"]),
            ("config-diff-off", "edited", ["-c", "diff.renames=false", "status", "--porcelain"]),
            ("config-status-off", "edited", ["-c", "status.renames=false", "status", "--porcelain"]),
            ("limit", "limit", ["-c", "diff.renameLimit=1", "status", "--porcelain"]),
            ("limit-override", "limit", ["-c", "diff.renameLimit=1", "-c", "status.renameLimit=0", "status", "--porcelain"]),
            ("config-override", "edited", ["-c", "diff.renames=false", "-c", "status.renames=true", "status", "--porcelain"]))]
    elif mode == "diff":
        cases = [("diff-" + name, kind, args) for name, kind, args in (
            ("patch", "edited", ["diff", "--cached"]),
            ("names", "edited", ["diff", "--cached", "--name-only"]),
            ("score", "edited", ["diff", "--cached", "--name-status"]),
            ("head-worktree", "edited", ["diff", "HEAD"]),
            ("unstaged-patch", "unstaged", ["diff"]),
            ("unstaged-score", "unstaged", ["diff", "--name-status"]),
            ("binary", "binary", ["diff", "--cached"]),
            ("mode", "mode", ["diff", "--cached"]),
            ("ambiguous", "ambiguous", ["diff", "--cached", "--name-status"]),
            ("repeat-exact", "repeat-exact", ["diff", "--cached", "--name-status"]),
            ("exact", "exact", ["diff", "--cached"]))]
    elif mode in ("boundaries", "audit-regressions"):
        cases = [("boundary-" + name, kind, args) for name, kind, args in (
            ("threshold-100", "edited", ["diff", "--cached", "--name-status", "-M100%"]),
            ("audit-exact-only-basename-reordered", "reordered-basename",
             ["diff", "--cached", "--name-status", "-M100%"]),
            ("audit-empty-long-threshold", "edited",
             ["diff", "--cached", "--name-status", "--find-renames="]),
            ("threshold-50", "edited", ["diff", "--cached", "--name-status", "-M50%"]),
            ("threshold-equal", "edited", ["diff", "--cached", "--name-status", "-M95%"]),
            ("threshold-above", "edited", ["diff", "--cached", "--name-status", "-M96%"]),
            ("threshold-low", "low", ["diff", "--cached", "--name-status", "-M30%"]),
            ("threshold-low-default", "low", ["diff", "--cached", "--name-status"]),
            ("threshold-fraction", "edited", ["diff", "--cached", "--name-status", "-M5"]),
            ("threshold-long", "edited", ["diff", "--cached", "--find-renames=90%"]),
            ("threshold-reset", "edited", ["diff", "--cached", "-M100%", "-M"]),
            ("no-renames", "exact", ["diff", "--cached", "--no-renames"]),
            ("config-disabled", "edited", ["-c", "diff.renames=false", "diff", "--cached"]),
            ("enable-override", "edited", ["-c", "diff.renames=false", "diff", "--cached", "-M"]),
            ("status-target-path", "edited", ["status", "--porcelain", "--", "new"]),
            ("diff-target-path", "edited", ["diff", "--cached", "--name-status", "--", "new"]),
            ("status-magic", "edited", ["status", "--porcelain", "--", ":(glob)new/**"]),
            ("diff-magic", "edited", ["diff", "--cached", "--name-status", "--", ":(glob)**/*.txt"]),
            ("nested-status", "edited", ["-C", "new/deep", "status", "--porcelain", "--", "."]),
            ("nested-diff", "edited", ["-C", "new/deep", "diff", "--cached", "--name-status", "--", "."]),
            ("limit", "limit", ["-c", "diff.renameLimit=1", "diff", "--cached", "--name-status"]),
            ("limit-zero", "limit", ["-c", "diff.renameLimit=0", "diff", "--cached", "--name-status"]))]
        if mode == "audit-regressions":
            cases = [case for case in cases if case[0].startswith("boundary-audit-")]
    elif mode == "e2e":
        cases = []
        name = "e2e-edit-move-add-status-diff-commit"
        if selected(name):
            left, right = area / "git", area / "ngit"
            fixture(left)
            run(left, ["reset", "--hard", "HEAD"], check=True)
            shutil.copytree(left, right)
            compare(name + "-move", left, right, ["mv", "old/sub/file.txt", "moved.txt"])
            for repo in (left, right):
                put(repo, "moved.txt", EDIT + b"workflow edit\n")
                put(repo, "new/addition", b"nonempty added file\n")
            for n, args in enumerate((["add", "-A"], ["status", "--porcelain"],
                                      ["diff", "--cached"], ["diff", "--cached", "--name-status"],
                                      ["commit", "-m", "rename workflow"], ["status", "--porcelain"],
                                      ["diff", "HEAD~", "HEAD"], ["fsck", "--full"])):
                compare(name + "-" + str(n), left, right, args)
            require(state(left) == state(right), "E2E refs/index/worktree/reflogs/full fsck differ")
    else:
        raise RuntimeError("Unknown mode: " + mode)
    for number, (name, kind, args) in enumerate(cases):
        if selected(name):
            left, right = area / (str(number) + "-git"), area / (str(number) + "-ngit")
            fixture(left, kind)
            while args and args[0] == "-c":
                key, value = args[1].split("=", 1)
                run(left, ["config", key, value], check=True)
                args = args[2:]
            shutil.copytree(left, right)
            before = state(right)
            before_objects = objects(right)
            compare(name, left, right, args)
            require(state(right) == before, "Read command changed repository state: " + name)
            require(objects(right) == before_objects, "Read command changed object storage: " + name)
    require(REPORT["comparisons"] > 0, "Zero selected command comparisons")


if __name__ == "__main__":
    resource.setrlimit(resource.RLIMIT_AS, (8 * 1024**3, 8 * 1024**3))
    sources = [ROOT / "bin/ngit", *sorted((ROOT / "lisp").glob("*.el")),
               *sorted((ROOT / "test").glob("*"))]
    def fingerprint():
        return {str(path.relative_to(ROOT)): hashlib.sha256(path.read_bytes()).hexdigest()
                for path in sources if path.is_file()}
    REPORT["source_before"] = fingerprint()
    try:
        with tempfile.TemporaryDirectory(prefix="ngit-similarity-") as temporary:
            main(sys.argv[1], Path(temporary))
    except Exception as error:
        REPORT["unexpected_errors"] += 1
        print("ERROR", str(error), file=sys.stderr)
    REPORT["source_after"] = fingerprint()
    if REPORT["source_before"] != REPORT["source_after"]:
        REPORT["unexpected_errors"] += 1
        print("ERROR source fingerprint changed", file=sys.stderr)
    if os.environ.get("NGIT_COMPAT_REPORT"):
        Path(os.environ["NGIT_COMPAT_REPORT"]).write_text(json.dumps(REPORT, indent=2) + "\n")
    print(json.dumps({key: REPORT[key] for key in ("comparisons", "failures", "unexpected_errors", "assertions")}))
    sys.exit(2 if REPORT["unexpected_errors"] else 1 if REPORT["failures"] else 0)
