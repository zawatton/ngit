#!/usr/bin/env python3
"""Run isolated, deterministic native-Git comparisons for nested commands."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
MODE = sys.argv[1] if len(sys.argv) == 2 else ""
if MODE not in ("read", "write", "boundaries", "e2e"):
    sys.exit("usage: subdir-suite.sh read|write|boundaries|e2e")
BASE = Path(os.environ.get("NGIT_FIXTURE_DIR", ROOT / "target/subdir-fixtures"))
AREA = BASE.resolve() / ("subdir-" + MODE)
REPO = AREA / "repo"
LOG = Path(os.environ.get("NGIT_SUBDIR_LOG_DIR", ROOT / "target/subdir-lane")) / MODE
LOG.mkdir(parents=True, exist_ok=True)
NGIT = os.environ.get("NGIT_SUBDIR_COMMAND", str(ROOT / "bin/ngit"))
ENV = dict(os.environ)
for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR"):
    ENV.pop(key, None)
ENV.update(GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL="/dev/null",
           GIT_OPTIONAL_LOCKS="0", LC_ALL="C", TZ="UTC",
           GIT_AUTHOR_NAME="Subdir Test", GIT_AUTHOR_EMAIL="subdir@example.com",
           GIT_COMMITTER_NAME="Subdir Test", GIT_COMMITTER_EMAIL="subdir@example.com",
           GIT_AUTHOR_DATE="1700600000 +0000", GIT_COMMITTER_DATE="1700600000 +0000")
COUNT = 0
FAIL = 0
MATCH = os.environ.get("NGIT_SUBDIR_MATCH", "")


def run(argv, cwd=REPO, check=False):
    result = subprocess.run(argv, cwd=cwd, env=ENV, capture_output=True, timeout=45)
    if check and result.returncode:
        raise RuntimeError(f"fixture command failed: {argv}: {result.stderr!r}")
    return result


def git(*args):
    return run(["git", *args], check=True).stdout


def put(path, text):
    dest = REPO / path
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_bytes(text.encode())


def setup(kind="dirty"):
    if AREA.exists():
        shutil.rmtree(AREA)
    REPO.mkdir(parents=True)
    git("init", "-q", "-b", "main")
    git("config", "core.filemode", "true")
    git("config", "core.autocrlf", "false")
    put(".gitignore", "*.ignored\n")
    put(".gitattributes", "*.crlf text eol=crlf\n")
    for path in ("top.txt", "dir/file.txt", "dir/deep/nested.txt", "sibling/other.txt",
                 "dir/sp ace.txt", "dir/--name-only", "dir/attr.crlf"):
        put(path, "one\r\ntwo\r\n" if path.endswith(".crlf") else "one\ntwo\n")
    git("add", ".")
    git("commit", "-qm", "base")
    put("dir/file.txt", "one\ntwo\nthree\n")
    git("add", ".")
    git("commit", "-qm", "second")
    if kind in ("dirty", "linked"):
        for path in ("top.txt", "dir/file.txt", "dir/deep/nested.txt", "sibling/other.txt",
                     "dir/sp ace.txt", "dir/--name-only"):
            put(path, "changed\n")
        git("add", "dir/sp ace.txt")
        put("dir/new.txt", "new\n")
        put("sibling/new.txt", "new\n")
        put("dir/hide.ignored", "ignored\n")
        put("root.ignored", "ignored\n")
    if kind == "linked":
        git("worktree", "add", "-qb", "linked", str(AREA / "linked"))
        (AREA / "linked/dir/file.txt").write_text("linked change\n")


def state(repo):
    def inspect(*args):
        r = run(["git", "-C", str(repo), *args])
        return [r.returncode, r.stdout.hex(), r.stderr.hex()]
    files = {}
    for path in sorted(repo.rglob("*")):
        rel = path.relative_to(repo)
        if rel.parts[0] == ".git" or path.is_dir():
            continue
        files[str(rel)] = [path.lstat().st_mode & 0o777,
                           os.readlink(path) if path.is_symlink() else
                           hashlib.sha256(path.read_bytes()).hexdigest()]
    return dict(index=inspect("ls-files", "--stage", "-v", "-z"),
                refs=inspect("show-ref", "--head"), files=files,
                status=inspect("status", "--porcelain"),
                fsck=inspect("fsck", "--no-dangling"))


def compare(name, args, cwd="dir", kind="dirty", mutate=False, prepare=None, expected_rc=0):
    global COUNT, FAIL
    if MATCH and MATCH not in name:
        return
    COUNT += 1
    observations = []
    for exe, tag in (("git", "git"), (NGIT, "ngit")):
        setup(kind)
        if prepare:
            prepare()
        work = AREA / "linked" if kind == "linked" else REPO
        here = work / cwd
        result = run([exe, *args], cwd=here)
        if tag == "git" and result.returncode != expected_rc:
            raise RuntimeError(f"oracle command {name} returned {result.returncode}, expected {expected_rc}")
        observation = dict(rc=result.returncode, stdout=result.stdout.hex(),
                           stderr=result.stderr.hex())
        if mutate:
            observation["state"] = state(work)
        (LOG / (name + "." + tag + ".json")).write_text(json.dumps(observation, indent=2))
        observations.append(observation)
    if observations[0] != observations[1]:
        FAIL += 1
        fields = [key for key in observations[0] if observations[0][key] != observations[1][key]]
        print(f"FAIL {name}: {','.join(fields)}", flush=True)
    else:
        print(f"PASS {name}", flush=True)


def read_cases():
    for label, path in (("modified", "file.txt/"), ("staged", "sp ace.txt/"),
                        ("untracked", "new.txt/"), ("missing", "missing/"),
                        ("directory", "deep/")):
        compare("slash-status-" + label, ["status", "--short", "--", path], mutate=True)
    compare("slash-status-multiple", ["status", "--short", "--", "file.txt/", "sp ace.txt/"],
            mutate=True)
    compare("slash-status-untracked-directory", ["status", "--short", "--", "loose/"],
            mutate=True, prepare=lambda: put("dir/loose/new.txt", "new\n"))
    for label, opts, path in (("worktree", [], "file.txt/"),
                              ("cached", ["--cached"], "sp ace.txt/"),
                              ("head", ["HEAD"], "file.txt/"),
                              ("trees", ["HEAD~1", "HEAD"], "file.txt/")):
        compare("slash-diff-" + label, ["diff", *opts, "--name-only", "--", path], mutate=True)
    for label, args in (("long", ["status"]), ("short", ["status", "--short"]),
                        ("porcelain", ["status", "--porcelain"])):
        compare("nested-" + label, args)
        compare("root-" + label, args, cwd=".")
        compare("deep-" + label, args, cwd="dir/deep")
        compare("dot-" + label, args + ["--", "."])
        compare("sibling-" + label, args + ["--", "../sibling"])
    for label, options in (("patch", []), ("names", ["--name-only"]),
                           ("name-status", ["--name-status"])):
        compare("diff-all-" + label, ["diff", *options])
        compare("diff-dot-" + label, ["diff", *options, "--", "."])
        compare("diff-sibling-" + label, ["diff", *options, "--", "../sibling"])
        compare("diff-cached-" + label, ["diff", "--cached", *options, "--", "."])
        compare("diff-head-" + label, ["diff", "HEAD", *options, "--", "file.txt"])
        compare("diff-trees-" + label, ["diff", "HEAD~1", "HEAD", *options, "--", "file.txt"])
    compare("diff-dash-path", ["diff", "--", "--name-only"])
    compare("status-relative-false", ["status", "--short"],
            prepare=lambda: git("config", "status.relativePaths", "false"))
    def loose_files():
        put("dir/loose/deep/one.txt", "new\n")
        put("dir/loose/deep/two.txt", "new\n")
        put("dir/loose/other.txt", "new\n")
    for label, path in (("directory", "loose"), ("deep-directory", "loose/deep"),
                        ("file", "loose/deep/one.txt")):
        compare("untracked-" + label, ["status", "--short", "--", path], prepare=loose_files)
    compare("untracked-cwd", ["status", "--short"], cwd="dir/loose/deep", prepare=loose_files)
    compare("untracked-cwd-dot", ["status", "--short", "--", "."],
            cwd="dir/loose/deep", prepare=loose_files)
    compare("untracked-overlap", ["status", "--short", "--", "loose/deep", "loose/deep/one.txt"],
            prepare=loose_files)
    compare("untracked-root-overlap", ["status", "--short", "--", ".", "loose/deep/one.txt"],
            prepare=loose_files)
    for path in (".", "../sibling/moved.txt", "../sibling", "../"):
        label = path.replace("/", "_")
        for command in (["status", "--short"], ["diff", "--cached", "--name-status"]):
            compare("rename-" + command[0] + "-" + label, [*command, "--", path],
                    kind="clean", prepare=lambda: git("mv", "dir/file.txt", "sibling/moved.txt"))
    def quoted_rename():
        git("mv", "dir/sp ace.txt", "dir/é space.txt")
    compare("quoted-relative-status", ["status", "--short"], kind="clean", prepare=quoted_rename)
    compare("quoted-literal-diff", ["diff", "--cached", "--", "é space.txt"],
            kind="clean", prepare=quoted_rename)


def write_cases():
    for label, args in (("file", ["file.txt"]), ("dot", ["."]),
                        ("sibling", ["../sibling"]), ("parent", [".."]),
                        ("all", ["-A"]), ("update", ["-u"]),
                        ("dry", ["-n", "."]), ("verbose", ["-v", "."]),
                        ("intent", ["-N", "new.txt"]),
                        ("force", ["-f", "hide.ignored"])):
        compare("add-" + label, ["add", *args], mutate=True)
    for label, opts in (("worktree", []), ("staged", ["--staged"]),
                        ("both", ["-SW"]), ("source", ["-s", "HEAD~1"]),
                        ("source-staged", ["-s", "HEAD~1", "-S"]),
                        ("source-both", ["-s", "HEAD~1", "-SW"])):
        compare("restore-" + label, ["restore", *opts, "--", "."], mutate=True)
    compare("restore-sibling", ["restore", "--", "../sibling"], mutate=True)
    compare("restore-parent", ["restore", "--", ".."], mutate=True)


def boundaries():
    compare("slash-repeated-C-status", ["-C", "..", "-C", "dir", "status", "--short", "--", "file.txt/"],
            mutate=True)
    compare("slash-repeated-C-diff", ["-C", "..", "-C", "dir", "diff", "--name-only", "--", "file.txt/"],
            mutate=True)
    compare("repeated-C-status", ["-C", "..", "-C", "sibling", "status", "--short"])
    compare("repeated-C-diff", ["-C", "..", "-C", "sibling", "diff", "--", "."])
    compare("equivalent-C-status", ["-C", "dir", "status", "--short"], cwd=".")
    compare("equivalent-C-add", ["-C", "dir", "add", "."], cwd=".", mutate=True)
    compare("equivalent-C-restore", ["-C", "dir", "restore", "--", "."], cwd=".", mutate=True)
    compare("repeated-C-add", ["-C", "..", "-C", "sibling", "add", "."], mutate=True)
    compare("repeated-C-restore", ["-C", "..", "-C", "sibling", "restore", "--", "."], mutate=True)
    for cmd in ("add", "restore", "diff", "status"):
        compare("outside-" + cmd, [cmd, "--", "../../outside"], mutate=True, expected_rc=128)
    compare("ignored-add", ["add", "hide.ignored"], mutate=True, expected_rc=1)
    compare("attributes-add", ["add", "attr.crlf"], mutate=True,
            prepare=lambda: put("dir/attr.crlf", "new\r\nvalue\r\n"))
    for cmd in (["status"], ["status", "--short"], ["status", "--porcelain"],
                ["diff", "--", "."], ["add", "."], ["restore", "--", "."]):
        compare("linked-" + "-".join(cmd).replace("/", "_"), cmd,
                kind="linked", mutate=True)


def e2e():
    global COUNT, FAIL
    if MATCH and MATCH not in "nested-workflow":
        return
    COUNT += 1
    records = []
    for exe, tag in (("git", "git"), (NGIT, "ngit")):
        setup("clean")
        put("dir/file.txt", "nested edit\n")
        put("sibling/other.txt", "sibling edit\n")
        steps = []
        for args in (["status", "--short"], ["diff", "--", "."],
                     ["add", ".", "../sibling"], ["diff", "--cached", "--", "."],
                     ["commit", "-m", "nested workflow"]):
            r = run([exe, *args], cwd=REPO / "dir")
            if tag == "git" and r.returncode:
                raise RuntimeError(f"oracle workflow failed: {args}: {r.stderr!r}")
            steps.append([args, r.returncode, r.stdout.hex(), r.stderr.hex(), state(REPO)])
        put("dir/file.txt", "discard me\n")
        for args in (["restore", "--", "."], ["status", "--porcelain"]):
            r = run([exe, *args], cwd=REPO / "dir")
            if tag == "git" and r.returncode:
                raise RuntimeError(f"oracle workflow failed: {args}: {r.stderr!r}")
            steps.append([args, r.returncode, r.stdout.hex(), r.stderr.hex(), state(REPO)])
        (LOG / ("workflow." + tag + ".json")).write_text(json.dumps(steps, indent=2))
        records.append(steps)
    if records[0] != records[1]:
        FAIL += 1
        print("FAIL nested-workflow", flush=True)
    else:
        print("PASS nested-workflow", flush=True)


print(run(["git", "--version"], cwd=ROOT, check=True).stdout.decode().strip(), flush=True)
{"read": read_cases, "write": write_cases, "boundaries": boundaries, "e2e": e2e}[MODE]()
if not COUNT:
    sys.exit("no subdir comparisons matched")
print(f"subdir {MODE}: {COUNT - FAIL}/{COUNT} comparisons passed; {FAIL} failed", flush=True)
sys.exit(bool(FAIL))
