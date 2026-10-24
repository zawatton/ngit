"""Differential directory-rename fixtures against Git's ort implementation."""
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
ENV = dict(os.environ, GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1",
           GIT_AUTHOR_NAME="Directory Author", GIT_AUTHOR_EMAIL="dir@example.com",
           GIT_COMMITTER_NAME="Directory Author", GIT_COMMITTER_EMAIL="dir@example.com",
           GIT_AUTHOR_DATE="1700800000 +0900", GIT_COMMITTER_DATE="1700800000 +0900",
           GIT_EDITOR="true", GIT_MERGE_AUTOEDIT="no", LC_ALL="C")
COMMAND = shlex.split(os.environ.get("NGIT_COMPAT_COMMAND", str(ROOT / "bin/ngit")))
BODY = b"one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten\n"
REPORT = {"comparisons": 0, "failures": 0, "unexpected_errors": 0, "cases": []}


def run(repo, args, native=True, check=True):
    p = subprocess.run((["git"] if native else COMMAND) + ["-C", str(repo)] + args,
                       env=ENV, capture_output=True, timeout=45)
    if check and p.returncode:
        raise RuntimeError((args, p.returncode, p.stderr[:600]))
    return p


def put(repo, path, body=BODY):
    p = repo / path
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_bytes(body)


def commit(repo, message):
    run(repo, ["add", "-A"])
    run(repo, ["commit", "-qm", message])


def fixture(repo, kind, config):
    repo.mkdir()
    run(repo, ["init", "-qb", "main"])
    for key, value in [("gc.auto", "0"), ("commit.gpgsign", "false"),
                       ("merge.stat", "false"), ("merge.directoryRenames", config)]:
        if value is not None:
            run(repo, ["config", key, value])
    if kind == "cycle":
        put(repo, "A/a", b"original A\n")
        put(repo, "B/b", b"original B\n")
        commit(repo, "base")
        run(repo, ["branch", "side"])
        (repo / "A/a").rename(repo / "B/a")
        put(repo, "B/z", b"ours addition\x00\xff\n")
        commit(repo, "ours")
        run(repo, ["switch", "-q", "side"])
        (repo / "B/b").rename(repo / "A/b")
        put(repo, "A/z", b"theirs distinct addition\n")
        commit(repo, "theirs")
        run(repo, ["switch", "-q", "main"])
        return
    old = "old/sub" if kind == "nested" else "old"
    new = "new/sub" if kind == "nested" else "new"
    put(repo, old + "/a")
    put(repo, old + "/b", BODY.replace(b"one", b"second"))
    if kind == "majority":
        put(repo, old + "/c", b"third original\n")
    if kind == "chain":
        put(repo, "new/c", b"third original\n")
        put(repo, "new/d", b"fourth original\n")
    if kind == "multiple":
        put(repo, "second/c", b"third original\n")
        put(repo, "second/d", b"fourth original\n")
    if kind == "split-patch":
        put(repo, "root-source", b"root to nested\n")
        put(repo, "nested/source", b"nested to root\n")
        put(repo, "nested/delete", b"nested deletion\n")
    if kind in ("revert", "revert-split"):
        put(repo, old + "/deleted", b"reverted addition\n")
    if kind in ("rename-into", "rename-into-occupied", "rename-into-edit"):
        put(repo, "external", b"moved into directory\n")
    put(repo, "keep", b"keep\n")
    commit(repo, "base")
    if kind in ("revert", "revert-split"):
        (repo / old / "deleted").unlink()
        commit(repo, "remove file")
        run(repo, ["branch", "deleted"])
    run(repo, ["branch", "side"])
    if kind == "chain":
        (repo / "new").rename(repo / "final")
    (repo / "old").rename(repo / "new")
    if kind == "multiple":
        (repo / "second/c").rename(repo / "new/c")
        (repo / "second/d").rename(repo / "new/d")
    if kind == "partial":
        put(repo, "old/remains", b"directory survives\n")
    if kind in ("occupied", "rename-into-occupied"):
        put(repo, "new/add", b"ours addition\n")
    if kind == "directory-target":
        put(repo, "new/add/sub", b"directory destination\n")
    if kind == "link-target":
        (repo / "new/add").symlink_to("../keep")
    if kind in ("split", "majority", "split-patch", "revert-split"):
        (repo / "other").mkdir()
        (repo / "new/b").rename(repo / "other/b")
    if kind == "source-replacement":
        put(repo, "old/a", b"new source replacement\n")
    if kind == "split-patch":
        (repo / "new/inner").mkdir()
        (repo / "root-source").rename(repo / "new/inner/root-source")
        (repo / "nested/source").rename(repo / "root-target")
        (repo / "nested/delete").unlink()
        put(repo, "nested/add", b"nested addition\n")
        put(repo, "binary-root", b"binary\0\xff\n")
    put(repo, new + "/a", BODY.replace(b"two", b"ours two"))
    if kind == "rename-into-edit":
        put(repo, "external", b"main independently modified\n")
    put(repo, "ours-only", b"ours\n")
    if kind == "root-destination":
        (repo / "new/a").rename(repo / "a")
        (repo / "new/b").rename(repo / "b")
        (repo / "new").rmdir()
    commit(repo, "ours")
    run(repo, ["switch", "-q", "side"])
    put(repo, old + "/a", BODY.replace(b"nine", b"theirs nine"))
    if kind == "divergent":
        (repo / "old").rename(repo / "other")
    put(repo, old + "/add", b"independent addition\n")
    if kind in ("rename-into", "rename-into-occupied", "rename-into-edit"):
        (repo / old / "add").unlink()
        (repo / "external").rename(repo / old / "add")
    if kind == "binary":
        put(repo, old + "/add", b"binary\x00\xff\n")
    if kind == "symlink":
        (repo / old / "add").unlink()
        (repo / old / "add").symlink_to("../keep")
    if kind == "unicode":
        (repo / old / "add").rename(repo / old / "日本語")
    if kind == "chain":
        put(repo, "new/extra", b"chain addition\n")
    if kind == "multiple":
        put(repo, "second/add", b"other source addition\n")
    if kind == "gitlink":
        (repo / old / "add").unlink()
        (repo / old / "add").mkdir()
        head = run(repo, ["rev-parse", "HEAD"]).stdout.decode().strip()
        run(repo, ["update-index", "--add", "--cacheinfo", "160000," + head + "," + old + "/add"])
    if kind == "same-side-occupied":
        put(repo, "new/add", b"theirs other addition\n")
    put(repo, "theirs-only", b"theirs\n")
    commit(repo, "theirs")
    run(repo, ["switch", "-q", "main"])


def snapshot(repo):
    files = []
    for p in sorted(repo.rglob("*")):
        rel = p.relative_to(repo)
        if ".git" in rel.parts or (p.is_dir() and not p.is_symlink()):
            continue
        body = os.fsencode(os.readlink(p)) if p.is_symlink() else p.read_bytes()
        files.append((str(rel), "link" if p.is_symlink() else p.stat().st_mode & 0o111,
                      base64.b64encode(body).decode()))
    state = {name: base64.b64encode(run(repo, args, check=False).stdout).decode()
             for name, args in [("index", ["ls-files", "--stage"]),
                                ("refs", ["show-ref"]), ("head", ["rev-parse", "HEAD"]),
                                ("tree", ["rev-parse", "HEAD^{tree}"]),
                                ("status", ["status", "--porcelain=v1"]),
                                ("reflogs", ["reflog", "show", "--all", "--format=%H %gs"]),
                                ("auto-merge", ["rev-parse", "--verify", "AUTO_MERGE"]) ]}
    state["files"] = files
    for name in ["MERGE_HEAD", "MERGE_MSG", "MERGE_MODE", "ORIG_HEAD",
                 "CHERRY_PICK_HEAD", "REVERT_HEAD", "SQUASH_MSG"]:
        p = repo / ".git" / name
        state[name] = base64.b64encode(p.read_bytes()).decode() if p.exists() else None
    for directory in ("sequencer", "rebase-merge", "rebase-apply"):
        directory_path = repo / ".git" / directory
        state[directory] = {str(p.relative_to(directory_path)): base64.b64encode(p.read_bytes()).decode()
                            for p in sorted(directory_path.rglob("*")) if p.is_file()}
    fsck = run(repo, ["fsck", "--full", "--no-reflogs"], check=False)
    if fsck.returncode:
        raise RuntimeError(("fsck", fsck.returncode, fsck.stderr))
    return state


def command_tuple(p):
    return {"exit": p.returncode, "stdout": base64.b64encode(p.stdout).decode(),
            "stderr": base64.b64encode(p.stderr).decode()}


def compare(parent, name, kind="basic", config="true", action="merge", finish=None):
    seed = parent / (name + "-seed")
    fixture(seed, kind, config)
    outputs, states = [], []
    for native in (True, False):
        repo = parent / (name + ("-git" if native else "-ngit"))
        shutil.copytree(seed, repo, symlinks=True)
        if action == "reverse":
            run(repo, ["switch", "-q", "side"])
        if action == "untracked":
            put(repo, "new/add", b"precious untracked\n")
        if action == "dirty":
            put(repo, "new/a", b"precious modified\n")
        args = (["merge", "--no-edit", "-m", "directory merge", "main" if action == "reverse" else "side"]
                if action in ("merge", "reverse", "dirty", "untracked") else
                ["cherry-pick", "side"] if action == "cherry-pick" else ["rebase", "side"])
        if action == "stash":
            run(repo, ["switch", "-q", "side"])
            put(repo, "old/stashed", b"stashed addition\n")
            run(repo, ["add", "old/stashed"])
            run(repo, ["stash", "push", "-qm", "directory stash"])
            if not run(repo, ["rev-parse", "--verify", "refs/stash"]).stdout:
                raise RuntimeError("stash fixture did not create a stash")
            run(repo, ["switch", "-q", "main"])
            args = ["stash", "apply"]
        if action == "revert":
            args = ["revert", "--no-edit", "deleted"]
        p = run(repo, args, native, check=False)
        if native and action == "stash" and p.returncode != (1 if kind == "split" else 0):
            raise RuntimeError(("invalid stash oracle fixture", name, p.returncode, p.stderr))
        if not native and (p.returncode >= 126 or b"Lisp error" in p.stderr or b"void-function" in p.stderr):
            raise RuntimeError(("runtime failure", p.returncode, p.stderr[:600]))
        outputs.append(command_tuple(p))
        states.append(snapshot(repo))
        if native and kind == "cycle":
            expected = {"A/b": b"original B\n", "A/z": b"theirs distinct addition\n",
                        "B/a": b"original A\n", "B/z": b"ours addition\x00\xff\n"}
            actual = {path: base64.b64decode(body) for path, mode, body in states[-1]["files"]}
            if p.returncode or actual != expected or any(not line.split(b"\t")[0].endswith(b" 0")
                    for line in run(repo, ["ls-files", "--stage"]).stdout.splitlines()):
                raise RuntimeError("invalid cycle oracle: expected four intact stage-zero files")
        if finish:
            if finish == "continue":
                if p.returncode != 1:
                    raise RuntimeError((name, "expected conflict before continue", p.returncode))
                run(repo, ["add", "-A"], native)
            control = run(repo, [action, "--" + finish], native, check=False)
            outputs[-1][finish] = command_tuple(control)
            states[-1][finish] = snapshot(repo)
    errors = []
    if outputs[0] != outputs[1]:
        errors.append("command tuple")
    errors += ["state:" + key for key in states[0] if states[0][key] != states[1][key]]
    case = {"name": name, "args": args, "git": outputs[0], "ngit": outputs[1],
            "errors": errors, "git_state": states[0], "ngit_state": states[1]}
    REPORT["cases"].append(case)
    REPORT["comparisons"] += 1
    REPORT["failures"] += bool(errors)
    print(("FAIL" if errors else "PASS"), name, ", ".join(errors), flush=True)


def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else "clean"
    parent = Path(tempfile.mkdtemp(prefix="ngit-directory-"))
    try:
        cases = {"clean": [("basic", "basic", "true"), ("partial", "partial", "true"),
                           ("nested", "nested", "true"), ("chain", "chain", "true"),
                           ("majority", "majority", "true"), ("binary", "binary", "true"),
                           ("symlink", "symlink", "true"), ("unicode", "unicode", "true"),
                           ("gitlink", "gitlink", "true"),
                           ("rename-into", "rename-into", "true"),
                           ("rename-into-edit", "rename-into-edit", "true"),
                           ("root-destination", "root-destination", "true"),
                           ("source-replacement", "source-replacement", "true"),
                           ("reverse", "basic", "true", "reverse"),
                           ("disabled", "basic", "false"),
                           ("cycle-true", "cycle", "true"),
                           ("cycle-conflict", "cycle", "conflict"),
                           ("cycle-default", "cycle", None)],
                 "conflicts": [("suggested", "basic", "conflict"),
                               ("default", "basic", None),
                               ("occupied", "occupied", "true"),
                               ("rename-into-occupied", "rename-into-occupied", "true"),
                               ("rename-into-suggest", "rename-into", "conflict"),
                               ("rename-into-edit-suggest", "rename-into-edit", "conflict"),
                               ("same-side-occupied", "same-side-occupied", "true"),
                               ("split", "split", "true"), ("divergent", "divergent", "true"),
                               ("multiple", "multiple", "true"),
                               ("directory-target", "directory-target", "true"),
                               ("link-target", "link-target", "true")],
                 "e2e": [("continue", "basic", "conflict", "merge", "continue"),
                         ("abort", "basic", "conflict", "merge", "abort"),
                         ("split-continue", "split", "true", "merge", "continue"),
                         ("split-abort", "split", "true", "merge", "abort"),
                         ("dirty", "basic", "true", "dirty"),
                         ("untracked", "basic", "true", "untracked"),
                         ("pick", "basic", "true", "cherry-pick"),
                         ("pick-split-abort", "split", "true", "cherry-pick", "abort"),
                         ("pick-split-continue", "split", "true", "cherry-pick", "continue"),
                         ("rebase", "basic", "true", "rebase"),
                         ("rebase-split-abort", "split", "true", "rebase", "abort"),
                         ("rebase-split-continue", "split", "true", "rebase", "continue"),
                         ("rebase-patch", "split-patch", "true", "rebase", "abort"),
                         ("revert", "revert", "true", "revert"),
                         ("revert-abort", "revert", "conflict", "revert", "abort"),
                         ("revert-continue", "revert", "conflict", "revert", "continue"),
                         ("revert-split-abort", "revert-split", "true", "revert", "abort"),
                         ("revert-split-continue", "revert-split", "true", "revert", "continue"),
                         ("stash", "basic", "true", "stash"),
                         ("stash-split", "split", "true", "stash")]}[mode]
        match = os.environ.get("NGIT_COMPAT_MATCH")
        if match:
            cases = [case for case in cases if case[0] == match]
        for case in cases:
            try:
                compare(parent, *case)
            except (RuntimeError, subprocess.TimeoutExpired, OSError) as exc:
                REPORT["unexpected_errors"] += 1
                REPORT["cases"].append({"name": case[0], "unexpected_error": str(exc)})
                print("ERROR", case[0], str(exc)[:700], flush=True)
    finally:
        destination = os.environ.get("NGIT_COMPAT_REPORT")
        if destination:
            Path(destination).write_text(json.dumps(REPORT, indent=2) + "\n")
        if not os.environ.get("NGIT_KEEP_FIXTURES"):
            shutil.rmtree(parent)
        else:
            print("fixtures:", parent)
    print(mode, REPORT["comparisons"], "comparisons", REPORT["failures"], "failures",
          REPORT["unexpected_errors"], "unexpected errors")
    return bool(REPORT["failures"] or REPORT["unexpected_errors"] or not REPORT["comparisons"])


if __name__ == "__main__":
    sys.exit(main())
