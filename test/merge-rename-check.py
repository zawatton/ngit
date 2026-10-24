"""Compare real merge callers against installed Git, including index stages."""
import os
from pathlib import Path
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
OUT = Path(os.environ.get("NGIT_FIXTURE_DIR", ROOT / "target/rename-lane/fixtures"))
OUT.mkdir(parents=True, exist_ok=True)
ENV = dict(os.environ, GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1",
           GIT_AUTHOR_NAME="Rename Author", GIT_AUTHOR_EMAIL="rename@example.com",
           GIT_COMMITTER_NAME="Rename Author", GIT_COMMITTER_EMAIL="rename@example.com",
           GIT_AUTHOR_DATE="1700800000 +0900", GIT_COMMITTER_DATE="1700800000 +0900",
           GIT_EDITOR="true", GIT_MERGE_AUTOEDIT="no", LC_ALL="C")
BODY = b"one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten\n"


def run(repo, args, ngit=False, check=True):
    exe = [str(ROOT / "bin/ngit")] if ngit else ["git"]
    p = subprocess.run(exe + ["-C", str(repo)] + args, env=ENV,
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=45)
    if check and p.returncode:
        raise AssertionError(f"{args}: {p.returncode}: {p.stderr.decode(errors='replace')[:600]}")
    return p


def put(repo, name, data=BODY):
    p = repo / name
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_bytes(data)


def commit(repo, name):
    run(repo, ["add", "-A"])
    run(repo, ["commit", "-qm", name])


def fixture(name, kind):
    variant = kind
    kind = {"delete-inexact": "delete", "split-content": "split",
            "split-binary": "split", "split-occupied": "split", "split-occupied-both": "split",
            "type-file-link": "exact", "type-link-file": "exact",
            "type-file-link-occupied": "exact",
            "two-source-types-same": "two-source", "two-source-types": "two-source",
            "two-source-type-one": "two-source",
            "recursive": "inexact"}.get(kind, kind)
    body = BODY.replace(b"one", b"one\0") if variant == "split-binary" else BODY
    repo = OUT / (name + "-seed")
    if repo.exists():
        shutil.rmtree(repo)
    repo.mkdir()
    run(repo, ["init", "-q", "-b", "main"])
    for key, val in [("gc.auto", "0"), ("commit.gpgsign", "false"),
                     ("core.autocrlf", "false"), ("merge.stat", "false")]:
        run(repo, ["config", key, val])
    put(repo, "a", body)
    put(repo, "keep", b"keep\n")
    if kind.startswith("two-source"):
        put(repo, "b", BODY if variant == "two-source-types-same" else
            BODY.replace(b"one", b"second source"))
    if kind == "occupied":
        put(repo, "dest", b"occupied\n")
    if kind == "symlink" or variant == "type-link-file":
        (repo / "a").unlink()
        (repo / "a").symlink_to("keep")
    commit(repo, "base")
    run(repo, ["tag", "base"])
    run(repo, ["branch", "side"])
    if kind in ("exact", "inexact", "content", "mode", "same", "split", "delete",
                "two-source", "two-source-modify", "add", "occupied", "symlink"):
        if kind == "occupied":
            (repo / "dest").unlink()
        (repo / "a").rename(repo / "dest")
    if kind == "inexact":
        put(repo, "dest", BODY.replace(b"two", b"ours two"))
    if kind == "content":
        put(repo, "dest", BODY.replace(b"two", b"ours two"))
    if variant in ("delete-inexact", "split-content"):
        put(repo, "dest", BODY.replace(b"two", b"ours two"))
    if variant == "split-binary":
        put(repo, "dest", body.replace(b"two", b"ours two"))
    if variant in ("split-occupied", "split-occupied-both"):
        put(repo, "other", b"precious independent addition\n")
    if kind == "two-source-modify":
        put(repo, "b", BODY.replace(b"one", b"second source").replace(b"two", b"ours two"))
    if variant in ("two-source-types-same", "two-source-types", "two-source-type-one"):
        (repo / "b").unlink()
        (repo / "b").symlink_to("keep")
    put(repo, "ours-only", b"ours\n")
    commit(repo, "ours")
    run(repo, ["switch", "-q", "side"])
    if kind == "delete":
        (repo / "a").unlink()
    elif kind in ("same", "split"):
        (repo / "a").rename(repo / ("dest" if kind == "same" else "other"))
        put(repo, "dest" if kind == "same" else "other",
            body.replace(b"two" if variant == "split-content" else b"nine", b"theirs nine"))
        if variant == "split-occupied-both":
            put(repo, "dest", b"precious opposite addition\n")
    elif kind.startswith("two-source"):
        (repo / "b").rename(repo / "dest")
        if kind == "two-source-modify":
            put(repo, "a", BODY.replace(b"nine", b"theirs nine"))
        if variant in ("two-source-types-same", "two-source-types"):
            (repo / "a").unlink()
            (repo / "a").symlink_to("keep")
    elif kind == "occupied":
        put(repo, "dest", b"changed occupied\n")
    elif kind == "symlink" or variant in ("type-file-link", "type-link-file", "type-file-link-occupied"):
        (repo / "a").unlink()
        if variant == "type-link-file":
            put(repo, "a", b"regular replacement\n")
        else:
            (repo / "a").symlink_to("keep" if variant.startswith("type-file-link") else "other-target")
        if variant == "type-file-link-occupied":
            put(repo, "dest", b"independent destination addition\n")
    else:
        put(repo, "a", BODY.replace(b"two" if kind == "content" else b"nine", b"theirs edit"))
        if kind == "mode":
            (repo / "a").chmod(0o755)
        if kind == "add":
            put(repo, "dest", b"added destination\n")
    put(repo, "theirs-only", b"theirs\n")
    commit(repo, "theirs")
    run(repo, ["switch", "-q", "main"])
    if variant == "recursive":
        a = run(repo, ["rev-parse", "main"]).stdout.decode().strip()
        b = run(repo, ["rev-parse", "side"]).stdout.decode().strip()
        tree = run(repo, ["merge-tree", "--write-tree", "main", "side"]).stdout.decode().splitlines()[0]
        left = run(repo, ["commit-tree", tree, "-p", a, "-p", b, "-m", "left base"]).stdout.decode().strip()
        right = run(repo, ["commit-tree", tree, "-p", b, "-p", a, "-m", "right base"]).stdout.decode().strip()
        run(repo, ["branch", "-f", "side", right])
        run(repo, ["reset", "--hard", left])
        put(repo, "dest", (repo / "dest").read_bytes().replace(b"four", b"ours four"))
        commit(repo, "left tip")
        run(repo, ["switch", "-q", "side"])
        put(repo, "dest", (repo / "dest").read_bytes().replace(b"seven", b"theirs seven"))
        commit(repo, "right tip")
        run(repo, ["switch", "-q", "main"])
        assert len(run(repo, ["merge-base", "--all", "main", "side"]).stdout.splitlines()) == 2
    return repo


def snapshot(repo):
    files = []
    for p in sorted(repo.rglob("*")):
        if ".git" in p.relative_to(repo).parts or p.is_dir():
            continue
        files.append((str(p.relative_to(repo)), os.readlink(p) if p.is_symlink() else p.read_bytes(),
                      "link" if p.is_symlink() else bool(p.stat().st_mode & 0o111)))
    state = {"index": run(repo, ["ls-files", "--stage"]).stdout,
             "files": files, "refs": run(repo, ["show-ref"]).stdout,
             "head": run(repo, ["rev-parse", "HEAD"]).stdout,
             "status": run(repo, ["status", "--porcelain=v1"]).stdout}
    for name in ["MERGE_HEAD", "CHERRY_PICK_HEAD", "REVERT_HEAD", "MERGE_MSG"]:
        p = repo / ".git" / name
        state[name] = p.read_bytes() if p.exists() else None
    # Git's automatic merge tree is a materialized worktree, never index stages.
    state["auto-merge"] = run(repo, ["rev-parse", "--verify", "AUTO_MERGE"], check=False).stdout
    run(repo, ["fsck", "--no-reflogs"])
    return state


def compare(name, kind, action="merge", reverse=False, disabled=False, finish=None):
    seed = fixture(name, kind)
    repos = []
    outputs = []
    for is_ngit in (False, True):
        repo = OUT / (name + ("-ngit" if is_ngit else "-git"))
        if repo.exists():
            shutil.rmtree(repo)
        shutil.copytree(seed, repo, symlinks=True)
        if reverse:
            run(repo, ["switch", "-q", "side"])
        if disabled:
            run(repo, ["config", "merge.renames", "false"])
        if action == "stash":
            run(repo, ["switch", "-q", "side"])
            put(repo, "a", BODY.replace(b"nine", b"stashed edit"))
            run(repo, ["stash", "push", "-qm", "saved"])
            run(repo, ["switch", "-q", "main"])
            args = ["stash", "apply"]
        elif action == "cherry-pick":
            args = ["cherry-pick", "side"]
        elif action == "revert":
            # Reverting a modification must follow a rename introduced later.
            run(repo, ["switch", "-q", "side"])
            run(repo, ["mv", "a", "dest"])
            commit(repo, "later rename")
            args = ["revert", "--no-edit", "side"]
        elif action == "rebase":
            args = ["rebase", "side"]
        else:
            args = ["merge", "--no-edit", "-m", "merge renames", "main" if reverse else "side"]
        p = run(repo, args, is_ngit, check=False)
        (OUT / (name + ("-ngit" if is_ngit else "-git") + ".output")).write_bytes(p.stdout + p.stderr)
        outputs.append((p.returncode, [line for line in p.stdout.splitlines()
                                     if line.startswith((b"Auto-merging ", b"CONFLICT (",
                                                         b"warning: Cannot merge binary files:"))]))
        if not is_ngit:
            expected = 1 if action == "stash" or kind in {
                "content", "delete", "delete-inexact", "split", "split-content",
                "two-source", "two-source-modify", "add", "occupied", "split-binary",
                "split-occupied", "split-occupied-both", "type-file-link", "type-link-file"} else 0
            assert p.returncode == expected, (name, "invalid native fixture", p.returncode, expected)
        if finish:
            if finish == "continue":
                assert p.returncode == 1, (name, "expected conflict", p.returncode)
                put(repo, "dest", b"resolved\n")
                run(repo, ["add", "-A"], is_ngit)
            run(repo, [action, "--" + finish], is_ngit)
        repos.append(repo)
    errors = []
    if outputs[0] != outputs[1]:
        errors.append("exit/diagnostics: " + repr(outputs))
    left, right = map(snapshot, repos)
    for key in left:
        if left[key] != right[key]:
            errors.append(f"{key}: git={left[key]!r} ngit={right[key]!r}"[:1800])
    if errors:
        raise AssertionError("\n".join(errors))


def main():
    group = sys.argv[1]
    cases = {
        "clean": [("exact", "exact"), ("inexact", "inexact"), ("mode", "mode"),
                  ("same-destination", "same"), ("reverse", "inexact", "merge", True),
                  ("disabled", "exact", "merge", False, True), ("symlink", "symlink"),
                  ("two-source-types-same", "two-source-types-same"),
                  ("two-source-types-same-reverse", "two-source-types-same", "merge", True),
                  ("two-source-types", "two-source-types"),
                  ("two-source-type-one", "two-source-type-one"),
                  ("two-source-type-one-reverse", "two-source-type-one", "merge", True),
                  ("type-file-link-occupied", "type-file-link-occupied"),
                  ("type-file-link-occupied-reverse", "type-file-link-occupied", "merge", True)],
        "conflicts": [(kind, kind) for kind in ["content", "delete", "delete-inexact", "split",
                                                "split-content", "two-source", "two-source-modify",
                                                "add", "occupied"]]
                     + [("delete-reverse", "delete", "merge", True),
                        ("split-reverse", "split", "merge", True),
                        ("two-source-reverse", "two-source-modify", "merge", True),
                        ("split-binary", "split-binary"),
                        ("split-binary-reverse", "split-binary", "merge", True),
                        ("split-occupied", "split-occupied"),
                        ("split-occupied-reverse", "split-occupied", "merge", True),
                        ("split-occupied-both", "split-occupied-both"),
                        ("type-file-link", "type-file-link"),
                        ("type-file-link-reverse", "type-file-link", "merge", True),
                        ("type-link-file", "type-link-file"),
                        ("type-link-file-reverse", "type-link-file", "merge", True)],
        "sequencer": [("pick-rename", "inexact", "cherry-pick"),
                      ("revert-rename", "exact", "revert"),
                      ("rebase-rename", "inexact", "rebase"),
                      ("stash-rename", "exact", "stash"),
                      ("recursive-base", "recursive")],
        "e2e": [("complete", "inexact"),
                ("continue", "content", "merge", False, False, "continue"),
                ("abort", "delete", "merge", False, False, "abort")],
    }[group]
    failures = 0
    for args in cases:
        try:
            compare(*args)
            print("PASS", args[0], flush=True)
        except (AssertionError, subprocess.TimeoutExpired) as e:
            failures += 1
            print("FAIL", args[0], str(e), flush=True)
    print(f"{group}: {len(cases) - failures}/{len(cases)} cases passed", flush=True)
    return bool(failures)


if __name__ == "__main__":
    sys.exit(main())
