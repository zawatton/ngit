#!/usr/bin/env python3
"""Compare complete native/NeLisp command tuples for configuration includes."""
import base64
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
COMMAND = shlex.split(os.environ.get("NGIT_COMPAT_COMMAND", str(ROOT / "bin/ngit")))
MATCH = os.environ.get("NGIT_COMPAT_MATCH", "")
REPORT = {"comparisons": 0, "failures": 0, "unexpected_errors": 0, "assertions": 0, "cases": []}
DEADLINE = time.monotonic() + 540
BAD = re.compile(rb"void-function|void-variable|wrong-type-argument|Lisp error|[Ll]oader error|Cannot open load file|[Oo]ut of memory|signal [0-9]|resource exhausted")


def run(command, cwd, env):
    budget = min(60, DEADLINE - time.monotonic())
    if budget <= 0:
        raise RuntimeError("overall timeout")
    process = subprocess.Popen(command, cwd=cwd, env=env, stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, start_new_session=True)
    try:
        stdout, stderr = process.communicate(timeout=budget)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGKILL)
        process.communicate()
        raise RuntimeError("command timeout")
    return {"exit": process.returncode,
            "stdout_b64": base64.b64encode(stdout).decode(),
            "stderr_b64": base64.b64encode(stderr).decode()}


def setup(command, cwd, env):
    result = run(["git", *command], cwd, env)
    if result["exit"]:
        raise RuntimeError(f"fixture failed: {command}: {result}")


def compare(name, args, cwd, env, mutate=False):
    if MATCH and MATCH not in name:
        return
    backup = cwd.parent / (cwd.name + ".snapshot")
    home = Path(env["HOME"])
    home_backup = cwd.parent / "home.snapshot"
    if mutate:
        shutil.copytree(cwd, backup, symlinks=True)
        shutil.copytree(home, home_backup, symlinks=True)
    expected = run(["git", *args], cwd, env)
    expected_state = state(cwd, env) if mutate else None
    if mutate:
        shutil.rmtree(cwd)
        shutil.copytree(backup, cwd, symlinks=True)
        shutil.rmtree(backup)
        shutil.rmtree(home)
        shutil.copytree(home_backup, home, symlinks=True)
        shutil.rmtree(home_backup)
    actual = run([*COMMAND, *args], cwd, env)
    unexpected = actual["exit"] < 0 or BAD.search(base64.b64decode(actual["stderr_b64"]) + base64.b64decode(actual["stdout_b64"])) is not None
    equal = expected == actual
    if mutate:
        REPORT["assertions"] += 1
        equal = equal and expected_state == state(cwd, env)
    REPORT["comparisons"] += 1
    REPORT["unexpected_errors"] += int(unexpected)
    REPORT["failures"] += int(not equal or unexpected)
    REPORT["cases"].append(dict(name=name, argv=args, expected=expected, actual=actual,
                                state_equal=(expected_state == state(cwd, env)) if mutate else None))
    if not equal or unexpected:
        print(f"FAIL {name}: {expected} != {actual}", file=sys.stderr)


def state(cwd, env):
    """Compare logical commit/ref/index state and exact primary/include bytes."""
    commands = [["show-ref", "--head"], ["ls-files", "--stage"],
                ["log", "--all", "--format=%H %P %an <%ae> %cn <%ce> %s"],
                ["reflog", "show", "--all", "--format=%H %gs"], ["fsck", "--no-reflogs"]]
    result = [run(["git", *args], cwd, env) for args in commands]
    files = {}
    for path in cwd.rglob("*"):
        if path.is_file() and (".git" not in path.parts or path.name in ("config", "HEAD")):
            files[str(path.relative_to(cwd))] = base64.b64encode(path.read_bytes()).decode()
    home = Path(env["HOME"])
    for path in home.rglob("*"):
        if path.is_file():
            files["HOME/" + str(path.relative_to(home))] = base64.b64encode(path.read_bytes()).decode()
    return result, files


def main(mode):
    if mode not in ("read", "conditional", "boundaries", "e2e"):
        raise RuntimeError("expected read, conditional, boundaries or e2e")
    with tempfile.TemporaryDirectory(prefix="ngit-config-includes-") as tmp:
        base = Path(tmp)
        repo = base / "repo"
        repo.mkdir()
        home = base / "home"
        home.mkdir()
        env = dict(os.environ)
        for key in list(env):
            if key.startswith("GIT_"):
                del env[key]
        env.update(HOME=str(home), XDG_CONFIG_HOME=str(home / "xdg"), LC_ALL="C", TZ="UTC",
                   GIT_CONFIG_NOSYSTEM="1", GIT_AUTHOR_DATE="1700000000 +0000",
                   GIT_COMMITTER_DATE="1700000000 +0000")
        setup(["init", "-q", "-b", "main"], repo, env)
        probe = run([*COMMAND, "config", "--get", "core.repositoryformatversion"], repo, env)
        REPORT["runtime_probe"] = probe
        if probe != {"exit": 0, "stdout_b64": "MAo=", "stderr_b64": ""}:
            raise RuntimeError(f"runtime preflight failed: {probe}")
        config = repo / ".git/config"
        original = config.read_text()
        (repo / "inc").write_text('[demo]\nvalue = included\nmulti = middle\n[include]\npath = nested\n')
        (repo / "nested").write_text('[demo]\nnested = deep\nmulti = nested\n')
        (home / "tilde").write_text('[demo]\ntilde = home\n')
        (home / ".gitconfig").write_text('[demo]\nprecedence = global\n[include]\npath = global.inc\n')
        (home / "global.inc").write_text('[demo]\nglobal = included-global\n')
        xdg = home / "xdg/git"
        xdg.mkdir(parents=True)
        (xdg / "config").write_text('[include]\npath = xdg.inc\n[demo]\nprecedence = xdg\n')
        (xdg / "xdg.inc").write_text('[demo]\nxdg = included-xdg\n')
        config.write_text(original + '[demo]\nvalue = before\nmulti = first\nprecedence = local\n'
                          '[include]\npath = ../inc\npath = ~/tilde\npath = ../missing\n'
                          '[demo]\nvalue = after\nmulti = last\n')
        def case(name, args, **kwargs):
            compare(name, args, repo, env, **kwargs)
        if mode == "read":
            for name, args in [
                ("config-include-source-old-red", ["--get", "demo.nested"]),
                ("normal-last", ["--get", "demo.value"]),
                ("normal-multivalue", ["--get-all", "demo.multi"]),
                ("normal-list", ["--list"]),
                ("tilde", ["--get", "demo.tilde"]),
                ("global-include", ["--get", "demo.global"]),
                ("xdg-include", ["--get", "demo.xdg"]),
                ("precedence", ["--get", "demo.precedence"]),
                ("no-includes", ["--no-includes", "--get", "demo.nested"]),
                ("local-default", ["--local", "--get", "demo.nested"]),
                ("local-includes", ["--local", "--includes", "--get", "demo.nested"]),
                ("global-default", ["--global", "--get", "demo.global"]),
                ("global-explicit", ["--global", "--includes", "--get", "demo.global"]),
                ("file-default", ["--file", ".git/config", "--get", "demo.nested"]),
                ("file-includes", ["--file", ".git/config", "--includes", "--get-all", "demo.multi"])]:
                case(name, ["config", *args])
        elif mode == "conditional":
            (repo / "conditional").write_text('[demo]\nconditional = yes\n')
            for name, condition in [
                ("gitdir-absolute", f"gitdir:{repo}/.git"),
                ("gitdir-relative", "gitdir:./"),
                ("gitdir-prefix", f"gitdir:{base}/"),
                ("gitdir-implicit-glob", "gitdir:repo/.git"),
                ("gitdir-insensitive", f"gitdir/i:{str(repo).upper()}/.GIT"),
                ("gitdir-miss", "gitdir:/nonexistent/**"),
                ("branch-main", "onbranch:main"),
                ("branch-miss", "onbranch:other"),
                ("branch-glob", "onbranch:ma*"),
                ("unknown-condition", "unknown:main"),
                ("unmatched-hasconfig", "hasconfig:remote.*.url:https://invalid.test/**")]:
                config.write_text(original + f'[includeIf "{condition}"]\npath = ../conditional\n')
                case(name, ["config", "--get", "demo.conditional"])
            config.write_text(original + '[includeIf "onbranch:main"]\npath = ../conditional\n')
            sub = repo / "nested-dir"
            sub.mkdir()
            compare("nested-invocation", ["config", "--get", "demo.conditional"], sub, env)
            link = base / "alias"
            link.symlink_to(repo, target_is_directory=True)
            compare("symlink-invocation", ["config", "--get", "demo.conditional"], link, env)
            (repo / ".git/HEAD").write_text("ref: refs/heads/release/topic\n")
            config.write_text(original + '[includeIf "onbranch:release/"]\npath = ../conditional\n')
            case("branch-trailing-directory", ["config", "--get", "demo.conditional"])
            (repo / ".git/HEAD").write_text("0" * 40 + "\n")
            case("detached-branch-ignored", ["config", "--get", "demo.conditional"])
            (repo / ".git/HEAD").write_text("ref: refs/heads/main\n")
            config.write_text(original)
            setup(["-c", "user.name=Fixture", "-c", "user.email=fixture@example.test",
                   "commit", "--allow-empty", "-qm", "worktree fixture"], repo, env)
            linked = base / "linked"
            setup(["worktree", "add", "-q", "-b", "linked/topic", str(linked)], repo, env)
            (repo / ".git/worktree.inc").write_text('[demo]\nworktree = matched\n')
            linked_gitdir = repo / ".git/worktrees/linked"
            for name, condition in [("worktree-gitdir", f"gitdir:{linked_gitdir}"),
                                    ("worktree-branch", "onbranch:linked/")]:
                config.write_text(original + f'[includeIf "{condition}"]\npath = worktree.inc\n')
                compare(name, ["config", "--get", "demo.worktree"], linked, env)
        elif mode == "boundaries":
            case("write-primary-only", ["config", "--includes", "demo.nested", "primary"], mutate=True)
            case("write-included-file-unchanged", ["config", "--get", "demo.nested"])
            case("unset-primary-only", ["config", "--includes", "--unset", "demo.nested"], mutate=True)
            case("included-value-restored", ["config", "--get", "demo.nested"])
            for name, contents in [
                ("invalid-section", "[broken\n"),
                ("invalid-escape", "[demo]\nvalue = bad\\q\n"),
                ("invalid-quote", '[demo]\nvalue = "bad\n'),
                ("include-cycle", "[include]\npath = inc\n")]:
                (repo / "inc").write_text(contents)
                case(name, ["config", "--get", "demo.value"])
            (repo / "inc").unlink()
            case("missing-include-ignored", ["config", "--get", "demo.value"])
            (repo / "inc").write_text('[include]\npath\n')
            case("missing-include-value", ["config", "--get", "demo.value"])
            case("disabled-invalid-include", ["config", "--no-includes", "--get", "demo.value"])
            config.write_text(original)
            standalone = repo / "standalone.conf"
            standalone.write_text('[include]\npath = inc\n[broken-parent\n')
            (repo / "inc").write_text('[broken-child\n')
            case("repair-streaming-first-error", ["config", "--file", str(standalone), "--includes", "--get", "demo.value"])
            config.write_text(original + '[demo]\nvalue = first\\\n  second\\q\n')
            case("repair-physical-continuation-line", ["config", "--get", "demo.value"])
            config.write_text(original + '[demo]\nvalue = first\\q\\\n  second\n')
            case("repair-physical-error-before-continuation", ["config", "--get", "demo.value"])
            config.write_text(original + '[demo]\nvalue = first\\\n  second\n')
            case("repair-valid-continuation", ["config", "--get", "demo.value"])
            config.write_text(original + '[include]\npath = ../inc\n')
            case("repair-explicit-absolute-file-diagnostic", ["config", "--file", str(config), "--includes", "--get", "demo.value"])
            config.write_text(original)
            (home / ".gitconfig").unlink()
            case("repair-xdg-global-read", ["config", "--global", "--get", "demo.precedence"])
            case("repair-xdg-global-default-includes", ["config", "--global", "--get", "demo.xdg"])
            case("repair-xdg-global-includes", ["config", "--global", "--includes", "--get", "demo.xdg"])
            case("repair-xdg-global-write", ["config", "--global", "demo.precedence", "written-xdg"], mutate=True)
            case("startup-missing-selected-list", ["config", "--file", "missing.conf", "--list"])
            case("startup-missing-selected-get", ["config", "--file", "missing.conf", "--get", "demo.value"])
            denied = repo / "denied.inc"
            denied.write_text('[demo]\nvalue = denied\n')
            denied.chmod(0)
            try:
                case("startup-denied-selected-get", ["config", "--file", "denied.inc", "--get", "demo.value"])
                case("startup-denied-selected-list", ["config", "--file", "denied.inc", "--list"])
                config.write_text(original + '[include]\npath = ../denied.inc\n')
                case("startup-denied-include", ["config", "--get", "demo.value"])
            finally:
                denied.chmod(0o600)
            (repo / "inc").write_text('[broken-child\n')
            config.write_text(original + '[include]\npath = ../inc\n[broken-parent\n')
            case("startup-parent-syntax-priority", ["config", "--get", "demo.value"])
            config.write_text(original)
            denied_dir = repo / "denied-lock-dir"
            denied_dir.mkdir()
            denied_primary = denied_dir / "config"
            denied_primary.write_text('[demo]\nvalue = initial\n')
            denied_dir.chmod(0o500)
            try:
                case("lock-create-permission-denied", ["config", "--file", "denied-lock-dir/config", "demo.value", "updated"])
                if not MATCH or MATCH in "lock-create-permission-denied":
                    REPORT["assertions"] += 1
                    if denied_primary.read_text() != '[demo]\nvalue = initial\n' or (denied_dir / "config.lock").exists():
                        raise RuntimeError("failed lock creation changed primary or leaked a lock")
            finally:
                denied_dir.chmod(0o700)
            locked = repo / "locked.conf"
            locked.write_text('[demo]\nvalue = initial\n')
            foreign_lock = repo / "locked.conf.lock"
            foreign_lock.write_bytes(b"foreign lock must survive\n")
            case("lock-existing-foreign-owner", ["config", "--file", "locked.conf", "demo.value", "updated"])
            if not MATCH or MATCH in "lock-existing-foreign-owner":
                REPORT["assertions"] += 1
                if locked.read_text() != '[demo]\nvalue = initial\n' or foreign_lock.read_bytes() != b"foreign lock must survive\n":
                    raise RuntimeError("failed config write changed primary or foreign lock")
            foreign_lock.unlink()
            sentinel = repo / "lock-sentinel"
            sentinel.write_bytes(b"symlink sentinel must survive\n")
            foreign_lock.symlink_to(sentinel.name)
            case("lock-existing-symlink-owner", ["config", "--file", "locked.conf", "demo.value", "updated"])
            if not MATCH or MATCH in "lock-existing-symlink-owner":
                REPORT["assertions"] += 1
                if not foreign_lock.is_symlink() or sentinel.read_bytes() != b"symlink sentinel must survive\n" or locked.read_text() != '[demo]\nvalue = initial\n':
                    raise RuntimeError("failed config write changed foreign lock symlink or sentinel")
        else:
            config.write_text(original + '[include]\npath = ../identity\n'
                              '[includeIf "onbranch:main"]\npath = ../branch-policy\n')
            (repo / "identity").write_text('[user]\nname = Included User\nemail = included@example.test\n')
            (repo / "branch-policy").write_text('[user]\nemail = branch@example.test\n[demo]\npolicy = enabled\n')
            (repo / "payload").write_text("nonempty commit payload\n")
            case("e2e-included-identity", ["config", "--get", "user.name"])
            case("e2e-included-branch", ["config", "--get", "demo.policy"])
            case("e2e-add", ["add", "payload"], mutate=True)
            case("e2e-commit", ["commit", "-q", "-m", "Included identity commit"], mutate=True)
            case("e2e-inspect-commit", ["show", "-s", "--format=%an <%ae>%n%B", "HEAD"])
            case("e2e-inspect-index", ["ls-files", "--stage"])
            case("e2e-inspect-status", ["status", "--porcelain"])
    if not REPORT["comparisons"]:
        raise RuntimeError("zero selected comparisons")


try:
    main(sys.argv[1] if len(sys.argv) > 1 else "read")
except Exception as error:
    REPORT["unexpected_errors"] += 1
    print(f"UNEXPECTED: {error}", file=sys.stderr)
finally:
    if os.environ.get("NGIT_COMPAT_REPORT"):
        Path(os.environ["NGIT_COMPAT_REPORT"]).write_text(json.dumps(REPORT, indent=2) + "\n")
print(json.dumps({key: value for key, value in REPORT.items() if key != "cases"}))
sys.exit(bool(REPORT["failures"] or REPORT["unexpected_errors"]))
