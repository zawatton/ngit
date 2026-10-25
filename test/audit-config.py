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
import stat
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
COMMAND = shlex.split(os.environ.get("NGIT_COMPAT_COMMAND", str(ROOT / "bin/ngit")))
MATCH = os.environ.get("NGIT_COMPAT_MATCH", "")
REPORT = {"comparisons": 0, "failures": 0, "unexpected_errors": 0, "assertions": 0, "intended": {}, "exact": 0, "cases": [], "command": COMMAND, "match": MATCH}
STARTED = time.monotonic()
DEADLINE = STARTED + 500
BAD = re.compile(rb"uncaught error|invalid-read-syntax|void-function|void-variable|wrong-type-argument|Lisp error|[Ll]oader error|Cannot open load file|[Oo]ut of memory|signal [0-9]|resource exhausted")


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
    REPORT["exact"] += int(equal and not unexpected)
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
        if ".git" not in path.parts or path.name in ("config", "HEAD", "audit-parameters", "audit-child"):
            mode = stat.S_IMODE(path.lstat().st_mode)
            if path.is_symlink():
                value = ["symlink", mode, os.readlink(path)]
            elif path.is_dir():
                value = ["directory", mode]
            elif path.is_file():
                value = ["file", mode, base64.b64encode(path.read_bytes()).decode()]
            else:
                value = ["other", mode]
            files[str(path.relative_to(cwd))] = value
    home = Path(env["HOME"])
    for path in home.rglob("*"):
        if path.is_file():
            files["HOME/" + str(path.relative_to(home))] = base64.b64encode(path.read_bytes()).decode()
    return result, files


def count_reads(repo, env, base):
    "Exercise the production read function against the 273be60 read budget."
    candidate = Path(COMMAND[0]).resolve().parents[1]
    entry = candidate / "lisp/ngit-main.el"
    if not entry.is_file():
        return  # External compatibility commands need not be NeLisp trees.
    source = entry.read_text().replace(
        '(file-name-directory (or load-file-name buffer-file-name))',
        json.dumps(str(candidate / "lisp") + "/"))
    injection = """(let ((original (symbol-function 'ngit--read-file)))
 (fset 'ngit--read-file (lambda (path)
  (when (equal path (getenv "NGIT_COUNT_CONFIG"))
   (ngit-native-write-stderr "AUDIT-CONFIG-READ\\n"))
  (funcall original path))))
"""
    at = source.rindex('(let ((args (cdr (member "--" command-line-args))))')
    driver = base / "count-driver.el"
    driver.write_text(source[:at] + injection + source[at:])
    for name, args, limit in [("reads-config", ["config", "--get", "core.bare"], 2),
                              ("reads-status", ["status", "--porcelain"], 2),
                              ("reads-rev-parse", ["rev-parse", "--is-inside-work-tree"], 0),
                              ("reads-ls-files", ["ls-files", "--stage"], 0),
                              ("reads-branch", ["branch", "--list"], 2),
                              ("reads-stash", ["stash", "list"], 0),
                              ("reads-mv", ["mv", "--dry-run", "integer-object", "moved"], 0)]:
        if MATCH and MATCH not in name:
            continue
        compare(name, args, repo, env)
        case = REPORT["cases"][-1]
        result = run([os.environ.get("NGIT_NELISP", "nelisp"), "--load", str(driver),
                      "--", "--@", *["@" + arg for arg in args]], repo,
                     env | {"NGIT_NO_ELC": "1", "NGIT_COUNT_CONFIG": str(repo / ".git/config")})
        stderr = base64.b64decode(result["stderr_b64"])
        marker = b"AUDIT-CONFIG-READ\n"
        count = stderr.count(marker)
        stripped = dict(result, stderr_b64=base64.b64encode(stderr.replace(marker, b"")).decode())
        error = result["exit"] != 0 or bool(BAD.search(stderr))
        equal = not error and stripped == case["actual"] and count <= limit and (count > 0 or limit == 0)
        REPORT["assertions"] += 1
        REPORT["failures"] += int(not equal)
        REPORT["unexpected_errors"] += int(error)
        case.update(reads=count, baseline_273be60_max=limit, read_probe=result,
                    read_assertion_equal=equal)
        if not equal:
            print(f"FAIL {name}: reads={count}, baseline maximum={limit}: {result}", file=sys.stderr)



def main(mode):
    if mode not in ("core", "e2e"):
        raise RuntimeError("expected core or e2e")
    with tempfile.TemporaryDirectory(prefix="ngit-audit-config-") as tmp:
        base = Path(tmp)
        repo = base / "repo"
        repo.mkdir()
        home = base / "home"
        home.mkdir()
        env = {key: value for key, value in os.environ.items() if not key.startswith("GIT_")}
        env.update(HOME=str(home), XDG_CONFIG_HOME=str(home / "xdg"), LC_ALL="C", TZ="UTC",
                   GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL="/dev/null",
                   GIT_AUTHOR_NAME="Config Fixture", GIT_AUTHOR_EMAIL="fixture@example.test",
                   GIT_COMMITTER_NAME="Config Fixture", GIT_COMMITTER_EMAIL="fixture@example.test",
                   GIT_AUTHOR_DATE="1700000000 +0000", GIT_COMMITTER_DATE="1700000000 +0000")
        setup(["init", "-q", "-b", "main"], repo, env)
        def case(name, args, extra=None, mutate=False):
            compare(name, args, repo, env | (extra or {}), mutate=mutate)
        config = repo / ".git/config"
        original = config.read_text()
        hook = repo / ".git/hooks/pre-commit"
        if mode == "core":
            values = ["0x1", "0X0", "010", "08", "-010", "+0x1", "0x1K", "01m", "1G",
                      "2g", "-2G", "2147483647", "2147483648", "-2147483647", "-2147483648",
                      "999999999999999999999999999999", "1kb", "1 ", " 1", "0x", "true", "OFF", ""]
            for index, value in enumerate(values):
                case(f"environment-base-zero-{index}-{value!r}", ["config", "--get", "core.bare"],
                     {"GIT_CONFIG_NOSYSTEM": value})
            for value in ("0x1", "010", "08", "2147483648", "-2147483648"):
                config.write_text(original + f"[extensions]\nworktreeConfig = {value}\n")
                case(f"extension-base-zero-{value}", ["config", "--get", "core.bare"])
            config.write_text(original)
            for key in ("gc.auto", "gc.autopacklimit"):
                for value in ("0x0", "010", "08", "2147483648", "-2147483648", "1kb", "0x1K", "9223372036854775808oops", "2147483648oops"):
                    case(f"integer-{key}-{value}", ["-c", key + "=" + value, "gc", "--auto"])
                config.write_text(original + "[gc]\n" + key.split(".")[1] + " = 08\n")
                case(f"integer-file-origin-{key}", ["gc", "--auto"])
                config.write_text(original)
            for key in ("status.renameLimit", "diff.renameLimit"):
                for value in ("0x1", "08"):
                    case(f"integer-{key}-{value}", ["-c", key + "=" + value, "status", "--porcelain"])
            (repo / "integer-object").write_text("integer compression fixture\n")
            for command, arguments, keys in [
                ("status", ["--porcelain"], ["core.compression", "core.loosecompression", "pack.compression", "diff.renameLimit", "status.renameLimit"]),
                ("diff", ["--stat"], ["core.compression", "core.loosecompression", "pack.compression", "diff.renameLimit"]),
                ("log", ["-1"], ["core.compression", "core.loosecompression", "pack.compression", "diff.renameLimit"]),
                ("show", ["--stat", "HEAD"], ["core.compression", "core.loosecompression", "pack.compression", "diff.renameLimit"]),
                ("hash-object", ["integer-object"], ["core.compression", "core.loosecompression", "pack.compression"])]:
                for key in keys:
                    case(f"validation-{command}-{key}-08", ["-c", key + "=08", command, *arguments])
            for command, arguments in [("add", ["--dry-run", "integer-object"]),
                                       ("rm", ["--dry-run", "integer-object"]),
                                       ("reset", ["--quiet", "HEAD"]), ("clean", ["--dry-run"]),
                                       ("checkout", ["--", "integer-object"]), ("commit", ["--dry-run"]),
                                       ("restore", ["--", "integer-object"]),
                                       ("mv", ["--dry-run", "integer-object", "renamed"]),
                                       ("stash", ["list"]), ("switch", ["main"])]:
                case(f"validation-default-{command}", ["-c", "core.compression=08", command, *arguments])
            for command, arguments in [("rev-parse", ["--is-inside-work-tree"]),
                                       ("branch", ["--list"]), ("ls-files", ["--stage"]),
                                       ("stash", ["list"]),
                                       ("mv", ["--dry-run", "integer-object", "moved"])]:
                case(f"validation-zero-reader-{command}", ["-c", "core.compression=08", command, *arguments])
            for command, arguments in [("commit", ["--dry-run"]), ("stash", ["list"])]:
                case(f"validation-diff-ui-{command}", ["-c", "diff.renameLimit=08", command, *arguments])
            for command, arguments in [("status", ["--porcelain"]), ("diff", ["--stat"]), ("log", ["-1"])]:
                case(f"validation-ordered-{command}", ["-c", "diff.renameLimit=08", "-c", "diff.renameLimit=5", command, *arguments])
            config.write_text("[broken\n")
            case("validation-count-primary-order", ["status", "--porcelain"], {"GIT_CONFIG_COUNT": "bogus"})
            config.write_text(original)
            case("validation-count-standalone", ["status", "--porcelain"], {"GIT_CONFIG_COUNT": "bogus"})
            config.write_text(original + "[core]\ncompression = 08\n")
            case("validation-priority-file-before-command-diff", ["-c", "diff.renameLimit=08", "diff", "--stat"])
            case("validation-priority-file-before-command-status", ["-c", "pack.compression=08", "status", "--porcelain"])
            config.write_text(original)
            config.write_text(original + "[core]\ncompression = 08\ncompression = 1\n")
            case("validation-file-ordered-origin", ["status", "--porcelain"])
            config.write_text(original)
            case("validation-config-raw-integer", ["-c", "core.compression=08", "config", "--get", "core.compression"])
            case("validation-unconsumed-gc-status", ["-c", "gc.auto=08", "status", "--porcelain"])
            case("validation-final-gc-override", ["-c", "gc.auto=08", "-c", "gc.auto=0", "gc", "--auto"])
            for key in ("core.compression", "core.loosecompression"):
                for value in ("0x1", "010", "08", "1K", "2147483648", "-2147483648", "1kb"):
                    case(f"integer-{key}-{value}", ["-c", key + "=" + value,
                         "hash-object", "-w", "integer-object"], mutate=True)
            setup(["add", "integer-object"], repo, env)
            setup(["commit", "-q", "-m", "read fixture"], repo, env)
            count_reads(repo, env, base)
            for count in ("0x1", "08", "-1", "2147483648"):
                case(f"count-remains-decimal-{count}", ["config", "--get", "core.bare"],
                     {"GIT_CONFIG_COUNT": count})
            hook.write_text('#!/bin/sh\nprintf "params=<%s>\\n" "$GIT_CONFIG_PARAMETERS" >&2\n'
                            'git config --get-all audit.value >&2\nexit 1\n')
            hook.chmod(0o755)
            command = ["commit", "--allow-empty", "-q", "-m", "rejected fixture"]
            for name, options, extra in [
                ("quote-special", ["-c", "audit.value=space ' bang! = x"], {}),
                ("quote-empty-and-bool", ["-c", "audit.value=", "-c", "audit.enabled"], {}),
                ("quote-key-preserved", ["-c", " Audit.value =mixed"], {}),
                ("quote-inherited-modern", ["-c", "audit.value=last"],
                 {"GIT_CONFIG_PARAMETERS": "'audit.value'='inherited'"}),
                ("quote-inherited-legacy", ["-c", "audit.value=last"],
                 {"GIT_CONFIG_PARAMETERS": "'audit.value=legacy'"}),
                ("quote-count-precedence", ["-c", "audit.value=last"],
                 {"GIT_CONFIG_COUNT": "1", "GIT_CONFIG_KEY_0": "audit.value", "GIT_CONFIG_VALUE_0": "count"}),
                ("quote-bare-boolean", ["-c", "audit.value"], {}),
                ("quote-duplicate", ["-c", "audit.value=first", "-c", "audit.value=last"], {})]:
                case(name, options + command, extra, mutate=True)
            if not MATCH or MATCH in "integer-merge.renameLimit-08" or MATCH in "integer-merge.renameLimit-0x1":
                hook.unlink()
                (repo / "merge-base").write_text("rename source\n")
                setup(["add", "merge-base"], repo, env)
                setup(["commit", "-q", "-m", "merge base"], repo, env)
                setup(["checkout", "-q", "-b", "side"], repo, env)
                (repo / "merge-base").rename(repo / "renamed")
                setup(["add", "-A", "--", "merge-base", "renamed"], repo, env)
                setup(["commit", "-q", "-m", "side rename"], repo, env)
                setup(["checkout", "-q", "main"], repo, env)
                (repo / "main-change").write_text("main change\n")
                setup(["add", "main-change"], repo, env)
                setup(["commit", "-q", "-m", "main change"], repo, env)
                for value in ("08", "0x1"):
                    case(f"integer-merge.renameLimit-{value}", ["-c", "merge.renameLimit=" + value,
                         "merge", "--no-ff", "-q", "-m", "merge fixture", "side"], mutate=True)
            if not MATCH or MATCH in "boolean-normalized-log-abbrevCommit" or MATCH in "boolean-normalized-log-showRoot":
                if hook.exists():
                    hook.unlink()
                setup(["commit", "--allow-empty", "-q", "-m", "boolean fixture"], repo, env)
                for key in ("log.abbrevCommit", "log.showRoot"):
                    case("boolean-normalized-" + key.replace(".", "-"),
                         ["-c", key + "=08", "log", "-1"])
            if not MATCH or MATCH in "config-remote-rename-cache-invalidation":
                setup(["remote", "add", "origin", "../remote"], repo, env)
                setup(["config", "branch.main.remote", "origin"], repo, env)
                setup(["config", "branch.main.merge", "refs/heads/main"], repo, env)
                case("config-remote-rename-cache-invalidation", ["remote", "rename", "origin", "upstream"], mutate=True)
        else:
            hook.write_text('#!/bin/sh\nprintf "%s\\n" "$GIT_CONFIG_PARAMETERS" >.git/audit-parameters\n'
                            'git config --get audit.value >.git/audit-child\n')
            hook.chmod(0o755)
            (repo / "payload").write_text("first revision\n")
            case("workflow-config-name", ["config", "user.name", "Configured User"], mutate=True)
            case("workflow-config-email", ["config", "user.email", "configured@example.test"], mutate=True)
            case("workflow-add-first", ["add", "payload"], mutate=True)
            case("workflow-commit-first", ["-c", "audit.value=first ' !", "commit", "-q", "-m", "first"], mutate=True)
            case("workflow-first-inspect", ["log", "-1", "--format=%H %an <%ae> %s"])
            (repo / "payload").write_text("second revision\n")
            case("workflow-add-second", ["add", "payload"], mutate=True)
            case("workflow-commit-inherited", ["-c", "audit.value=second=override", "commit", "-q", "-m", "second"],
                 {"GIT_CONFIG_PARAMETERS": "'audit.value'='inherited'", "GIT_CONFIG_COUNT": "1",
                  "GIT_CONFIG_KEY_0": "audit.value", "GIT_CONFIG_VALUE_0": "count"}, mutate=True)
            case("workflow-history", ["log", "--format=%H %P %s"])
            case("workflow-status", ["status", "--porcelain"])
            case("workflow-command-config-does-not-leak", ["config", "--get", "audit.value"])
            setup(["remote", "add", "origin", "../remote"], repo, env)
            setup(["config", "branch.main.remote", "origin"], repo, env)
            setup(["config", "branch.main.merge", "refs/heads/main"], repo, env)
            case("workflow-remote-rename", ["remote", "rename", "origin", "upstream"], mutate=True)
            case("workflow-remote-fetch-preserved", ["config", "--get", "remote.upstream.fetch"])
            case("workflow-remote-branch-preserved", ["config", "--get", "branch.main.remote"])
    if not REPORT["comparisons"]:
        raise RuntimeError("zero selected comparisons")


MODE = sys.argv[1] if len(sys.argv) > 1 else "core"
try:
    main(MODE)
except Exception as error:
    REPORT["unexpected_errors"] += 1
    print(f"UNEXPECTED: {error}", file=sys.stderr)
finally:
    REPORT["seconds"] = time.monotonic() - STARTED
    report_dir = Path(os.environ.get("NGIT_REPORT_DIR") or str(ROOT / "target/include-pathspec/reports"))
    report_dir.mkdir(parents=True, exist_ok=True)
    (report_dir / f"audit-config-{MODE}.json").write_text(json.dumps(REPORT, indent=2) + "\n")
print(json.dumps({key: value for key, value in REPORT.items() if key != "cases"}))
sys.exit(0 if REPORT["comparisons"] and not REPORT["failures"] and not REPORT["unexpected_errors"] else 1)
