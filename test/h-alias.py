#!/usr/bin/env python3
"""Differential coverage for Git alias tokenization and dispatch."""
import base64
import json
import os
import pty
from pathlib import Path
import re
import resource
import select
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
import time
import tty

ROOT = Path(__file__).resolve().parents[1]
REPORT = Path(os.environ.get("NGIT_REPORT_DIR", ROOT / "target/h-alias"))
BAD = re.compile(rb"uncaught error|void-function|void-variable|wrong-type-argument|Lisp error|invalid-read-syntax|[Oo]ut of memory|resource exhausted|[Ll]oader error")
ENV = dict(os.environ)
for key in list(ENV):
    if key.startswith(("GIT_", "NGIT_")) and key not in ("NGIT_NELISP", "NGIT_NELISP_ROOT"):
        ENV.pop(key)
ENV.update(LC_ALL="C", TZ="UTC", GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL="/dev/null",
           GIT_AUTHOR_NAME="Alias User", GIT_AUTHOR_EMAIL="alias@example.invalid",
           GIT_COMMITTER_NAME="Alias User", GIT_COMMITTER_EMAIL="alias@example.invalid",
           GIT_AUTHOR_DATE="2000-01-01T00:00:00 +0000", GIT_COMMITTER_DATE="2000-01-01T00:00:00 +0000",
           GIT_TERMINAL_PROMPT="0", GIT_PAGER="cat")


def run(command, cwd):
    result = subprocess.run(command, cwd=cwd, env=ENV, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, input=b"alias stdin\n", timeout=45)
    if result.returncode < 0 or BAD.search(result.stdout + result.stderr):
        raise RuntimeError("unexpected process failure: " + repr(command) + ": " + repr(result.stderr[:300]))
    return (result.returncode, result.stdout, result.stderr)


def git(args, cwd):
    result = run(["git", *args], cwd)
    if result[0]:
        raise RuntimeError("fixture Git failed: " + repr(result))
    return result[1]


def state(cwd):
    files = {str(p.relative_to(cwd)): base64.b64encode(p.read_bytes()).decode()
             for p in cwd.rglob("*") if p.is_file() and ".git" not in p.relative_to(cwd).parts}
    return [git(["status", "--porcelain=v1", "--untracked-files=all"], cwd),
            git(["show-ref", "--head"], cwd), git(["ls-files", "--stage"], cwd),
            git(["log", "--format=%H%n%B", "--all"], cwd),
            git(["config", "--local", "--list"], cwd), files]


def fixture(cwd):
    cwd.mkdir()
    git(["init", "-q", "-b", "main"], cwd)
    git(["config", "core.autocrlf", "false"], cwd)
    (cwd / "tracked").write_text("initial\n")
    git(["add", "tracked"], cwd)
    git(["commit", "-q", "-m", "initial"], cwd)
    (cwd / "tracked").write_text("changed\n")
    (cwd / "space name").write_text("new\n")
    (cwd / "other").write_text("other\n")


def alias(name, value, *args):
    return ["-c", "alias." + name + "=" + value, name, *args]


CASES = [
    ("short", [alias("st", "status --short")]),
    ("append", [alias("st", "status --short --", "space name")]),
    ("single-quote", [alias("st", "status --short -- 'space name'")]),
    ("double-quote", [alias("st", 'status --short -- "space name"')]),
    ("escape-space", [alias("st", "status --short -- space\\ name")]),
    ("tabs", [alias("st", "status\t--short\t--\ttracked")]),
    ("newlines", [alias("st", "status\n--short\r\n--\ttracked")]),
    ("vertical-space", [alias("st", "status\v--short\f--\ttracked")]),
    ("trailing-space", [alias("st", "status --short ")]),
    ("leading-space", [alias("st", " status --short")]),
    ("empty", [alias("st", "")]),
    ("spaces-only", [alias("st", " \t ")]),
    ("empty-command", [alias("st", "''")]),
    ("empty-argument", [alias("r", "config aliasprobe.value ''")]),
    ("quote-concatenation", [alias("r", 'config aliasprobe.value ab"cd"\'ef\'')]),
    ("quoted-literals", [alias("r", "config aliasprobe.value '$HOME;*'")]),
    ("append-literal", [alias("r", "config aliasprobe.value", "a b")]),
    ("append-empty", [alias("r", "config aliasprobe.value", "")]),
    ("single-backslash", [alias("r", "config aliasprobe.value 'a\\b'")]),
    ("double-backslash", [alias("r", 'config aliasprobe.value "a\\b"')]),
    ("escaped-quote", [alias("r", 'config aliasprobe.value "a\\\"b"')]),
    ("escaped-newline", [alias("r", "config aliasprobe.value a\\\nb")]),
    ("bad-ending", [alias("st", "status\\")]),
    ("bad-single", [alias("st", "status '")]),
    ("bad-double", [alias("st", 'status "')]),
    ("self-recursion", [alias("st", "st --short")]),
    ("nested", [["-c", "alias.first=second --short", "-c", "alias.second=status", "first", "--", "tracked"]]),
    ("loop", [["-c", "alias.first=second", "-c", "alias.second=first", "first"]]),
    ("builtin-wins", [["-c", "alias.status=log", "status", "--short"]]),
    ("lazy-log", [alias("l", "log -1 --format=%s")]),
    ("lazy-config", [alias("c", "config --get core.autocrlf")]),
    ("pager-option", [alias("st", "--no-pager status --short")]),
    ("paginate-option", [alias("st", "-p status --short")]),
    ("unknown-option", [alias("st", "--alias-invalid status --short")]),
    ("env-option", [alias("st", "-C . status --short")]),
    ("config-option", [alias("st", "-c status.showUntrackedFiles=no status --short")]),
    ("outer-config", [["-c", "status.showUntrackedFiles=no", *alias("st", "status --short")]]),
    ("shell", [alias("s", "!printf 'shell\\n'")]),
    ("shell-arguments", [alias("s", '!f() { printf "<%s>\\n" "$@"; }; f', "a b", "")]),
    ("shell-exit", [alias("s", "!exit 7")]),
    ("shell-stdin", [alias("s", "!cat")]),
    ("shell-cwd", [alias("s", '!printf "%s\\n" "$GIT_PREFIX"')]),
]
E2E = ("session", [alias("a", "add --", "space name", "tracked"),
                    alias("c", "commit -q -m 'alias commit'"),
                    alias("s", "status --short"), alias("l", "log -2 --format=%s")])
DEFECTS = [case for case in CASES if case[0] in ("short", "append", "single-quote", "lazy-log")]
PAGER_CASES = [
    ("pager-local-short", alias("st", "-p status --short")),
    ("pager-local-long", alias("st", "--paginate status --short")),
    ("pager-chain", ["-c", "alias.first=-p second --short", "-c", "alias.second=status", "first"]),
    ("pager-config-chain", ["-c", "alias.first=-p -c status.showUntrackedFiles=no second --short",
                            "-c", "alias.second=status", "first"]),
    ("pager-shell-chain", ["-c", "alias.first=--paginate second", "-c", "alias.second=!printf 'shell pager\\n'", "first"]),
    ("pager-no-pager-fatal", alias("st", "-P status --short")),
]
SHELL_CANCEL = '!printf ready > "$ALIAS_SHELL_READY"; sleep 1; printf LATE'
SHELL_CANCEL_CASES = [
    ("shell-cancel-outer", ["-p", *alias("st", SHELL_CANCEL)], True),
    ("shell-cancel-chain", ["-c", "alias.first=-p second", "-c",
                            "alias.second=" + SHELL_CANCEL, "first"], True),
    ("shell-pager-stdio", ["-p", *alias("st", "!printf out; printf err >&2; exit 7")], False),
]


def terminal_run(command, cwd, pager, marker, cancel=False):
    "Capture real terminal output and proof that the pager finished."
    marker.unlink(missing_ok=True)
    ready = marker.with_name("shell-ready")
    ready.unlink(missing_ok=True)
    environment = dict(ENV, GIT_PAGER=str(pager), ALIAS_PAGER_MARKER=str(marker),
                       ALIAS_SHELL_READY=str(ready))
    master, slave = pty.openpty()
    tty.setraw(slave)
    process = None
    output = bytearray()
    try:
        process = subprocess.Popen(command, cwd=cwd, env=environment, stdin=subprocess.PIPE,
                                   stdout=slave, stderr=subprocess.PIPE, start_new_session=True)
        os.close(slave)
        slave = -1
        process.stdin.write(b"alias stdin\n")
        process.stdin.close()
        deadline = time.monotonic() + 45
        cancelled = False
        while True:
            if time.monotonic() > deadline:
                raise RuntimeError("terminal command timeout")
            if cancel and not cancelled and ready.exists():
                os.kill(process.pid, signal.SIGTERM)
                cancelled = True
            if select.select([master], [], [], 0.1)[0]:
                try:
                    chunk = os.read(master, 65536)
                except OSError as error:
                    if error.errno != 5:
                        raise
                    break
                if not chunk:
                    break
                output.extend(chunk)
        status = process.wait(timeout=max(0.1, deadline - time.monotonic()))
        stderr = process.stderr.read()
        if (status < 0 and not (cancel and cancelled and status == -signal.SIGTERM)) or BAD.search(bytes(output) + stderr):
            raise RuntimeError("unexpected terminal process failure")
        if cancel and not cancelled:
            raise RuntimeError("shell cancellation fixture never became ready")
        return (status, bytes(output), stderr, marker.read_bytes() if marker.exists() else b"")
    finally:
        if process is not None and process.poll() is None:
            os.killpg(process.pid, signal.SIGKILL)
            process.wait()
        os.close(master)
        if slave >= 0:
            os.close(slave)


def compare_pager(command, cases):
    rows = []
    with tempfile.TemporaryDirectory(prefix="ngit-h-alias-pager-") as temporary:
        root = Path(temporary)
        ENV["HOME"] = str(root / "home")
        Path(ENV["HOME"]).mkdir()
        pager = root / "pager"
        pager.write_text('#!/bin/sh\nprintf "started\\n" > "$ALIAS_PAGER_MARKER"\ncat\nprintf "finished\\n" >> "$ALIAS_PAGER_MARKER"\n')
        pager.chmod(0o755)
        marker = root / "marker"
        cwd = root / "repo"
        fixture(cwd)
        before = state(cwd)
        for case in cases:
            name, args = case[:2]
            cancel = case[2] if len(case) > 2 else False
            expected = terminal_run(["git", *args], cwd, pager, marker, cancel)
            actual = terminal_run([*command, *args], cwd, pager, marker, cancel)
            if cancel and expected[:3] != (-signal.SIGTERM, b"", b""):
                raise RuntimeError("native Git shell cancellation contract failed")
            should_page = name != "pager-no-pager-fatal"
            expected_marker = b"started\nfinished\n" if should_page else b""
            if expected[3] != expected_marker:
                raise RuntimeError("native Git pager fixture did not meet its contract: " + name)
            same_state = before == state(cwd)
            rows.append({"name": name, "equal": expected == actual and same_state,
                         "state_equal": same_state, "expected": encoded([expected]), "actual": encoded([actual])})
    return rows


def encoded(value):
    if isinstance(value, bytes):
        return {"base64": base64.b64encode(value).decode()}
    if isinstance(value, (tuple, list)):
        return [encoded(v) for v in value]
    return value


def compare(command, cases):
    rows = []
    with tempfile.TemporaryDirectory(prefix="ngit-h-alias-") as temporary:
        root = Path(temporary)
        ENV["HOME"] = str(root / "home")
        Path(ENV["HOME"]).mkdir()
        for name, steps in cases:
            cwd = root / "repo"
            fixture(cwd)
            snapshot = root / "snapshot"
            shutil.copytree(cwd, snapshot)
            expected = [run(["git", *args], cwd) for args in steps]
            expected_state = state(cwd)
            shutil.rmtree(cwd)
            shutil.copytree(snapshot, cwd)
            actual = [run([*command, *args], cwd) for args in steps]
            same_state = expected_state == state(cwd)
            rows.append({"name": name, "equal": expected == actual and same_state,
                         "state_equal": same_state, "expected": encoded(expected), "actual": encoded(actual)})
            shutil.rmtree(cwd)
            shutil.rmtree(snapshot)
    return rows


def main():
    resource.setrlimit(resource.RLIMIT_AS, (8388608 * 1024, 8388608 * 1024))
    mode = sys.argv[1] if len(sys.argv) > 1 else "all"
    if mode not in ("all", "e2e", "controls", "pager", "shell-cancel"):
        raise RuntimeError("mode must be all, e2e, controls, pager or shell-cancel")
    subprocess.run(["sh", str(ROOT / "tools/build-elc.sh")], cwd=ROOT, check=True,
                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=120)
    cases = (SHELL_CANCEL_CASES if mode == "shell-cancel" else
             (PAGER_CASES if mode == "pager" else ([E2E] if mode == "e2e" else CASES)))
    only = os.environ.get("NGIT_UX_ONLY")
    if only:
        cases = [c for c in cases if only in c[0]]
    if not cases:
        raise RuntimeError("zero selected cases")
    result = {"mode": mode, "selected": len(cases)}
    if mode == "controls":
        calibration = compare(["git"], cases + [E2E])
        baseline = Path(os.environ.get("NGIT_H_BASELINE", "target/defects-h/baseline"))
        if not baseline.is_absolute():
            baseline = ROOT / baseline
        if not (baseline / "bin/ngit").is_file():
            raise RuntimeError("baseline repo is missing")
        red = compare([str(baseline / "bin/ngit")], DEFECTS)
        result.update(calibration=calibration, baseline=red)
        pager_calibration = compare_pager(["git"], PAGER_CASES)
        pager_red = compare_pager([str(baseline / "bin/ngit")], PAGER_CASES)
        result.update(pager_calibration=pager_calibration, pager_baseline=pager_red)
        expected_red = []
        for row, (_, steps) in zip(red, DEFECTS):
            actual = row["actual"][0]
            expected = row["expected"][0]
            diagnostic = ("ngit: unsupported command: " + steps[0][2] + "\n").encode()
            expected_red.append(not row["equal"] and row["state_equal"]
                                and expected[0] == 0 and actual[0] == 1
                                and actual[1] == encoded(b"")
                                and actual[2] == encoded(diagnostic))
        result["expected_baseline_failures"] = expected_red
        expected_pager_red = []
        for row, (_, args) in zip(pager_red, PAGER_CASES):
            actual = row["actual"][0]
            diagnostic = ("ngit: unsupported command: " + args[-1] + "\n").encode()
            expected_pager_red.append(not row["equal"] and row["state_equal"]
                                      and actual == encoded((1, b"", diagnostic, b"")))
        result["expected_pager_baseline_failures"] = expected_pager_red
        success = (all(r["equal"] for r in calibration + pager_calibration)
                   and all(expected_red + expected_pager_red))
    else:
        command = shlex.split(os.environ.get("NGIT_UX_COMMAND", str(ROOT / "bin/ngit")))
        rows = compare_pager(command, cases) if mode in ("pager", "shell-cancel") else compare(command, cases)
        result["cases"] = rows
        success = all(r["equal"] for r in rows)
    REPORT.mkdir(parents=True, exist_ok=True)
    (REPORT / (mode + ".json")).write_text(json.dumps(result, indent=2) + "\n")
    if mode == "controls":
        print(f"controls: calibration {sum(r['equal'] for r in calibration)}/{len(calibration)}, baseline mismatches {sum(not r['equal'] for r in red)}/{len(red)}")
        print(f"pager controls: calibration {sum(r['equal'] for r in pager_calibration)}/{len(pager_calibration)}, expected baseline failures {sum(expected_pager_red)}/{len(pager_red)}")
    else:
        print(f"{mode}: {sum(r['equal'] for r in rows)}/{len(rows)} exact; failures: " + ", ".join(r["name"] for r in rows if not r["equal"]))
    return 0 if success else 1


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as error:
        print(f"ERROR: {error}", file=sys.stderr)
        sys.exit(2)
