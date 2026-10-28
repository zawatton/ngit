#!/usr/bin/env python3
"""Compare explicit pager dispatch with Git using terminating real PTYs."""
import errno
import fcntl
import hashlib
import json
import os
from pathlib import Path
import pty
import selectors
import shlex
import signal
import struct
import subprocess
import sys
import tempfile
import termios
import time

ROOT = Path(__file__).resolve().parents[1]
REPORT = Path(os.environ.get("NGIT_REPORT_DIR", ROOT / "target/h-pager"))
COMMAND = shlex.split(os.environ.get("NGIT_UX_COMMAND", str(ROOT / "bin/ngit")))
if "/" in COMMAND[0] and not Path(COMMAND[0]).is_absolute():
    COMMAND[0] = str((ROOT / COMMAND[0]).resolve())
MODE = sys.argv[1] if len(sys.argv) == 2 else "all"
RESULTS = []
ENV = {key: value for key, value in os.environ.items()
       if not key.startswith("GIT_") and key not in ("PAGER", "LESS", "LV")}
ENV.update(LC_ALL="C", TZ="UTC", TERM="dumb", COLUMNS="88",
           GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL="/dev/null",
           GIT_AUTHOR_NAME="Pager Fixture", GIT_AUTHOR_EMAIL="pager@example.test",
           GIT_COMMITTER_NAME="Pager Fixture", GIT_COMMITTER_EMAIL="pager@example.test",
           GIT_AUTHOR_DATE="1700000000 +0000", GIT_COMMITTER_DATE="1700000000 +0000")


def execute(command, cwd, env, terminal=False, stderr_terminal=False, lifetime=None, terminate=None, blocked=False, shell_marker=None):
    """Drain both streams concurrently and kill only the owned process group."""
    master = slave = None
    if terminal:
        master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 93, 0, 0))
    process = subprocess.Popen(command, cwd=cwd, env=env, stdin=subprocess.DEVNULL,
                               stdout=slave if terminal else subprocess.PIPE,
                               stderr=slave if stderr_terminal else subprocess.PIPE,
                               start_new_session=True)
    if slave is not None:
        os.close(slave)
    streams = selectors.DefaultSelector()
    streams.register(master if terminal else process.stdout, selectors.EVENT_READ, 0)
    if process.stderr:
        streams.register(process.stderr, selectors.EVENT_READ, 1)
    output = [bytearray(), bytearray()]
    exit_observation = None
    signal_sent = False
    blocked_witness = None
    def observe_exit():
        marker, completed = lifetime
        identity = json.loads(marker.read_text()) if marker.exists() else None
        status = Path("/proc", str(identity["pid"]), "status") if identity else None
        blocked_status = Path("/proc", str(blocked_witness["pid"]), "status") if blocked_witness else None
        shell_pid = int(shell_marker.read_text()) if shell_marker and shell_marker.exists() and shell_marker.read_text().strip() else None
        shell_status = Path("/proc", str(shell_pid), "status") if shell_pid else None
        return dict(identity=identity, parent_pid=process.pid,
                    pager_status_at_command_exit=status.read_text() if status and status.exists() else None,
                    completed_before_command_exit=completed.exists(), signal_sent=signal_sent,
                    blocked_witness=blocked_witness,
                    shell_pid=shell_pid,
                    shell_status_at_exit=shell_status.read_text() if shell_status and shell_status.exists() else None,
                    blocked_command_status_at_exit=blocked_status.read_text() if blocked_status and blocked_status.exists() else None)
    deadline = time.monotonic() + 30
    try:
        while streams.get_map():
            if terminate and not signal_sent and lifetime[0].exists() and process.poll() is None:
                if blocked:
                    identity = json.loads(lifetime[0].read_text())
                    children = Path("/proc", str(process.pid), "task", str(process.pid), "children")
                    candidates = [process.pid] + ([int(pid) for pid in children.read_text().split()] if children.exists() else [])
                    for pid in candidates:
                        wchan = Path("/proc", str(pid), "wchan")
                        if pid != identity["pid"] and wchan.exists() and "wait_for_partner" in wchan.read_text():
                            blocked_witness = dict(pid=pid, wchan=wchan.read_text())
                shell_ready = not shell_marker or (shell_marker.exists() and shell_marker.read_text().strip())
                if (not blocked or blocked_witness) and shell_ready:
                    os.kill(process.pid, terminate)
                    signal_sent = True
            if lifetime and exit_observation is None and process.poll() is not None:
                exit_observation = observe_exit()
            if time.monotonic() >= deadline:
                raise TimeoutError("pager command exceeded 30 seconds")
            for key, _ in streams.select(0.1):
                try:
                    chunk = os.read(key.fd, 65536)
                except OSError as error:
                    if error.errno != errno.EIO or key.data != 0 or not terminal:
                        raise
                    chunk = b""
                if chunk:
                    output[key.data].extend(chunk)
                else:
                    streams.unregister(key.fileobj)
        rc = process.wait(timeout=max(0.1, deadline - time.monotonic()))
        if lifetime and exit_observation is None:
            exit_observation = observe_exit()
    finally:
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        process.wait(timeout=5)
        streams.close()
        if master is not None:
            os.close(master)
        if process.stdout:
            process.stdout.close()
        if process.stderr:
            process.stderr.close()
    # The PTY's ONLCR converts LF to CRLF; no other output is normalized.
    out = bytes(output[0]).replace(b"\r\n", b"\n") if terminal else bytes(output[0])
    result = dict(rc=rc, stdout=out.decode("utf-8"), stderr=bytes(output[1]).decode("utf-8"))
    if lifetime:
        result["lifetime"] = exit_observation
    return result


def state(repo):
    return {str(path.relative_to(repo)): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in sorted(repo.rglob("*")) if path.is_file()}


def observe(command, repo, scratch, flags, config, environment, terminal, stderr_terminal=False, extra_args=()):
    marker = scratch / "pager.jsonl"
    marker.unlink(missing_ok=True)
    env = dict(ENV, **environment, H_PAGER_LOG=str(marker))
    env = {key: value for key, value in env.items() if value is not None}
    args = [*flags]
    for key, value in config.items():
        args += ["-c", key + "=" + value]
    args += ["log", "--format=%h %s", "--no-decorate", "--no-color"]
    args += list(extra_args)
    before = state(repo)
    result = execute(command + args, repo, env, terminal, stderr_terminal)
    result["pager"] = [json.loads(line) for line in marker.read_text().splitlines()] if marker.exists() else []
    result["state_unchanged"] = before == state(repo)
    return result


def healthy(result, failure=False):
    return ((failure or (result["rc"] == 0 and result["stderr"] == "")) and result["state_unchanged"]
            and (result["rc"] >= 0 or (failure and result["rc"] == -signal.SIGPIPE))
            and result["rc"] not in (-11, -6, 126, 127, 134, 139)
            and not any(word in result["stdout"] + result["stderr"]
                        for word in ("void-function", "void-variable", "wrong-type-argument", "backtrace", "panicked at")))


def compare(name, command, repo, scratch, case, negative=False):
    if os.environ.get("NGIT_H_MATCH") and os.environ["NGIT_H_MATCH"] not in name:
        return
    expected = observe(["git"], repo, scratch, **case)
    actual = observe(command, repo, scratch, **case)
    failure = name.startswith(("lifecycle-", "git-calibration-lifecycle-"))
    passed = expected == actual and healthy(expected, failure) and healthy(actual, failure)
    if negative:
        # A real old-code mismatch must be exactly the absent pager invocation.
        passed = (healthy(expected) and healthy(actual) and len(expected["pager"]) == 1
                  and actual["pager"] == [] and expected["stdout"] == actual["stdout"])
    RESULTS.append(dict(name=name, passed=passed, expected=expected, actual=actual))
    print(("PASS " if passed else "FAIL ") + name, flush=True)


def main():
    if MODE not in ("all", "e2e", "controls", "lifecycle", "reaping"):
        raise ValueError("mode must be all, e2e, controls, lifecycle, or reaping")
    REPORT.mkdir(parents=True, exist_ok=True)
    if COMMAND != ["git"] and MODE != "controls":
        subprocess.run(["sh", str(ROOT / "tools/build-elc.sh")], check=True, timeout=120)
    with tempfile.TemporaryDirectory(prefix="ngit-h-pager-") as directory:
        scratch = Path(directory)
        repo = scratch / "repo"
        repo.mkdir()
        def git(*args):
            value = execute(["git", *args], repo, ENV)
            if value["rc"]:
                raise RuntimeError(value)
        git("init", "-q", "-b", "main")
        for i in range(12):
            (repo / "fixture").write_text(str(i) + "\n")
            git("add", "fixture")
            git("commit", "-qm", "fixture " + str(i))
        pagers = {}
        for label in ("environment", "core", "fallback", "command"):
            script = scratch / ("pager-" + label)
            script.write_text("#!/usr/bin/env python3\nimport hashlib,json,os,sys\n"
                              "record={'label':" + repr(label) + ", 'env':{k:os.environ.get(k) for k in "
                              "('LESS','LV','GIT_PAGER_IN_USE','COLUMNS')},'tty':[os.isatty(i) for i in range(3)]}\n"
                              "digest=hashlib.sha256(); size=0\n"
                              "while True:\n data=os.read(0,65536)\n if not data: break\n os.write(1,data)\n")
            with script.open("a") as stream:
                stream.write(" digest.update(data); size+=len(data)\n"
                             "record.update(input_sha256=digest.hexdigest(),input_size=size)\n"
                             "with open(os.environ['H_PAGER_LOG'],'a') as f: f.write(json.dumps(record)+'\\n')\n")
            script.chmod(0o755)
            pagers[label] = shlex.quote(str(script))
        blocked = scratch / "nonexec-path"
        executable = scratch / "exec-path"
        blocked.mkdir()
        executable.mkdir()
        bare_name = "h-path-pager"
        (blocked / bare_name).write_text("#!/bin/sh\nexit 0\n")
        (executable / bare_name).write_bytes((scratch / "pager-environment").read_bytes())
        (executable / bare_name).chmod(0o755)
        def case(flags=("-p",), config=None, environment=None, terminal=True, stderr_terminal=False):
            return dict(flags=list(flags), config=config or {},
                        environment=environment if environment is not None else {"GIT_PAGER": pagers["environment"]},
                        terminal=terminal, stderr_terminal=stderr_terminal)
        matrix = [
            ("short-paginate", case()),
            ("long-paginate", case(("--paginate",))),
            ("no-pager-short", case(("-p", "-P"))),
            ("no-pager-long", case(("-p", "--no-pager"))),
            ("last-option-enables", case(("-P", "-p"))),
            ("non-tty", case(terminal=False)),
            ("empty-git-pager", case(environment={"GIT_PAGER": "", "PAGER": pagers["fallback"]})),
            ("cat-git-pager", case(environment={"GIT_PAGER": "cat"})),
            ("core-pager", case(config={"core.pager": pagers["core"]}, environment={})),
            ("environment-precedence", case(config={"core.pager": pagers["core"]})),
            ("pager-fallback", case(environment={"PAGER": pagers["fallback"]})),
            ("empty-core-pager", case(config={"core.pager": ""}, environment={"PAGER": pagers["fallback"]})),
            ("cat-core-pager", case(config={"core.pager": "cat"}, environment={})),
            ("shell-pager", case(environment={"GIT_PAGER": pagers["environment"] + " ; :"})),
            ("preserve-pager-environment", case(environment={"GIT_PAGER": pagers["environment"], "LESS": "custom", "LV": "custom"})),
            ("explicit-bypasses-command-pager", case(config={"pager.log": pagers["command"], "core.pager": pagers["core"]}, environment={})),
            ("explicit-overrides-command-disable", case(config={"pager.log": "false"})),
            ("stderr-terminal", case(stderr_terminal=True)),
            ("terminal-width", case(environment={"GIT_PAGER": pagers["environment"], "COLUMNS": None})),
            ("empty-pager-environment", case(environment={"GIT_PAGER": pagers["environment"], "LESS": "", "LV": ""})),
            ("inherited-pager-marker", case(environment={"GIT_PAGER": pagers["environment"], "GIT_PAGER_IN_USE": "true"})),
        ]
        failures = [
            ("lifecycle-missing-executable", case(environment={"GIT_PAGER": "h-no-such-pager-executable"})),
            # More than a pipe's capacity prevents Git's small-output race:
            # writing must encounter the closed reader, regardless of scheduling.
            ("lifecycle-shell-failure", dict(case(environment={"GIT_PAGER": "exit 7"}),
                                             extra_args=["--format=format:" + "x" * 8192])),
            ("lifecycle-early-close", dict(case(environment={"GIT_PAGER": "/bin/true"}),
                                           extra_args=["--format=format:" + "x" * 8192])),
            ("lifecycle-command-failure", dict(case(), extra_args=["--h-invalid-option"])),
            ("lifecycle-bare-nonexec", case(environment={"GIT_PAGER": bare_name,
                                                        "PATH": str(blocked) + ":" + ENV["PATH"]})),
            ("lifecycle-later-executable", case(environment={"GIT_PAGER": bare_name,
                                                             "PATH": str(blocked) + ":" + str(executable) + ":" + ENV["PATH"]})),
            ("lifecycle-explicit-nonexec", case(environment={"GIT_PAGER": str(blocked / bare_name)})),
        ]
        if MODE == "reaping":
            marker, completed = scratch / "lifetime.json", scratch / "completed"
            sleeper = scratch / "slow-closed-pager"
            sleeper.write_text("#!/usr/bin/env python3\nimport json,os,time\n"
                               "if os.environ.get('H_WAIT_FOR_INPUT'): os.read(0,1)\n"
                               "os.close(0)\n"
                               "marker=os.environ['H_LIFETIME_MARKER']\n"
                               "with open(marker+'.tmp','w') as f: "
                               "json.dump({'pid':os.getpid(),'ppid':os.getppid()},f)\n"
                               "os.replace(marker+'.tmp',marker)\n"
                               "time.sleep(2)\n"
                               "open(os.environ['H_LIFETIME_COMPLETED'],'w').write('completed\\n')\n")
            sleeper.chmod(0o755)
            copier = scratch / "cancellation-copy-pager"
            copier.write_text("#!/usr/bin/env python3\nimport json,os\n"
                              "marker=os.environ['H_LIFETIME_MARKER']\n"
                              "with open(marker+'.tmp','w') as f: json.dump({'pid':os.getpid(),'ppid':os.getppid()},f)\n"
                              "os.replace(marker+'.tmp',marker)\n"
                              "while True:\n data=os.read(0,65536)\n if not data: break\n os.write(1,data)\n"
                              "open(os.environ['H_LIFETIME_COMPLETED'],'w').write('completed\\n')\n")
            copier.chmod(0o755)
            shell_pid_marker = scratch / "shell.pid"
            os.mkfifo(repo / "wait-fifo")
            env = dict(ENV, GIT_PAGER=str(sleeper), H_LIFETIME_MARKER=str(marker),
                       H_LIFETIME_COMPLETED=str(completed))
            for name, requested in (("closed-reader-wait-and-reap", None),
                                    ("primary-sigterm-wait-and-reap", signal.SIGTERM),
                                    ("primary-sighup-wait-and-reap", signal.SIGHUP),
                                    ("primary-sigterm-cancels-blocked-command", signal.SIGTERM),
                                    ("primary-sigterm-cancels-shell-alias", signal.SIGTERM)):
                if os.environ.get("NGIT_H_MATCH") and os.environ["NGIT_H_MATCH"] not in name:
                    continue
                values, proof = [], []
                for command in (["git"], COMMAND):
                    marker.unlink(missing_ok=True)
                    completed.unlink(missing_ok=True)
                    shell_pid_marker.unlink(missing_ok=True)
                    before = state(repo)
                    blocked = "blocked-command" in name
                    shell_alias = "shell-alias" in name
                    invocation = (["-p", "hash-object", "wait-fifo"] if blocked else
                                  ["-p", "status", "--short"] if requested else
                                  ["-p", "log", "--format=format:" + "x" * 8192])
                    run_env = dict(env)
                    if shell_alias:
                        run_env.update(GIT_PAGER=str(copier), H_SHELL_MARKER=str(shell_pid_marker))
                        invocation = ["-p", "-c", "alias.cancel=!printf '%s\\n' \"$$\" >\"$H_SHELL_MARKER\"; sleep 2; printf LATE", "cancel"]
                    if not requested:
                        run_env["H_WAIT_FOR_INPUT"] = "1"
                    value = execute(command + invocation, repo, run_env, terminal=True,
                                    lifetime=(marker, completed), terminate=requested, blocked=blocked,
                                    shell_marker=shell_pid_marker if shell_alias else None)
                    lifetime = value.pop("lifetime")
                    identity = lifetime["identity"]
                    value.update(state_unchanged=before == state(repo),
                                 signal_sent=lifetime["signal_sent"],
                                 blocked_command_observed=bool(lifetime["blocked_witness"]),
                                 blocked_command_reaped=lifetime["blocked_command_status_at_exit"] is None,
                                 shell_started=bool(lifetime["shell_pid"]),
                                 shell_reaped_before_command_exit=lifetime["shell_status_at_exit"] is None,
                                 pager_was_direct_child=bool(identity and identity["ppid"] == lifetime["parent_pid"]),
                                 pager_reaped_before_command_exit=bool(identity and lifetime["pager_status_at_command_exit"] is None),
                                 pager_completed_before_command_exit=lifetime["completed_before_command_exit"])
                    values.append(value)
                    proof.append(lifetime)
                expected, actual = values
                passed = (expected == actual and actual["rc"] == -(requested or signal.SIGPIPE)
                          and actual["stdout"] == actual["stderr"] == ""
                          and actual["signal_sent"] == bool(requested)
                          and actual["blocked_command_observed"] == blocked
                          and actual["shell_started"] == shell_alias
                          and all(actual[key] for key in ("state_unchanged", "pager_was_direct_child",
                                                         "pager_reaped_before_command_exit", "pager_completed_before_command_exit",
                                                         "blocked_command_reaped", "shell_reaped_before_command_exit")))
                RESULTS.append(dict(name=name, passed=passed, expected=expected, actual=actual, process_evidence=proof))
                print(("PASS " if passed else "FAIL ") + name, flush=True)
        elif MODE == "lifecycle":
            for name, scenario in failures:
                compare(name, COMMAND, repo, scratch, scenario)
        elif MODE == "controls":
            baseline = Path(os.environ.get("NGIT_H_BASELINE", "target/defects-h/baseline"))
            baseline = baseline if baseline.is_absolute() else ROOT / baseline
            if not (baseline / "bin/ngit").is_file():
                raise FileNotFoundError("baseline checkout missing: " + str(baseline))
            for name, scenario in matrix:
                compare("git-calibration-" + name, ["git"], repo, scratch, scenario)
            for name, scenario in failures:
                compare("git-calibration-" + name, ["git"], repo, scratch, scenario)
            for index in (0, 1, 4, 8, 9, 10, 13, 14, 15, 16):
                name, scenario = matrix[index]
                compare("baseline-negative-" + name, [str(baseline / "bin/ngit")], repo, scratch, scenario, True)
        elif MODE == "e2e":
            compare("e2e-paginated-log", COMMAND, repo, scratch, case(("--paginate",)))
        else:
            for name, scenario in matrix + failures:
                compare(name, COMMAND, repo, scratch, scenario)
    (REPORT / (MODE + ".json")).write_text(json.dumps(RESULTS, indent=2) + "\n")
    passed = sum(result["passed"] for result in RESULTS)
    print(f"{MODE}: {passed}/{len(RESULTS)} passed")
    return 0 if RESULTS and passed == len(RESULTS) else 1


if __name__ == "__main__":
    sys.exit(main())
