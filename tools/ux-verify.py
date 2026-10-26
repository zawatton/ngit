#!/usr/bin/env python3
"""Verifier for the daily-use ledger (tools/ai/git-daily-ux-progress.org).

controls       every graded mode fails against the 5db0c7f baseline by compared
               cases, Git compared with itself passes it, and the suite sources
               are new (no test present at 5db0c7f changes)
mode SUITE MODE  one mode meets its floor with no failures
regression     the existing suites stay green
"""
import json
import os
import resource
import shutil
import signal
import subprocess
import sys
import tempfile
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
BASE = "5db0c7f"
TASK = Path(os.environ.get("NGIT_UX_DIR", ROOT / "target/ux")).resolve()
DEADLINE = time.monotonic() + float(os.environ.get("NGIT_UX_DEADLINE", "580"))

# Suite -> graded modes; each suite is test/SUITE.sh MODE writing
# $NGIT_REPORT_DIR/SUITE-MODE.json and reading the command under test from
# NGIT_UX_COMMAND (default bin/ngit).
SUITES = {
    "ux-commit": ("editor", "identity", "paths", "e2e"),
    "ux-log": ("follow", "dates", "format", "history", "e2e"),
    "ux-cmds": ("status", "refs", "worktree", "stash", "e2e"),
    "apply-gen": ("review", "apply-gen"),
}
# Exact comparisons each mode must reach: every case the calibrated suite runs.
FLOORS = {
    "ux-commit/editor": 76, "ux-commit/identity": 54, "ux-commit/paths": 39, "ux-commit/e2e": 4,
    "ux-log/follow": 49, "ux-log/dates": 65, "ux-log/format": 100, "ux-log/history": 141, "ux-log/e2e": 8,
    "ux-cmds/status": 141, "ux-cmds/refs": 55, "ux-cmds/worktree": 118, "ux-cmds/stash": 170,
    "ux-cmds/e2e": 14,
    "apply-gen/review": 118, "apply-gen/apply-gen": 448,
}

# Longest suites first, so each starts early enough for the deadline.
REGRESSION = [["sh", "test/audit-reaudit.sh", "round4-pull"]] + \
             [["sh", "test/compare.sh", s] for s in
              ("hooks", "commit", "merge", "cherry-pick", "rebase", "log-walk", "log-path",
               "show", "tag-write", "tag-list", "status", "status-long", "branch-create",
               "branch-modify", "describe", "rev", "stash-push", "reset-index", "checkout")]


def fail(message):
    print("FAIL: " + message)
    sys.exit(1)


def cap():
    resource.setrlimit(resource.RLIMIT_AS, (8 << 30, 8 << 30))


def kill_marked(token):
    """SIGKILL every process whose environment carries TOKEN, until none is left."""
    marker = ("NGIT_VERIFY_RUN=" + token).encode()
    for _ in range(20):
        found = False
        for entry in Path("/proc").iterdir():
            if not entry.name.isdigit():
                continue
            try:
                if marker in (entry / "environ").read_bytes().split(b"\0"):
                    os.kill(int(entry.name), signal.SIGKILL)
                    found = True
            except (OSError, ValueError):
                pass
        if not found:
            return
        time.sleep(0.1)


def run(argv, cwd, env, timeout=600):
    timeout = min(timeout, DEADLINE - time.monotonic())
    if timeout <= 0:
        fail("overall deadline exceeded")
    # Descendants may start their own sessions, so each run is marked in the
    # environment and every marked process is killed on timeout.
    token = "%d-%d" % (os.getpid(), time.monotonic_ns())
    p = subprocess.Popen(argv, cwd=cwd, env=dict(env, NGIT_VERIFY_RUN=token),
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                         preexec_fn=cap, start_new_session=True)
    try:
        out, err = p.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        kill_marked(token)
        p.communicate()
        return (-1, b"", b"timeout")
    return (p.returncode, out, err)


def suite_environment(**extra):
    """The caller's environment without Git or ngit overrides, so a suite
    cannot be redirected to another command or repository; only EXTRA adds
    NGIT_UX_COMMAND, NGIT_NO_ELC or the report directory."""
    env = {k: v for k, v in os.environ.items()
           if not k.startswith(("GIT_", "NGIT_")) and k != "VISUAL"}
    env.update(EDITOR="true", **extra)
    return env


def suite(name, mode, label, **extra):
    """Run one mode and return its report, or None when it wrote none."""
    reports = Path(tempfile.mkdtemp(dir=TASK, prefix="reports-"))
    try:
        rc, out, err = run(["sh", "test/%s.sh" % name, mode], ROOT,
                           suite_environment(NGIT_REPORT_DIR=str(reports), **extra))
        path = reports / ("%s-%s.json" % (name, mode))
        report = json.loads(path.read_text()) if path.exists() else None
        if report is not None:
            report["rc"] = rc
            report.pop("cases", None)
        else:
            print("%s %s %s: no report, rc=%d\n%s" % (label, name, mode, rc,
                                                     (out + err)[-400:].decode(errors="replace")))
        return report
    finally:
        shutil.rmtree(reports, ignore_errors=True)


def summary(label, key, r):
    return "%-9s %-22s comparisons=%s exact=%s intended=%s failures=%s unexpected=%s rc=%s" % (
        label, key, r["comparisons"], r["exact"], len(r["intended"]), r["failures"],
        r["unexpected_errors"], r["rc"])


def modes():
    return [(s, m) for s, ms in SUITES.items() for m in ms]


def baseline():
    tree = TASK / "baseline"
    if not (tree / "bin/ngit").exists():
        subprocess.run(["git", "worktree", "add", "--detach", str(tree), BASE], cwd=ROOT,
                       check=True, stdout=subprocess.DEVNULL)
    head = subprocess.run(["git", "rev-parse", "--short=7", "HEAD"], cwd=tree,
                          capture_output=True, text=True).stdout.strip()
    if head != BASE:
        fail("baseline checkout is at %s, not %s" % (head, BASE))
    # A nested worktree finds nelisp only through NGIT_NELISP_ROOT; byte code
    # keeps the slow source mode from exhausting the deadline.
    if run(["sh", "tools/build-elc.sh"], tree,
           suite_environment(NGIT_NELISP_ROOT=str(ROOT.parent / "nelisp")), timeout=300)[0]:
        fail("baseline byte compilation failed")
    return tree


def controls(only=None):
    # One suite per call keeps each run inside the meter's 600 s limit.
    if only is not None and only not in SUITES:
        fail("unknown suite %s" % only)
    TASK.mkdir(parents=True, exist_ok=True)
    present = subprocess.run(["git", "ls-tree", "-r", "--name-only", BASE, "test"], cwd=ROOT,
                             capture_output=True, text=True).stdout.split()
    for name in SUITES:
        for ext in (".py", ".sh"):
            if "test/%s%s" % (name, ext) in present:
                fail("test/%s%s existed at %s; suites must be new" % (name, ext, BASE))
    old = str(baseline() / "bin/ngit")
    # A nested baseline cannot find ../../nelisp, and the suites strip NGIT_*
    # from their environment, so the root travels inside the command.
    old = "env NGIT_NELISP_ROOT=%s %s" % (ROOT.parent / "nelisp", old)
    chosen = [(s, m) for s, m in modes() if only in (None, s)]
    jobs = [(s, m, "git", {"NGIT_UX_COMMAND": "git"}) for s, m in chosen] + \
           [(s, m, "baseline", {"NGIT_UX_COMMAND": old}) for s, m in chosen]
    with ThreadPoolExecutor(max_workers=4) as pool:
        results = list(pool.map(lambda j: (j, suite(j[0], j[1], j[2], **j[3])), jobs))
    bad = []
    for (s, m, label, _), r in results:
        key = "%s/%s" % (s, m)
        if r is None:
            bad.append("%s %s: no report" % (label, key))
            continue
        print(summary(label, key, r))
        if label == "git" and (r["failures"] or r["unexpected_errors"] or r["rc"] or r["intended"]
                               or r["comparisons"] < FLOORS.get(key, 1)):
            bad.append("Git differs from itself in %s" % key)
        # Unexpected errors on the baseline can be the defects being fixed;
        # only a run whose failures are all crashes is rejected.
        if label == "baseline" and (r["failures"] <= r["unexpected_errors"] or r["rc"] != 1
                                    or r["comparisons"] < FLOORS.get(key, 1)):
            bad.append("baseline %s is not red by compared cases" % key)
    if bad:
        fail("; ".join(bad))


def one(name, mode):
    key = "%s/%s" % (name, mode)
    if (name, mode) not in modes():
        fail("unknown mode %s" % key)
    TASK.mkdir(parents=True, exist_ok=True)
    r = suite(name, mode, "ngit")
    if r is None:
        fail("%s: no report" % key)
    print(summary("ngit", key, r))
    errors = []
    if r["failures"] or r["unexpected_errors"] or r["rc"]:
        errors.append("%d failures, %d unexpected errors, rc=%d"
                      % (r["failures"], r["unexpected_errors"], r["rc"]))
    if key not in FLOORS:
        errors.append("no floor yet")
    elif r["exact"] < FLOORS[key]:
        errors.append("%d exact comparisons, floor %d" % (r["exact"], FLOORS[key]))
    errors += ["intended difference %s has no reason" % n for n, why in r["intended"].items() if not why]
    if errors:
        fail("%s: %s" % (key, "; ".join(errors)))


def regression(group=None):
    # round4-pull alone takes most of the 600 s limit, so the meter runs it
    # as its own group; no group runs everything.
    groups = {"pull": REGRESSION[:1], "compare": REGRESSION[1:]}
    if group is not None and group not in groups:
        fail("unknown regression group %s" % group)
    chosen = REGRESSION if group is None else groups[group]
    fixtures = Path(tempfile.mkdtemp())

    def job(argv):
        env = suite_environment(NGIT_FIXTURE_DIR=tempfile.mkdtemp(dir=fixtures))
        start = time.monotonic()
        rc, out, err = run(argv, ROOT, env, timeout=900)
        return argv, rc, time.monotonic() - start, (out + err)[-400:]
    try:
        with ThreadPoolExecutor(max_workers=6) as pool:
            results = list(pool.map(job, chosen))
    finally:
        shutil.rmtree(fixtures, ignore_errors=True)
    bad = [r for r in results if r[1]]
    for argv, rc, seconds, _ in results:
        print("rc=%d %6.1fs %s" % (rc, seconds, " ".join(argv[1:])))
    for argv, rc, _, tail in bad:
        print("--- " + " ".join(argv) + "\n" + tail.decode(errors="replace"))
    if bad:
        fail("%d regression suites failed" % len(bad))


if __name__ == "__main__":
    args = sys.argv[1:]
    if args[:1] == ["mode"] and len(args) == 3:
        one(args[1], args[2])
    elif args[:1] == ["regression"] and len(args) == 2:
        regression(args[1])
    elif args[:1] == ["controls"] and len(args) == 2:
        controls(args[1])
    elif args in (["controls"], ["regression"]):
        {"controls": controls, "regression": regression}[args[0]]()
    else:
        fail("usage: ux-verify.py controls [SUITE]|mode SUITE MODE|regression [pull|compare]")
    print("PASS " + " ".join(args))
