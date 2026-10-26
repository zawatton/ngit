#!/usr/bin/env python3
"""Verifier for the protocol v2 ledger (tools/ai/git-protocol-v2-progress.org).

controls     every transcript mode fails against the 6a462c1 baseline, Git
             compared with itself passes every mode, and the test sources
             present at the baseline are unchanged (the HTTP fixture may only
             grow backward compatible options)
mode MODE    one test/protocol-v2.sh mode meets its floor with no failures
differences  every mode at once
regression   existing suites stay green
e2e          the e2e modes with byte code and with sources
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
BASE = "6a462c1"
TASK = Path(os.environ.get("NGIT_PROTOCOL_V2_DIR", ROOT / "target/protocol-v2")).resolve()
DEADLINE = time.monotonic() + float(os.environ.get("NGIT_PV2_DEADLINE", "580"))

MODES = ("select", "lsrefs", "e2e-ssh", "http-discovery", "http-fetch", "e2e-https",
         "negotiate", "negotiate-http", "negotiate-v0", "e2e-negotiate", "errors",
         "server-option", "fallback", "review",
         # Follow-ups to 5db0c7f (tools/ai/git-pv2-followups-progress.org).
         "followups-launcher", "file", "followups", "config-case")
FOLLOWUP_BASE = "5db0c7f"
FOLLOWUP_MODES = ("followups-launcher", "file", "followups", "config-case")
E2E_MODES = ("e2e-ssh", "e2e-https", "e2e-negotiate", "file")
# Exact comparisons each mode must reach: every case the calibrated harness runs.
FLOORS = {"select": 40, "lsrefs": 18, "e2e-ssh": 4, "http-discovery": 12, "http-fetch": 24,
          "e2e-https": 4, "negotiate": 6, "negotiate-http": 6, "negotiate-v0": 6,
          "e2e-negotiate": 4, "errors": 7, "server-option": 36, "fallback": 6,
          "review": 59, "followups-launcher": 60, "file": 60, "followups": 8,
          "config-case": 50}
# Test files the harness may change; everything else present at BASE stays.
GROWN = {"test/smart-http-fixture.py"}

# Longest suites first (round4-pull alone takes about 440-480 s), so every
# suite starts early enough to finish inside the 580 s deadline.
REGRESSION = [["sh", "test/audit-reaudit.sh", m] for m in ("round4-pull", "pull")] + \
             [["sh", "test/compare.sh", s] for s in ("hooks", "clone")] + \
             [["sh", "test/round6.sh", "pull"]] + \
             [["sh", "test/compare.sh", s] for s in ("fetch", "push", "remote", "ssh")] + \
             [["sh", "test/smart-http.sh", m] for m in ("discovery", "fetch", "push", "auth", "e2e")] + \
             [["sh", "test/credential-helpers.sh", m] for m in ("helpers", "auth", "boundaries", "e2e")]


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
    NGIT_PV2_COMMAND, NGIT_NO_ELC or the report directory."""
    env = {k: v for k, v in os.environ.items()
           if not k.startswith(("GIT_", "NGIT_")) and k != "VISUAL"}
    env.update(EDITOR="true", **extra)
    return env


def suite(mode, label, **extra):
    """Run one mode and return its report, or None when it wrote none."""
    reports = Path(tempfile.mkdtemp(dir=TASK, prefix="reports-"))
    try:
        rc, out, err = run(["sh", "test/protocol-v2.sh", mode], ROOT,
                           suite_environment(NGIT_REPORT_DIR=str(reports), **extra))
        path = reports / ("protocol-v2-%s.json" % mode)
        report = json.loads(path.read_text()) if path.exists() else None
        if report is not None:
            report["rc"] = rc
            report.pop("cases", None)
        else:
            print("%s %s: no report, rc=%d\n%s" % (label, mode, rc,
                                                  (out + err)[-400:].decode(errors="replace")))
        return report
    finally:
        shutil.rmtree(reports, ignore_errors=True)


def summary(label, mode, r):
    return "%-9s %-15s comparisons=%s exact=%s intended=%s failures=%s unexpected=%s rc=%s" % (
        label, mode, r["comparisons"], r["exact"], len(r["intended"]), r["failures"],
        r["unexpected_errors"], r["rc"])


def baseline(base=BASE):
    """The checkout of BASE."""
    tree = TASK / ("baseline" if base == BASE else "baseline-" + base)
    if not (tree / "bin/ngit").exists():
        subprocess.run(["git", "worktree", "add", "--detach", str(tree), base], cwd=ROOT,
                       check=True, stdout=subprocess.DEVNULL)
    head = subprocess.run(["git", "rev-parse", "--short=7", "HEAD"], cwd=tree,
                          capture_output=True, text=True).stdout.strip()
    if head != base:
        fail("baseline checkout is at %s, not %s" % (head, base))
    # A nested worktree finds nelisp only through NGIT_NELISP_ROOT; byte code
    # keeps the slow source mode from exhausting the deadline.
    if run(["sh", "tools/build-elc.sh"], tree,
           suite_environment(NGIT_NELISP_ROOT=str(ROOT.parent / "nelisp")), timeout=300)[0]:
        fail("baseline byte compilation failed")
    return tree


def controls(base=BASE, modes=MODES[:14], grown=GROWN, end="5db0c7f"):
    TASK.mkdir(parents=True, exist_ok=True)
    # The guard covers the objective's own commit (END); later objectives
    # may change shared suites such as test/compare.sh.
    changed = subprocess.run(["git", "diff", "--name-only", "--diff-filter=MDT", base, end, "--", "test"], cwd=ROOT,
                             capture_output=True, text=True).stdout.split()
    # Deleted files count as changed, so a removed test cannot pass this check.
    if set(changed) - grown:
        fail("test sources present at the baseline changed: %s" % sorted(set(changed) - grown))
    old = str(baseline(base) / "bin/ngit")
    # A nested baseline cannot find ../../nelisp, and the suites strip NGIT_*
    # from their environment, so the root travels inside the command.
    old = "env NGIT_NELISP_ROOT=%s %s" % (ROOT.parent / "nelisp", old)
    jobs = [(m, "git", {"NGIT_PV2_COMMAND": "git"}) for m in modes] + \
           [(m, "baseline", {"NGIT_PV2_COMMAND": old}) for m in modes]
    with ThreadPoolExecutor(max_workers=4) as pool:
        results = list(pool.map(lambda j: (j[0], j[1], suite(j[0], j[1], **j[2])), jobs))
    bad = []
    for mode, label, r in results:
        if r is None:
            bad.append("%s %s: no report" % (label, mode))
            continue
        print(summary(label, mode, r))
        if label == "git" and (r["failures"] or r["unexpected_errors"] or r["rc"] or r["intended"]
                               or r["comparisons"] < FLOORS.get(mode, 1)):
            bad.append("Git differs from itself in %s" % mode)
        if label == "baseline" and not r["failures"]:
            bad.append("%s passes on the baseline, so it cannot show the change" % mode)
        # A red baseline must come from compared cases, not from a crashed
        # suite or command: it exits 1 having run every case the floor counts,
        # with no case ending in a crash or timeout.
        # Unexpected errors on the baseline can be the very defects fixed
        # here; only a run whose failures are all crashes is rejected.
        if label == "baseline" and (r["rc"] != 1 or r["failures"] <= r["unexpected_errors"]
                                    or r["comparisons"] < FLOORS.get(mode, 1)):
            bad.append("baseline %s: rc=%d with %d comparisons is not a compared failure"
                       % (mode, r["rc"], r["comparisons"]))
    if bad:
        fail("; ".join(bad))


def check(mode, r, label="ngit"):
    if r is None:
        return ["%s %s: no report" % (label, mode)]
    print(summary(label, mode, r))
    errors = []
    if r["failures"] or r["unexpected_errors"] or r["rc"]:
        errors.append("%s %s: %d failures, %d unexpected errors, rc=%d" % (
            label, mode, r["failures"], r["unexpected_errors"], r["rc"]))
    if mode not in FLOORS:
        errors.append("%s has no floor yet" % mode)
    elif r["exact"] < FLOORS[mode]:
        errors.append("%s %s: %d exact comparisons, floor %d" % (label, mode, r["exact"], FLOORS[mode]))
    for name, reason in r["intended"].items():
        if not reason:
            errors.append("%s %s: intended difference %s has no reason" % (label, mode, name))
    return errors


def one(mode):
    if mode not in MODES:
        fail("unknown mode %s" % mode)
    TASK.mkdir(parents=True, exist_ok=True)
    errors = check(mode, suite(mode, "ngit"))
    if errors:
        fail("; ".join(errors))


def differences():
    TASK.mkdir(parents=True, exist_ok=True)
    with ThreadPoolExecutor(max_workers=4) as pool:
        results = list(pool.map(lambda m: (m, suite(m, "ngit")), MODES))
    errors = [e for m, r in results for e in check(m, r)]
    if errors:
        fail("; ".join(errors))


PULL_SUITES = ("audit-reaudit.sh", "round6.sh")


def regression(group="all"):
    # "pull" runs the long pull suites alone and "transport" the rest, so
    # each half fits the meter's 600 s limit on a loaded machine.
    suites = [a for a in REGRESSION
              if group == "all" or (a[1].split("/")[-1] in PULL_SUITES) == (group == "pull")]
    if group not in ("all", "pull", "transport"):
        fail("unknown regression group %s" % group)
    fixtures = Path(tempfile.mkdtemp())

    def job(argv):
        # Suites build shared fixture names, so each job gets its own directory.
        env = suite_environment(NGIT_FIXTURE_DIR=tempfile.mkdtemp(dir=fixtures))
        start = time.monotonic()
        rc, out, err = run(argv, ROOT, env, timeout=900)
        return argv, rc, time.monotonic() - start, (out + err)[-400:]
    try:
        with ThreadPoolExecutor(max_workers=6) as pool:
            results = list(pool.map(job, suites))
    finally:
        shutil.rmtree(fixtures, ignore_errors=True)
    bad = [r for r in results if r[1]]
    for argv, rc, seconds, _ in results:
        print("rc=%d %6.1fs %s" % (rc, seconds, " ".join(argv[1:])))
    for argv, rc, _, tail in bad:
        print("--- " + " ".join(argv) + "\n" + tail.decode(errors="replace"))
    if bad:
        fail("%d regression suites failed" % len(bad))


def e2e():
    # Byte-code mode is real only while the stamp covers every source; the
    # suite environment drops NGIT_NO_ELC, so only the source mode sets it.
    if run(["sh", "tools/build-elc.sh"], ROOT, suite_environment(), timeout=300)[0]:
        fail("byte compilation failed")
    stamp = ROOT / "lisp/.elc-stamp"
    if not stamp.exists() or any(p.stat().st_mtime > stamp.stat().st_mtime
                                 for p in (ROOT / "lisp").glob("*.el")):
        fail("byte code is older than the sources")
    TASK.mkdir(parents=True, exist_ok=True)
    jobs = [(m, "byte-code", {}) for m in E2E_MODES] + \
           [(m, "source", {"NGIT_NO_ELC": "1"}) for m in E2E_MODES]
    with ThreadPoolExecutor(max_workers=3) as pool:
        results = list(pool.map(lambda j: (j[0], j[1], suite(j[0], j[1], **j[2])), jobs))
    errors = [e for m, label, r in results for e in check(m, r, label)]
    if errors:
        fail("; ".join(errors))


if __name__ == "__main__":
    mode = sys.argv[1] if len(sys.argv) > 1 else ""
    if mode == "mode" and len(sys.argv) > 2:
        one(sys.argv[2])
    else:
        {"controls": controls,
         "controls-followups": lambda: controls(FOLLOWUP_BASE, FOLLOWUP_MODES,
                                                {"test/protocol-v2.py",
                                                 "test/smart-http-fixture.py"},
                                                "60924e0"),
         "differences": differences, "regression": lambda: regression(*sys.argv[2:3]),
         "e2e": e2e}.get(mode, lambda: fail("usage: protocol-v2-verify.py "
                                           "controls|mode MODE|differences|regression|e2e"))()
    print("PASS " + " ".join(sys.argv[1:]))
