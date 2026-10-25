#!/usr/bin/env python3
"""Linux process-tree memory guard for ngit transports.

NGIT_MEMORY_BUDGET_MIB is a positive integer, default 8192.  Every 50 ms,
sample /proc smaps_rollup PSS for the owned tree AND this supervisor.  If PSS
is unavailable for any live member, sum RSS for every member instead and
label it rss-upper-bound (shared fork pages are counted more than once).
Sampling is monitored termination, NOT a hard allocation ceiling: allocation
between samples, /proc scan time and shutdown can overshoot the budget.
No cgroup/shared service changes are made.  Child RLIMIT_AS stays <= 8 GiB.

Requires Linux prctl subreaping and Python pidfd signal support, so orphaned
descendants are adopted and every signal is protected against PID reuse.
The child starts a new session; the caller's group is never signalled.
--observe is only a measurement harness: sample without enforcing the budget.
--report FILE writes JSON evidence; it does not change enforcement.
"""

import argparse
import ctypes
import json
import os
from pathlib import Path
import re
import resource
import select
import signal
import subprocess
import sys
import time

INTERVAL = 0.05
AS_CAP = 8 * 1024 ** 3


def identity(pid):
    """Return (start ticks, parent, session), or None after exit/reap."""
    try:
        fields = Path(f"/proc/{pid}/stat").read_bytes().rsplit(b")", 1)[1].split()
        return int(fields[19]), int(fields[1]), int(fields[3])
    except (FileNotFoundError, ProcessLookupError):
        return None


class OlderProcesses:
    """Pin exclusions born before this supervisor, never its descendants."""

    def __init__(self):
        self.started = identity(os.getpid())[0]
        limit = resource.getrlimit(resource.RLIMIT_NOFILE)[0]
        if limit == resource.RLIM_INFINITY:
            self.capacity = 4096
        else:
            with os.scandir('/proc/self/fd') as entries:
                opened = sum(1 for _ in entries)
            self.capacity = max(0, min(4096, limit // 4, limit - opened - 64))
        self.poller = select.poll()
        self.by_pid = {}
        self.by_fd = {}

    def discard(self, fd):
        pid = self.by_fd.pop(fd)
        del self.by_pid[pid]
        self.poller.unregister(fd)
        try:
            os.close(fd)
        except OSError:
            pass
        return pid

    def close(self):
        for fd in list(self.by_fd):
            self.discard(fd)

    def expired(self):
        try:
            return [self.discard(fd) for fd, _events in self.poller.poll(0)]
        except OSError:
            # Failure of this optimization must restore the uncached census.
            pids = list(self.by_pid)
            self.close()
            self.capacity = 0
            return pids

    def remember(self, pid, value):
        if value[0] >= self.started or len(self.by_pid) >= self.capacity:
            return False
        fd = None
        try:
            fd = os.pidfd_open(pid)
            if identity(pid) != value:
                return False
            self.poller.register(fd, select.POLLIN)
            self.by_pid[pid] = fd
            self.by_fd[fd] = pid
            fd = None
            return True
        except ProcessLookupError:
            return False
        except OSError:
            self.close()
            self.capacity = 0
            return False
        finally:
            if fd is not None:
                os.close(fd)


def snapshot(known, older=None):
    if older is not None:
        older.expired()
    processes = {}
    skipped = set()
    for entry in os.scandir("/proc"):
        if entry.name.isdigit():
            pid = int(entry.name)
            if older is not None and pid not in known and pid in older.by_pid:
                skipped.add(pid)
                continue
            value = identity(pid)
            if value:
                if older is not None and pid not in known and older.remember(pid, value):
                    skipped.add(pid)
                    continue
                processes[pid] = value
    if older is not None:
        # A cached PID can exit and be reused during enumeration.  Read its
        # new identity before computing ownership, rather than skipping a scan.
        for pid in older.expired():
            if pid in skipped:
                value = identity(pid)
                if value:
                    processes[pid] = value
        if older.capacity == 0 and skipped:
            return snapshot(known)
    owned = {pid: start for pid, start in known.items()
             if pid in processes and processes[pid][0] == start}
    # Adopted grandchildren and descendants that changed session remain owned.
    changed = True
    while changed:
        changed = False
        for pid, (start, parent, _session) in processes.items():
            if pid != os.getpid() and pid not in owned and (
                    parent == os.getpid() or parent in owned):
                owned[pid] = start
                changed = True
    return owned


def aggregate(owned):
    pss = rss = 0
    complete_pss = True
    for pid, start in {**owned, os.getpid(): identity(os.getpid())[0]}.items():
        value = identity(pid)
        if not value or value[0] != start:
            continue
        try:
            resident = int(Path(f"/proc/{pid}/statm").read_text().split()[1])
            rss += resident * os.sysconf("SC_PAGE_SIZE")
            try:
                lines = Path(f"/proc/{pid}/smaps_rollup").read_text().splitlines()
                pss += next(int(line.split()[1]) * 1024
                            for line in lines if line.startswith("Pss:"))
            except (PermissionError, FileNotFoundError, StopIteration):
                # An exited process contributes no pages.  A live unreadable
                # process requires the conservative RSS bound for this sample.
                if identity(pid):
                    complete_pss = False
        except (FileNotFoundError, ProcessLookupError):
            continue
    return (pss, "pss") if complete_pss else (rss, "rss-upper-bound")


def send(owned, sig):
    for pid, start in list(owned.items()):
        fd = None
        try:
            fd = os.pidfd_open(pid)
            value = identity(pid)
            if value and value[0] == start:
                signal.pidfd_send_signal(fd, sig)
        except ProcessLookupError:
            pass
        finally:
            if fd is not None:
                os.close(fd)


def reap(child):
    while True:
        try:
            pid, status = os.waitpid(-1, os.WNOHANG)
            if pid == 0:
                return
            # An exit can race child.poll() in cleanup.  Preserve the exact
            # Popen root status if waitpid(-1) happens to reap it first.
            if pid == child.pid and child.returncode is None:
                child.returncode = os.waitstatus_to_exitcode(status)
        except ChildProcessError:
            return


def child_cap():
    soft, hard = resource.getrlimit(resource.RLIMIT_AS)
    hard = AS_CAP if hard == resource.RLIM_INFINITY else min(hard, AS_CAP)
    soft = hard if soft == resource.RLIM_INFINITY else min(soft, hard)
    resource.setrlimit(resource.RLIMIT_AS, (soft, hard))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--observe", action="store_true")
    parser.add_argument("--report")
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    command = args.command[1:] if args.command[:1] == ["--"] else args.command
    budget = os.environ.get("NGIT_MEMORY_BUDGET_MIB", "8192")
    if not re.fullmatch(r"[0-9]+", budget) or int(budget) <= 0:
        print("fatal: NGIT_MEMORY_BUDGET_MIB must be a positive integer", file=sys.stderr)
        return 2
    if not command:
        parser.error("command required")
    if not sys.platform.startswith("linux") or not hasattr(os, "pidfd_open") or not hasattr(signal, "pidfd_send_signal"):
        print("fatal: transport memory supervision requires Linux pidfd support", file=sys.stderr)
        return 2
    libc = ctypes.CDLL(None, use_errno=True)
    if libc.prctl(36, 1, 0, 0, 0) != 0:  # PR_SET_CHILD_SUBREAPER
        print("fatal: cannot enable transport descendant reaping", file=sys.stderr)
        return 2
    os.environ["NGIT_MEMORY_BUDGET_MIB"] = budget
    received = []
    for sig in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
        signal.signal(sig, lambda number, frame: received.append(number))
    started = time.monotonic()
    child = None
    known = {}
    peak = {"pss": 0, "rss-upper-bound": 0}
    reason = "exit"
    result = 2
    samples = 0
    older = OlderProcesses()
    try:
        child = subprocess.Popen(command, start_new_session=True, preexec_fn=child_cap)
        value = identity(child.pid)
        if value:
            known[child.pid] = value[0]
        while True:
            known = snapshot(known, older)
            used, metric = aggregate(known)
            samples += 1
            peak[metric] = max(peak[metric], used)
            if received:
                result, reason = 128 + received[0], "signal"
                send(known, received[0])
                break
            if not args.observe and used > int(budget) * 1048576:
                result, reason = 137, "budget"
                print(f"fatal: aggregate transport memory budget exceeded: {used / 1048576:.1f} MiB {metric} > {budget} MiB", file=sys.stderr)
                break
            code = child.poll()
            if code is not None:
                result = code if code >= 0 else 128 - code
                # A successful runtime may leave already-exited helper zombies.
                # Reap them before distinguishing live orphaned descendants.
                reap(child)
                if known:
                    # Snapshot may include the just-reaped root; refresh it.
                    known = snapshot(known)
                if known:
                    reason = "orphaned-descendants"
                    result = result or 1
                    print("fatal: transport exited leaving descendants; terminating owned tree", file=sys.stderr)
                break
            time.sleep(INTERVAL)
    except (OSError, ValueError) as error:
        reason = "supervisor-error"
        print(f"fatal: transport memory supervisor: {error}", file=sys.stderr)
    finally:
        # Exit and cleanup always use a full census, with descriptor headroom
        # restored before signaling or examining any late adopted children.
        older.close()
        if child is not None:
            # Adopt and kill new descendants during cleanup too.  No process
            # group signals, inherited groups or unrelated sentinel processes.
            deadline = time.monotonic() + 3
            first = True
            while True:
                known = snapshot(known)
                if not known:
                    break
                send(known, signal.SIGTERM if first else signal.SIGKILL)
                first = False
                child.poll()
                reap(child)
                if time.monotonic() >= deadline:
                    result = result or 1
                    print("fatal: transport descendant cleanup timed out", file=sys.stderr)
                    break
                time.sleep(INTERVAL)
            try:
                child.wait(timeout=0.2)
            except subprocess.TimeoutExpired:
                result = result or 1
                print("fatal: transport root did not exit within cleanup deadline", file=sys.stderr)
            reap(child)
        if args.report:
            report = {"budget_mib": int(budget), "interval_seconds": INTERVAL,
                      "elapsed_seconds": time.monotonic() - started,
                      "peak_bytes": peak, "samples": samples,
                      "reason": reason, "exit_status": result,
                      "observe_only": args.observe}
            Path(args.report).write_text(json.dumps(report, indent=2) + "\n")
    return result


if __name__ == "__main__":
    sys.exit(main())
