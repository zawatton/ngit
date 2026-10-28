#!/usr/bin/env python3
"""Black-box resident-status acceptance tests and calibrated latency benchmark."""
import argparse
import base64
import json
import os
from pathlib import Path
import re
import resource
import select
import signal
import statistics
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
REPORT = Path(os.environ.get("NGIT_STATUS_REPORT_DIR", ROOT / "target/status-session-20261028/tests"))
NGIT = Path(os.environ.get("NGIT_STATUS_BIN", ROOT / "bin/ngit")).resolve()
TIMEOUT = 30
BAD = re.compile(rb"uncaught error|void-function|void-variable|wrong-type-argument|Lisp error|out of memory|resource exhausted", re.I)
UNSUPPORTED = re.compile(rb"unsupported|not supported|cannot certify|uncertified|unproven|not eligible", re.I)
FIELDS = ("requests", "cache_hits", "full_refreshes", "incremental_updates",
          "tracked_entries_checked", "watch_rebuilds", "overflows", "cache_enabled")
MINIMUM = dict(protocol=9, resources=4, basic=4, incremental=7, invalidation=22,
               recovery=3, boundaries=7, e2e=7, controls=2, benchmark=1)
RESULTS = []
SERIAL = 0
RUN = "run"


def require(value, message):
    if not value:
        raise AssertionError(message)


def reap_group(process):
    """Reap the leader and also terminate any inherited owned descendants."""
    try:
        os.killpg(process.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    process.wait(timeout=TIMEOUT)


def environment(home):
    env = {k: v for k, v in os.environ.items()
           if not k.startswith(("GIT_", "NGIT_")) or k in ("NGIT_NELISP", "NGIT_NELISP_ROOT")}
    env.setdefault("NGIT_NELISP_ROOT", str(ROOT.parent / "nelisp"))
    env.update(HOME=str(home), XDG_CONFIG_HOME=str(home / "xdg"), LC_ALL="C", TZ="UTC",
               GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL=str(home / "global-config"),
               GIT_AUTHOR_NAME="Session Test", GIT_AUTHOR_EMAIL="session@example.invalid",
               GIT_COMMITTER_NAME="Session Test", GIT_COMMITTER_EMAIL="session@example.invalid",
               GIT_AUTHOR_DATE="2000-01-01T00:00:00 +0000", GIT_COMMITTER_DATE="2000-01-01T00:00:00 +0000",
               GIT_TERMINAL_PROMPT="0", GIT_PAGER="cat", GIT_OPTIONAL_LOCKS="0")
    return env


def execute(command, cwd, env, expected=0):
    global SERIAL
    SERIAL += 1
    prefix = REPORT / ("%s-command-%04d" % (RUN, SERIAL))
    # Files avoid a child blocking on a full diagnostic pipe.
    with prefix.with_suffix(".stdout").open("wb") as out, prefix.with_suffix(".stderr").open("wb") as err:
        process = subprocess.Popen([str(x) for x in command], cwd=cwd, env=env,
                                   stdin=subprocess.DEVNULL, stdout=out, stderr=err, start_new_session=True)
        try:
            code = process.wait(timeout=TIMEOUT)
        except subprocess.TimeoutExpired:
            reap_group(process)
            raise AssertionError("command timeout: " + repr(command))
        finally:
            reap_group(process)
    stdout, stderr = prefix.with_suffix(".stdout").read_bytes(), prefix.with_suffix(".stderr").read_bytes()
    require(not BAD.search(stdout + stderr), "runtime diagnostic: " + repr(stderr[:250]))
    if expected is not None:
        require(code == expected, "command exit %s: %r; stderr=%r" % (code, command, stderr[:250]))
    return code, stdout, stderr


class Fixture:
    def __init__(self, count=4):
        self.temp = tempfile.TemporaryDirectory(prefix="fixture-", dir=REPORT)
        self.base = Path(self.temp.name)
        self.repo = self.base / "repo"
        self.repo.mkdir()
        self.home = self.base / "home"
        self.home.mkdir()
        self.env = environment(self.home)
        self.git("init", "-q", "-b", "main")
        self.git("config", "core.autocrlf", "false")
        self.git("config", "core.filemode", "true")
        for n in range(count):
            self.write("f%05d" % n, "initial %d\n" % n)
        self.write("nested/tracked", "nested initial\n")
        self.git("add", ".")
        self.git("commit", "-q", "-m", "initial")

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.temp.cleanup()

    def git(self, *args, expected=0):
        return execute(["git", *args], self.repo, self.env, expected)[1]

    def write(self, name, text):
        path = self.repo / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)

    def oracle(self, options=()):
        code, out, err = execute(["git", *options, "status", "--porcelain=v1"], self.repo, self.env)
        require(not err, "Git status stderr: " + repr(err))
        return code, out


class Server:
    def __init__(self, fixture, options=(), command=None):
        global SERIAL
        SERIAL += 1
        self.fixture, self.options = fixture, tuple(options)
        self.log = REPORT / ("%s-server-%04d.stderr" % (RUN, SERIAL))
        self.err = self.log.open("wb")
        self.buffer = bytearray()
        self.finished = False
        try:
            self.process = subprocess.Popen(command or [str(NGIT), "-C", str(fixture.repo), *options, "status-server"],
                                            cwd=fixture.repo, env=fixture.env, stdin=subprocess.PIPE,
                                            stdout=subprocess.PIPE, stderr=self.err, start_new_session=True)
        except Exception:
            self.err.close()
            raise

    def __enter__(self):
        return self

    def __exit__(self, typ, value, trace):
        try:
            if not self.finished and typ is None:
                self.close()
        finally:
            reap_group(self.process)
            self.process.stdin.close()
            self.process.stdout.close()
            self.err.close()

    def write(self, data):
        require(self.process.poll() is None, "server exited before request")
        # Requests remain below the platform pipe capacity, except bounded oversized probes.
        os.set_blocking(self.process.stdin.fileno(), False)
        deadline = time.monotonic() + TIMEOUT
        view = memoryview(data)
        while view:
            require(time.monotonic() < deadline, "request write timeout")
            if select.select([], [self.process.stdin], [], max(0, deadline - time.monotonic()))[1]:
                try:
                    view = view[os.write(self.process.stdin.fileno(), view):]
                except BlockingIOError:
                    pass

    def take(self, count=None):
        deadline = time.monotonic() + TIMEOUT
        while (b"\n" not in self.buffer if count is None else len(self.buffer) < count):
            require(time.monotonic() < deadline, "frame read timeout")
            require(select.select([self.process.stdout], [], [], max(0, deadline - time.monotonic()))[0], "frame read timeout")
            chunk = os.read(self.process.stdout.fileno(), 65536)
            require(chunk, "unexpected EOF in frame: " + repr(bytes(self.buffer[:150])))
            self.buffer.extend(chunk)
            require(len(self.buffer) <= 64 * 1024 * 1024, "oversized server frame")
            require(count is not None or len(self.buffer) <= 4096 or b"\n" in self.buffer, "oversized frame header")
        end = self.buffer.index(10) + 1 if count is None else count
        out = bytes(self.buffer[:end])
        del self.buffer[:end]
        return out

    def frame(self):
        header = self.take()
        match = re.fullmatch(rb"(OK|ERR ([0-9]+)) ([0-9]+)\n", header)
        require(match, "invalid response header: " + repr(header[:150]))
        count = int(match[3])
        require(count <= 64 * 1024 * 1024, "oversized declared payload")
        return (int(match[2]) if match[2] else 0, self.take(count))

    def request(self, verb="status"):
        self.write(verb.encode("ascii") + b"\n")
        return self.frame()

    def stats(self):
        code, body = self.request("stats")
        require(code == 0, "stats failed: " + repr(body[:200]))
        pairs = dict(line.split("=", 1) for line in body.decode("ascii").splitlines())
        require(all(key in pairs for key in FIELDS), "missing stats fields: " + repr(pairs))
        result = {key: int(pairs[key]) for key in FIELDS}
        require(all(n >= 0 for n in result.values()), "negative stats counter")
        return result

    def verify(self, verb="status", ordinary=True, boundary=False):
        expected = self.fixture.oracle(self.options)
        actual = self.request(verb)
        if boundary and actual[0] != 0:
            require(UNSUPPORTED.search(actual[1]), "boundary refusal lacks explicit diagnostic: " + repr(actual))
            return "explicit refusal"
        require(actual == expected, "status differs: expected=%r actual=%r" % (expected, actual))
        if ordinary:
            code, out, err = execute([NGIT, "-C", self.fixture.repo, *self.options, "status", "--porcelain=v1"],
                                     self.fixture.repo, self.fixture.env, expected=None if boundary else 0)
            if boundary and code:
                require(UNSUPPORTED.search(out + err), "ordinary boundary failed without explicit refusal")
            else:
                require(not err and (code, out) == expected, "ordinary ngit differs: " + repr((code, out, err)))
        if boundary:
            stats = self.stats()
            require(stats["cache_enabled"] == 0, "unproven boundary still cache-enabled: " + repr(stats))
            return "fresh uncached output"
        return "oracle and ordinary ngit agree"

    def shutdown(self, eof=False, allow_stderr=False):
        if eof:
            self.process.stdin.close()
        else:
            require(self.request("quit") == (0, b""), "quit must acknowledge empty OK")
            self.process.stdin.close()
        try:
            code = self.process.wait(timeout=TIMEOUT)
        except subprocess.TimeoutExpired:
            raise AssertionError("server did not shut down")
        tail = self.closed_tail()
        require(code == 0 and not tail and not self.buffer, "shutdown exit/tail: " + repr((code, tail, self.buffer)))
        self.err.flush()
        diagnostic = self.log.read_bytes()
        require(not BAD.search(diagnostic), "server runtime diagnostic: " + repr(diagnostic[:250]))
        require(allow_stderr or not diagnostic, "unexpected server stderr: " + repr(diagnostic[:250]))
        self.finished = True

    def closed_tail(self):
        """Read EOF boundedly even if a broken child inherited the output pipe."""
        tail = bytearray()
        deadline = time.monotonic() + TIMEOUT
        while True:
            remaining = deadline - time.monotonic()
            require(remaining > 0 and select.select([self.process.stdout], [], [], max(0, remaining))[0],
                    "server output pipe remained open after exit")
            chunk = os.read(self.process.stdout.fileno(), 65536)
            if not chunk:
                return bytes(tail)
            tail.extend(chunk)
            require(len(tail) < 65536, "unexpected excessive output after exit")

    def close(self):
        self.shutdown()


def case(name, function):
    started = time.monotonic()
    try:
        detail = function()
        RESULTS.append(dict(name=name, ok=True, detail=detail, seconds=time.monotonic() - started))
    except Exception as error:
        RESULTS.append(dict(name=name, ok=False, detail=repr(error), seconds=time.monotonic() - started))


def protocol():
    def valid(variant):
        with Fixture() as f, Server(f) as s:
            if variant == "framing":
                f.write("space name", "untracked\n")
                s.verify()
            elif variant == "pipeline":
                s.write(b"status\nstats\nstatus\n")
                require(s.frame() == f.oracle(), "first pipelined frame differs")
                require(s.frame()[0] == 0, "pipelined stats failed")
                require(s.frame() == f.oracle(), "last pipelined frame differs")
            else:
                s.verify("refresh")
                s.stats()
    for variant in ("framing", "pipeline", "refresh"):
        case(variant, lambda v=variant: valid(v))
    def invalid(payload, closes=False):
        with Fixture() as f, Server(f) as s:
            marker = f.repo / "EVALUATED"
            s.write(payload)
            require(s.frame()[0] == 64, "malformed request did not return ERR 64")
            require(not marker.exists(), "request evaluated input")
            if closes:
                s.process.stdin.close()
                require(s.process.wait(timeout=TIMEOUT) == 0, "oversized EOF did not exit cleanly")
                require(not s.buffer and not s.closed_tail(), "unexpected oversized trailing output")
                s.err.flush()
                require(not s.log.read_bytes(), "oversized probe stderr")
                s.finished = True
            else:
                s.verify()
    for name, payload in (("unknown", b"invalid\n"), ("empty-line", b"\n"),
                          ("no-evaluation", b'(write-region "bad" nil "EVALUATED")\n'),
                          ("non-ascii", b"\xff\n")):
        case(name, lambda p=payload: invalid(p))
    case("oversized-unterminated", lambda: invalid(b"x" * 4097, True))
    case("oversized-line", lambda: invalid(b"x" * 4097 + b"\n", True))


def resources():
    def lifecycle(kind):
        before = len(list(Path("/proc/self/fd").iterdir()))
        with Fixture() as f:
            for _ in range(3):
                with Server(f) as s:
                    pid = s.process.pid
                    if kind != "empty-eof":
                        s.verify()
                        if kind == "repeated-refresh":
                            fd_before = len(list(Path("/proc/%d/fd" % pid).iterdir()))
                            for _ in range(12):
                                s.verify("refresh", ordinary=False)
                            fd_after = len(list(Path("/proc/%d/fd" % pid).iterdir()))
                            require(fd_after <= fd_before + 2, "server descriptors grew across refresh")
                    s.shutdown(eof=kind in ("empty-eof", "status-eof"))
                require(not Path("/proc/%d" % pid).exists(), "owned child remains after reap")
        require(len(list(Path("/proc/self/fd").iterdir())) <= before, "client descriptor leak")
    for kind in ("quit", "empty-eof", "status-eof", "repeated-refresh"):
        case(kind, lambda k=kind: lifecycle(k))


def basic():
    def scenario(kind):
        with Fixture() as f:
            if kind == "dirty":
                f.write("f00000", "changed\n")
            elif kind == "staged-and-unstaged":
                f.write("f00000", "staged\n")
                f.git("add", "f00000")
                f.write("f00000", "unstaged\n")
            elif kind == "quoted-untracked-collapse":
                for name in ("new/deep/file", "space name", "quote\"name", "line\nname", "日本語"):
                    f.write(name, "new\n")
            with Server(f) as s:
                s.verify()
                s.verify()
                s.stats()
    for kind in ("clean", "dirty", "staged-and-unstaged", "quoted-untracked-collapse"):
        case(kind, lambda k=kind: scenario(k))


def incremental():
    with Fixture() as f, Server(f) as s:
        s.verify()
        def warm():
            before = s.stats()
            s.verify()
            after = s.stats()
            require(after["cache_enabled"] == 1, "ordinary fixture cache disabled")
            require(after["cache_hits"] > before["cache_hits"], "unchanged request not a cache hit")
            require(after["tracked_entries_checked"] == before["tracked_entries_checked"], "cache hit rescanned entries")
            require(after["full_refreshes"] == before["full_refreshes"], "cache hit did a full refresh")
        case("unchanged-cache-hit", warm)
        def selected(name, mutation):
            before = s.stats()
            mutation()
            s.verify()
            after = s.stats()
            delta = after["tracked_entries_checked"] - before["tracked_entries_checked"]
            require(after["full_refreshes"] == before["full_refreshes"], name + " caused full refresh")
            require(after["incremental_updates"] > before["incremental_updates"], name + " not incremental")
            require(0 < delta < 5, "selected-entry count not selective: " + repr(delta))
        case("tracked-edit", lambda: selected("edit", lambda: f.write("f00001", "edited\n")))
        case("tracked-revert", lambda: selected("revert", lambda: f.write("f00001", "initial 1\n")))
        case("tracked-chmod", lambda: selected("chmod", lambda: (f.repo / "f00001").chmod(0o755)))
        def staged():
            f.write("f00000", "staged\n")
            f.git("add", "f00000")
            s.verify()
            selected("staged column", lambda: f.write("f00000", "unstaged after staging\n"))
            require(b"MM f00000\n" in s.request()[1], "staged column was lost")
        case("preserve-staged-column", staged)
        def hardlink():
            alias = f.base / "tracked-hardlink-alias"
            os.link(f.repo / "f00002", alias)
            selected("hardlink alias write", lambda: alias.write_text("external alias edit\n"))
        case("tracked-hardlink-alias", hardlink)
    def unreadable_retry():
        require(os.geteuid() != 0, "permission regression requires an actual unprivileged uid; root cannot certify chmod000")
        with Fixture() as f, Server(f) as s:
            s.verify()
            path = f.repo / "f00000"
            original = path.read_bytes()
            replacement = b"changed 0\n"
            require(len(original) == len(replacement), "permission regression must preserve file size")
            mode = path.stat().st_mode & 0o7777
            try:
                path.write_bytes(replacement)
                path.chmod(0)
                for attempt in (1, 2):
                    code, body = s.request()
                    require(code != 0, "unreadable request %d returned cached success: %r" % (attempt, body))
                    require(body.strip() and re.search(rb"permission|denied|cannot open|cannot read|unreadable|errno 13", body, re.I),
                            "unreadable request %d lacks a clear diagnostic: %r" % (attempt, body))
            finally:
                path.chmod(mode)
            return s.verify()
    case("unreadable-edit-retry-never-caches-success", unreadable_retry)


def atomic(path, data):
    temp = path.with_name(path.name + ".session-replacement")
    temp.write_bytes(data)
    temp.replace(path)


def invalidation():
    with Fixture() as f, Server(f) as s:
        s.verify()
        def change(name, mutation):
            before = s.stats()
            mutation()
            s.verify()
            after = s.stats()
            require(after["full_refreshes"] > before["full_refreshes"], name + " did not full-refresh")
        def add():
            f.write("f00000", "staged external\n")
            f.git("add", "f00000")
        def head():
            path = f.repo / ".git/HEAD"
            atomic(path, path.read_bytes())
        def index():
            path = f.repo / ".git/index"
            atomic(path, path.read_bytes())
        def ref():
            path = f.repo / ".git/refs/heads/main"
            atomic(path, path.read_bytes())
        def global_create():
            (f.home / "global-config").write_text("[core]\n quotePath = false\n")
            f.write("日本語", "new\n")
        def global_replace():
            atomic(f.home / "global-config", b"[core]\n quotePath = true\n")
        actions = [
            ("external-add", add),
            ("external-commit", lambda: f.git("commit", "-q", "-m", "external")),
            ("external-reset", lambda: f.git("reset", "-q", "HEAD~1")),
            ("external-checkout", lambda: f.git("checkout", "--", "f00000")),
            ("atomic-index", index), ("atomic-HEAD", head), ("atomic-ref", ref),
            ("ignore-create", lambda: (f.write(".gitignore", "ignored/\n"), f.write("ignored/secret", "hidden\n"))),
            ("ignore-replace", lambda: atomic(f.repo / ".gitignore", b"other/\n")),
            ("attributes-create", lambda: f.write(".gitattributes", "f00000 text\n")),
            ("attributes-replace", lambda: atomic(f.repo / ".gitattributes", b"f00000 -text\n")),
            ("local-config", lambda: f.git("config", "status.showUntrackedFiles", "no")),
            ("local-config-replace", lambda: f.git("config", "status.showUntrackedFiles", "normal")),
            ("global-config-create", global_create), ("global-config-replace", global_replace),
            ("nested-directory-create", lambda: f.write("unknown/deep/file", "new\n")),
            ("tracked-delete", lambda: (f.repo / "f00002").unlink()),
            ("tracked-type-change", lambda: ((f.repo / "f00003").unlink(), (f.repo / "f00003").symlink_to("f00000"))),
            ("rename", lambda: (f.repo / "f00001").rename(f.repo / "renamed")),
            ("intent-to-add", lambda: (f.write("ita", "intent\n"), f.git("add", "-N", "ita"))),
            ("ignored-tracked-ancestor", lambda: (f.write(".gitignore", "nested/\n"), f.write("nested/tracked", "edited hidden ancestor\n"))),
        ]
        for name, mutation in actions:
            case(name, lambda n=name, m=mutation: change(n, m))
    def unmerged():
        with Fixture() as f:
            f.git("checkout", "-q", "-b", "other")
            f.write("f00000", "other\n")
            f.git("commit", "-qam", "other")
            f.git("checkout", "-q", "main")
            f.write("f00000", "main\n")
            f.git("commit", "-qam", "main")
            with Server(f) as s:
                s.verify()
                f.git("merge", "other", expected=1)
                s.verify()
                before = s.stats()
                f.write("f00000", "resolution\n")
                s.verify()
                require(s.stats()["full_refreshes"] > before["full_refreshes"], "unmerged edit reused unsafe snapshot")
    case("unmerged-index", unmerged)


def recovery():
    def scenario(kind):
        with Fixture() as f, Server(f) as s:
            s.verify()
            before = s.stats()
            if kind == "watch-deletion-replacement":
                (f.repo / "nested/tracked").unlink()
                (f.repo / "nested").rmdir()
                f.write("nested/tracked", "replaced\n")
            elif kind == "event-storm":
                # Distinct names prevent inotify coalescing identical queued events.
                for n in range(4600):
                    f.write("nested/storm-%05d" % n, "storm\n")
                f.write("f00000", "final storm edit\n")
            else:
                f.write("f00000", "explicit\n")
                s.verify("refresh")
            s.verify()
            after = s.stats()
            require(after["full_refreshes"] > before["full_refreshes"], "recovery did not rebuild snapshot")
            if kind != "explicit-refresh":
                require(after["watch_rebuilds"] > before["watch_rebuilds"], "recovery did not rebuild watches")
            f.write("f00001", "continued progress\n")
            s.verify()
            return dict(before=before, after=after)
    for kind in ("watch-deletion-replacement", "event-storm", "explicit-refresh"):
        case(kind, lambda k=kind: scenario(k))


def boundaries():
    def scenario(kind):
        with Fixture() as f:
            if kind in ("include", "conditional-include", "symlink-policy"):
                policy = f.home / "policy"
                policy.write_text("[core]\n quotePath = false\n")
                if kind == "symlink-policy":
                    (f.home / "global-config").symlink_to(policy)
                else:
                    key = "include.path" if kind == "include" else "includeIf.gitdir:" + str(f.repo) + "/.path"
                    f.git("config", key, str(policy))
            elif kind == "external-index":
                index = f.base / "external-index"
                index.write_bytes((f.repo / ".git/index").read_bytes())
                f.env["GIT_INDEX_FILE"] = str(index)
            elif kind == "linked-worktree":
                linked = f.base / "linked"
                f.git("worktree", "add", "-q", "-b", "linked", str(linked))
                f.repo = linked
            elif kind == "filter":
                f.git("config", "filter.identity.clean", "cat")
                f.git("config", "filter.identity.smudge", "cat")
                f.write(".gitattributes", "f00000 filter=identity\n")
                f.git("add", ".gitattributes")
                f.git("commit", "-qm", "filter policy")
            elif kind == "submodule":
                child = f.base / "child"
                child.mkdir()
                execute(["git", "init", "-q", child], f.base, f.env)
                (child / "file").write_text("child\n")
                execute(["git", "add", "."], child, f.env)
                execute(["git", "commit", "-qm", "child"], child, f.env)
                f.git("-c", "protocol.file.allow=always", "submodule", "add", "-q", str(child), "sub")
                f.git("commit", "-qm", "submodule")
            with Server(f) as s:
                first = s.verify(boundary=True)
                f.write("f00000", "boundary edit\n")
                f.write("日本語", "boundary untracked\n")
                if kind in ("include", "conditional-include", "symlink-policy"):
                    atomic(policy, b"[core]\n quotePath = true\n")
                elif kind == "external-index":
                    f.git("add", "f00000")
                elif kind == "submodule":
                    f.write("sub/file", "changed submodule worktree\n")
                second = s.verify(boundary=True)
                return dict(initial=first, changed=second)
    for kind in ("include", "conditional-include", "symlink-policy", "linked-worktree", "external-index", "submodule", "filter"):
        case(kind, lambda k=kind: scenario(k))


def e2e():
    with Fixture() as f, Server(f) as s:
        case("initial", s.verify)
        def step(mutation):
            mutation()
            return s.verify()
        case("edit-and-untracked", lambda: step(lambda: (f.write("f00000", "edited\n"), f.write("new/deep", "new\n"))))
        case("external-stage", lambda: step(lambda: f.git("add", ".")))
        case("external-commit", lambda: step(lambda: f.git("commit", "-qm", "editor save")))
        case("edit-revert", lambda: step(lambda: (f.write("f00000", "temporary\n"), f.git("checkout", "--", "f00000"))))
        case("reset-history", lambda: step(lambda: f.git("reset", "-q", "HEAD~1")))
        case("refresh-and-continue", lambda: (s.verify("refresh"), step(lambda: f.write("nested/tracked", "continued\n"))))


FAKE = r'''
import base64, sys
snapshot = base64.b64decode(sys.argv[1])
for request in sys.stdin.buffer:
    if request == b"quit\n":
        sys.stdout.buffer.write(b"OK 0\n"); sys.stdout.buffer.flush(); break
    sys.stdout.buffer.write(b"OK " + str(len(snapshot)).encode() + b"\n" + snapshot)
    sys.stdout.buffer.flush()
'''


def stale_control():
    with Fixture() as f:
        snapshot = f.oracle()[1]
        command = [sys.executable, "-u", "-c", FAKE, base64.b64encode(snapshot).decode()]
        with Server(f, command=command) as s:
            s.verify(ordinary=False)
            f.write("f00000", "actual edit after cached snapshot\n")
            try:
                s.verify(ordinary=False)
            except AssertionError as error:
                require("status differs" in str(error), "control failed for wrong reason")
                return "same protocol/oracle rejected deliberately stale output"
            raise AssertionError("stale implementation passed verifier")


def controls(stale_only=False):
    case("stale-fake-rejected", stale_control)
    if stale_only:
        return
    def baseline():
        configured = os.environ.get("NGIT_STATUS_BASELINE")
        require(configured, "NGIT_STATUS_BASELINE is required")
        path = Path(configured).resolve()
        binary = path / "bin/ngit" if path.is_dir() else path
        require(binary.is_file(), "baseline binary missing")
        with Fixture() as f:
            # A nested baseline still uses the current sibling runtime checkout.
            f.env.setdefault("NGIT_NELISP_ROOT", str(ROOT.parent / "nelisp"))
            code, out, err = execute([binary, "-C", f.repo, "status-server"], f.repo, f.env, expected=None)
            require(code != 0 and not re.match(rb"(?:OK|ERR) [0-9]", out), "pre-feature baseline passed protocol")
            require(re.search(rb"unknown|unsupported|unrecognized|not.*command", out + err, re.I),
                    "baseline rejected for unrelated reason: " + repr((code, out[:150], err[:150])))
            return "frozen pre-feature CLI rejects status-server"
    case("pre-feature-baseline-rejected", baseline)


def benchmark():
    def measure():
        count = int(os.environ.get("NGIT_STATUS_BENCH_FILES", "1200"))
        require(1000 <= count <= 5000, "benchmark fixture must have 1000..5000 requested files")
        with Fixture(count) as f:
            f.git("update-index", "--test-untracked-cache")
            f.git("config", "core.untrackedCache", "true")
            f.git("update-index", "--untracked-cache")
            require(not f.git("config", "--get", "core.fsmonitor", expected=1), "unexpected fsmonitor")
            f.write("untracked/deep/file", "visible\n")
            prime_env = dict(f.env, GIT_OPTIONAL_LOCKS="1")
            execute(["git", "status", "--porcelain=v1"], f.repo, prime_env)
            execute(["git", "status", "--porcelain=v1"], f.repo, prime_env)
            expected = f.oracle()
            def native_sample():
                global SERIAL
                SERIAL += 1
                prefix = REPORT / ("%s-benchmark-git-%04d" % (RUN, SERIAL))
                with prefix.with_suffix(".stderr").open("wb") as err:
                    start = time.perf_counter_ns()
                    process = subprocess.Popen(["git", "status", "--porcelain=v1"], cwd=f.repo, env=f.env,
                                               stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                                               stderr=err, start_new_session=True)
                    try:
                        output, _ = process.communicate(timeout=TIMEOUT)
                        elapsed = time.perf_counter_ns() - start
                    finally:
                        reap_group(process)
                diagnostic = prefix.with_suffix(".stderr").read_bytes()
                prefix.with_suffix(".stdout").write_bytes(output)
                require(not diagnostic and process.returncode == 0, "measured Git failed")
                return (process.returncode, output), elapsed
            cold_start = time.perf_counter_ns()
            with Server(f) as s:
                require(s.request() == expected, "cold status mismatch")
                cold = time.perf_counter_ns() - cold_start
                s.verify(ordinary=True)
                before = s.stats()
                warm, native, edits = [], [], []
                # Alternate measured implementations to reduce order/thermal bias.
                for n in range(24):
                    order = ("resident", "git") if n % 2 == 0 else ("git", "resident")
                    for impl in order:
                        if impl == "resident":
                            start = time.perf_counter_ns()
                            actual = s.request()
                            elapsed = time.perf_counter_ns() - start
                        else:
                            actual, elapsed = native_sample()
                        require(actual == expected, "measured output differs: " + impl)
                        (warm if impl == "resident" else native).append(elapsed)
                after = s.stats()
                require(after["cache_hits"] - before["cache_hits"] >= 24, "warm samples not certified cache hits")
                require(after["tracked_entries_checked"] == before["tracked_entries_checked"], "warm samples rescanned files")
                require(after["full_refreshes"] == before["full_refreshes"], "warm samples regenerated status")
                for n in range(8):
                    f.write("f00000", "measured edit %d\n" % n)
                    expected = f.oracle()
                    start = time.perf_counter_ns()
                    actual = s.request()
                    edits.append(time.perf_counter_ns() - start)
                    require(actual == expected, "edit refresh mismatch")
                final = s.stats()
                require(final["incremental_updates"] - after["incremental_updates"] >= 8, "edits were not incremental")
                require(final["full_refreshes"] == after["full_refreshes"], "edits caused full regeneration")
                data = dict(tracked_files=count + 1, cwd="same temporary fixture", output_bytes=len(expected[1]),
                            cold_startup_first_status_ns=cold, warm_roundtrip_ns=warm,
                            git_cli_ns=native, tracked_edit_refresh_ns=edits,
                            warm_median_ns=statistics.median(warm), git_median_ns=statistics.median(native),
                            edit_median_ns=statistics.median(edits), counters_before=before,
                            counters_after=after, counters_after_edits=final,
                            settings=dict(untracked_cache=True, untracked_cache_capability="verified",
                                          git_cache_priming_statuses=2, fsmonitor="absent", GIT_OPTIONAL_LOCKS="0"),
                            claim="warm resident round trips only; no cold or universal superiority claim")
                (REPORT / "benchmark-samples.json").write_text(json.dumps(data, indent=2) + "\n")
                require(data["warm_median_ns"] < data["git_median_ns"], "warm resident median does not beat Git CLI")
                return data
    case("verified-warm-roundtrip-latency", measure)


def main():
    global RUN
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=tuple(MINIMUM))
    parser.add_argument("--stale-only", action="store_true", help="calibrate fake server before backend is available")
    args = parser.parse_args()
    RUN = "%s-%d" % (args.mode, os.getpid())
    require(not args.stale_only or args.mode == "controls", "--stale-only requires controls")
    cap = 8 * 1024 * 1024 * 1024
    soft, hard = resource.getrlimit(resource.RLIMIT_AS)
    resource.setrlimit(resource.RLIMIT_AS, (min(cap, soft) if soft != resource.RLIM_INFINITY else cap, hard))
    REPORT.mkdir(parents=True, exist_ok=True)
    try:
        if args.mode == "controls":
            controls(args.stale_only)
        else:
            globals()[args.mode]()
    except Exception as error:
        RESULTS.append(dict(name="mode-completion", ok=False, detail=repr(error)))
    expected_count = 1 if args.stale_only else MINIMUM[args.mode]
    if len(RESULTS) < expected_count:
        RESULTS.append(dict(name="scenario-count", ok=False,
                            detail="checked %d scenarios; require >=%d" % (len(RESULTS), expected_count)))
    findings = sum(not row["ok"] for row in RESULTS)
    payload = dict(mode=args.mode, stale_only=args.stale_only, checked=len(RESULTS), findings=findings,
                   complete=True, results=RESULTS)
    suffix = "-stale-only" if args.stale_only else ""
    (REPORT / (args.mode + suffix + ".json")).write_text(json.dumps(payload, indent=2) + "\n")
    print("checked=%d findings=%d %s mode=%s complete=1%s" %
          (len(RESULTS), findings, "FAIL" if findings else "PASS", args.mode, suffix))
    for row in RESULTS:
        if not row["ok"]:
            print("  %s: %s" % (row["name"], str(row["detail"])[:350]))
    return bool(findings)


if __name__ == "__main__":
    sys.exit(main())
