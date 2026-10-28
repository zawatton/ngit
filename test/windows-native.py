#!/usr/bin/env python3
"""Exercise PE ngit directly, on Windows or in an explicitly owned Wine prefix."""

import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import queue
import shutil
import subprocess
import sys
import tempfile
import threading
import time


ROOT = Path(__file__).resolve().parents[1]


class NativeTest:
    def __init__(self, args):
        self.args = args
        self.runtime = Path(args.runtime).resolve()
        with self.runtime.open("rb") as stream:
            if stream.read(2) != b"MZ":
                raise AssertionError("the test requires a native PE runtime")
        self.out = Path(args.artifacts).resolve()
        self.out.mkdir(parents=True, exist_ok=True)
        self.env = {key: value for key, value in os.environ.items() if not key.startswith("GIT_")}
        self.env.update(WINEDEBUG="-all", WINEDLLOVERRIDES="mscoree,mshtml=",
                        GIT_CONFIG_NOSYSTEM="1", GIT_TERMINAL_PROMPT="0",
                        GIT_AUTHOR_NAME="Windows Test", GIT_AUTHOR_EMAIL="test@example.invalid",
                        GIT_COMMITTER_NAME="Windows Test", GIT_COMMITTER_EMAIL="test@example.invalid",
                        GIT_AUTHOR_DATE="1730000000 +0000", GIT_COMMITTER_DATE="1730000000 +0000",
                        GIT_PAGER="cat", PAGER="cat", NGIT_NO_ELC="1")
        if args.wine:
            if not args.wine_prefix:
                raise AssertionError("--wine requires an explicit --wine-prefix")
            self.env.update(WINEPREFIX=str(Path(args.wine_prefix).resolve()), WINEARCH="win64")
        self.env["NGIT_NELISP_ROOT"] = self.winpath(args.nelisp_root)
        self.env["NGIT_WINDOWS_DLL_DIR"] = self.winpath(args.dll_dir)
        dll_dir = Path(args.dll_dir).resolve()
        self.env["PATH"] = ";".join([self.winpath(dll_dir), self.winpath(dll_dir.parent / "libexec/git-core"),
                                    self.winpath(dll_dir.parents[1] / "cmd"),
                                    "C:/windows/system32", "C:/windows"])
        self.env["GIT_EXEC_PATH"] = self.winpath(dll_dir.parent / "libexec/git-core")
        if args.wine:
            self.env["WINEPATH"] = self.env["PATH"]
        self.sequence = 0
        self.fixture_serial = 0
        self.results = []
        self.source_digest = self.sources()
        self.work = Path(tempfile.mkdtemp(prefix="native case ", dir=self.out))
        self.env["HOME"] = self.winpath(self.work)
        self.env["USERPROFILE"] = self.winpath(self.work)
        self.env["XDG_CONFIG_HOME"] = self.winpath(self.work / "xdg")
        self.env["GIT_CONFIG_GLOBAL"] = self.winpath(self.work / "absent.config")

    def winpath(self, path):
        path = str(Path(path).resolve())
        return "Z:" + path if self.args.wine else path.replace("\\", "/")

    def sources(self):
        files = sorted((ROOT / "lisp").glob("*.el")) + [ROOT / path for path in
                 ("bin/ngit-git", "bin/ngit.cmd", "bin/ngit-git.cmd", "tools/ngit-windows.py")]
        return {path.relative_to(ROOT).as_posix(): hashlib.sha256(path.read_bytes()).hexdigest() for path in files}

    def report(self, failure=None):
        stable = self.sources() == self.source_digest
        report = {"status": "FAIL" if failure or not stable else "PASS", "cases": self.results,
                  "failure": str(failure) if failure else (None if stable else "source changed during test"),
                  "source_sha256": self.source_digest,
                  "runtime_sha256": hashlib.sha256(self.runtime.read_bytes()).hexdigest(),
                  "commands": self.sequence}
        (self.out / "report.json").write_text(json.dumps(report, indent=2) + "\n")
        if not failure and not stable:
            raise AssertionError("source changed during test; result retained as FAIL")

    def run(self, exe, argv, cwd=None, input=None, status=0):
        command = ([self.args.wine] if self.args.wine else []) + [str(exe)] + argv
        self.sequence += 1
        record = self.out / ("%03d" % self.sequence)
        started = time.monotonic()
        try:
            result = subprocess.run(command, cwd=cwd or self.work, env=self.env,
                                    input=input, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                    timeout=self.args.timeout)
        except subprocess.TimeoutExpired as error:
            record.with_suffix(".stdout").write_bytes(error.stdout or b"")
            record.with_suffix(".stderr").write_bytes(error.stderr or b"")
            record.with_suffix(".json").write_text(json.dumps({"argv": command, "status": "timeout"}))
            raise
        record.with_suffix(".stdout").write_bytes(result.stdout)
        record.with_suffix(".stderr").write_bytes(result.stderr)
        record.with_suffix(".json").write_text(json.dumps({"argv": command, "status": result.returncode,
                                                         "elapsed": time.monotonic() - started}))
        if status is not None and result.returncode != status:
            raise AssertionError("exit %s, expected %s; %s" %
                                 (result.returncode, status, result.stderr[-600:].decode(errors="replace")))
        if b"uncaught" in result.stdout.lower() or b"uncaught" in result.stderr.lower():
            raise AssertionError("runtime reported an uncaught error; see " + str(record))
        return result

    def evaluate(self, form):
        load = self.winpath(ROOT / "lisp")
        prefix = '(progn (add-to-list \'load-path %s) ' % json.dumps(load)
        return self.run(self.runtime, ["--eval", prefix + form + ")"]).stdout

    def ngit(self, *argv, repo=None, input=None, status=0):
        argv = (["-C", self.winpath(repo)] if repo else []) + list(argv)
        return self.run(self.runtime, ["--load", self.winpath(ROOT / "lisp/ngit-main.el"),
                                      "--", "--@"] + ["@" + x for x in argv],
                        input=input, status=status)

    def git(self, *argv, repo=None, input=None, status=0):
        argv = (["-C", self.winpath(repo)] if repo else []) + list(argv)
        return self.run(Path(self.args.git).resolve(), argv, input=input, status=status)

    def fixture(self):
        self.fixture_serial += 1
        repo = self.work / ("repository-%d" % self.fixture_serial)
        self.git("init", "-q", "-b", "main", self.winpath(repo))
        (repo / "plain.txt").write_bytes(b"one\ntwo\n")
        (repo / "binary.dat").write_bytes(bytes(range(256)) * 8)
        self.git("add", ".", repo=repo)
        self.git("commit", "-qm", "fixture", repo=repo)
        return repo

    def runtime_case(self):
        output = self.evaluate('(list "NGIT-PE-OK" system-type (fboundp \'ptr-call))')
        assert b'("NGIT-PE-OK" windows-nt t)' in output, output[-500:]

    def foundation_case(self):
        source = self.work / "source.bin"
        packed = self.work / "packed.bin"
        source.write_bytes(bytes([0, 1, 127, 128, 255, 10, 13]))
        output = self.evaluate('''(progn (require 'ngit-native)
          (let* ((path %s) (target %s)
                 (bytes (ngit-native-read-bytes path 0 7))
                 (compressed (ngit-native-deflate-file path 7 1))
                 (fd (ngit-native-fd-open target #o1101 #o666))
                 (memory (nl-ffi-memory-allocate (length compressed))))
            (unwind-protect
                (progn (ptr-write-bytes (nl-ffi-memory-address memory) compressed)
                  (ngit-native-fd-write fd (nl-ffi-memory-address memory) (length compressed)))
              (ngit-native-fd-close fd) (nl-ffi-memory-release memory))
            (unless (equal bytes (car (ngit-native-inflate-file target 0 7)))
              (error "binary zlib round trip failed"))
            (list "NGIT-NATIVE-OK" (length (ngit-native-sha1 bytes)))))'''
                               % (json.dumps(self.winpath(source)), json.dumps(self.winpath(packed))))
        assert b'("NGIT-NATIVE-OK" 20)' in output, output[-500:]
        output = self.evaluate('''(progn (require 'ngit-native)
          (let* ((owner (nl-ffi-memory-allocate 7))
                 (address (nl-ffi-memory-address owner))
                 (forged (copy-sequence owner)) (rejected nil))
            (condition-case nil (nl-ffi-memory-release forged) (error (setq rejected t)))
            (unless rejected (error "forged memory owner accepted"))
            (nl-ffi-memory-release owner)
            (setq rejected nil)
            (condition-case nil (nl-ffi-memory-release owner) (error (setq rejected t)))
            (unless rejected (error "double memory release accepted"))
            (unless (equal (ngit-native-sha1 "abc")
               (unibyte-string 169 153 62 54 71 6 129 106 186 62 37 113 120 80 194 108 156 208 216 157))
              (error "known SHA1 mismatch"))
            (list "NGIT-OWNER-OK" (= 0 (% address 4096)))))''')
        assert b'("NGIT-OWNER-OK" t)' in output, output[-500:]

    def controls_case(self):
        previous = self.env["NGIT_WINDOWS_DLL_DIR"]
        self.env["NGIT_WINDOWS_DLL_DIR"] = self.winpath(self.work / "missing DLL directory")
        try:
            source = self.work / "control.bin"
            source.write_bytes(b"abc")
            result = self.ngit("hash-object", self.winpath(source), status=None)
            assert result.returncode != 0 and b"cannot load DLL" in result.stderr, result.stderr[-500:]
        finally:
            self.env["NGIT_WINDOWS_DLL_DIR"] = previous

    def process_case(self):
        if not self.args.python:
            raise AssertionError("process verification requires native --python")
        python = self.winpath(self.args.python)
        echo = "import sys\nwhile True:\n b=sys.stdin.buffer.read1(4096)\n if not b: break\n sys.stdout.buffer.write(b);sys.stdout.buffer.flush()"
        argv = "(list " + json.dumps(python) + ' "-c" ' + json.dumps(echo) + ")"
        output = self.evaluate('''(progn (require 'ngit-windows-process)
          (let* ((conn (ngit-win-process-spawn %s process-environment nil nil nil nil))
                 (bytes (apply #'unibyte-string (number-sequence 0 255))) (payload ""))
            (dotimes (_ 512) (setq payload (concat payload bytes)))
            (unwind-protect
                (progn (ngit-win-process-write conn payload)
                  (ngit-win-process-finish-write conn)
                  (unless (equal payload (ngit-win-process-read conn (length payload)))
                    (error "duplex bytes differ"))
                  (when (ngit-win-process-read conn 1) (error "expected EOF"))
                  (unless (= 0 (ngit-win-process-close conn)) (error "child exit"))
                  "NGIT-DUPLEX-OK")
              (ngit-win-process-close conn t))))''' % argv)
        assert b'"NGIT-DUPLEX-OK"' in output, output[-500:]
        arguments = ["", 'a"b', "ending\\", "\u65e5\u672c\u8a9e", "a&b%PATH%"]
        marker = "marker-\u03c0"
        expected = (json.dumps([marker, arguments]) + "\n").encode()
        code = "import os,sys,json;assert os.path.normcase(os.path.normpath(os.getcwd()))==os.path.normcase(os.path.normpath(sys.argv[1]));sys.stdout.buffer.write((json.dumps([os.getenv('NGIT_TEST_MARKER'),sys.argv[2:]])+'\\n').encode());sys.stderr.buffer.write(b'process-stderr-token\\n');sys.exit(7)"
        argv = "(list " + " ".join(json.dumps(arg, ensure_ascii=False) for arg in
                    [python, "-c", code, self.winpath(self.work), *arguments]) + ")"
        target = self.work / "process.json"
        output = self.evaluate('''(progn (require 'ngit-windows-process)
          (let ((conn (ngit-win-process-spawn %s
                       (cons %s process-environment) %s nil t nil)))
            (unwind-protect
                (let ((bytes (ngit-win-process-read conn %d)))
                  (ngit-win-write-file %s bytes t nil)
                  (when (ngit-win-process-read conn 1) (error "extra process output"))
                  (unless (= 7 (ngit-win-process-close conn)) (error "exit code lost"))
                  "NGIT-PROCESS-OK")
              (ngit-win-process-close conn t))))'''
                     % (argv, json.dumps("NGIT_TEST_MARKER=" + marker, ensure_ascii=False),
                        json.dumps(self.winpath(self.work)), len(expected), json.dumps(self.winpath(target))))
        assert b'"NGIT-PROCESS-OK"' in output and target.read_bytes() == expected, output[-500:]
        assert (self.out / ("%03d.stderr" % self.sequence)).read_bytes() == b"process-stderr-token\n"

    def process_saturation_case(self):
        """Bound continuous child stdout before stdin, preserving every owner."""
        import json
        import re
        import subprocess
        import time
        from pathlib import Path

        if not self.args.python:
            raise AssertionError("process saturation requires native --python")
        source = Path(__file__).resolve().parents[1] / "lisp/ngit-windows-process.el"
        ready = self.work / "stdout-child-ready"
        child = ("import os,sys,msvcrt;msvcrt.setmode(1,os.O_BINARY);"
                 "open(sys.argv[1],'wb').write(b'ready');chunk=b'x'*65536\n"
                 "while True: os.write(1,chunk)\n")
        quote = lambda value: json.dumps(value, ensure_ascii=False)
        form = r'''(progn
          (require 'ngit-windows)
          (load %s nil t)
          (ngit-win-process--thunk)
          (dolist (name '("GetHandleInformation" "GetTickCount64" "PeekNamedPipe"
                           "ReadFile" "WriteFile" "WaitForSingleObject" "Sleep"))
            (ngit-win-symbol "kernel32.dll" name))
          (let* ((memory (ngit-map-anon 4))
                 (baseline (hash-table-count ngit-win--allocations))
                 (conn (vconcat (ngit-win-process-spawn (list %s "-c" %s %s)
                         process-environment nil nil nil nil t t) %s))
                 (handles (list (aref conn 1) (aref conn 2) (aref conn 3) (aref conn 12)))
                 (call (symbol-function 'ngit-win-call))
                 (drain (symbol-function 'ngit-win-process--drain))
                 (reads 0) (maximum 0) (peak 0) (closed t) (outcome 0)
                 (start (ngit-win-call "kernel32.dll" "GetTickCount64")) elapsed balanced pending)
            (fset 'ngit-win-call
              (lambda (dll name &rest args)
                (when (equal name "ReadFile") (setq reads (1+ reads)))
                (let ((result (apply call dll name args)))
                  (when (equal name "CreateThread") (push result handles)) result)))
            (fset 'ngit-win-process--drain
              (lambda (connection)
                (setq reads 0)
                (unwind-protect (funcall drain connection)
                  (setq maximum (max maximum reads)
                        peak (max peak (length (or (aref connection 11) "")))))))
            (unwind-protect
                (condition-case err
                    (ngit-win-process-write conn (make-string 131072 ?i) (+ start %d))
                  (error
                   (let ((message (error-message-string err)))
                     (setq outcome
                       (cond ((equal message "Windows child input deadline expired") 1)
                             ((string-prefix-p "Windows child output limit exceeded" message) 2)
                             (t 0))))))
              (fset 'ngit-win-call call)
              (fset 'ngit-win-process--drain drain)
              (ngit-win-process-close conn t))
            (when ngit-win-process--pending-writes
              (ngit-win-call "kernel32.dll" "Sleep" 100)
              (ngit-win-process-write conn ""))
            (setq elapsed (- (ngit-win-call "kernel32.dll" "GetTickCount64") start))
            (dolist (handle handles)
              (unless (and (= 0 (ngit-win-call "kernel32.dll" "GetHandleInformation" handle memory))
                           (= 6 (ngit-win-call "kernel32.dll" "GetLastError")))
                (setq closed nil)))
            (setq balanced (= baseline (hash-table-count ngit-win--allocations))
                  pending (length ngit-win-process--pending-writes))
            (ngit-win-free memory)
            (princ (format "NGIT_SATURATION %%d %%d %%d %%d %%d %%d %%d\n"
                     outcome elapsed maximum peak (if closed 1 0) (if balanced 1 0) pending))))'''
        arguments = (quote(self.winpath(source)), quote(self.winpath(self.args.python)),
                     quote(child), quote(self.winpath(ready)))
        observations = []
        for limit, budget, accepted in ((None, 3000, (1, 2)), (131072, 10000, (2,))):
            ready.unlink(missing_ok=True)
            actual = form % (*arguments, "[]" if limit is None else "(vector %d)" % limit, budget)
            previous = self.args.timeout
            started = time.monotonic()
            try:
                self.args.timeout = min(previous, 45)
                output = self.evaluate(actual)
            except subprocess.TimeoutExpired as error:
                (self.out / "saturation-timeout.stdout").write_bytes(error.stdout or b"")
                (self.out / "saturation-timeout.stderr").write_bytes(error.stderr or b"")
                raise AssertionError("continuous stdout exceeded 45s bound; child ready=%s" %
                                     ready.exists()) from error
            finally:
                self.args.timeout = previous
            match = re.search(rb"NGIT_SATURATION (\d+) (\d+) (\d+) (\d+) (\d+) (\d+) (\d+)", output)
            assert match, output[-1200:]
            outcome, elapsed, maximum, peak, closed, balanced, pending = map(int, match.groups())
            observed = dict(limit=limit, outcome=outcome, elapsed_ms=elapsed,
                            max_reads_per_drain=maximum, peak_queue_bytes=peak,
                            handles_closed=closed, allocations_balanced=balanced,
                            pending_writes=pending, wall_seconds=time.monotonic() - started)
            observations.append(observed)
            (self.out / "saturation-observed.json").write_text(json.dumps(observations, indent=2) + "\n")
            assert ready.read_bytes() == b"ready", "native child never reached stdout producer"
            assert outcome in accepted and elapsed < budget + 7000, observed
            assert 0 < maximum <= 4 and peak <= (limit or 16 * 1024 * 1024), observed
            assert closed == balanced == 1 and pending == 0, observed


    def reads_case(self):
        repo = self.fixture()
        for args in [("status", "--porcelain"), ("log", "--format=%H"),
                     ("ls-files", "-z"), ("cat-file", "blob", "HEAD:binary.dat")]:
            actual = self.ngit(*args, repo=repo).stdout
            expected = self.git(*args, repo=repo).stdout
            assert actual == expected, (args, actual[-300:], expected[-300:])
        self.git("gc", "--prune=now", repo=repo)
        assert self.ngit("cat-file", "blob", "HEAD:binary.dat", repo=repo).stdout == (repo / "binary.dat").read_bytes()
        (repo / "plain.txt").write_bytes(b"changed\n")
        assert self.ngit("diff", repo=repo).stdout == self.git("diff", repo=repo).stdout

    def writes_case(self):
        repo = self.work / "new repository"
        self.ngit("init", self.winpath(repo))
        (repo / "unicode-\u65e5\u672c\u8a9e.txt").write_bytes(b"a\r\nb\x00\xff\n")
        self.ngit("add", ".", repo=repo)
        self.ngit("commit", "-m", "native Windows commit", repo=repo)
        self.git("fsck", "--full", repo=repo)
        assert self.ngit("status", "--porcelain", repo=repo).stdout == b""
        assert self.git("status", "--porcelain", repo=repo).stdout == b""
        self.ngit("branch", "other", repo=repo)
        self.ngit("checkout", "other", repo=repo)
        (repo / "unicode-\u65e5\u672c\u8a9e.txt").write_bytes(b"modified\n")
        self.ngit("reset", "--hard", "HEAD", repo=repo)
        assert (repo / "unicode-\u65e5\u672c\u8a9e.txt").read_bytes() == b"a\r\nb\x00\xff\n"
        (repo / "unicode-\u65e5\u672c\u8a9e.txt").write_bytes(b"restore replacement\n")
        self.ngit("restore", "--", "unicode-\u65e5\u672c\u8a9e.txt", repo=repo)
        assert (repo / "unicode-\u65e5\u672c\u8a9e.txt").read_bytes() == b"a\r\nb\x00\xff\n"
        before = (repo / ".git/index").read_bytes()
        (repo / ".git/index.lock").write_bytes(b"owned lock")
        result = self.ngit("add", ".", repo=repo, status=None)
        assert result.returncode != 0, "existing index lock must fail"
        assert (repo / ".git/index").read_bytes() == before
        assert (repo / ".git/index.lock").read_bytes() == b"owned lock"

    def transport_case(self):
        repo = self.fixture()
        remote = self.work / "remote.git"
        clone = self.work / "clone with spaces"
        # Native Git's local clone invokes its MSYS shell.  Construct the
        # same valid bare fixture without relying on that separate provider.
        shutil.copytree(repo / ".git", remote)
        self.git("--git-dir=" + self.winpath(remote), "config", "core.bare", "true")
        self.ngit("clone", self.winpath(remote), self.winpath(clone))
        self.git("fsck", "--full", repo=clone)
        (clone / "new.txt").write_bytes(b"new from Windows ngit\n")
        self.ngit("add", ".", repo=clone)
        self.ngit("commit", "-m", "push", repo=clone)
        self.ngit("push", "origin", "HEAD:refs/heads/native", repo=clone)
        assert self.git("rev-parse", "refs/heads/native", repo=remote).stdout == self.git("rev-parse", "HEAD", repo=clone).stdout
        self.ngit("fetch", "origin", repo=clone)

    def boundaries_case(self):
        repo = self.fixture()
        self.ngit("config", "--file", "-", "--get", "section.key", repo=repo,
                  input=b"[section]\r\nkey=value\r\n")
        result = self.ngit("config", "--file", "-", "--get", "section.key", repo=repo,
                           input=b"[section]\nkey=value\n")
        assert result.stdout == b"value\n", result.stdout
        actual = self.ngit("count-objects", "-v", repo=repo).stdout
        expected = self.git("count-objects", "-v", repo=repo).stdout
        assert actual == expected, (actual, expected)

    def paths_case(self):
        repo = self.fixture()
        invalid = ["x\\..\\.git\\config", ".GIT/config", ".g\u0131t/config", "git~1/config", "git~23/config",
                   "a:stream", "C:/escape", "//server/share", "a//b", "a/./b", "a/../b",
                   "trailing.", "trailing ", "NUL.txt", "NUL .txt", "CONIN$", "CONOUT$.log",
                   "COM\u00b9.txt", "LPT\u00b2.bin", "CON\u0131N$", "bad?name", "control\u001fname", "~root/escape", ""]
        valid = "\u65e5\u672c\u8a9e/space file-\U0001f600.txt"
        forms = "(list " + " ".join('(concat "control" (unibyte-string 31) "name")'
                                   if "\x1f" in path else json.dumps(path, ensure_ascii=False)
                                   for path in invalid) + ")"
        def snapshot():
            entries = sorted(repo.rglob("*"))
            return {str(path.relative_to(repo)): hashlib.sha256(path.read_bytes()).hexdigest()
                    if path.is_file() else "directory" for path in entries}

        def blob(data):
            return self.git("hash-object", "-w", "--stdin", repo=repo, input=data).stdout.strip()

        replacement = blob(b"must not replace the safe tracked file\n")
        metadata = blob(b"[core]\nrepositoryformatversion = 999\n")
        # Store raw tree bytes instead of asking Windows Git to create or
        # check out the forbidden names; PE ngit performs the actual checkout.
        for name in ("x\\..\\.git\\config", ".GIT/config", "git~1/config", "NUL.txt", "trailing.", "a:stream"):
            records = [(b"plain.txt", replacement), (name.encode(), metadata)]
            tree_bytes = b"".join(b"100644 " + path + b"\0" + bytes.fromhex(oid.decode())
                                  for path, oid in sorted(records))
            tree = self.git("hash-object", "--literally", "-t", "tree", "-w", "--stdin",
                            repo=repo, input=tree_bytes).stdout.strip()
            before = snapshot()
            result = self.ngit("checkout", tree.decode(), "--", ".", repo=repo, status=None)
            after = snapshot()
            changed = sorted(path for path in before.keys() | after.keys() if before.get(path) != after.get(path))
            with (self.out / "path-checkout.jsonl").open("a") as stream:
                stream.write(json.dumps({"name": name, "status": result.returncode, "changed": changed}) + "\n")
            assert result.returncode != 0 and b"unsafe Windows worktree path" in result.stderr, (name, result.stderr[-500:])
            assert after == before, "rejected checkout changed metadata/index/worktree: " + name

        collision = b"".join(b"100644 " + name + b"\0" + bytes.fromhex(replacement.decode())
                             for name in (b"Foo", b"foo"))
        tree = self.git("hash-object", "--literally", "-t", "tree", "-w", "--stdin",
                        repo=repo, input=collision).stdout.strip()
        before = snapshot()
        result = self.ngit("checkout", tree.decode(), "--", ".", repo=repo, status=None)
        assert result.returncode != 0 and b"colliding Windows worktree paths" in result.stderr, result.stderr[-500:]
        assert snapshot() == before, "case-colliding checkout changed metadata/index/worktree"

        patch = b"".join(b"diff --git a/" + name + b" b/" + name + b"\nnew file mode 100644\n"
                         b"--- /dev/null\n+++ b/" + name + b"\n@@ -0,0 +1 @@\n+new\n"
                         for name in (b"ApplyName", b"applyname"))
        for options in ((), ("--cached",)):
            before = snapshot()
            result = self.ngit("apply", *options, repo=repo, input=patch, status=None)
            assert result.returncode != 0 and b"colliding Windows worktree paths" in result.stderr, result.stderr[-500:]
            assert snapshot() == before, "case-colliding apply changed metadata/index/worktree"

        output = self.evaluate('''(progn (require 'ngit-native)
          (let ((rejected 0))
            (dolist (path %s)
              (condition-case nil (ngit-win-validate-worktree-path path)
                (error (setq rejected (1+ rejected)))))
            (unless (= rejected %d) (error "unsafe Windows path accepted"))
            (ngit-win-validate-worktree-path %s)
            (let ((collision nil))
              (condition-case nil
                  (ngit-win-validate-worktree-records (list (vector "π") (vector "Π")))
                (error (setq collision t)))
              (unless collision (error "Unicode case collision accepted")))
            (list "NGIT-PATHS-OK" rejected)))'''
                               % (forms, len(invalid), json.dumps(valid, ensure_ascii=False)))
        assert ('("NGIT-PATHS-OK" %d)' % len(invalid)).encode() in output, output[-500:]

        target = repo / valid
        target.parent.mkdir()
        original = b"valid Unicode binary\r\n\x00\xff\n"
        target.write_bytes(original)
        self.ngit("add", ".", repo=repo)
        assert self.ngit("status", "--porcelain", repo=repo).stdout == self.git("status", "--porcelain", repo=repo).stdout
        assert self.ngit("ls-files", "-z", repo=repo).stdout == self.git("ls-files", "-z", repo=repo).stdout
        target.write_bytes(b"replacement\n")
        self.ngit("restore", "--", valid, repo=repo)
        assert target.read_bytes() == original

    def http_case(self):
        spec = importlib.util.spec_from_file_location("windows_http_fixture", ROOT / "test/smart-http-fixture.py")
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        repo = self.fixture()
        remote = self.work / "http.git"
        shutil.copytree(repo / ".git", remote)
        self.git("--git-dir=" + self.winpath(remote), "config", "core.bare", "true")
        self.git("--git-dir=" + self.winpath(remote), "config", "http.receivepack", "true")
        with module.SmartHTTPServer(self.work, tls=False) as server:
            clone = self.work / "http clone"
            self.ngit("clone", server.url("http.git"), self.winpath(clone))
            self.git("fsck", "--full", repo=clone)
            (clone / "http.txt").write_bytes(b"native HTTP push\n")
            self.ngit("add", ".", repo=clone)
            self.ngit("commit", "-m", "http", repo=clone)
            self.ngit("push", "origin", "HEAD:refs/heads/http", repo=clone)
            self.ngit("fetch", "origin", repo=clone)
            assert self.git("rev-parse", "refs/heads/http", repo=remote).stdout == self.git("rev-parse", "HEAD", repo=clone).stdout

    def launcher_case(self):
        if not self.args.python:
            raise AssertionError("launcher verification requires --python native python.exe")
        self.env["NGIT_NELISP"] = self.winpath(self.runtime)
        self.env["NGIT_PYTHON"] = self.winpath(self.args.python)
        root = self.work / "ngit checkout with spaces"
        shutil.copytree(ROOT / "lisp", root / "lisp", ignore=shutil.ignore_patterns("*.elc", ".elc-*"))
        (root / "bin").mkdir()
        (root / "tools").mkdir()
        shutil.copy2(ROOT / "bin/ngit.cmd", root / "bin/ngit.cmd")
        shutil.copy2(ROOT / "bin/ngit-git", root / "bin/ngit-git")
        shutil.copy2(ROOT / "bin/ngit-git.cmd", root / "bin/ngit-git.cmd")
        shutil.copy2(ROOT / "tools/ngit-windows.py", root / "tools/ngit-windows.py")
        repo = self.fixture()
        argv = ["-C", self.winpath(repo), "rev-parse", "--show-toplevel"]
        result = self.run(Path(self.args.python).resolve(), [self.winpath(root / "tools/ngit-windows.py"), *argv])
        assert b"repository" in result.stdout, result.stdout
        cmd = "C:\\windows\\system32\\cmd.exe"
        batch = self.winpath(root / "bin/ngit.cmd").replace("/", "\\")
        command = '""' + batch + '" -C "' + self.winpath(repo) + '" rev-parse --show-toplevel"'
        raw = '"' + cmd + '" /d /s /c ' + command
        code = "import subprocess,sys;sys.exit(subprocess.run(" + repr(raw) + ").returncode)"
        result = self.run(Path(self.args.python).resolve(), ["-c", code])
        assert b"repository" in result.stdout, result.stdout
        self.env["NGIT_REAL_GIT"] = self.winpath(self.args.git)
        self.env["INSIDE_EMACS"] = "30.2,magit"
        self.env["NGIT_GIT_BACKEND"] = "auto"
        # The certificate deliberately rejects arbitrary GIT_* overrides.
        # Status does not need the transport-only fixture environment.
        self.env.pop("GIT_EXEC_PATH", None)
        self.env.pop("GIT_TERMINAL_PROMPT", None)
        self.env["GIT_ATTR_NOSYSTEM"] = "1"
        trace = self.work / "dispatch.jsonl"
        self.env["NGIT_GIT_TRACE"] = self.winpath(trace)
        result = self.run(Path(self.args.python).resolve(),
                          [self.winpath(ROOT / "bin/ngit-git"), "-C", self.winpath(repo), "status", "--porcelain"])
        assert result.stdout == b"", result.stdout
        rows = [json.loads(line) for line in trace.read_text().splitlines()]
        assert rows[-1]["backend"] == "ngit", rows
        batch = self.winpath(root / "bin/ngit-git.cmd").replace("/", "\\")
        raw = '"' + cmd + '" /d /s /c ""' + batch + '" -C "' + self.winpath(repo) + '" status --porcelain"'
        code = "import subprocess,sys;sys.exit(subprocess.run(" + repr(raw) + ").returncode)"
        result = self.run(Path(self.args.python).resolve(), ["-c", code])
        assert result.stdout == b"", result.stdout
        rows = [json.loads(line) for line in trace.read_text().splitlines()]
        assert rows[-1]["backend"] == "ngit", rows

    def status_case(self):
        repo = self.fixture()
        command = ([self.args.wine] if self.args.wine else []) + [str(self.runtime), "--load",
                   self.winpath(ROOT / "lisp/ngit-main.el"), "--", "--@", "@-C",
                   "@" + self.winpath(repo), "@status-server"]
        frames = queue.Queue()
        captured = bytearray()
        error_log = self.out / "status-server.stderr"
        with error_log.open("wb") as errors:
            process = subprocess.Popen(command, cwd=self.work, env=self.env,
                                       stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=errors)

            def read_frames():
                try:
                    while True:
                        header = process.stdout.readline()
                        if not header:
                            raise AssertionError("unexpected status-server EOF")
                        captured.extend(header)
                        assert header.endswith(b"\n") and b"\r" not in header, header
                        words = header.split()
                        assert words[0] in (b"OK", b"ERR"), header
                        size = int(words[-1])
                        assert 0 <= size <= 65536, header
                        body = bytearray()
                        while len(body) < size:
                            chunk = process.stdout.read(size - len(body))
                            assert chunk, "truncated response"
                            body.extend(chunk)
                        captured.extend(body)
                        frames.put((words[0], bytes(body)))
                except BaseException as error:
                    frames.put(error)

            reader = threading.Thread(target=read_frames, daemon=True)
            reader.start()

            def request(verb, code=b"OK"):
                process.stdin.write(verb + b"\n")
                process.stdin.flush()
                reply = frames.get(timeout=self.args.timeout)
                if isinstance(reply, BaseException):
                    raise reply
                assert reply[0] == code, reply
                return reply[1]

            try:
                assert request(b"status") == b""
                (repo / "plain.txt").write_bytes(b"external edit\n")
                expected = self.git("status", "--porcelain", repo=repo).stdout
                assert expected and request(b"status") == expected
                request(b"bad\x1a", b"ERR")
                assert request(b"status") == expected
                stats = request(b"stats")
                values = dict(line.split("=", 1) for line in stats.decode().splitlines())
                assert values["cache_enabled"] == "0", stats
                (repo / "plain.txt").write_bytes(b"one\ntwo\n")
                assert request(b"status") == b""
                request(b"quit")
                process.wait(timeout=10)
                assert process.returncode == 0, process.returncode
            finally:
                if process.poll() is None:
                    process.kill()
                    process.wait(timeout=10)
                process.stdin.close()
                process.stdout.close()
                reader.join(timeout=2)
                (self.out / "status-server.stdout").write_bytes(captured)
        assert b"uncaught" not in error_log.read_bytes().lower()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runtime", required=True)
    parser.add_argument("--nelisp-root", required=True)
    parser.add_argument("--dll-dir", required=True)
    parser.add_argument("--git", required=True, help="native Windows git.exe reference")
    parser.add_argument("--python", help="native Windows python.exe for launcher verification")
    parser.add_argument("--wine")
    parser.add_argument("--wine-prefix")
    parser.add_argument("--timeout", type=int, default=90)
    parser.add_argument("--artifacts", default=str(ROOT / "target/windows-native-tests"))
    parser.add_argument("--case", choices=["runtime", "foundation", "reads", "writes", "transport", "boundaries", "paths", "http", "launcher", "status", "process", "process_saturation", "controls", "all"], default="all")
    args = parser.parse_args()
    test = None
    try:
        test = NativeTest(args)
        cases = [args.case] if args.case != "all" else ["runtime", "foundation", "reads", "writes", "boundaries", "paths", "process", "process_saturation", "status", "transport", "http", "launcher", "controls"]
        for case in cases:
            getattr(test, case + "_case")()
            test.results.append(case)
            test.report()
            print("PASS " + case, flush=True)
    except (AssertionError, OSError, ValueError, KeyError, queue.Empty, subprocess.SubprocessError) as error:
        if test:
            test.report(error)
        print("FAIL " + str(error), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
