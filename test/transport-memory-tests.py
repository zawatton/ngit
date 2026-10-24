#!/usr/bin/env python3
"""Bounded policy and cleanup checks, including an executable old-bin control."""
import importlib.util
import hashlib
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
import time
import zlib

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "target/memory-lane/checks"
OUT.mkdir(parents=True, exist_ok=True)
BIN = ROOT / "bin/ngit"
FIXTURE = ROOT / "test/transport-memory-fixture.py"
FIXTURE.chmod(0o755)
ENV = dict(os.environ, NGIT_NELISP=str(FIXTURE))


def run(command, env=ENV, **kwargs):
    return subprocess.run(command, env=env, text=True, capture_output=True,
                          timeout=25, **kwargs)


def fixture(binary, mode, name, budget="128", extra=()):
    directory = OUT / name
    shutil.rmtree(directory, ignore_errors=True)
    directory.mkdir()
    env = dict(ENV, NGIT_MEMORY_FIXTURE_MODE=mode,
               NGIT_MEMORY_PID_DIR=str(directory), NGIT_MEMORY_BUDGET_MIB=budget)
    result = run([*extra, str(binary), "-C", ".", "-C", ".", "clone", "a", "b"], env)
    return result, directory


def dead(directory):
    # Reaped means /proc entry gone, including zombies.  The supervisor must
    # reap adopted grandchildren rather than just signal them.
    for entry in directory.iterdir():
        if entry.name.isdigit():
            assert not Path("/proc", entry.name).exists(), f"unreaped {entry.name}"


def policy():
    plain = run([str(BIN), "-C", ".", "-C", ".", "status", "-q", "a b"])
    assert plain.returncode == 0 and "@-q\n@a b" in plain.stdout
    # A deliberately unusable Python on PATH proves the ordinary fast path.
    no_python = OUT / "no-python"
    no_python.mkdir(exist_ok=True)
    (no_python / "python3").write_text("#!/bin/sh\nexit 99\n")
    (no_python / "python3").chmod(0o755)
    shell_runtime = OUT / "shell-runtime"
    shell_runtime.write_text("#!/bin/sh\nprintf '%s\\n' \"$@\"\n")
    shell_runtime.chmod(0o755)
    env = dict(os.environ, PATH=str(no_python) + ":" + os.environ["PATH"], NGIT_NELISP=str(shell_runtime))
    env.pop("NGIT_MEMORY_BUDGET_MIB", None)
    result = run([str(BIN), "status"], env)
    assert result.returncode == 0, result.stderr
    for bad in ("", "0", "-1", "1.5", "abc", " 128"):
        result = run([str(BIN), "status"], dict(ENV, NGIT_MEMORY_BUDGET_MIB=bad))
        assert result.returncode == 2 and "positive integer" in result.stderr, (bad, result)
    elisp = """(progn (require 'ngit-transport-memory)
      (dolist (pair '((\"8192\" 8 238609294) (\"1536\" 2 134217728) (\"512\" 1 67108864)))
        (setenv \"NGIT_MEMORY_BUDGET_MIB\" (car pair))
        (let ((workers (ngit-transport-memory-workers 8)))
          (unless (and (= workers (nth 1 pair))
                       (= (ngit-transport-memory-cache-bytes workers 1073741824) (nth 2 pair)))
            (error \"memory policy mismatch %S\" pair))))
      (princ \"memory-policy: PASS\\n\"))"""
    result = run(["emacs", "--batch", "-Q", "-L", str(ROOT / "lisp"), "--eval", elisp])
    assert result.returncode == 0, result.stderr
    spec = importlib.util.spec_from_file_location("memory_guard", ROOT / "tools/transport-memory.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    identity_parser(module)
    original = module.Path.read_text
    def unreadable_rollup(path, *args, **kwargs):
        if path.name == "smaps_rollup":
            raise PermissionError("test unreadable PSS")
        return original(path, *args, **kwargs)
    module.Path.read_text = unreadable_rollup
    try:
        used, metric = module.aggregate({})
        assert used > 0 and metric == "rss-upper-bound"
    finally:
        module.Path.read_text = original
    cache_copy()
    print("transport-memory policy: PASS (invalid budgets, worker/cache policy, no-Python ordinary argv)")


def identity_parser(module):
    fields = [b"0"] * 30
    fields[0], fields[1], fields[3], fields[19] = b"S", b"17", b"29", b"123456789"
    original = module.Path
    try:
        for comm in (b"ordinary", b"embedded ) space", b"raw\xff",
                     b"truncated\xe3\x81", b"mixed \xe6\x97\xa5 ) \xf0\x9f"):
            raw = b"123 (" + comm + b") " + b" ".join(fields)
            class PrivateStat:
                def read_text(self):
                    return raw.decode("utf-8")
                def read_bytes(self):
                    return raw
            # A module-private source avoids changing any live /proc comm.
            module.Path = lambda path: PrivateStat()
            assert module.identity(123) == (123456789, 17, 29), comm
    finally:
        module.Path = original
    print("transport-memory stat parser: PASS (ordinary/raw/truncated UTF8, embedded parenthesis/space, numeric fields)")


def cache_copy():
    size = 2097152
    header = bytearray([0x30 | (size & 15)])
    value = size >> 4
    while value:
        header[-1] |= 128
        header.append(value & 127)
        value >>= 7
    data = b"PACK" + (2).to_bytes(4, "big") + (1).to_bytes(4, "big") + header + zlib.compress(b"x" * size)
    pack = OUT / "direct.pack"
    pack.write_bytes(data + hashlib.sha1(data).digest())
    reference = OUT / "direct.reference.idx"
    run(["git", "index-pack", "-o", str(reference), str(pack)])
    baseline = OUT / "cache-baseline"
    shutil.rmtree(baseline, ignore_errors=True)
    baseline.mkdir()
    archive = subprocess.Popen(["git", "archive", os.environ.get("NGIT_MEMORY_BASE_REV", "e38c3c0"), "lisp"], cwd=ROOT, stdout=subprocess.PIPE)
    subprocess.run(["tar", "-x", "-C", str(baseline)], stdin=archive.stdout, check=True, timeout=10)
    archive.stdout.close()
    assert archive.wait(timeout=5) == 0
    common = Path(subprocess.check_output(["git", "rev-parse", "--git-common-dir"], cwd=ROOT, text=True).strip()).resolve()
    env = dict(os.environ, NGIT_NELISP_ROOT=os.environ.get("NGIT_NELISP_ROOT", str(common.parent.parent / "nelisp")), NGIT_MEMORY_BUDGET_MIB="8", NGIT_TRANSPORT_PACK=str(pack))
    for name, source in (("old", baseline), ("new", ROOT)):
        index = OUT / ("direct." + name + ".idx")
        env.update(NGIT_MEMORY_CACHE_SOURCE=str(source), NGIT_TRANSPORT_IDX=str(index))
        result = run([os.environ.get("NGIT_NELISP", "nelisp"), "--load", str(ROOT / "test/transport-memory-cache.el")], env)
        if name == "old":
            assert result.returncode != 0 and "uncacheable direct native copy" in result.stderr, result
        else:
            assert result.returncode == 0, result.stderr
            assert index.read_bytes() == reference.read_bytes()
    print("transport-memory direct cache: old RED/new GREEN; uncacheable native copy avoided; exact Git index")


def cleanup():
    baseline = OUT / "old-ngit"
    baseline.write_bytes(subprocess.check_output(["git", "show", os.environ.get("NGIT_MEMORY_BASE_REV", "e38c3c0") + ":bin/ngit"], cwd=ROOT))
    baseline.chmod(0o755)
    sentinel = subprocess.Popen(["sleep", "30"])
    try:
        old, oldp = fixture(baseline, "private", "old-private")
        assert old.returncode == 0, (old.returncode, old.stderr)
        dead(oldp)
        new, newp = fixture(BIN, "private", "new-private")
        assert new.returncode == 137 and "budget exceeded" in new.stderr, new
        dead(newp)
        assert sentinel.poll() is None, "unrelated sentinel killed"
        shared, sharedp = fixture(BIN, "shared", "new-shared")
        assert shared.returncode == 0, shared.stderr
        dead(sharedp)
        zombie, zombiep = fixture(BIN, "zombie", "new-zombie", "512")
        assert zombie.returncode == 0, zombie.stderr
        dead(zombiep)
        failed, failedp = fixture(BIN, "zombie-error", "new-zombie-error", "512")
        assert failed.returncode == 23, (failed.returncode, failed.stderr)
        dead(failedp)
        orphan, orphanp = fixture(BIN, "orphan", "new-orphan", "512")
        assert orphan.returncode != 0 and "leaving descendants" in orphan.stderr, orphan
        dead(orphanp)
        directory = OUT / "signal"
        shutil.rmtree(directory, ignore_errors=True)
        directory.mkdir()
        env = dict(ENV, NGIT_MEMORY_FIXTURE_MODE="signal", NGIT_MEMORY_PID_DIR=str(directory), NGIT_MEMORY_BUDGET_MIB="512")
        process = subprocess.Popen([str(BIN), "fetch"], env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        deadline = time.monotonic() + 5
        while len(list(directory.iterdir())) < 3 and time.monotonic() < deadline:
            time.sleep(0.02)
        assert len(list(directory.iterdir())) == 3
        process.send_signal(signal.SIGTERM)
        process.communicate(timeout=5)
        assert process.returncode == 143, process.returncode
        dead(directory)
        assert sentinel.poll() is None
    finally:
        sentinel.terminate()
        sentinel.wait()
    print("transport-memory cleanup: PASS; genuine old-bin budget contract RED (exit0), new GREEN (137); shared fork pages succeed; private pages fail; orphans/signals reaped; sentinel survives")


globals()[sys.argv[1]]()
