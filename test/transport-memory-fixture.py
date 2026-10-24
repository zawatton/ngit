#!/usr/bin/env python3
"""Fake runtime with genuinely shared fork pages and private descendant pages."""
import os
from pathlib import Path
import resource
import sys
import time

mode = os.environ.get("NGIT_MEMORY_FIXTURE_MODE", "argv")
if mode == "argv":
    print("\n".join(sys.argv[1:]))
    sys.exit(0)
assert resource.getrlimit(resource.RLIMIT_AS)[0] <= 8 * 1024 ** 3
directory = Path(os.environ["NGIT_MEMORY_PID_DIR"])
directory.mkdir(exist_ok=True)
directory.joinpath(str(os.getpid())).write_text("parent")
shared = bytearray(72 * 1024 ** 2)
for index in range(0, len(shared), 4096):
    shared[index] = 1
children = []
for _ in range(2):
    pid = os.fork()
    if not pid:
        directory.joinpath(str(os.getpid())).write_text("child")
        if mode in ("private", "orphan", "signal"):
            private = bytearray(48 * 1024 ** 2)
            for index in range(0, len(private), 4096):
                private[index] = 2
        time.sleep(0.02 if mode.startswith("zombie") else 15 if mode in ("orphan", "signal") else 1.3)
        os._exit(0)
    children.append(pid)
if mode in ("orphan", "zombie", "zombie-error"):
    time.sleep(0.15)
    os._exit(23 if mode == "zombie-error" else 0)
if mode == "signal":
    time.sleep(15)
for pid in children:
    os.waitpid(pid, 0)
