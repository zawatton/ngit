#!/usr/bin/env python3
"""Strengthen existing real transport fsck calls without editing its tests."""
import os
import sys

args = sys.argv[1:]
if "fsck" in args:
    args = [arg for arg in args if arg != "--connectivity-only"]
    args.append("--full")
    with open(os.environ["NGIT_MEMORY_FSCK_LOG"], "a") as log:
        log.write("full fsck\n")
os.execv(os.environ["NGIT_MEMORY_REAL_GIT"], ["git", *args])
