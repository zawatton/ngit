#!/usr/bin/env python3
"""Launch native PE NeLisp with lossless ngit argument framing."""

import os
from pathlib import Path
import subprocess
import sys


def main():
    root = Path(__file__).resolve().parents[1]
    runtime = os.environ.get("NGIT_NELISP", "nelisp.exe")
    os.environ["NGIT_NO_ELC"] = "1"
    command = [runtime, "--load", (root / "lisp/ngit-main.el").as_posix(),
               "--", "--@"] + ["@" + arg for arg in sys.argv[1:]]
    try:
        return subprocess.run(command).returncode
    except OSError as error:
        print("ngit: cannot start native NeLisp: " + str(error), file=sys.stderr)
        return 128


if __name__ == "__main__":
    sys.exit(main())
