#!/usr/bin/env python3
"""Validate recorded native Windows cases against the current application sources."""

import argparse
import copy
import hashlib
import json
from pathlib import Path
import re
import sys
import tempfile


ROOT = Path(__file__).resolve().parents[1]
ENTRIES = ("bin/ngit-git", "bin/ngit.cmd", "bin/ngit-git.cmd", "tools/ngit-windows.py")


def sources(root):
    paths = sorted((root / "lisp").glob("*.el")) + [root / name for name in ENTRIES]
    return {path.relative_to(root).as_posix(): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in paths}


def validate(report, cases, root):
    if report.get("status") != "PASS" or report.get("failure") is not None:
        raise ValueError("the recorded run failed")
    completed = report.get("cases")
    if not isinstance(completed, list) or not set(cases).issubset(completed):
        raise ValueError("requested cases did not complete")
    if not isinstance(report.get("commands"), int) or report["commands"] <= 0:
        raise ValueError("no commands were recorded")
    if not re.fullmatch(r"[0-9a-f]{64}", report.get("runtime_sha256", "")):
        raise ValueError("missing runtime fingerprint")
    recorded = report.get("source_sha256")
    actual = sources(root)
    if not recorded or recorded != actual:
        raise ValueError("application sources differ from the recorded run")
    return len(actual)


def self_test():
    with tempfile.TemporaryDirectory(prefix="ngit-evidence-") as directory:
        root = Path(directory)
        for name in (*ENTRIES, "lisp/example.el"):
            path = root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("fixture\n")
        good = dict(status="PASS", failure=None, cases=["fixture"], commands=1,
                    runtime_sha256="0" * 64, source_sha256=sources(root))
        validate(good, ["fixture"], root)
        broken = []
        for field, value in (("status", "FAIL"), ("cases", []),
                             ("source_sha256", {}), ("commands", 0),
                             ("runtime_sha256", "")):
            report = copy.deepcopy(good)
            report[field] = value
            broken.append(report)
        report = copy.deepcopy(good)
        del report["source_sha256"]["lisp/example.el"]
        broken.append(report)
        for report in broken:
            try:
                validate(report, ["fixture"], root)
            except ValueError:
                pass
            else:
                raise AssertionError("broken evidence was accepted")
        source = root / "lisp/example.el"
        for operation in (lambda: source.write_text("changed\n"), source.unlink):
            operation()
            try:
                validate(good, ["fixture"], root)
            except (ValueError, OSError):
                pass
            else:
                raise AssertionError("changed or missing source was accepted")
    print("checked=9 accepted=1 rejected=8")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--self-test", action="store_true")
    parser.add_argument("report", nargs="?")
    parser.add_argument("cases", nargs="*")
    args = parser.parse_args()
    try:
        if args.self_test:
            self_test()
        else:
            if not args.report or not args.cases:
                parser.error("a report and at least one case are required")
            report = json.loads(Path(args.report).read_text())
            checked = validate(report, args.cases, ROOT)
            print("checked=%d cases=%d" % (checked, len(args.cases)))
    except (ValueError, OSError, TypeError, AttributeError) as error:
        print("FAIL: " + str(error), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
