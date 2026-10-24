#!/usr/bin/env python3
"""Deterministic three-way mutations compared byte for byte with Git."""

import json
import os
from pathlib import Path
import random
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parent.parent
ENV = dict(os.environ, GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL=os.devnull,
           LC_ALL="C", GIT_CONFIG_COUNT="0")


def main():
    count = int(sys.argv[1]) if len(sys.argv) > 1 else 100
    rng = random.Random(int(os.environ.get("NGIT_FUZZ_SEED0", "1")))
    words = [b"a", b"b", b"c", b"", b"}", b"{", b"   ", b"\t", b";", b"alpha()", b"\xff"]
    cases = []
    expected = []
    with tempfile.TemporaryDirectory(prefix="ngit-merge-fuzz-") as tmp:
        tmp = Path(tmp)

        def add(base, ours, theirs, style, favor="none", marker=7, level=3, result=None):
            directory = tmp / str(len(cases))
            directory.mkdir()
            for name, content in zip(("base", "ours", "theirs"), (base, ours, theirs)):
                (directory / name).write_bytes(content)
            if result is None:
                options = ["-p", f"--marker-size={marker}"]
                if style != "merge":
                    options.append("--" + style)
                if favor != "none":
                    options.append("--" + favor)
                p = subprocess.run(["git", "merge-file", *options, "ours", "base", "theirs"],
                                   cwd=directory, env=ENV, capture_output=True, check=False)
                assert not p.stderr and p.returncode < 128, p.stderr
                result = (p.stdout, p.returncode)
            cases.append([str(directory), style, favor, marker, level])
            expected.append(result)

        for _ in range(count):
            eol = rng.choice([b"\n"] * 4 + [b"\r\n"])
            lines = [rng.choice(words) for _ in range(rng.randrange(45))]

            def mutate():
                out = lines[:]
                for _ in range(rng.randrange(1, 8)):
                    start = rng.randrange(len(out) + 1)
                    end = min(len(out), start + rng.randrange(7))
                    out[start:end] = [rng.choice(words) for _ in range(rng.randrange(6))]
                return out

            def join(records):
                if not records:
                    return b""
                return eol.join(records) + (eol if rng.randrange(4) else b"")

            inputs = [join(records) for records in (lines, mutate(), mutate())]
            # Check all styles on the same input: diff3 and zdiff3 must not
            # accidentally inherit merge's refinement and simplification.
            favor = rng.choice(["none"] * 4 + ["ours", "theirs", "union"])
            marker = rng.choice([0, 1, 7, 11])
            for style in ("merge", "diff3", "zdiff3"):
                add(*inputs, style, favor, marker)

        # Levels 0..3 have deliberately different outcomes on these inputs.
        for level in range(4):
            same = b"new\n"
            conflict = b"<<<<<<< ours\nnew\n=======\nnew\n>>>>>>> theirs\n"
            add(b"old\n", same, same, "merge", level=level,
                result=(conflict, 1) if level == 0 else (same, 0))
            ours = b"A\n{\n\n}\n;\nZ\n"
            theirs = b"X\n{\n\n}\n;\nY\n"
            whole = b"<<<<<<< ours\n" + ours + b"=======\n" + theirs + b">>>>>>> theirs\n"
            split = (b"<<<<<<< ours\nA\n=======\nX\n>>>>>>> theirs\n{\n\n}\n;\n"
                     b"<<<<<<< ours\nZ\n=======\nY\n>>>>>>> theirs\n")
            # One original replacement hunk; level 2 splits at matching
            # punctuation, while level 3 coalesces that gap again.
            add(b"old\n", ours, theirs, "merge", level=level,
                result=(split, 2) if level == 2 else (whole, 1))

        manifest = tmp / "cases.el"
        manifest.write_text("(" + "\n".join(
            "(" + " ".join(json.dumps(value) for value in case) + ")" for case in cases) + ")")
        start = time.monotonic()
        p = subprocess.run([os.environ.get("NGIT_NELISP", "nelisp"), "--load",
                            str(ROOT / "test/merge-driver.el"), "--", str(manifest)],
                           env=ENV, capture_output=True)
        failures = 0
        if p.returncode:
            print("merge driver failed:", p.returncode, p.stdout[-2000:], p.stderr[-2000:])
            return 1
        for i, (want, conflicts) in enumerate(expected):
            directory = tmp / str(i)
            actual = (directory / "actual").read_bytes()
            actual_count = int((directory / "count").read_text())
            if (actual, actual_count) != (want, conflicts):
                failures += 1
                if failures > 10:
                    continue
                print("MISMATCH case", i, cases[i][1:])
                for name in ("base", "ours", "theirs"):
                    print(name, repr((directory / name).read_bytes())[:500])
                print("git", conflicts, repr(want)[:800])
                print("ngit", actual_count, repr(actual)[:800])
        print(f"merge-fuzz: {len(cases) - failures}/{len(cases)} cases identical "
              f"({count} three-way inputs, driver {time.monotonic() - start:.1f}s)")
        return int(failures > 0)


if __name__ == "__main__":
    raise SystemExit(main())
