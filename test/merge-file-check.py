#!/usr/bin/env python3
"""Compare merge-file's streams, status and all three input files with Git."""

import itertools
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
NGIT = str(ROOT / "bin/ngit")
ENV = dict(os.environ, GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL=os.devnull,
           LC_ALL="C", GIT_CONFIG_COUNT="0")


def run_checks():
    checked = 0
    failures = 0
    with tempfile.TemporaryDirectory(prefix="ngit-merge-cli-") as tmp:
        work = Path(tmp)

        def check(name, base=b"base\n", ours=b"ours\n", theirs=b"theirs\n",
                  options=(), paths=("ours", "base", "theirs"), config=None,
                  symlink=False, stdout_target=None):
            nonlocal checked, failures
            results = []
            for command in (["git", "merge-file"], [NGIT, "merge-file"]):
                for filename, data in zip(("base", "ours", "theirs"),
                                          (base, ours, theirs)):
                    (work / filename).write_bytes(data)
                files = ("ours", "base", "theirs")
                if symlink:
                    (work / "actual/sub").mkdir(parents=True, exist_ok=True)
                    (work / "actual/ours").write_bytes(base)
                    if not (work / "sym").exists():
                        (work / "sym").symlink_to("actual/sub")
                    files += ("actual/ours",)
                if config:
                    subprocess.run(["git", "init", "-q", str(work)],
                                   env=ENV, check=True)
                    subprocess.run(["git", "-C", str(work), "config",
                                    "merge.conflictStyle", config], env=ENV, check=True)
                if stdout_target:
                    with open(stdout_target, "wb") as stream:
                        p = subprocess.run(command + list(options) + list(paths),
                                           cwd=work, env=ENV, stdout=stream,
                                           stderr=subprocess.PIPE)
                else:
                    p = subprocess.run(command + list(options) + list(paths),
                                       cwd=work, env=ENV, capture_output=True)
                results.append((p.returncode, p.stdout, p.stderr,
                                tuple((work / f).read_bytes()
                                      for f in files)))
            checked += 1
            if results[0] != results[1]:
                failures += 1
                print("MISMATCH:", name, "options=", options, "paths=", paths)
                for field, expected, actual in zip(
                        ("status", "stdout", "stderr", "files"), *results):
                    if expected != actual:
                        print(f"  {field}: git={expected!r} ngit={actual!r}"[:1600])

        samples = [
            ("unchanged", b"a\nb\nc\n", b"a\nb\nc\n", b"a\nb\nc\n"),
            ("ours only", b"a\nb\n", b"A\nb\n", b"a\nb\n"),
            ("theirs only", b"a\nb\n", b"a\nb\n", b"a\nB\n"),
            ("disjoint", b"a\nb\nc\nd\n", b"A\nb\nc\nd\n", b"a\nb\nc\nD\n"),
            ("one conflict", b"a\nb\nc\n", b"a\nB1\nc\n", b"a\nB2\nc\n"),
            ("adjacent", b"a\nb\nc\n", b"A\nb\nc\n", b"a\nB\nc\n"),
            ("overlap", b"a\nb\nc\nd\n", b"a\nX\nd\n", b"a\nb\nY\n"),
            ("same change", b"a\nb\n", b"a\nB\n", b"a\nB\n"),
            ("delete edit", b"a\nb\nc\n", b"a\nc\n", b"a\nB\nc\n"),
            ("delete both", b"a\nb\nc\n", b"a\nc\n", b"a\nc\n"),
            ("insert both", b"a\nc\n", b"a\nB1\nc\n", b"a\nB2\nc\n"),
            ("empty base", b"", b"one\n", b"two\n"),
            ("empty ours", b"base\n", b"", b"two\n"),
            ("empty theirs", b"base\n", b"one\n", b""),
            ("all empty", b"", b"", b""),
            ("two conflicts", b"a\n1\n2\n3\n4\nz\n", b"A\n1\n2\n3\n4\nZ\n",
             b"X\n1\n2\n3\n4\nY\n"),
            ("punctuation gap", b"a\n{\n\n}\n;\nz\n", b"A\n{\n\n}\n;\nZ\n",
             b"X\n{\n\n}\n;\nY\n"),
            ("refine edges", b"base\n", b"prefix\nours\nsuffix\n",
             b"prefix\ntheirs\nsuffix\n"),
            ("refine middle", b"base\n", b"ours\na\nb\nc\nd\nours2\n",
             b"theirs\na\nb\nc\nd\ntheirs2\n"),
            ("nonascii", b"\xff\n", b"\xc3\xa9\n", b"\xfe\n"),
        ]
        for name, base, ours, theirs in samples:
            check(name, base, ours, theirs)
        # Every possible combination of missing final newlines.
        for endings in itertools.product((b"", b"\n"), repeat=3):
            values = [text + ending for text, ending in
                      zip((b"base", b"ours", b"theirs"), endings)]
            check("endings " + repr(endings), *values, options=("-p",))
        for style in ((), ("--diff3",), ("--zdiff3",)):
            for favor in ((), ("--ours",), ("--theirs",), ("--union",)):
                check("style favor", b"base\n", b"prefix\nours\nsuffix\n",
                      b"prefix\ntheirs\nsuffix\n", options=style + favor + ("-p",))
            for base, ours, theirs in (
                    (b"a\r\nb\r\n", b"a\r\nours\r\n", b"a\r\ntheirs\r\n"),
                    (b"base\r\n", b"ours", b"theirs\r\n"),
                    (b"base\n", b"ours\r\n", b"theirs\r\n"),
                    (b"", b"ours\r\n", b"theirs\r\n")):
                check("CRLF", base, ours, theirs, options=style)
        for options in (
                ("--stdout",), ("-pq",), ("-q",),
                ("--marker-size=2",), ("--marker-size", "12"),
                ("--marker-size=0",), ("--marker-size=-2",),
                ("--marker-size=2147483648",),
                ("-L", "current label", "-L", "ancestor", "-L", "other", "--diff3"),
                ("-L", "", "-L", "", "-L", "", "--zdiff3"),
                ("-Lours", "-Lbase", "--diff3"),
                ("--diff3", "--zdiff3", "--ours", "--theirs"),
                ("--stdout", "--no-stdout", "--ours", "--no-ours")):
            check("options", options=options)
        for paths, options in (
                ((), ()), (("ours", "base"), ()),
                (("ours", "base", "theirs", "extra"), ()),
                (("ours", "base", "theirs"), ("--unknown",)),
                (("ours", "base", "theirs"), ("--d",)),
                (("ours", "base", "theirs"), ("--o",)),
                (("ours", "base", "theirs"), ("-x",)),
                (("ours", "base", "theirs"), ("--marker-size=bad",)),
                ((), ("--marker-size",)), ((), ("-L",)),
                (("ours", "base", "theirs"), ("--diff3=bad",)),
                (("ours", "base", "theirs"), ("-L", "a", "-L", "b", "-L", "c", "-L", "d")),
                (("missing", "base", "theirs"), ()),
                (("ours", "missing", "theirs"), ("-q",)),
                (("ours", "base", "missing"), ()),
                (("ours", "base", "ours/child"), ()),
                (("", "base", "theirs"), ()),
                (("ours", "base", "theirs"), ("-h",))):
            check("bad arguments", options=options, paths=paths)
        for index in range(3):
            values = [b"base\n", b"ours\n", b"theirs\n"]
            values[index] += b"\x00binary\n"
            check("binary " + str(index), *values)
            check("quiet binary " + str(index), *values, options=("-q",))
        # Git detects NUL only in the first 8000 bytes.
        check("late NUL", b"a" * 8000 + b"\0", b"a" * 8000 + b"\0", b"changed\n")
        check("UTF-8 labels with raw bytes", b"base\n", b"\xff\n", b"\xfe\n",
              options=("-p", "--diff3", "-L", "現在", "-L", "基点", "-L", "相手"))
        for options in ((), ("-p",)):
            check("symlink dotdot", ours=b"wrong\n", symlink=True, options=options,
                  paths=("sym/../ours", "base", "theirs"))
        full_device = str(Path(os.devnull).with_name("full"))
        if Path(full_device).exists():
            for options in (("-p",), ("-pq",)):
                check("stdout failure", options=options, stdout_target=full_device)
        # Exercise the conflict-count exit cap without expensive diff work.
        base = b"".join(b"base\n1\n2\n3\n4\n" for _ in range(130))
        check("status cap", base, base.replace(b"base", b"ours"),
              base.replace(b"base", b"theirs"), options=("--diff3", "-p"))
        # Config is read only when merge-file is run inside a repository.
        check("config diff3", config="diff3")
        check("config override", config="diff3", options=("--zdiff3",))
    print(f"merge-file: {checked - failures}/{checked} CLI cases identical")
    return failures == 0


if __name__ == "__main__":
    raise SystemExit(0 if run_checks() else 1)
