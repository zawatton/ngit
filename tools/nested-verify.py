#!/usr/bin/env python3
"""Verify nested-command controls, focused regressions and integrated read paths."""
import argparse
import ast
import base64
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
import os
from pathlib import Path
import re
import resource
import signal
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
TASK = Path(os.environ.get("NGIT_NESTED_TASK_DIR", ROOT / "target/nested-20261024")).resolve()
OUT = TASK / "verification"
BASE = "b59825a"
RECORD = {"commands": [], "checks": [], "overall_deadline_seconds": 540}
DEADLINE = time.monotonic() + 540


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def remaining_timeout(timeout):
    remaining = DEADLINE - time.monotonic()
    require(remaining > 0, "Overall verification deadline expired")
    return min(timeout, remaining)


def execute(argv, cwd=ROOT, env=None, label="command", timeout=600):
    """Capture complete streams and terminate the whole child group on timeout."""
    OUT.mkdir(parents=True, exist_ok=True)
    budget = remaining_timeout(timeout)
    process = subprocess.Popen(list(map(str, argv)), cwd=cwd, env=env,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE, start_new_session=True)
    timed_out = False
    try:
        stdout, stderr = process.communicate(timeout=min(budget, max(0, DEADLINE - time.monotonic())))
    except subprocess.TimeoutExpired:
        timed_out = True
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        stdout, stderr = process.communicate(timeout=5)
    name = f"{label}-{process.pid}"
    (OUT / (name + ".stdout")).write_bytes(stdout)
    (OUT / (name + ".stderr")).write_bytes(stderr)
    RECORD["commands"].append({"argv": list(map(str, argv)), "cwd": str(cwd),
                               "exit": process.returncode, "timeout": timed_out, "streams": name,
                               "timeout_budget": budget})
    require(not timed_out, f"Timed out: {argv}")
    return process.returncode, stdout, stderr


def git(*args, cwd=ROOT, env=None):
    result = execute(["git", *args], cwd, env, "git", 60)
    require(result[0] == 0, f"Fixture/provenance Git failed: {args}: {result[2]!r}")
    return result[1]


def environment():
    env = dict(os.environ)
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR"):
        env.pop(key, None)
    env.update(LC_ALL="C", TZ="UTC", GIT_OPTIONAL_LOCKS="0", GIT_CONFIG_NOSYSTEM="1",
               GIT_CONFIG_GLOBAL=os.devnull, GIT_AUTHOR_NAME="Nested Verify",
               GIT_AUTHOR_EMAIL="verify@example.test", GIT_COMMITTER_NAME="Nested Verify",
               GIT_COMMITTER_EMAIL="verify@example.test", GIT_AUTHOR_DATE="1700000000 +0000",
               GIT_COMMITTER_DATE="1700000000 +0000")
    for key in ("NGIT_NESTED_COMMAND", "NGIT_NESTED_REPORT", "NGIT_TEST_MATCH", "NGIT_NO_ELC"):
        env.pop(key, None)
    env.setdefault("NGIT_NELISP_ROOT", str(notes_root() / "dev/nelisp"))
    return env


def notes_root():
    if os.environ.get("NOTES_ROOT"):
        notes = Path(os.environ["NOTES_ROOT"]).resolve()
    else:
        common = Path(git("rev-parse", "--git-common-dir").decode().strip())
        common = (ROOT / common).resolve()
        notes = next((p for p in common.parents if (p / "bin/heavy-run.sh").is_file()), None)
    require(notes is not None and (notes / "bin/heavy-run.sh").is_file(), "Notes heavy gate unavailable")
    return notes


def source_proof(root):
    paths = [root / "bin/ngit", *sorted((root / "lisp").glob("*.el"))]
    require(len(paths) > 1 and all(p.is_file() for p in paths), "Missing executable sources")
    return {str(p.relative_to(root)): sha(p) for p in paths}


def fingerprint():
    paths = [*sorted((ROOT / "test").glob("*")), *sorted((ROOT / "tools").glob("*.py")),
             ROOT / "tools/build-elc.sh", ROOT / "tools/compile.el"]
    return {**source_proof(ROOT), **{str(p.relative_to(ROOT)): sha(p) for p in paths if p.is_file()}}


def verify_hashes(root, expected):
    require(len(expected) == 51, "Expected exactly 51 original test sources")
    for name, digest in expected.items():
        path = root / name
        require(path.is_file() and sha(path) == digest, f"Original test source drift: {name}")


def provenance():
    commit = git("rev-parse", BASE).decode().strip()
    git("merge-base", "--is-ancestor", commit, "HEAD")
    expected = json.loads((TASK / "existing-test-hashes.json").read_text())
    names = git("ls-tree", "-r", "--name-only", commit, "test").decode().splitlines()
    require(set(names) == set(expected) and len(names) == 51, "Original test manifest membership drift")
    for name, digest in expected.items():
        require(hashlib.sha256(git("show", f"{commit}:{name}")).hexdigest() == digest,
                f"Original hash manifest drift: {name}")
    verify_hashes(ROOT, expected)
    RECORD["baseline_commit"] = commit
    return commit


def driver_tuples(report, stdout, streams):
    """Read complete command tuples from the three supported driver formats."""
    for case in report.get("cases", []):
        if "git" in case and "ngit" in case:
            def unpack(value):
                return (value["exit"], base64.b64decode(value["stdout"], validate=True),
                        base64.b64decode(value["stderr"], validate=True))
            yield unpack(case["git"]), unpack(case["ngit"])
        elif "name" in case and "git_exit" in case and "ngit_exit" in case:
            paths = [streams / (case["name"] + f".{side}.{stream}")
                     for side in ("git", "ngit") for stream in ("stdout", "stderr")]
            require(all(p.is_file() for p in paths), "Missing recorded command streams")
            yield ((case["git_exit"], paths[0].read_bytes(), paths[1].read_bytes()),
                   (case["ngit_exit"], paths[2].read_bytes(), paths[3].read_bytes()))
    for line in stdout.decode("utf-8", errors="replace").splitlines():
        match = re.search(r"git= (.*) ngit= (.*)$", line)
        if match:
            try:
                left, right = (ast.literal_eval(value) for value in match.groups())
                if len(left) == len(right) == 3:
                    yield left, right
            except (ValueError, SyntaxError, TypeError):
                pass


def reject_runtime_failure(actual):
    code, _, stderr = actual
    runtime = re.search(rb"(?im)^nelisp: (?:uncaught|fatal|internal) error\b|"
                        rb"^backtrace \(innermost first\)|error while loading shared libraries|"
                        rb"\bexec: .*\b(?:nelisp|ngit)\b.*(?:not found|Permission denied)", stderr)
    require(not (code < 0 or code in (126, 127, 134, 139) or runtime),
            "Command runtime/startup failure is not a semantic negative control")


def validate_driver(exit_code, report_path, stdout, streams, old):
    require(report_path.is_file(), "Missing driver report")
    report = json.loads(report_path.read_text())
    count, failures, errors = (report.get(k) for k in ("comparisons", "failures", "unexpected_errors"))
    require(all(type(v) is int and v >= 0 for v in (count, failures, errors)), "Invalid driver counters")
    require(count > 0 and errors == 0 and failures <= count, "Empty/error driver run")
    tuples = list(driver_tuples(report, stdout, streams))
    for _, actual in tuples:
        reject_runtime_failure(actual)
    if old and failures:
        require(exit_code == 1 and any(left != right for left, right in tuples),
                "Not a genuine old-red tuple mismatch")
    else:
        require(exit_code == 0 and failures == 0, "Current driver or passing control failed")
    return {"comparisons": count, "failures": failures, "unexpected_errors": errors}


def controls(env, commit):
    baseline = TASK / "baseline"
    require(git("rev-parse", "HEAD", cwd=baseline).decode().strip() == commit, "Baseline HEAD mismatch")
    old_sources = source_proof(baseline)
    for name, digest in old_sources.items():
        require(hashlib.sha256(git("show", f"{commit}:{name}")).hexdigest() == digest,
                f"Baseline source drift: {name}")
    RECORD["executables"] = {"baseline": old_sources, "current": source_proof(ROOT)}
    for lane, modes in (("history", ("boundaries",)), ("search", ("grep", "blame")), ("tree", ("selection",))):
        red = False
        for old in (True, False):
            for mode in modes:
                label = f"{lane}-{mode}-{'old' if old else 'new'}"
                area = Path(tempfile.mkdtemp(prefix=label + "-", dir=OUT))
                report = area / "report.json"
                child_env = dict(env, NGIT_NESTED_COMMAND=str((baseline if old else ROOT) / "bin/ngit"),
                                 NGIT_NESTED_REPORT=str(report), NGIT_FIXTURE_DIR=str(area / "fixtures"),
                                 NGIT_NESTED_LOG_DIR=str(area / "streams"), NGIT_NO_ELC="1")
                result = execute([sys.executable, ROOT / f"test/nested-{lane}.py", mode],
                                 env=child_env, label=label, timeout=1200)
                counts = validate_driver(result[0], report, result[1], area / "streams" / mode, old)
                RECORD["checks"].append({"lane": lane, "mode": mode, "old": old, **counts})
                red |= old and counts["failures"] > 0
        require(red, f"No genuine baseline-red in lane {lane}")
    require(source_proof(baseline) == old_sources, "Baseline sources changed during controls")


def compare_tuple(expected, actual, meaningful=True):
    require(expected[0] == 0 and actual[0] == 0, "Integrated command failed")
    require(not meaningful or bool(expected[1]), "Empty expected selection")
    require(expected == actual, "Exact exit/stdout/stderr tuple mismatch")


def all_objects(fixtures, env, gate):
    counts = {}
    for name in ("loose", "packed"):
        repo = fixtures / name
        objects = git("cat-file", "--batch-all-objects", "--batch-check=%(objectname)", cwd=repo, env=env)
        names = objects.splitlines()
        require(names and all(re.fullmatch(rb"[0-9a-f]{40}", n) for n in names), "Empty/invalid native object list")
        listing = fixtures / (name + "-objects.txt")
        listing.write_bytes(objects)
        expected = bytearray()
        for oid in names:
            for option in ("-t", "-p"):
                result = execute(["git", "-C", repo, "cat-file", option, oid.decode()], env=env,
                                 label=f"objects-{name}", timeout=60)
                require(result[0] == 0 and result[2] == b"", "Native object read failed")
                expected.extend(result[1])
        actual = execute([gate, ROOT / "bin/ngit", "-C", repo, "test-cat-all"],
                         env=dict(env, NGIT_BATCH_LIST=str(listing)), label=f"batch-{name}", timeout=600)
        compare_tuple((0, bytes(expected), b""), actual)
        counts[name] = len(names)
    return {"comparisons": 2, "objects": counts}


def regression(env):
    gate = notes_root() / "bin/heavy-run.sh"
    suites = ("log-path", "show", "grep", "blame", "ls-tree", "cat", "all-objects", "log-diff")

    def child(suite):
        fixtures = Path(tempfile.mkdtemp(prefix="regression-" + suite + "-", dir=OUT))
        child_env = dict(env, NGIT_FIXTURE_DIR=str(fixtures))
        result = execute([gate, "sh", ROOT / "test/compare.sh", suite], env=child_env,
                         label="regression-" + suite, timeout=1800)
        matches = re.findall(rb"^PASS " + suite.encode() + rb": ([0-9]+) comparisons$", result[1], re.M)
        require(result[0] == 0 and len(matches) == 1 and
                not re.search(rb"^(MISMATCH|FAIL)\b", result[1], re.M), f"Regression failed: {suite}")
        count = int(matches[0])
        record = {"suite": suite, "exit": result[0], "legacy_count": count}
        if suite == "all-objects":
            record.update(all_objects(fixtures, child_env, gate))
        else:
            require(count > 0, f"Zero checked comparisons in {suite}")
            record["comparisons"] = count
        return record

    with ThreadPoolExecutor(max_workers=3) as pool:
        for record in pool.map(child, suites):
            RECORD["checks"].append(record)


def state(repo, env):
    files = {}
    for path in sorted(repo.rglob("*")):
        if path.is_symlink():
            files[str(path.relative_to(repo))] = [path.lstat().st_mode, os.readlink(path)]
        elif path.is_file():
            files[str(path.relative_to(repo))] = [path.stat().st_mode, sha(path)]
    fsck = execute(["git", "fsck", "--full", "--no-dangling"], cwd=repo, env=env, label="fsck", timeout=60)
    require(fsck[0] == 0, "Full fsck failed")
    return {"files": files, "stages": git("ls-files", "--stage", "-z", cwd=repo, env=env).hex(),
            "refs": git("show-ref", cwd=repo, env=env).hex(),
            "fsck": [fsck[0], fsck[1].hex(), fsck[2].hex()]}


def e2e(env):
    result = execute(["sh", ROOT / "tools/build-elc.sh"], env=env, label="build", timeout=600)
    require(result[0] == 0 and (ROOT / "lisp/.elc-stamp").is_file(), "Compiled mode unavailable")
    stamp = (ROOT / "lisp/.elc-stamp").stat().st_mtime_ns
    require(all(p.stat().st_mtime_ns <= stamp for p in (ROOT / "lisp").glob("*.el")), "Stale compiled mode")
    excluded = {"ngit-main.el", "ngit-profile.el", "ngit-native.el", "ngit-transport-kernel-source.el"}
    compiled = [p.with_suffix(".elc") for p in (ROOT / "lisp").glob("*.el") if p.name not in excluded]
    require(all(p.is_file() for p in compiled), "Missing compiled module")
    RECORD["compiled_artifacts"] = {str(p.relative_to(ROOT)): sha(p) for p in compiled}
    for mode in ("compiled", "source"):
        repo = Path(tempfile.mkdtemp(prefix="e2e-" + mode + "-", dir=OUT))
        child_env = dict(env)
        if mode == "source":
            child_env["NGIT_NO_ELC"] = "1"
        git("init", "-q", "-b", "main", cwd=repo, env=child_env)
        for name in ("sub/deep/a.txt", "sibling.txt", "sub/deep/sp ace.txt", "sub/deep/日本語.txt"):
            path = repo / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("needle one\nsecond line\n", encoding="utf-8")
        git("add", ".", cwd=repo, env=child_env)
        git("commit", "-qm", "base", cwd=repo, env=child_env)
        (repo / "sub/deep/a.txt").write_text("needle updated\nsecond line\n")
        git("add", ".", cwd=repo, env=child_env)
        git("commit", "-qm", "next", cwd=repo, env=child_env)
        before = state(repo, child_env)
        prefix = ["-C", repo, "-C", "sub", "-C", "deep"]

        def compare(args):
            expected = execute(["git", *prefix, *args], env=child_env, label=f"e2e-{mode}-git", timeout=60)
            actual = execute([ROOT / "bin/ngit", *prefix, *args], env=child_env, label=f"e2e-{mode}-ngit", timeout=60)
            compare_tuple(expected, actual)
            return expected[1]

        history = compare(["log", "--format=%H", "--", "a.txt"]).splitlines()
        require(len(history) == 2 and all(re.fullmatch(rb"[0-9a-f]{40}", h) for h in history), "Wrong history selection")
        rev = history[0].decode()
        shown = compare(["show", "--format=", "--name-only", rev, "--", "a.txt"])
        require(shown == b"sub/deep/a.txt\n", "Wrong show selection")
        tree = compare(["ls-tree", "-r", "--name-only", rev, "--", "."])
        require(b"a.txt\n" in tree and b"sp ace.txt\n" in tree, "Wrong tree selection")
        found = compare(["grep", "-n", "needle", rev, "--", "a.txt"])
        require(b":a.txt:1:needle updated\n" in found, "Wrong grep selection")
        blamed = compare(["blame", "--porcelain", rev, "--", "a.txt"])
        require(b"filename sub/deep/a.txt\n" in blamed and b"\tneedle updated\n" in blamed, "Wrong blame selection")
        blob = compare(["show", rev + ":sub/deep/a.txt"])
        require(blob == b"needle updated\nsecond line\n", "Wrong blob selection")
        require(state(repo, child_env) == before, "Index/stages/refs/reflogs/worktree/fsck changed")
        RECORD["checks"].append({"mode": mode, "comparisons": 6, "state_unchanged": True})


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("controls", "regression", "e2e"))
    args = parser.parse_args()
    cap = 8 * 1024 ** 3
    soft, hard = resource.getrlimit(resource.RLIMIT_AS)
    resource.setrlimit(resource.RLIMIT_AS, (min(soft, cap) if soft >= 0 else cap, hard))
    OUT.mkdir(parents=True, exist_ok=True)
    RECORD["mode"] = args.mode
    status = 1
    try:
        env = environment()
        commit = provenance()
        before = fingerprint()
        RECORD["source_before"] = before
        if args.mode == "controls":
            controls(env, commit)
        elif args.mode == "regression":
            regression(env)
        else:
            e2e(env)
        after = fingerprint()
        RECORD["source_after"] = after
        require(before == after, "Source mutation during verification")
        verify_hashes(ROOT, json.loads((TASK / "existing-test-hashes.json").read_text()))
        status = 0
    except Exception as error:
        RECORD["error"] = f"{type(error).__name__}: {error}"
    try:
        RECORD["source_final"] = fingerprint()
        if "source_before" in RECORD:
            require(RECORD["source_before"] == RECORD["source_final"], "Final source fingerprint changed")
        remaining_timeout(1)
    except Exception as error:
        RECORD["final_source_error"] = str(error)
        status = 1
    RECORD["exit"] = status
    (OUT / (args.mode + ".json")).write_text(json.dumps(RECORD, indent=2) + "\n")
    print(f"{args.mode}: {'PASS' if status == 0 else 'FAIL'}; {len(RECORD['checks'])} checks; "
          f"record={OUT / (args.mode + '.json')}")
    return status


if __name__ == "__main__":
    sys.exit(main())
