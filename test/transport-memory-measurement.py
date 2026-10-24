#!/usr/bin/env python3
"""Same-source real clone PSS/time comparison against immutable baseline.

Both samples include this lane's supervisor overhead; baseline is observed,
not budget-enforced, and retains its original index/checkout policy.  This is
a reproducible cache-heavy 128 MiB object fixture, not a general speed claim.
Evidence is reused only if all source, harness and runtime hashes match.
"""
import hashlib
import json
import os
from pathlib import Path
import shutil
import statistics
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "target/memory-lane/measurement"
OUT.mkdir(parents=True, exist_ok=True)
BASE = os.environ.get("NGIT_MEMORY_BASE_REV", "e38c3c0")
RUNTIME = Path(shutil.which(os.environ.get("NGIT_NELISP", "nelisp"))).resolve()


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def run(args, **kwargs):
    return subprocess.run([str(arg) for arg in args], check=True, timeout=150, **kwargs)


sources = [ROOT / "bin/ngit", ROOT / "tools/transport-memory.py", Path(__file__), *sorted((ROOT / "lisp").glob("*.el"))]
manifest = {str(path.relative_to(ROOT)): sha(path) for path in sources}
manifest["runtime_sha256"] = sha(RUNTIME)
manifest["runtime_path"] = str(RUNTIME)
manifest["baseline_commit"] = subprocess.check_output(["git", "rev-parse", BASE], cwd=ROOT, text=True).strip()
manifest["config"] = {"budget_mib": 512, "files": 64, "blob_bytes": 2097152, "git_pack_window": 0,
                      "old_workers": 8, "new_workers": 1, "old_cache_bytes": 1073741824,
                      "new_cache_bytes": 67108864, "note": "fixture object count selects sequential index and checkout in both versions"}
saved = OUT / "evidence.json"
if saved.exists():
    evidence = json.loads(saved.read_text())
    previous = dict(evidence["manifest"])
    previous.pop("fixture_pack_sha256", None)
    if previous == manifest:
        print("transport-memory measurement: PASS (reused matching source/runtime evidence)")
        print(json.dumps(evidence["results"], sort_keys=True))
        sys.exit(0)

baseline = OUT / "baseline"
shutil.rmtree(baseline, ignore_errors=True)
baseline.mkdir()
archive = subprocess.Popen(["git", "archive", BASE], cwd=ROOT, stdout=subprocess.PIPE)
run(["tar", "-x", "-C", baseline], stdin=archive.stdout)
archive.stdout.close()
assert archive.wait(timeout=5) == 0
for tree in (baseline, ROOT):
    run(["sh", tree / "tools/build-elc.sh"], stdout=subprocess.DEVNULL)
source = OUT / "source"
shutil.rmtree(source, ignore_errors=True)
source.mkdir()
env = dict(os.environ, LC_ALL="C", GIT_CONFIG_NOSYSTEM="1", GIT_AUTHOR_NAME="Memory Test",
           GIT_AUTHOR_EMAIL="memory@example.com", GIT_COMMITTER_NAME="Memory Test",
           GIT_COMMITTER_EMAIL="memory@example.com", GIT_AUTHOR_DATE="1700600000 +0000",
           GIT_COMMITTER_DATE="1700600000 +0000", NGIT_MEMORY_BUDGET_MIB="512")
run(["git", "init", "-q", "--template=", "-b", "main", source], env=env)
for index in range(64):
    block = hashlib.sha256(str(index).encode()).digest() * 2048
    (source / f"blob-{index:03}.dat").write_bytes(block * 32)
run(["git", "-C", source, "add", "."], env=env)
run(["git", "-C", source, "commit", "-qm", "memory fixture"], env=env)
run(["git", "-C", source, "repack", "-adf", "--window=0"], env=env)
pack = next((source / ".git/objects/pack").glob("*.pack"))
manifest["fixture_pack_sha256"] = sha(pack)
results = {}
for name, tree in (("baseline", baseline), ("changed", ROOT)):
    target = OUT / (name + "-clone")
    shutil.rmtree(target, ignore_errors=True)
    report = OUT / (name + ".json")
    with (OUT / (name + ".log")).open("w") as log:
        # Both commands are wrapped once with identical measurement overhead.
        # The changed launcher would otherwise add a second supervisor; run
        # its exact encoded runtime command directly for this comparison.
        if name == "baseline":
            command = [str(tree / "bin/ngit"), "clone", "-q", "--template=", "file://" + str(source), str(target)]
        else:
            command = [str(RUNTIME), "--load", str(tree / "lisp/ngit-main.el"), "--", "--@", "@clone", "@-q", "@--template=", "@file://" + str(source), "@" + str(target)]
        flags = ["--observe"] if name == "baseline" else []
        run([sys.executable, ROOT / "tools/transport-memory.py", *flags, "--report", report, "--", *command], env=env, stdout=log, stderr=log)
    state = subprocess.check_output(["git", "-C", str(target), "ls-files", "--stage"], env=env)
    refs = subprocess.check_output(["git", "-C", str(target), "for-each-ref", "--format=%(refname) %(objectname) %(symref)"], env=env)
    assert all(sha(target / f"blob-{i:03}.dat") == sha(source / f"blob-{i:03}.dat") for i in range(64))
    run(["git", "-C", target, "fsck", "--full", "--no-progress"], env=env, stdout=subprocess.DEVNULL)
    results[name] = json.loads(report.read_text())
    results[name]["state_sha256"] = hashlib.sha256(refs + state).hexdigest()
    assert results[name]["samples"] > 2
assert results["baseline"]["state_sha256"] == results["changed"]["state_sha256"]
ordinary_env = dict(env)
ordinary_env.pop("NGIT_MEMORY_BUDGET_MIB", None)
results["ordinary_startup"] = {}
for name, tree in (("baseline", baseline), ("changed", ROOT)):
    timings = []
    for repeat in range(6):
        start = time.monotonic()
        run([tree / "bin/ngit", "-C", source, "rev-parse", "HEAD"], env=ordinary_env, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        if repeat:
            timings.append(time.monotonic() - start)
    results["ordinary_startup"][name] = {"median_seconds": statistics.median(timings), "samples_seconds": timings}
saved.write_text(json.dumps({"manifest": manifest, "results": results}, indent=2) + "\n")
print("transport-memory measurement: PASS (same source, refs/index/worktree, full fsck)")
print(json.dumps(results, sort_keys=True))
