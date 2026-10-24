#!/usr/bin/env python3
"""Run existing progress meters and retain bounded integration provenance."""
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[1]
TASK = ROOT / "target" / "parallel-20261024"
METER = ROOT.parents[1] / "bin" / "progress-meter.sh"
BASE = "e38c3c0"
LEDGERS = (
    "git-read-core-progress", "git-status-diff-progress",
    "git-add-commit-progress", "git-log-branch-progress",
    "git-switch-reset-tag-show-progress", "git-rm-mv-restore-stash-progress",
    "git-completion-progress", "git-takeover-audit-progress",
    "git-gc-commit-graph-progress",
)


def git(*args):
    return subprocess.check_output(["git", "-C", str(ROOT), *args])


def ids(name):
    text = (ROOT / "tools" / "ai" / f"{name}.org").read_text()
    result = re.findall(r"^\*\* (S\d+\.\d+) ", text, re.M)
    if not result or len(result) != len(set(result)):
        raise RuntimeError(f"Invalid or empty criterion set: {name}")
    return result


def checks(report):
    return [item for stage in report["stages"] for item in stage["criteria"]]


def require_pass(items, expected):
    selected = {item["id"]: item for item in items}
    if set(selected) != set(expected) or len(items) != len(expected):
        raise RuntimeError("Missing, duplicated or unexpected validation criteria")
    bad = [(key, item["status"], item.get("detail"))
           for key, item in selected.items()
           if item["status"] != "PASS" or item.get("stale", True)]
    if bad:
        raise RuntimeError(str(bad))


def fingerprint():
    names = git("ls-files", "-c", "-o", "--exclude-standard", "-z").decode().split("\0")
    digest = hashlib.sha256()
    for name in sorted(set(filter(None, names))):
        path = ROOT / name
        if path.is_file():
            digest.update(name.encode() + b"\0" + path.read_bytes() + b"\0")
    return digest.hexdigest()


def environment():
    env = os.environ.copy()
    env.update(LC_ALL="C", GIT_EDITOR="true",
               NGIT_REAL_REPO=str(ROOT.parents[1]),
               NGIT_NELISP_ROOT=str(ROOT.parent / "nelisp"),
               NGIT_FIXTURE_DIR=str(TASK / "regression-fixtures"))
    return env


def meter(name, env, only=None):
    args = ["bash", str(METER), "--ledger", f"tools/ai/{name}.org",
            "--checkpoint", "--json"]
    if only:
        args += ["--only", only]
    label = f"{name}-{only or 'all'}"
    with (TASK / f"{label}.log").open("w") as log:
        subprocess.run(args, cwd=ROOT, env=env, stdout=log,
                       stderr=subprocess.STDOUT, check=True)
    out = Path(env.get("PROGRESS_OUT_DIR", ROOT / "target" / "progress"))
    return json.loads((out / f"{name}.json").read_text())


def run_baseline():
    name = "git-completion-progress"
    expected = set(ids(name)) - {"S8.4", "S9.6"}
    if len(expected) != 24:
        raise RuntimeError("Baseline criterion set changed")
    env = environment()
    env["NGIT_FIXTURE_DIR"] = str(TASK / "baseline-fixtures")
    env["PROGRESS_OUT_DIR"] = str(TASK / "baseline-meter")
    record = {"base": git("rev-parse", BASE).decode().strip(),
              "tree": git("rev-parse", f"{BASE}^{{tree}}").decode().strip(),
              "criteria": [], "started": time.time()}
    for criterion in ids(name):
        if criterion not in expected:
            continue
        subprocess.run(["git", "diff", "--quiet", BASE, "--", "lisp", "test", "bin"],
                       cwd=ROOT, check=True)
        report = meter(name, env, criterion)
        item = next(item for item in checks(report) if item["id"] == criterion)
        record["criteria"].append(item)
        (TASK / "baseline-results.json").write_text(json.dumps(record, indent=2) + "\n")
        print(f"baseline {criterion}: {item['status']}", flush=True)
    check_baseline()


def check_baseline():
    record = json.loads((TASK / "baseline-results.json").read_text())
    if record["base"] != git("rev-parse", BASE).decode().strip():
        raise RuntimeError("Baseline revision mismatch")
    if record["tree"] != git("rev-parse", f"{BASE}^{{tree}}").decode().strip():
        raise RuntimeError("Baseline tree mismatch")
    expected = set(ids("git-completion-progress")) - {"S8.4", "S9.6"}
    require_pass(record["criteria"], expected)
    print(f"baseline checked={len(expected)} failures=0")


def run_features():
    name = "git-parallel-compat-progress"
    source = fingerprint()
    env = environment()
    env["NGIT_FIXTURE_DIR"] = str(TASK / "feature-fixtures")
    selected = ["S5.3"] + [key for key in ids(name) if key not in {"S5.2", "S5.3"}]
    for criterion in selected:
        if fingerprint() != source:
            raise RuntimeError("Source changed during acceptance")
        report = meter(name, env, criterion)
        item = next(item for item in checks(report) if item["id"] == criterion)
        require_pass([item], [criterion])
        print(f"acceptance {criterion}: PASS; {report['overall']}", flush=True)


def run_regression():
    source = fingerprint()
    env = environment()
    record = {"source_sha256": source, "reports": {}}
    for name in LEDGERS:
        if fingerprint() != source:
            raise RuntimeError("Source changed during regression")
        report = meter(name, env)
        record["reports"][name] = report
        (TASK / "regression-results.json").write_text(json.dumps(record, indent=2) + "\n")
        print(f"regression {name}: {report['overall']}", flush=True)
    check_regression()


def check_regression():
    record = json.loads((TASK / "regression-results.json").read_text())
    if record["source_sha256"] != fingerprint():
        raise RuntimeError("Regression results describe a different source tree")
    if set(record["reports"]) != set(LEDGERS):
        raise RuntimeError("Missing regression ledgers")
    count = 0
    for name in LEDGERS:
        expected = set(ids(name))
        require_pass(checks(record["reports"][name]), expected)
        count += len(expected)
    print(f"regression checked={count} failures=0")


if __name__ == "__main__":
    TASK.mkdir(parents=True, exist_ok=True)
    actions = {"run-baseline": run_baseline, "baseline-check": check_baseline,
               "run-features": run_features,
               "run-regression": run_regression, "regression-check": check_regression}
    if len(sys.argv) != 2 or sys.argv[1] not in actions:
        raise SystemExit("Expected " + ", ".join(actions))
    actions[sys.argv[1]]()
