#!/usr/bin/env python3
"""Independent retained-policy acceptance and immediate-edit comparisons."""
import importlib.util
import json
import os
from pathlib import Path
import resource
import select
import statistics
import subprocess
import sys
import time

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[1]
SOURCE = Path(os.environ.get("NGIT_CONFIG_SOURCE_ROOT", ROOT)).resolve()
REPORT = Path(os.environ.get("NGIT_STATUS_CONFIG_REPORT_DIR", ROOT / "target/status-config-20261028/tests"))
spec = importlib.util.spec_from_file_location("status_session", ROOT / "test/status-session.py")
h = importlib.util.module_from_spec(spec)
spec.loader.exec_module(h)
h.REPORT = REPORT
h.NGIT = Path(os.environ.get("NGIT_STATUS_BIN", SOURCE / "bin/ngit")).resolve()
require, case, Fixture, Server = h.require, h.case, h.Fixture, h.Server
MINIMUM = dict(reuse=3, ownership=9, controls=2, protocol=4, benchmark=1)


def probe(f, source=SOURCE, mode="server"):
    f.env.update(NGIT_CONFIG_SOURCE_ROOT=str(source), NGIT_CONFIG_PROBE_MODE=mode)
    return [f.env.get("NGIT_NELISP", "nelisp"), "--load", str(ROOT / "test/status-session-config.el")]


def counts(s):
    code, output = s.request("stats")
    require(code == 0, "instrumented stats failed")
    result = dict(line.split("=", 1) for line in output.decode().splitlines())
    require(result.get("caller_context_ok") == "1", "private caller configuration changed")
    return {k: int(v) for k, v in result.items() if k != "cache_reason"}


def reuse_probe(source=SOURCE):
    with Fixture() as f, Server(f, command=probe(f, source)) as s:
        s.verify(ordinary=False)
        before = counts(s)
        require(before["cache_enabled"] == 1 and before["config_loads"] > 0,
                "startup did not certify actual configuration reader")
        for n in range(4):
            f.write("f00000", "selected edit %02d\n" % n)
            s.verify(ordinary=False)
            current = counts(s)
            require(current["incremental_updates"] == before["incremental_updates"] + n + 1,
                    "ordinary edit did not select an update")
            require(current["full_refreshes"] == before["full_refreshes"], "ordinary edit fully refreshed")
            require(current["tracked_entries_checked"] == before["tracked_entries_checked"] + n + 1,
                    "ordinary edit did not check exactly one tracked entry")
            require(current["config_loads"] == before["config_loads"],
                    "selected edit reloaded configuration: before=%d after=%d" %
                    (before["config_loads"], current["config_loads"]))
        return dict(selected_edits=4, initial=before, final=current)


def reload_probe(kind):
    with Fixture() as f, Server(f, command=probe(f)) as s:
        s.verify(ordinary=False)
        before = counts(s)
        if kind == "policy":
            f.write("日本語", "untracked\n")
            f.git("config", "core.quotePath", "false")
        elif kind == "metadata":
            f.write("f00000", "staged\n")
            f.git("add", "f00000")
        s.verify("refresh" if kind == "refresh" else "status", ordinary=False)
        after = counts(s)
        require(after["config_loads"] > before["config_loads"], kind + " failed to reload configuration")
        require(after["full_refreshes"] > before["full_refreshes"], kind + " failed to fully refresh")
        return dict(before=before, after=after)


def reuse():
    case("selected-edits-no-configuration-reload", reuse_probe)
    for kind in ("refresh", "policy"):
        case(kind + "-discards-policy", lambda k=kind: reload_probe(k))


def failed_read():
    with Fixture() as f, Server(f, command=probe(f)) as s:
        s.verify(ordinary=False)
        original = (f.repo / ".git/config").read_text()
        f.write(".git/config", "[broken\n")
        code, output = s.request()
        require(code != 0 and b"config" in output.lower(), "invalid configuration was not rejected")
        failed = counts(s)
        require(failed["cache_enabled"] == 0, "failure retained certified state")
        f.write(".git/config", original + "\n[core]\n quotePath = false\n")
        f.write("日本語", "untracked\n")
        s.verify(ordinary=False)
        recovered = counts(s)
        require(recovered["config_loads"] > failed["config_loads"], "recovery reused failed policy")
        require(recovered["cache_enabled"] == 1, "recovery failed to recertify")
        return dict(failed=failed, recovered=recovered)


def uncached():
    with Fixture() as f:
        included = f.home / "included"
        included.write_text("[core]\n quotePath = true\n")
        f.git("config", "include.path", str(included))
        f.write("日本語", "untracked\n")
        with Server(f, command=probe(f)) as s:
            s.verify(ordinary=False)
            before = counts(s)
            require(before["cache_enabled"] == 0, "include dependency incorrectly certified")
            included.write_text("[core]\n quotePath = false\n")
            s.verify(ordinary=False)
            after = counts(s)
            require(after["cache_enabled"] == 0 and after["config_loads"] > before["config_loads"],
                    "unsupported dependency reused retained policy")
            s.verify(ordinary=False)
            final = counts(s)
            require(final["config_loads"] > after["config_loads"], "uncached unchanged read reused policy")
            return dict(initial=before, changed=after, unchanged=final)


def lifecycle():
    with Fixture() as f, Fixture() as second:
        f.write("日本語", "untracked\n")
        second.write("日本語", "untracked\n")
        second.git("config", "core.quotePath", "false")
        f.env["NGIT_CONFIG_SECOND_REPO"] = str(second.repo)
        code, out, err = h.execute(probe(f, mode="ownership"), f.repo, f.env)
        require(not err and b"assertions=16 PASS" in out, "lifecycle probe incomplete")
        return out.decode().strip()


def nonlocal_exit(phase, kind):
    with Fixture() as f:
        if phase == "status":
            f.write("f00000", "interrupted dirty content\n")
            expected = f.oracle()[1]
            f.write("f00000", "initial 0\n")
        else:
            expected = f.oracle()[1]
        f.env["NGIT_CONFIG_EXPECTED"] = expected.decode()
        mode = "nonlocal-%s-%s" % (phase, kind)
        code, out, err = h.execute(probe(f, mode=mode), f.repo, f.env)
        require(not err and ("nonlocal " + mode).encode() in out and b" PASS\n" in out,
                "nonlocal public API probe incomplete")
        return out.decode().strip()


def false_policy():
    with Fixture() as f:
        f.write("日本語", "initial unicode\n")
        f.git("add", "日本語")
        f.git("commit", "-qm", "tracked Unicode policy fixture")
        f.git("config", "core.filemode", "false")
        f.git("config", "core.quotePath", "false")
        with Server(f, command=probe(f)) as s:
            s.verify(ordinary=False)
            transitions = []
            def selected(mutation, name):
                before = counts(s)
                mutation()
                s.verify(ordinary=False)
                after = counts(s)
                require(after["incremental_updates"] == before["incremental_updates"] + 1,
                        name + " was not selected")
                require(after["tracked_entries_checked"] == before["tracked_entries_checked"] + 1,
                        name + " did not check one entry")
                require(after["full_refreshes"] == before["full_refreshes"] and
                        after["config_loads"] == before["config_loads"], name + " reloaded policy")
                transitions.append(dict(name=name, before=before, after=after))
            def policy(key, value):
                before = counts(s)
                f.git("config", key, value)
                s.verify(ordinary=False)
                after = counts(s)
                require(after["full_refreshes"] > before["full_refreshes"] and
                        after["config_loads"] > before["config_loads"], key + " failed to invalidate")
                transitions.append(dict(name=key + "=" + value, before=before, after=after))
            selected(lambda: os.chmod(f.repo / "f00000", 0o755), "false-filemode-selected-chmod")
            require(f.oracle()[1] == b"", "false filemode should ignore executable bit")
            policy("core.filemode", "true")
            require(b"f00000" in f.oracle()[1], "true filemode should detect executable bit")
            policy("core.filemode", "false")
            require(f.oracle()[1] == b"", "false filemode should become clean again")
            selected(lambda: f.write("日本語", "selected unquoted edit\n"), "false-quotepath-selected-edit")
            require("日本語".encode() in f.oracle()[1], "false quotePath did not render UTF-8")
            policy("core.quotePath", "true")
            selected(lambda: f.write("日本語", "selected quoted edit\n"), "true-quotepath-selected-edit")
            require("日本語".encode() not in f.oracle()[1], "true quotePath did not escape UTF-8")
            return transitions


def ownership():
    case("same-process-lifecycle-and-private-context", lifecycle)
    case("failed-computation-reloads-new-policy", failed_read)
    case("metadata-discards-policy", lambda: reload_probe("metadata"))
    case("unsupported-reads-never-retain-policy", uncached)
    for phase in ("status", "open"):
        for kind in ("quit", "throw"):
            case("nonlocal-%s-%s" % (phase, kind), lambda p=phase, k=kind: nonlocal_exit(p, k))
    case("false-filemode-and-quotepath-selected-policy", false_policy)


def controls():
    def baseline():
        configured = os.environ.get("NGIT_STATUS_CONFIG_BASELINE")
        require(configured, "NGIT_STATUS_CONFIG_BASELINE is required")
        source = Path(configured).resolve()
        require((source / "lisp/ngit-status-session.el").is_file(), "baseline source missing")
        try:
            reuse_probe(source)
        except AssertionError as error:
            require("selected edit reloaded configuration:" in str(error),
                    "baseline failed for unrelated reason: " + str(error))
            return "frozen backend rejected by exact no-reload assertion: " + str(error)
        raise AssertionError("pre-change configuration reload backend passed")
    case("pre-change-reload-backend-rejected", baseline)
    case("stale-fake-output-rejected", h.stale_control)


def protocol():
    def incomplete(s):
        require(not s.buffer and not select.select([s.process.stdout], [], [], 0.03)[0],
                "server responded before a complete request line")
    def split():
        with Fixture() as f, Server(f) as s:
            s.verify(ordinary=False)
            for part in (b"sta", b"tus"):
                s.write(part)
                incomplete(s)
            s.write(b"\n")
            require(s.frame() == f.oracle(), "split verb/newline response differs")
    def pipeline():
        with Fixture() as f, Server(f) as s:
            expected = f.oracle()
            s.write(b"status\nstats\nrefresh\ninvalid\nstatus\nquit\nstatus\n")
            require(s.frame() == expected, "first pipelined status differs")
            code, body = s.frame()
            pairs = dict(line.split("=", 1) for line in body.decode().splitlines())
            require(code == 0 and all(key in pairs for key in h.FIELDS), "pipelined stats frame differs")
            require(int(pairs["requests"]) == 1, "pipelined stats order differs")
            require(s.frame() == expected, "pipelined refresh frame differs")
            code, body = s.frame()
            require(code == 64 and b"unknown request" in body, "pipelined invalid frame differs")
            require(s.frame() == expected, "last pipelined status differs")
            require(s.frame() == (0, b""), "pipelined quit acknowledgement differs")
            s.shutdown(eof=True)
    def limits():
        for size in (4095, 4096):
            with Fixture() as f, Server(f) as s:
                s.verify(ordinary=False)
                s.write(b"x" * size + b"\nstatus\n")
                code, body = s.frame()
                require(code == 64 and b"unknown request" in body,
                        "legal-length unknown line incorrectly oversized: %d" % size)
                require(s.frame() == f.oracle(), "status after legal-length unknown line differs")
        with Fixture() as f, Server(f) as s:
            s.verify(ordinary=False)
            s.write(b"x" * 4096)
            incomplete(s)
            s.write(b"xstatus\nquit\n")
            code, body = s.frame()
            require(code == 64 and b"exceeds 4096 bytes" in body, "split oversized line was not rejected")
            s.shutdown(eof=True)
    def partial_eof():
        with Fixture() as f, Server(f) as s:
            s.verify(ordinary=False)
            s.write(b"sta")
            incomplete(s)
            s.shutdown(eof=True)
    case("split-verb-and-newline", split)
    case("mixed-pipeline-order-and-terminal-quit", pipeline)
    case("exact-and-split-oversized-line-boundaries", limits)
    case("partial-line-eof-is-clean", partial_eof)


def benchmark():
    def measure():
        with Fixture(1200) as f:
            require(len(f.git("ls-files", "-z").split(b"\0")) - 1 == 1201, "benchmark fixture changed")
            index_size = len(f.git("show", ":f00000"))
            require(index_size == 10, "original index content size changed")
            f.git("update-index", "--test-untracked-cache")
            f.git("config", "core.untrackedCache", "true")
            f.git("update-index", "--untracked-cache")
            require(f.git("config", "--get", "core.untrackedCache").strip() == b"true", "cache not enabled")
            require(not f.git("config", "--get", "core.fsmonitor", expected=1), "unexpected fsmonitor")
            f.write("untracked/deep/file", "visible\n")
            for _ in range(2):
                h.execute(["git", "status", "--porcelain=v1"], f.repo, dict(f.env, GIT_OPTIONAL_LOCKS="1"))
            require(b"UNTR" in (f.repo / ".git/index").read_bytes(), "Git did not persist untracked cache")
            samples = dict(tracked_files=1201, samples_per_scenario=24, scenarios={},
                           settings=dict(untracked_cache="verified/enabled/primed", priming_queries=2,
                                         fsmonitor="absent", measured_optional_locks="0"))
            serial = 0
            def git_sample():
                nonlocal serial
                serial += 1
                prefix = REPORT / ("%s-git-%03d" % (h.RUN, serial))
                with prefix.with_suffix(".stderr").open("wb") as err:
                    start = time.perf_counter_ns()
                    process = subprocess.Popen(["git", "status", "--porcelain=v1"], cwd=f.repo, env=f.env,
                                               stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                                               stderr=err, start_new_session=True)
                    try:
                        output, _ = process.communicate(timeout=h.TIMEOUT)
                        elapsed = time.perf_counter_ns() - start
                    finally:
                        h.reap_group(process)
                diagnostic = prefix.with_suffix(".stderr").read_bytes()
                prefix.with_suffix(".stdout").write_bytes(output)
                require(not diagnostic and process.returncode == 0, "measured Git failed")
                return (process.returncode, output), elapsed
            expected = f.oracle()
            cold_start = time.perf_counter_ns()
            with Server(f) as s:
                require(s.request() == expected, "cold first status mismatch")
                samples["cold_first_startup_ns"] = time.perf_counter_ns() - cold_start
                for scenario in ("different-index-size", "same-index-size", "stable-dirty"):
                    if scenario == "stable-dirty":
                        f.write("f00000", "stable tracked dirty\n")
                        s.verify(ordinary=False)
                    resident, native, pairs = [], [], []
                    before = s.stats()
                    for n in range(24):
                        if scenario != "stable-dirty":
                            text = ("different-size-edit-%02d\n" if scenario == "different-index-size"
                                    else "edited%03d\n") % n
                            if scenario == "same-index-size" and n % 2:
                                text = "initial 0\n"
                            require((len(text.encode()) == index_size) == (scenario == "same-index-size"),
                                    "edit size does not match scenario")
                            f.write("f00000", text)
                        expected = f.oracle()
                        order = ("resident", "git") if n % 2 == 0 else ("git", "resident")
                        row = dict(pair=n, order=order, oracle_porcelain=expected[1].decode())
                        for impl in order:
                            if impl == "resident":
                                start = time.perf_counter_ns()
                                actual = s.request()
                                elapsed = time.perf_counter_ns() - start
                            else:
                                actual, elapsed = git_sample()
                            require(actual == expected, "measured output differs: " + scenario + "/" + impl)
                            (resident if impl == "resident" else native).append(elapsed)
                            row[impl + "_ns"] = elapsed
                        pairs.append(row)
                    after = s.stats()
                    require(after["full_refreshes"] == before["full_refreshes"], "unexpected benchmark full refresh")
                    edits = 0 if scenario == "stable-dirty" else 24
                    require(after["incremental_updates"] - before["incremental_updates"] == edits,
                            "selected update count differs: " + scenario)
                    require(after["tracked_entries_checked"] - before["tracked_entries_checked"] == edits,
                            "selected entry count differs: " + scenario)
                    if not edits:
                        require(after["cache_hits"] - before["cache_hits"] == 24, "stable dirty cache missed")
                    samples["scenarios"][scenario] = dict(pairs=pairs, before=before, after=after,
                        resident_median_ns=statistics.median(resident), git_median_ns=statistics.median(native))
            (REPORT / "benchmark-samples.json").write_text(json.dumps(samples, indent=2) + "\n")
            failures = [name for name, data in samples["scenarios"].items()
                        if name != "stable-dirty" and data["resident_median_ns"] >= data["git_median_ns"]]
            require(not failures, "new-edit medians do not beat Git CLI: " + ", ".join(failures))
            return {name: {k: data[k] for k in ("resident_median_ns", "git_median_ns")}
                    for name, data in samples["scenarios"].items()}
    case("verified-new-edit-latency-both-sizes", measure)


def main():
    require(len(sys.argv) == 2 and sys.argv[1] in MINIMUM, "mode must be reuse/ownership/controls/protocol/benchmark")
    mode = sys.argv[1]
    cap = 8 * 1024 ** 3
    soft, hard = resource.getrlimit(resource.RLIMIT_AS)
    resource.setrlimit(resource.RLIMIT_AS, (cap if soft < 0 else min(cap, soft), hard))
    REPORT.mkdir(parents=True, exist_ok=True)
    h.RUN = "%s-%d" % (mode, os.getpid())
    try:
        globals()[mode]()
    except Exception as error:
        h.RESULTS.append(dict(name="mode-completion", ok=False, detail=repr(error)))
    require_count = MINIMUM[mode]
    if len(h.RESULTS) != require_count:
        h.RESULTS.append(dict(name="scenario-count", ok=False,
                             detail="checked %d; require %d" % (len(h.RESULTS), require_count)))
    findings = sum(not row["ok"] for row in h.RESULTS)
    payload = dict(mode=mode, checked=len(h.RESULTS), required=require_count, findings=findings,
                   complete=len(h.RESULTS) == require_count, results=h.RESULTS)
    (REPORT / (mode + ".json")).write_text(json.dumps(payload, indent=2) + "\n")
    print("checked=%d findings=%d %s mode=%s complete=%d" %
          (len(h.RESULTS), findings, "FAIL" if findings else "PASS", mode, payload["complete"]))
    for row in h.RESULTS:
        if not row["ok"]:
            print("  %s: %s" % (row["name"], str(row["detail"])[:350]))
    return bool(findings)


if __name__ == "__main__":
    sys.exit(main())
