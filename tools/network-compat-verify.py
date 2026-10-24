#!/usr/bin/env python3
"""Verify network/pathspec/directory-rename controls, regressions and integration."""
import argparse
import base64
from concurrent.futures import ThreadPoolExecutor
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import resource
import shutil
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
TASK = Path(os.environ.get("NGIT_NETWORK_TASK_DIR", ROOT / "target/network-20261024")).resolve()
OUT = TASK / "verification"
BASE = "8eb1cd5"
RECORD = {"commands": [], "checks": [], "overall_deadline_seconds": 540}
spec = importlib.util.spec_from_file_location("network_nested_verifier", ROOT / "tools/nested-verify.py")
V = importlib.util.module_from_spec(spec)
spec.loader.exec_module(V)
V.ROOT, V.TASK, V.OUT, V.RECORD = ROOT, TASK, OUT, RECORD
execute, remaining_timeout = V.execute, V.remaining_timeout
compare_tuple = V.compare_tuple
source_proof, fingerprint, state, notes_root = V.source_proof, V.fingerprint, V.state, V.notes_root
require, git = V.require, V.git
CONTROL_CASES = (("pathspec-magic.py", "read", "status-0"),
                 ("directory-rename.py", "clean", "basic"),
                 ("smart-http.py", "discovery", "https refs"))


def reject_runtime_failure(actual):
    "Reject raw runtime diagnostics before evaluating semantic predicates."
    V.reject_runtime_failure(actual)
    require(actual[0] != 124 and not re.search(
        rb"void-function|void-variable|wrong-type-argument|Cannot open load file|"
        rb"Symbol's function definition is void|memory limit|panicked at|"
        rb"^ngit: unsupported command", actual[2], re.M),
        "Runtime/load/timeout failure is not a semantic result")


def environment():
    env = V.environment()
    for key in ("NGIT_COMPAT_MATCH", "NGIT_COMPAT_COMMAND", "NGIT_COMPAT_REPORT",
                "NGIT_TEST_MATCH", "NGIT_PUSH_CASE", "NGIT_FIXTURE_DIR", "NGIT_PUSH_OUTPUT",
                "GIT_SSL_NO_VERIFY", "GIT_CONFIG_PARAMETERS", "GIT_CONFIG_COUNT",
                "http_proxy", "https_proxy", "all_proxy", "HTTP_PROXY", "HTTPS_PROXY", "ALL_PROXY"):
        env.pop(key, None)
    for key in list(env):
        if key.startswith(("GIT_CONFIG_KEY_", "GIT_CONFIG_VALUE_")):
            env.pop(key)
    env.update(GIT_TERMINAL_PROMPT="0", GIT_EDITOR="true", GIT_MERGE_AUTOEDIT="no")
    return env


def verify_originals(root, expected):
    require(len(expected) == 57, "Expected exactly 57 original test sources")
    for name, digest in expected.items():
        path = root / name
        require(path.is_file() and V.sha(path) == digest, f"Original test source drift: {name}")


def provenance(env=None):
    commit = git("rev-parse", BASE, env=env).decode().strip()
    git("merge-base", "--is-ancestor", commit, "HEAD", env=env)
    names = git("ls-tree", "-r", "--name-only", commit, "test", env=env).decode().splitlines()
    require(len(names) == 57, "Pinned commit does not contain exactly 57 test sources")
    expected = {name: hashlib.sha256(git("show", f"{commit}:{name}", env=env)).hexdigest() for name in names}
    verify_originals(ROOT, expected)
    manifest = TASK / "original-test-hashes.json"
    if manifest.exists():
        require(json.loads(manifest.read_text()) == expected, "Saved manifest differs from Git objects")
    else:
        TASK.mkdir(parents=True, exist_ok=True)
        manifest.write_text(json.dumps(expected, sort_keys=True) + "\n")
    RECORD.update(baseline_commit=commit, original_test_hashes=expected)
    return commit, expected


def unpack(value):
    require(isinstance(value, dict) and type(value.get("exit")) is int, "Invalid command tuple")
    return (value["exit"],
            base64.b64decode(value.get("stdout_b64", value.get("stdout", "")), validate=True),
            base64.b64decode(value.get("stderr_b64", value.get("stderr", "")), validate=True))


def validate_control(exit_code, path, stdout, streams, old, name):
    """Adapt lane tuple formats, then invoke the existing audited validator."""
    require(path.is_file(), "Missing control report")
    report = json.loads(path.read_text())
    cases = report.get("cases", [])
    named = [c for c in cases if c.get("name") == name]
    require(len(named) == 1, "Named control missing or ambiguous")
    converted = dict(report, cases=[])
    for case in cases:
        left, right = case.get("expected", case.get("git")), case.get("actual", case.get("ngit"))
        if not isinstance(left, dict) or "exit" not in left:
            continue  # State assertions are not command tuples.
        a, b = unpack(left), unpack(right)
        reject_runtime_failure(a)
        reject_runtime_failure(b)
        def encode(value):
            return {"exit": value[0], "stdout": base64.b64encode(value[1]).decode(),
                    "stderr": base64.b64encode(value[2]).decode()}
        converted["cases"].append({"git": encode(a), "ngit": encode(b)})
    left, right = unpack(named[0].get("expected", named[0].get("git"))), unpack(named[0].get("actual", named[0].get("ngit")))
    require(bool(converted["cases"]), "No actual control tuples")
    if old:
        require(report.get("failures", 0) > 0 and left != right, "Named old control is not semantic RED")
    else:
        require(left == right, "Named current control tuple is not GREEN")
    adapted = streams / "adapted-report.json"
    adapted.write_text(json.dumps(converted))
    return V.validate_driver(exit_code, adapted, stdout, streams, old)


def controls(env, commit):
    baseline = TASK / "baseline"
    if not baseline.exists():
        result = execute(["git", "worktree", "add", "--detach", baseline, commit], label="baseline-create", timeout=60)
        require(result[0] == 0, "Cannot create pinned baseline worktree")
    require(git("rev-parse", "HEAD", cwd=baseline).decode().strip() == commit, "Baseline HEAD mismatch")
    proof = source_proof(baseline)
    names = git("ls-tree", "-r", "--name-only", commit, "bin/ngit", "lisp", env=env).decode().splitlines()
    expected = {name: hashlib.sha256(git("show", f"{commit}:{name}", env=env)).hexdigest()
                for name in names if name == "bin/ngit" or name.endswith(".el")}
    require(proof == expected, "Baseline executable source membership/content drift")
    RECORD["executables"] = {"baseline": proof, "current": source_proof(ROOT)}
    for driver, mode, name in CONTROL_CASES:
        for old in (True, False):
            label = f"control-{driver}-{mode}-{'old' if old else 'new'}"
            area = Path(tempfile.mkdtemp(prefix=label + "-", dir=OUT))
            report = area / "report.json"
            child_env = dict(env, NGIT_COMPAT_COMMAND=str((baseline if old else ROOT) / "bin/ngit"),
                             NGIT_COMPAT_MATCH=name, NGIT_COMPAT_REPORT=str(report), NGIT_NO_ELC="1",
                             NGIT_FIXTURE_DIR=str(area / "fixtures"))
            start = time.monotonic()
            result = execute([notes_root() / "bin/heavy-run.sh", sys.executable, ROOT / "test" / driver, mode],
                             env=child_env, label=label, timeout=180)
            counts = validate_control(result[0], report, result[1], area, old, name)
            RECORD["checks"].append({"driver": driver, "mode": mode, "name": name,
                                     "old": old, "wall_seconds": time.monotonic()-start, **counts})
    require(source_proof(baseline) == proof, "Baseline sources changed during controls")


def validate_regression(kind, mode, result, area):
    reject_runtime_failure(result)
    require(result[0] == 0 and not re.search(rb"^(FAIL|MISMATCH)\b", result[1], re.M),
            f"Regression failed: {kind}/{mode}")
    if kind == "transport":
        require(f"transport {mode}: PASS".encode() in result[1], "Missing transport success")
        trace = area / "transport.trace"
        require(trace.is_file(), "Missing owned transport trace")
        count = len(re.findall(rb"^\+ cmp -s ", trace.read_bytes(), re.M))
        require(count > 0, "No actual traced transport byte comparisons")
    elif kind == "merge-rename":
        match = re.findall(rb"^"+mode.encode()+rb": ([0-9]+)/([0-9]+) cases passed$", result[1], re.M)
        require(len(match) == 1 and int(match[0][0]) == int(match[0][1]) > 0,
                "Empty/incomplete merge-rename run")
        count = int(match[0][0])
        require(len(re.findall(rb"^PASS ", result[1], re.M)) == count, "Missing merge case checks")
    elif kind == "push-safety":
        match = re.findall(rb"^push-safety "+mode.encode()+rb": ([0-9]+) cases, failures=0$", result[1], re.M)
        require(len(match) == 1 and int(match[0]) > 0, "Empty/incomplete push-safety run")
        count = int(match[0])
        require(len(re.findall(rb"^CHECK ", result[1], re.M)) == count, "Missing push case checks")
        checked = 0
        for left in area.glob("push-output/"+mode+"/git-*"):
            if left.suffix not in (".out", ".err", ".rc", ".local", ".remote"):
                continue
            right = left.with_name("ngit-" + left.name[4:])
            require(right.is_file() and left.read_bytes() == right.read_bytes(), "Push artifact byte mismatch")
            checked += 1
        require(checked == 5*count, "Incomplete push byte artifacts")
    else:
        match = re.findall(rb"^PASS "+mode.encode()+rb": ([0-9]+) comparisons$", result[1], re.M)
        require(len(match) == 1 and int(match[0]) > 0, "Empty compare.sh run")
        count = int(match[0])
    return {"kind": kind, "mode": mode, "comparisons": count}


def run_regression_job(job, env):
    "Run one unchanged suite, keeping tracing outside compared stderr streams."
    kind, mode = job
    area = Path(tempfile.mkdtemp(prefix=f"regression-{kind}-{mode}-", dir=OUT))
    child_env = dict(env, NGIT_FIXTURE_DIR=str(area / "fixtures"),
                     NGIT_PUSH_OUTPUT=str(area / "push-output"))
    script = "compare.sh" if kind == "compare" else kind + ".sh"
    gate = notes_root() / "bin/heavy-run.sh"
    if kind == "transport":
        child_env["NGIT_TRACE_FILE"] = str(area / "transport.trace")
        child_env.pop("BASH_ENV", None)
        child_env.pop("ENV", None)
        # The fixed wrapper opens its owned descriptor before the traced
        # interpreter starts. Paths travel only through environment/argv.
        wrapper = 'exec 9>"$NGIT_TRACE_FILE"; export BASH_XTRACEFD=9; PS4="+ "; export PS4; exec bash -x "$@"'
        argv = [gate, "bash", "-c", wrapper, "network-transport-trace", ROOT / "test" / script, mode]
    else:
        argv = [gate, "sh", ROOT / "test" / script, mode]
    start = time.monotonic()
    result = execute(argv, env=child_env, label=f"regression-{kind}-{mode}", timeout=540)
    return dict(validate_regression(kind, mode, result, area), wall_seconds=time.monotonic()-start)


def regression(env):
    ordinary = ("status", "status-long", "diff", "add", "add-all", "restore", "log-path", "log-diff",
                "show", "grep", "ls-tree", "merge", "cherry-pick", "rebase", "stash-merge", "commit-output")
    jobs = [("compare", m) for m in ordinary]
    jobs += [("transport", m) for m in ("clone", "fetch", "push", "ssh")]
    jobs += [("merge-rename", m) for m in ("clean", "conflicts", "sequencer", "e2e")]
    jobs += [("push-safety", m) for m in ("explicit", "tracking", "atomic", "e2e")]
    with ThreadPoolExecutor(max_workers=3) as pool:
        for record in pool.map(lambda job: run_regression_job(job, env), jobs):
            RECORD["checks"].append(record)


def semantic_state(repo, env):
    "Preserve repository state while normalizing physical object/index/ref storage."
    value = state(repo, env)
    def physical(path):
        return (path in (".git/objects", ".git/refs", ".git/index", ".git/packed-refs")
                or path.startswith((".git/objects/", ".git/refs/", ".git/sharedindex.")))
    value["files"] = {path: item for path, item in value["files"].items()
                      if not physical(path) and path != ".git/config"}
    # Empty operation directories also change Git's behavior. Keep all
    # metadata directories outside the normalized physical storage trees.
    value["metadata_directories"] = sorted(str(path.relative_to(repo))
        for path in (repo/".git").rglob("*")
        if path.is_dir() and not path.is_symlink() and not physical(str(path.relative_to(repo))))
    head = execute(["git", "symbolic-ref", "--quiet", "HEAD"], cwd=repo, env=env,
                   label="state-symbolic-head", timeout=30)
    reject_runtime_failure(head)
    require(head[0] in (0, 1) and not head[2], "Cannot inspect symbolic HEAD")
    value["symbolic_head"] = [head[0], head[1].hex()]
    value["symbolic_refs"] = git("for-each-ref", "--format=%(refname)%00%(objectname)%00%(symref)",
                                  cwd=repo, env=env).hex()
    value["index_flags"] = git("ls-files", "-v", "--stage", "-z", cwd=repo, env=env).hex()
    config = {}
    for entry in git("config", "--local", "--null", "--list", cwd=repo, env=env).split(b"\0"):
        if entry:
            key, separator, item = entry.partition(b"\n")
            config.setdefault(key.hex(), []).append(item.hex() if separator else None)
    value["local_config"] = config
    return value


def require_state(expected, actual):
    require(expected == actual, "Integrated refs/index/worktree/reflogs/fsck mismatch")


def http_evidence(requests):
    "Require real smart HTTP exchanges using the fixture's ordered headers API."
    require(requests and all(len(item) == 5 for item in requests), "Missing HTTP request evidence")
    for method, path, headers, body, pairs in requests:
        require(isinstance(pairs, list) and all(len(pair) == 2 for pair in pairs),
                "Missing raw ordered request headers")
    require(any(method == "GET" and "service=git-upload-pack" in path
                for method, path, *_ in requests), "Missing upload-pack discovery")
    for service in ("git-upload-pack", "git-receive-pack"):
        require(any(method == "POST" and path.endswith("/"+service)
                    for method, path, *_ in requests), f"Missing {service} RPC")
    return [{"method": method, "path": path, "headers": pairs,
             "body_bytes": len(body), "body_sha256": hashlib.sha256(body).hexdigest()}
            for method, path, headers, body, pairs in requests]


def remote_state(repo, env):
    fsck = execute(["git", "--git-dir="+str(repo), "fsck", "--full", "--no-dangling"], env=env, label="remote-fsck", timeout=60)
    require(fsck[0] == 0, "Remote full fsck failed")
    return {"refs": git("--git-dir="+str(repo), "show-ref", env=env).hex(),
            "objects": git("--git-dir="+str(repo), "rev-list", "--objects", "--all", env=env).hex(),
            "fsck": [fsck[0], fsck[1].hex(), fsck[2].hex()]}


def e2e(env):
    build = execute([notes_root()/"bin/heavy-run.sh", "sh", ROOT/"tools/build-elc.sh"], env=env, label="build", timeout=180)
    require(build[0] == 0, "Compilation failed")
    stamp = ROOT / "lisp/.elc-stamp"
    require(stamp.is_file() and all(p.stat().st_mtime_ns <= stamp.stat().st_mtime_ns for p in (ROOT/"lisp").glob("*.el")), "Stale compiled mode")
    excluded = {"ngit-main.el", "ngit-profile.el", "ngit-native.el", "ngit-transport-kernel-source.el"}
    artifacts = [p.with_suffix(".elc") for p in (ROOT/"lisp").glob("*.el") if p.name not in excluded]
    require(all(p.is_file() for p in artifacts), "Missing compiled module")
    RECORD["compiled_artifacts"] = {str(p.relative_to(ROOT)): V.sha(p) for p in artifacts}
    fixture_spec = importlib.util.spec_from_file_location("network_http_fixture", ROOT/"test/smart-http-fixture.py")
    fixture = importlib.util.module_from_spec(fixture_spec)
    fixture_spec.loader.exec_module(fixture)
    gate = notes_root()/"bin/heavy-run.sh"
    for mode in ("compiled", "source"):
        area = Path(tempfile.mkdtemp(prefix="e2e-"+mode+"-", dir=OUT))
        seed, origin, backup, client, upstream = [area/n for n in ("seed", "origin.git", "backup.git", "client", "upstream")]
        child_env = dict(env)
        if mode == "source": child_env["NGIT_NO_ELC"] = "1"
        git("init", "-q", "--template=", "-b", "main", str(seed), env=child_env)
        for name, content in (("old/a.txt", b"first original\n"*10), ("old/b.txt", b"second original\n"*10), ("work/deep/anchor", b"nested\n")):
            path = seed/name; path.parent.mkdir(parents=True, exist_ok=True); path.write_bytes(content)
        git("add", ".", cwd=seed, env=child_env); git("commit", "-qm", "base", cwd=seed, env=child_env)
        git("clone", "-q", "--bare", "--template=", str(seed), str(origin), env=child_env)
        git("--git-dir="+str(origin), "config", "http.receivepack", "true", env=child_env)
        shutil.copytree(origin, backup)
        observations = []
        with fixture.SmartHTTPServer(area, tls=True) as server:
            url = server.url("origin.git")
            child_env["GIT_SSL_CAINFO"] = server.ca_file
            tls_ca_sha256 = V.sha(Path(server.ca_file))
            requests = []
            for native in (True, False):
                request_start = len(server.requests)
                for path in (origin, client, upstream):
                    if path.exists(): shutil.rmtree(path)
                shutil.copytree(backup, origin)
                tuples = []
                def command(label, args, cwd=area):
                    argv = (["git"] if native else [gate, ROOT/"bin/ngit"]) + args
                    result = execute(argv, cwd=cwd, env=child_env, label=f"e2e-{mode}-{native}-{label}", timeout=120)
                    reject_runtime_failure(result)
                    require(result[0] == 0, f"Integrated step failed: {label}")
                    tuples.append((label, result))
                    return result[1]
                command("clone", ["clone", "-q", "--template=", url, str(client)])
                for key, value in (("merge.directoryRenames", "true"), ("merge.stat", "false"), ("gc.auto", "0"), ("commit.gpgsign", "false")):
                    git("config", key, value, cwd=client, env=child_env)
                git("clone", "-q", "--template=", str(origin), str(upstream), env=child_env)
                (upstream/"old").rename(upstream/"new")
                git("add", "-A", cwd=upstream, env=child_env); git("commit", "-qm", "remote directory rename", cwd=upstream, env=child_env)
                git("push", "-q", str(origin), "main", cwd=upstream, env=child_env)
                remote_tip = git("rev-parse", "HEAD", cwd=upstream, env=child_env).strip().decode()
                content = b"opposite addition\0\xff\n"
                (client/"old/added.txt").write_bytes(content)
                (client/"old/excluded.txt").write_bytes(b"excluded precious\n")
                (client/"outside.txt").write_bytes(b"outside precious\n")
                nested = client/"work/deep"
                command("magic-add", ["add", "--", ":(top,glob)old/*.txt", ":(top,exclude)old/excluded.txt"], nested)
                selected = command("selected", ["diff", "--cached", "--name-only"], nested)
                require(selected == b"old/added.txt\n", "Empty/wrong nested magic selection")
                command("commit", ["commit", "-q", "-m", "opposite addition"], nested)
                require(git("show", "HEAD:old/added.txt", cwd=client, env=child_env) == content, "Selected bytes lost")
                require(git("ls-files", "--", "old/excluded.txt", "outside.txt", cwd=client, env=child_env) == b"", "Excluded changes staged")
                command("fetch", ["fetch", "-q", "origin"], nested)
                command("merge", ["merge", "-q", "--no-edit", "-m", "network integration", "origin/main"], nested)
                require((client/"new/added.txt").read_bytes() == content and not (client/"old/added.txt").exists(), "Directory addition not relocated")
                require((client/"old/excluded.txt").read_bytes() == b"excluded precious\n", "Excluded worktree bytes lost")
                require((client/"outside.txt").read_bytes() == b"outside precious\n", "Outside worktree bytes lost")
                command("atomic-leased-push", ["push", "-q", "--atomic", "--force-with-lease=refs/heads/main:"+remote_tip,
                                                "origin", "HEAD:refs/heads/main", "HEAD:refs/heads/integration"], nested)
                observations.append((tuples, semantic_state(client, child_env), remote_state(origin, child_env)))
                requests.append({"native": native,
                                 "requests": http_evidence(server.requests[request_start:])})
            expected, actual = observations
            require(len(expected[0]) == len(actual[0]) == 7, "Missing integrated command observations")
            for (name, left), (other, right) in zip(expected[0], actual[0]):
                require(name == other, "Integrated step ordering mismatch")
                compare_tuple(left, right, meaningful=name == "selected")
            require_state(expected[1:], actual[1:])
            RECORD["checks"].append({"mode": mode, "comparisons": 7, "state_equal": True,
                                     "verified_tls": True, "tls_ca_sha256": tls_ca_sha256,
                                     "http": requests})


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("controls", "regression", "e2e"))
    args = parser.parse_args()
    soft, hard = resource.getrlimit(resource.RLIMIT_AS)
    resource.setrlimit(resource.RLIMIT_AS, (min(soft, 8*1024**3) if soft >= 0 else 8*1024**3, hard))
    OUT.mkdir(parents=True, exist_ok=True)
    RECORD["mode"] = args.mode
    start, status = time.monotonic(), 1
    try:
        before = fingerprint(); RECORD["source_before"] = before
        env = environment()
        commit, expected = provenance(env)
        {"controls": lambda: controls(env, commit), "regression": lambda: regression(env), "e2e": lambda: e2e(env)}[args.mode]()
        RECORD["source_after"] = fingerprint()
        require(before == RECORD["source_after"], "Source mutation during verification")
        verify_originals(ROOT, expected)
        status = 0
    except Exception as error:
        RECORD["error"] = f"{type(error).__name__}: {error}"
    try:
        RECORD["source_final"] = fingerprint()
        require(RECORD.get("source_before") == RECORD["source_final"], "Final source fingerprint changed")
        remaining_timeout(1)
    except Exception as error:
        RECORD["final_source_error"] = str(error); status = 1
    RECORD.update(exit=status, wall_seconds=time.monotonic()-start)
    (OUT/(args.mode+".json")).write_text(json.dumps(RECORD, indent=2)+"\n")
    print(f"{args.mode}: {'PASS' if status == 0 else 'FAIL'}; record={OUT/(args.mode+'.json')}")
    return status


if __name__ == "__main__":
    sys.exit(main())
