#!/usr/bin/env python3
"""Verify configuration, credential and similarity integration using audited helpers."""
import argparse
import base64
from concurrent.futures import ThreadPoolExecutor
import copy
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import resource
import shlex
import shutil
import sys
import tempfile
import time
from urllib.parse import urlsplit, urlunsplit

ROOT = Path(__file__).resolve().parents[1]
TASK = Path(os.environ.get("NGIT_USABILITY_TASK_DIR", ROOT / "target/usability-20261024")).resolve()
OUT = TASK / "out/integration"
BASE = "57bcb52a8617cb7327bf0f8f425cb2e8a68c7698"
spec = importlib.util.spec_from_file_location("usability_network", ROOT / "tools/network-compat-verify.py")
N = importlib.util.module_from_spec(spec)
spec.loader.exec_module(N)
V = N.V
RECORD = {"commands": [], "checks": [], "overall_deadline_seconds": 540}
for module in (N, V):
    module.ROOT, module.TASK, module.OUT, module.RECORD = ROOT, TASK, OUT, RECORD
N.BASE = BASE
N.CONTROL_CASES = (("config-includes.py", "read", "config-include-source-old-red"),
                   ("similarity-read.py", "baseline-semantic-red", "similarity-status-edited-baseline-semantic-red"),
                   ("credential-helpers.py", "helpers", "helper TLS challenge semantic old-red"))
DIRECTORY_RENAME_MODES = ("clean", "conflicts", "e2e")
HTTP_POLICY_MODES = {**dict.fromkeys(("untrusted CA", "wrong hostname", "discovery redirect", "redirect loop", "dumb response", "truncated packet", "bad service", "bad ref"), "discovery"),
                     "truncated pack rejected": "fetch",
                     **dict.fromkeys(("empty extraHeader resets", "URL secrets redacted", "RPC redirect refused without replay", "header injection refused"), "auth")}
HTTP_POLICY_ASSERTIONS = {"discovery redirect": ("redirect RPC correct endpoint",),
                          "truncated pack rejected": ("corrupt fetch preserves refs", "corrupt fetch full fsck", "corrupt fetch cleans incoming"),
                          "empty extraHeader resets": ("Basic headers on GET and POST",),
                          "URL secrets redacted": ("clone persistent URL anonymized", "fetch persistent URL anonymized", "cross origin strips secrets"),
                          "RPC redirect refused without replay": ("RPC redirect one POST",),
                          "header injection refused": ("ordered repeated headers", "RPC redirect one POST")}
HTTP_ASSERTIONS = {"discovery": {"runtime predicate rejects broken command", "redirect RPC correct endpoint"},
                   "fetch": {"unchanged fetch has no POST", "clone full fsck", "corrupt fetch preserves refs", "corrupt fetch full fsck", "corrupt fetch cleans incoming"},
                   "push": {"push sends one complete RPC", "stale lease has no POST", "race push has no replay", "atomic rejection leaves no refs", "push full fsck"},
                   "auth": {"clone persistent URL anonymized", "fetch persistent URL anonymized", "configured URL retains credentials", "Basic headers on GET and POST", "ordered repeated headers", "cross origin strips secrets", "RPC redirect one POST"},
                   "e2e": {"unchanged fetch has no POST", "clone full fsck", "remote full fsck"}}
require, execute, git = N.require, N.execute, N.git


def environment():
    env = N.environment()
    for key in list(env):
        if (key.startswith(("NGIT_COMPAT_", "NGIT_TEST_", "GIT_CONFIG_KEY_", "GIT_CONFIG_VALUE_"))
                or key in ("GIT_ASKPASS", "SSH_ASKPASS", "GIT_SSH", "GIT_SSH_COMMAND", "GIT_CONFIG",
                           "GIT_CONFIG_SYSTEM", "GIT_SSL_CAINFO", "CURL_CA_BUNDLE", "SSL_CERT_FILE",
                           "GIT_SSL_CERT", "GIT_SSL_KEY", "BASH_ENV", "ENV", "NO_PROXY", "no_proxy")):
            env.pop(key, None)
    return env


def verify_originals(root, expected):
    names = git("ls-tree", "-r", "--name-only", BASE, "test").decode().splitlines()
    require(len(names) == 64 and set(names) == set(expected), "Original manifest membership drift")
    for name in names:
        pinned = hashlib.sha256(git("show", BASE + ":" + name)).hexdigest()
        require(expected[name] == pinned, "Original manifest hash drift: " + name)
        require((root / name).is_file() and V.sha(root / name) == pinned, "Original source drift: " + name)


def provenance(env):
    commit = git("rev-parse", BASE, env=env).decode().strip()
    git("merge-base", "--is-ancestor", commit, "HEAD", env=env)
    names = git("ls-tree", "-r", "--name-only", commit, "test", env=env).decode().splitlines()
    expected = {name: hashlib.sha256(git("show", commit + ":" + name, env=env)).hexdigest() for name in names}
    verify_originals(ROOT, expected)
    manifest = TASK / "original-test-hashes.json"
    if manifest.exists():
        require(json.loads(manifest.read_text()) == expected, "Saved manifest differs from Git objects")
    else:
        manifest.parent.mkdir(parents=True, exist_ok=True)
        manifest.write_text(json.dumps(expected, sort_keys=True) + "\n")
    RECORD.update(baseline_commit=commit, original_test_hashes=expected, source_root=str(ROOT))
    return commit, expected


def smart_http_policy(case, expected, actual, mode, assertions):
    """Check one frozen-source policy without treating its tuples as exact matches."""
    name = case["name"]
    require(HTTP_POLICY_MODES.get(name) == mode, "HTTP policy name/mode is not allowed")
    require(case.get("passed") is True and case.get("runtime_error") is False, "HTTP driver flags contradict policy")
    for companion in HTTP_POLICY_ASSERTIONS.get(name, ()):
        require(assertions.get(companion) is True, "Missing/false HTTP companion invariant: " + companion)
    if name == "truncated pack rejected":
        command = case.get("command", [])
        require(len(command) == 5 and command[1] == "-C" and command[3:] == ["fetch", "-q"] and
                Path(command[2]).is_absolute() and Path(command[2]).name == "clone" and
                Path(command[2]).parent.name.startswith("ngit-smart-http-"), "Invalid corrupt-fetch command shape")
        require(expected[0] == 128 and expected[1] == b"" and re.fullmatch(rb"(?:warning: no common commits\n)?fatal: protocol error: bad pack header\n", expected[2]) and
                actual == (128, b"", b"fatal: pack is not a version 2 pack file\n"), "Invalid corrupt-fetch policy tuples")
        return "native corrupt-fetch tuple plus specific pack rejection and state invariants"
    urls = [value for value in case.get("command", []) if isinstance(value, str) and value.startswith(("http://", "https://"))]
    require(len(urls) == 1, "Missing/ambiguous HTTP policy target")
    parsed = urlsplit(urls[0])
    require(parsed.hostname in ("localhost", "127.0.0.1") and parsed.port and 0 < parsed.port < 65536, "Unexpected HTTP fixture origin")
    path = "/repo.git"
    if name == "discovery redirect": path = "/redirect.git"
    elif name == "redirect loop": path = "/loop.git"
    elif name in ("dumb response", "truncated packet", "bad service", "bad ref"): path = "/bad.git"
    scheme = "http" if mode == "discovery" and name not in ("untrusted CA", "wrong hostname") else "https"
    host = "127.0.0.1" if name == "wrong hostname" else "localhost"
    require((parsed.scheme, parsed.hostname, parsed.path) == (scheme, host, path), "HTTP policy target differs from frozen fixture")
    if mode == "auth":
        require((parsed.username, parsed.password) == ("alice", "password"), "HTTP policy authentication target differs")
    safe = urlunsplit((parsed.scheme, parsed.hostname + ":" + str(parsed.port), parsed.path.rstrip("/"), "", ""))
    target = re.escape(safe.encode())
    require(not any(secret in actual[1] + actual[2] for secret in (b"password", b"alice", b"YWxpY2U6cGFzc3dvcmQ=")), "Reflected HTTP credentials")
    if name == "discovery redirect":
        redirected = safe.rsplit("/", 1)[0] + "/repo.git/"
        require(expected[0] == actual[0] == 0 and expected[1] == actual[1] and
                re.fullmatch(rb"([0-9a-f]{40})\tHEAD\n\1\trefs/heads/main\n", actual[1]) and
                expected[2] == ("warning: redirecting to " + redirected + "\n").encode() and actual[2] == b"",
                "Invalid discovery redirect result/warning")
        return "native tuple with documented redirect-warning difference"
    if name == "URL secrets redacted":
        require(parsed.username == "alice" and parsed.password == "password" and expected == (0, b"", b""), "Invalid redaction placeholder/target")
        require("push" in case["command"] and case["command"][-1] == "HEAD:refs/heads/diagnostic" and actual[0] == 0 and actual[1] == b"" and
                actual[2] == ("To " + safe + "\n * [new branch]      HEAD -> diagnostic\n").encode(), "Invalid redacted push result")
        return "frozen-source successful-push placeholder plus bound redacted diagnostic"
    native = {
        "untrusted CA": rb"fatal: unable to access '" + target + rb"/': server verification failed: certificate signer not trusted\. \(CAfile: [^\r\n]+ CRLfile: none\)\n",
        "wrong hostname": rb"fatal: unable to access '" + target + rb"/': SSL: certificate subject name \(localhost\) does not match target hostname '127\.0\.0\.1'\n",
        "redirect loop": rb"fatal: unable to access '" + target + rb"/': Maximum \(20\) redirects followed\n",
        "dumb response": rb"fatal: " + target + rb"/info/refs not valid: is this a git repository\?\n",
        "truncated packet": rb"fatal: Could not read from remote repository\.\n\nPlease make sure you have the correct access rights\nand the repository exists\.\n",
        "bad service": rb"fatal: invalid server response; got '# service=git-receive-pack'\n",
        "bad ref": rb"fatal: protocol error: unexpected 'invalid-ref'\n",
        "empty extraHeader resets": rb"fatal: unable to access '" + target + rb"/': GnuTLS recv error \(-110\): The TLS connection was non-properly terminated\.\n",
    }
    diagnostics = {"untrusted CA": b"fatal: HTTP transfer failed (code 60)\n", "wrong hostname": b"fatal: HTTP transfer failed (code 60)\n",
                   "redirect loop": b"fatal: HTTP redirect limit or missing location\n", "dumb response": b"fatal: unsupported smart HTTP response type\n",
                   "truncated packet": b"fatal: unexpected EOF in Git transport (3 of 36 bytes)\n", "bad service": b"fatal: invalid smart HTTP service preamble\n",
                   "bad ref": b"fatal: invalid HTTP advertised ref\n",
                   "empty extraHeader resets": b"fatal: HTTP transfer failed (code 56)\n", "RPC redirect refused without replay": b"fatal: HTTP RPC redirect refused\n",
                   "header injection refused": b"fatal: invalid http.extraHeader\n"}
    require(actual == (128, b"", diagnostics[name]), "Wrong HTTP policy exit/stdout/diagnostic")
    if name in ("RPC redirect refused without replay", "header injection refused"):
        require(expected == (1, b"", b""), "Invalid frozen-source error-policy placeholder")
        if name == "RPC redirect refused without replay":
            require("push" in case["command"] and case["command"][-1] == "HEAD:refs/heads/no-replay", "RPC refusal ref differs from fixture")
        return "frozen-source refusal placeholder plus specific error diagnostic"
    require(expected[0] == 128 and expected[1] == b"" and re.fullmatch(native[name], expected[2]), "Native HTTP policy diagnostic differs")
    return "native policy tuple with specific implementation diagnostic"


def strict_report(report, legacy=None, old=False, mode=None):
    """Validate every command, distinguishing explicitly allowlisted HTTP policies."""
    count, failures, errors = [report.get(key) for key in ("comparisons", "failures", "unexpected_errors")]
    require(all(type(value) is int and value >= 0 for value in (count, failures, errors)), "Invalid report counters")
    require(count > 0 and errors == 0, "Empty/error report")
    commands = assertions = workflows = policies = 0
    policy_cases = []
    companions = {case.get("name"): case.get("passed") for case in report.get("cases", []) if case.get("assertion") is True}
    mismatches = 0
    for case in report.get("cases", []):
        left, right = case.get("expected", case.get("git")), case.get("actual", case.get("ngit"))
        if legacy == "pathspec-magic.py" and case.get("name", "").endswith("-worktree-bytes"):
            require(isinstance(left, dict) and isinstance(right, dict), "Invalid legacy state maps")
            for value in (left, right):
                for key, body in value.items():
                    require(isinstance(key, str) and isinstance(body, str), "Invalid legacy state entry")
                    base64.b64decode(body, validate=True)
            require(type(case.get("pass")) is bool and case["pass"] == (left == right), "Legacy state pass flag differs")
            require(left == right, "Legacy worktree state mismatch")
            assertions += 1
            continue
        if "assertion" in case:
            require(case.get("assertion") is True and case.get("passed") is True, "Invalid/failed explicit state assertion")
            if legacy == "smart-http.py":
                require(case.get("assertion") is True and left is None and right is None and
                        case.get("name") in HTTP_ASSERTIONS.get(mode, set()), "Unknown/mixed HTTP state assertion")
                allowed = {"name", "assertion", "passed"}
                if case["name"] == "runtime predicate rejects broken command":
                    allowed.add("negative_control")
                    negative = case.get("negative_control", {})
                    require(negative.get("runtime_error") is True and negative.get("passed") is False and
                            N.unpack(negative.get("expected")) == (1, b"", b"") and
                            N.unpack(negative.get("actual")) == (1, b"", b"void-function: audit-runtime-fault\n"), "Invalid original runtime-guard evidence")
                require(set(case) <= allowed, "Unknown HTTP assertion fields")
            assertions += 1
            continue
        for value in (left, right):
            require(isinstance(value, dict) and
                    (("stdout_b64" in value and "stderr_b64" in value) or
                     ("stdout" in value and "stderr" in value)), "Incomplete command tuple")
        a, b = N.unpack(left), N.unpack(right)
        N.reject_runtime_failure(a); N.reject_runtime_failure(b)
        if legacy == "smart-http.py":
            require(case.get("passed") is True and case.get("runtime_error") is False, "HTTP command flags contradict evidence")
        commands += 1
        workflows += 1
        mismatches += a != b
        if legacy == "smart-http.py" and case.get("name") in HTTP_POLICY_MODES:
            policy_cases.append({"name": case["name"], "mode": mode, "oracle": smart_http_policy(case, a, b, mode, companions)})
            policies += 1
        elif not old:
            require(a == b, "Command tuple mismatch: " + str(case.get("name")))
        if legacy == "directory-rename.py":
            allowed = {"exit", "stdout", "stderr", "continue", "abort"}
            require(set(left) <= allowed and set(right) <= allowed, "Unknown workflow tuple field")
            require(set(left) == set(right), "Workflow command membership mismatch")
            for finish in ("continue", "abort"):
                if finish in left:
                    require(set(left[finish]) == set(right[finish]) == {"exit", "stdout", "stderr"}, "Invalid nested command fields")
                    x, y = N.unpack(left[finish]), N.unpack(right[finish])
                    N.reject_runtime_failure(x); N.reject_runtime_failure(y)
                    require(x == y, "Nested workflow tuple mismatch")
                    commands += 1
            require(case.get("errors") == [] and isinstance(case.get("git_state"), dict)
                    and case["git_state"] == case.get("ngit_state"), "Workflow state mismatch")
            assertions += 1
    require(commands > 0, "No actual command tuples")
    if legacy == "smart-http.py":
        require(commands > policies, "HTTP mode has no exact command comparisons")
        require(type(report.get("assertions")) is int and report["assertions"] == assertions, "HTTP assertion counter differs from evidence")
    expected_count = commands + assertions if legacy == "pathspec-magic.py" else workflows if legacy == "directory-rename.py" else commands
    require(count == expected_count, "Reported comparison count differs from actual evidence")
    require((failures > 0 and mismatches > 0) if old else failures == 0, "Invalid semantic failure counters")
    return {"comparisons": commands - policies, "assertions": assertions, "reported_comparisons": count,
            **({"exact_commands": commands - policies, "policy_commands": policies, "commands": commands, "policy_cases": policy_cases} if legacy == "smart-http.py" else {}),
            **({"reported_workflows": workflows} if legacy == "directory-rename.py" else {})}


def runtime_probe(argv, env):
    value = execute(argv + ["config", "--get", "core.repositoryformatversion"], env=env, label="runtime-probe", timeout=60)
    N.reject_runtime_failure(value)
    require(value == (0, b"0\n", b""), "Runtime preflight failed")


def controls(env, commit):
    baseline = TASK / "baseline"
    if not baseline.exists():
        require(execute(["git", "worktree", "add", "--detach", baseline, commit], env=env, label="baseline-create", timeout=60)[0] == 0, "Cannot create baseline")
    for root in (baseline, ROOT):
        runtime_probe([N.notes_root() / "bin/heavy-run.sh", root / "bin/ngit"], dict(env, NGIT_NO_ELC="1"))
    previous = set(OUT.glob("control-*/report.json"))
    N.controls(env, commit)
    reports = set(OUT.glob("control-*/report.json")) - previous
    require(len(reports) == 6, "Missing fresh control reports")
    for report in reports:
        strict_report(json.loads(report.read_text()), old="-old-" in report.parent.name)


def validate_trace(area, captures):
    trace = area / "transport.trace"
    require(trace.is_file(), "Missing transport trace")
    operands = []
    for line in trace.read_text().splitlines():
        if line.startswith("+ cmp"):
            args = shlex.split(line)
            require(len(args) == 5 and args[:3] == ["+", "cmp", "-s"], "Malformed cmp trace")
            operands.append(args[3:])
    require(operands, "Zero traced comparisons")
    records = [json.loads(path.read_text()) for path in captures.glob("*.json")]
    require(len(records) == len(operands), "Trace/captured comparison count differs")
    for pair in operands:
        matches = [record for record in records if record["operands"] == pair]
        require(matches, "Missing captured comparison")
        record = matches.pop()
        records.remove(record)
        left, right = (base64.b64decode(record[key], validate=True) for key in ("left", "right"))
        require(left == right, "Captured comparison byte mismatch")
    require(not records, "Unmatched captured comparisons")
    return len(operands)


def regression_job(job, env):
    kind, mode = job
    if kind != "json":
        if kind != "transport":
            return N.run_regression_job(job, env)
        capture = Path(tempfile.mkdtemp(prefix="cmp-capture-", dir=OUT))
        wrapper = capture / "bin"
        wrapper.mkdir()
        real = shutil.which("cmp", path=env.get("PATH"))
        require(real, "Native cmp unavailable")
        (wrapper / "cmp").write_text(
            "#!/usr/bin/env python3\nimport base64,json,os,subprocess,sys\nfrom pathlib import Path\n"
            "args=sys.argv[1:]\n"
            "if len(args)==3 and args[0]=='-s':\n"
            " p=Path(os.environ['NGIT_CMP_CAPTURE'])/(str(os.getpid())+'.json')\n"
            " p.write_text(json.dumps(dict(operands=args[1:],left=base64.b64encode(Path(args[1]).read_bytes()).decode(),right=base64.b64encode(Path(args[2]).read_bytes()).decode(),trace=os.environ['NGIT_TRACE_FILE'])))\n"
            "os.execv(os.environ['NGIT_REAL_CMP'],[os.environ['NGIT_REAL_CMP'],*args])\n")
        (wrapper / "cmp").chmod(0o755)
        child = dict(env, PATH=str(wrapper) + os.pathsep + env.get("PATH", ""), NGIT_CMP_CAPTURE=str(capture), NGIT_REAL_CMP=real)
        result = N.run_regression_job(job, child)
        records = list(capture.glob("*.json"))
        require(records, "No captured cmp calls")
        area = Path(json.loads(records[0].read_text())["trace"]).parent
        result["verified_byte_comparisons"] = validate_trace(area, capture)
        return result
    driver, mode = mode.split(":")
    area = Path(tempfile.mkdtemp(prefix="regression-json-", dir=OUT))
    report = area / "report.json"
    child = dict(env, NGIT_COMPAT_REPORT=str(report), NGIT_COMPAT_COMMAND=str(ROOT / "bin/ngit"))
    start = time.monotonic()
    result = execute([N.notes_root() / "bin/heavy-run.sh", sys.executable, ROOT / "test" / driver, mode], env=child,
                     label="regression-" + driver + "-" + mode, timeout=540)
    N.reject_runtime_failure(result)
    require(result[0] == 0 and report.is_file(), "JSON regression failed: " + driver + "/" + mode)
    legacy = driver if driver in ("pathspec-magic.py", "directory-rename.py", "smart-http.py") else None
    if legacy:
        verify_originals(ROOT, RECORD["original_test_hashes"])
    return {"kind": driver, "mode": mode, "wall_seconds": time.monotonic() - start,
            **strict_report(json.loads(report.read_text()), legacy, mode=mode)}


def regression(env):
    jobs = [("compare", mode) for mode in ("status", "status-long", "diff", "log-diff", "show", "commit-output", "merge", "cherry-pick", "rebase", "stash-merge", "attr-config", "misc", "hooks", "hooks-extra")]
    jobs += [(kind, mode) for kind, modes in (("transport", ("clone", "fetch", "push", "ssh")), ("merge-rename", ("clean", "conflicts", "sequencer", "e2e")), ("push-safety", ("explicit", "tracking", "atomic", "e2e"))) for mode in modes]
    jobs += [("json", driver + ":" + mode) for driver, modes in (("smart-http.py", ("discovery", "fetch", "push", "auth", "e2e")), ("pathspec-magic.py", ("read", "write", "boundaries", "e2e")), ("directory-rename.py", DIRECTORY_RENAME_MODES)) for mode in modes]
    with ThreadPoolExecutor(max_workers=3) as pool:
        for record in pool.map(lambda job: regression_job(job, env), jobs):
            RECORD["checks"].append(record)


def e2e(env):
    fixture_spec = importlib.util.spec_from_file_location("usability_http", ROOT / "test/credential-http-fixture.py")
    fixture = importlib.util.module_from_spec(fixture_spec)
    fixture_spec.loader.exec_module(fixture)
    build = execute([N.notes_root() / "bin/heavy-run.sh", "sh", ROOT / "tools/build-elc.sh"], env=env, label="build", timeout=180)
    require(build[0] == 0, "Compilation failed")
    stamp = ROOT / "lisp/.elc-stamp"
    require(stamp.is_file() and all(path.stat().st_mtime_ns <= stamp.stat().st_mtime_ns for path in (ROOT / "lisp").glob("*.el")), "Stale compiled mode")
    excluded = {"ngit-main.el", "ngit-profile.el", "ngit-native.el", "ngit-transport-kernel-source.el"}
    artifacts = [path.with_suffix(".elc") for path in (ROOT / "lisp").glob("*.el") if path.name not in excluded]
    require(all(path.is_file() for path in artifacts), "Missing compiled module")
    RECORD["compiled_artifacts"] = {str(path.relative_to(ROOT)): V.sha(path) for path in artifacts}
    for mode in ("compiled", "source"):
        area = Path(tempfile.mkdtemp(prefix="e2e-" + mode + "-", dir=OUT))
        seed, origin, backup, client = [area / name for name in ("seed", "origin.git", "backup.git", "client")]
        log, helper = area / "helper.log", area / "helper"
        helper.write_text("#!/bin/sh\nprintf '%s %s\\n' \"$1\" \"$2\" >> \"$NGIT_HELPER_LOG\"\ncat >/dev/null\n[ \"$2\" = get ] || exit 0\ncase $1 in user) printf 'username=alice\\n';; password) printf 'password=password\\n';; poison) exit 1;; esac\n")
        helper.chmod(0o755)
        auth = '[credential]\n\thelper =\n' + ''.join('\thelper = ' + json.dumps('!' + shlex.quote(str(helper)) + ' ' + part) + '\n' for part in ('user', 'password'))
        (area / "auth.inc").write_text(auth)
        (area / "identity.inc").write_text('[user]\nname = Included Integration\nemail = included@example.test\n[diff]\nrenames = true\n[status]\nrenames = true\n')
        global_file = area / "global.config"
        global_file.write_text('[credential]\nhelper = ' + json.dumps('!' + shlex.quote(str(helper)) + ' poison') + '\n[include]\npath = auth.inc\npath = identity.inc\n')
        child = dict(env, GIT_CONFIG_GLOBAL=str(global_file), NGIT_HELPER_LOG=str(log))
        for key in ("GIT_AUTHOR_NAME", "GIT_AUTHOR_EMAIL", "GIT_COMMITTER_NAME", "GIT_COMMITTER_EMAIL"):
            child.pop(key, None)
        if mode == "source": child["NGIT_NO_ELC"] = "1"
        git("init", "-q", "--template=", "-b", "main", str(seed), env=child)
        content = b''.join((f"line {number:03d} stable original payload\n".encode()) for number in range(80))
        (seed / "old.txt").write_bytes(content)
        git("add", ".", cwd=seed, env=child); git("commit", "-qm", "base", cwd=seed, env=child)
        git("clone", "-q", "--bare", "--template=", str(seed), str(origin), env=child)
        git("--git-dir=" + str(origin), "config", "http.receivepack", "true", env=child)
        shutil.copytree(origin, backup)
        observations = []
        with fixture.CredentialHTTPServer(area, credentials="alice:password", tls=True) as server:
            child["GIT_SSL_CAINFO"] = server.ca_file
            evidence = []
            for native in (True, False):
                start = len(server.requests)
                for path in (origin, client):
                    if path.exists(): shutil.rmtree(path)
                shutil.copytree(backup, origin)
                log.write_text("")
                tuples = []
                def command(name, args, cwd=client):
                    argv = (["git"] if native else [N.notes_root() / "bin/heavy-run.sh", ROOT / "bin/ngit"]) + args
                    result = execute(argv, cwd=cwd, env=child, label=f"e2e-{mode}-{native}-{name}", timeout=120)
                    N.reject_runtime_failure(result)
                    require(result[0] == 0, "Integrated step failed: " + name)
                    tuples.append((name, result))
                    return result[1]
                command("clone", ["clone", "-q", "--template=", server.url("origin.git"), str(client)], area)
                require(command("included-name", ["config", "--get", "user.name"]) == b"Included Integration\n", "Included identity missing")
                command("included-helper", ["config", "--get", "credential.helper"])
                require(command("nonempty-clone", ["ls-files"]) == b"old.txt\n", "Empty/wrong clone")
                old_tip = git("rev-parse", "HEAD", cwd=client, env=child).strip().decode()
                (client / "old.txt").unlink()
                (client / "new.txt").write_bytes(content.replace(b"line 040 stable", b"line 040 edited"))
                command("add", ["add", "-A"])
                status = command("rename-status", ["status", "--porcelain"])
                require(b"R " in status and b"old.txt -> new.txt" in status, "No inexact status rename")
                score = command("rename-score", ["diff", "--cached", "--name-status"])
                match = re.fullmatch(rb"R([0-9]+)\told.txt\tnew.txt\n", score)
                require(match and 50 <= int(match[1]) < 100, "Rename score is not inexact")
                command("rename-patch", ["diff", "--cached"])
                command("commit", ["commit", "-q", "-m", "included credentials and edited rename"])
                identity = command("commit-identity", ["show", "-s", "--format=%an <%ae>", "HEAD"])
                require(identity == b"Included Integration <included@example.test>\n", "Commit ignored include identity")
                command("leased-atomic-push", ["push", "-q", "--atomic", "--force-with-lease=refs/heads/main:" + old_tip, "origin", "HEAD:refs/heads/main", "HEAD:refs/heads/integration"])
                command("fetch", ["fetch", "-q", "origin"])
                command("clean-status", ["status", "--porcelain"])
                command("refs", ["rev-parse", "HEAD", "refs/remotes/origin/main"])
                command("index", ["ls-files", "--stage"])
                operations = log.read_text().splitlines()
                require(all(name in operations for name in ("user get", "password get", "user store", "password store")) and not any("poison" in name for name in operations), "Missing helper chain/reset/lifecycle")
                requests = server.requests[start:]
                expected_auth = "Basic " + base64.b64encode(b"alice:password").decode()
                require(any(not item[2].get("Authorization") for item in requests) and any(item[2].get("Authorization") == expected_auth for item in requests), "Missing real challenge/authenticated request")
                evidence.append({"native": native, "operations": operations, "http": N.http_evidence(requests)})
                observations.append((tuples, N.semantic_state(client, child), N.remote_state(origin, child)))
            left, right = observations
            require(len(left[0]) == len(right[0]) == 15, "Missing integrated command tuples")
            tuples = []
            for (name, expected), (other, actual) in zip(left[0], right[0]):
                require(name == other, "Integrated ordering differs")
                N.compare_tuple(expected, actual, meaningful=name in ("rename-status", "rename-score", "nonempty-clone"))
                def packed(value):
                    return {"exit": value[0], "stdout_b64": base64.b64encode(value[1]).decode(), "stderr_b64": base64.b64encode(value[2]).decode()}
                tuples.append({"name": name, "expected": packed(expected), "actual": packed(actual)})
            N.require_state(left[1:], right[1:])
            RECORD["checks"].append({"mode": mode, "comparisons": 15, "assertions": 2, "state_equal": True, "cases": tuples, "authentication": evidence, "tls_ca_sha256": V.sha(Path(server.ca_file))})


def private_controls(env):
    """Exercise actual validators with corrupted reports, traces and real Git state."""
    require(DIRECTORY_RENAME_MODES == ("clean", "conflicts", "e2e"), "Unsupported immutable directory driver mode")
    RECORD["checks"].append({"name": "immutable-directory-driver-supported-modes", "modes": DIRECTORY_RENAME_MODES, "passed": True})
    def reject(name, function):
        try: function()
        except (RuntimeError, ValueError, KeyError, TypeError):
            RECORD["checks"].append({"name": name, "rejected": True})
        else: raise RuntimeError("Negative control accepted: " + name)
    value = {"exit": 0, "stdout_b64": "eAo=", "stderr_b64": ""}
    good = {"comparisons": 1, "failures": 0, "unexpected_errors": 0, "cases": [{"name": "one", "expected": copy.deepcopy(value), "actual": copy.deepcopy(value)}]}
    strict_report(good)
    reject("zero-report", lambda: strict_report(dict(good, comparisons=0, cases=[])))
    bad = copy.deepcopy(good); bad["cases"][0]["actual"]["stdout_b64"] = "eQo="
    reject("bad-tuple", lambda: strict_report(bad))
    bad = copy.deepcopy(good)
    bad["cases"].append({"name": "laundered-command", "assertion": False, "passed": True,
                         "expected": {"exit": 0, "stdout_b64": "eAo=", "stderr_b64": ""},
                         "actual": {"exit": 0, "stdout_b64": "eQo=", "stderr_b64": ""}})
    reject("false-assertion-command-laundering", lambda: strict_report(bad))
    for marker in (b"void-function: broken", b"Cannot open load file", b"nelisp: uncaught error"):
        bad = copy.deepcopy(good); bad["failures"] = 1
        bad["cases"][0]["actual"] = {"exit": 1, "stdout_b64": "", "stderr_b64": base64.b64encode(marker).decode()}
        reject("runtime-false-red-" + marker.decode(), lambda: strict_report(bad, old=True))
    legacy = copy.deepcopy(good); legacy["comparisons"] = 2
    legacy["cases"].append({"name": "one-worktree-bytes", "expected": {"x": "eAo="}, "actual": {"x": "eAo="}, "pass": True})
    strict_report(legacy, "pathspec-magic.py")
    bad = copy.deepcopy(legacy); bad["cases"][1]["actual"]["x"] = "eQo="
    reject("legacy-bad-state", lambda: strict_report(bad, "pathspec-magic.py"))
    reject("legacy-counter", lambda: strict_report(dict(legacy, comparisons=3), "pathspec-magic.py"))
    reject("legacy-state-only", lambda: strict_report(dict(legacy, comparisons=1, cases=legacy["cases"][1:]), "pathspec-magic.py"))
    command = {"exit": 0, "stdout": "eAo=", "stderr": ""}
    workflow = {"comparisons": 1, "failures": 0, "unexpected_errors": 0, "cases": [{"name": "workflow", "git": dict(command, abort=copy.deepcopy(command)), "ngit": dict(command, abort=copy.deepcopy(command)), "errors": [], "git_state": {"abort": {"x": 1}}, "ngit_state": {"abort": {"x": 1}}}]}
    strict_report(workflow, "directory-rename.py")
    bad = copy.deepcopy(workflow); bad["cases"][0]["ngit"]["abort"]["stdout"] = "eQo="
    reject("nested-bad-tuple", lambda: strict_report(bad, "directory-rename.py"))
    bad = copy.deepcopy(workflow); bad["cases"][0]["ngit_state"]["abort"]["x"] = 2
    reject("nested-bad-state", lambda: strict_report(bad, "directory-rename.py"))
    reject("workflow-counter", lambda: strict_report(dict(workflow, comparisons=2), "directory-rename.py"))
    def packed(value):
        return {"exit": value[0], "stdout_b64": base64.b64encode(value[1]).decode(), "stderr_b64": base64.b64encode(value[2]).decode()}
    refs = b"1" * 40 + b"\tHEAD\n" + b"1" * 40 + b"\trefs/heads/main\n"
    def policy_report(name, native, actual, companions=()):
        exact = {"name": "https refs", "expected": packed((0, refs, b"")), "actual": packed((0, refs, b"")), "passed": True, "runtime_error": False}
        policy = {"name": name, "command": ["ngit", "ls-remote", "https://localhost:12345/repo.git"], "expected": packed(native), "actual": packed(actual), "passed": True, "runtime_error": False}
        return {"comparisons": 2, "failures": 0, "unexpected_errors": 0, "assertions": len(companions), "cases": [exact, policy, *[{"name": item, "assertion": True, "passed": True} for item in companions]]}
    native_tls = (128, b"", b"fatal: unable to access 'https://localhost:12345/repo.git/': server verification failed: certificate signer not trusted. (CAfile: fixture-ca.pem CRLfile: none)\n")
    tls = policy_report("untrusted CA", native_tls, (128, b"", b"fatal: HTTP transfer failed (code 60)\n"))
    require(strict_report(tls, "smart-http.py", mode="discovery")["policy_commands"] == 1, "HTTP policy positive failed")
    for name, actual in (("wrong-tls-error", (128, b"", b"fatal: HTTP transfer failed (code 56)\n")), ("empty-policy-error", (128, b"", b"")), ("wrong-policy-exit", (1, b"", b"fatal: HTTP transfer failed (code 60)\n")), ("nonempty-policy-stdout", (128, b"unexpected\n", b"fatal: HTTP transfer failed (code 60)\n")), ("runtime-as-policy", (128, b"", b"void-function: false-green\n"))):
        bad = copy.deepcopy(tls); bad["cases"][1]["actual"] = packed(actual)
        reject(name, lambda: strict_report(bad, "smart-http.py", mode="discovery"))
    bad = copy.deepcopy(tls); bad["cases"][1]["expected"] = packed((128, b"", b"unrelated failure\n")); bad["cases"][1]["actual"] = copy.deepcopy(bad["cases"][1]["expected"])
    reject("equal-wrong-policy-tuples", lambda: strict_report(bad, "smart-http.py", mode="discovery"))
    reject("wrong-policy-mode", lambda: strict_report(tls, "smart-http.py", mode="auth"))
    bad = copy.deepcopy(tls); bad["cases"][1]["name"] = "unknown HTTP policy"
    reject("unknown-named-http-mismatch", lambda: strict_report(bad, "smart-http.py", mode="discovery"))
    reject("http-false-counter", lambda: strict_report(dict(tls, comparisons=3), "smart-http.py", mode="discovery"))
    reject("policy-only-fake-green", lambda: strict_report(dict(tls, comparisons=1, cases=tls["cases"][1:]), "smart-http.py", mode="discovery"))
    redirect = policy_report("discovery redirect", (0, refs, b"warning: redirecting to http://localhost:12345/repo.git/\n"), (0, refs, b""), HTTP_POLICY_ASSERTIONS["discovery redirect"])
    redirect["cases"][1]["command"][-1] = "http://localhost:12345/redirect.git"
    strict_report(redirect, "smart-http.py", mode="discovery")
    bad = copy.deepcopy(redirect); bad["cases"][1]["actual"]["stdout_b64"] = base64.b64encode(refs.replace(b"1", b"2")).decode()
    reject("redirect-success-stdout", lambda: strict_report(bad, "smart-http.py", mode="discovery"))
    bad = copy.deepcopy(redirect); bad["cases"][-1]["passed"] = False
    reject("false-http-companion", lambda: strict_report(bad, "smart-http.py", mode="discovery"))
    bad = copy.deepcopy(redirect); bad["cases"].pop(); bad["assertions"] = 0
    reject("missing-http-companion", lambda: strict_report(bad, "smart-http.py", mode="discovery"))
    bad = copy.deepcopy(redirect); bad["assertions"] = 2
    reject("http-assertion-counter", lambda: strict_report(bad, "smart-http.py", mode="discovery"))
    bad = copy.deepcopy(redirect); bad["cases"][-1]["actual"] = packed((0, b"", b""))
    reject("mixed-http-assertion-command", lambda: strict_report(bad, "smart-http.py", mode="discovery"))
    redacted = policy_report("URL secrets redacted", (0, b"", b""), (0, b"", b"To https://localhost:12345/repo.git\n * [new branch]      HEAD -> diagnostic\n"), HTTP_POLICY_ASSERTIONS["URL secrets redacted"])
    redacted["cases"][1]["command"] = ["ngit", "push", "https://alice:password@localhost:12345/repo.git", "HEAD:refs/heads/diagnostic"]
    strict_report(redacted, "smart-http.py", mode="auth")
    bad = copy.deepcopy(redacted); bad["cases"][1]["actual"]["stderr_b64"] = base64.b64encode(b"To https://alice:password@localhost:12345/repo.git\n * [new branch]      HEAD -> diagnostic\n").decode()
    reject("http-secret-reflection", lambda: strict_report(bad, "smart-http.py", mode="auth"))
    expected = RECORD["original_test_hashes"]
    bad = dict(expected); bad.pop(next(iter(bad)))
    reject("manifest-member", lambda: verify_originals(ROOT, bad))
    bad = dict(expected); bad[next(iter(bad))] = "0" * 64
    reject("manifest-hash", lambda: verify_originals(ROOT, bad))
    deadline = V.DEADLINE
    try:
        V.DEADLINE = time.monotonic() - 1
        reject("expired-deadline", lambda: V.remaining_timeout(1))
    finally: V.DEADLINE = deadline
    area = Path(tempfile.mkdtemp(prefix="private-", dir=OUT))
    reject("startup-false-red", lambda: runtime_probe(["false"], env))
    reject("runtime-signal", lambda: N.reject_runtime_failure((-11, b"", b"")))
    report = area / "report.json"; report.write_text(json.dumps(good))
    reject("zero-matches", lambda: N.validate_control(0, report, b"", area, False, "absent"))
    report.write_text(json.dumps(dict(good, comparisons=0, cases=[])))
    reject("fake-empty-green", lambda: N.validate_control(0, report, b"", area, False, "one"))
    captures = area / "captures"; captures.mkdir()
    trace = area / "transport.trace"; trace.write_text("")
    reject("trace-zero", lambda: validate_trace(area, captures))
    trace.write_text("+ cmp -s\n")
    reject("trace-malformed", lambda: validate_trace(area, captures))
    trace.write_text("+ cmp -s left right\n")
    record = {"operands": ["left", "right"], "left": "eAo=", "right": "eQo="}
    (captures / "one.json").write_text(json.dumps(record))
    reject("trace-byte-mismatch", lambda: validate_trace(area, captures))
    record["right"] = record["left"]; (captures / "one.json").write_text(json.dumps(record))
    require(validate_trace(area, captures) == 1, "Trace positive failed")
    repo = area / "repo"
    git("init", "-q", "--template=", "-b", "main", str(repo), env=env)
    (repo / "tracked").write_text("tracked\n")
    git("add", ".", cwd=repo, env=env); git("commit", "-qm", "fixture", cwd=repo, env=env)
    before = N.semantic_state(repo, env)
    for name, change, undo in (
        ("detached-same-oid", lambda: git("checkout", "-q", "--detach", "HEAD", cwd=repo, env=env), lambda: git("checkout", "-q", "main", cwd=repo, env=env)),
        ("fetch-head", lambda: (repo / ".git/FETCH_HEAD").write_text("corruption\n"), lambda: (repo / ".git/FETCH_HEAD").unlink()),
        ("merge-head", lambda: (repo / ".git/MERGE_HEAD").write_text(git("rev-parse", "HEAD", cwd=repo, env=env).decode()), lambda: (repo / ".git/MERGE_HEAD").unlink()),
        ("config", lambda: git("config", "core.filemode", "false", cwd=repo, env=env), lambda: git("config", "core.filemode", "true", cwd=repo, env=env)),
        ("index-flag", lambda: git("update-index", "--assume-unchanged", "tracked", cwd=repo, env=env), lambda: git("update-index", "--no-assume-unchanged", "tracked", cwd=repo, env=env)),
        ("operation-directory", lambda: (repo / ".git/rebase-merge").mkdir(), lambda: (repo / ".git/rebase-merge").rmdir())):
        baseline = N.semantic_state(repo, env)
        change(); changed = N.semantic_state(repo, env)
        reject("state-" + name, lambda: N.require_state(baseline, changed))
        undo()
    before = N.semantic_state(repo, env)
    git("repack", "-n", "-d", cwd=repo, env=env); git("pack-refs", "--all", cwd=repo, env=env)
    config = repo / ".git/config"; config.write_text("# formatting only\n" + config.read_text())
    N.require_state(before, N.semantic_state(repo, env))
    RECORD["checks"].append({"name": "physical-repack-packedrefs-config-format", "passed": True})


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("controls", "regression", "e2e"), nargs="?", default="controls")
    parser.add_argument("--private-controls", action="store_true")
    args = parser.parse_args()
    soft, hard = resource.getrlimit(resource.RLIMIT_AS)
    resource.setrlimit(resource.RLIMIT_AS, (min(soft, 8 * 1024**3) if soft >= 0 else 8 * 1024**3, hard))
    OUT.mkdir(parents=True, exist_ok=True)
    mode = "private-controls" if args.private_controls else args.mode
    RECORD.update(mode=mode, argv=sys.argv, source_root=str(ROOT))
    start, status = time.monotonic(), 1
    V.DEADLINE = start + 540
    expected = None
    try:
        RECORD["source_before"] = N.fingerprint()
        env = environment()
        commit, expected = provenance(env)
        if args.private_controls: private_controls(env)
        else: {"controls": lambda: controls(env, commit), "regression": lambda: regression(env), "e2e": lambda: e2e(env)}[mode]()
        status = 0
    except Exception as error:
        RECORD["error"] = f"{type(error).__name__}: {error}"
    try:
        RECORD["source_after"] = N.fingerprint()
        RECORD["source_final"] = N.fingerprint()
        if expected: verify_originals(ROOT, expected)
        require(RECORD["source_before"] == RECORD["source_after"] == RECORD["source_final"], "Source fingerprint changed")
        V.remaining_timeout(1)
    except Exception as error:
        RECORD["final_source_error"] = str(error); status = 1
    RECORD.update(exit=status, wall_seconds=time.monotonic() - start)
    (OUT / (mode + ".json")).write_text(json.dumps(RECORD, indent=2) + "\n")
    print(f"{mode}: {'PASS' if not status else 'FAIL'}; record={OUT / (mode + '.json')}")
    return status


if __name__ == "__main__":
    sys.exit(main())
