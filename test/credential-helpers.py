"""Credential transport tuple comparisons and independent policy assertions."""
import base64
import importlib.util
import json
import os
from pathlib import Path
import re
import shlex
import signal
import shutil
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("credential_http_fixture", ROOT / "test/credential-http-fixture.py")
fixture = importlib.util.module_from_spec(spec)
spec.loader.exec_module(fixture)
CASES = []
ASSERTIONS = []
ENV = dict(os.environ, LC_ALL="C", TZ="UTC", GIT_CONFIG_NOSYSTEM="1",
           GIT_TERMINAL_PROMPT="0", GIT_ASKPASS="/bin/false",
           GIT_AUTHOR_NAME="Credential Fixture", GIT_AUTHOR_EMAIL="fixture@example.test",
           GIT_COMMITTER_NAME="Credential Fixture", GIT_COMMITTER_EMAIL="fixture@example.test",
           GIT_AUTHOR_DATE="1700600000 +0000", GIT_COMMITTER_DATE="1700600000 +0000")
COMMAND = shlex.split(os.environ.get("NGIT_COMPAT_COMMAND", str(ROOT / "bin/ngit")))
GROUPED_WORKFLOW = False


def run(argv, cwd=None, input_bytes=None):
    process = subprocess.Popen(argv, cwd=cwd, env=ENV, stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, stdin=subprocess.PIPE if input_bytes is not None else None,
                               start_new_session=True)
    try:
        out, err = process.communicate(input=input_bytes, timeout=45)
    except BaseException:
        os.killpg(process.pid, signal.SIGKILL)
        process.communicate(timeout=5)
        raise
    return process.returncode, out, err


def encoded(value):
    return dict(exit=value[0], stdout_b64=base64.b64encode(value[1]).decode(),
                stderr_b64=base64.b64encode(value[2]).decode())


def record_tuple(name, expected, actual):
    bad = runtime_error(actual)
    CASES.append(dict(name=name, expected=encoded(expected), actual=encoded(actual),
                      passed=not bad and actual == expected, runtime_error=bad))
    print(("PASS " if CASES[-1]["passed"] else "FAIL ") + name, flush=True)


def runtime_error(value):
    return value[0] < 0 or value[0] in (126, 127, 134, 139) or bool(re.search(
        rb"void-function|void-variable|wrong-type-argument|Cannot open load file|"
        rb"uncaught.*error|memory limit|panicked at|backtrace \(innermost", value[2]))


def compare(name, argv, reset=None):
    match = os.environ.get("NGIT_COMPAT_MATCH", "")
    selected = not match or match in name
    if not selected and not GROUPED_WORKFLOW:
        return
    expected = run(["git"] + argv)
    if reset:
        reset()
    actual = run(COMMAND + argv)
    if selected:
        record_tuple(name, expected, actual)
    elif runtime_error(actual) or expected != actual:
        raise RuntimeError("Grouped workflow prerequisite failed: " + name)


def credential_unit(name, root, url):
    """Compare the real private credential API with native Git's fill output."""
    if os.environ.get("NGIT_COMPAT_MATCH") and os.environ["NGIT_COMPAT_MATCH"] not in name:
        return
    module_root = Path(ENV.get("NGIT_CREDENTIAL_MODULE_ROOT", ROOT / "lisp"))
    module = module_root / ("ngit-credential.el" if ENV.get("NGIT_NO_ELC") else "ngit-credential.elc")
    script = root / "credential-unit.el"
    script.write_text("(setq load-path (cons " + json.dumps(str(ROOT / "lisp")) + " load-path))\n" +
        "(load " + json.dumps(str(module)) + " nil t)\n" +
        "(condition-case nil (let ((context (ngit-credential-create nil " + json.dumps(url) + "))) " +
        "(ngit-credential-fill context) " +
        "(dolist (key '(\"protocol\" \"host\" \"path\" \"username\" \"password\" \"password_expiry_utc\")) " +
        "(let ((entry (assoc key (aref context 1)))) (when entry (princ (concat key \"=\" (cdr entry) \"\\n\")))))) " +
        "(ngit-credential-error (ngit-native-write-stderr \"fatal: credential helper failed safely\\n\") (kill-emacs 128)))\n" +
        "(kill-emacs 0)\n")
    expected = run(["git", "credential", "fill"], input_bytes=("url=" + url + "\n\n").encode())
    actual = run([ENV.get("NGIT_NELISP", "nelisp"), "--load", str(script)])
    record_tuple(name, expected, actual)


def assertion(name, value):
    ASSERTIONS.append(dict(name=name, passed=bool(value)))
    print(("PASS " if value else "FAIL ") + name, flush=True)


def policy(name, argv, predicate):
    value = run(COMMAND + argv)
    assertion(name, not runtime_error(value) and predicate(value))
    ASSERTIONS[-1]["actual"] = encoded(value)
    ASSERTIONS[-1]["runtime_error"] = runtime_error(value)


def private_controls(root):
    """Exercise the real child deadline and the notifier's error boundary."""
    marker = root / "expired-child"
    log = root / "lifecycle-children"
    script = root / "private-controls.el"
    module = ROOT / "lisp/ngit-credential.el"
    if not ENV.get("NGIT_NO_ELC"):
        module = module.with_suffix(".elc")
    script.write_text("(setq load-path (cons " + json.dumps(str(ROOT / "lisp")) + " load-path))\n" +
        "(load " + json.dumps(str(module)) + " nil t)\n" +
        "(let ((failed nil)) (condition-case nil (ngit-credential--exchange " +
        json.dumps("touch " + shlex.quote(str(marker))) + " \"\" 0) (ngit-credential-error (setq failed t))) " +
        "(unless (and failed (not (file-exists-p " + json.dumps(str(marker)) + "))) (error \"expired child forked\")))\n" +
        "(let ((original (symbol-function 'ngit-credential--exchange)) (propagated nil)) " +
        "(unwind-protect (progn (fset 'ngit-credential--exchange (lambda (&rest ignored) (signal 'void-function '(credential-fixture-fault)))) " +
        "(condition-case nil (ngit-credential--notify (vector nil nil '(\"!ignored\") nil nil nil) \"store\") " +
        "(void-function (setq propagated t)))) (fset 'ngit-credential--exchange original)) " +
        "(unless propagated (error \"runtime fault swallowed\")))\n" +
        "(ngit-credential--notify (vector nil '((\"protocol\" . \"https\") (\"host\" . \"localhost\")) " +
        "(make-list 1000 " + json.dumps("!f() { printf 'child\\n' >> " + shlex.quote(str(log)) + "; sleep 20; }; f") + ") nil nil nil) \"store\")\n" +
        "(princ \"private controls PASS\\n\")\n(kill-emacs 0)\n")
    started = time.monotonic()
    value = run([ENV.get("NGIT_NELISP", "nelisp"), "--load", str(script)])
    assertion("private expired fork runtime and lifecycle control", value == (0, b"private controls PASS\n", b""))
    ASSERTIONS[-1]["actual"] = encoded(value)
    ASSERTIONS[-1]["runtime_error"] = runtime_error(value)
    assertion("lifecycle has at most three children", log.exists() and len(log.read_text().splitlines()) <= 3)
    assertion("lifecycle wall bounded", time.monotonic() - started < 36)


def validator_controls():
    global COMMAND
    if os.environ.get("NGIT_COMPAT_MATCH"):
        return
    original = COMMAND
    try:
        for name, script, runtime in [
            ("runtime negative", "printf 'void-function: fixture\\n' >&2; exit 1", True),
            ("tuple negative", "printf 'wrong bytes\\n'", False),
        ]:
            COMMAND = ["/bin/sh", "-c", script, "fixture"]
            compare(name, ["--version"])
            observed = CASES.pop()
            assertion(name + " validator", not observed["passed"] and observed["runtime_error"] == runtime)
            ASSERTIONS[-1]["negative_control"] = observed
    finally:
        COMMAND = original


def git(*args, cwd=None):
    value = run(["git", *map(str, args)], cwd)
    if value[0]:
        raise RuntimeError(repr(value))
    return value[1]


def logged_helper(root, label, output):
    """Create an owned fake helper and retain complete operation contexts."""
    script, log = root / (label + "-helper"), root / (label + ".jsonl")
    script.write_text("#!/usr/bin/env python3\nimport json,sys\n" +
        "fields=dict(line.rstrip('\\n').split('=',1) for line in sys.stdin if '=' in line)\n" +
        "with open(" + repr(str(log)) + ",'a') as stream: stream.write(json.dumps({'operation':sys.argv[-1],'fields':fields})+'\\n')\n" +
        "if sys.argv[-1]=='get': sys.stdout.write(" + repr(output) + ")\n")
    script.chmod(0o755)
    return script, log


def helper_records(log):
    return [json.loads(line) for line in log.read_text().splitlines()] if log.exists() else []


def trace_assertion(name, value, servers=(), logs=()):
    assertion(name, value)
    ASSERTIONS[-1]["requests"] = [[{"method": r[0], "path": r[1], "headers": r[2]} for r in server.requests] for server in servers]
    ASSERTIONS[-1]["helper_records"] = [helper_records(log) for log in logs]


def security_outcome(name, argv, reset, expected_stdout):
    expected = run(["git"] + argv)
    reset()
    actual = run(COMMAND + argv)
    assertion(name, not runtime_error(actual) and actual == (0, expected_stdout, b""))
    ASSERTIONS[-1].update(expected=encoded(expected), actual=encoded(actual), runtime_error=runtime_error(actual),
        policy="A changed origin discards all custom headers and credentials and challenges the target's independently selected helper context")


def security_workflows(root, config):
    """Observe real redirect destinations, header isolation and helper contexts."""
    saved_ca = ENV["GIT_SSL_CAINFO"]
    output = "username=alice\npassword=password\n"
    suffix = "/info/refs?service=git-upload-pack"
    try:
        with fixture.CredentialHTTPServer(root) as origin, fixture.CredentialHTTPServer(root, credentials="bob:target") as target:
            bundle = root / "redirect-ca.pem"
            bundle.write_bytes(Path(origin.ca_file).read_bytes() + Path(target.ca_file).read_bytes())
            ENV["GIT_SSL_CAINFO"] = str(bundle)
            source_helper, source_log = logged_helper(root, "cross-source", output)
            target_helper, target_log = logged_helper(root, "cross-target", "username=bob\npassword=target\n")
            source_url, target_url = origin.url("repo.git"), target.url("repo.git")
            config.write_text('[protocol]\nversion = 0\n[credential]\nuseHttpPath = true\n' +
                '[credential "' + source_url + '"]\nhelper = ' + json.dumps(str(source_helper)) + '\n' +
                '[credential "' + target_url + '"]\nhelper = ' + json.dumps(str(target_helper)) + '\n' +
                '[http]\nextraHeader = X-Origin-Secret: fake-origin\nextraHeader = Cookie: fake-cookie\nextraHeader = X-Other: fake-other\n')
            origin.routes["/repo.git" + suffix] = (302, {"Location": target_url + suffix}, b"")
            def reset_cross():
                origin.requests.clear(); target.requests.clear()
                source_log.unlink(missing_ok=True); target_log.unlink(missing_ok=True)
            security_outcome("cross origin credentials all headers isolated", ["ls-remote", source_url.replace("://", "://alice:password@")],
                             reset_cross, git("ls-remote", str(root / "repo.git")))
            target_records = helper_records(target_log)
            basic = "Basic " + base64.b64encode(b"bob:target").decode()
            trace_assertion("cross origin all custom headers absent", len(origin.requests) == 1 and
                all(origin.requests[0][2].get(key) == value for key, value in
                    {"X-Origin-Secret": "fake-origin", "Cookie": "fake-cookie", "X-Other": "fake-other"}.items()) and
                origin.requests[0][2].get("Authorization") == "Basic " + base64.b64encode(b"alice:password").decode() and
                len(target.requests) == 2 and
                all(not any(k.lower() in ("x-origin-secret", "cookie", "x-other") for k in r[2]) for r in target.requests),
                (origin, target), (source_log, target_log))
            trace_assertion("cross origin challenge and own helper context", len(origin.requests) == 1 and
                len(target.requests) == 2 and "Authorization" not in target.requests[0][2] and
                target.requests[1][2].get("Authorization") == basic and not helper_records(source_log) and
                [r["operation"] for r in target_records] == ["get", "store"] and
                target_records[0]["fields"] == {"protocol": "https", "host": "localhost:" + str(target.server.server_port), "path": "repo.git"} and
                target_records[1]["fields"].get("username") == "bob" and target_records[1]["fields"].get("password") == "target",
                (origin, target), (source_log, target_log))
        for alias in ("from.git", "to.git"):
            shutil.copytree(root / "repo.git", root / alias)
        with fixture.CredentialHTTPServer(root) as server:
            ENV["GIT_SSL_CAINFO"] = server.ca_file
            source_helper, source_log = logged_helper(root, "path-source", output)
            target_helper, target_log = logged_helper(root, "path-target", output)
            source_url, target_url = server.url("from.git"), server.url("to.git")
            config.write_text('[protocol]\nversion = 0\n[credential]\nuseHttpPath = true\n' +
                '[credential "' + source_url + '"]\nhelper = ' + json.dumps(str(source_helper)) + '\n' +
                '[credential "' + target_url + '"]\nhelper = ' + json.dumps(str(target_helper)) + '\n')
            server.routes["/from.git" + suffix] = (302, {"Location": target_url + suffix}, b"")
            def reset_path():
                server.requests.clear(); source_log.unlink(missing_ok=True); target_log.unlink(missing_ok=True)
            redirect_args = ["ls-remote", "-q", source_url.replace("://", "://alice:password@")]
            expected = run(["git"] + redirect_args)
            reset_path()
            actual = run(COMMAND + redirect_args)
            refs = git("ls-remote", str(root / "repo.git"))
            assertion("same origin redirected repository inherited diagnostic", not runtime_error(actual) and
                expected == (0, refs, ("warning: redirecting to " + target_url + "/\n").encode()) and
                actual == (0, refs, b""))
            ASSERTIONS[-1].update(expected=encoded(expected), actual=encoded(actual), runtime_error=runtime_error(actual),
                limitation="Native Git emits a redirect warning; preserved original ngit redirect tests require empty stderr")
            records = helper_records(target_log)
            trace_assertion("redirected path challenged and stored target context", len(server.requests) == 3 and
                server.requests[0][1].startswith("/from.git/") and "Authorization" in server.requests[0][2] and
                server.requests[1][1].startswith("/to.git/") and "Authorization" not in server.requests[1][2] and
                server.requests[2][1].startswith("/to.git/") and "Authorization" in server.requests[2][2] and
                not helper_records(source_log) and [r["operation"] for r in records] == ["get", "store"] and
                all(r["fields"].get("path") == "to.git" for r in records) and "username" not in records[0]["fields"],
                (server,), (source_log, target_log))
        with fixture.CredentialHTTPServer(root) as server:
            ENV["GIT_SSL_CAINFO"] = server.ca_file
            helper, log = logged_helper(root, "custom-authorization", "username=unused\npassword=unused\n")
            basic = "Basic " + base64.b64encode(b"alice:password").decode()
            config.write_text('[protocol]\nversion = 0\n[credential]\nhelper = ' + json.dumps(str(helper)) +
                              '\n[http]\nextraHeader = Authorization: ' + basic + '\n')
            url = server.url("repo.git").replace("://", "://unused:unused@")
            def reset_custom():
                server.requests.clear(); log.unlink(missing_ok=True)
            compare("custom Authorization suppresses helper Basic", ["ls-remote", url], reset=reset_custom)
            trace_assertion("custom Authorization only and no helper operations", len(server.requests) == 1 and
                server.requests[0][2].get("Authorization") == basic and not helper_records(log), (server,), (log,))
            reset_custom()
            server.routes["/repo.git" + suffix] = (401, {}, b"")
            policy("custom Authorization 401 terminal", ["ls-remote", url], lambda r: r[0] == 128)
            trace_assertion("custom Authorization never erase unused credential", len(server.requests) == 1 and
                server.requests[0][2].get("Authorization") == basic and not helper_records(log), (server,), (log,))
    finally:
        ENV["GIT_SSL_CAINFO"] = saved_ca


def malformed_workflows(root, server, url, configure):
    """Git echoes malformed lines; compare outcomes separately from safe diagnostics."""
    for name, first, second, success in [
        ("malformed after partial", "username=alice\nMALFORMED_SENTINEL\n", "password=password\n", True),
        ("malformed after complete", "username=alice\npassword=password\nMALFORMED_SENTINEL\n", "username=wrong\npassword=wrong\n", True),
        ("incomplete quit", "username=alice\nquit=true\n", "password=password\n", False),
    ]:
        helper1, log1 = logged_helper(root, name.replace(" ", "-") + "-one", first)
        helper2, log2 = logged_helper(root, name.replace(" ", "-") + "-two", second)
        configure([str(helper1), str(helper2)])
        expected = run(["git", "ls-remote", url])
        log1.unlink(missing_ok=True); log2.unlink(missing_ok=True); server.requests.clear()
        actual = run(COMMAND + ["ls-remote", url])
        assertion(name + " native outcome safe diagnostics", not runtime_error(actual) and
            (actual[0] == expected[0] == 0 and actual[1] == expected[1] if success else actual[0] != 0 and expected[0] != 0) and
            b"MALFORMED_SENTINEL" not in actual[2])
        ASSERTIONS[-1].update(expected=encoded(expected), actual=encoded(actual), runtime_error=runtime_error(actual),
                              policy="Malformed helper text is never reflected in diagnostics")
        operations = [[r["operation"] for r in helper_records(log)] for log in (log1, log2)]
        wanted = [["get", "store"], ["get", "store"]] if name == "malformed after partial" else (
            [["get", "store"], ["store"]] if success else [["get"], []])
        trace_assertion(name + " helper stop and lifecycle", operations == wanted and
            len(server.requests) == (2 if success else 1) and
            (not success or server.requests[-1][2].get("Authorization") == "Basic " + base64.b64encode(b"alice:password").decode()),
            (server,), (log1, log2))


def main(mode):
    global GROUPED_WORKFLOW
    GROUPED_WORKFLOW = mode in ("auth", "e2e")
    with tempfile.TemporaryDirectory(prefix="ngit-credentials-") as tmp:
        root = Path(tmp)
        config = root / "global.config"
        ENV["GIT_CONFIG_GLOBAL"] = str(config)
        config.write_text("[protocol]\nversion = 0\n")
        source = root / "source"
        git("init", "-q", "-b", "main", source)
        (source / "binary").write_bytes(bytes(range(256)) * 16)
        git("add", ".", cwd=source)
        git("commit", "-qm", "initial", cwd=source)
        git("clone", "-q", "--bare", source, root / "repo.git")
        git("--git-dir=" + str(root / "repo.git"), "config", "http.receivepack", "true")
        with fixture.CredentialHTTPServer(root) as server:
            ENV["GIT_SSL_CAINFO"] = server.ca_file
            url = server.url("repo.git")
            helper = "!f() { test \"$1\" != get || printf 'username=alice\\npassword=password\\n'; }; f"
            git("config", "--global", "credential.helper", helper)
            compare("helper TLS challenge semantic old-red", ["ls-remote", url])
            if CASES and not CASES[-1]["passed"]:
                print("Request trace: " + repr([(r[0], r[1], bool(r[2].get("Authorization"))) for r in server.requests]), flush=True)
            if os.environ.get("NGIT_COMPAT_MATCH") and os.environ["NGIT_COMPAT_MATCH"] in "helper TLS challenge semantic old-red":
                return
            validator_controls()
            def configure(helpers, extra=""):
                config.write_text("[protocol]\nversion = 0\n[credential]\n" +
                                  "".join("helper = " + json.dumps(h) + "\n" for h in helpers) + extra)
            complete = "!f() { test \"$1\" != get || printf 'username=alice\\npassword=password\\n'; }; f"
            if mode == "helpers":
                future = "!f() { test \"$1\" != get || printf 'password=password\\npassword_expiry_utc=4102444800\\n'; }; f"
                username = "!f() { test \"$1\" != get || printf 'username=alice\\n'; }; f"
                configure([future, username])
                credential_unit("future expiry partial chain semantic old-red", root, "https://example.invalid/repo.git")
                configure([], '[credential "example.invalid"]\nhelper = ' + json.dumps(complete) + '\n')
                credential_unit("partial config URL semantic old-red", root, "https://example.invalid/repo.git")
                configure([complete], "useHttpPath = true\n")
                credential_unit("trailing credential path semantic old-red", root, "https://example.invalid/repo.git/")
                for name, helpers in [
                    ("partial chain", ["!f() { test \"$1\" != get || printf 'username=alice\\n'; }; f", "!f() { test \"$1\" != get || printf 'password=password\\n'; }; f"]),
                    ("empty reset", ["!exit 1", "", complete]),
                    ("nonzero complete", ["!f() { test \"$1\" != get || { printf 'username=alice\\npassword=password\\n'; return 7; }; }; f"]),
                    ("duplicate last wins", ["!f() { test \"$1\" != get || printf 'username=wrong\\nusername=alice\\npassword=password\\n'; }; f"]),
                    ("unknown output attribute", ["!f() { test \"$1\" != get || printf 'ignored=secret\\nusername=alice\\npassword=password\\n'; }; f"]),
                    ("EOF without blank", [complete]),
                    ("early stop complete quit", ["!f() { test \"$1\" != get || printf 'username=alice\\npassword=password\\nquit=true\\n'; }; f", "!exit 1"]),
                ]:
                    configure(helpers)
                    compare(name, ["ls-remote", url])
                executable = root / "path with spaces" / "git-credential-fixture"
                executable.parent.mkdir()
                executable.write_text("#!/bin/sh\nwhile [ $# -gt 1 ]; do shift; done\ncat >/dev/null\n[ \"$1\" != get ] || printf 'username=alice\\npassword=password\\n'\n")
                executable.chmod(0o755)
                configure([str(executable).replace(" ", "\\ ") + " argument"])
                compare("absolute executable spaces and arguments", ["ls-remote", url])
                ENV["PATH"] = str(executable.parent) + os.pathsep + ENV["PATH"]
                configure(["fixture argument"])
                compare("named PATH helper arguments", ["ls-remote", url])
                stock_exec = Path(git("--exec-path").decode().strip())
                ENV["GIT_EXEC_PATH"] = str(executable.parent)
                for transport in ("git-remote-http", "git-remote-https"):
                    (executable.parent / transport).symlink_to(stock_exec / transport)
                configure(["fixture argument"])
                compare("explicit exec path helper", ["ls-remote", url])
                ENV.pop("GIT_EXEC_PATH")
                malformed_workflows(root, server, url, configure)
            elif mode == "auth":
                configure([complete])
                for name, target in [
                    ("explicit pair preemptive", url.replace("://", "://alice:password@")),
                    ("explicit username completed", url.replace("://", "://alice@")),
                    ("percent encoded username", url.replace("://", "://%61lice:password@")),
                    ("percent encoded password", url.replace("://", "://alice:pass%77ord@")),
                    ("helper authenticated symref", url),
                ]:
                    compare(name, ["ls-remote", "--symref", target])
                configure([complete], "username = alice\n")
                compare("configured username", ["ls-remote", url])
                configure([complete], "useHttpPath = true\n")
                compare("credential HTTP path", ["ls-remote", url])
                security_workflows(root, config)
            elif mode == "boundaries":
                for name, extra in [
                    ("matching URL helper", '[credential "' + url + '"]\nhelper = ' + json.dumps(complete) + '\n'),
                    ("matching component prefix", '[credential "' + server.url("") + '"]\nhelper = ' + json.dumps(complete) + '\n'),
                    ("path boundary nonmatch", '[credential "' + server.url("repo") + '"]\nhelper = "!exit 1"\n'),
                    ("source order reset", '[credential "' + url + '"]\nhelper =\nhelper = ' + json.dumps(complete) + '\n'),
                    ("unrelated host ignored", '[credential "https://elsewhere.invalid"]\nhelper = "!exit 1"\n'),
                    ("explicit port preserved", '[credential "' + url + '"]\nuseHttpPath = true\n'),
                    ("case preserved path", '[credential "' + server.url("REPO.git") + '"]\nhelper = "!exit 1"\n'),
                ]:
                    configure([complete], extra)
                    compare(name, ["ls-remote", url])
                for name, bad in [
                    ("helper hangs bounded", "!sleep 20"),
                    ("inherited stdout descendant bounded", "!sleep 20 &"),
                    ("helper flooding bounded", "!yes x"),
                    ("line budget rejection", "!head -c 65536 /dev/zero | tr '\\000' x"),
                    ("origin mutation rejection", "!printf 'host=elsewhere.invalid\\nusername=alice\\npassword=password\\n'"),
                ]:
                    configure([bad])
                    started = time.monotonic()
                    policy(name, ["ls-remote", url], lambda r: r[0] != 0 and b"elsewhere.invalid" not in r[2])
                    assertion(name + " wall deadline", time.monotonic() - started < 16)
                private_controls(root)
            if mode == "e2e":
                clone = root / "clone"
                states = []
                def clone_state():
                    return {"refs": git("for-each-ref", "--format=%(refname) %(objectname) %(symref)", cwd=clone).decode(),
                            "index": git("ls-files", "--stage", "-v", cwd=clone).decode(),
                            "worktree": (clone / "binary").read_bytes().hex(),
                            "metadata": {str(p.relative_to(clone / ".git")): p.read_bytes().hex()
                                         for p in (clone / ".git").rglob("*") if p.is_file() and
                                         (p.name in ("HEAD", "ORIG_HEAD", "FETCH_HEAD", "MERGE_HEAD", "CHERRY_PICK_HEAD")
                                          or "logs" in p.relative_to(clone / ".git").parts)}}
                def remove_native_clone():
                    states.append(clone_state())
                    shutil.rmtree(clone)
                compare("authenticated nonempty TLS clone", ["clone", "-q", url, str(clone)], reset=remove_native_clone)
                if clone.exists():
                    assertion("clone logical refs index worktree metadata reflogs", states[0] == clone_state())
                    compare("authenticated unchanged fetch", ["-C", str(clone), "fetch", "-q"])
                    compare("authenticated binary show", ["-C", str(clone), "show", "HEAD:binary"])
                    compare("authenticated complete fsck", ["-C", str(clone), "fsck", "--full"])
                    compare("authenticated status state", ["-C", str(clone), "status", "--porcelain=v1"])
                    compare("authenticated refs state", ["-C", str(clone), "rev-parse", "HEAD", "refs/remotes/origin/main"])
                    compare("authenticated index state", ["-C", str(clone), "ls-files", "--stage"])
                    compare("authenticated HEAD state", ["-C", str(clone), "rev-parse", "HEAD"])
                    assertion("binary worktree faithful", (clone / "binary").read_bytes() == (source / "binary").read_bytes())
                    assertion("no outstanding operation", all(not (clone / ".git" / p).exists() for p in ("MERGE_HEAD", "CHERRY_PICK_HEAD", "rebase-merge", "rebase-apply")))
                    assertion("real nonempty authenticated POST", any(r[0] == "POST" and r[3] and r[2].get("Authorization") for r in server.requests))
                    (source / "next").write_bytes(b"new authenticated fetch\0\xff\n" * 100)
                    git("add", ".", cwd=source)
                    git("commit", "-qm", "second", cwd=source)
                    git("push", "-q", str(root / "repo.git"), "main", cwd=source)
                    saved = root / "saved-clone"
                    shutil.copytree(clone, saved)
                    def reset_fetch():
                        states.append(clone_state())
                        shutil.rmtree(clone)
                        shutil.copytree(saved, clone)
                    server.requests.clear()
                    compare("authenticated nonempty fetch", ["-C", str(clone), "fetch", "-q"], reset=reset_fetch)
                    assertion("fetch logical state native equality", states[-1] == clone_state())
                    assertion("fetch nonempty RPC observed", sum(r[0] == "POST" for r in server.requests) == 2)
                    compare("authenticated fetched binary", ["-C", str(clone), "show", "origin/main:next"])
                    server.requests.clear()
                    compare("authenticated nonempty push", ["-C", str(source), "push", "-q", url, "HEAD:refs/heads/credential-push"],
                            reset=lambda: git("--git-dir=" + str(root / "repo.git"), "update-ref", "-d", "refs/heads/credential-push"))
                    assertion("push RPC observed once per implementation", sum(r[0] == "POST" for r in server.requests) == 2)
                    assertion("push ref matches", git("--git-dir=" + str(root / "repo.git"), "rev-parse", "refs/heads/credential-push") == git("rev-parse", "HEAD", cwd=source))
                    assertion("remote full fsck after push", run(["git", "--git-dir=" + str(root / "repo.git"), "fsck", "--full"])[0] == 0)
            if mode in ("auth", "e2e"):
                log = root / "helper-log"
                script = root / "helper"
                script.write_text("#!/bin/sh\nprintf '%s\\n' \"$1\" >> " + shlex.quote(str(log)) + "\ncat >/dev/null\n[ \"$1\" != get ] || printf 'username=alice\\npassword=password\\n'\n")
                script.chmod(0o755)
                configure([str(script)])
                server.requests.clear()
                compare("snapshot get and store lifecycle", ["ls-remote", url], reset=lambda: log.unlink(missing_ok=True))
                assertion("get store observed", log.read_text().splitlines() == ["get", "store"])
                log.unlink()
                preemptive = url.replace("://", "://alice:password@")
                compare("explicit credentials store without get", ["ls-remote", preemptive], reset=lambda: log.unlink(missing_ok=True))
                assertion("preemptive store only", log.read_text().splitlines() == ["store"])
                suffix = "/info/refs?service=git-upload-pack"
                for status in (401, 403, 500):
                    server.routes["/repo.git" + suffix] = (status, {}, b"")
                    log.unlink(missing_ok=True)
                    policy("HTTP " + str(status) + " reject policy", ["ls-remote", preemptive], lambda r: r[0] != 0)
                    assertion("erase iff credential 401 " + str(status), (log.read_text().splitlines() if log.exists() else []) == (["erase"] if status == 401 else []))
                del server.routes["/repo.git" + suffix]
                server.routes["/repo.git/git-upload-pack"] = (401, {}, b"")
                destination = root / "denied-clone"
                server.requests.clear()
                policy("POST failure no replay", ["clone", "-q", url, str(destination)], lambda r: r[0] != 0)
                assertion("POST observed exactly once", sum(r[0] == "POST" for r in server.requests) == 1)


if __name__ == "__main__":
    error = None
    try:
        mode = sys.argv[1]
        if mode not in ("helpers", "auth", "boundaries", "e2e"):
            raise ValueError("unknown mode")
        main(mode)
    except Exception as exc:
        error = repr(exc)
    report = dict(comparisons=len(CASES), failures=sum(not c["passed"] for c in CASES),
                  assertion_failures=sum(not a["passed"] for a in ASSERTIONS),
                  unexpected_errors=sum(c["runtime_error"] for c in CASES) + sum(a.get("runtime_error", False) for a in ASSERTIONS) + bool(error),
                  assertions=ASSERTIONS, cases=CASES, error=error)
    if os.environ.get("NGIT_COMPAT_REPORT"):
        Path(os.environ["NGIT_COMPAT_REPORT"]).write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({k: v for k, v in report.items() if k not in ("cases", "assertions")}))
    sys.exit(bool(error or not CASES or report["failures"] or report["unexpected_errors"] or any(not a["passed"] for a in ASSERTIONS)))
