"""Exact command evidence and state checks for native smart HTTP(S)."""
import base64
import importlib.util
import json
import os
import re
from pathlib import Path
import shlex
import shutil
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("smart_http_fixture", ROOT / "test/smart-http-fixture.py")
fixture = importlib.util.module_from_spec(spec)
spec.loader.exec_module(fixture)
SmartHTTPServer = fixture.SmartHTTPServer
CASES = []
ERRORS = 0
ENV = dict(os.environ, LC_ALL="C", TZ="UTC", GIT_CONFIG_NOSYSTEM="1",
           GIT_CONFIG_GLOBAL="/dev/null", GIT_TERMINAL_PROMPT="0",
           GIT_AUTHOR_NAME="Fixture", GIT_AUTHOR_EMAIL="fixture@example.test",
           GIT_COMMITTER_NAME="Fixture", GIT_COMMITTER_EMAIL="fixture@example.test",
           GIT_AUTHOR_DATE="1700600000 +0000", GIT_COMMITTER_DATE="1700600000 +0000")
NGIT = shlex.split(os.environ.get("NGIT_COMPAT_COMMAND", str(ROOT / "bin/ngit")))


def run(cmd, cwd=None):
    p = subprocess.run(cmd, cwd=cwd, env=ENV, stdout=subprocess.PIPE,
                       stderr=subprocess.PIPE, timeout=90)
    return p.returncode, p.stdout, p.stderr


def encoded(result):
    return {"exit": result[0], "stdout_b64": base64.b64encode(result[1]).decode(),
            "stderr_b64": base64.b64encode(result[2]).decode()}


def runtime_failed(result):
    "Keep runtime, signal and loader failures out of semantic predicates."
    global ERRORS
    failed = result[0] < 0 or result[0] in (126,127,134,139) or bool(re.search(
        rb"void-function|void-variable|wrong-type-argument|Cannot open load file|"
        rb"Symbol's function definition is void|memory limit|panicked at|"
        rb"^nelisp: (?:uncaught|fatal|internal) error|^backtrace \(innermost first\)|"
        rb"error while loading shared libraries|(?:nelisp|ngit).*?(?:not found|Permission denied)",
        result[2], re.M))
    ERRORS += bool(failed)
    return failed


def check(name, args, expected=None, cwd=None, predicate=None, _quiet=False):
    if os.environ.get("NGIT_COMPAT_MATCH") and os.environ["NGIT_COMPAT_MATCH"] != name:
        return (0,b"",b"")
    actual = run(NGIT + args, cwd)
    runtime_error = runtime_failed(actual)
    if expected is None:
        expected = run(["git"] + args, cwd)
    ok = not runtime_error and (predicate(actual) if predicate else actual == expected)
    CASES.append(dict(name=name, command=NGIT+args, expected=encoded(expected),
                      actual=encoded(actual), passed=bool(ok), runtime_error=bool(runtime_error)))
    if not _quiet:
        print(("PASS " if ok else "FAIL ") + name, flush=True)
    return actual


def runtime_guard_control():
    "Run an actual broken command through check, including its predicate gate."
    global NGIT, ERRORS
    name="runtime predicate rejects broken command"
    if os.environ.get("NGIT_COMPAT_MATCH") and os.environ["NGIT_COMPAT_MATCH"] != name:
        return
    original, errors = NGIT, ERRORS
    invoked=[]
    try:
        NGIT=["sh","-c","printf '%s\\n' 'void-function: audit-runtime-fault' >&2; exit 1"]
        check(name,[],expected=(1,b"",b""),predicate=lambda r:invoked.append(True) or True,_quiet=True)
        observed=CASES.pop()
        good=observed["runtime_error"] and not observed["passed"] and not invoked and ERRORS==errors+1
    finally:
        NGIT, ERRORS = original, errors
    invariant(name,good)
    CASES[-1]["negative_control"] = observed


def pair(name, args, reset, predicate=None):
    if os.environ.get("NGIT_COMPAT_MATCH") and os.environ["NGIT_COMPAT_MATCH"] != name:
        return None
    reset()
    expected = run(["git"] + args)
    reset()
    return check(name, args, expected=expected, predicate=predicate)


def invariant(name, value):
    if os.environ.get("NGIT_COMPAT_MATCH") and os.environ["NGIT_COMPAT_MATCH"] != name:
        return
    CASES.append(dict(name=name, assertion=True, passed=bool(value)))
    print(("PASS " if value else "FAIL ") + name, flush=True)


def git(*args, cwd=None):
    r = run(["git"] + list(args), cwd)
    if r[0]:
        raise RuntimeError(r)
    return r[1]


def main(mode):
    with tempfile.TemporaryDirectory(prefix="ngit-smart-http-") as tmp:
        root = Path(tmp)
        ENV["GIT_CONFIG_GLOBAL"] = str(root / "global.config")
        Path(ENV["GIT_CONFIG_GLOBAL"]).write_text("[protocol]\nversion = 0\n")
        source = root / "source"
        git("init", "-q", "-b", "main", str(source))
        (source / "binary").write_bytes(bytes(range(256)) * 128)
        (source / "日本語").write_text("日本語\n")
        git("add", ".", cwd=source)
        git("commit", "-qm", "initial", cwd=source)
        remote = root / "repo.git"
        git("clone", "-q", "--bare", str(source), str(remote))
        git("--git-dir="+str(remote), "config", "http.receivepack", "true")
        backup = root / "backup.git"
        shutil.copytree(remote, backup)
        def reset():
            shutil.rmtree(remote)
            shutil.copytree(backup, remote)
        with SmartHTTPServer(root) as server:
            git("config", "--global", "http.sslCAInfo", server.ca_file)
            cfg = []
            url = server.url("repo.git")
            if mode == "discovery":
                check("https refs", cfg+["ls-remote", "--symref", url])
                if os.environ.get("NGIT_COMPAT_MATCH") == "https refs":
                    return
                runtime_guard_control()
                preamble=b"# service=git-upload-pack"
                advertisement=git("upload-pack","--advertise-refs",str(remote))
                server.routes["/no-lf.git/info/refs?service=git-upload-pack"]=(200,
                    {"Content-Type":"application/x-git-upload-pack-advertisement"},
                    f"{len(preamble)+4:04x}".encode()+preamble+b"0000"+advertisement)
                check("service preamble optional LF",["ls-remote",server.url("no-lf.git")])
                git("config", "--global", "--unset", "http.sslCAInfo")
                check("untrusted CA", ["ls-remote", url], predicate=lambda r:r[0]!=0)
                git("config", "--global", "http.sslCAInfo", server.ca_file)
                check("wrong hostname", cfg+["ls-remote", url.replace("localhost", "127.0.0.1")], predicate=lambda r:r[0]!=0)
                with SmartHTTPServer(root, tls=False, chunked=True) as plain:
                    check("chunked HTTP refs", ["ls-remote", plain.url("repo.git")])
                    suffix="/info/refs?service=git-upload-pack"
                    plain.routes["/redirect.git"+suffix]=(302,{"Location":plain.url("repo.git")+suffix},b"")
                    check("discovery redirect", ["ls-remote",plain.url("redirect.git")],
                          predicate=lambda r:r[0]==0 and r[1]==git("ls-remote",url))
                    dest=root/"redirect-clone"
                    check("redirect updates RPC base",["clone","-q",plain.url("redirect.git"),str(dest)],expected=(0,b"",b""))
                    invariant("redirect RPC correct endpoint",any(r[0]=="POST" and r[1]=="/repo.git/git-upload-pack" for r in plain.requests)
                              and not any(r[0]=="POST" and r[1].startswith("/redirect.git/") for r in plain.requests))
                    plain.routes["/loop.git"+suffix]=(302,{"Location":"/loop.git"+suffix},b"")
                    check("redirect loop",["ls-remote",plain.url("loop.git")],predicate=lambda r:r[0]!=0)
                    for name,body,mime in [
                        ("dumb response",b"not smart\n","text/plain"),
                        ("truncated packet",b"001e# service=git-upload-pack\n00000028abc","application/x-git-upload-pack-advertisement"),
                        ("bad service",b"001f# service=git-receive-pack\n0000","application/x-git-upload-pack-advertisement"),
                        ("bad ref",b"001e# service=git-upload-pack\n00000010invalid-ref\n0000","application/x-git-upload-pack-advertisement")]:
                        plain.routes["/bad.git"+suffix]=(200,{"Content-Type":mime},body)
                        check(name,["ls-remote",plain.url("bad.git")],predicate=lambda r:r[0]!=0)
            elif mode in ("fetch", "e2e"):
                clone = root / "clone"
                check("https clone", cfg+["clone", "-q", url, str(clone)], expected=(0,b"",b""))
                if clone.exists() and (clone / ".git").exists():
                    posts=sum(r[0]=="POST" for r in server.requests)
                    check("unchanged fetch", cfg+["-C",str(clone),"fetch","-q"], expected=(0,b"",b""))
                    invariant("unchanged fetch has no POST", sum(r[0]=="POST" for r in server.requests)==posts)
                    check("cloned binary", ["-C",str(clone),"show","HEAD:binary"])
                    if run(["git", "-C",str(clone),"fsck","--full"])[0]:
                        raise RuntimeError("fsck")
                    assert (clone / "binary").read_bytes() == (source / "binary").read_bytes()
                    (source / "next").write_bytes(b"\0\xff\x80\n"*2048)
                    git("add",".",cwd=source)
                    git("commit","-qm","next",cwd=source)
                    git("push","-q",str(remote),"main",cwd=source)
                    check("fetch new binary pack",["-C",str(clone),"fetch","-q"],expected=(0,b"",b""))
                    check("fetched binary",["-C",str(clone),"show","origin/main:next"])
                    invariant("clone full fsck",run(["git","-C",str(clone),"fsck","--full"])[0]==0)
                    if mode == "fetch":
                        old=git("rev-parse","origin/main",cwd=clone)
                        (source/"third").write_text("third\n")
                        git("add",".",cwd=source)
                        git("commit","-qm","third",cwd=source)
                        git("push","-q",str(remote),"main",cwd=source)
                        server.routes["/repo.git/git-upload-pack"]=(200,{"Content-Type":"application/x-git-upload-pack-result"},b"0008NAK\n0009\x01PACK0000")
                        check("truncated pack rejected",["-C",str(clone),"fetch","-q"],predicate=lambda r:r[0]!=0)
                        invariant("corrupt fetch preserves refs",git("rev-parse","origin/main",cwd=clone)==old)
                        invariant("corrupt fetch full fsck",run(["git","-C",str(clone),"fsck","--full"])[0]==0)
                        invariant("corrupt fetch cleans incoming",not list((clone/".git/objects/pack").glob("incoming-*")))
                    if mode == "e2e":
                        check("e2e push",["-C",str(source),"push","-q",url,"HEAD:refs/heads/e2e"],expected=(0,b"",b""))
                        invariant("remote full fsck",run(["git","--git-dir="+str(remote),"fsck","--full"])[0]==0)
            elif mode == "push":
                args=["-C",str(source),"push","-q",url]
                pair("https push",args+["HEAD:refs/heads/new"],reset)
                invariant("push sends one complete RPC",len([r for r in server.requests if r[0]=="POST"])==2
                          and all(b"PACK" in r[3] for r in server.requests if r[0]=="POST"))
                oid=git("rev-parse","HEAD",cwd=source).strip().decode()
                (source/"change").write_text("new\n")
                git("add",".",cwd=source)
                git("commit","-qm","change",cwd=source)
                before=len([r for r in server.requests if r[0]=="POST"])
                pair("stale lease",args+["--force-with-lease=refs/heads/main:"+"1"*40,"HEAD:refs/heads/main"],reset)
                invariant("stale lease has no POST",len([r for r in server.requests if r[0]=="POST"])==before)
                pair("explicit lease",args+["--force-with-lease=refs/heads/main:"+oid,"HEAD:refs/heads/main"],reset)
                pair("accepted atomic batch",args+["--atomic","HEAD:refs/heads/a","HEAD:refs/heads/b"],reset)
                def deletion():
                    reset()
                    git("--git-dir="+str(remote),"update-ref","refs/heads/to-delete",oid)
                pair("HTTPS delete",args+["--delete","to-delete"],deletion)
                def raced():
                    reset()
                    def advance():
                        git("--git-dir="+str(remote),"fetch","-q",str(source),"main")
                        git("--git-dir="+str(remote),"update-ref","refs/heads/main",git("rev-parse","HEAD",cwd=source).strip().decode())
                    server.before_rpc=advance
                pair("advertised ref race",args+["--force-with-lease=refs/heads/main:"+oid,"HEAD:refs/heads/main"],raced)
                invariant("race push has no replay",server.before_rpc is None)
                def reject():
                    reset()
                    hook=remote/"hooks/update"
                    hook.write_text('#!/bin/sh\n[ "$1" != refs/heads/blocked ]\n')
                    hook.chmod(0o755)
                pair("rejected atomic batch",args+["--atomic","HEAD:refs/heads/blocked","HEAD:refs/heads/good"],reject)
                invariant("atomic rejection leaves no refs", run(["git","--git-dir="+str(remote),"show-ref","--verify","refs/heads/good"])[0]!=0)
                invariant("push full fsck",run(["git","--git-dir="+str(remote),"fsck","--full"])[0]==0)
            elif mode == "auth":
                with SmartHTTPServer(root, auth="alice:password", headers={"X-Fixture":"yes"}) as auth:
                    git("config", "--global", "http.sslCAInfo", auth.ca_file)
                    git("config", "--global", "http.extraHeader", "X-Fixture: yes")
                    args=[]
                    aurl=auth.url("repo.git").replace("://","://alice:password@")
                    expected = run(["git"]+args+["ls-remote",aurl])
                    auth.requests.clear()
                    check("Basic and extraHeader", args+["ls-remote",aurl],expected=expected)
                    metadata_clone=root/"metadata-clone"
                    git("clone","-q",aurl,str(metadata_clone))
                    def metadata():
                        return {str(p.relative_to(metadata_clone/".git")):p.read_bytes()
                                for p in (metadata_clone/".git/logs").rglob("*") if p.is_file()}
                    expected_logs=metadata()
                    git("fetch","-q",cwd=metadata_clone)
                    expected_fetch=(metadata_clone/".git/FETCH_HEAD").read_bytes()
                    shutil.rmtree(metadata_clone)
                    check("authenticated metadata clone",["clone","-q",aurl,str(metadata_clone)],expected=(0,b"",b""))
                    if metadata_clone.exists() and (metadata_clone/".git/logs").exists():
                        invariant("clone persistent URL anonymized",metadata()==expected_logs and all(b"password" not in v and b"alice" not in v for v in metadata().values()))
                        check("authenticated metadata fetch",["-C",str(metadata_clone),"fetch","-q"],expected=(0,b"",b""))
                        invariant("fetch persistent URL anonymized",(metadata_clone/".git/FETCH_HEAD").read_bytes()==expected_fetch and b"password" not in expected_fetch)
                        invariant("configured URL retains credentials",git("config","remote.origin.url",cwd=metadata_clone).strip().decode()==aurl)
                    auth.requests.clear()
                    check("authenticated push",args+["-C",str(source),"push","-q",aurl,"HEAD:refs/heads/auth"],expected=(0,b"",b""))
                    invariant("Basic headers on GET and POST", any(r[0]=="GET" for r in auth.requests) and any(r[0]=="POST" for r in auth.requests)
                              and all(r[2].get("Authorization")=="Basic YWxpY2U6cGFzc3dvcmQ=" and r[2].get("X-Fixture")=="yes" for r in auth.requests))
                    git("config","--global","--add","http.extraHeader","")
                    check("empty extraHeader resets",["ls-remote",aurl],predicate=lambda r:r[0]!=0)
                    git("config","--global","--add","http.extraHeader","X-Fixture: yes")
                    git("config","--global","--add","http.extraHeader","X-Order: first")
                    git("config","--global","--add","http.extraHeader","X-Order: second")
                    expected=run(["git","ls-remote",aurl])
                    auth.requests.clear()
                    check("repeated headers preserve order",["ls-remote",aurl],expected=expected)
                    invariant("ordered repeated headers",bool(auth.requests) and
                              [v for k,v in auth.requests[0][4] if k.lower()=="x-order"]==["first","second"])
                    with SmartHTTPServer(root,tls=False) as capture:
                        suffix="/info/refs?service=git-upload-pack"
                        auth.routes["/redirect.git"+suffix]=(302,{"Location":capture.url("repo.git")+suffix},b"")
                        expected=run(["git","ls-remote",capture.url("repo.git")])
                        capture.requests.clear()
                        check("cross origin redirect",["ls-remote",aurl.replace("repo.git","redirect.git")],expected=expected)
                        invariant("cross origin strips secrets",bool(capture.requests) and all(not any(k.lower() in ("authorization","x-fixture","x-order") for k in r[2]) for r in capture.requests))
                    check("URL secrets redacted",["-C",str(source),"push",aurl,"HEAD:refs/heads/diagnostic"],expected=(0,b"",b""),
                          predicate=lambda r:r[0]==0 and b"password" not in r[2] and b"alice" not in r[2])
                    auth.routes["/repo.git/git-receive-pack"]=(307,{"Location":auth.url("other.git/git-receive-pack")},b"")
                    before=sum(r[0]=="POST" for r in auth.requests)
                    check("RPC redirect refused without replay",["-C",str(source),"push","-q",aurl,"HEAD:refs/heads/no-replay"],expected=(1,b"",b""),predicate=lambda r:r[0]!=0 and b"password" not in r[2])
                    invariant("RPC redirect one POST",sum(r[0]=="POST" for r in auth.requests)==before+1)
                    git("config","--global","--add","http.extraHeader","X-Injection: yes\r\nOther: bad")
                    check("header injection refused",["ls-remote",aurl],expected=(1,b"",b""),predicate=lambda r:r[0]!=0 and b"password" not in r[2])
            else:
                raise ValueError(mode)


if __name__ == "__main__":
    unexpected = 0
    try:
        main(sys.argv[1] if len(sys.argv)>1 else "discovery")
    except Exception as err:
        unexpected = 1
        print(f"HARNESS ERROR {type(err).__name__}: {err}", file=sys.stderr)
    report = dict(comparisons=sum("actual" in c for c in CASES),
                  assertions=sum("assertion" in c for c in CASES), failures=sum(not c["passed"] for c in CASES),
                  unexpected_errors=unexpected+ERRORS, cases=CASES)
    if os.environ.get("NGIT_COMPAT_REPORT"):
        Path(os.environ["NGIT_COMPAT_REPORT"]).write_text(json.dumps(report, indent=2))
    sys.exit(bool(report["unexpected_errors"] or report["failures"] or not report["comparisons"]))
