#!/usr/bin/env python3
"""Integration checks for remote-URL includes, attribute pathspecs and
subdirectory commands (ledger tools/ai/git-include-pathspec-progress.org).

controls     fixed cases differ from Git on the baseline and match Git now;
             test sources present at the baseline are unchanged
differences  every suite report is fresh, has exact comparisons, and each
             non-exact case is a declared intended difference with a reason
regression   affected existing suites pass
e2e          a combined workflow on a clone of this repository matches Git
             with byte code and with sources
"""
from concurrent.futures import ThreadPoolExecutor
import json
import os
from pathlib import Path
import resource
import signal
import shutil
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
BASE = "273be609eb2080cf3f811bc116970d7969fcd466"
TASK = Path(os.environ.get("NGIT_INCLUDE_PATHSPEC_DIR", ROOT / "target/include-pathspec")).resolve()
NGIT = str(ROOT / "bin/ngit")
DEADLINE = time.monotonic() + 560
# Comparison counts at integration; a suite may grow but never shrink.
FLOORS = {("include-hasconfig", "match"): 81, ("include-hasconfig", "boundaries"): 48,
          ("include-hasconfig", "e2e"): 13, ("pathspec-attr", "select"): 163,
          ("pathspec-attr", "boundaries"): 58, ("pathspec-attr", "e2e"): 10,
          ("subdir-commands", "inspect"): 216, ("subdir-commands", "mutate"): 72,
          ("subdir-commands", "boundaries"): 78, ("subdir-commands", "e2e"): 10,
          ("audit-config", "core"): 138, ("audit-config", "e2e"): 13,
          ("audit-paths", "core"): 41, ("audit-paths", "e2e"): 10,
          ("audit-fixes", "core"): 222, ("audit-fixes", "e2e"): 8,
          ("audit-fixes", "prefix"): 224, ("audit-fixes", "prefix-boundary"): 44,
          ("audit-fixes", "prefix-untracked"): 120, ("audit-fixes", "prefix-rm"): 80,
          ("audit-fixes", "prefix-e2e"): 6,
          ("audit-reaudit", "core"): 138, ("audit-reaudit", "special"): 30,
          ("audit-reaudit", "boundaries"): 16, ("audit-reaudit", "e2e"): 8,
          ("audit-reaudit", "pull"): 92, ("audit-reaudit", "attrs"): 16,
          ("audit-reaudit", "binary"): 142, ("audit-reaudit", "objects"): 37,
          ("audit-reaudit", "round4-paths"): 70, ("audit-reaudit", "round4-pull"): 185,
          ("audit-reaudit", "round4-unified"): 91,
          ("audit-reaudit", "round5-autostash"): 52, ("audit-reaudit", "round5-headers"): 192,
          ("round6", "headers"): 126, ("round6", "pull"): 54,
          ("round6", "mail_starts"): 46}
SUITES = {"include-hasconfig": ("match", "boundaries", "e2e"),
          "pathspec-attr": ("select", "boundaries", "e2e"),
          "subdir-commands": ("inspect", "mutate", "boundaries", "e2e")}
REGRESSION = [["sh", "test/compare.sh", s] for s in
              ("status", "status-long", "diff", "add", "add-all", "rm", "mv", "restore",
               "misc", "grep", "log-path", "stash-push", "reset-index", "checkout")] + \
             [["sh", "test/config-includes.sh", m] for m in ("read", "conditional", "boundaries", "e2e")] + \
             [["sh", "test/%s.sh" % s, m] for s in ("pathspec-magic", "subdir-suite")
              for m in ("read", "write", "boundaries", "e2e")] + \
             [["sh", "test/compare.sh", s] for s in ("attr-config", "hooks", "commit", "patch",
                                                       "log-walk", "clone", "fetch", "ssh",
                                                       "describe", "rev")]


def fail(message):
    print("FAIL: " + message)
    sys.exit(1)


def cap():
    resource.setrlimit(resource.RLIMIT_AS, (8 << 30, 8 << 30))


def run(argv, cwd, env, timeout=120):
    timeout = min(timeout, DEADLINE - time.monotonic())
    if timeout <= 0:
        fail("overall deadline exceeded")
    # Descendants may start their own sessions, so each run is marked in the
    # environment and every marked process is killed on timeout.
    token = "%d-%d" % (os.getpid(), time.monotonic_ns())
    p = subprocess.Popen(argv, cwd=cwd, env=dict(env, NGIT_VERIFY_RUN=token),
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                         preexec_fn=cap, start_new_session=True)
    try:
        out, err = p.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        kill_marked(token)
        p.communicate()
        return (-1, b"", b"timeout")
    return (p.returncode, out, err)


def kill_marked(token):
    """SIGKILL every process whose environment carries TOKEN, until none is left."""
    marker = ("NGIT_VERIFY_RUN=" + token).encode()
    for _ in range(20):
        found = False
        for entry in Path("/proc").iterdir():
            if not entry.name.isdigit():
                continue
            try:
                if marker in (entry / "environ").read_bytes().split(b"\0"):
                    os.kill(int(entry.name), signal.SIGKILL)
                    found = True
            except (OSError, ValueError):
                pass
        if not found:
            return
        time.sleep(0.1)


def environment(home):
    env = {k: v for k, v in os.environ.items()
           if not k.startswith(("GIT_", "NGIT_", "XDG_CONFIG"))}
    env.update(HOME=str(home), LC_ALL="C", TZ="UTC", GIT_CONFIG_NOSYSTEM="1",
               GIT_AUTHOR_DATE="1700000000 +0000", GIT_COMMITTER_DATE="1700000000 +0000",
               GIT_EDITOR="true", GIT_PAGER="cat", PAGER="cat")
    return env


def suite_environment(**extra):
    """The caller's environment without Git or ngit overrides, so a suite
    cannot be redirected to another command (NGIT_COMPAT_COMMAND,
    NGIT_SUBDIR_COMMAND, ...), filtered, or pointed at another repository.
    EDITOR=true gives both implementations the same noninteractive editor
    without putting GIT_EDITOR into the hook environments the suites record."""
    env = {k: v for k, v in os.environ.items()
           if not k.startswith(("GIT_", "NGIT_")) and k != "VISUAL"}
    env.update(EDITOR="true", **extra)
    return env


def ident(env):
    return dict(env, GIT_AUTHOR_NAME="A", GIT_AUTHOR_EMAIL="a@example.com",
                GIT_COMMITTER_NAME="A", GIT_COMMITTER_EMAIL="a@example.com")


def git(args, cwd, env):
    rc, out, err = run(["git", *args], cwd, env)
    if rc:
        fail("fixture git %s: %s" % (args, err.decode(errors="replace")))
    return out


def write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)


# Fixtures: each builds DIR/home and DIR/r with Git only.

def fx_hasconfig(d, defines_remote=False):
    home = d / "home"
    home.mkdir()
    env = environment(home)
    git(["init", "-q", "r"], d, env)
    git(["remote", "add", "origin", "https://example.com/acme/tool.git"], d / "r", env)
    # Written last: Git refuses every command once an include defines a remote.
    write(home / ".gitconfig", '[includeIf "hasconfig:remote.*.url:https://example.com/acme/**"]\n'
                               "\tpath = acme.inc\n")
    write(home / "acme.inc", "[user]\n\tname = Acme Inc\n"
          + ('[remote "x"]\n\turl = https://example.com/other.git\n' if defines_remote else ""))


def fx_attr(d):
    env = ident(environment(d / "home"))
    (d / "home").mkdir()
    git(["init", "-q", "r"], d, env)
    r = d / "r"
    write(r / ".gitattributes", "*.c text\n*.h -text\n")
    for name, text in (("a.c", "1\n"), ("b.h", "2\n"), ("c.txt", "3\n")):
        write(r / name, text)
    git(["add", "."], r, env)
    git(["commit", "-qm", "i"], r, env)
    for name in ("a.c", "b.h", "c.txt"):
        with open(r / name, "a") as f:
            f.write("x\n")


def fx_subdir(d):
    env = ident(environment(d / "home"))
    (d / "home").mkdir()
    git(["init", "-q", "r"], d, env)
    r = d / "r"
    for name, text in (("top", "a\n"), ("d/f", "b\n"), ("d/e/g", "c\n"), ("d/h", "x\n")):
        write(r / name, text)
    git(["add", "."], r, env)
    git(["commit", "-qm", "i"], r, env)
    for name, text in (("d/f", "b\nm\n"), ("top", "a\nn\n"), ("d/new", "u\n")):
        write(r / name, text)


CONTROLS = [
    ("hasconfig include applied", fx_hasconfig, ".", ["config", "user.name"]),
    ("hasconfig include defining a remote URL", lambda d: fx_hasconfig(d, True), ".", ["config", "user.name"]),
    ("attr set", fx_attr, ".", ["ls-files", ":(attr:text)"]),
    ("attr unset", fx_attr, ".", ["ls-files", ":(attr:-text)"]),
    ("attr unspecified", fx_attr, ".", ["ls-files", ":(attr:!text)"]),
    ("attr status", fx_attr, ".", ["status", "--short", ":(attr:text)"]),
    ("attr diff", fx_attr, ".", ["diff", "--name-only", ":(attr:-text)"]),
    ("attr rejected by ls-tree", fx_attr, ".", ["ls-tree", "-r", "HEAD", "--name-only", ":(attr:text)"]),
    ("subdir ls-files", fx_subdir, "d", ["ls-files"]),
    ("subdir ls-files -m", fx_subdir, "d", ["ls-files", "-m"]),
    ("subdir ls-files -o", fx_subdir, "d", ["ls-files", "-o"]),
    ("subdir ls-files ../top", fx_subdir, "d", ["ls-files", "../top"]),
    ("subdir diff --relative", fx_subdir, "d", ["diff", "--relative"]),
    ("subdir diff --relative --stat", fx_subdir, "d", ["diff", "--relative", "--stat"]),
    ("subdir diff --name-only --relative", fx_subdir, "d", ["diff", "--name-only", "--relative"]),
    ("subdir diff --stat ../top", fx_subdir, "d", ["diff", "--stat", "--", "../top"]),
    ("subdir clean -n", fx_subdir, "d", ["clean", "-n"]),
    ("subdir clean -n ../", fx_subdir, "d", ["clean", "-n", "../"]),
]


def observe(command, fixture, where, argv, scratch):
    """Run ARGV with COMMAND in a fresh copy of FIXTURE; return output and state."""
    d = Path(tempfile.mkdtemp(dir=scratch))
    fixture(d)
    env = environment(d / "home")
    result = run([*command, *argv], d / "r" / where, env)
    state = run(["git", "status", "--porcelain", "--ignored"], d / "r", env)
    shutil.rmtree(d)
    return result, state


def baseline():
    tree = TASK / "baseline"
    if not (tree / "bin/ngit").exists():
        TASK.mkdir(parents=True, exist_ok=True)
        if run(["git", "worktree", "add", "--detach", str(tree), BASE], ROOT, suite_environment())[0]:
            fail("cannot create baseline worktree")
    head = run(["git", "rev-parse", "HEAD"], tree, suite_environment())[1].decode().strip()
    if head != BASE:
        fail("baseline worktree is at %s, not %s" % (head, BASE))
    return str(tree / "bin/ngit")


def controls():
    names = run(["git", "ls-tree", "-r", "--name-only", BASE, "test"], ROOT, suite_environment())[1].decode().split()
    # The guard covers this objective's commit 6a462c1; later objectives may
    # change shared suites such as test/compare.sh.
    changed = run(["git", "diff", "--name-only", BASE, "6a462c1", "--", *names], ROOT, suite_environment())[1].decode().split()
    if not names or changed:
        fail("original test sources changed: %s" % changed)
    old = baseline()
    scratch = Path(tempfile.mkdtemp())
    bad = []
    try:
        for name, fixture, where, argv in CONTROLS:
            ref = observe(["git"], fixture, where, argv, scratch)
            before = observe([old], fixture, where, argv, scratch)
            now = observe([NGIT], fixture, where, argv, scratch)
            red, green = before != ref, now == ref
            print("%-5s %-5s %s" % ("red" if red else "SAME", "green" if green else "DIFF", name))
            if not (red and green):
                bad.append(name)
    finally:
        shutil.rmtree(scratch, ignore_errors=True)
    print("controls: %d, original test sources: %d unchanged" % (len(CONTROLS), len(names)))
    if bad:
        fail("controls not red on baseline and green now: %s" % bad)


# Suite modes checked by each `differences GROUP` run; each group fits the
# meter's 600 s limit.
GROUPS = {"slice": [(s, m) for s, modes in SUITES.items() for m in modes],
          "audit-config-paths": [("audit-config", "core"), ("audit-config", "e2e"),
                                 ("audit-paths", "core"), ("audit-paths", "e2e")],
          "audit-fixes": [("audit-fixes", m) for m in ("core", "e2e", "prefix-boundary",
                                                        "prefix-e2e")],
          "audit-prefix": [("audit-fixes", m) for m in ("prefix", "prefix-untracked",
                                                         "prefix-rm")],
          "audit-reaudit": [("audit-reaudit", m) for m in ("core", "special", "boundaries",
                                                           "e2e")],
          "audit-final": [("audit-reaudit", m) for m in ("pull", "attrs", "binary", "objects")],
          "audit-round5": [("audit-reaudit", m) for m in ("round4-paths", "round4-pull",
                                                          "round4-unified")],
          "audit-round6": [("audit-reaudit", m) for m in ("round5-autostash", "round5-headers")],
          "audit-round7": [("round6", m) for m in ("headers", "pull", "mail_starts")]}


def differences(group="slice"):
    """Run every suite mode afresh with the default command and no case
    filter, then check its report; a stored or partial report never counts."""
    reports = Path(tempfile.mkdtemp())
    env = suite_environment(NGIT_REPORT_DIR=str(reports))
    if group not in GROUPS:
        fail("unknown group " + group)
    jobs = GROUPS[group]

    def job(item):
        suite, mode = item
        return item, run(["sh", "test/%s.sh" % suite, mode], ROOT, env, timeout=540)
    try:
        with ThreadPoolExecutor(max_workers=5) as pool:
            results = dict(pool.map(job, jobs))
        total_exact = 0
        for (suite, mode), (rc, out, err) in sorted(results.items()):
            name = "%s %s" % (suite, mode)
            path = reports / ("%s-%s.json" % (suite, mode))
            if rc or not path.exists():
                fail("%s: rc=%d %s" % (name, rc, (out + err)[-300:].decode(errors="replace")))
            r = json.loads(path.read_text())
            intended = r.get("intended", {})
            if r.get("failures") or r.get("unexpected_errors"):
                fail("%s has failures" % name)
            if r.get("comparisons", 0) < FLOORS[(suite, mode)]:
                fail("%s: %s comparisons, below the floor %d"
                     % (name, r.get("comparisons"), FLOORS[(suite, mode)]))
            if r.get("exact", 0) <= 0 or r["exact"] + len(intended) != r["comparisons"]:
                fail("%s: exact %s + intended %d != comparisons %s"
                     % (name, r.get("exact"), len(intended), r.get("comparisons")))
            if not all(isinstance(v, str) and v.strip() for v in intended.values()):
                fail("%s: intended difference without a reason" % name)
            total_exact += r["exact"]
            print("%-30s exact %4d  intended %d" % (name, r["exact"], len(intended)))
            for case, reason in sorted(intended.items()):
                print("    intended: %s -- %s" % (case, reason))
        print("exact comparisons: %d" % total_exact)
    finally:
        shutil.rmtree(reports, ignore_errors=True)


def regression():
    fixtures = Path(tempfile.mkdtemp())

    def job(argv):
        # Suites build shared fixture names, so each job gets its own directory.
        env = suite_environment(NGIT_FIXTURE_DIR=tempfile.mkdtemp(dir=fixtures))
        start = time.monotonic()
        rc, out, err = run(argv, ROOT, env, timeout=540)
        return argv, rc, time.monotonic() - start, (out + err)[-400:]
    try:
        with ThreadPoolExecutor(max_workers=6) as pool:
            results = list(pool.map(job, REGRESSION))
    finally:
        shutil.rmtree(fixtures, ignore_errors=True)
    bad = [r for r in results if r[1]]
    for argv, rc, seconds, _ in results:
        print("rc=%d %6.1fs %s" % (rc, seconds, " ".join(argv[1:])))
    for argv, rc, _, tail in bad:
        print("--- " + " ".join(argv) + "\n" + tail.decode(errors="replace"))
    if bad:
        fail("%d regression suites failed" % len(bad))


E2E_STEPS = [
    (".", ["config", "user.name"]),
    ("lisp", ["ls-files", "-m"]),
    ("lisp", ["diff", "--relative", "--stat"]),
    ("lisp", ["clean", "-n"]),
    ("lisp", ["clean", "-f"]),
    ("lisp", ["add", ":(attr:text)"]),
    ("lisp", ["status", "--short"]),
    ("lisp", ["commit", "-q", "-m", "e2e"]),
    ("lisp", ["log", "-1", "--format=%an %ae %s"]),
    (".", ["ls-files", ":(attr:-text)"]),
    (".", ["status", "--short"]),
]


def e2e_fixture(d):
    home = d / "home"
    write(home / ".gitconfig", '[includeIf "hasconfig:remote.*.url:https://example.com/acme/**"]\n'
                               "\tpath = acme.inc\n")
    write(home / "acme.inc", "[user]\n\tname = Acme Inc\n\temail = inc@example.com\n")
    env = environment(home)
    git(["clone", "-q", "--no-local", str(ROOT), "r"], d, env)
    r = d / "r"
    git(["remote", "set-url", "origin", "https://example.com/acme/nelisp-git.git"], r, env)
    write(r / ".git/info/attributes", "*.el text\n*.org -text\n")
    with open(r / "lisp/ngit-rev.el", "a") as f:
        f.write(";; e2e\n")
    with open(r / "README.org", "a") as f:
        f.write("e2e\n")
    write(r / "lisp/scratch.tmp", "scratch\n")


def e2e():
    # Byte-code mode is real only while the stamp covers every source; the
    # environment drops NGIT_NO_ELC, so only the source mode sets it.
    if run(["sh", "tools/build-elc.sh"], ROOT, suite_environment(), timeout=300)[0]:
        fail("byte compilation failed")
    stamp = ROOT / "lisp/.elc-stamp"
    if not stamp.exists() or any(p.stat().st_mtime > stamp.stat().st_mtime
                                 for p in (ROOT / "lisp").glob("*.el")):
        fail("byte code is older than the sources")
    # The loader falls back to a source file whose .elc is missing; the
    # sources build-elc.sh keeps as source are the only allowed gaps.
    kept = {"ngit-main.el", "ngit-profile.el", "ngit-native.el", "ngit-transport-kernel-source.el"}
    missing = [p.name for p in (ROOT / "lisp").glob("*.el")
               if p.name not in kept and not p.with_suffix(".elc").exists()]
    if missing:
        fail("byte code missing for %s" % missing)
    scratch = Path(tempfile.mkdtemp())
    traces = {}
    try:
        for label, command, extra in (("git", ["git"], {}), ("byte code", [NGIT], {}),
                                      ("source", [NGIT], {"NGIT_NO_ELC": "1"})):
            d = scratch / label.replace(" ", "-")
            d.mkdir()
            e2e_fixture(d)
            env = dict(environment(d / "home"), **extra)
            trace = [run([*command, *argv], d / "r" / where, env, timeout=180)
                     for where, argv in E2E_STEPS]
            trace.append(tuple(run(["git", *a], d / "r", environment(d / "home"))
                               for a in (["rev-parse", "HEAD"], ["ls-files", "-s"],
                                         ["status", "--porcelain", "--ignored"])))
            traces[label] = trace
    finally:
        shutil.rmtree(scratch, ignore_errors=True)
    bad = False
    for label in ("byte code", "source"):
        for i, (want, got) in enumerate(zip(traces["git"], traces[label])):
            name = " ".join(E2E_STEPS[i][1]) if i < len(E2E_STEPS) else "final state"
            if want != got:
                bad = True
                print("DIFF %s: %s\n  git:  %r\n  ngit: %r" % (label, name, want, got))
    print("e2e steps: %d per mode, modes: byte code, source" % (len(E2E_STEPS) + 1))
    if bad:
        fail("e2e differs from Git")


if __name__ == "__main__":
    mode = sys.argv[1] if len(sys.argv) > 1 else ""
    {"controls": controls, "differences": lambda: differences(*sys.argv[2:3]),
     "regression": regression,
     "e2e": e2e}.get(mode, lambda: fail("usage: include-pathspec-verify.py "
                                       "controls|differences [GROUP]|regression|e2e"))()
    print("PASS " + mode)
