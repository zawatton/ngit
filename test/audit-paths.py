#!/usr/bin/env python3
"""Exact command and state parity for mixed pathspecs and index attributes."""
import base64
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
COMMAND = shlex.split(os.environ.get("NGIT_COMPAT_COMMAND", str(ROOT / "bin/ngit")))
MATCH = os.environ.get("NGIT_COMPAT_MATCH", "")
START = time.monotonic()
DEADLINE = START + 530
REPORT = dict(comparisons=0, exact=0, intended={}, failures=0, unexpected_errors=0,
              command=COMMAND, match=MATCH, cases=[])
BAD = re.compile(rb"void-function|void-variable|Symbol.s (?:function definition|value).*void|wrong-type-argument|Lisp error|[Ll]oader error|Cannot open load file|[Oo]ut of memory|resource exhausted")

def run(argv, cwd, env, input_data=None):
    budget = min(35, DEADLINE - time.monotonic())
    if budget <= 0:
        raise RuntimeError("overall deadline exceeded")
    p = subprocess.Popen(argv, cwd=cwd, env=env, stdout=subprocess.PIPE,
                         stdin=subprocess.PIPE if input_data is not None else None,
                         stderr=subprocess.PIPE, start_new_session=True)
    try:
        stdout, stderr = p.communicate(input=input_data, timeout=budget)
    except subprocess.TimeoutExpired:
        os.killpg(p.pid, signal.SIGKILL)
        p.communicate()
        raise RuntimeError(f"command timeout: {argv}")
    return dict(exit=p.returncode, stdout_b64=base64.b64encode(stdout).decode(),
                stderr_b64=base64.b64encode(stderr).decode())


def setup(args, repo, env):
    result = run(["git", *args], repo, env)
    if result["exit"]:
        raise RuntimeError(f"fixture failed: {args}: {result}")


def state(repo, env):
    """Logical refs/index plus exact worktree and relevant configuration bytes."""
    commands = [["show-ref", "--head"], ["ls-files", "--stage"],
                ["log", "--all", "--format=%H %P %an <%ae> %cn <%ce> %s"],
                ["reflog", "show", "--all", "--format=%H %gs"],
                ["config", "--list", "--local"]]
    files = {}
    for path in sorted(repo.rglob("*")):
        rel = path.relative_to(repo)
        if path.is_symlink():
            files[str(rel)] = ["symlink", os.readlink(path)]
        elif path.is_dir() and rel.parts[0] != ".git":
            files[str(rel)] = ["directory", path.stat().st_mode & 0o777]
        elif path.is_file() and (rel.parts[0] != ".git" or
                                str(rel) in (".git/config", ".git/HEAD", ".git/info/attributes")):
            files[str(rel)] = [path.stat().st_mode & 0o777, base64.b64encode(path.read_bytes()).decode()]
    for path in sorted(Path(env["HOME"]).rglob("*")):
        if path.is_file():
            files["HOME/" + str(path.relative_to(env["HOME"]))] = base64.b64encode(path.read_bytes()).decode()
    return [run(["git", *a], repo, env) for a in commands], files


def compare(name, args, repo, env, sub="", mutate=False, keep=False):
    if MATCH and MATCH not in name:
        return
    snapshot = repo.parent / "snapshot"
    if mutate:
        shutil.copytree(repo, snapshot, symlinks=True)
    expected = run(["git", *args], repo / sub, env)
    expected_state = state(repo, env) if mutate else None
    if mutate:
        shutil.rmtree(repo)
        shutil.copytree(snapshot, repo, symlinks=True)
    actual = run([*COMMAND, *args], repo / sub, env)
    actual_state = state(repo, env) if mutate else None
    unexpected = actual["exit"] < 0 or bool(BAD.search(
        base64.b64decode(actual["stdout_b64"]) + base64.b64decode(actual["stderr_b64"])))
    equal = expected == actual and expected_state == actual_state
    differences = None
    if mutate and expected_state != actual_state:
        differences = dict(commands=[dict(index=i, expected=a, actual=b)
                                     for i, (a, b) in enumerate(zip(expected_state[0], actual_state[0])) if a != b],
                           files={k: dict(expected=expected_state[1].get(k), actual=actual_state[1].get(k))
                                  for k in expected_state[1].keys() | actual_state[1].keys()
                                  if expected_state[1].get(k) != actual_state[1].get(k)})
    REPORT["comparisons"] += 1
    # Every case uses the exact-comparison contract, including failed cases.
    REPORT["exact"] += 1
    REPORT["failures"] += int(not equal or unexpected)
    REPORT["unexpected_errors"] += int(unexpected)
    REPORT["cases"].append(dict(name=name, argv=args, subdirectory=sub, equal=equal, expected=expected,
                                actual=actual, state_equal=(expected_state == actual_state) if mutate else None,
                                state_differences=differences))
    if not equal or unexpected:
        def brief(result):
            return dict(exit=result["exit"], **{
                key: repr(base64.b64decode(result[key + "_b64"])[:160])
                for key in ("stdout", "stderr")})
        print(f"FAIL {name}: git={brief(expected)}, ngit={brief(actual)}, "
              f"state_equal={expected_state == actual_state}", file=sys.stderr)
    if mutate:
        if not keep:
            shutil.rmtree(repo)
            shutil.copytree(snapshot, repo, symlinks=True)
        shutil.rmtree(snapshot)


def fixture(base):
    repo, home = base / "repo", base / "home"
    repo.mkdir()
    home.mkdir()
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    env.update(HOME=str(home), XDG_CONFIG_HOME=str(home / "xdg"), LC_ALL="C", TZ="UTC",
               GIT_CONFIG_NOSYSTEM="1", GIT_AUTHOR_DATE="1700000000 +0000",
               GIT_COMMITTER_DATE="1700000000 +0000")
    for args in (["init", "-q", "-b", "main"], ["config", "user.name", "Fixture"],
                 ["config", "user.email", "fixture@example.test"]):
        setup(args, repo, env)
    (repo / "sub").mkdir()
    (repo / ".gitattributes").write_text("*.txt chosen kind=root\n*.dat -chosen\n")
    (repo / "sub/.gitattributes").write_text("a.txt -chosen kind=nested\nb.txt chosen kind=nested\n")
    for name in ("a.txt", "b.txt", "keep.dat", "sub/a.txt", "sub/b.txt"):
        (repo / name).write_text("base\n")
    setup(["add", "."], repo, env)
    setup(["commit", "-qm", "base"], repo, env)
    return repo, env


def change(repo):
    for name in ("a.txt", "b.txt", "keep.dat", "sub/a.txt", "sub/b.txt"):
        (repo / name).write_text("changed\n")


def early_exit_probe(repo, env):
    """Count real pattern matches while preserving Git's requested states."""
    name = "early-exit-pattern-count"
    if MATCH and MATCH not in name:
        return
    source_root = ROOT
    if COMMAND and Path(COMMAND[0]).name == "ngit":
        candidate = Path(COMMAND[0]).resolve().parents[1]
        if (candidate / "lisp/ngit-match.el").is_file():
            source_root = candidate
    harness = repo.parent / "attr-count.el"
    harness.write_text(
        ";;; -*- lexical-binding: t -*-\n"
        "(provide 'ngit-config)\n"
        f"(load {json.dumps(str(source_root / 'lisp/ngit-match.el'))} nil t)\n"
        "(defvar audit-count 0)\n"
        "(let ((matcher (symbol-function 'ngit-pattern-match-p)) (pats nil))\n"
        "  (dotimes (_ 1000) (push (vector \"file\" '(nodir literal) \"\" '((\"other\" . set))) pats))\n"
        "  (push (vector \"file\" '(nodir literal) \"\" '((\"wanted\" . set))) pats)\n"
        "  (fset 'ngit-pattern-match-p (lambda (&rest args) (setq audit-count (1+ audit-count)) (apply matcher args)))\n"
        "  (fset 'ngit-attr-stack (lambda (&rest _) (list pats)))\n"
        "  (dolist (st (ngit-attrs nil \"file\" '(\"wanted\")))\n"
        "    (princ (format \"file: %s: %s\\n\" (car st) (cdr st))))\n"
        "  (princ (format \"matches=%d\\n\" audit-count)))\n"
        "(kill-emacs 0)\n")
    args = ["check-attr", "wanted", "--", "file"]
    # This comparison uses Git's untouched rc/stdout/stderr tuple.
    compare(name, args, repo, env)
    oracle = run(["git", *args], repo, env)
    actual = run([os.environ.get("NGIT_NELISP", "nelisp"), "--load", str(harness)], repo, env)
    output = base64.b64decode(actual["stdout_b64"])
    states, separator, counter = output.rpartition(b"matches=")
    count = int(counter.strip()) if separator and counter.strip().isdigit() else None
    states_equal = states == base64.b64decode(oracle["stdout_b64"])
    unexpected = actual["exit"] != 0 or bool(BAD.search(
        output + base64.b64decode(actual["stderr_b64"])))
    passed = (oracle["exit"] == 0 and actual["exit"] == 0 and
              actual["stderr_b64"] == "" and states_equal and count == 1)
    # Instrumentation is an assertion, never an exact Git comparison.
    REPORT.setdefault("performance", {})[name] = dict(
        oracle=oracle, raw=actual, states_equal=states_equal,
        matches=count, required_matches=1, passed=passed)
    REPORT["failures"] += int(not passed or unexpected)
    REPORT["unexpected_errors"] += int(unexpected)
    if not passed or unexpected:
        print(f"FAIL instrumentation {name}: matches={count}, "
              f"states_equal={states_equal}, raw={actual}", file=sys.stderr)


def main(mode):
    if mode not in ("core", "e2e"):
        raise RuntimeError("expected core or e2e")
    with tempfile.TemporaryDirectory(prefix="ngit-audit-paths-") as tmp:
        repo, env = fixture(Path(tmp))
        def case(name, args, **kwargs):
            compare(name, args, repo, env, **kwargs)
        glob = ["*.txt", ":(exclude)a.txt"]
        if mode == "core":
            case("mixed-glob-rm", ["rm", "-f", "--", *glob], mutate=True)
            change(repo)
            case("mixed-glob-checkout", ["checkout", "--", *glob], mutate=True)
            setup(["add", "-u"], repo, env)
            case("mixed-glob-reset", ["reset", "-q", "HEAD", "--", *glob], mutate=True)
            case("mixed-glob-stash", ["stash", "push", "-q", "--", *glob], mutate=True)
            for missing in ("root", "nested", "both"):
                root = repo / ".gitattributes"
                nested = repo / "sub/.gitattributes"
                if missing in ("root", "both"):
                    root.unlink()
                if missing in ("nested", "both"):
                    nested.unlink()
                for suffix, specs in (("set", [":(attr:chosen)"]),
                                      ("unset", [":(attr:-chosen)"]),
                                      ("value", [":(attr:kind=nested)"]),
                                      ("excluded", ["*.txt", ":(exclude,attr:-chosen)"])):
                    case("index-fallback-" + missing + "-" + suffix,
                         ["ls-files", "--", *specs])
                case("index-fallback-" + missing + "-status", ["status", "--short", "--", ":(attr:chosen)"])
                root.write_text("*.txt chosen kind=root\n*.dat -chosen\n")
                nested.write_text("a.txt -chosen kind=nested\nb.txt chosen kind=nested\n")
            # Existing empty files override the index rather than falling back.
            (repo / ".gitattributes").write_text("")
            case("empty-worktree-overrides-index", ["ls-files", "--", ":(attr:chosen)"])
            (repo / ".gitattributes").write_text("*.txt -chosen\n")
            case("worktree-overrides-index", ["ls-files", "--", ":(attr:chosen)"])
            case("worktree-overrides-index-exclude", ["ls-files", "--", "*.txt", ":(exclude,attr:-chosen)"])
            # An actual directory at this location is an empty attribute
            # source; it does not count as a missing worktree file.
            directory_attr = repo / ".gitattributes"
            directory_attr.unlink()
            directory_attr.mkdir()
            case("directory-attributes-check-attr", ["check-attr", "chosen", "--", "a.txt"])
            case("directory-attributes-include", ["ls-files", "--", ":(attr:chosen)a.txt"])
            case("directory-attributes-exclude", ["ls-files", "--", "a.txt", ":(exclude,attr:chosen)a.txt"])
            directory_attr.rmdir()
            # Index fallback accepts blob contents regardless of entry mode.
            indexed = run(["git", "rev-parse", ":.gitattributes"], repo, env)
            if indexed["exit"]:
                raise RuntimeError("fixture indexed attributes failed")
            oid = base64.b64decode(indexed["stdout_b64"]).strip().decode()
            setup(["update-index", "--cacheinfo", "120000," + oid + ",.gitattributes"], repo, env)
            case("index-blob-symlink-check-attr", ["check-attr", "chosen", "--", "a.txt"])
            case("index-blob-symlink-include", ["ls-files", "--", ":(attr:chosen)a.txt"])
            case("index-blob-symlink-exclude", ["ls-files", "--", "a.txt", ":(exclude,attr:chosen)a.txt"])
            commit = run(["git", "rev-parse", "HEAD"], repo, env)
            commit_oid = base64.b64decode(commit["stdout_b64"]).strip().decode()
            setup(["update-index", "--cacheinfo", "160000," + commit_oid + ",.gitattributes"], repo, env)
            case("index-blob-nonblob-ignored", ["check-attr", "chosen", "--", "a.txt"])
            setup(["update-index", "--cacheinfo", "100644," + oid + ",.gitattributes"], repo, env)
            # read-cache.c reads stage 2 (ours) when no stage-0 file exists.
            attr = repo / ".gitattributes"
            versions = []
            for contents in ("*.txt -chosen kind=base\n", "*.txt chosen kind=ours\n",
                             "*.txt -chosen kind=theirs\n"):
                attr.write_text(contents)
                blob = run(["git", "hash-object", "-w", ".gitattributes"], repo, env)
                if blob["exit"]:
                    raise RuntimeError("fixture attribute blob failed")
                versions.append(base64.b64decode(blob["stdout_b64"]).strip().decode())
            attr.unlink()
            for label, stages in (("ours", (1, 2, 3)), ("without-ours", (1, 3))):
                setup(["update-index", "--force-remove", ".gitattributes"], repo, env)
                data = "".join(f"100644 {versions[stage - 1]} {stage}\t.gitattributes\n" for stage in stages)
                result = run(["git", "update-index", "--index-info"], repo, env, data.encode())
                if result["exit"]:
                    raise RuntimeError("fixture unmerged attributes failed")
                case("unmerged-" + label + "-check-attr", ["check-attr", "chosen", "kind", "--", "a.txt"], mutate=True)
                case("unmerged-" + label + "-include", ["ls-files", "--", ":(attr:chosen)a.txt"])
                case("unmerged-" + label + "-exclude", ["ls-files", "--", "a.txt", ":(exclude,attr:chosen)a.txt"])
            (repo / ".gitattributes").write_text("file other\n" * 1000 + "file wanted\n")
            (repo / "file").write_text("payload\n")
            case("early-exit-states", ["check-attr", "wanted", "--", "file"])
            early_exit_probe(repo, env)
            for label, attributes in (
                    ("expanded", "file binary\n"),
                    ("blocked", "file binary\nfile -binary\n"),
                    ("unspecified", "file binary\nfile !binary\n"),
                    ("direct", "file binary\nfile text\n")):
                (repo / ".gitattributes").write_text(attributes)
                case("macro-" + label, ["check-attr", "binary", "text", "diff", "merge", "--", "file"])
        else:
            change(repo)
            case("e2e-stage", ["add", "-u"], mutate=True, keep=True)
            case("e2e-reset-mixed", ["reset", "-q", "HEAD", "--", *glob], mutate=True, keep=True)
            case("e2e-staged", ["diff", "--cached", "--name-only"])
            (repo / ".gitattributes").unlink()
            (repo / "sub/.gitattributes").unlink()
            case("e2e-fallback-status", ["status", "--short", "--", ":(attr:chosen)"])
            case("e2e-fallback-checkout", ["checkout", "--", ":(attr:chosen)", ":(exclude)a.txt"], mutate=True, keep=True)
            case("e2e-stash-mixed", ["stash", "push", "-q", "--", *glob], mutate=True, keep=True)
            case("e2e-stash-list", ["stash", "list"])
            case("e2e-rm-mixed", ["rm", "-f", "--", *glob], mutate=True, keep=True)
            case("e2e-final-status", ["status", "--porcelain"])
            case("e2e-final-index", ["ls-files", "--stage"], mutate=True, keep=True)
    if not REPORT["comparisons"]:
        raise RuntimeError("zero selected comparisons")


mode = sys.argv[1] if len(sys.argv) > 1 else "core"
try:
    main(mode)
except Exception as error:
    REPORT["unexpected_errors"] += 1
    print(f"UNEXPECTED: {error}", file=sys.stderr)
finally:
    REPORT["seconds"] = round(time.monotonic() - START, 3)
    directory = Path(os.environ.get("NGIT_REPORT_DIR") or ROOT / "target/include-pathspec/reports")
    directory.mkdir(parents=True, exist_ok=True)
    (directory / f"audit-paths-{mode}.json").write_text(json.dumps(REPORT, indent=2) + "\n")
    if os.environ.get("NGIT_COMPAT_REPORT"):
        Path(os.environ["NGIT_COMPAT_REPORT"]).write_text(json.dumps(REPORT, indent=2) + "\n")
print(json.dumps({k: v for k, v in REPORT.items() if k not in ("cases", "source_policy")}))
sys.exit(bool(REPORT["failures"] or REPORT["unexpected_errors"]))
