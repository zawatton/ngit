#!/usr/bin/env python3
"""Git 2.47.3 attribute pathspec selection, diagnostics and workflow parity."""
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
REPORT = dict(comparisons=0, exact=0, intended={}, failures=0, unexpected_errors=0, cases=[])
BAD = re.compile(rb"void-function|void-variable|Symbol.s (?:function definition|value).*void|wrong-type-argument|Wrong type argument|Invalid function|Lisp error|[Ll]oader error|Cannot open load file|[Oo]ut of memory|resource exhausted")

# Derived from parse_pathspec's magic_mask, not from the command's name.
# Shared callers: revision.c (log/show/diff family); checkout.c serves
# both checkout and restore, commit.c serves status.  Commands outside
# ngit's implemented surface are listed separately and never compared.
SOURCE_POLICY = {
    "accept": {
        "ls-files": "builtin/ls-files.c: mask 0",
        "add": "builtin/add.c: mask 0, PATHSPEC_ATTR guard",
        "status": "builtin/commit.c: mask 0",
        "diff": "revision.c: mask 0; dir.c/tree-walk.c attr matching",
        "grep": "builtin/grep.c: mask 0",
        "log": "revision.c: mask 0",
        "rev-list": "revision.c: mask 0",
        "shortlog": "revision.c: mask 0",
        "format-patch": "builtin/log.c through revision.c: mask 0",
        "show": "revision.c: mask 0",
        "clean": "builtin/clean.c: mask 0",
        "rm": "builtin/rm.c: mask 0",
        "checkout": "builtin/checkout.c: mask 0",
        "restore": "builtin/checkout.c: mask 0",
        "reset": "builtin/reset.c: mask 0",
        "stash": "builtin/stash.c: mask 0",
    },
    "reject": {
        "ls-tree": "builtin/ls-tree.c: ALL_MAGIC & ~(FROMTOP | LITERAL)",
    },
}
# Keep a complete inventory tied to the actual CLI dispatch table.  A new
# command must receive an explicit classification rather than disappearing
# from the acceptance ledger.
NO_SELECTOR = {
    'blame': 'one literal filename, not a pathspec',
    'mv': 'literal source/destination names, not pathspecs',
    'check-attr': 'literal filenames, not pathspecs',
    'rev-parse': 'passes path arguments through, does not select files',
    'apply': '--include/--exclude are patch-name patterns, not pathspecs',
    'am': 'mailbox filenames, not pathspecs',
    **dict.fromkeys(('init', 'clone', 'fetch', 'pull', 'push', 'remote', 'ls-remote',
                     'test-cat-all', 'symbolic-ref', 'cat-file', 'describe', 'name-rev',
                     'worktree', 'bisect', 'config', 'merge-file', 'merge', 'cherry-pick',
                     'revert', 'rebase', 'pack-objects', 'index-pack', 'verify-pack',
                     'repack', 'gc', 'prune', 'pack-refs', 'count-objects', 'reflog',
                     'fsck', 'hash-object', 'write-tree', 'update-index', 'switch',
                     'branch', 'tag'), 'no file-selection pathspec in the implemented CLI'),
}
SOURCE_POLICY['literal_or_no_selector'] = NO_SELECTOR
SOURCE_POLICY['unimplemented_path_form'] = {
    'commit': 'Git accepts attr through builtin/commit.c; ngit implements whole-index commits only',
}
cli = (ROOT / 'lisp/ngit-cli.el').read_text().split('(defconst ngit--commands', 1)[1].split('(defun ', 1)[0]
IMPLEMENTED_COMMANDS = re.findall(r'\("([^"\n]+)" \.', cli)
classified = {command for group in SOURCE_POLICY.values() for command in group}
if classified != set(IMPLEMENTED_COMMANDS):
    raise RuntimeError(f'incomplete attr command inventory: missing={set(IMPLEMENTED_COMMANDS) - classified}, '
                       f'extra={classified - set(IMPLEMENTED_COMMANDS)}')
REPORT['implemented_commands'] = {command: next(group for group, entries in SOURCE_POLICY.items()
                                               if command in entries)
                                  for command in IMPLEMENTED_COMMANDS}
REPORT["source_policy"] = SOURCE_POLICY
UNSUPPORTED_COMMANDS = {
    "commit with paths": "ngit implements whole-index commits only",
    "archive": "ngit does not implement archive",
    "check-ignore": "ngit does not implement check-ignore",
    "update-index --again": "ngit implements --force-write-index only",
}
REPORT["unsupported"] = UNSUPPORTED_COMMANDS


def run(argv, cwd, env):
    budget = min(35, DEADLINE - time.monotonic())
    if budget <= 0:
        raise RuntimeError("overall deadline exceeded")
    p = subprocess.Popen(argv, cwd=cwd, env=env, stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE, start_new_session=True)
    try:
        stdout, stderr = p.communicate(timeout=budget)
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
    setup(["init", "-q", "-b", "main"], repo, env)
    setup(["config", "user.name", "Fixture"], repo, env)
    setup(["config", "user.email", "fixture@example.test"], repo, env)
    (repo / ".gitattributes").write_text(
        "*.c text kind=source review\n*.h -text kind=header\n*.txt !text kind=note\n"
        "*.bin binary\n*.empty marker=\n*.comma marker=a,b\n"
        "blocked.bin -binary\nCase.c !text\n")
    (repo / "sub").mkdir()
    (repo / "sub/.gitattributes").write_text("local.h text kind=source\n")
    for name in ("a.c", "b.h", "c.txt", "blob.bin", "blocked.bin", "Case.c", "v.empty", "v.comma",
                 "sub/a.c", "sub/b.h", "sub/c.txt", "sub/local.h"):
        (repo / name).write_text("needle base\n")
    setup(["add", "."], repo, env)
    setup(["commit", "-qm", "base"], repo, env)
    for path in repo.rglob("*"):
        if path.is_file() and ".git" not in path.parts and path.name != ".gitattributes":
            path.write_text("needle changed\n")
    for name in ("new.c", "new.h", "new.txt", "sub/new.c", "sub/new.h", "sub/new.txt"):
        (repo / name).write_text("needle new\n")
    return repo, env


SELECT_COMMANDS = {
    "ls-files": ["ls-files", "--"],
    "add": ["add", "--"],
    "status": ["status", "--short", "--"],
    "diff": ["diff", "--name-only", "--"],
    "grep": ["grep", "-l", "needle", "--"],
    "log": ["log", "--format=%s", "--name-only", "--"],
    "rev-list": ["rev-list", "HEAD", "--"],
    "shortlog": ["shortlog", "-s", "HEAD", "--"],
    "format-patch": ["format-patch", "-1", "--stdout", "HEAD", "--"],
    "show": ["show", "--format=", "--name-only", "HEAD", "--"],
    "clean": ["clean", "-n", "--"],
    "rm": ["rm", "-r", "-f", "--cached", "--"],
    "checkout": ["checkout", "--"],
    "restore": ["restore", "--"],
    "reset": ["reset", "-q", "HEAD", "--"],
    "stash": ["stash", "push", "-q", "--"],
}
MUTATING = {"add", "rm", "checkout", "restore", "reset", "stash"}


def main(mode):
    if mode not in ("select", "boundaries", "e2e"):
        raise RuntimeError("expected select, boundaries or e2e")
    print("Unsupported (not compared): " + "; ".join(UNSUPPORTED_COMMANDS))
    with tempfile.TemporaryDirectory(prefix="ngit-pathspec-attr-") as tmp:
        repo, env = fixture(Path(tmp))
        def case(name, args, **kwargs):
            compare(name, args, repo, env, **kwargs)
        if mode == "select":
            for command, argv in SELECT_COMMANDS.items():
                staged_snapshot = repo.parent / "staged-selection"
                if command == "reset":
                    shutil.copytree(repo, staged_snapshot, symlinks=True)
                    setup(["add", "-u"], repo, env)
                for label, spec in [("set", "text"), ("unset", "-text"),
                                    ("unspecified", "!text"), ("value", "kind=source")]:
                    for sub in ("", "sub"):
                        case(f"{command}-{label}-{'subdir' if sub else 'root'}",
                             [*argv, f":(attr:{spec})"], sub=sub, mutate=command in MUTATING)
                case(f"{command}-exclude", [*argv, ":(attr:text)", ":(exclude,attr:kind=source)sub/**"],
                     mutate=command in MUTATING)
                if staged_snapshot.exists():
                    shutil.rmtree(repo)
                    shutil.copytree(staged_snapshot, repo, symlinks=True)
                    shutil.rmtree(staged_snapshot)
            # Attribute grammar and precedence share the status selector.
            for label, spec in [
                ("multi", ":(attr:text kind=source review)"),
                ("repeat", ":(attr:text text)"),
                ("contradiction", ":(attr:text -text)"),
                ("spaces", ":(attr:  text   kind=source  )"),
                ("spaces-only", ":(attr:   )"),
                ("empty-value", ":(attr:marker=)"),
                ("escaped-comma", r":(attr:marker=a\,b)"),
                ("escaped-letter", r":(attr:kind=so\urce)"),
                ("bare-attr", ":(attr)"),
                ("bare-and-requirement", ":(attr,attr:text)"),
                ("glob", ":(glob,attr:text)**/*.c"),
                ("literal", ":(literal,attr:text)a.c"),
                ("icase-original-attrs", ":(icase,attr:text)CASE.C"),
                ("binary-text", ":(attr:-text -diff -merge binary)"),
                ("binary-blocked", ":(attr:!text -binary)"),
                ("only-exclude", ":(exclude,attr:text)"),
            ]:
                case(label, ["status", "--short", "--", spec])
            case("top-subdir", ["status", "--short", "--", ":(top,attr:text)a.c"], sub="sub")
            case("exclude-subdir", ["status", "--short", "--", ":(attr:text)", ":(exclude)../a.c"], sub="sub")
            (repo / ".git/info/attributes").write_text("a.c -text\nsub/local.h -text\n")
            case("info-precedence", ["status", "--short", "--", ":(attr:-text)"])
            (Path(env["HOME"]) / "global-attrs").write_text("*.txt text\n*.h text\n")
            setup(["config", "core.attributesFile", "~/global-attrs"], repo, env)
            case("global-precedence", ["status", "--short", "--", ":(attr:text)"])
            (repo / ".git/info/attributes").unlink()
            (repo / ".gitattributes").write_text("a.c -binary\n*.bin binary\n")
            case("global-source", ["status", "--short", "--", ":(attr:text)"])
            case("history-worktree-attrs", ["log", "--format=%s", "--name-only", "--", ":(attr:text)"])
            (repo / ".git/info/attributes").write_text("*.bin !binary\n")
            case("binary-info-unspecified", ["status", "--short", "--", ":(attr:!text !diff !merge !binary)*.bin"])
            (repo / ".gitignore").write_text("ignored.c\n")
            (repo / "ignored.c").write_text("needle ignored\n")
            (repo / ".gitattributes").write_text("*.c text\n")
            case("add-explicit-ignored", ["add", "--", ":(attr:text)ignored.c"], mutate=True)
            # Git applies attribute requirements to an untracked directory
            # before visiting its children, but always descends tracked dirs.
            (repo / ".git/info/attributes").unlink()
            (repo / "loose").mkdir()
            (repo / "loose/a.c").write_text("needle loose source\n")
            (repo / "loose/b.h").write_text("needle loose header\n")
            for label, attributes in [("pruned", "*.c text\n*.h -text\n"),
                                      ("admitted", "* text\n*.h -text\n")]:
                (repo / ".gitattributes").write_text(attributes)
                for name, argv in [
                    ("ls-files-others", ["ls-files", "-o", "--"]),
                    ("status", ["status", "--short", "--"]),
                    ("clean", ["clean", "-nd", "--"]),
                    ("clean-force", ["clean", "-fd", "--"]),
                    ("add", ["add", "--"]),
                    ("stash-untracked", ["stash", "push", "-u", "-q", "--"]),
                ]:
                    case(f"untracked-directory-{label}-{name}", [*argv, ":(attr:text)"],
                         mutate=name in ("add", "clean-force", "stash-untracked"))
                case(f"untracked-directory-{label}-literal",
                     ["add", "--", ":(attr:text)loose/a.c"], mutate=True)
                case(f"untracked-directory-{label}-glob",
                     ["add", "--", ":(attr:text)loose/*.c"], mutate=True)

            (repo / ".gitattributes").write_text("*.c text\n*.h -text\n")
            (repo / ".gitignore").write_text("ignored.c\nignored-dir/\n")
            (repo / "ignored-dir").mkdir()
            (repo / "ignored-dir/a.c").write_text("ignored source\n")
            (repo / "ignored-dir/b.h").write_text("ignored header\n")
            case("ignored-directory-ls-files",
                 ["ls-files", "-o", "-i", "--exclude-standard", "--", ":(attr:text)"])
            for label, spec in [("set", ":(attr:text)"), ("missing", ":(attr:missing)"),
                                ("glob", ":(attr:text)ignored-dir/*.c"),
                                ("other", ":(attr:text)other"),
                                ("exclude", ":(exclude,attr:text)ignored-dir")]:
                case("ignored-directory-clean-" + label, ["clean", "-fdX", "--", spec], mutate=True)
        elif mode == "boundaries":
            specs = [":(attr:)", ":(attr:-)", ":(attr:!)", ":(attr:=value)", ":(attr:--bad)",
                     ":(attr:bad/name)", ":(attr:text\tother)", ":(attr:-name=value)",
                     ":(attr:!name=value)", ":(attr:é)", ":(attr:x=a.b)", ":(attr:x=a/b)",
                     ":(attr:x=a\\)", r":(attr:x=a\))", ":(attr:text,attr:kind=source)",
                     ":(literal,glob,attr:text)", ":(literal,glob,attr:)", ":(attr,top)",
                     ":(attr:x=.\\", ":(attr:x=.\\)", ":(attr:x=\\\\)", ":(attr:x=é)"]
            for i, spec in enumerate(specs):
                case(f"invalid-{i}", ["status", "--short", "--", spec], mutate=True)
            for command, argv in [("ls-tree", ["ls-tree", "-r", "HEAD", "--name-only"])]:
                for label, spec in [("set", ":(attr:text)"), ("unset", ":(attr:-text)"),
                                    ("unspecified", ":(attr:!text)"), ("value", ":(attr:kind=source)"),
                                    ("combined", ":(glob,icase,exclude,attr:text)*")]:
                    case(f"reject-{command}-{label}", [*argv, spec])
            # Native controls retain their argument spelling, including diff's
            # revision/path disambiguation when there is no explicit --.
            controls(case)
            for command, argv in SELECT_COMMANDS.items():
                if command in ("rm", "checkout", "stash", "add", "restore"):
                    case(f"{command}-unmatched-attr",
                         [*argv, ":(attr:missing)a.c"], mutate=True)
                    case(f"{command}-exclude-everything",
                         [*argv, ":(exclude)**"], mutate=True)
            for flag in ("--exit-code", "--quiet"):
                case("diff-dirty-" + flag, ["diff", flag, "--name-only", ":(attr:text)"])
                case("diff-clean-" + flag, ["diff", flag, "--name-only", ":(attr:missing)"])
                case("diff-patch-" + flag, ["diff", flag, ":(attr:-text)"])
            case("diff-revision-magic", ["diff", "--name-only", "HEAD", ":(attr:-text)"])
            for command, argv in SELECT_COMMANDS.items():
                if command in ("diff", "log", "show", "reset", "checkout"):
                    case(f"{command}-implicit-magic", [*argv[:-1], ":(attr:text)"],
                         mutate=command in MUTATING)
            case("checkout-positive-fully-excluded",
                 ["checkout", "--", ":(attr:text)", ":(exclude)**"], mutate=True)
            case("checkout-tree-fully-excluded",
                 ["checkout", "HEAD", "--", ":(attr:text)", ":(exclude)**"], mutate=True)
            case("stash-unmatched-subdir",
                 ["stash", "push", "-q", "--", ":(attr:missing)a.c"], sub="sub", mutate=True)

        else:
            case("e2e-stage-text", ["add", "--", ":(attr:text)"], mutate=True, keep=True)
            case("e2e-staged-names", ["diff", "--cached", "--name-only", "--", ":(attr:text)"])
            case("e2e-commit", ["commit", "-q", "-m", "text update"], mutate=True, keep=True)
            case("e2e-log", ["log", "--format=%s", "--name-only", "--", ":(attr:text)"])
            case("e2e-grep-worktree", ["grep", "-n", "changed", "--", ":(attr:text)"])
            case("e2e-grep-tree", ["grep", "-n", "changed", "HEAD", "--", ":(attr:text)"])
            case("e2e-remaining-unset", ["diff", "--name-only", "--", ":(attr:-text)"])
            case("e2e-status", ["status", "--short"])
            case("e2e-index", ["ls-files", "--stage"])
            case("e2e-final-state", ["diff", "--exit-code", "HEAD", "--", ":(attr:text)"], mutate=True, keep=True)
    if not REPORT["comparisons"]:
        raise RuntimeError("zero selected comparisons")


def controls(case):
    for label, args in [
        ("set", ["ls-files", ":(attr:text)"]),
        ("unset", ["ls-files", ":(attr:-text)"]),
        ("unspecified", ["ls-files", ":(attr:!text)"]),
        ("status", ["status", "--short", ":(attr:text)"]),
        ("diff", ["diff", "--name-only", ":(attr:-text)"]),
        ("ls-tree", ["ls-tree", "-r", "HEAD", "--name-only", ":(attr:text)"]),
    ]:
        case("control-" + label, args)


mode = sys.argv[1] if len(sys.argv) > 1 else "select"
try:
    main(mode)
except Exception as error:
    REPORT["unexpected_errors"] += 1
    print(f"UNEXPECTED: {error}", file=sys.stderr)
finally:
    REPORT["seconds"] = round(time.monotonic() - START, 3)
    directory = Path(os.environ.get("NGIT_REPORT_DIR", ROOT / "target/include-pathspec/reports"))
    directory.mkdir(parents=True, exist_ok=True)
    (directory / f"pathspec-attr-{mode}.json").write_text(json.dumps(REPORT, indent=2) + "\n")
    if os.environ.get("NGIT_COMPAT_REPORT"):
        Path(os.environ["NGIT_COMPAT_REPORT"]).write_text(json.dumps(REPORT, indent=2) + "\n")
print(json.dumps({k: v for k, v in REPORT.items() if k not in ("cases", "source_policy")}))
sys.exit(bool(REPORT["failures"] or REPORT["unexpected_errors"]))
