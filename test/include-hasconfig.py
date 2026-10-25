#!/usr/bin/env python3
"""Compare remote URL conditional configuration with Git, byte for byte."""
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
BAD = re.compile(rb"void-function|void-variable|wrong-type-argument|Wrong type argument|Lisp error|[Ll]oader error|Cannot open load file|[Oo]ut of memory|resource exhausted")
PREFIX = "hasconfig:remote.*.url:"
URL = "https://example.com/acme/tool.git"


def run(command, cwd, env, input_bytes=None):
    budget = min(50, DEADLINE - time.monotonic())
    if budget <= 0:
        raise RuntimeError("overall timeout")
    process = subprocess.Popen(command, cwd=cwd, env=env, stdin=subprocess.PIPE if input_bytes is not None else None, stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, start_new_session=True)
    try:
        out, err = process.communicate(input=input_bytes, timeout=budget)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGKILL)
        process.communicate()
        raise RuntimeError(f"command timeout: {command}")
    return dict(exit=process.returncode, stdout_b64=base64.b64encode(out).decode(),
                stderr_b64=base64.b64encode(err).decode())


def setup(args, cwd, env):
    result = run(["git", *args], cwd, env)
    if result["exit"]:
        raise RuntimeError(f"fixture failed: {args}: {result}")


def state(base, env):
    """Logical refs/index/objects, work tree, and exact relevant config bytes."""
    repos = []
    metadata = []
    result = {}
    for head in sorted(base.rglob("HEAD")):
        gitdir = head.parent
        if (gitdir / "config").is_file() and (gitdir / "objects").is_dir():
            metadata.append(gitdir)
            repos.append(gitdir.parent if gitdir.name == ".git" else gitdir)
    for repo in repos:
        result[str(repo.relative_to(base))] = [
            run(["git", *args], repo, env) for args in (
                ["show-ref", "--head"], ["ls-files", "--stage"],
                ["log", "--all", "--format=%H %P %an <%ae> %cn <%ce> %s"],
                ["fsck", "--no-reflogs"])]
    files = {}
    for path in sorted(base.rglob("*")):
        if path.is_file() and (not any(m in path.parents for m in metadata)
                               or path.name in ("config", "config.worktree", "HEAD")
                               or any(m / "logs" in path.parents for m in metadata)):
            files[str(path.relative_to(base))] = base64.b64encode(path.read_bytes()).decode()
    return dict(repositories=result, files=files)


def compare(name, args, cwd, env, base, mutate=False, input_bytes=None, oracle_state=None):
    if MATCH and MATCH not in name:
        return
    with tempfile.TemporaryDirectory(prefix="ngit-hasconfig-snapshot-") as tmp:
        backup = Path(tmp) / "fixture"
        if mutate:
            shutil.copytree(base, backup, symlinks=True)
        expected = run(["git", *args], cwd, env, input_bytes)
        expected_state = oracle_state if oracle_state is not None else state(base, env) if mutate else None
        if mutate:
            shutil.rmtree(base)
            shutil.copytree(backup, base, symlinks=True)
        actual = run([*COMMAND, *args], cwd, env, input_bytes)
        actual_state = state(base, env) if mutate or oracle_state is not None else None
    unexpected = actual["exit"] < 0 or BAD.search(base64.b64decode(actual["stdout_b64"]) +
                                                  base64.b64decode(actual["stderr_b64"])) is not None
    equal = expected == actual and expected_state == actual_state
    REPORT["comparisons"] += 1
    # Exact counts comparisons governed by the exact contract, including reds.
    REPORT["exact"] += 1
    REPORT["failures"] += int(not equal)
    REPORT["unexpected_errors"] += int(unexpected)
    record = dict(name=name, argv=args, expected=expected, actual=actual,
                  state_equal=expected_state == actual_state)
    if expected_state != actual_state:
        record.update(expected_state=expected_state, actual_state=actual_state)
    REPORT["cases"].append(record)
    if not equal or unexpected:
        print(f"FAIL {name}: {record}", file=sys.stderr)


def include(pattern="https://example.com/acme/**", path="acme.inc", key="path"):
    return f'[includeIf "{PREFIX}{pattern}"]\n{key} = {path}\n'


def main(mode):
    if mode not in ("match", "boundaries", "e2e"):
        raise RuntimeError("expected match, boundaries or e2e")
    with tempfile.TemporaryDirectory(prefix="ngit-include-hasconfig-") as tmp:
        base = Path(tmp) / "fixture"
        base.mkdir()
        repo = base / "repo"
        repo.mkdir()
        home = base / "home"
        home.mkdir()
        xdg = home / "xdg/git"
        xdg.mkdir(parents=True)
        env = {key: value for key, value in os.environ.items() if not key.startswith("GIT_")}
        env.update(HOME=str(home), XDG_CONFIG_HOME=str(home / "xdg"),
                   GIT_CONFIG_SYSTEM=str(base / "system"), LC_ALL="C", TZ="UTC",
                   GIT_AUTHOR_DATE="1700000000 +0000", GIT_COMMITTER_DATE="1700000000 +0000")
        setup(["init", "-q", "-b", "main"], repo, env)
        config = repo / ".git/config"
        original = config.read_text()
        remote = f'[remote "origin"]\nurl = {URL}\n'
        identity = '[user]\nname = Acme User\nemail = acme@example.test\n[demo]\nvalue = included\n'
        global_file = home / ".gitconfig"
        inc = home / "acme.inc"
        inc.write_text(identity)
        (base / "acme.inc").write_text(identity)
        (repo / ".git/acme.inc").write_text(identity)
        (xdg / "acme.inc").write_text(identity)
        (base / "system").write_text("")
        (xdg / "config").write_text("")
        global_file.write_text(include())
        config.write_text(original + remote)

        def case(name, args=None, cwd=None, overrides=None, mutate=False, input_bytes=None, oracle_state=None):
            if mutate and MATCH and MATCH not in name:
                setup(args, cwd or repo, dict(env, **(overrides or {})))
                return
            compare(name, args or ["config", "user.name"], cwd or repo,
                    dict(env, **(overrides or {})), base, mutate, input_bytes, oracle_state)

        case("control-1")
        if mode == "match":
            for name, pattern, url in [
                ("literal", URL, URL), ("nonmatching", "https://other/**", URL),
                ("star-component", "https://example.com/acme/*", URL),
                ("star-no-slash", "https://example.com/*", URL),
                ("globstar", "https://example.com/**", URL),
                ("globstar-zero", "https://example.com/**/tool.git", "https://example.com/tool.git"),
                ("globstar-middle", "https://example.com/**/tool.git", "https://example.com/a/b/tool.git"),
                ("question", "https://example.com/acme/t?ol.git", URL),
                ("class", "https://example.com/acme/[a-z]ool.git", URL),
                ("case-sensitive", "HTTPS://example.com/acme/**", URL),
                ("no-auto-prefix", "acme/**", URL),
                ("no-auto-suffix", "https://example.com/acme/", URL),
                ("empty-pattern", "", URL), ("empty-url", "", ""),
                ("scp-style", "host:acme/*", "host:acme/tool.git")]:
                global_file.write_text(include(pattern))
                config.write_text(original + f'[remote "origin"]\nurl = {url}\n')
                case(name)
            global_file.write_text(include())
            config.write_text(original + '[remote "first"]\nurl = https://other/project\n' + remote +
                              '[remote "origin"]\nurl = https://last/project\n')
            case("several-remotes")
            case("list-order", ["config", "--list"])
            case("origin-list", ["config", "--show-origin", "--list"])
            case("origin-value", ["config", "--show-origin", "--get", "user.name"])
            nested = repo / "a/b"
            nested.mkdir(parents=True)
            case("nested-cwd", cwd=nested)
            case("nested-origin", ["config", "--show-origin", "--list"], cwd=nested)
            case("nested-C", ["-C", "a", "-C", "b", "config", "user.name"])
            (repo / "a/b/selected").write_text(include(path=str(inc)) + remote)
            case("nested-selected-origin", ["config", "-f", "selected", "--includes", "--show-origin", "--list"], cwd=nested)
            (repo / "relative-global").write_text(include(path=str(inc)))
            case("nested-relative-global", cwd=nested, overrides=dict(GIT_CONFIG_GLOBAL="relative-global"))
            case("no-includes", ["config", "--no-includes", "user.name"])
            global_file.write_text('[demo]\nvalue = before\n' + include() + '[demo]\nvalue = after\n')
            config.write_text(original + remote)
            case("position-all", ["config", "--show-origin", "--get-all", "demo.value"])
            case("position-last", ["config", "demo.value"])
            # Every source can define the condition and/or the URL.
            for source, path, option in [("system", base / "system", "--system"),
                                         ("xdg", xdg / "config", None),
                                         ("global", global_file, "--global"),
                                         ("local", config, "--local")]:
                global_file.write_text("")
                config.write_text(original)
                path.write_text((original if source == "local" else "") + include() + remote)
                case(f"scope-{source}")
                case(f"scope-{source}-origin", ["config", "--show-origin", "--list"])
                if option:
                    case(f"selected-{source}-default", ["config", option, "user.name"])
                    case(f"selected-{source}-includes", ["config", option, "--includes", "user.name"])
                path.write_text(original if source == "local" else "")
            global_file.write_text(include())
            config.write_text(original)
            (xdg / "config").write_text(remote)
            case("url-in-xdg")
            (xdg / "config").write_text("")
            (base / "system").write_text(remote)
            case("url-in-system")
            for value in ("on", "2", "false"):
                case("nosystem-" + value, overrides=dict(GIT_CONFIG_NOSYSTEM=value))
            (base / "system").write_text("")
            global_file.write_text(include() + remote)
            case("url-later-global")
            global_file.write_text(include())
            config.write_text(original + '[include]\npath = urls.inc\n')
            (repo / ".git/urls.inc").write_text(remote)
            case("url-in-unconditional-include")
            config.write_text(original + '[extensions]\nworktreeConfig = true\n')
            worktree = repo / ".git/config.worktree"
            worktree.write_text(remote + include())
            case("worktree-url")
            case("worktree-origin", ["config", "--show-origin", "--list"])
            case("selected-worktree", ["config", "--worktree", "--includes", "user.name"])
            for value in ("2", "-1", "0", "2k", "garbage"):
                config.write_text(original + f'[extensions]\nworktreeConfig = {value}\n')
                case("worktree-boolean-" + value)
            worktree.unlink()
            config.write_text(original)
            global_file.write_text(include())
            case("cli-url", ["-c", f"remote.origin.url={URL}", "config", "user.name"])
            case("cli-nested", ["-C", "a", "-c", f"remote.origin.url={URL}", "-C", "b", "config", "user.name"])
            case("env-count-url", overrides=dict(GIT_CONFIG_COUNT="1", GIT_CONFIG_KEY_0="remote.origin.url", GIT_CONFIG_VALUE_0=URL))
            case("env-old-url", overrides=dict(GIT_CONFIG_PARAMETERS=f"'remote.origin.url={URL}'"))
            case("env-new-url", overrides=dict(GIT_CONFIG_PARAMETERS=f"'remote.origin.url'='{URL}'"))
            case("cli-include", ["-c", f"remote.origin.url={URL}", "-c",
                                 f"includeif.{PREFIX}https://example.com/acme/**.path={inc}",
                                 "config", "--show-origin", "--get-all", "user.name"])
            custom = base / "selected"
            custom.write_text(include() + remote)
            for name, flags, extra in [("file", ["--file", str(custom)], {}),
                                       ("env-file", [], {"GIT_CONFIG": str(custom)}),
                                       ("env-global", [], {"GIT_CONFIG_GLOBAL": str(custom)})]:
                case(name + "-default", ["config", *flags, "user.name"], overrides=extra)
                case(name + "-includes", ["config", *flags, "--includes", "--show-origin", "--list"], overrides=extra)
            config.write_text(original + remote)
            custom.write_text(include())
            case("selected-does-not-see-repo", ["config", "--file", str(custom), "--includes", "user.name"])
            global_file.write_text("")
            for order, text in [("after", include(path=str(inc)) + remote),
                                ("before", remote + include(path=str(inc)))]:
                for action in (["user.name"], ["--show-origin", "--list"]):
                    case("stdin-url-" + order + "-" + action[-1], ["config", "-f", "-", *action],
                         input_bytes=text.encode())
            case("stdin-explicit-includes", ["config", "-f", "-", "--includes", "user.name"],
                 input_bytes=(include(path=str(inc)) + remote).encode())
            case("stdin-explicit-no-includes", ["config", "-f", "-", "--no-includes", "--list"],
                 input_bytes=(include(path=str(inc)) + remote).encode())
            # Linked worktree config sources use the common config for URLs.
            linked = base / "linked"
            setup(["-c", "user.name=Fixture", "-c", "user.email=fixture@example.test",
                   "commit", "--allow-empty", "-q", "-m", "worktree fixture"], repo, env)
            setup(["worktree", "add", "-q", "-b", "linked", str(linked)], repo, env)
            global_file.write_text(include())
            case("linked-worktree-without-extension", ["config", "--worktree", "--includes", "user.name"], cwd=linked)
            config.write_text(config.read_text() + '[extensions]\nworktreeConfig = true\n')
            case("linked-common-url", cwd=linked)
            case("linked-origins", ["config", "--show-origin", "--list"], cwd=linked)
            bare = base / "bare.git"
            setup(["init", "--bare", "-q", "-b", "main", str(bare)], base, env)
            with (bare / "config").open("a") as stream:
                stream.write(remote)
            case("bare-origin", ["config", "--show-origin", "--list"], cwd=bare)
            case("relative-gitdir-origin", ["config", "--show-origin", "--list"], overrides=dict(GIT_DIR="./.git"))
        elif mode == "boundaries":
            inc.write_text(identity + '[remote "x"]\nurl = https://other/x\n')
            case("control-2")
            case("fatal-list-prefix", ["config", "--show-origin", "--list"])
            case("fatal-before-no-includes", ["config", "--no-includes", "--list"])
            global_file.write_text(include("https://never/**"))
            case("unmatched-still-forbidden")
            global_file.write_text(include())
            inc.write_text('[include]\npath = deeper.inc\n')
            (home / "deeper.inc").write_text('[remote "x"]\nurl = https://other/x\n')
            case("indirect-forbidden")
            (home / "deeper.inc").write_text(identity)
            case("nested-ordinary-include")
            inc.write_text('[includeIf "onbranch:main"]\npath = deeper.inc\n')
            case("nested-onbranch-without-remote")
            inc.write_text(include(path="deeper.inc"))
            case("nested-hasconfig-include")
            (home / "deeper.inc").write_text('[remote "x"]\nurl = https://other/x\n')
            case("nested-hasconfig-forbidden")
            for condition in ("onbranch:main", "gitdir:**/repo/.git"):
                inc.write_text(f'[includeIf "{condition}"]\npath = deeper.inc\n')
                case("indirect-" + condition.split(":")[0] + "-forbidden")
            inc.write_text(identity)
            for name, condition in [("unsupported-key", "hasconfig:user.name:Acme*"),
                                    ("missing-pattern-colon", "hasconfig:remote.*.url"),
                                    ("literal-remote", "hasconfig:remote.origin.url:**"),
                                    ("uppercase-condition", "HASCONFIG:remote.*.url:**"),
                                    ("unsupported-case-flag", "hasconfig/i:remote.*.url:**")]:
                global_file.write_text(f'[includeIf "{condition}"]\npath = absent\n')
                case(name)
                case(name + "-list", ["config", "--show-origin", "--list"])
            global_file.write_text(include(path="missing"))
            case("missing-file")
            global_file.write_text(include() + '[includeIf "onbranch:main"]\npath = branch.inc\n')
            (home / "branch.inc").write_text('[remote "x"]\nurl = https://other/x\n')
            case("collection-forbids-other-conditional")
            (home / "branch.inc").write_text('[demo]\nbranch = yes\n')
            case("interplay-onbranch", ["config", "--show-origin", "--list"])
            global_file.write_text('[includeIf "gitdir:**/repo/.git"]\npath = nested.inc\n')
            (home / "nested.inc").write_text(include())
            case("hasconfig-inside-gitdir")
            global_file.write_text(include(key="ignored"))
            inc.write_text('[remote "x"]\nurl = other\n')
            global_file.write_text(global_file.read_text() + include(path="acme.inc", key="path"))
            case("non-path-key-triggers-scan")
            global_file.write_text(include())
            inc.write_text('[remote]\nurl = ignored\n[remote "x"]\npushurl = ignored\n')
            case("non-url-keys-permitted", ["config", "--list"])
            inc.write_text('[remote "a.b"]\nurl = forbidden\n')
            case("dotted-remote-forbidden")
            global_file.write_text(include("https://never/**"))
            inc.write_text('[broken\n')
            case("unmatched-invalid-included-file")
            global_file.write_text(f'[includeIf "{PREFIX}**"]\npath\n')
            case("missing-path-value")
            global_file.write_text(include())
            inc.write_text(identity)
            config.write_text(original + '[remote "origin"]\npushurl = ' + URL + '\n')
            case("pushurl-not-collected")
            # A selected file's scan cannot escape to the repository sources.
            selected = base / "selected"
            selected.write_text(include() + remote)
            (base / "acme.inc").write_text('[remote "x"]\nurl = forbidden\n')
            case("selected-file-forbidden", ["config", "-f", str(selected), "--includes", "user.name"])
            case("selected-file-disabled", ["config", "-f", str(selected), "--no-includes", "--list"])
            global_file.write_text('[includeIf "onbranch:main"]\npath = branch.inc\n')
            (home / "branch.inc").write_text('[remote "x"]\nurl = https://example.com/a\n')
            trigger = f"includeif.{PREFIX}**.ignored=unused"
            case("command-unknown-key-scan", ["-c", trigger, "config", "--list"])
            case("environment-unknown-key-scan", ["config", "--list"],
                 overrides=dict(GIT_CONFIG_PARAMETERS=f"'{trigger}'"))
            global_file.write_text("")
            case("command-invalid-key", ["-c", "nosection=value", "config", "--list"])
            case("command-missing-argument", ["-c"])
            case("environment-invalid-key", ["config", "--list"],
                 overrides=dict(GIT_CONFIG_COUNT="1", GIT_CONFIG_KEY_0="nosection", GIT_CONFIG_VALUE_0="value"))
            for count in ("+", "-0"):
                case("environment-count-" + count, ["config", "--list"], overrides=dict(GIT_CONFIG_COUNT=count))
            case("command-relative-gitdir", ["-c", f"includeif.gitdir:./x.path={inc}", "config", "--list"])
            inc.write_text(identity)
            (home / "sub").mkdir()
            case("command-origin-spelling", ["-c", f"include.path={home}/sub/../acme.inc",
                                              "config", "--show-origin", "--list"])
            (base / "acme.inc").write_text(identity)
            case("selected-file-fatal-list", ["config", "-f", str(selected), "--includes", "--list"],
                 input_bytes=None)
            inc.write_text('[remote "x"]\nurl = forbidden\n')
            case("stdin-forbidden", ["config", "-f", "-", "--list"],
                 input_bytes=(include(path=str(inc)) + remote).encode())
            case("stdin-relative-include", ["config", "-f", "-", "--list"],
                 input_bytes=('[include]\npath = relative\n').encode())
            inc.write_text(identity)
            case("stdin-scan-line-number", ["config", "-f", "-", "--includes", "--list"],
                 input_bytes=(include(path=str(inc)) + '\n\n[broken\n').encode())
            case("stdin-scan-bare-key", ["config", "-f", "-", "--includes", "--list"],
                 input_bytes=(include(path=str(inc)) + 'ignored = true\n').encode())
        else:
            global_file.write_text("")
            seed = base / "seed"
            seed.mkdir()
            setup(["init", "-q", "-b", "main"], seed, env)
            (seed / "payload").write_text("initial\n")
            setup(["add", "payload"], seed, env)
            setup(["-c", "user.name=Seed", "-c", "user.email=seed@example.test",
                   "commit", "-q", "-m", "Initial"], seed, env)
            bare = base / "upstream.git"
            setup(["clone", "--bare", "-q", str(seed), str(bare)], base, env)
            global_file.write_text(include(str(base / "*.git")))
            inc.write_text(identity + '[push]\ndefault = simple\n[core]\nquotePath = false\n')
            clone = base / "clone"
            initial = base.parent / "workflow.initial"
            shutil.copytree(base, initial, symlinks=True)
            # Clone writes reflogs before origin exists, so the hasconfig
            # identity cannot apply yet; give it an explicit committer.
            clone_ident = dict(GIT_COMMITTER_NAME="Clone", GIT_COMMITTER_EMAIL="clone@example.test")
            case("e2e-clone", ["clone", "-q", str(bare), "clone"], cwd=base, overrides=clone_ident,
                 mutate=True)
            case("e2e-identity", cwd=clone)
            case("e2e-settings", ["config", "push.default"], cwd=clone)
            case("e2e-origins", ["config", "--show-origin", "--list"], cwd=clone)
            (clone / "payload").write_text("initial\nconditional config payload\n")
            case("e2e-add", ["add", "payload"], cwd=clone, mutate=True)
            case("e2e-commit", ["commit", "-q", "-m", "Conditional identity"], cwd=clone, mutate=True)
            case("e2e-author", ["show", "-s", "--format=%an <%ae>%n%cn <%ce>%n%B", "HEAD"], cwd=clone)
            case("e2e-push", ["push", "-q"], cwd=clone, mutate=True)
            case("e2e-upstream", ["log", "-1", "--format=%H %an <%ae> %s"], cwd=bare)
            case("e2e-final-status", ["status", "--porcelain"], cwd=clone)
            case("e2e-final-refs", ["rev-parse", "HEAD", "refs/remotes/origin/main",
                                    "refs/remotes/origin/HEAD"], cwd=clone)
            if not MATCH or MATCH in "e2e-final-state":
                # Replay the whole native workflow from its initial fixture,
                # so an earlier mismatch cannot become the later oracle.
                actual_fixture = base.parent / "workflow.actual"
                shutil.copytree(base, actual_fixture, symlinks=True)
                shutil.rmtree(base)
                shutil.copytree(initial, base, symlinks=True)
                setup(["clone", "-q", str(bare), "clone"], base, dict(env, **clone_ident))
                (clone / "payload").write_text("initial\nconditional config payload\n")
                setup(["add", "payload"], clone, env)
                setup(["commit", "-q", "-m", "Conditional identity"], clone, env)
                setup(["push", "-q"], clone, env)
                oracle = state(base, env)
                shutil.rmtree(base)
                shutil.copytree(actual_fixture, base, symlinks=True)
                case("e2e-final-state", ["status", "--porcelain"], cwd=clone, oracle_state=oracle)
    if not REPORT["comparisons"]:
        raise RuntimeError("zero selected comparisons")


MODE = sys.argv[1] if len(sys.argv) > 1 else "match"
try:
    main(MODE)
except Exception as error:
    REPORT["unexpected_errors"] += 1
    print(f"UNEXPECTED: {error}", file=sys.stderr)
finally:
    REPORT["elapsed_seconds"] = round(time.monotonic() - START, 3)
    directory = Path(os.environ.get("NGIT_REPORT_DIR", ROOT / "target/include-pathspec/reports"))
    directory.mkdir(parents=True, exist_ok=True)
    (directory / f"include-hasconfig-{MODE}.json").write_text(json.dumps(REPORT, indent=2) + "\n")
print(json.dumps({key: value for key, value in REPORT.items() if key != "cases"}))
sys.exit(bool(REPORT["failures"] or REPORT["unexpected_errors"]))
