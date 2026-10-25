#!/usr/bin/env python3
"""Exact controls for the Task E re-audit and Task F final-audit findings."""
import base64
import hashlib
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
import zlib

ROOT = Path(__file__).resolve().parents[1]
COMMAND = shlex.split(os.environ.get("NGIT_COMPAT_COMMAND", str(ROOT / "bin/ngit")))
MATCH = os.environ.get("NGIT_COMPAT_MATCH", "")
START = time.monotonic()
DEADLINE = START + 530
REPORT = dict(comparisons=0, exact=0, intended={}, failures=0, unexpected_errors=0, cases=[])
BAD = re.compile(rb"void-function|void-variable|Symbol.s (?:function definition|value).*void|wrong-type-argument|Wrong type argument|Invalid function|Lisp error|[Ll]oader error|Cannot open load file|[Oo]ut of memory|resource exhausted")

# Git repeats this warning after resetting its attribute direction and in
# stash's subprocesses.  ngit shares its in-process attribute cache.  Replaying
# warnings without reads would invent internal work; retain one exact warning
# and require exact Git refs/index/worktree/config state, including exclusions.
INTENDED_WARNINGS = {
    'symlink-attr-' + command + '-' + sub: dict(
        git_count=count,
        reason='Git repeats the nofollow warning across independent attribute contexts; '
               'ngit emits it once from its shared in-process cache. Repository state, '
               'exit status, stdout and each diagnostic text are asserted exactly.')
    for command, count in (('reset', 3), ('stash', 14)) for sub in ('root', 'sub')
}


def run(argv, cwd, env, data=None):
    budget = min(35, DEADLINE - time.monotonic())
    if budget <= 0:
        raise RuntimeError("overall deadline exceeded")
    p = subprocess.Popen(argv, cwd=cwd, env=env, stdin=subprocess.PIPE if data is not None else None, stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE, start_new_session=True)
    try:
        stdout, stderr = p.communicate(input=data, timeout=budget)
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
                                str(rel) in (".git/config", ".git/HEAD", ".git/info/attributes",
                                             ".git/FETCH_HEAD", ".git/ORIG_HEAD", ".git/MERGE_AUTOSTASH") or
                                rel.parts[:2] == (".git", "logs")):
            files[str(rel)] = [path.stat().st_mode & 0o777, base64.b64encode(path.read_bytes()).decode()]
    for path in sorted(Path(env["HOME"]).rglob("*")):
        if path.is_file():
            files["HOME/" + str(path.relative_to(env["HOME"]))] = base64.b64encode(path.read_bytes()).decode()
    return [run(["git", *a], repo, env) for a in commands], files


def repository_bytes(repo):
    """Include the raw index, FETCH_HEAD and every ref for refusal checks."""
    return {str(p.relative_to(repo)): (os.readlink(p) if p.is_symlink() else p.read_bytes())
            for p in repo.rglob('*') if p.is_symlink() or p.is_file()}


def compare(name, args, repo, env, sub="", mutate=False, keep=False, retain=(), data=None,
            refusal=None, editor_exit=0):
    if MATCH and MATCH not in name:
        return
    before_state = state(repo, env)
    before_bytes = repository_bytes(repo) if refusal else None
    snapshot = repo.parent / "snapshot"
    if mutate:
        shutil.copytree(repo, snapshot, symlinks=True)
    expected = run(["git", *args], repo / sub, env, data)
    expected_state = state(repo, env) if mutate else None
    if mutate:
        shutil.rmtree(repo)
        shutil.copytree(snapshot, repo, symlinks=True)
    actual = run([*COMMAND, *args], repo / sub, env, data)
    actual_state = state(repo, env) if mutate else None
    unexpected = actual["exit"] < 0 or bool(BAD.search(
        base64.b64decode(actual["stdout_b64"]) + base64.b64decode(actual["stderr_b64"])))
    def retained_index(snapshot):
        lines = base64.b64decode(snapshot[0][1]['stdout_b64']).splitlines()
        return [line for line in lines if line.partition(b'\t')[2].decode() in retain]
    retained = (all(before_state[1].get(path) == actual_state[1].get(path) for path in retain)
                and (not retain or retained_index(before_state) == retained_index(actual_state)))
    equal = expected == actual and expected_state == actual_state and retained
    intended = INTENDED_WARNINGS.get(name)
    if intended:
        expected_warning = b"warning: unable to access '.gitattributes': Too many levels of symbolic links\n"
        recorded = dict(exit=0, stdout_b64='', stderr_b64=base64.b64encode(expected_warning).decode())
        oracle = dict(recorded, stderr_b64=base64.b64encode(expected_warning * intended['git_count']).decode())
        # Both diagnostics are fixed expectations; only repetition differs.
        equal = actual == recorded and expected == oracle and expected_state == actual_state and retained
        REPORT['intended'][name] = intended['reason']
    if refusal:
        intended = True
        recorded = dict(exit=128, stdout_b64='', stderr_b64=base64.b64encode(
            f'fatal: pull with rebase={refusal} is not supported\n'.encode()).decode())
        editor_called = (repo / 'sequence-called').exists()
        # The oracle's sequence editor records its call in the snapshot that
        # was captured before restoring the ngit fixture.
        oracle_editor = 'sequence-called' in (expected_state[1] if mutate else {})
        equal = (expected['exit'] == editor_exit and actual == recorded and
                 (editor_exit == 0 or oracle_editor) and not editor_called and
                 before_state == actual_state and before_bytes == repository_bytes(repo))
        REPORT['intended'][name] = ('ngit rebase does not implement ' + refusal +
            '; received objects remain temporary and no repository file changes.')
    differences = None
    if mutate and expected_state != actual_state:
        differences = dict(commands=[dict(index=i, expected=a, actual=b)
                                     for i, (a, b) in enumerate(zip(expected_state[0], actual_state[0])) if a != b],
                           files={k: dict(expected=expected_state[1].get(k), actual=actual_state[1].get(k))
                                  for k in expected_state[1].keys() | actual_state[1].keys()
                                  if expected_state[1].get(k) != actual_state[1].get(k)})
    REPORT["comparisons"] += 1
    # Every case uses the exact-comparison contract, including failed cases.
    REPORT["exact"] += int(not intended)
    REPORT["failures"] += int(not equal or unexpected)
    REPORT["unexpected_errors"] += int(unexpected)
    REPORT["cases"].append(dict(name=name, argv=args, subdirectory=sub, equal=equal, expected=expected,
                                actual=actual, state_equal=(expected_state == actual_state) if mutate else None,
                                state_differences=differences,
                                retained_paths=list(retain), retained_unchanged=retained,
                                repository_bytes_unchanged=(before_bytes == repository_bytes(repo)) if refusal else None,
                                before_state=before_state if mutate else None,
                                actual_state=actual_state if mutate else None))
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



BOOLEAN_SPECIALS = {
    'diff.renames / status.renames': 'ngit-config-renames-p: case-insensitive copy/copies, then boolean',
    'pull.rebase / branch.<name>.rebase': 'ngit-tr--config-rebase: merges/m, interactive/i; preserve/p diagnostic',
    'core.autocrlf': 'ngit--crlf-action and ngit--checkout-eol-action guard input before boolean',
    'core.logallrefupdates': 'ngit--append-reflog guards always before boolean',
    'gc.packrefs': 'ngit-cmd-gc guards notbare before boolean',
    'branch.autosetupmerge': 'ngit-cmd-branch guards always/simple/inherit before boolean',
}
REPORT['boolean_only_keys'] = [
    'GIT_CONFIG_NOSYSTEM', 'extensions.worktreeconfig', 'core.filemode',
    'core.symlinks', 'core.bare', 'gc.autodetach', 'advice.*', 'remote.<name>.prune',
    'fetch.prune', 'merge.stat', 'diff.relative', 'status.relativepaths',
    'log.abbrevcommit', 'log.showroot', 'clean.requireforce',
]
REPORT['boolean_special_values'] = BOOLEAN_SPECIALS
REPORT['boolean_callers'] = [
    dict(file=str(path.relative_to(ROOT)), line=number, expression=line.strip())
    for path in sorted((ROOT / 'lisp').glob('*.el'))
    for number, line in enumerate(path.read_text().splitlines(), 1)
    if ('(ngit-config-bool ' in line or '(ngit--config-boolean-value ' in line)
       and not line.startswith('(defun ')
]


def write(repo, name, text):
    path = repo / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)


def fixture(base):
    repo, home = base / 'repo', base / 'home'
    repo.mkdir()
    home.mkdir()
    env = {k: v for k, v in os.environ.items() if not k.startswith('GIT_')}
    env.update(HOME=str(home), XDG_CONFIG_HOME=str(home / 'xdg'), LC_ALL='C', TZ='UTC',
               GIT_CONFIG_NOSYSTEM='1', GIT_AUTHOR_NAME='Fixture', GIT_AUTHOR_EMAIL='fixture@example.test',
               GIT_COMMITTER_NAME='Fixture', GIT_COMMITTER_EMAIL='fixture@example.test',
               GIT_AUTHOR_DATE='1700000000 +0000', GIT_COMMITTER_DATE='1700000000 +0000')
    setup(['init', '-q', '-b', 'main'], repo, env)
    write(repo, '.gitattributes', 'keep.txt text\nsub/keep.txt text\n')
    for name in ('keep.txt', 'other.txt', 'sub/keep.txt', 'sub/other.txt'):
        write(repo, name, 'base\n')
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'base'], repo, env)
    return repo, env


def symlinks(repo, env):
    write(repo, 'attr-target', '* !text\n')
    (repo / '.gitattributes').unlink()
    (repo / '.gitattributes').symlink_to('attr-target')
    for name in ('keep.txt', 'other.txt', 'sub/keep.txt', 'sub/other.txt'):
        write(repo, name, 'changed\n')
    # Neither the symlink target nor either excluded tracked path may change.
    for command, args in [
        ('rm', ['rm', '-f', '--']),
        ('checkout', ['checkout', '--']),
        ('reset', ['reset', '-q', 'HEAD', '--']),
        ('stash', ['stash', 'push', '-q', '--']),
    ]:
        for sub in ('', 'sub'):
            if command == 'reset':
                setup(['add', 'keep.txt', 'other.txt', 'sub/keep.txt', 'sub/other.txt'], repo, env)
            compare('symlink-attr-' + command + ('-sub' if sub else '-root'),
                    [*args, '*.txt', ':(exclude,attr:text)'], repo, env, sub=sub, mutate=True,
                    retain=('.gitattributes', 'attr-target', 'keep.txt', 'sub/keep.txt'))
    # A worktree ignore symlink is ignored; explicit exclude sources still follow it.
    write(repo, 'ignore-target', 'loose.txt\n')
    (repo / '.gitignore').symlink_to('ignore-target')
    write(repo, 'loose.txt', 'untracked\r\n')
    compare('symlink-ignore-status', ['status', '--porcelain', '--', 'loose.txt'], repo, env)
    compare('symlink-ignore-add', ['add', '--', 'loose.txt'], repo, env, mutate=True)
    compare('symlink-ignore-clean', ['clean', '-f', '--', 'loose.txt'], repo, env, mutate=True)
    compare('symlink-ignore-explicit-source',
            ['-c', 'core.excludesFile=.gitignore', 'ls-files', '-o', '--exclude-standard', '--', 'loose.txt'], repo, env)


def mixed(repo, env):
    for sub in ('', 'sub'):
        for name in ('keep.txt', 'other.txt'):
            write(repo, (sub + '/' if sub else '') + name, 'changed\n')
        for cmd in ('add', 'restore'):
            compare('mixed-glob-' + cmd + ('-sub' if sub else '-root'),
                    [cmd, '--', '*.txt', ':(exclude)keep.txt'], repo, env, sub=sub, mutate=True)


def boundaries(repo, env):
    for sub in ('', 'sub'):
        for cmd in ('add', 'restore'):
            for paths in (['*.missing', ':(exclude)keep.txt'],
                          ['*.missing', 'other.txt', ':(exclude)keep.txt'],
                          ['*.txt', ':(exclude)missing.txt'],
                          ['*.missing', ':(glob)absent*']):
                compare('mixed-missing-' + cmd + '-' + sub + '-' + repr(paths),
                        [cmd, '--', *paths], repo, env, sub=sub, mutate=True)


def config(repo, env):
    for word in ('copy', 'copies', 'COPY', 'COPIES', 'true', 'false', '08'):
        for command in (['log', '-1', '--format=%s'], ['show', '-s', '--format=%s'],
                        ['diff', '--name-only'], ['status', '--porcelain']):
            compare('renames-' + word + '-' + command[0],
                    ['-c', 'diff.renames=' + word, *command], repo, env)
    for whitespace in (' ', '\t', '\n', '\r', '\v', '\f'):
        for count in (whitespace + '1', '1' + whitespace, whitespace, whitespace + '-0'):
            compare('count-whitespace-' + repr(count), ['config', '--get', 'audit.value'], repo,
                    env | dict(GIT_CONFIG_COUNT=count, GIT_CONFIG_KEY_0='audit.value', GIT_CONFIG_VALUE_0='ok'))
        for text in ("'audit.value'='ok'" + whitespace + "'audit.other'='yes'",
                     "'audit.value=ok'" + whitespace + "'audit.other=yes'",
                     "'audit.value'=" + whitespace + "'audit.other'='yes'",
                     whitespace + "'audit.value'='ok'", "'audit.value'='ok'" + whitespace):
            compare('parameters-whitespace-' + repr(text), ['config', '--get', 'audit.value'], repo,
                    env | dict(GIT_CONFIG_PARAMETERS=text))


def revisions(repo, env):
    for command in (['rev-list', '-1', 'HEAD', '--'], ['shortlog', '-s', 'HEAD', '--']):
        for sub in ('', 'sub'):
            for spec in (':(attr:!__audit_absent__)', ':(attr:text)', ':(attr:missing)',
                         '*.txt', ':(exclude,attr:text)'):
                compare('revision-' + command[0] + '-' + sub + '-' + spec,
                        [*command, spec], repo, env, sub=sub)

    for flags in (['--objects'], ['--count'], ['--parents']):
        for spec in (':(attr:text)', ':(attr:-text)', ':(attr:!__audit_absent__)'):
            compare('revision-extended-' + flags[0] + '-' + spec,
                    ['rev-list', *flags, 'HEAD', '--', spec], repo, env)
    for command in (['rev-list', 'HEAD'], ['shortlog', '-s', 'HEAD']):
        compare('revision-mixed-' + command[0],
                [*command, '--', '*.txt', ':(exclude,attr:text)'], repo, env)

    snapshot = repo.parent / 'object-prefix-fixture'
    shutil.copytree(repo, snapshot, symlinks=True)
    write(repo, 'else/keep.txt', 'unrelated directory\n')
    setup(['add', 'else/keep.txt'], repo, env)
    setup(['commit', '-qm', 'object prefix fixture'], repo, env)
    try:
        for sub in ('', 'sub'):
            for spec in (':(attr:text)sub/', ':(top,attr:text)sub/',
                         ':(glob,attr:text)sub/*.txt', ':(icase,attr:text)SUB/'):
                compare('revision-object-prefix-' + sub + '-' + spec,
                        ['rev-list', '--objects', 'HEAD', '--', spec], repo, env, sub=sub)
    finally:
        shutil.rmtree(repo)
        shutil.copytree(snapshot, repo, symlinks=True)
        shutil.rmtree(snapshot)


def special(repo, env):
    for key, words, args in [
        ('branch.autoSetupMerge', ('always', 'simple', 'inherit'), ['branch', 'created', 'main']),
        ('core.logAllRefUpdates', ('always',), ['branch', 'created', 'main']),
        ('core.autocrlf', ('input',), ['add', '--', 'other.txt']),
        ('gc.packRefs', ('notbare',), ['gc', '--prune=never']),
        ('status.renames', ('copy', 'copies', 'COPY', 'COPIES'), ['status', '--porcelain']),
    ]:
        for word in words:
            compare('special-' + key + '-' + word, ['-c', key + '=' + word, *args],
                    repo, env, mutate=args[0] != 'status')
    remote = repo.parent / 'remote'
    setup(['clone', '-q', '--bare', str(repo), str(remote)], repo.parent, env)
    setup(['remote', 'add', 'origin', '../remote'], repo, env)
    setup(['fetch', '-q', 'origin'], repo, env)
    setup(['config', 'branch.main.remote', 'origin'], repo, env)
    setup(['config', 'branch.main.merge', 'refs/heads/main'], repo, env)
    for key in ('pull.rebase', 'branch.main.rebase'):
        for word in ('merges', 'm', 'interactive', 'i', 'preserve', 'p', 'invalid'):
            compare('special-' + key + '-' + word,
                    ['-c', key + '=' + word, 'pull', '-q'], repo, env, mutate=True)
    compare('special-branch-false-overrides-pull',
            ['-c', 'pull.rebase=invalid', '-c', 'branch.main.rebase=false', 'pull', '-q'],
            repo, env, mutate=True)
    for spec in (':(attr:!__audit_absent__)', ':(attr:text)', ':(attr:missing)'):
        compare('revision-format-patch-' + spec,
                ['format-patch', '-1', '--stdout', 'HEAD', '--', spec], repo, env)

    # Binary additions/deletions selected by attributes retain format-patch's
    # literal bodies, including multi-line base85 and its partial final word.
    with (repo / '.gitattributes').open('a') as stream:
        stream.write('*.bin binary\n')
    (repo / 'payload.bin').write_bytes(bytes(range(256)) * 2)
    setup(['add', '.gitattributes', 'payload.bin'], repo, env)
    setup(['commit', '-qm', 'binary addition'], repo, env)
    compare('binary-addition-attr',
            ['format-patch', '-1', '--stdout', 'HEAD', '--', ':(attr:binary)'], repo, env)
    (repo / 'payload.bin').unlink()
    setup(['add', '-u'], repo, env)
    setup(['commit', '-qm', 'binary deletion'], repo, env)
    compare('binary-deletion-attr',
            ['format-patch', '-1', '--stdout', 'HEAD', '--', ':(attr:binary)'], repo, env)


def final_pull(repo, env):
    remote = repo.parent / 'remote'
    setup(['clone', '-q', str(repo), str(remote)], repo.parent, env)
    setup(['remote', 'add', 'origin', '../remote'], repo, env)
    setup(['config', 'branch.main.remote', 'origin'], repo, env)
    setup(['config', 'branch.main.merge', 'refs/heads/main'], repo, env)
    env = env | dict(GIT_SEQUENCE_EDITOR='sh -c \'echo called >sequence-called; exit 1\'')

    def no_replay(shape):
        for word in ('merges', 'm', 'interactive', 'i'):
            forms = [(key, ['-c', key + '=' + word, 'pull'])
                     for key in ('pull.rebase', 'branch.main.rebase')]
            forms.append(('cli', ['pull', '--rebase=' + word]))
            for form, args in forms:
                for quiet in (False, True):
                    compare('final-pull-' + shape + '-' + form + '-' + word +
                            ('-quiet' if quiet else '-normal'),
                            [*args, *(['-q'] if quiet else [])], repo, env, mutate=True,
                            refusal='interactive' if shape == 'ahead' and word in ('interactive', 'i') else None,
                            editor_exit=1 if shape == 'ahead' and word in ('interactive', 'i') else 0)

    no_replay('equal')
    write(repo, 'ahead.txt', 'local ahead\n')
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'already ahead'], repo, env)
    no_replay('ahead')
    setup(['reset', '--hard', '-q', 'HEAD~1'], repo, env)
    write(remote, 'remote.txt', 'remote change\n')
    setup(['add', '.'], remote, env)
    setup(['commit', '-qm', 'remote divergence'], remote, env)
    no_replay('fast-forward')
    write(repo, 'local.txt', 'local change\n')
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'local divergence'], repo, env)
    setup(['checkout', '-qb', 'side', 'HEAD~1'], repo, env)
    write(repo, 'side.txt', 'side change\n')
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'side branch'], repo, env)
    setup(['checkout', '-q', 'main'], repo, env)
    setup(['merge', '-q', '--no-ff', '-m', 'local merge', 'side'], repo, env)
    for word in ('merges', 'm', 'interactive', 'i'):
        for specs in (['main:refs/remotes/a', 'main:refs/remotes/b'],
                      ['main:refs/remotes/a', 'side:refs/remotes/b']):
            setup(['branch', '-f', 'side', 'HEAD~1'], remote, env)
            compare('round3-pull-multiple-' + word + '-' + specs[-1],
                    ['pull', '-q', '--rebase=' + word, 'origin', *specs], repo, env, mutate=True)
    env = env | dict(GIT_SEQUENCE_EDITOR='true', GIT_EDITOR='true')
    for word, mode in (('merges', 'merges'), ('m', 'merges'),
                       ('interactive', 'interactive'), ('i', 'interactive')):
        for key in ('pull.rebase', 'branch.main.rebase'):
            compare('final-pull-divergent-' + key + '-' + word,
                    ['-c', key + '=' + word, 'pull', '-q'], repo, env,
                    mutate=True, refusal=mode)
        compare('final-pull-divergent-cli-' + word, ['pull', '-q', '--rebase=' + word],
                repo, env, mutate=True, refusal=mode)


def final_attrs(repo, env):
    names = ['a.txt', 'audit space.txt', 'octal name.txt', 'tab\tname.txt', 'quote"name.txt', 'café.txt',
             '"unterminated.txt', '"badq.txt', 'nul.txt']
    patterns = ['"a.txt"', '"audit space.txt"', r'"octal\040name.txt"',
                r'"tab\tname.txt"', r'"quote\"name.txt"', r'"caf\303\251.txt"',
                '"unterminated.txt', r'"bad\q.txt', r'"nul.txt\000suffix"']
    for sub in ('', 'sub/'):
        write(repo, sub + '.gitattributes', '\n'.join(p + ' protected' for p in patterns) + '\n')
        for name in names:
            write(repo, sub + name, 'protected base\n')
        write(repo, sub + 'unprotected.txt', 'unprotected base\n')
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'quoted attribute fixture'], repo, env)
    for sub in ('', 'sub'):
        for args in (['check-attr', 'protected', '--', 'nul.txt'],
                     ['ls-files', '--', 'nul.txt', ':(exclude,attr:protected)']):
            compare('round3-nul-' + args[0] + '-' + sub, args, repo, env, sub=sub)
        compare('final-quoted-check-attr-' + sub,
                ['check-attr', 'protected', '--', *names], repo, env, sub=sub)
        attributes = {p: p.read_bytes() for p in (repo / '.gitattributes', repo / 'sub/.gitattributes')}
        for p in attributes:
            p.unlink()
        compare('final-quoted-check-attr-cached-' + sub,
                ['check-attr', 'protected', '--', *names], repo, env, sub=sub)
        for p, contents in attributes.items():
            p.write_bytes(contents)
        protected = tuple((sub + '/' if sub else '') + n for n in names)
        for path in (*protected, (sub + '/' if sub else '') + 'unprotected.txt'):
            write(repo, path, 'changed\n')
        for command in (['ls-files', '--'], ['rm', '-f', '--'],
                        ['checkout', '--'], ['add', '--']):
            compare('final-quoted-exclude-' + command[0] + '-' + sub,
                    [*command, '*.txt', ':(exclude,attr:protected)'], repo, env,
                    sub=sub, mutate=True, retain=protected)


def binary_mail(repo, env, patch):
    oid = base64.b64decode(run(['git', 'rev-parse', 'HEAD'], repo, env)['stdout_b64']).strip()
    return (b'From ' + oid + b' Mon Sep 17 00:00:00 2001\n'
            b'From: Fixture <fixture@example.test>\n'
            b'Date: Tue, 14 Nov 2023 22:13:20 +0000\n'
            b'Subject: [PATCH] binary change\n\n---\n' + patch)


def binary_literal(data, method=b'literal'):
    compressed = zlib.compress(data)
    lines = []
    for offset in range(0, len(compressed), 52):
        chunk = compressed[offset:offset + 52]
        count = len(chunk)
        lines.append(bytes([count + (64 if count <= 26 else 70)]) + base64.b85encode(chunk, pad=True) + b'\n')
    return method + b' ' + str(len(data)).encode() + b'\n' + b''.join(lines) + b'\n'


def delta_size(size):
    result = bytearray()
    while size >= 128:
        result.append((size & 127) | 128)
        size >>= 7
    return bytes(result) + bytes([size])


def round3_binary(repo, env):
    """Exercise path resolution without letting the red control escape."""
    payload = b'audit\x00binary\xff\n'
    oid = hashlib.sha1(b'blob ' + str(len(payload)).encode() + b'\0' + payload).hexdigest().encode()
    zero = b'0' * 40
    def addition(path, mode, contents):
        sha = hashlib.sha1(b'blob ' + str(len(contents)).encode() + b'\0' + contents).hexdigest().encode()
        return (b'diff --git a/' + path + b' b/' + path + b'\nnew file mode ' + mode +
                b'\nindex ' + zero + b'..' + sha + b'\nGIT binary patch\n' +
                binary_literal(contents) + binary_literal(b''))
    # The real apply/am red probes point into the work tree. Only --check
    # sees the external target, so the known broken writer cannot escape.
    for target in (b'inside', b'../outside'):
        (repo / 'inside').mkdir(exist_ok=True)
        parts = [addition(b'link', b'120000', target), addition(b'link/child.bin', b'100644', payload)]
        for order in ('forward', 'backward'):
            patch = b''.join(parts if order == 'forward' else reversed(parts))
            for args in (['apply', '--check'], ['apply'], ['apply', '--index'], ['apply', '--cached'], ['am']):
                if target == b'../outside' and args != ['apply', '--check']:
                    continue
                compare('round3-symlink-' + target.decode() + '-' + order + '-' + '-'.join(args), args,
                        repo, env, mutate=True, data=binary_mail(repo, env, patch) if args == ['am'] else patch)
    (repo / 'data.bin').write_bytes(payload)
    (repo / 'data.bin copy').write_bytes(payload)
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'space path fixture'], repo, env)
    for quoted in (False, True):
        def name(prefix, value):
            value = prefix + value
            return b'"' + value + b'"' if quoted else value
        source, dest = b'data.bin copy', b'renamed file.bin'
        same = b'diff --git ' + name(b'a/', source) + b' ' + name(b'b/', source) + b'\n'
        deletion = (same + b'deleted file mode 100644\nindex ' + oid + b'..' + zero +
                    b'\nGIT binary patch\n' + binary_literal(b'') + binary_literal(payload))
        # The old name must not fall back to the identically populated decoy.
        for missing in (False, True):
            if missing:
                (repo / 'data.bin copy').unlink()
            for args in (['apply', '--check'], ['apply'], ['apply', '--cached'], ['am']):
                compare('round3-space-delete-' + str(quoted) + '-' + str(missing) + '-' + '-'.join(args),
                        args, repo, env, mutate=True, retain=('data.bin',),
                        data=binary_mail(repo, env, deletion) if args == ['am'] else deletion)
            if missing:
                (repo / 'data.bin copy').write_bytes(payload)
        for kind in ('rename', 'copy'):
            patch = (b'diff --git ' + name(b'a/', source) + b' ' + name(b'b/', dest) +
                     b'\nsimilarity index 100%\n' + kind.encode() + b' from ' + name(b'', source) +
                     b'\n' + kind.encode() + b' to ' + name(b'', dest) + b'\n')
            for args in (['apply', '--check'], ['apply', '--index']):
                setup(['update-index', '--refresh'], repo, env)
                compare('round3-space-' + kind + '-' + str(quoted) + '-' + '-'.join(args),
                        args, repo, env, mutate=True, data=patch, retain=('data.bin',))
        text = (same + b'--- ' + name(b'a/', source) + b'\n+++ ' + name(b'b/', source) +
                b'\n@@ -1 +1 @@\n-before\n+after\n')
        (repo / 'data.bin copy').write_bytes(b'before\n')
        for args in (['apply', '--check'], ['apply']):
            compare('round3-space-text-' + str(quoted) + '-' + '-'.join(args), args,
                    repo, env, mutate=True, data=text, retain=('data.bin',))
        (repo / 'data.bin copy').write_bytes(payload)
    for first, second in ((b'a/data.bin copy', b'"b/data.bin copy"'),
                          (b'"a/data.bin copy"', b'b/data.bin copy'),
                          (b'"a/data.bin\\040copy"', b'"b/data.bin\\040copy"')):
        patch = (b'diff --git ' + first + b' ' + second + b'\ndeleted file mode 100644\nindex ' +
                 oid + b'..' + zero + b'\nGIT binary patch\n' + binary_literal(b'') + binary_literal(payload))
        for args in (['apply', '--check'], ['apply']):
            compare('round3-space-mixed-' + repr(first) + '-' + repr(second) + '-' + '-'.join(args),
                    args, repo, env, mutate=True, data=patch, retain=('data.bin',))


def final_binary(repo, env):
    round3_binary(repo, env)
    old = bytes(range(256)) * 8
    new = old[:900] + b'changed\x00payload' + old[915:]
    path = repo / 'payload.bin'
    path.write_bytes(old)
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'binary preimage'], repo, env)
    for kind in ('deletion', 'modification'):
        if kind == 'deletion':
            path.unlink()
        else:
            path.write_bytes(new)
        patch = base64.b64decode(run(['git', 'diff', '--binary', '--full-index'], repo, env)['stdout_b64'])
        summaries = {label: base64.b64decode(run(['git', 'diff', *flags], repo, env)['stdout_b64'])
                     for label, flags in (('short', []), ('full', ['--full-index']))}
        if b'GIT binary patch\n' not in patch:
            raise RuntimeError('fixture did not produce a binary patch')
        path.write_bytes(old)
        setup(['update-index', '--refresh'], repo, env)
        for args in (['apply', '--check'], ['apply'], ['apply', '--cached'], ['apply', '--index']):
            if '--index' in args:
                setup(['update-index', '--refresh'], repo, env)
            compare('final-binary-' + kind + '-valid-' + '-'.join(args), args, repo, env,
                    mutate=True, data=patch)
        compare('final-binary-' + kind + '-am-valid', ['am'], repo, env,
                mutate=True, data=binary_mail(repo, env, patch))
        for label, summary in summaries.items():
            for args in (['apply', '--check'], ['apply'], ['am']):
                compare('final-binary-bodyless-' + kind + '-' + label + '-' + '-'.join(args),
                        args, repo, env, mutate=True,
                        data=binary_mail(repo, env, summary) if args == ['am'] else summary)
        path.write_bytes(old + b'wrong\x00')
        setup(['add', 'payload.bin'], repo, env)
        setup(['commit', '-qm', 'mismatching preimage'], repo, env)
        for label, summary in summaries.items():
            for args in (['apply', '--check'], ['apply']):
                compare('final-binary-bodyless-mismatch-' + kind + '-' + label + '-' + '-'.join(args),
                        args, repo, env, mutate=True, data=summary, retain=('payload.bin',))
        for args in (['apply', '--check'], ['apply'], ['apply', '--cached'], ['apply', '--index'], ['am']):
            if '--index' in args:
                setup(['update-index', '--refresh'], repo, env)
            compare('final-binary-' + kind + '-mismatch-' + '-'.join(args), args, repo, env,
                    mutate=True, data=binary_mail(repo, env, patch) if args == ['am'] else patch,
                    retain=('payload.bin',))
        path.write_bytes(old)
        setup(['add', 'payload.bin'], repo, env)
        setup(['commit', '-qm', 'restore preimage'], repo, env)
        if kind == 'modification':
            header = patch.split(b'GIT binary patch\n', 1)[0] + b'GIT binary patch\n'
            literal = header + binary_literal(new) + binary_literal(old)
            for args in (['apply', '--check'], ['apply'], ['am']):
                compare('final-binary-literal-modification-' + '-'.join(args), args, repo, env,
                        mutate=True, data=binary_mail(repo, env, literal) if args == ['am'] else literal)
            delta_header = delta_size(len(old)) + delta_size(len(new))
            for label, delta in (('bounds', delta_header + b'\x93\xff\xff\x04'),
                                 ('opcode', delta_header + b'\x00'),
                                 ('truncated', delta_header + b'\x91'),
                                 ('base-size', b'\x01\x01\x01x'),
                                 ('continuation-base', b'\x80\x80\x80\x80'),
                                 ('size-overflow', b'\x80' * 9 + b'\x02'),
                                 ('result-size-overflow', delta_size(len(old)) + b'\x80' * 9 + b'\x02'),
                                 ('extended-size', b'\x80\x90' + b'\x80' * 8 + b'\x00\x00'),
                                 ('truncated-size', delta_size(len(old)) + b'\x80\x80'),
                                 ('insert-before-opcode', delta_size(len(old)) + b'\x01\x02AB\x00')):
                broken = header + binary_literal(delta, b'delta') + binary_literal(old)
                for args in (['apply', '--check'], ['apply'], ['am']):
                    compare('final-binary-delta-invalid-' + label + '-' + '-'.join(args),
                            args, repo, env, mutate=True,
                            data=binary_mail(repo, env, broken) if args == ['am'] else broken,
                            retain=('payload.bin',))
            empty_oid = hashlib.sha1(b'blob 0\x00').hexdigest().encode()
            empty_header = re.sub(rb'(index [0-9a-f]{40}\.\.)[0-9a-f]{40}',
                                  rb'\g<1>' + empty_oid, header, count=1)
            empty_delta = empty_header + binary_literal(delta_size(len(old)) + b'\x80\x80', b'delta')
            for args in (['apply', '--check'], ['apply']):
                compare('final-binary-delta-bounded-empty-' + '-'.join(args), args, repo, env,
                        mutate=True, data=empty_delta)
            wrong_oid = re.sub(rb'(index [0-9a-f]{40}\.\.)[0-9a-f]{40}',
                               rb'\g<1>' + b'1' * 40, patch, count=1)
            # Alter a valid binary body's declared inflated size, and then its
            # base85 framing. Both must be rejected even in --check mode.
            size = re.search(rb'\n(?:literal|delta) ([0-9]+)\n', patch)
            bad_size = patch[:size.start(1)] + b'9999' + patch[size.end(1):]
            body = size.end()
            bad_base85 = patch[:body] + b'!' + patch[body + 1:]
            for label, broken in (('postimage', wrong_oid), ('inflate', bad_size), ('base85', bad_base85)):
                for args in (['apply', '--check'], ['apply'], ['am']):
                    compare('final-binary-corrupt-' + label + '-' + '-'.join(args),
                            args, repo, env, mutate=True,
                            data=binary_mail(repo, env, broken) if args == ['am'] else broken,
                            retain=('payload.bin',))
            broken_literal = header + binary_literal(b'wrong\x00result') + binary_literal(old)
            compare('final-binary-corrupt-literal-result', ['apply'], repo, env,
                    mutate=True, data=broken_literal, retain=('payload.bin',))
            compare('final-binary-corrupt-literal-result-am', ['am'], repo, env,
                    mutate=True, data=binary_mail(repo, env, broken_literal), retain=('payload.bin',))
            # The reverse hunk and OIDs must swap together.
            path.write_bytes(new)
            compare('final-binary-reverse-modification', ['apply', '-R'], repo, env,
                    mutate=True, data=patch)
            receiver = repo.parent / 'reverse-receiver'
            receiver.mkdir()
            setup(['init', '-q', '-b', 'main'], receiver, env)
            (receiver / 'payload.bin').write_bytes(new)
            setup(['add', '.'], receiver, env)
            setup(['commit', '-qm', 'reverse preimage only'], receiver, env)
            for label, reverse_patch in (('execute', patch),
                                         ('missing', header + binary_literal(new)),
                                         ('bodyless', summaries['full']),
                                         ('wrong-result', header + binary_literal(new) + binary_literal(b'wrong\x00'))):
                compare('final-binary-reverse-body-' + label, ['apply', '-R'], receiver, env,
                        mutate=True, data=reverse_patch,
                        retain=() if label == 'execute' else ('payload.bin',))
            # A postimage already in the object database takes precedence over
            # a validly encoded body, as in apply.c's has_object branch.
            path.write_bytes(new)
            setup(['hash-object', '-w', 'payload.bin'], repo, env)
            path.write_bytes(old)
            compare('final-binary-existing-postimage', ['apply'], repo, env,
                    mutate=True, data=broken_literal)
    write(repo, 'other.txt', 'plain text change\n')
    text_patch = base64.b64decode(run(['git', 'diff', '--full-index', '--', 'other.txt'], repo, env)['stdout_b64'])
    write(repo, 'other.txt', 'base\n')
    for prefix in (b'Files ', b'Binary files '):
        for args in (['apply', '--check'], ['apply']):
            compare('final-text-trailing-marker-' + prefix.decode().strip() + '-' + '-'.join(args),
                    args, repo, env, mutate=True, data=text_patch + prefix + b'x and y differ\n')


def final_objects(repo, env):
    write(repo, 'dir/file.txt', 'nested text\n')
    write(repo, 'dir/ignored.bin', 'nested binary\x00')
    write(repo, 'deep/dir/file.txt', 'deep text\n')
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'object walk fixture'], repo, env)
    for paths in (['keep.txt'], ['sub/'], ['dir/file.txt'], ['*.txt'], ['dir/*.txt'],
                  [':(attr:!__audit_absent__)*.txt'], [':(attr:text)*.txt'],
                  [':(glob)**/*.txt'], [':(literal)dir/file.txt'],
                  [':(top,attr:!__audit_absent__)dir/'],
                  ['*.txt', ':(exclude)dir/'], [':(icase)*.TXT'],
                  ['*.txt', ':(exclude,attr:text)'], ['dir/', ':(exclude,attr:missing)'],
                  [':(icase)DIR/*.TXT'], [':(attr:text)sub/']):
        for command in (['rev-list', '--objects', 'HEAD'], ['log', '--objects', '--format=%H', 'HEAD']):
            compare('final-objects-' + command[0] + '-' + repr(paths),
                    [*command, '--', *paths], repo, env)
    for paths in ([':(exclude)dir'], ['*', ':(glob,exclude)d*'],
                  ['*', ':(exclude)dir/'], ['*', ':(exclude)dir'],
                  [':(exclude,attr:text)']):
        compare('final-objects-exclusion-edge-' + repr(paths),
                ['rev-list', '--objects', 'HEAD', '--', *paths], repo, env)


def counts(repo, env, base):
    if MATCH and MATCH not in 'ordinary-config-output':
        return
    compare('ordinary-config-output', ['config', '--get', 'core.bare'], repo, env)
    candidate = Path(COMMAND[0]).resolve().parents[1]
    entry = candidate / 'lisp/ngit-main.el'
    if not entry.is_file():
        return
    source = entry.read_text().replace('(file-name-directory (or load-file-name buffer-file-name))',
                                       json.dumps(str(candidate / 'lisp') + '/'))
    injection = r'''
(let ((original (symbol-function 'ngit--read-file)))
 (fset 'ngit--read-file (lambda (path)
  (when (equal path (getenv "NGIT_COUNT_CONFIG"))
   (ngit-native-write-stderr "AUDIT-READ\n"))
  (funcall original path))))
(let ((original (symbol-function 'ngit--config-parse)))
 (fset 'ngit--config-parse (lambda (&rest args)
  (when (equal ngit--config-source ".git/config")
   (ngit-native-write-stderr "AUDIT-PARSE\n"))
  (apply original args))))
'''
    at = source.rindex('(let ((args (cdr (member "--" command-line-args))))')
    driver = base / 'count-driver.el'
    driver.write_text(source[:at] + injection + source[at:])
    result = run([os.environ.get('NGIT_NELISP', 'nelisp'), '--load', str(driver),
                  '--', '--@', '@config', '@--get', '@core.bare'], repo,
                 env | dict(NGIT_NO_ELC='1', NGIT_COUNT_CONFIG=str(repo / '.git/config')))
    stderr = base64.b64decode(result['stderr_b64'])
    reads, parses = stderr.count(b'AUDIT-READ\n'), stderr.count(b'AUDIT-PARSE\n')
    stripped = dict(result, stderr_b64=base64.b64encode(stderr.replace(b'AUDIT-READ\n', b'').replace(b'AUDIT-PARSE\n', b'')).decode())
    expected = run(['git', 'config', '--get', 'core.bare'], repo, env)
    equal = stripped == expected and 0 < reads <= 2 and 0 < parses <= 2
    REPORT['counts'] = dict(reads=reads, parses=parses, expected_max_parses=2, result=result)
    REPORT['failures'] += int(not equal)
    REPORT['assertions'] = REPORT.get('assertions', 0) + 1
    if not equal:
        print(f'FAIL ordinary config budget: reads={reads}, parses={parses}', file=sys.stderr)


def round4_paths(repo, env):
    """Invalid components and alternate spellings cannot hide symlinks."""
    zero = b'0' * 40

    def addition(path, contents, binary, mode=b'100644', headers=None):
        sha = hashlib.sha1(b'blob ' + str(len(contents)).encode() + b'\0' + contents).hexdigest().encode()
        head = (b'diff --git a/' + path + b' b/' + path + b'\nnew file mode ' + mode +
                b'\nindex ' + zero + b'..' + sha + b'\n')
        if binary:
            return head + b'GIT binary patch\n' + binary_literal(contents) + binary_literal(b'')
        return (head + b'--- /dev/null\n+++ b/' + (headers or path) +
                b'\n@@ -0,0 +1 @@\n+' + contents + b'\n\\ No newline at end of file\n')

    (repo / 'inside').mkdir()
    for binary in (False, True):
        for path in (b'./link', b'a/./b', b'a//b'):
            patch = addition(path, b'content', binary)
            for args in (['apply', '--check'], ['apply'], ['apply', '--index'], ['apply', '--cached'], ['am']):
                compare('round4-invalid-' + str(binary) + '-' + path.decode() + '-' + '-'.join(args),
                        args, repo, env, mutate=True,
                        data=binary_mail(repo, env, patch) if args == ['am'] else patch)
        for spelling in (b'./link', b'link', b'link//nested'):
            child = b'link/nested/child' if b'nested' in spelling else b'link/child'
            parts = [addition(spelling, b'inside', binary, b'120000'),
                     addition(child, b'child', binary)]
            for reverse in (False, True):
                patch = b''.join(reversed(parts) if reverse else parts)
                for args in (['apply', '--check'], ['apply'], ['am']):
                    compare('round4-alias-' + str(binary) + '-' + spelling.decode() + '-' +
                            str(reverse) + '-' + '-'.join(args), args, repo, env, mutate=True,
                            data=binary_mail(repo, env, patch) if args == ['am'] else patch)
    # Traditional find_name squashes repeated slashes before validation.
    patch = b'--- /dev/null\n+++ b/dir//file\n@@ -0,0 +1 @@\n+content\n'
    for args in (['apply', '--check'], ['apply'], ['apply', '--index'], ['am']):
        compare('round4-unified-slash-' + '-'.join(args), args, repo, env, mutate=True,
                data=binary_mail(repo, env, patch) if args == ['am'] else patch)


def round4_unified(repo, env):
    """Traditional names, fallback selection and explicit component counts."""
    for name in ('old', 'new', 'file', 'file.orig', 'dir/file', 'root/dir/file'):
        write(repo, name, 'before\n')
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'traditional preimages'], repo, env)
    cases = [
        ('addition', b'--- /dev/null\n+++ filename\n@@ -0,0 +1 @@\n+content\n'),
        ('different', b'--- old\n+++ new\n@@ -1 +1 @@\n-before\n+after\n'),
        ('shorter', b'--- file\n+++ file.orig\n@@ -1 +1 @@\n-before\n+after\n'),
        ('fallback', b'--- a/file\n+++ file\n@@ -1 +1 @@\n-before\n+after\n'),
        ('deletion', b'--- file\n+++ /dev/null\n@@ -1 +0,0 @@\n-before\n'),
        ('nested', b'--- root/dir/file\n+++ root/dir/file\n@@ -1 +1 @@\n-before\n+after\n'),
        ('quoted', b'--- "file"\n+++ "file.orig"\n@@ -1 +1 @@\n-before\n+after\n'),
        ('timestamp', b'--- file\t2023-11-14 22:13:20 +0000\n+++ file.orig\t2023-11-14 22:13:20 +0000\n@@ -1 +1 @@\n-before\n+after\n'),
    ]
    for name, patch in cases:
        for p in (None, 0, 1, 2, 3):
            for args in (['apply', '--check'], ['apply', '--index'], ['am']):
                # am did not support -p before this round; exercise its
                # default and all supported apply component counts.
                if args == ['am'] and p is not None:
                    continue
                argv = args + ([] if p is None else ['-p' + str(p)])
                # Restoring a snapshot changes inode/stat information.
                # Refresh before the oracle's --index precondition check.
                setup(['update-index', '--refresh'], repo, env)
                compare('round4-traditional-' + name + '-p' + str(p) + '-' + '-'.join(args),
                        argv, repo, env, mutate=True,
                        data=binary_mail(repo, env, patch) if args == ['am'] else patch)
    # Both headers are traditional patches in one input; the inferred -p
    # persists, and a subsequent file must start a fresh patch record.
    patch = (b'--- /dev/null\n+++ first\n@@ -0,0 +1 @@\n+one\n'
             b'--- /dev/null\n+++ second\n@@ -0,0 +1 @@\n+two\n')
    for args in (['apply'], ['apply', '--cached'], ['am']):
        compare('round4-traditional-multi-' + '-'.join(args), args, repo, env, mutate=True,
                data=binary_mail(repo, env, patch) if args == ['am'] else patch)


def round5_headers(repo, env):
    """Extended headers strip p-1 components; diff names strip p."""
    for name in ('file', 'dir/file', 'a/dir/file', 'space file', 'dir/space file'):
        write(repo, name, name + '\n')
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'extended header preimages'], repo, env)
    variants = ([], ['--check'], ['--index'], ['--index', '--check'],
                ['--cached'], ['--cached', '--check'])
    for p in (0, 1, 2):
        for kind in ('rename', 'copy', 'mode', 'rename-mode', 'copy-mode',
                     'rename-text', 'copy-text', 'rename-quoted', 'copy-quoted', 'mode-quoted'):
            quoted = kind.endswith('-quoted')
            old = 'dir/space file' if quoted else 'dir/file'
            new = old.replace('file', 'moved') if not kind.startswith('mode') else old
            def quote(name):
                return json.dumps(name) if quoted else name
            patch = 'diff --git ' + quote('a/' + old) + ' ' + quote('b/' + new) + '\n'
            if kind.startswith('mode'):
                patch += 'old mode 100644\nnew mode 100755\n'
            else:
                op = kind.split('-')[0]
                if kind.endswith('-mode'):
                    patch += 'old mode 100644\nnew mode 100755\n'
                patch += 'similarity index 100%\n' + op + ' from ' + quote(old) + '\n'
                patch += op + ' to ' + quote(new) + '\n'
                if kind.endswith('-text'):
                    source = old if p < 2 else 'file'
                    patch += ('--- a/' + old + '\n+++ b/' + new + '\n@@ -1 +1 @@\n-' +
                              source + '\n+changed\n')
            for opts in variants:
                setup(['update-index', '--refresh'], repo, env)
                compare('round5-headers-p' + str(p) + '-' + kind + '-' + '-'.join(opts),
                        ['apply', '-p' + str(p), *opts], repo, env, mutate=True,
                        data=patch.encode())
    patch = b'diff --git dir/file dir/file\nold mode 100644\nnew mode 100755\n'
    for opts in variants:
        setup(['update-index', '--refresh'], repo, env)
        compare('round5-headers-p0-mode-bare-' + '-'.join(opts),
                ['apply', '-p0', *opts], repo, env, mutate=True, data=patch)
    setup(['rm', '-q', 'dir/file'], repo, env)
    setup(['commit', '-qm', 'only stripped preimage remains'], repo, env)
    for kind in ('rename', 'copy'):
        patch = ('diff --git a/dir/file b/dir/moved\nsimilarity index 100%\n' +
                 kind + ' from dir/file\n' + kind + ' to dir/moved\n')
        for opts in (['--check'], ['--index', '--check'], ['--cached', '--check']):
            setup(['update-index', '--refresh'], repo, env)
            compare('round5-headers-leaf-only-' + kind + '-' + '-'.join(opts),
                    ['apply', '-p2', *opts], repo, env, mutate=True, data=patch.encode())


def round4_pull(repo, env):
    remote = repo.parent / 'remote'
    setup(['clone', '-q', str(repo), str(remote)], repo.parent, env)
    setup(['remote', 'add', 'origin', '../remote'], repo, env)
    setup(['config', 'branch.main.remote', 'origin'], repo, env)
    setup(['config', 'branch.main.merge', 'refs/heads/main'], repo, env)
    env = env | dict(GIT_SEQUENCE_EDITOR='sh -c \'echo called >sequence-called; exit 1\'')

    def matrix(shape):
        for word in ('interactive', 'merges'):
            forms = [
                ('cli-ff-only', [], ['--rebase=' + word, '--ff-only']),
                ('cli-ff-only-first', [], ['--ff-only', '--rebase=' + word]),
                ('pull-ff-only', ['pull.rebase=' + word], ['--ff-only']),
                ('branch-ff-only', ['branch.main.rebase=' + word], ['--ff-only']),
                ('config-ff-only', ['pull.ff=only', 'pull.rebase=' + word], []),
                ('branch-config-ff-only', ['pull.ff=only', 'branch.main.rebase=' + word], []),
                ('cli-no-rebase', [], ['--rebase=' + word, '--no-rebase']),
                ('branch-no-rebase', ['branch.main.rebase=' + word], ['--no-rebase']),
                ('branch-false', ['pull.rebase=' + word, 'branch.main.rebase=false'], []),
                ('config-ff-cli-rebase', ['pull.ff=only'], ['--rebase=' + word]),
                ('config-ff-cli-no-rebase', ['pull.ff=only'], ['--rebase=' + word, '--no-rebase']),
                ('last-ff-only', [], ['--rebase=' + word, '--no-ff', '--ff-only']),
                ('last-ff', [], ['--rebase=' + word, '--ff-only', '--ff']),
            ]
            for name, config, opts in forms:
                special = name in ('config-ff-cli-rebase', 'last-ff')
                refusal = word if special and (shape == 'divergent' or
                                               (shape == 'ahead' and word == 'interactive')) else None
                argv = [a for value in config for a in ('-c', value)] + ['pull', '-q', *opts]
                compare('round4-pull-' + shape + '-' + word + '-' + name,
                        argv, repo, env, mutate=True, refusal=refusal,
                        editor_exit=1 if refusal == 'interactive' else 0)

    matrix('equal')
    write(repo, 'ahead', 'local\n')
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'local ahead'], repo, env)
    matrix('ahead')
    # Requested rebase checks precede fetch even when ff-only later selects merge.
    # Keep FETCH_HEAD distinct so a premature fetch cannot pass a refusal case.
    (repo / '.git/FETCH_HEAD').write_text('precondition sentinel\n')
    for dirty in ('unstaged', 'staged', 'both'):
        write(repo, 'other.txt', 'staged change\n')
        if dirty != 'unstaged':
            setup(['add', 'other.txt'], repo, env)
        if dirty == 'both':
            write(repo, 'other.txt', 'unstaged change\n')
        for word in ('true', 'interactive', 'merges'):
            forms = [
                ('cli', [], ['--rebase=' + word, '--ff-only']),
                ('config', ['pull.rebase=' + word, 'pull.ff=only'], []),
                ('branch', ['branch.main.rebase=' + word, 'pull.ff=only'], []),
                ('config-auto', ['pull.rebase=' + word, 'pull.ff=only', 'rebase.autoStash=true'], []),
                ('cli-auto', ['rebase.autoStash=false'], ['--rebase=' + word, '--ff-only', '--autostash']),
                ('cli-no-auto', ['rebase.autoStash=true'], ['--rebase=' + word, '--ff-only', '--no-autostash']),
                ('merge-auto', ['merge.autoStash=true'], ['--rebase=' + word, '--ff-only']),
                ('last-no-auto', [], ['--rebase=' + word, '--ff-only', '--autostash', '--no-autostash']),
                ('last-auto', [], ['--rebase=' + word, '--ff-only', '--no-autostash', '--autostash']),
            ]
            for name, config, opts in forms:
                argv = [a for value in config for a in ('-c', value)] + ['pull', '-q', *opts]
                compare('round4-pull-dirty-' + dirty + '-' + word + '-' + name,
                        argv, repo, env, mutate=True)
        setup(['restore', '--source=HEAD', '--staged', '--worktree', 'other.txt'], repo, env)
    (repo / '.git/FETCH_HEAD').unlink()
    setup(['reset', '--hard', '-q', 'HEAD~1'], repo, env)
    write(remote, 'remote', 'remote\n')
    setup(['add', '.'], remote, env)
    setup(['commit', '-qm', 'remote ahead'], remote, env)
    matrix('fast-forward')
    write(repo, 'local', 'local\n')
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'local divergence'], repo, env)
    matrix('divergent')


def round5_autostash(repo, env):
    """Dirty fast-forwards exercise the selected rebase autostash setting."""
    remote = repo.parent / 'remote'
    setup(['clone', '-q', str(repo), str(remote)], repo.parent, env)
    setup(['remote', 'add', 'origin', '../remote'], repo, env)
    setup(['config', 'branch.main.remote', 'origin'], repo, env)
    setup(['config', 'branch.main.merge', 'refs/heads/main'], repo, env)
    write(remote, 'remote', 'upstream\n')
    setup(['add', '.'], remote, env)
    setup(['commit', '-qm', 'remote ahead'], remote, env)
    for dirty in ('unstaged', 'staged', 'both', 'same-size'):
        write(repo, 'other.txt', 'same\n' if dirty == 'same-size' else 'staged change\n')
        if dirty in ('staged', 'both'):
            setup(['add', 'other.txt'], repo, env)
        if dirty == 'both':
            write(repo, 'other.txt', 'unstaged change\n')
        for word in ('true', 'interactive', 'merges'):
            for name, config, opts in (
                    ('config', ['pull.rebase=' + word, 'pull.ff=only', 'rebase.autoStash=true'], []),
                    ('cli', ['rebase.autoStash=false'], ['--rebase=' + word, '--ff-only', '--autostash']),
                    *([('loud', [], ['--rebase=true', '--ff-only', '--autostash'])] if word == 'true' else []),
                    ('disabled', ['rebase.autoStash=true'], ['--rebase=' + word, '--ff-only', '--no-autostash'])):
                argv = [a for value in config for a in ('-c', value)] + ['pull', *([] if name == 'loud' else ['-q']), *opts]
                compare('round5-autostash-' + dirty + '-' + word + '-' + name,
                        argv, repo, env, mutate=True)
        setup(['restore', '--source=HEAD', '--staged', '--worktree', 'other.txt'], repo, env)
    write(remote, 'other.txt', 'upstream conflict\n')
    setup(['add', '.'], remote, env)
    setup(['commit', '-qm', 'conflicting upstream'], remote, env)
    for dirty in ('unstaged', 'staged', 'both'):
        write(repo, 'other.txt', 'staged change\n')
        if dirty != 'unstaged':
            setup(['add', 'other.txt'], repo, env)
        if dirty == 'both':
            write(repo, 'other.txt', 'unstaged change\n')
        for name, config, opts in (
                ('config', ['pull.rebase=interactive', 'pull.ff=only', 'rebase.autoStash=true'], []),
                ('cli', [], ['--rebase=merges', '--ff-only', '--autostash'])):
            argv = [a for value in config for a in ('-c', value)] + ['pull', '-q', *opts]
            compare('round5-autostash-conflict-' + dirty + '-' + name,
                    argv, repo, env, mutate=True)
        setup(['restore', '--source=HEAD', '--staged', '--worktree', 'other.txt'], repo, env)
    setup(['symbolic-ref', 'HEAD', 'refs/heads/unborn'], repo, env)
    for word in ('true', 'interactive', 'merges'):
        for opt in ('--autostash', '--no-autostash'):
            compare('round5-autostash-unborn-' + word + '-' + opt,
                    ['pull', '-q', '--rebase=' + word, '--ff-only', opt], repo, env, mutate=True)


def main(mode):
    final_modes = dict(pull=final_pull, attrs=final_attrs, binary=final_binary, objects=final_objects,
                       **{'round4-paths': round4_paths, 'round4-unified': round4_unified,
                          'round4-pull': round4_pull, 'round5-headers': round5_headers,
                          'round5-autostash': round5_autostash})
    if mode not in ('core', 'special', 'boundaries', 'e2e', *final_modes):
        raise ValueError('expected core, special, boundaries, e2e, pull, attrs, binary or objects')
    with tempfile.TemporaryDirectory(prefix='ngit-reaudit-') as tmp:
        base = Path(tmp)
        repo, env = fixture(base)
        if mode in final_modes:
            final_modes[mode](repo, env)
        elif mode == 'core':
            counts(repo, env, base)
            revisions(repo, env)
            mixed(repo, env)
            config(repo, env)
            symlinks(repo, env)
        elif mode == 'special':
            special(repo, env)
        elif mode == 'boundaries':
            boundaries(repo, env)
        else:
            write(repo, 'other.txt', 'release\n')
            compare('e2e-stage', ['add', '--', '*.txt', ':(exclude,attr:text)'], repo, env, mutate=True, keep=True)
            compare('e2e-review', ['diff', '--cached', '--name-only'], repo, env)
            compare('e2e-commit', ['commit', '-q', '-m', 'release'], repo, env, mutate=True, keep=True)
            compare('e2e-history', ['-c', 'diff.renames=copies', 'log', '-1', '--format=%s'], repo, env)
            compare('e2e-rev-list', ['rev-list', 'HEAD', '--', ':(attr:!__audit_absent__)'], repo, env)
            compare('e2e-shortlog', ['shortlog', '-s', 'HEAD', '--', ':(attr:!__audit_absent__)'], repo, env)
            compare('e2e-config', ['config', '--get', 'audit.value'], repo,
                    env | dict(GIT_CONFIG_PARAMETERS="'audit.value'='before'\r'audit.value'='release'"))
            compare('e2e-final-status', ['status', '--porcelain'], repo, env, mutate=True, keep=True)
    if not REPORT['comparisons']:
        raise RuntimeError('zero selected comparisons')


mode = sys.argv[1] if len(sys.argv) > 1 else 'core'
try:
    main(mode)
except Exception as error:
    REPORT['unexpected_errors'] += 1
    print(f'UNEXPECTED: {error}', file=sys.stderr)
finally:
    if REPORT['exact'] + len(REPORT['intended']) != REPORT['comparisons']:
        REPORT['unexpected_errors'] += 1
    REPORT['seconds'] = round(time.monotonic() - START, 3)
    directory = Path(os.environ.get('NGIT_REPORT_DIR', ROOT / 'target/include-pathspec/reports'))
    directory.mkdir(parents=True, exist_ok=True)
    (directory / f'audit-reaudit-{mode}.json').write_text(json.dumps(REPORT, indent=2) + '\n')
print(json.dumps({k: REPORT[k] for k in ('comparisons', 'exact', 'intended', 'failures', 'unexpected_errors', 'seconds')}))
sys.exit(bool(REPORT['failures'] or REPORT['unexpected_errors']))
