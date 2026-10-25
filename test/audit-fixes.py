#!/usr/bin/env python3
"""Exact Git controls for clean traversal boundaries and combined diff formats."""
import base64
import hashlib
import itertools
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import signal
import stat
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
COMMAND = shlex.split(os.environ.get('NGIT_COMPAT_COMMAND', str(ROOT / 'bin/ngit')))
MATCH = os.environ.get('NGIT_COMPAT_MATCH', '')
START = time.monotonic()
DEADLINE = START + 530
REPORT = dict(comparisons=0, exact=0, intended={}, failures=0, unexpected_errors=0,
              assertions=0, cases=[], command=COMMAND, match=MATCH)
BAD = re.compile(rb'void-function|void-variable|wrong-type-argument|Lisp error|[Ll]oader error|Cannot open load file|[Oo]ut of memory|resource exhausted')


def run(command, cwd, env, data=None):
    budget = min(45, DEADLINE - time.monotonic())
    if budget <= 0:
        raise RuntimeError('overall timeout')
    process = subprocess.Popen(command, cwd=cwd, env=env, stdin=subprocess.PIPE if data is not None else None, stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, start_new_session=True)
    try:
        out, err = process.communicate(input=data, timeout=budget)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGKILL)
        process.communicate()
        raise RuntimeError(f'command timeout: {command}')
    return dict(exit=process.returncode, stdout_b64=base64.b64encode(out).decode(),
                stderr_b64=base64.b64encode(err).decode())


def setup(args, repo, env):
    result = run(['git', *args], repo, env)
    if result['exit']:
        raise RuntimeError(f'fixture setup failed: {args}: {result}')


def state(repo, env):
    """Logical refs/index plus exact worktree/config bytes, modes and directories."""
    files = {}
    for path in sorted(repo.rglob('*')):
        rel = path.relative_to(repo)
        if '.git' in rel.parts and rel.parts[0] == '.git':
            if path.name != 'config':
                continue
        mode = path.lstat().st_mode
        if stat.S_ISLNK(mode):
            data = os.readlink(path)
        elif stat.S_ISREG(mode):
            data = hashlib.sha256(path.read_bytes()).hexdigest()
        else:
            data = None
        files[str(rel)] = [stat.S_IFMT(mode), stat.S_IMODE(mode), data]
    return dict(refs=run(['git', 'show-ref', '--head'], repo, env),
                index=run(['git', 'ls-files', '--stage'], repo, env), files=files)


def compare(name, args, repo, env, sub='', mutate=False, outside=False, retain=()):
    if MATCH and MATCH not in name:
        return
    before = state(repo, env)
    backup = repo.parent / 'snapshot'
    if mutate:
        shutil.copytree(repo, backup, symlinks=True)
    expected = run(['git', *args], repo / sub, env)
    expected_state = state(repo, env)
    if mutate:
        shutil.rmtree(repo)
        shutil.copytree(backup, repo, symlinks=True)
        shutil.rmtree(backup)
    actual = run([*COMMAND, *args], repo / sub, env)
    actual_state = state(repo, env)
    unexpected = actual['exit'] < 0 or bool(BAD.search(base64.b64decode(actual['stdout_b64']) +
                                                    base64.b64decode(actual['stderr_b64'])))
    state_equal = expected_state == actual_state
    outside_equal = True
    outside_scope = (sub if outside is True else outside) if outside else None
    if outside:
        def retained(snapshot):
            return {key: value for key, value in snapshot['files'].items()
                    if not key.startswith(outside_scope + '/') and key != outside_scope}
        outside_equal = retained(before) == retained(actual_state)
        REPORT['assertions'] += 1
    retained_equal = all(before['files'].get(path) == actual_state['files'].get(path) for path in retain)
    REPORT['assertions'] += int(bool(retain))
    equal = expected == actual and state_equal and outside_equal and retained_equal
    REPORT['comparisons'] += 1
    REPORT['exact'] += int(equal and not unexpected)
    REPORT['failures'] += int(not equal or unexpected)
    REPORT['unexpected_errors'] += int(unexpected)
    REPORT['cases'].append(dict(name=name, argv=args, subdirectory=sub, expected=expected,
                                actual=actual, state_equal=state_equal,
                                expected_state=expected_state if mutate else None,
                                actual_state=actual_state if mutate else None,
                                outside_unchanged=outside_equal if outside else None, outside_scope=outside_scope,
                                retained_paths=list(retain), retained_unchanged=retained_equal))
    if not equal or unexpected:
        print(f'FAIL {name}: {expected} != {actual}; state={state_equal}; outside={outside_equal}',
              file=sys.stderr, flush=True)
    if mutate and name.startswith('clean-'):
        # Each destructive matrix case starts from the same complete fixture.
        # Preserve the actual result only for the sequential e2e workflow.
        shutil.rmtree(repo)
        shutil.copytree(BASELINE, repo, symlinks=True)


def write(repo, name, contents):
    path = repo / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(contents if isinstance(contents, bytes) else contents.encode())


def fixture(repo, env):
    repo.mkdir()
    setup(['init', '-q', '-b', 'main'], repo, env)
    setup(['config', 'user.name', 'Audit Fixture'], repo, env)
    setup(['config', 'user.email', 'audit@example.test'], repo, env)
    for name, data in {'.gitignore': 'build/\ncache/\n*.tmp\n',
                       '.gitattributes': '* text\n', 'docs/tracked': 'before\n',
                       'docs/removed': 'removed\n', 'docs/mode': 'mode\n',
                       'docs/old': 'rename\n', 'docs/binary': b'old\0binary', 'root-tracked': 'root\n',
                       'docs/sub/deep-tracked': 'deep\n'}.items():
        write(repo, name, data)
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'initial'], repo, env)
    for name in ['build/secret', 'Docs/build/secret', 'other/build/secret',
                 'docs/cache/keep', 'docs/cache/selected.tmp', 'docs/cache/inner/work.tmp',
                 'docs/build/out', 'docs/plain/file', 'docs/untracked',
                 'docs/sub/cache/deep.tmp', 'docs/sub/build/out',
                 'docs/sub/plain/file', 'docs/sub/untracked', 'outside.tmp',
                 'docs/loose.tmp', 'docs/sub/deep.tmp', 'docs/star*']:
        write(repo, name, name + '\n')


def clean_cases(repo, env):
    forms = [('.', ['.']), ('ordinary', ['*']), ('implicit-glob', ['*.tmp']),
             ('literal', [':(literal)untracked']), ('literal-star', [':(literal)star*']),
             ('glob', [':(glob)*']), ('glob-recursive', [':(glob)**']),
             ('icase', [':(icase)*']), ('icase-glob', [':(icase,glob)*']),
             ('attr', [':(attr:text)*']), ('icase-attr', [':(icase,attr:text)*']),
             ('glob-attr', [':(glob,attr:text)*']),
             ('literal-attr', [':(literal,attr:text)cache/selected.tmp']),
             ('exclude', ['*', ':!cache']),
             ('icase-attr-exclude', [':(icase,attr:text)*', ':!cache']),
             ('exclude-only', [':!cache']),
             ('explicit-parent', ['../build']), ('top', [':(top)build']),
             ('top-icase-attr', [':(top,icase,attr:text)build']),
             ('ancestor', [':(attr:text)cache/selected.tmp']),
             ('icase-ancestor', [':(icase,attr:text)cache/selected.tmp'])]
    for sub in ['docs', 'docs/sub']:
        for label, paths in forms:
            for flags in (['-n', '-nd', '-nfdx', '-ndX'] if sub == 'docs' else ['-ndX']):
                compare(f'clean-{sub}-{label}-{flags}', ['clean', flags, '--', *paths],
                        repo, env, sub=sub)
        for label, paths in forms:
            if label in ('explicit-parent', 'top', 'top-icase-attr'):
                continue
            compare(f'clean-{sub}-{label}-destructive', ['clean', '-fdX', '--', *paths],
                    repo, env, sub=sub, mutate=True, outside=True)
    for flags in ['-nd', '-ndx', '-ndX', '-fdX']:
        compare(f'clean-default-docs-{flags}', ['clean', flags], repo, env, sub='docs',
                mutate='n' not in flags, outside=True)
    # Git's treat_leading_path selects the ignored cache/ ancestor whole:
    # it removes ../keep and ../selected.tmp as well as inner/work.tmp, then
    # refuses to remove the cwd.  Compare that exact state; protect everything
    # outside docs here.  Normal docs and docs/sub cases above strictly protect
    # everything outside their invocation directory.
    REPORT['oracle_notes'] = [dict(case='clean-deep-ignored-ancestor',
        note='Git itself removes siblings under an ignored invocation ancestor; '
             'this case compares exact Git state and protects paths outside docs.')]
    compare('clean-deep-ignored-ancestor', ['clean', '-fdX', '--', ':(icase,attr:text)*'],
            repo, env, sub='docs/cache/inner', mutate=True, outside='docs')


def diff_cases(repo, env):
    write(repo, 'docs/binary', b'new\0binary bytes')
    write(repo, 'docs/tracked', 'after\nextra\n')
    (repo / 'docs/removed').unlink()
    os.chmod(repo / 'docs/mode', 0o755)
    (repo / 'docs/old').rename(repo / 'docs/renamed')
    write(repo, 'docs/created', 'created\n')
    setup(['add', '-A', 'docs'], repo, env)
    formats = ['--stat', '--numstat', '--shortstat', '--summary']
    variants = [[flag] for flag in formats]
    variants += [list(pair) for pair in itertools.permutations(formats, 2)]
    variants += [formats, list(reversed(formats)),
                 ['--stat', '--stat', '--numstat'], ['--summary', '-p'],
                 ['-p', '--summary'], ['--stat', '-p'], ['-p', '--stat'],
                 ['--patch-with-stat'], ['--stat', '--no-patch'],
                 ['--stat', '-s', '--numstat'], ['-s', '--stat'],
                 ['--stat', '-s', '-p'], ['--stat', '--no-patch', '--summary'],
                 ['--summary', '--patch', '--numstat'], ['--summary', '-U0'],
                 ['--stat', '--numstat', '--shortstat', '--summary', '-p']]
    for name in ['--name-only', '--name-status']:
        for flag in formats + ['-p', '--patch']:
            variants += [[name, flag], [flag, name]]
    variants += [['--name-only', '--name-status'], ['--name-status', '--name-only'],
                 ['-s', '--name-only'], ['--name-status', '-s'],
                 ['--name-only', '-s', '--stat']]
    for i, flags in enumerate(variants):
        compare(f'diff-cached-{i}-' + '-'.join(flags), ['diff', '--cached', *flags], repo, env)
    # Worktree bytes are not yet objects, and display paths can be relative.
    write(repo, 'docs/tracked', 'working\n')
    for i, flags in enumerate([['--stat', '--numstat'], ['--numstat', '--stat'],
                               ['--shortstat', '--stat'], ['--summary', '-p'],
                               ['--summary', '--stat', '--numstat', '--shortstat', '-p'],
                               ['--name-only', '--stat'], ['--summary', '--relative'],
                               ['--stat', '--relative', '--numstat']]):
        compare(f'diff-worktree-{i}', ['diff', *flags], repo, env, sub='docs')
    # Empty queues do not produce separators or summary lines.
    for i, flags in enumerate([['--stat', '--numstat', '--shortstat', '--summary', '-p'],
                               ['--summary'], ['--name-only', '--stat']]):
        compare(f'diff-empty-{i}', ['diff', '--cached', *flags, '--', 'root-tracked'], repo, env)

    # Construct real index stages without depending on merge output or clocks.
    stages = []
    for number, content in enumerate([b'base\n', b'ours\n', b'theirs\n'], 1):
        result = run(['git', 'hash-object', '-w', '--stdin'], repo, env, data=content)
        if result['exit']:
            raise RuntimeError(f'conflict blob failed: {result}')
        oid = base64.b64decode(result['stdout_b64']).strip().decode()
        stages.append(f'100644 {oid} {number}\tconflict\n')
    result = run(['git', 'update-index', '--index-info'], repo, env,
                 data=''.join(stages).encode())
    if result['exit']:
        raise RuntimeError(f'conflict index failed: {result}')
    write(repo, 'conflict', 'resolved\n')
    for i, flags in enumerate([['--stat', '--numstat', '--shortstat', '-p'],
                               ['--numstat', '--stat', '--summary'],
                               ['--summary', '--stat', '-p'],
                               ['--name-only', '--stat', '-p']]):
        compare(f'diff-unmerged-{i}', ['diff', *flags], repo, env)


def e2e(repo, env):
    compare('e2e-preview-scoped-cache', ['clean', '-ndX', '--', ':(icase,attr:text)*'],
            repo, env, sub='docs')
    compare('e2e-remove-scoped-cache', ['clean', '-fdX', '--', ':(icase,attr:text)*'],
            repo, env, sub='docs', mutate=True, outside=True)
    write(repo, 'docs/tracked', 'release\nupdated\n')
    (repo / 'docs/old').rename(repo / 'docs/renamed')
    write(repo, 'docs/created', 'release artifact\n')
    (repo / 'docs/removed').unlink()
    os.chmod(repo / 'docs/mode', 0o755)
    compare('e2e-review-unstaged', ['diff', '--stat', '--numstat', '--summary', '-p'], repo, env)
    compare('e2e-stage-release', ['add', '-A', 'docs'], repo, env, mutate=True)
    compare('e2e-review-staged', ['diff', '--cached', '--summary', '--numstat', '--stat', '-p'], repo, env)
    compare('e2e-ci-file-list', ['diff', '--cached', '--summary', '--stat', '--name-status'], repo, env)
    compare('e2e-final-working-tree', ['diff', '--stat', '--numstat', '--shortstat', '--summary', '-p'], repo, env)
    compare('e2e-final-index', ['ls-files', '--stage'], repo, env)
    REPORT['workflow'] = ['preview cache', 'remove docs cache', 'edit release files',
                          'review worktree', 'stage', 'review staged', 'list changes', 'verify final state']
    REPORT['final_state'] = state(repo, env)


def prefix_fixture(repo, env):
    global BASELINE
    prefixes = [('star', 'docs*'), ('question', 'docs?'),
                ('bracket', 'docs[ab]'), ('backslash', 'docs\\name')]
    for name in [value for _, value in prefixes] + ['docsOther', 'docsa', 'docsname']:
        for file in ['own.txt', 'extra.log', 'keep.txt', 'nested/deep.txt',
                     '!before.txt', '*.txt', 'z-after.txt',
                     'box[ab]/leaf', 'boxa/leaf']:
            write(repo, name + '/' + file, name + '/' + file + '\n')
    setup(['add', '-A'], repo, env)
    setup(['commit', '-qm', 'literal invocation prefixes'], repo, env)
    for name in [value for _, value in prefixes] + ['docsOther', 'docsa', 'docsname']:
        for file in ['f', 'keep.tmp', 'build/secret', 'cache/secret', 'nested/loose.tmp']:
            write(repo, name + '/' + file, name + '/' + file + '\n')
    shutil.rmtree(BASELINE)
    shutil.copytree(repo, BASELINE, symlinks=True)
    return prefixes


def prefix_cases(repo, env):
    forms = [('ordinary', ['*']), ('implicit-glob', ['*.tmp']),
             ('literal', [':(literal)f']), ('glob', [':(glob)**']),
             ('glob-one', [':(glob)*']), ('icase', [':(icase)*']),
             ('attr', [':(attr:text)*']), ('icase-attr', [':(icase,attr:text)*']),
             ('exclude', ['*', ':!f']), ('exclude-only', [':!f']),
             ('top', [':(top)docsOther/*']), ('parent', ['../docsOther/*'])]
    prefixes = prefix_fixture(repo, env)
    REPORT['prefix_boundary'] = 'Invocation metacharacters are literal; explicit top/../ selectors may reach siblings.'
    for label, sub in prefixes:
        for form, paths in forms:
            for flags in ['-nd', '-ndx', '-ndX']:
                compare(f'prefix-{label}-{form}-{flags}', ['clean', flags, '--', *paths],
                        repo, env, sub=sub, outside=form not in ('top', 'parent'))
            if form not in ('top', 'parent'):
                for flags in ['-fdX', '-fdx']:
                    compare(f'clean-prefix-{label}-{form}-{flags}',
                            ['clean', flags, '--', *paths], repo, env,
                            sub=sub, mutate=True, outside=True)


def prefix_rm_cases(repo, env):
    prefixes = prefix_fixture(repo, env)
    for label, sub in prefixes:
        for index, paths in enumerate([['*.txt', ':!keep.txt'],
                                       ['*.txt', ':(glob)nested/*.txt', ':!keep.txt'],
                                       ['../' + sub + '/own.txt', ':(exclude)keep.txt'],
                                       ['*.txt'], ['*.txt', ':(exclude)keep.txt'],
                                       ['../docsOther/*.txt', ':!keep.txt'],
                                       [':(glob)*.txt', ':!keep.txt'],
                                       [':(icase)*.TXT', ':!keep.txt'],
                                       ['*', ':!*.txt'], ['nested', ':!keep.txt'],
                                       [':(glob)../' + sub + '/own.txt', ':!keep.txt'],
                                       [':(literal)../' + sub + '/own.txt', ':!keep.txt'],
                                       ['../' + sub + '/own.txt'],
                                       [':(glob)../' + sub + '/own.txt'],
                                       [':(literal)../' + sub + '/own.txt'],
                                       ['box[ab]'], ['box[ab]', ':!keep.txt']]):
            compare(f'prefix-{label}-mixed-rm-{index}', ['rm', '-n', '--', *paths],
                    repo, env, sub=sub, outside=True)
        compare(f'prefix-{label}-literal-rm', ['rm', '-n', '--', 'own.txt'],
                repo, env, sub=sub, outside=True)
        compare(f'prefix-{label}-recursive-glob-rm', ['rm', '-rn', '--', 'box[ab]'],
                repo, env, sub=sub, outside=True)
    # Destructive mixed ordinary/magic rm verifies refs, index and worktree,
    # including every sibling that the metacharacters could otherwise match.
    for label, sub in prefixes:
        compare(f'prefix-{label}-destructive-rm',
                ['rm', '--', '*.txt', ':!keep.txt'], repo, env,
                sub=sub, mutate=True, outside=True)


def prefix_e2e(repo, env):
    prefix_fixture(repo, env)
    sub = 'docs*'
    compare('prefix-e2e-preview', ['clean', '-ndX', '--', '*'], repo, env, sub=sub, outside=True)
    compare('prefix-e2e-remove-ignored', ['clean', '-fdX', '--', ':(attr:text)*'],
            repo, env, sub=sub, mutate=True, outside=True)
    write(repo, sub + '/own.txt', 'release update\n')
    compare('prefix-e2e-stage-literal', ['add', '--', 'own.txt'],
            repo, env, sub=sub, mutate=True, outside=True)
    compare('prefix-e2e-remove-mixed', ['rm', '--', '*.txt', ':!keep.txt'],
            repo, env, sub=sub, mutate=True, outside=True)
    compare('prefix-e2e-review', ['diff', '--cached', '--stat', '--numstat', '--summary', '-p'],
            repo, env, sub=sub, outside=True)
    compare('prefix-e2e-final-index', ['ls-files', '--stage'], repo, env)
    REPORT['workflow'] = ['preview scoped ignored files', 'delete ignored files',
                          'edit and stage literal path', 'remove mixed pathspecs',
                          'review staged changes', 'verify sibling and index state']
    REPORT['final_state'] = state(repo, env)


def prefix_boundary_cases(repo, env):
    prefix_fixture(repo, env)
    for label, sub in [('plain', 'docs'), ('star', 'docs*')]:
        forms = [('dot', '.'), ('explicit-dot', ':().'), ('empty-magic', ':()'),
                 ('empty-attr', ':(attr:text)'), ('normalized', 'nested/..'),
                 ('normalized-attr', ':(attr:text)nested/..'),
                 ('parent', '..'), ('top', ':(top)docsOther')]
        for form, path in forms:
            for flags in ['-ndX', '-ndx']:
                compare(f'prefix-boundary-{label}-{form}-{flags}',
                        ['clean', flags, '--', path], repo, env, sub=sub,
                        outside=form not in ('parent', 'top'))
            if form not in ('parent', 'top'):
                compare(f'clean-prefix-boundary-{label}-{form}-destructive',
                        ['clean', '-fdX', '--', path], repo, env,
                        sub=sub, mutate=True, outside=True)


def prefix_untracked_cases(repo, env):
    prefix_fixture(repo, env)
    for sub in ['loose', 'loose*', 'looseOther']:
        for file in ['f', 'build/secret', 'only/build/secret', 'only/keep']:
            write(repo, sub + '/' + file, sub + '/' + file + '\n')
    with (repo / '.gitattributes').open('a') as stream:
        stream.write('loose*/only/keep -text\n')
    shutil.rmtree(BASELINE)
    shutil.copytree(repo, BASELINE, symlinks=True)
    REPORT['oracle_notes'] = [dict(case='prefix-untracked-attr', note=
        'Git may delete an unignored keep file inside a selected directory when '
        'attribute matching hides that child. Exact Git state is compared; '
        'all sibling paths outside the literal invocation directory are protected.')]
    # Git may collapse only/ after attr matching hides its nonignored keep
    # file.  Exact state comparison records that oracle behavior.  Explicit
    # retain assertions apply to the non-attr forms; all forms protect siblings
    # outside the literal invocation directory.
    forms = [('ordinary', '*'), ('glob', ':(glob)**'), ('glob-one', ':(glob)*'),
             ('icase-attr', ':(icase,attr:text)*'), ('attr', ':(attr:text)*'), ('dot', '.'),
             ('only', 'only*'), ('only-glob', ':(glob)only*'),
             ('only-attr', ':(attr:text)only*'), ('only-icase', ':(icase,attr:text)only*')]
    for label, sub in [('plain', 'loose'), ('star', 'loose*')]:
        for form, path in forms:
            for flags in ['-nd', '-ndx', '-ndX']:
                compare(f'prefix-untracked-{label}-{form}-{flags}',
                        ['clean', flags, '--', path], repo, env, sub=sub, outside=True)
            for flags in ['-fdX', '-fdx', '-fd']:
                compare(f'clean-prefix-untracked-{label}-{form}-{flags}',
                        ['clean', flags, '--', path], repo, env,
                        sub=sub, mutate=True, outside=True,
                        retain=([sub + '/build/secret', sub + '/only/build/secret'] +
                                ([sub + '/only/keep'] if form in ('glob-one', 'only-glob') else []))
                               if flags == '-fd' else
                               ([sub + '/only/keep'] if flags == '-fdX' and form not in
                                ('attr', 'icase-attr', 'only-attr', 'only-icase') else ()))


def main(mode):
    global BASELINE
    if mode not in ('core', 'e2e', 'prefix', 'prefix-e2e', 'prefix-boundary', 'prefix-rm', 'prefix-untracked'):
        raise ValueError('expected core, e2e, prefix, prefix-e2e, prefix-boundary, prefix-rm or prefix-untracked')
    with tempfile.TemporaryDirectory(prefix='ngit-audit-fixes-') as tmp:
        base = Path(tmp)
        home = base / 'home'
        home.mkdir()
        env = {key: value for key, value in os.environ.items() if not key.startswith('GIT_')}
        env.update(HOME=str(home), XDG_CONFIG_HOME=str(home / 'xdg'), LC_ALL='C', TZ='UTC',
                   GIT_CONFIG_NOSYSTEM='1', GIT_AUTHOR_DATE='1700000000 +0000',
                   GIT_COMMITTER_DATE='1700000000 +0000')
        repo = base / 'repo'
        fixture(repo, env)
        BASELINE = base / 'baseline'
        shutil.copytree(repo, BASELINE, symlinks=True)
        if mode == 'core':
            clean_cases(repo, env)
            diff_cases(repo, env)
        elif mode == 'e2e':
            e2e(repo, env)
        elif mode == 'prefix':
            prefix_cases(repo, env)
        elif mode == 'prefix-e2e':
            prefix_e2e(repo, env)
        elif mode == 'prefix-boundary':
            prefix_boundary_cases(repo, env)
        elif mode == 'prefix-rm':
            prefix_rm_cases(repo, env)
        else:
            prefix_untracked_cases(repo, env)


if __name__ == '__main__':
    mode = sys.argv[1] if len(sys.argv) > 1 else 'core'
    try:
        main(mode)
    except Exception as error:
        REPORT['unexpected_errors'] += 1
        REPORT['failures'] += 1
        REPORT['error'] = repr(error)
        print(repr(error), file=sys.stderr)
    REPORT['seconds'] = round(time.monotonic() - START, 3)
    report_dir = Path(os.environ.get('NGIT_REPORT_DIR') or ROOT / 'target/include-pathspec/reports')
    report_dir.mkdir(parents=True, exist_ok=True)
    path = report_dir / f'audit-fixes-{mode}.json'
    path.write_text(json.dumps(REPORT, indent=2) + '\n')
    print(json.dumps({key: REPORT[key] for key in
                      ('comparisons', 'exact', 'intended', 'failures', 'unexpected_errors', 'seconds')}), flush=True)
    print(path)
    sys.exit(int(REPORT['failures'] != 0))
