#!/usr/bin/env python3
"""Compare subdirectory path selection, display and mutations with Git."""
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
COMMAND = shlex.split(os.environ.get('NGIT_COMPAT_COMMAND', str(ROOT / 'bin/ngit')))
MATCH = os.environ.get('NGIT_COMPAT_MATCH', '')
START = time.monotonic()
DEADLINE = START + 525
REPORT = dict(comparisons=0, exact=0, intended={}, failures=0, unexpected_errors=0, cases=[])
BAD = re.compile(rb'void-function|void-variable|wrong-type-argument|Lisp error|[Ll]oader error|Cannot open load file|[Oo]ut of memory|resource exhausted|Wrong type argument|Symbol.s (?:function|value) definition is void', re.I)


def run(command, cwd, env):
    budget = min(35, DEADLINE - time.monotonic())
    if budget <= 0:
        raise RuntimeError('overall deadline')
    proc = subprocess.Popen(command, cwd=cwd, env=env, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, start_new_session=True)
    try:
        out, err = proc.communicate(timeout=budget)
    except subprocess.TimeoutExpired:
        os.killpg(proc.pid, signal.SIGKILL)
        proc.communicate()
        raise RuntimeError(f'command timeout: {command}')
    return dict(exit=proc.returncode, stdout=out.decode('utf-8', 'surrogateescape'),
                stderr=err.decode('utf-8', 'surrogateescape'))


def setup(args, repo, env):
    result = run(['git', *args], repo, env)
    if result['exit']:
        raise RuntimeError(f'fixture: {args}: {result}')


def state(repo, env):
    commands = [['show-ref', '--head'], ['ls-files', '--stage'],
                ['reflog', 'show', '--all', '--format=%H %gs'], ['config', '--local', '--list']]
    logical = [run(['git', *args], repo, env) for args in commands]
    files = {}
    for path in sorted(repo.rglob('*')):
        rel = path.relative_to(repo)
        if rel.parts[0] == '.git':
            continue
        if path.is_symlink():
            files[str(rel)] = ['link', os.readlink(path)]
        elif path.is_file():
            files[str(rel)] = [path.stat().st_mode & 0o777, base64.b64encode(path.read_bytes()).decode()]
        elif path.is_dir():
            files[str(rel)] = ['directory']
    return logical, files


def compare(name, args, repo, env, cwd='d', mutate=False):
    if MATCH and MATCH not in name:
        return
    backup = repo.parent / 'snapshot'
    if mutate:
        shutil.copytree(repo, backup, symlinks=True)
    expected = run(['git', *args], repo / cwd, env)
    expected_state = state(repo, env) if mutate else None
    if mutate:
        shutil.rmtree(repo)
        shutil.copytree(backup, repo, symlinks=True)
        shutil.rmtree(backup)
    actual = run([*COMMAND, *args], repo / cwd, env)
    actual_state = state(repo, env) if mutate else None
    unexpected = actual['exit'] < 0 or bool(BAD.search((actual['stdout'] + actual['stderr']).encode('utf-8', 'surrogateescape')))
    equal = expected == actual and expected_state == actual_state and not unexpected
    REPORT['comparisons'] += 1
    REPORT['exact'] += int(equal)
    REPORT['failures'] += int(not equal)
    REPORT['unexpected_errors'] += int(unexpected)
    REPORT['cases'].append(dict(name=name, argv=args, cwd=cwd, expected=expected, actual=actual,
                                state_equal=expected_state == actual_state))
    if not equal:
        print(f'FAIL {name}: {expected!r} != {actual!r}; state={expected_state == actual_state}', flush=True)


def fixture(base, env):
    repo = base / 'repo'
    if repo.exists():
        shutil.rmtree(repo)
    repo.mkdir()
    setup(['init', '-q', '-b', 'main'], repo, env)
    setup(['config', 'user.name', 'Fixture'], repo, env)
    setup(['config', 'user.email', 'fixture@example.test'], repo, env)
    (repo / 'd/e').mkdir(parents=True)
    for path in ['top', 'd/f', 'd/e/g', 'd/h']:
        (repo / path).write_text('before\n')
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'base'], repo, env)
    for path in ['top', 'd/f', 'd/e/g']:
        (repo / path).write_text('after\n')
    (repo / 'd/new').write_text('new\n')
    (repo / 'outer').write_text('outer\n')
    return repo


def main(mode):
    if mode not in ('inspect', 'mutate', 'boundaries', 'e2e'):
        raise RuntimeError('expected inspect, mutate, boundaries or e2e')
    with tempfile.TemporaryDirectory(prefix='ngit-subdir-commands-') as tmp:
        base = Path(tmp)
        home = base / 'home'
        home.mkdir()
        env = {k: v for k, v in os.environ.items() if not k.startswith('GIT_')}
        env.update(HOME=str(home), XDG_CONFIG_HOME=str(home / 'xdg'), LC_ALL='C', TZ='UTC',
                   GIT_CONFIG_NOSYSTEM='1', GIT_AUTHOR_DATE='1700000000 +0000',
                   GIT_COMMITTER_DATE='1700000000 +0000')
        repo = fixture(base, env)
        def case(name, args, **kwargs):
            compare(name, args, repo, env, **kwargs)
        if mode == 'inspect':
            (repo / 'd/e/g').write_text('before\n')
            (repo / 'outer').unlink()
            for args in [['ls-files'], ['ls-files', '-m'], ['ls-files', '-o'],
                         ['ls-files', '../top'], ['diff', '--relative'],
                         ['diff', '--relative', '--stat'], ['diff', '--name-only', '--relative'],
                         ['diff', '--stat', '--', '../top'], ['clean', '-n'], ['clean', '-n', '../']]:
                case('control-' + ' '.join(args), args)
            repo = fixture(base, env)
            for cwd in ['d', 'd/e']:
                for flags in [[], ['-m'], ['-o'], ['-s'], ['-d'], ['--others', '--exclude-standard']]:
                    for paths in [[], ['../top' if cwd == 'd' else '../../top'], [':/'], ['../']]:
                        case(f'ls-{cwd}-{flags}-{paths}', ['ls-files', *flags, '--', *paths], cwd=cwd)
                for options in [[], ['--relative'], ['--relative='], ['--relative=d'],
                                ['--relative=d/e/'], ['--relative=../'], ['--relative=/'],
                                ['--relative=.'], ['--relative=./'], ['--relative=d//'], ['--relative=d/f'], ['--relative', '--no-relative'],
                                ['--no-relative', '--relative'],
                                ['--relative=d/e', '--relative'],
                                ['--relative=d/e', '--no-relative', '--relative']]:
                    for output in [[], ['--stat'], ['--name-only'], ['--name-status']]:
                        case(f'diff-{cwd}-{options}-{output}', ['diff', *options, *output], cwd=cwd)
            for args in [['diff', '--stat', '--', '../top'], ['diff', '--', '../top'],
                         ['diff', '--relative', '--', '../top'], ['diff', '--', ':/']]:
                case(str(args), args)
            (repo / 'd/h').unlink()
            old = run(['git', 'rev-parse', 'HEAD:d/f'], repo, env)['stdout'].strip()
            data = repo / 'other-blob'
            data.write_text('other\n')
            other = run(['git', 'hash-object', '-w', str(data)], repo, env)['stdout'].strip()
            data.unlink()
            info = f'0 {"0" * 40}\td/f\n100644 {old} 2\td/f\n100644 {other} 3\td/f\n'
            index_info = base / 'index-info'
            index_info.write_text(info)
            with index_info.open('rb') as source:
                result = subprocess.run(['git', 'update-index', '--index-info'], cwd=repo, env=env,
                                        stdin=source, capture_output=True, timeout=10)
            if result.returncode:
                raise RuntimeError('unmerged fixture failed')
            for options in [[], ['--cached']]:
                for output in [[], ['--stat'], ['--numstat'], ['--shortstat']]:
                    case(f'unmerged-{options}-{output}', ['diff', '--relative', *options, *output])
            repo = fixture(base, env)
            setup(['config', 'diff.relative', 'true'], repo, env)
            for args in [['diff'], ['diff', '--stat'], ['diff', '--no-relative'], ['diff', '--relative=']]:
                case('config-relative-' + str(args), args)
            (repo / 'd/h').unlink()
            for flags in [['-d'], ['-m'], ['-d', '-m'], ['-s', '-d'], ['-c', '-d']]:
                for path in ['../', ':/']:
                    case(f'deleted-{flags}-{path}', ['ls-files', *flags, '--', path])
            setup(['add', '.'], repo, env)
            for path in ['../top', ':/', '../']:
                for opts in [[], ['--relative'], ['--stat'], ['--relative', '--stat']]:
                    case(f'cached-{path}-{opts}', ['diff', '--cached', *opts, '--', path])
        elif mode == 'mutate':
            for opts in [['-n'], ['-f'], ['-nd'], ['-fd'], ['-ndx'], ['-fdx'], ['-ndX'], ['-fdX']]:
                for paths in [[], ['../'], [':/'], ['../outer']]:
                    repo = fixture(base, env)
                    (repo / 'd/junk/sub').mkdir(parents=True)
                    (repo / 'd/junk/sub/a').write_text('junk\n')
                    (repo / 'd/.gitignore').write_text('*.ignored\n')
                    (repo / 'd/x.ignored').write_text('ignored\n')
                    case(f'clean-{opts}-{paths}', ['clean', *opts, '--', *paths], mutate=True)
            for path in ['../top', ':/top', '../', ':/']:
                for cmd in [['rm', '-rf'], ['rm', '-rnf'], ['checkout', '--'], ['checkout', 'HEAD', '--'],
                            ['reset', '--'], ['restore', '--'], ['restore', '--staged', '--'],
                            ['stash', 'push', '--'], ['stash', 'push', '-u', '--']]:
                    repo = fixture(base, env)
                    if cmd[0] in ('reset',) or '--staged' in cmd:
                        setup(['add', '.'], repo, env)
                    case(f'{cmd}-{path}', [*cmd, path], mutate=True)
            for args in [['mv', '-v', '../top', 'renamed'], ['mv', '-n', '../top', '../renamed'],
                         ['mv', '../top', '../renamed'], ['mv', ':/top', 'renamed']]:
                repo = fixture(base, env)
                case(str(args), args, mutate=True)
        elif mode == 'boundaries':
            for args in [['ls-files', '*.txt'], ['ls-files', ':(exclude)h'],
                         ['rm', '-rnf', '../', ':(exclude)h'], ['rm', '-rnf', ':(exclude)h'],
                         ['rm', '-nf', ':(exclude)h'], ['rm', '-nf', ':(literal)../d'],
                         ['rm', '-nf', ':(icase)../D']]:
                (repo / 'd/a.txt').write_text('tracked\n')
                setup(['add', 'd/a.txt'], repo, env)
                case('audit-magic-' + str(args), args, mutate=True)
            bare = base / 'bare'
            setup(['clone', '--bare', '-q', str(repo), str(bare)], base, env)
            for args in [['ls-files'], ['diff', 'HEAD', 'HEAD']]:
                compare('audit-bare-' + str(args), args, bare, env, cwd='.')
            for args in [['ls-files', '../../outside'], ['diff', '--', '../../outside'],
                         ['clean', '-n', '../../outside'], ['rm', '-f', '../../outside'],
                         ['checkout', '--', '../../outside'], ['reset', '--', '../../outside'],
                         ['restore', '--', '../../outside'], ['stash', 'push', '--', '../../outside'],
                         ['mv', '../top', '../../outside']]:
                case('outside-' + str(args), args, mutate=True)
            (repo / '.gitignore').write_text('*.ignored\nignored-dir/\nh\ntop\n')
            (repo / 'd/x.ignored').write_text('ignored\n')
            (repo / 'd/ignored-dir').mkdir()
            (repo / 'd/ignored-dir/a').write_text('ignored\n')
            for flags in [['-o'], ['--others', '--exclude-standard'], ['-o', '-i', '--exclude-standard'],
                          ['-c', '-i', '--exclude-standard']]:
                for paths in [[], ['../'], [':/']]:
                    case(f'ignored-{flags}-{paths}', ['ls-files', *flags, '--', *paths])
            for name in ['d/nested', 'd/container/nested']:
                nested = repo / name
                nested.mkdir(parents=True)
                setup(['init', '-q'], nested, env)
                (nested / 'keep').write_text('nested\n')
            (repo / 'd/container/junk').write_text('junk\n')
            nested_fixture = base / 'nested-fixture'
            shutil.copytree(repo, nested_fixture, symlinks=True)
            for opts in [['-nd'], ['-fd'], ['-nffd'], ['-ffd']]:
                for paths in [[], ['../'], [':/']]:
                    shutil.rmtree(repo)
                    shutil.copytree(nested_fixture, repo, symlinks=True)
                    case(f'nested-{opts}-{paths}', ['clean', *opts, '--', *paths], mutate=True)
            for args in [['ls-files', '--', ''], ['clean', '-n', '--', ''],
                         ['diff', '--relative=', '--', ':/'], ['ls-files', '--', ':/:'],
                         ['ls-files', '--', '../d/../'], ['clean', '-n', '--', '../d/../']]:
                case('empty-prefix-' + str(args), args, mutate=True)
            for opts in [['-n'], ['-nd'], ['-fd'], ['-nqd'], ['-fqd']]:
                for paths in [[], ['../'], [':/'], ['.'], [':(exclude)keep']]:
                    repo = fixture(base, env)
                    (repo / 'scratch').mkdir()
                    (repo / 'scratch/drop').write_text('drop\n')
                    (repo / 'scratch/keep').write_text('keep\n')
                    case(f'untracked-cwd-{opts}-{paths}', ['clean', *opts, '--', *paths], cwd='scratch', mutate=True)
            repo = fixture(base, env)
            link = base / 'alias'
            link.symlink_to(repo / 'd', target_is_directory=True)
            for args in [['ls-files'], ['diff', '--relative'], ['clean', '-n'],
                         ['ls-files', '../top'], ['diff', '--', '../top']]:
                case('symlink-' + str(args), args, cwd='../alias')
        else:
            for name, args in [
                ('inspect', ['ls-files', '-m']), ('patch', ['diff', '--relative']),
                ('clean', ['clean', '-fd']), ('stage-parent', ['add', '../top']),
                ('stage-current', ['add', '.']), ('cached', ['diff', '--cached', '--relative', '--stat']),
                ('index', ['ls-files', '-s', ':/']), ('commit', ['commit', '-q', '-m', 'Subdirectory workflow']),
                ('final', ['status', '--porcelain']), ('history', ['log', '-1', '--oneline'])]:
                case('e2e-' + name, args, mutate=True)
    if not REPORT['comparisons']:
        raise RuntimeError('zero selected comparisons')


mode = sys.argv[1] if len(sys.argv) > 1 else 'inspect'
try:
    main(mode)
except Exception as error:
    REPORT['unexpected_errors'] += 1
    print(f'UNEXPECTED: {error}', file=sys.stderr)
finally:
    REPORT['seconds'] = round(time.monotonic() - START, 3)
    directory = Path(os.environ.get('NGIT_REPORT_DIR', ROOT / 'target/include-pathspec/reports'))
    directory.mkdir(parents=True, exist_ok=True)
    (directory / f'subdir-commands-{mode}.json').write_text(json.dumps(REPORT, indent=2) + '\n')
print(json.dumps({key: value for key, value in REPORT.items() if key != 'cases'}))
sys.exit(bool(REPORT['failures'] or REPORT['unexpected_errors']))
