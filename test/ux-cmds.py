#!/usr/bin/env python3
"""Daily status, ref, worktree and stash comparisons against Git."""
import base64
import itertools
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
COMMAND = shlex.split(os.environ.get('NGIT_UX_COMMAND', str(ROOT / 'bin/ngit')))
NO_ELC = os.environ.get('NGIT_NO_ELC')
START = time.monotonic()
REPORT = dict(comparisons=0, exact=0, intended={}, failures=0, unexpected_errors=0, cases=[])
BAD = re.compile(r'void-function|void-variable|wrong-type-argument|Lisp error|[Ll]oader error|[Oo]ut of memory|resource exhausted|Wrong type argument|Wrong number of arguments|Invalid function|Symbol.s function definition is void|Symbol.s value as variable is void|uncaught error|file-missing', re.I)
MATCH = os.environ.get('UX_CMDS_MATCH', '')


def run(argv, repo, env, data=None):
    budget = min(25, START + 470 - time.monotonic())
    if budget <= 0:
        raise RuntimeError('overall deadline exceeded')
    p = subprocess.Popen(argv, cwd=repo, env=env, stdin=subprocess.PIPE,
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE, start_new_session=True)
    try:
        out, err = p.communicate(data, timeout=budget)
    except subprocess.TimeoutExpired:
        os.killpg(p.pid, signal.SIGKILL)
        p.communicate()
        raise RuntimeError('timeout: ' + repr(argv))
    def normalize(b):
        return b.decode('utf-8', errors='surrogateescape').replace(str(repo.parent), '<fixture>')
    return dict(exit=p.returncode, stdout=normalize(out), stderr=normalize(err))


def git(repo, env, *args, data=None):
    r = run(['git', *args], repo, env, data)
    if r['exit']:
        raise RuntimeError('fixture: ' + repr((args, r)))
    return r['stdout'].strip()


def write(repo, name, data):
    p = repo / name
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(data)


def commit(repo, env, message):
    git(repo, env, 'add', '.')
    git(repo, env, 'commit', '-qm', message)


def state(repo, env):
    def index_state(*args):
        # Record a corrupt post-command index as a comparison failure instead
        # of treating it as a fixture failure and abandoning the remaining cases.
        result = run(['git', *args], repo, env)
        return result['stdout'].strip() if result['exit'] == 0 else result

    files = {}
    for p in sorted(repo.rglob('*')):
        rel = p.relative_to(repo)
        if rel.parts[0] == '.git' and str(rel) != '.git/editor-input':
            continue
        if p.is_symlink():
            files[str(rel)] = ['link', os.readlink(p)]
        elif p.is_file():
            files[str(rel)] = [p.stat().st_mode & 0o777, base64.b64encode(p.read_bytes()).decode()]
        elif p.is_dir():
            files[str(rel)] = ['directory', p.stat().st_mode & 0o777]
    refs = run(['git', 'show-ref', '--head'], repo, env)
    commits = git(repo, env, 'rev-list', '--all')
    objects = {oid: run(['git', 'cat-file', '-p', oid], repo, env)['stdout'] for oid in commits.splitlines()}
    # Tag objects carry editor messages; stash commits include index/untracked parents.
    for line in refs['stdout'].splitlines():
        oid = line.split()[0]
        objects[oid] = run(['git', 'cat-file', '-p', oid], repo, env)['stdout']
    return dict(files=files, refs=refs, head=git(repo, env, 'rev-parse', '--symbolic-full-name', 'HEAD'),
                index=index_state('ls-files', '-s'),
                resolve_undo=index_state('ls-files', '--resolve-undo'),
                status=index_state('status', '--porcelain=v1', '--ignored'),
                config=git(repo, env, 'config', '--local', '--list'),
                config_bytes=base64.b64encode((repo / '.git/config').read_bytes()).decode(), objects=objects)


def base(repo, env):
    git(repo, env, 'init', '-q', '-b', 'main')
    write(repo, 'file', 'one\ntwo\nthree\nfour\nfive\n')
    write(repo, 'other', 'other\n')
    write(repo, '.gitignore', '*.log\nignored/\n')
    commit(repo, env, 'base')


def case(name, setup, commands, extra=None, cwd='.'):
    if MATCH and MATCH not in name:
        return
    with tempfile.TemporaryDirectory(prefix='ngit-ux-cmds-') as tmp:
        parent = Path(tmp)
        home = parent / 'home'
        home.mkdir()
        (home / '.gitconfig').write_text('[user]\n name = Fixture\n email = fixture@example.test\n')
        env = {k: v for k, v in os.environ.items()
               if not k.startswith(('GIT_', 'NGIT_')) and k != 'VISUAL'}
        env.update(HOME=str(home), TZ='UTC', LC_ALL='C', COLUMNS='40',
                   GIT_AUTHOR_DATE='1700000000 +0000', GIT_COMMITTER_DATE='1700000000 +0000')
        if NO_ELC:
            env['NGIT_NO_ELC'] = NO_ELC
        env.update(extra or {})
        repo = parent / 'repo'
        repo.mkdir()
        base(repo, env)
        setup(repo, env)
        run(['git', 'update-index', '-q', '--refresh'], repo, env)
        backup = parent / 'snapshot'
        shutil.copytree(repo, backup, symlinks=True)
        sequences = []
        for command in (['git'], COMMAND):
            # Copies have different inode/ctime data; establish the same clean
            # stat cache while retaining every dirty or unmerged index entry.
            run(['git', 'update-index', '-q', '--refresh'], repo, env)
            results = []
            for step in commands:
                if callable(step):
                    step(repo, env)
                    continue
                result = run([*command, *step], repo / cwd, env)
                results.append((step, result, state(repo, env)))
            sequences.append(results)
            shutil.rmtree(repo)
            shutil.copytree(backup, repo, symlinks=True)
        for n, (expected, actual) in enumerate(zip(*sequences)):
            argv, er, es = expected
            _, ar, ast = actual
            equal = er == ar and es == ast
            unexpected = ar['exit'] < 0 or bool(BAD.search(ar['stdout'] + ar['stderr']))
            REPORT['comparisons'] += 1
            REPORT['exact'] += int(equal and not unexpected)
            REPORT['failures'] += int(not equal)
            REPORT['unexpected_errors'] += int(unexpected)
            REPORT['cases'].append(dict(name=f'{name}/{n}', argv=argv, equal=equal,
                expected=er, actual=ar, state_equal=es == ast,
                expected_state=es, actual_state=ast))
            if not equal:
                print('FAIL ' + f'{name}/{n}' + ' ' + repr((er, ar, es == ast)), flush=True)


def no_setup(repo, env):
    pass


def dirty(repo, env):
    write(repo, 'new/deep/a', 'a\n')
    write(repo, 'new/b', 'b\n')
    write(repo, 'new/x.log', 'ignored nested\n')
    write(repo, 'ignored/deep/a', 'ignored directory\n')
    write(repo, 'root.log', 'ignored\n')
    write(repo, 'loose', 'untracked\n')
    write(repo, 'file', 'modified\n')


def status():
    for style in ('traditional', 'matching'):
        case('audit19-empty-ignored-' + style,
             lambda r, e: (r / 'ignored').mkdir(),
             [['status', '--short', '--ignored=' + style]])
    for fmt in ([], ['--short'], ['--porcelain=v1'], ['--porcelain=v2']):
        for u in ('-uall', '-unormal', '-uno', '--untracked-files=all', '--untracked-files=normal', '--untracked-files=no'):
            for ignored in ([], ['--ignored'], ['--ignored=matching'], ['--ignored=no']):
                case('status-' + '-'.join(fmt + [u] + ignored), dirty, [['status', *fmt, u, *ignored]])
        for config in ('all', 'normal', 'no'):
            case('status-config-' + config + '-'.join(fmt),
                 lambda r, e, c=config: (dirty(r, e), git(r, e, 'config', 'status.showUntrackedFiles', c)),
                 [['status', *fmt], ['status', *fmt, '-uall']])
    for ignored, directory, empty in itertools.product((False, True), repeat=3):
        opts = ['--exclude-standard'] + (['-i'] if ignored else []) + (['--directory'] if directory else []) + (['--no-empty-directory'] if empty else [])
        def fixture(r, e):
            dirty(r, e)
            (r / 'empty').mkdir()
        case('ls-files-' + '-'.join(opts), fixture, [['ls-files', '-o', *opts]])


    def ignored_tree(r, e):
        dirty(r, e)
        write(r, 'allignored/a.log', 'ignored file\n')
        write(r, 'tracked/kept', 'tracked\n')
        git(r, e, 'add', 'tracked/kept')
        write(r, 'tracked/sub/a.log', 'nested ignored\n')
    for style in ('traditional', 'matching'):
        for u in ('-unormal', '-uall'):
            case('status-ignored-tree-' + style + u, ignored_tree,
                 [['status', '--short', u, '--ignored=' + style],
                  ['status', '--short', u, '--ignored=' + style, '--', 'ignored/deep/a']])

    def v2_changes(r, e):
        git(r, e, 'mv', 'other', 'moved')
        write(r, 'file', 'staged\n')
        git(r, e, 'add', 'file')
        write(r, 'file', 'unstaged\n')
        write(r, 'new file', 'new\n')
        git(r, e, 'add', '-N', 'new file')
    case('status-v2-index-and-worktree', v2_changes, [['status', '--porcelain=v2', '-uall']])
    case('ls-files-directory-explicit', dirty,
         [['ls-files', '-o', '--directory', '--exclude-standard', '--', 'new/deep/a'],
          ['ls-files', '-o', '-i', '--directory', '--exclude-standard', '--', 'new/x.log']])


def refs_fixture(r, e):
    for name in ('alpha', 'beta', 'gamma', 'long-name', 'zeta'):
        git(r, e, 'branch', name)
        git(r, e, 'tag', name)
    git(r, e, 'config', 'branch.main.remote', '.')
    git(r, e, 'config', 'branch.main.merge', 'refs/heads/alpha')
    git(r, e, 'tag', '-a', 'annotated', '-m', 'Tag subject\n\nTag body')
    write(r, '.git/editor.sh', '#!/bin/sh\ncp "$1" .git/editor-input\nprintf "edited description\\n\\nbody\\n" > "$1"\n')
    (r / '.git/editor.sh').chmod(0o755)
    e['GIT_EDITOR'] = './.git/editor.sh'


def refs():
    case('audit20-detached-format',
         lambda r, e: git(r, e, 'switch', '-q', '--detach'),
         [['branch', '--format=%(HEAD)|%(refname)'],
          ['branch', '--format=%(HEAD)|%(refname:short)']])
    formats = ['%(refname)', '%(refname:short)|%(objectname)|%(objecttype)',
               '%(HEAD) %(refname:short) %(objectname:short)',
               '%(refname:short)|%(upstream)|%(upstream:short)|%(upstream:track)|%(upstream:trackshort)',
               '%(subject)', '%(refname:short)%09%(objectname:short=10)%00']
    for command in ('branch', 'tag'):
        for fmt in formats:
            case(command + '-format-' + fmt, refs_fixture, [[command, '--format=' + fmt]])
        for opts in ('always', 'column', 'row', 'dense', 'column,dense', 'row,dense', 'never'):
            case(command + '-column-' + opts, refs_fixture, [[command, '--column=' + opts]])
        case(command + '-column-config',
             lambda r, e, c=command: (refs_fixture(r, e), git(r, e, 'config', 'column.' + c, 'always')),
             [[command]])
    for branch in ([], ['alpha']):
        case('branch-edit-' + repr(branch), refs_fixture, [['branch', '--edit-description', *branch]])
    for opt in ('--track=inherit', '-t'):
        case('branch-inherit-' + opt, refs_fixture, [['branch', opt, 'child', 'main']])
    case('branch-color', refs_fixture, [['branch', '--color=always']])
    case('branch-color-config', lambda r, e: (refs_fixture(r, e), git(r, e, 'config', 'color.branch.current', 'bold red'), git(r, e, 'config', 'color.branch.local', 'yellow')), [['branch', '--color=always']])
    case('tag-editor-message', refs_fixture, [['tag', '-a', '-m', 'initial', '-e', 'edited']])


    for command in ('branch', 'tag'):
        for fmt in ('%(authorname)|%(authoremail)|%(authordate:iso8601)',
                    '%(creatordate:unix)|%(tree)|%(parent)', '%(*objectname:short)|%(*subject)|%(contents:subject)'):
            case(command + '-extra-format-' + fmt, refs_fixture, [[command, '--format=' + fmt]])
    case('branch-edit-existing', lambda r, e: (refs_fixture(r, e), git(r, e, 'config', 'branch.main.description', 'old\n')),
         [['branch', '--edit-description']])


    case('branch-inherit-no-merge', lambda r, e: (refs_fixture(r, e), git(r, e, 'config', '--unset-all', 'branch.main.merge')),
         [['branch', '--track=inherit', 'child', 'main']])
    case('branch-inherit-multiple', lambda r, e: (refs_fixture(r, e), git(r, e, 'config', '--add', 'branch.main.merge', 'refs/heads/beta')),
         [['branch', '--track=inherit', 'child', 'main']])
    case('branch-color-never-config', lambda r, e: (refs_fixture(r, e), git(r, e, 'config', 'color.branch', 'always')),
         [['branch', '--color=never']])


    for command in ('branch', 'tag'):
        for color in ('always', 'never'):
            case(command + '-format-color-' + color, refs_fixture,
                 [[command, '--color=' + color, '--format=%(color:red)%(refname:short)']])
        case(command + '-column-verbose-error', refs_fixture,
             [[command, '--column=always', '-v' if command == 'branch' else '-n']])
    case('branch-verbose-color', refs_fixture, [['branch', '--color=always', '-vv']])
    case('branch-verbose-column-config',
         lambda r, e: (refs_fixture(r, e), git(r, e, 'config', 'column.branch', 'always')),
         [['branch', '-vv']])


def branches(r, e):
    git(r, e, 'branch', 'target')
    write(r, 'file', 'one\nTARGET\nthree\nfour\nfive\n')
    commit(r, e, 'new main')


def unmerged(r, e):
    branches(r, e)
    git(r, e, 'switch', '-q', 'target')
    write(r, 'file', 'one\nOTHER\nthree\nfour\nfive\n')
    commit(r, e, 'other side')
    run(['git', 'merge', 'main'], r, e)
    write(r, 'file', 'discarded conflict markers\n')


def reset_fixture(w, i, h, t, conflict=False):
    def fixture(r, e):
        ids = {}
        for value in set((w, i, h, t)):
            write(r, 'file', value + '\n')
            commit(r, e, 'value-' + value)
            ids[value] = git(r, e, 'rev-parse', 'HEAD')
        git(r, e, 'branch', 'target', ids[t])
        git(r, e, 'reset', '--hard', ids[h])
        if conflict:
            git(r, e, 'update-index', '--force-remove', 'file')
            blob = git(r, e, 'rev-parse', ids[i] + ':file')
            git(r, e, 'update-index', '--index-info', data=(''.join(f'100644 {blob} {s}\tfile\n' for s in (1, 2, 3))).encode())
        else:
            git(r, e, 'read-tree', ids[i])
        write(r, 'file', w + '\n')
    return fixture


def worktree():
    def reset_ita(r, e, missing):
        write(r, 'new', '')
        git(r, e, 'add', '-N', 'new')
        if missing:
            (r / 'new').unlink()
    for mode in ('--merge', '--keep'):
        for missing in (False, True):
            case(f'f10-B-reset-ita-{mode}-{missing}',
                 lambda r, e, m=missing: reset_ita(r, e, m), [['reset', mode]])
    def stale_reset(r, e):
        write(r, 'file', 'staged\n')
        git(r, e, 'add', 'file')
    case('f10-A-reset-merge-no-refresh', stale_reset,
         [lambda r, e: os.utime(r / 'file', (1, 1)), ['reset', '--merge']])
    def reset_cwd(r, e):
        unmerged(r, e)
        (r / 'file').unlink()
        (r / 'file').mkdir()
    case('f5-reset-merge-cwd', reset_cwd, [['reset', '--merge', 'HEAD']], cwd='file')
    case('f5-reset-merge-original-cwd', reset_cwd,
         [['-C', '..', 'reset', '--merge', 'HEAD']], cwd='file')
    case('f5-reset-merge-changed-cwd', reset_cwd,
         [['-C', 'file', 'reset', '--merge', 'HEAD']])
    case('regression-checkout-source-unmerged', unmerged, [['checkout', 'HEAD', '--', 'file']])
    def backup_collision(r, e, kind):
        start = git(r, e, 'rev-parse', 'HEAD')
        git(r, e, 'rm', 'file')
        write(r, 'file/q', 'entirely different target contents\n' * 20)
        commit(r, e, 'directory target')
        git(r, e, 'branch', 'target')
        git(r, e, 'reset', '--hard', start)
        write(r, 'file', 'precious local edit\n')
        with (r / '.git/info/exclude').open('a') as f:
            f.write('file~local*\n')
        if kind == 'directory':
            write(r, 'file~local/precious', 'precious directory\n')
        elif kind in ('symlink', 'dangling'):
            (r / 'file~local').symlink_to('other' if kind == 'symlink' else 'missing')
        else:
            write(r, 'file~local', 'precious backup\n')
        write(r, 'file~local_0', 'precious numbered backup\n')
        (r / 'file~local_1').mkdir()
    for cmd in ('checkout', 'switch'):
        for kind in ('file', 'directory', 'symlink', 'dangling'):
            case('rereview1-' + cmd + '-' + kind,
                 lambda r, e, k=kind: backup_collision(r, e, k), [[cmd, '-m', 'target']])
    def unmerged_directory(r, e, nested):
        unmerged(r, e)
        (r / 'file').unlink()
        write(r, 'file/' + ('deep/local' if nested else 'local'), 'precious\n')
    for nested in (False, True):
        case('rereview3-reset-unmerged-directory-' + str(nested),
             lambda r, e, n=nested: unmerged_directory(r, e, n), [['reset', '--merge', 'HEAD']])
    def keep_deletion(r, e, directory):
        start = git(r, e, 'rev-parse', 'HEAD')
        git(r, e, 'rm', 'file')
        if directory:
            write(r, 'file/q', 'target\n')
        commit(r, e, 'target deletion')
        git(r, e, 'branch', 'target')
        git(r, e, 'reset', '--hard', start)
        git(r, e, 'rm', '--cached', 'file')
        if directory:
            (r / 'file').unlink()
            write(r, 'file/local', 'precious\n')
    for directory in (False, True):
        case('rereview9-reset-keep-deletion-' + str(directory),
             lambda r, e, d=directory: keep_deletion(r, e, d), [['reset', '--keep', 'target']])
    def ignored_deletion(r, e):
        write(r, '.gitignore', '/file\n')
        commit(r, e, 'ignore tracked file')
        start = git(r, e, 'rev-parse', 'HEAD')
        write(r, 'file', 'target\n')
        commit(r, e, 'target')
        git(r, e, 'branch', 'target')
        git(r, e, 'reset', '--hard', start)
        git(r, e, 'rm', '--cached', 'file')
        write(r, 'file', 'precious\n')
    case('audit1-reset-keep-ignored-deletion', ignored_deletion,
         [['reset', '--keep', 'target']])
    for opt in ('--ours', '--merge'):
        case('audit3-checkout-force-' + opt, unmerged,
             [['checkout', '-f', opt, '--', 'file']])
    def file_directory(r, e):
        start = git(r, e, 'rev-parse', 'HEAD')
        git(r, e, 'rm', 'file')
        write(r, 'file/q', 'target\n')
        commit(r, e, 'directory target')
        git(r, e, 'branch', 'target')
        git(r, e, 'reset', '--hard', start)
        write(r, 'file', 'local\n')
    for cmd in ('checkout', 'switch'):
        case('audit4-' + cmd + '-file-directory', file_directory,
             [[cmd, '-m', 'target']])
    def union(r, e):
        unmerged(r, e)
        write(r, '.gitattributes', 'file merge=union\n')
    for cmd in ('checkout', 'restore'):
        case('audit5-' + cmd + '-union', union, [[cmd, '--merge', '--', 'file']])
    # Every equality partition of worktree/index/HEAD/target, including each row
    # in Documentation/git-reset.txt; also the unmerged-index rows.
    partitions = [p for p in itertools.product('ABCD', repeat=4)
                  if p[0] == 'A' and all(ord(p[n]) <= max(map(ord, p[:n])) + 1 for n in range(1, 4))]
    for mode in ('--merge', '--keep'):
        for values in partitions:
            case('reset-' + mode + '-' + ''.join(values), reset_fixture(*values), [['reset', mode, 'target']])
        for w, h, t in itertools.product('AB', repeat=3):
            case('reset-unmerged-' + mode + w + h + t, reset_fixture(w, 'C', h, t, True), [['reset', mode, 'target']])
    for cmd in ('checkout', 'switch'):
        for line in ('one\nLOCAL\nthree\nfour\nfive\n', 'one\ntwo\nthree\nfour\nLOCAL\n'):
            case(cmd + '-merge-' + repr(line), lambda r, e, text=line: (branches(r, e), write(r, 'file', text)), [[cmd, '-m', 'target']])
    for cmd in ('checkout', 'restore'):
        for opt in ('--ours', '--theirs', '--merge', '--conflict=merge', '--conflict=diff3', '--conflict=zdiff3'):
            case(cmd + '-unmerged-' + opt, unmerged, [[cmd, opt, '--', 'file']])
    for overlay in ('--overlay', '--no-overlay'):
        def fixture(r, e):
            git(r, e, 'branch', 'target')
            write(r, 'dir/added', 'new\n')
            commit(r, e, 'add file')
        case('checkout-' + overlay, fixture, [['checkout', overlay, 'target', '--', 'dir', 'file']])


    for cmd in ('checkout', 'switch'):
        for style in ('merge', 'diff3', 'zdiff3'):
            case(cmd + '-conflict-style-' + style,
                 lambda r, e: (branches(r, e), write(r, 'file', 'one\nLOCAL\nthree\nfour\nfive\n')),
                 [[cmd, '--conflict=' + style, 'target']])
    def missing_ours(r, e):
        unmerged(r, e)
        stages = git(r, e, 'ls-files', '-s', 'file').splitlines()
        data = '0 ' + '0' * 40 + '\tfile\n'
        data += ''.join(line + '\n' for line in stages if line.split()[2] != '2')
        git(r, e, 'update-index', '--index-info', data=data.encode())
    for cmd in ('checkout', 'restore'):
        case(cmd + '-unmerged-missing-side',
             missing_ours,
             [[cmd, '--ours', '--', 'file']])


    for cmd, opts in (('checkout', ['--ours', '--merge']),
                      ('restore', ['--ours', '--source=HEAD']),
                      ('restore', ['--merge', '--staged'])):
        case(cmd + '-invalid-' + '-'.join(opts), unmerged, [[cmd, *opts, '--', 'file']])
    def topology(r, e, shape):
        start = git(r, e, 'rev-parse', 'HEAD')
        if shape in ('directory-to-file', 'staged-descendant'):
            write(r, 'dir/a', 'tracked\n')
            commit(r, e, 'directory')
            start = git(r, e, 'rev-parse', 'HEAD')
            git(r, e, 'rm', '-qr', 'dir')
            write(r, 'dir', 'target\n')
        else:
            write(r, 'newdir/a', 'target\n')
        commit(r, e, 'target')
        git(r, e, 'branch', 'target')
        git(r, e, 'reset', '--hard', start)
        if shape == 'leading-untracked':
            write(r, 'newdir', 'precious untracked bytes\n')
        if shape == 'staged-descendant':
            write(r, 'dir/staged', 'staged extra\n')
            git(r, e, 'add', 'dir/staged')
    for mode in ('--merge', '--keep'):
        for shape in ('leading-untracked', 'directory-to-file', 'staged-descendant'):
            case('reset-topology-' + mode + shape,
                 lambda r, e, s=shape: topology(r, e, s), [['reset', mode, 'target']])
    case('reset-keep-index-only',
         lambda r, e: (git(r, e, 'rm', '--cached', 'file'), write(r, 'file', 'retained local changes\n')),
         [['reset', '--keep', 'HEAD']])
    def resolved(r, e):
        unmerged(r, e)
        write(r, 'file', 'resolution\n')
        git(r, e, 'add', 'file')
    for cmd in ('checkout', 'restore'):
        for opt in ('--merge', '--conflict=diff3', '--conflict=zdiff3'):
            case(cmd + '-resolve-undo-' + opt, resolved, [[cmd, opt, '--', 'file']])
        case(cmd + '-resolve-undo-deleted',
             lambda r, e: (unmerged(r, e), git(r, e, 'rm', 'file')),
             [[cmd, '--merge', '--', 'file']])


def stash_fixture(r, e):
    write(r, 'file', 'staged\ntwo\nthree\nfour\nfive\n')
    git(r, e, 'add', 'file')
    write(r, 'other', 'unstaged\n')
    write(r, 'new/deep/a', 'untracked\n')
    write(r, 'ignored/deep/a', 'ignored\n')
    write(r, 'root.log', 'ignored log\n')


def stash():
    def driver_binary(r, e, driver):
        write(r, 'f', 'old\nanchor\n')
        if driver != 'default':
            write(r, '.gitattributes', 'f diff=' + driver + '\n')
        commit(r, e, 'binary driver base')
        write(r, 'f', 'staged\nanchor\n')
        git(r, e, 'add', 'f')
        write(r, 'f', 'staged\nanchor\nprecious\n')
    for driver in ('default', 'named'):
        case('f8-4-staged-driver-binary-' + driver,
             lambda r, e, d=driver: driver_binary(r, e, d),
             [['-c', f'diff.{driver}.binary=true', 'stash', 'push', '--staged']])
    def gitlink_only(r, e):
        first = git(r, e, 'rev-parse', 'HEAD')
        second = git(r, e, 'commit-tree', 'HEAD^{tree}', '-p', first, data=b'submodule next\n')
        git(r, e, 'update-index', '--add', '--cacheinfo', f'160000,{first},sub')
        git(r, e, 'commit', '-qm', 'gitlink base')
        git(r, e, 'init', '-q', 'sub')
        git(r / 'sub', e, 'fetch', '-q', '..', second)
        git(r / 'sub', e, 'checkout', '-q', '--detach', second)
        git(r, e, 'update-index', '--cacheinfo', f'160000,{second},sub')
    case('f8-5-staged-gitlink-only', gitlink_only, [['stash', 'push', '--staged']])
    def configured_gitlink(r, e, value, source):
        # Name and path deliberately differ: ignore is keyed by module name.
        write(r, '.gitmodules', '[submodule "child"]\n path = sub\n url = ../child\n' +
              (f' ignore = {value}\n' if source == 'gitmodules' else ''))
        commit(r, e, 'module mapping')
        gitlink_only(r, e)
        git(r, e, 'submodule', 'absorbgitdirs')
        if source == 'config':
            git(r, e, 'config', 'submodule.child.ignore', value)
    for source in ('config', 'gitmodules'):
        for value in ('none', 'dirty', 'untracked', 'all'):
            for flags in ([], ['--staged']):
                case(f'f9-2-gitlink-{source}-{value}-' + '-'.join(flags),
                     lambda r, e, v=value, s=source: configured_gitlink(r, e, v, s),
                     [['stash', 'push', *flags]])
    def unicode_module(r, e, name, path, source, module_ignore, config_ignore=None):
        configured_gitlink(r, e, module_ignore, 'gitmodules')
        if path != 'sub':
            git(r, e, 'mv', 'sub', path)
        write(r, '.gitmodules', f'[submodule "{name}"]\n path = {path}\n'
              f' url = ../child\n ignore = {module_ignore}\n')
        # Commit the mapping with gitlink A, then stage B again.
        second = git(r, e, 'rev-parse', ':' + path)
        first = git(r, e, 'rev-parse', 'HEAD:sub')
        git(r, e, 'update-index', '--cacheinfo', f'160000,{first},{path}')
        git(r, e, 'add', '.gitmodules')
        git(r, e, 'commit', '-qm', 'UTF-8 module mapping')
        git(r, e, 'update-index', '--cacheinfo', f'160000,{second},{path}')
        if config_ignore is not None:
            git(r, e, 'config', f'submodule.{name}.ignore', config_ignore)
        if source != 'worktree':
            (r / '.gitmodules').unlink()
        if source == 'head':
            git(r, e, 'update-index', '--force-remove', '.gitmodules')
    def invalid_module(r, e, value, ordinary):
        configured_gitlink(r, e, 'none', 'gitmodules')
        write(r, '.gitmodules', '[submodule "child"]\n path = sub\n ignore = bogus\n')
        git(r, e, 'add', '.gitmodules')
        if isinstance(ordinary, str):
            # Keep the mapping clean so the gitlink is the first changed path.
            second = git(r, e, 'rev-parse', ':sub')
            first = git(r, e, 'rev-parse', 'HEAD:sub')
            git(r, e, 'update-index', '--cacheinfo', f'160000,{first},sub')
            git(r, e, 'commit', '-qm', 'invalid module mapping')
            git(r, e, 'update-index', '--cacheinfo', f'160000,{second},sub')
        if ordinary:
            path = 'file' if ordinary is True else ordinary
            write(r, path, 'staged ordinary change\n')
            git(r, e, 'add', path)
        if value is not None:
            git(r, e, 'config', 'submodule.child.ignore', value)
    for value in (None, 'all', 'bogus'):
        case(f'f10-C-invalid-gitmodules-{value}',
             lambda r, e, v=value: invalid_module(r, e, v, True), [['stash', 'push']])
    for flags in ([], ['--staged']):
        case('f10-C-invalid-gitmodules-submodule-first-' + '-'.join(flags),
             lambda r, e: invalid_module(r, e, None, 'z'), [['stash', 'push', *flags]])
    case('f10-C-invalid-command-config',
         lambda r, e: invalid_module(r, e, None, 'z'),
         [['-c', 'submodule.child.ignore=bogus', 'stash', 'push']])
    for flags in ([], ['-q']):
        case('f10-C-invalid-config-staged-child-' + '-'.join(flags),
             lambda r, e: invalid_module(r, e, 'bogus', True),
             [['stash', 'push', '--staged', *flags]])
    def deleted_bad_module(r, e):
        configured_gitlink(r, e, 'none', 'gitmodules')
        git(r, e, 'update-index', '--force-remove', 'sub')
        write(r, 'file', 'staged ordinary change\n')
        git(r, e, 'add', 'file')
        git(r, e, 'config', 'submodule.child.ignore', 'bogus')
    for flags in ([], ['--staged']):
        case('f10-C-invalid-deleted-module-' + '-'.join(flags), deleted_bad_module,
             [['stash', 'push', *flags]])
    def invalid_clean_module(r, e):
        configured_gitlink(r, e, 'bogus', 'gitmodules')
        first = git(r, e, 'rev-parse', 'HEAD:sub')
        git(r, e, 'update-index', '--cacheinfo', f'160000,{first},sub')
        git(r / 'sub', e, 'checkout', '-q', '--detach', first)
    for flags in ([], ['--staged']):
        case('f10-C-invalid-clean-module-' + '-'.join(flags), invalid_clean_module,
             [['stash', 'push', *flags]])
    def invalid_clean_config(r, e, source):
        configured_gitlink(r, e, 'none', 'gitmodules')
        first = git(r, e, 'rev-parse', 'HEAD:sub')
        git(r, e, 'update-index', '--cacheinfo', f'160000,{first},sub')
        git(r / 'sub', e, 'checkout', '-q', '--detach', first)
        if source == 'config':
            git(r, e, 'config', 'submodule.child.ignore', 'bogus')
    for source in ('config', 'command'):
        for flags in ([], ['--staged']):
            case('f10-C-invalid-clean-' + source + '-' + '-'.join(flags),
                 lambda r, e, s=source: invalid_clean_config(r, e, s),
                 [[*(['-c', 'submodule.child.ignore=bogus'] if source == 'command' else []),
                   'stash', 'push', *flags]])
    def clean_config_change(r, e, path, staged):
        if path == 'z':
            write(r, path, 'ordinary base\n')
            commit(r, e, 'tracked path after submodule')
        invalid_clean_config(r, e, 'config')
        write(r, path, 'ordinary local change\n')
        if staged:
            git(r, e, 'add', path)
    for path, staged in (('file', True), ('file', False), ('z', False)):
        for flags in ([], ['--staged']):
            case(f'f10-C-clean-config-order-{path}-{staged}-' + '-'.join(flags),
                 lambda r, e, p=path, s=staged: clean_config_change(r, e, p, s),
                 [['stash', 'push', *flags]])
    for index_flag in ('--skip-worktree', '--assume-unchanged'):
        for flags in ([], ['--staged']):
            case('f10-C-clean-config-' + index_flag + '-' + '-'.join(flags),
                 lambda r, e, f=index_flag: (invalid_clean_config(r, e, 'config'),
                                           git(r, e, 'update-index', f, 'sub')),
                 [['stash', 'push', *flags]])
    # An ordinary-only pathspec never asks for the module's settings.
    case('f10-C-invalid-gitmodules-ordinary-first',
         lambda r, e: invalid_module(r, e, None, True), [['stash', 'push', '--', 'file']])
    for source in ('worktree', 'index', 'head'):
        for name, path in (('child', '子'), ('子', 'sub')):
            case(f'f10-D-utf8-{source}-{name}-{path}',
                 lambda r, e, n=name, p=path, s=source:
                     unicode_module(r, e, n, p, s, 'all', 'none'),
                 [['stash', 'push', *(['--staged', '--', path] if source != 'worktree' else [])]])
            case(f'f10-D-utf8-gitmodules-only-{source}-{name}-{path}',
                 lambda r, e, n=name, p=path, s=source:
                     unicode_module(r, e, n, p, s, 'none'),
                 [['stash', 'push', *(['--staged', '--', path] if source != 'worktree' else [])]])
    for module_ignore, config_ignore in (('none', 'all'), ('all', 'none')):
        case(f'f10-E-utf8-config-{module_ignore}-{config_ignore}',
             lambda r, e, m=module_ignore, c=config_ignore:
                 unicode_module(r, e, '子', 'sub', 'worktree', m, c), [['stash', 'push']])
    def ignored_submodule_dirty(r, e, ignore):
        configured_gitlink(r, e, ignore, 'gitmodules')
        git(r, e, 'update-index', '--cacheinfo',
            '160000,' + git(r, e, 'rev-parse', 'HEAD:sub') + ',sub')
        git(r / 'sub', e, 'checkout', '-q', '--detach', git(r, e, 'rev-parse', 'HEAD:sub'))
        write(r / 'sub', 'file', 'submodule local change\n')
    for ignore in ('dirty', 'all', 'none', 'untracked'):
        case('f10-F-staged-submodule-dirty-' + ignore,
             lambda r, e, i=ignore: ignored_submodule_dirty(r, e, i),
             [['stash', 'push', '--staged']])
    def plain_symlink_delete(r, e):
        (r / 'link').symlink_to('target')
        commit(r, e, 'symlink base')
        git(r, e, 'config', 'core.symlinks', 'false')
        git(r, e, 'rm', '-q', 'link')
    case('f9-10b-staged-symlinks-false-restore', plain_symlink_delete,
         [['stash', 'push', '--staged']])
    def reverse_fixture(r, e, shape):
        name = 'd/f' if shape == 'directory-mode' else 'f.txt'
        if shape == 'empty-add':
            write(r, name, '')
            git(r, e, 'add', name)
            write(r, name, 'precious')
            return
        if shape == 'crlf':
            write(r, '.gitattributes', '*.txt text eol=crlf\n')
        before = ('prefix\npadding\nanchor\nold\n' if shape == 'end-interior' else
                  'anchor\nold\n' if shape == 'end' else 'old\nanchor\n')
        after = ('prefix\npadding\nanchor\nstaged\n' if shape == 'end-interior' else
                 'anchor\nstaged\n' if shape == 'end' else 'staged\nanchor\n')
        write(r, name, before.replace('\n', '\r\n') if shape == 'crlf' else before)
        commit(r, e, 'reverse base')
        write(r, name, after.replace('\n', '\r\n') if shape == 'crlf' else after)
        git(r, e, 'add', name)
        if shape == 'beginning':
            write(r, name, 'precious\nanchor\nstaged\nanchor\n')
        elif shape == 'end':
            write(r, name, 'anchor\nstaged\nanchor\nprecious\n')
        elif shape == 'end-interior':
            write(r, name, 'prefix\npadding\nanchor\nstaged\nanchor\nprecious\n')
        elif shape == 'directory-mode':
            (r / 'd').chmod(0o700)
    for shape in ('empty-add', 'beginning', 'end', 'end-interior', 'crlf', 'directory-mode'):
        case('f5-staged-' + shape, lambda r, e, s=shape: reverse_fixture(r, e, s),
             [['stash', 'push', '--staged']])
    def empty_index(r, e):
        git(r, e, 'rm', '-r', '.')
    case('f5-staged-keep-empty-index', empty_index,
         [['stash', 'push', '--staged', '--keep-index']])
    def parent_recreated(r, e, kind):
        write(r, 'd/f', 'base\n')
        commit(r, e, 'parent base')
        git(r, e, 'rm', '-r', 'd')
        if kind == 'file':
            write(r, 'd', 'precious\n')
        else:
            (r / 'd').symlink_to('missing' if kind == 'dangling' else 'other')
    for kind in ('file', 'dangling', 'symlink'):
        case('rereview2-staged-parent-' + kind,
             lambda r, e, k=kind: parent_recreated(r, e, k), [['stash', 'push', '--staged']])
    def staged_directory(r, e, shape):
        git(r, e, 'rm', 'file')
        (r / 'file').mkdir()
        if shape != 'empty':
            write(r, 'file/a', 'new\n')
        if shape == 'staged':
            git(r, e, 'add', 'file/a')
    for shape in ('staged', 'empty', 'untracked'):
        case('rereview4-staged-directory-' + shape,
             lambda r, e, s=shape: staged_directory(r, e, s), [['stash', 'push', '--staged']])
    def local_mode(r, e, executable):
        git(r, e, 'config', 'core.filemode', 'true')
        (r / 'file').chmod(0o755 if executable else 0o644)
        git(r, e, 'add', '.')
        git(r, e, 'commit', '--allow-empty', '-qm', 'base mode')
        write(r, 'file', 'new\n')
        git(r, e, 'add', 'file')
        (r / 'file').chmod(0o644 if executable else 0o755)
    for executable in (False, True):
        case('rereview5-staged-local-mode-' + str(executable),
             lambda r, e, x=executable: local_mode(r, e, x), [['stash', 'push', '--staged']])
    def type_change(r, e, symlink, local):
        (r / 'file').unlink()
        if symlink:
            (r / 'file').symlink_to('target')
        else:
            write(r, 'file', 'target')
        commit(r, e, 'base type')
        (r / 'file').unlink()
        if symlink:
            write(r, 'file', 'target')
        else:
            (r / 'file').symlink_to('target')
        git(r, e, 'add', 'file')
        if local:
            if symlink:
                write(r, 'file', 'local')
            else:
                (r / 'file').unlink()
                (r / 'file').symlink_to('local')
    for symlink in (False, True):
        for local in (False, True):
            case('rereview6-staged-type-change-' + str(symlink) + '-' + str(local),
                 lambda r, e, s=symlink, l=local: type_change(r, e, s, l), [['stash', 'push', '--staged']])
    def binary_mode(r, e, executable):
        git(r, e, 'config', 'core.filemode', 'true')
        (r / 'file').write_bytes(b'a\x00')
        (r / 'file').chmod(0o755 if executable else 0o644)
        commit(r, e, 'binary base')
        (r / 'file').chmod(0o644 if executable else 0o755)
        git(r, e, 'add', 'file')
        (r / 'file').write_bytes(b'b\x00')
    for executable in (False, True):
        case('rereview7-staged-binary-mode-' + str(executable),
             lambda r, e, x=executable: binary_mode(r, e, x), [['stash', 'push', '--staged']])
    def staged_type(r, e, deletion=False):
        if deletion:
            git(r, e, 'rm', 'file')
            write(r, 'file', '')
        else:
            write(r, 'file', 'target')
            git(r, e, 'add', 'file')
            (r / 'file').unlink()
            (r / 'file').symlink_to('target')
    for deletion in (False, True):
        case('audit2-staged-type-' + str(deletion),
             lambda r, e, d=deletion: staged_type(r, e, d), [['stash', 'push', '--staged']])
    for opts in (['--all', '--include-untracked'], ['--include-untracked', '--all']):
        case('audit6-stash-order-' + '-'.join(opts),
             lambda r, e: write(r, 'secret.log', 'precious\n'), [['stash', 'push', *opts]])
    def adjacent(r, e):
        lines = [str(n) + '\n' for n in range(1, 10)]
        write(r, 'file', ''.join(lines))
        commit(r, e, 'nine lines')
        lines[4] = 'staged\n'
        write(r, 'file', ''.join(lines))
        git(r, e, 'add', 'file')
        lines[1] = 'local\n'
        write(r, 'file', ''.join(lines))
    case('audit7-stash-one-context', adjacent, [['stash', 'push', '--staged']])
    for opts in (['--all'], ['-u'], ['--include-untracked'], ['--staged']):
        case('stash-push-' + '-'.join(opts), stash_fixture, [['stash', 'push', *opts], ['stash', 'show', '-p'], ['stash', 'pop']])
        for show in ('-u', '--include-untracked', '--only-untracked'):
            case('stash-show-' + '-'.join(opts) + show, stash_fixture, [['stash', 'push', *opts], ['stash', 'show', show], ['stash', 'show', '-p', show], ['stash', 'apply', '--index']])
    for opts in (['--staged', '-u'], ['--staged', '--all'], ['--staged']):
        case('stash-error-' + '-'.join(opts), dirty, [['stash', 'push', *opts]])
    case('stash-staged-overlap', lambda r, e: (stash_fixture(r, e), write(r, 'file', 'overlap\n')), [['stash', 'push', '--staged']])


    case('stash-staged-keep', stash_fixture,
         [['stash', 'push', '--staged', '--keep-index'], ['status', '--porcelain=v1']])
    case('stash-all-ignored-only', lambda r, e: write(r, 'ignored/a', 'ignored\n'),
         [['stash', 'push', '--all'], ['stash', 'show', '-p', '--only-untracked'], ['stash', 'pop']])
    case('stash-untracked-collision', stash_fixture,
         [['stash', 'push', '-u'], lambda r, e: write(r, 'new/deep/a', 'collision\n'), ['stash', 'pop']])


    case('stash-staged-paths', stash_fixture, [['stash', 'push', '--staged', '--', 'file'], ['stash', 'pop']])
    case('stash-staged-selected-index',
         lambda r, e: (stash_fixture(r, e), git(r, e, 'add', 'other')),
         [['stash', 'push', '--staged', '--', 'file'], ['status', '--porcelain=v1']])
    case('stash-staged-unselected-index',
         lambda r, e: (write(r, 'other', 'staged\n'), git(r, e, 'add', 'other')),
         [['stash', 'push', '--staged', '--', 'file']])
    case('stash-staged-clean', no_setup, [['stash', 'push', '--staged']])
    case('stash-staged-selected-worktree',
         lambda r, e: (write(r, 'other', 'staged\n'), git(r, e, 'add', 'other'),
                       write(r, 'file', 'local only\n')),
         [['stash', 'push', '--staged', '--', 'file'], ['status', '--porcelain=v1']])


def e2e():
    case('daily-working-session', lambda r, e: (refs_fixture(r, e), stash_fixture(r, e)), [
        ['status', '-uall', '--ignored=matching'], ['branch', '--format=%(HEAD) %(refname:short) %(objectname:short)'],
        ['tag', '--column=always'], ['stash', 'push', '--staged'], ['stash', 'push', '--all'],
        ['stash', 'show', '-p', '-u'], ['stash', 'pop'], ['status', '--porcelain=v2', '-uall', '--ignored']])
    case('daily-conflict-session', lambda r, e: (branches(r, e), write(r, 'file', 'one\nLOCAL\nthree\nfour\nfive\n')), [
        ['checkout', '-m', 'target'], ['status', '--short'], ['restore', '--ours', 'file'],
        ['status', '--porcelain=v2'], ['reset', '--merge'], ['status', '-uno']])


if __name__ == '__main__':
    mode = sys.argv[1] if len(sys.argv) == 2 else ''
    try:
        {'status': status, 'refs': refs, 'worktree': worktree, 'stash': stash, 'e2e': e2e}[mode]()
    except Exception as exc:
        REPORT['unexpected_errors'] += 1
        print('ERROR ' + repr(exc), flush=True)
    REPORT['seconds'] = round(time.monotonic() - START, 3)
    dest = Path(os.environ.get('NGIT_REPORT_DIR', ROOT / 'target/ux/reports'))
    dest.mkdir(parents=True, exist_ok=True)
    (dest / ('ux-cmds-' + mode + '.json')).write_text(json.dumps(REPORT, indent=2, ensure_ascii=True) + '\n')
    print('ux-cmds %s: comparisons=%d exact=%d intended=%d failures=%d unexpected_errors=%d' %
          (mode, REPORT['comparisons'], REPORT['exact'], len(REPORT['intended']), REPORT['failures'], REPORT['unexpected_errors']))
    sys.exit(int(bool(REPORT['failures'] or REPORT['unexpected_errors'] or not REPORT['comparisons'])))
