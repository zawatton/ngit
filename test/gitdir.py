#!/usr/bin/env python3
"""Git 2.47.3 repository setup and separate-git-dir working sessions."""
import base64
from concurrent.futures import ThreadPoolExecutor
import json
import os
import pwd
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
COMMAND = shlex.split(os.environ.get('NGIT_UX_COMMAND', str(ROOT / 'bin/ngit')))
ONLY = tuple(filter(None, os.environ.get('NGIT_UX_ONLY', '').split(',')))
START = time.monotonic()
DEADLINE = START + 450
REPORT = dict(comparisons=0, exact=0, intended={}, failures=0,
              unexpected_errors=0, pruned=[], cases=[])
BAD = re.compile(rb'void-function|void-variable|wrong-type-argument|[Ww]rong type argument|Lisp error|'
                 rb'[Ll]oader error|Cannot open load file|[Oo]ut of memory|resource exhausted')


def run(command, cwd, env, root):
    budget = min(30, DEADLINE - time.monotonic())
    if budget <= 0:
        raise RuntimeError('overall deadline')
    p = subprocess.Popen(command, cwd=cwd, env=env, stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE, start_new_session=True)
    try:
        out, err = p.communicate(timeout=budget)
    except subprocess.TimeoutExpired:
        os.killpg(p.pid, signal.SIGKILL)
        p.communicate()
        raise RuntimeError('command timeout: ' + repr(command))
    return dict(exit=p.returncode,
                stdout=out.replace(os.fsencode(root), b'<ROOT>').decode('utf-8', 'surrogateescape'),
                stderr=err.replace(os.fsencode(root), b'<ROOT>').decode('utf-8', 'surrogateescape'))


def setup(args, root, env, cwd=None):
    result = run(['git', *args], cwd or root, env, root)
    if result['exit']:
        raise RuntimeError('fixture failed: ' + repr((args, result)))


def write(root, path, text):
    dest = root / path
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_text(text)


def fixture(root, env, shape, config):
    (root / 'wt/sub/deep').mkdir(parents=True)
    (root / 'outside').mkdir()
    if shape == 'bare':
        setup(['init', '-q', '--bare', '-b', 'main', 'meta'], root, env)
    else:
        args = ['init', '-q', '-b', 'main']
        if shape == 'separate':
            args += ['--separate-git-dir=' + str(root / 'meta')]
        setup([*args, 'wt'], root, env)
        write(root, 'wt/file', 'base\n')
        write(root, 'wt/sub/deep/tracked', 'base\n')
        setup(['add', '.'], root, env, root / 'wt')
        setup(['commit', '-qm', 'base'], root, env, root / 'wt')
        write(root, 'wt/file', 'modified\n')
        write(root, 'wt/sub/deep/tracked', 'staged\n')
        setup(['add', 'sub/deep/tracked'], root, env, root / 'wt')
        write(root, 'wt/sub/untracked', 'untracked\n')
        write(root, 'outside/elsewhere', 'outside\n')
    gd = root / ('wt/.git' if shape == 'normal' else 'meta')
    for key, value in config:
        setup(['--git-dir=' + str(gd), 'config', key,
               value.replace('@ROOT@', str(root))], root, env)
    return gd


def state(root, env, logical_objects=False):
    """All file modes/bytes, logical objects and index entries (no stat cache)."""
    files, indexes, objects = {}, {}, {}
    for p in sorted(root.rglob('*')):
        rel = p.relative_to(root).as_posix()
        if logical_objects and p.is_dir() and p.name == 'objects':
            objects[rel] = run(['git', '--git-dir=' + str(p.parent), 'cat-file',
                                '--batch-all-objects', '--batch'], root, env, root)
        if p.is_symlink():
            files[rel] = ['link', os.readlink(p).replace(str(root), '<ROOT>')]
        elif p.is_file():
            # clone may hardlink loose objects or receive a pack, with derived
            # .idx/.rev files and different object-file permissions. Compare
            # every object's type/content instead, retaining all other files.
            # Existing non-clone cases retain their original byte/mode checks.
            if logical_objects and '/objects/' in rel:
                continue
            if p.name == 'index' and (p.parent / 'HEAD').is_file():
                # Stat fields depend on fresh fixture inodes and command refreshes.
                indexes[rel] = run(['git', '--git-dir=' + str(p.parent),
                                    'ls-files', '--stage', '-v'], root, env, root)
                continue
            # The alternate-index fixture is also an index, with per-fixture
            # stat fields. Compare its entries/flags exactly like the main one.
            if p.name == 'index' and p.read_bytes().startswith(b'DIRC'):
                index_env = dict(env, GIT_INDEX_FILE=str(p))
                indexes[rel] = run(['git', '--git-dir=' + str(root / 'wt/.git'),
                                    'ls-files', '--stage', '-v'], root, index_env, root)
                continue
            data = p.read_bytes()
            if '/objects/' in rel and re.fullmatch(r'[0-9a-f]{38}', p.name):
                data = zlib.decompress(data)
            data = data.replace(os.fsencode(root), b'<ROOT>')
            files[rel] = [p.stat().st_mode & 0o777, base64.b64encode(data).decode()]
    result = dict(files=files, indexes=indexes)
    if logical_objects:
        result['objects'] = objects
    return result


def record(name, steps, expected, actual):
    equal = expected == actual
    unexpected = any(r['result']['exit'] < 0 or BAD.search(
        (r['result']['stdout'] + r['result']['stderr']).encode('utf-8', 'surrogateescape'))
        for r in actual)
    REPORT['comparisons'] += 1
    REPORT['exact'] += int(equal and not unexpected)
    REPORT['failures'] += int(not equal or unexpected)
    REPORT['unexpected_errors'] += int(bool(unexpected))
    REPORT['cases'].append(dict(name=name, steps=steps, equal=equal,
                                expected=expected, actual=actual))
    if not equal or unexpected:
        differences = [i for i, pair in enumerate(zip(expected, actual)) if pair[0] != pair[1]]
        print('FAIL ' + name + ': steps ' + repr(differences), flush=True)


def compare(base, env, name, shape, config, cwd, options, environment, commands,
            prepare=()):
    if ONLY and not name.startswith(ONLY):
        return
    base = base / name
    base.mkdir()
    sides = []
    for command in (['git'], COMMAND):
        root = base / 'fixture'
        root.mkdir()
        if shape != 'empty':
            fixture(root, env, shape, config)
        else:
            (root / 'wt/sub/deep').mkdir(parents=True)
        for action in prepare:
            if action[0] == 'write':
                write(root, action[1], action[2].replace('@ROOT@', str(root)))
            elif action[0] == 'copy':
                shutil.copyfile(root / action[1], root / action[2])
            elif action[0] == 'move':
                (root / action[1]).rename(root / action[2])
            elif action[0] == 'chmod':
                (root / action[1]).chmod(action[2])
            elif action[0] == 'link':
                (root / action[2]).symlink_to(action[1])
            elif action[0] == 'git':
                setup([a.replace('@ROOT@', str(root)) for a in action[2:]],
                      root, env, root / action[1])
            else:
                raise ValueError(action)
        side_env = env.copy()
        side_env.update({k: v.replace('@ROOT@', str(root)) for k, v in environment.items()})
        opts = [a.replace('@ROOT@', str(root)) for a in options]
        results = []
        for args in commands:
            if args[0] == ':write':
                write(root, args[1], args[2])
                continue
            expanded = [a.replace('@ROOT@', str(root)).replace('@SOURCE@', str(root / 'source'))
                        for a in args]
            result = run([*command, *opts, *expanded], root / cwd, side_env, root)
            if name.startswith('g-matrix-') and command == ['git'] and not sides and result['exit']:
                REPORT['pruned'].append(dict(name=name, result=result))
                shutil.rmtree(root)
                return
            results.append(dict(result=result, state=state(root, env, args[0] == 'clone')))
        sides.append(results)
        shutil.rmtree(root)
    record(name, commands, *sides)


COMMANDS = [['status', '--porcelain=v1'], ['status', '--porcelain=v2'],
            ['status', '--short'], ['status', '--long']] + [
    ['rev-parse', opt] for opt in ('--show-toplevel', '--show-prefix', '--git-dir',
                                 '--absolute-git-dir', '--is-inside-work-tree',
                                 '--is-bare-repository', '--show-cdup')]


def all_cases(base, env):
    scenarios = []

    def add(name, shape='separate', config=(), cwd='wt', opts=(), variables=None):
        scenarios.append((name, shape, config, cwd, opts, variables or {}))

    for cwd in ('wt', 'wt/sub/deep', 'outside'):
        tag = cwd.replace('/', '-')
        for absolute in (False, True):
            gd = '@ROOT@/meta' if absolute else os.path.relpath('meta', cwd)
            wt = '@ROOT@/wt' if absolute else os.path.relpath('wt', cwd)
            suffix = tag + ('-abs' if absolute else '-rel')
            add('explicit-' + suffix, cwd=cwd, opts=['--git-dir=' + gd, '--work-tree=' + wt])
            add('env-' + suffix, cwd=cwd, variables=dict(GIT_DIR=gd, GIT_WORK_TREE=wt))
            add('implicit-cwd-' + suffix, cwd=cwd, opts=['--git-dir', gd])
            add('core-worktree-' + suffix, config=[('core.worktree', '@ROOT@/wt' if absolute else '../wt')],
                cwd=cwd, opts=['--git-dir=' + gd])
    for cwd in ('wt', 'wt/sub/deep'):
        tag = cwd.replace('/', '-')
        add('discovered-' + tag, cwd=cwd)
        add('worktree-only-' + tag, cwd=cwd, opts=['--work-tree=' + os.path.relpath('wt', cwd)])
        add('discovered-core-' + tag, cwd=cwd, config=[('core.worktree', '../wt')])
        add('discovered-bare-' + tag, cwd=cwd, config=[('core.bare', 'true')])
        add('discovered-bogus-' + tag, cwd=cwd,
            config=[('core.bare', 'true'), ('core.worktree', '../wt')])
    add('normal-dotgit', 'normal', opts=['--git-dir=.git'])
    add('explicit-gitfile', opts=['--git-dir=.git', '--work-tree=.'])
    add('env-cli-override', opts=['--git-dir=@ROOT@/meta', '--work-tree=@ROOT@/wt'],
        variables=dict(GIT_DIR='missing', GIT_WORK_TREE='missing'))
    add('bare-root', 'bare', cwd='meta')
    add('bare-subdir', 'bare', cwd='meta/objects')
    add('bare-explicit', 'bare', cwd='outside', opts=['--git-dir=../meta'])
    add('bare-worktree', 'bare', opts=['--git-dir=../meta', '--work-tree=.'])
    add('bare-core-false', 'bare', [('core.bare', 'false')], cwd='meta')
    add('bare-core-worktree', 'bare', [('core.bare', 'false'), ('core.worktree', '../wt')],
        cwd='outside', opts=['--git-dir=../meta'])
    add('bare-flag', 'bare', cwd='meta', opts=['--bare'])
    add('bare-flag-nonbare', opts=['--bare', '--git-dir=../meta'])
    add('bare-flag-worktree', opts=['--bare', '--git-dir=../meta', '--work-tree=.'])
    add('bare-config-worktree', config=[('core.bare', 'true'), ('core.worktree', '../wt')],
        opts=['--git-dir=../meta'])
    add('env-worktree-overrides-bare', config=[('core.bare', 'true'), ('core.worktree', 'missing')],
        variables=dict(GIT_DIR='../meta', GIT_WORK_TREE='.'))
    add('implicit-disabled', variables=dict(GIT_DIR='../meta', GIT_IMPLICIT_WORK_TREE='0'))
    add('invalid-gitdir', opts=['--git-dir=missing'])
    add('ordered-C', cwd='outside', opts=['--git-dir=../../../meta', '-C', '../wt',
                                        '-C', 'sub/deep', '--work-tree=../..'])
    # Independent fixtures allow parallel commands without shared repository state.
    with ThreadPoolExecutor(max_workers=16) as pool:
        futures = [pool.submit(compare, base, env, name + '-' + args[-1].lstrip('-'),
                               shape, config, cwd, opts, variables, [args])
                   for name, shape, config, cwd, opts, variables in scenarios
                   for args in COMMANDS]
        for future in futures:
            future.result()
    review_cases(base, env)
    second_review_cases(base, env)


def review_cases(base, env):
    """Stage-G review reproductions, plus discovery boundary controls."""
    pending = []

    def case(name, commands, *, shape='normal', config=(), cwd='wt',
             opts=(), variables=None, prepare=()):
        pending.append((base, env, name, shape, config, cwd, opts, variables or {},
                        commands, prepare))

    # Close-out regressions: a main-tree override must not redirect linked-tree
    # checkout or hide valuable changes from worktree remove's dirty check.
    for form in ('option-absolute', 'env-absolute'):
        override = dict(opts=['--work-tree=@ROOT@/wt']) if form == 'option-absolute' else dict(
            variables=dict(GIT_WORK_TREE='@ROOT@/wt'))
        case('g-closeout-worktree-add-' + form,
             [['worktree', 'add', '-b', 'topic', '../linked']],
             prepare=[('write', 'wt/file', 'valuable main uncommitted change\n')],
             **override)
        case('g-closeout-worktree-remove-' + form,
             [['worktree', 'remove', '../linked']],
             prepare=[('git', 'wt', 'add', '.'),
                      ('git', 'wt', 'commit', '-qm', 'clean main worktree'),
                      ('git', 'wt', 'worktree', 'add', '-q', '-b', 'topic', '../linked'),
                      ('write', 'linked/file', 'valuable linked uncommitted change\n')],
             **override)

    for bare in (True, False):
        case('g-reg-1-init-' + ('bare' if bare else 'normal'),
             [['init', '-q', '-b', 'main', *(['--bare'] if bare else []), 'child']],
             variables=dict(GIT_DIR='.git'))
    for key, value in (('core.bare', 'true'), ('core.worktree', '@ROOT@/outside')):
        case('g-reg-2-command-' + key, [['status', '--short'],
             ['rev-parse', '--show-toplevel', '--show-prefix']],
             opts=['-c', key + '=' + value])
    linked = [('git', '', 'clone', '-q', '--bare', 'wt', 'bare.git'),
              ('git', '', '--git-dir=bare.git', 'worktree', 'add', '-q',
               '--detach', 'linked', 'HEAD')]
    case('g-reg-3-bare-linked', [['status', '--short'],
         ['rev-parse', '--show-toplevel', '--is-bare-repository']],
         cwd='linked', prepare=linked)
    case('g-reg-3-shared-worktree', [['status', '--short']], cwd='linked',
         prepare=linked + [('git', '', '--git-dir=bare.git', 'config',
                           'core.worktree', '@ROOT@/outside')])
    for command in (['ls-files'], ['ls-tree', 'HEAD'], ['diff', 'HEAD', 'HEAD'],
                    ['diff', '--name-only'], ['log', '--oneline'],
                    ['check-attr', 'text', '--', 'file']):
        case('g-reg-4-outside-' + command[0] + '-' + str(len(command)), [command],
             variables=dict(GIT_WORK_TREE='../outside'))
    case('g-reg-4-outside-add', [['add', 'elsewhere'], ['ls-files', '--stage'],
         ['status', '--short']], variables=dict(GIT_WORK_TREE='../outside'))
    case('g-reg-5-symlink-worktree', [['status', '--short', '--', 'tracked']],
         cwd='wt/sub/deep', variables=dict(GIT_WORK_TREE='@ROOT@/link'),
         prepare=[('link', 'wt', 'link')])
    case('g-reg-5-symlink-component', [['status', '--short', '--', 'tracked']],
         cwd='wt/sub/deep', variables=dict(GIT_WORK_TREE='@ROOT@/alias/wt'),
         prepare=[('link', '.', 'alias')])
    case('g-reg-5-symlink-dotdot', [['status', '--short', '--', 'tracked']],
         cwd='wt/sub/deep', variables=dict(GIT_WORK_TREE='@ROOT@/link/..'),
         prepare=[('link', 'wt/sub', 'link')])
    for value in ('false', 'FALSE', 'no', 'off', '', '0', 'true', 'yes', 'on', '1', '2', 'invalid'):
        case('g-reg-6-implicit-' + (value or 'empty'),
             [['rev-parse', '--is-inside-work-tree', '--is-bare-repository']],
             variables=dict(GIT_DIR='.git', GIT_IMPLICIT_WORK_TREE=value))
    for kind in ('invalid', 'missing-target'):
        broken = 'broken\n' if kind == 'invalid' else 'gitdir: nowhere\n'
        for command in ('init', 'clone'):
            case('g-reg-7-' + kind + '-' + command,
                 [[command, '-q', *(['-b', 'main'] if command == 'init'
                                    else ['--no-local', '@SOURCE@']), 'child']],
                 shape='empty', prepare=[('write', 'wt/.git', broken),
                     ('git', '', 'init', '-q', '-b', 'main', 'source'),
                     ('write', 'source/file', 'source\n'),
                     ('git', 'source', 'add', 'file'),
                     ('git', 'source', 'commit', '-qm', 'source')])
    # The test-only owner switch is honoured by Git 2.47.3. The denied
    # case verifies this at runtime rather than relying on a source claim.
    owner = dict(GIT_TEST_ASSUME_DIFFERENT_OWNER='1')
    case('g-disc-safe-denied', [['status', '--short']], variables=owner)
    case('g-disc-safe-local-untrusted', [['status', '--short']], variables=owner,
         config=[('safe.directory', '*')])
    for value in ('@ROOT@/wt', '*', '@ROOT@/*'):
        case('g-disc-safe-command-' + ('wild' if value == '*' else
             'tree' if value.endswith('/*') else 'exact'), [['status', '--short']],
             variables=owner, opts=['-c', 'safe.directory=' + value])
    case('g-disc-safe-reset', [['status', '--short']], variables=owner,
         opts=['-c', 'safe.directory=*', '-c', 'safe.directory='])
    case('g-disc-safe-global', [['status', '--short']],
         variables=dict(owner, GIT_CONFIG_GLOBAL='@ROOT@/global'),
         prepare=[('write', 'global', '[safe]\n\tdirectory = @ROOT@/wt\n')])
    case('g-disc-safe-config-global',
         [['config', '--global', '--add', 'safe.directory', '@ROOT@/wt'],
          ['status', '--short']],
         variables=dict(owner, GIT_CONFIG_GLOBAL='@ROOT@/global'))
    case('g-disc-safe-config-local', [['config', '--local', '--get', 'core.bare']],
         variables=owner)
    for value, tag in (('@ROOT@/wt', 'root'), ('@ROOT@/wt/sub', 'sub'),
                       ('@ROOT@/absent:@ROOT@/wt', 'list'),
                       ('@ROOT@/link', 'symlink'), (':@ROOT@/link', 'literal-link'),
                       ('relative', 'relative'), ('@ROOT@/outside', 'unrelated')):
        case('g-disc-ceiling-' + tag, [['rev-parse', '--show-toplevel']],
             cwd='wt/sub/deep', variables=dict(GIT_CEILING_DIRECTORIES=value),
             prepare=[('link', 'wt', 'link')])
    case('g-disc-ceiling-start', [['rev-parse', '--show-toplevel']],
         variables=dict(GIT_CEILING_DIRECTORIES='@ROOT@/wt'))
    # /proc is an existing mount: no root, mount, or writes outside TMPDIR.
    # There is deliberately no repository above it. This checks the exact
    # stop diagnostic and the boolean opt-in to crossing the boundary.
    if Path('/proc').is_dir() and os.stat('/proc').st_dev != os.stat('/').st_dev:
        for value in (None, 'false', 'true', '1', 'invalid'):
            case('g-disc-filesystem-' + (value or 'default'),
                 [['rev-parse', '--show-toplevel']], cwd='/proc', shape='empty',
                 variables={} if value is None else dict(GIT_DISCOVERY_ACROSS_FILESYSTEM=value))
    with ThreadPoolExecutor(max_workers=16) as pool:
        futures = [pool.submit(compare, *args) for args in pending]
        for future in futures:
            future.result()



def second_review_cases(base, env):
    """Second review and the dispatch-family Cartesian product (G.8/G.9)."""
    pending = []

    def case(name, commands, *, cwd='wt', opts=(), variables=None, prepare=(), shape='normal'):
        pending.append((base, env, name, shape, (), cwd, opts, variables or {}, commands, prepare))

    hook = """#!/bin/sh
pwd
for v in GIT_DIR GIT_INDEX_FILE GIT_WORK_TREE GIT_OBJECT_DIRECTORY GIT_COMMON_DIR; do
 eval 'printf "%s=%s\\n" "$v" "${'"$v"'-<unset>}"'
done
git rev-parse --absolute-git-dir >&2
"""
    hook_prepare = [('write', 'wt/.git/hooks/pre-commit', hook),
                    ('chmod', 'wt/.git/hooks/pre-commit', 0o755)]
    outside = dict(GIT_DIR='.git', GIT_WORK_TREE='sub')
    case('g-reg2-1-init-implicit', [['init', '-q']], variables=dict(GIT_IMPLICIT_WORK_TREE='0'))
    case('g-reg2-2-index', [['ls-files', '--stage']],
         variables=dict(outside, GIT_INDEX_FILE='.git/index'))
    case('g-reg2-2-index-symlink', [['ls-files', '--stage']],
         variables=dict(GIT_INDEX_FILE='alias/../index'),
         prepare=[('link', 'sub/deep', 'wt/alias'), ('copy', 'wt/.git/index', 'wt/index')])
    case('g-reg2-3-hook', [['commit', '-q', '--allow-empty', '-m', 'probe']],
         variables=outside, prepare=hook_prepare)
    case('g-reg2-3-hook-discovered-sub', [['commit', '-q', '--allow-empty', '-m', 'probe']],
         cwd='wt/sub/deep', prepare=hook_prepare)
    case('g-reg2-4-hash', [['hash-object', 'file']], variables=outside)
    case('g-reg2-4-apply', [['apply', '--stat', 'probe.patch']], variables=outside,
         prepare=[('write', 'wt/probe.patch', 'diff --git a/new b/new\nnew file mode 100644\n--- /dev/null\n+++ b/new\n@@ -0,0 +1 @@\n+new\n')])
    owner = dict(GIT_TEST_ASSUME_DIFFERENT_OWNER='1')
    user = pwd.getpwuid(os.getuid())
    # Keep every fixture under TMPDIR; ~user/../../tmp/... addresses it via
    # the passwd home without depending on HOME, which is deliberately isolated.
    home_depth = len(Path(user.pw_dir).parts) - 1
    named = '~' + user.pw_name + '/' + '../' * home_depth + '@ROOT@/wt'
    case('g-reg2-5-safe-user', [['rev-parse', '--show-toplevel']], variables=owner,
         opts=['-c', 'safe.directory=' + named.replace('/@ROOT@', '@ROOT@')])
    case('g-reg2-5-safe-unknown-user', [['rev-parse', '--show-toplevel']], variables=owner,
         opts=['-c', 'safe.directory=*', '-c', 'safe.directory=~ngit-no-such-user-7391/repo'])
    case('g-reg2-6-safe-symlink-dotdot', [['rev-parse', '--show-toplevel']], variables=owner,
         opts=['-c', 'safe.directory=/proc/self/root/..@ROOT@/wt'])
    for path, tag in (('missing/..', 'missing'), ('file/child', 'not-directory')):
        case('g-reg2-6-worktree-' + tag, [['status', '--short']],
             variables=dict(GIT_WORK_TREE=path))
    case('g-reg2-7-safe-unresolvable', [['rev-parse', '--show-toplevel']], variables=owner,
         opts=['-c', 'safe.directory=*', '-c', 'safe.directory=' + '/proc/self/root' * 35 + '@ROOT@/wt'])
    for count in (20, 21, 22, 32, 33, 34, 40, 41, 42):
        case('g-reg2-7-link-limit-' + str(count), [['rev-parse', '--show-toplevel']],
             variables=owner, opts=['-c', 'safe.directory=@ROOT@/link0'],
             prepare=[('link', 'wt' if i == count - 1 else 'link' + str(i + 1),
                       'link' + str(i)) for i in range(count)])
    case('g-reg2-8-ceiling-missing', [['rev-parse', '--show-toplevel']], cwd='wt/sub/deep',
         variables=dict(GIT_CEILING_DIRECTORIES='@ROOT@/wt/missing/..'))
    for reset in ('safe.directory', 'safe.directory='):
        case('g-disc2-1-reset-' + ('unset' if '=' not in reset else 'empty'),
             [['rev-parse', '--show-toplevel']], variables=owner,
             opts=['-c', 'safe.directory=*', '-c', reset])
    case('g-disc2-2-init-worktree', [['init', '-q']],
         variables=dict(GIT_DIR='.git', GIT_WORK_TREE='sub'))

    # G.12: the four edge regressions from the G3 close-out review.
    case('g-reg3-1', [['symbolic-ref', 'HEAD']],
         variables=dict(GIT_DIR='.git', GIT_OBJECT_DIRECTORY='external-objects'),
         prepare=[('move', 'wt/.git/objects', 'wt/external-objects')])
    case('g-reg3-2', [['status', '--short']],
         variables=dict(GIT_COMMON_DIR='.git'),
         prepare=[('git', 'wt', 'config', 'core.bare', 'true')])
    case('g-reg3-3', [['status', '--short']],
         prepare=[('git', 'wt', 'config', 'core.bare', 'true'),
                  ('git', 'wt', 'config', '--unset', 'core.repositoryformatversion')])
    case('g-reg3-4', [['rev-parse', '--show-toplevel']], variables=owner,
         opts=['-c', 'safe.directory=%(prefix)/' + '../' * 16 + '@ROOT@/wt'])

    # All 72 environment/cwd/-C combinations, each of the ten requested
    # dispatch families on independent fixtures. No family is sampled away.
    patch = 'diff --git a/matrix-new b/matrix-new\nnew file mode 100644\n--- /dev/null\n+++ b/matrix-new\n@@ -0,0 +1 @@\n+new\n'
    families = dict(status=['status', '--short'],
                    revparse=['rev-parse', '--git-dir', '--show-prefix', '--show-toplevel'],
                    lsfiles=['ls-files', '--stage'], hash=['hash-object', 'matrix-input'],
                    apply=['apply', 'matrix.patch'],
                    commit=['commit', '-q', '--allow-empty', '-m', 'matrix'],
                    init=['init', '-q'], clone=['clone', '-q', '--no-local', '@ROOT@/wt', '@ROOT@/cloned'],
                    config=['config', '--get', 'core.bare'], log=['log', '--oneline'])
    for gd_form in ('rel', 'abs'):
        for wt_form in ('rel', 'abs', 'unset'):
            for ix_form in ('rel', 'unset'):
                for cwd in ('wt', 'wt/sub/deep', 'outside'):
                    for use_c in (False, True):
                        effective = cwd
                        launch = '' if use_c else cwd
                        opts = ['-C', cwd] if use_c else []
                        variables = dict(GIT_DIR=(os.path.relpath('wt/.git', effective)
                                                  if gd_form == 'rel' else '@ROOT@/wt/.git'))
                        if wt_form != 'unset':
                            variables['GIT_WORK_TREE'] = (os.path.relpath('wt', effective)
                                                         if wt_form == 'rel' else '@ROOT@/wt')
                        if ix_form == 'rel':
                            variables['GIT_INDEX_FILE'] = os.path.relpath('wt/.git/index', effective)
                        prepare = hook_prepare + [('write', effective + '/matrix-input', 'input\n'),
                                                  ('write', effective + '/matrix.patch',
                                                   patch.replace('matrix-new',
                                                    ('sub/deep/' if effective == 'wt/sub/deep' and wt_form != 'unset' else '') + 'matrix-new'))]
                        for family, command in families.items():
                            name = '-'.join(('g-matrix', gd_form, wt_form, ix_form,
                                             cwd.replace('/', '-'), 'C' if use_c else 'cwd', family))
                            case(name, [command], cwd=launch, opts=opts, variables=variables, prepare=prepare)
    # Environment overrides are not all reparented by Git's chdir callback.
    # Exercise their actual internal paths AND their unchanged hook values.
    for key, value in (('GIT_OBJECT_DIRECTORY', '.git/objects'),
                       ('GIT_COMMON_DIR', '.git'), ('GIT_INDEX_FILE', '.git/index')):
        for gd in ('.git', '@ROOT@/wt/.git'):
            case('g-matrix-env-' + key + ('-relgd' if gd == '.git' else '-absgd'),
                 [['commit', '-q', '--allow-empty', '-m', 'env']],
                 variables=dict(GIT_DIR=gd, GIT_WORK_TREE='sub', **{key: value}),
                 prepare=hook_prepare + [('link', '../.git', 'wt/sub/.git')])
    for key, value in (('GIT_DIR', 'alias/../../.git'),
                       ('GIT_COMMON_DIR', 'alias/../../.git'),
                       ('GIT_OBJECT_DIRECTORY', 'alias/../../.git/objects')):
        case('g-matrix-env-' + key + '-symlink-dotdot', [['log', '--oneline']],
             variables={key: value}, prepare=[('link', 'sub/deep', 'wt/alias')])
    with ThreadPoolExecutor(max_workers=16) as pool:
        futures = [pool.submit(compare, *args) for args in pending]
        for future in futures:
            future.result()


def e2e(base, env):
    compare(base, env, 'e2e-separate-session', 'empty', (), 'wt',
            ['--git-dir=../meta', '--work-tree=.'], {}, [
                ['init', '-q', '-b', 'main'],
                [':write', 'wt/file', 'first\n'],
                [':write', 'wt/sub/deep/tracked', 'first\n'],
                ['add', '.'], ['commit', '-q', '-m', 'first'],
                [':write', 'wt/file', 'second\n'],
                ['status', '--porcelain=v2'], ['diff'],
                ['add', 'file'], ['commit', '-q', '-m', 'second'],
                ['log', '--oneline'],
                ['-C', 'sub/deep', '--git-dir=../../../meta', '--work-tree=../..',
                 'rev-parse', '--show-toplevel', '--show-prefix', '--git-dir',
                 '--absolute-git-dir', '--is-inside-work-tree', '--is-bare-repository', '--show-cdup'],
                ['-C', 'sub/deep', '--git-dir=../../../meta', '--work-tree=../..', 'status', '--short']])


def main(mode):
    if mode not in ('all', 'e2e'):
        raise ValueError('expected all or e2e')
    with tempfile.TemporaryDirectory(prefix='ngit-gitdir-') as tmp:
        base = Path(tmp)
        home = base / 'home'
        home.mkdir()
        (home / 'template').mkdir()
        env = {k: v for k, v in os.environ.items() if not k.startswith('GIT_')}
        env.update(HOME=str(home), XDG_CONFIG_HOME=str(home / 'xdg'), LC_ALL='C', TZ='UTC',
                   EDITOR='true', GIT_CONFIG_NOSYSTEM='1', GIT_TEMPLATE_DIR=str(home / 'template'),
                   GIT_AUTHOR_NAME='Fixture', GIT_AUTHOR_EMAIL='fixture@example.test',
                   GIT_COMMITTER_NAME='Fixture', GIT_COMMITTER_EMAIL='fixture@example.test',
                   GIT_AUTHOR_DATE='1700000000 +0000', GIT_COMMITTER_DATE='1700000000 +0000')
        (all_cases if mode == 'all' else e2e)(base, env)
    if not REPORT['comparisons']:
        raise RuntimeError('zero selected comparisons')


mode = sys.argv[1] if len(sys.argv) > 1 else 'all'
try:
    main(mode)
except Exception as error:
    REPORT['unexpected_errors'] += 1
    print('UNEXPECTED: ' + str(error), file=sys.stderr)
finally:
    REPORT['cases'].sort(key=lambda case: case['name'])
    REPORT['seconds'] = round(time.monotonic() - START, 3)
    out = Path(os.environ.get('NGIT_REPORT_DIR', ROOT / 'target/ux/reports'))
    out.mkdir(parents=True, exist_ok=True)
    (out / f'gitdir-{mode}.json').write_text(json.dumps(REPORT, indent=2) + '\n')
print(json.dumps(dict({k: v for k, v in REPORT.items() if k not in ('cases', 'pruned')}, pruned=len(REPORT['pruned']))))
sys.exit(bool(REPORT['failures'] or REPORT['unexpected_errors'] or not REPORT['comparisons']))
