#!/usr/bin/env python3
"""Compare editor, identity and partial-commit behavior against Git 2.47.3."""
import base64
import contextlib
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
BAD = re.compile(rb'void-function|void-variable|wrong-type-argument|Lisp error|[Ll]oader error|Cannot open load file|[Oo]ut of memory|resource exhausted')


def run(argv, repo, env):
    budget = min(30, START + 450 - time.monotonic())
    if budget <= 0:
        raise RuntimeError('overall deadline')
    proc = subprocess.Popen(argv, cwd=repo, env=env, stdin=subprocess.DEVNULL,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, start_new_session=True)
    try:
        out, err = proc.communicate(timeout=budget)
    except subprocess.TimeoutExpired:
        os.killpg(proc.pid, signal.SIGKILL)
        proc.communicate()
        raise RuntimeError('command timeout: ' + repr(argv))
    return [proc.returncode, out.decode('utf-8', 'surrogateescape'),
            err.decode('utf-8', 'surrogateescape')]


def setup(repo, env, *args):
    result = run(['git', *args], repo, env)
    if result[0]:
        raise RuntimeError('fixture: ' + repr((args, result)))
    return result[1].strip()


def write(repo, name, data):
    path = repo / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(data)


def state(repo, env):
    """Public state, saved editor templates, and all reachable commit/tag objects."""
    result = {name: run(['git', *args], repo, env) for name, args in {
        'refs': ['show-ref', '--head'], 'head': ['symbolic-ref', '-q', 'HEAD'],
        'index': ['ls-files', '-s'], 'status': ['status', '--porcelain=v1', '--ignored'],
        'config': ['config', '--local', '--list']}.items()}
    files = {}
    alternate_index = env.get('GIT_INDEX_FILE', '.git/index')
    for path in sorted(repo.rglob('*')):
        rel = path.relative_to(repo)
        if rel.parts[:2] in (('.git', 'objects'), ('.git', 'logs')) or str(rel) in ('.git/index', alternate_index):
            continue
        if path.is_symlink():
            files[str(rel)] = ['link', os.readlink(path)]
        elif path.is_file():
            files[str(rel)] = base64.b64encode(path.read_bytes()).decode()
    result['files'] = files
    objects = {}
    for line in run(['git', 'rev-list', '--objects', '--all'], repo, env)[1].splitlines():
        oid = line.split()[0]
        kind = run(['git', 'cat-file', '-t', oid], repo, env)[1].strip()
        if kind in ('commit', 'tag'):
            objects[oid] = run(['git', 'cat-file', '-p', oid], repo, env)
    result['objects'] = objects
    return result


def compare(name, repo, env, args, cwd='.'):
    if os.environ.get('UX_COMMIT_MATCH') and os.environ['UX_COMMIT_MATCH'] not in name:
        return
    snapshot = repo.parent / 'snapshot'
    shutil.copytree(repo, snapshot, symlinks=True)
    expected = run(['git', *args], repo / cwd, env)
    expected_state = state(repo, env)
    shutil.rmtree(repo)
    shutil.copytree(snapshot, repo, symlinks=True)
    shutil.rmtree(snapshot)
    actual = run([*COMMAND, *args], repo / cwd, env)
    actual_state = state(repo, env)
    equal = expected == actual and expected_state == actual_state
    unexpected = actual[0] < 0 or bool(BAD.search((actual[1] + actual[2]).encode(errors='replace')))
    REPORT['comparisons'] += 1
    REPORT['exact'] += int(equal)
    REPORT['failures'] += int(not equal)
    REPORT['unexpected_errors'] += int(unexpected)
    case = dict(name=name, equal=equal, expected=expected, actual=actual)
    if not equal:
        case['expected_state'] = expected_state
        case['actual_state'] = actual_state
        print('FAIL ' + name + ': ' + repr((expected, actual)))
    REPORT['cases'].append(case)


def editor_script(repo, env, action='replace', text='edited subject\n\nbody\n# ignored\n', label='editor'):
    script = Path(env['HOME']) / label
    operation = {'replace': "printf %s " + shlex.quote(text) + ' > "$1"',
                 'append': "printf %s " + shlex.quote(text) + ' >> "$1"',
                 'noop': ':', 'fail': 'exit 7'}[action]
    script.write_text('#!/bin/sh\ncp "$1" .git/editor-before\nprintf "%s\\n" "$@" > .git/editor-args\n' + operation + '\n')
    script.chmod(0o755)
    env['GIT_EDITOR'] = str(script)
    return str(script)


@contextlib.contextmanager
def fixture():
    with tempfile.TemporaryDirectory(prefix='ngit-ux-commit-') as tmp:
        root = Path(tmp)
        repo = root / 'repo'
        repo.mkdir()
        home = root / 'home'
        home.mkdir()
        (home / '.gitconfig').write_text('[user]\n\tname = Fixture\n\temail = fixture@example.test\n')
        env = {k: v for k, v in os.environ.items()
               if not k.startswith(('GIT_', 'NGIT_')) and k != 'VISUAL'}
        env.update(HOME=str(home), XDG_CONFIG_HOME=str(home / 'xdg'), TZ='UTC', LC_ALL='C',
                   GIT_CONFIG_NOSYSTEM='1', GIT_AUTHOR_DATE='1700000000 +0000',
                   GIT_COMMITTER_DATE='1700000000 +0000', USER='fixture', LOGNAME='fixture',
                   NAME='Fixture Environment', EMAIL='environment@example.test')
        if NO_ELC is not None:
            env['NGIT_NO_ELC'] = NO_ELC
        setup(repo, env, 'init', '-q', '-b', 'main')
        for name in ('file', 'other', 'dir/nested'):
            write(repo, name, 'base\n')
        setup(repo, env, 'add', '.')
        setup(repo, env, 'commit', '-qm', 'base')
        yield repo, env


def staged(repo, env):
    write(repo, 'file', 'staged\n')
    setup(repo, env, 'add', 'file')


def divergent(repo, env):
    setup(repo, env, 'branch', 'side')
    write(repo, 'file', 'main\n')
    setup(repo, env, 'add', 'file')
    setup(repo, env, 'commit', '-qm', 'main work')
    setup(repo, env, 'switch', '-q', 'side')
    write(repo, 'other', 'side\n')
    setup(repo, env, 'add', 'other')
    setup(repo, env, 'commit', '-qm', 'side work')
    setup(repo, env, 'switch', '-q', 'main')


def editor():
    for earlier in (';', '@'):
        for message in ('# keep', earlier + ' remove\n# keep'):
            with fixture() as (repo, env):
                editor_script(repo, env, action='noop')
                compare('f5-tag-auto-earlier-' + earlier + repr(message), repo, env,
                        ['-c', 'core.commentChar=' + earlier, '-c', 'core.commentChar=auto',
                         'tag', '-a', 'v1', '-m', message])
    for action, text in (('noop', ''), ('replace', '# only a comment\n'),
                         ('replace', 'tag message\n# ignored\n')):
        with fixture() as (repo, env):
            setup(repo, env, 'config', 'core.commentChar', 'auto')
            editor_script(repo, env, action=action, text=text)
            compare('rereview8-tag-auto-' + action + repr(text), repo, env, ['tag', '-a', 'v1'])
    with fixture() as (repo, env):
        staged(repo, env)
        script = Path(editor_script(repo, env))
        script.write_text(script.read_text().removeprefix('#!/bin/sh\n'))
        compare('audit9-editor-no-shebang', repo, env, ['commit'])
    with fixture() as (repo, env):
        setup(repo, env, 'config', 'core.bare', 'true')
        script = Path(env['HOME']) / 'bare-editor'
        script.write_text('#!/bin/sh\nprintf "edited\\n" > "$1"\n')
        script.chmod(0o755)
        env['GIT_EDITOR'] = str(script)
        compare('audit10-bare-tag-editor', repo, env, ['tag', '-a', '-m', 'message', '-e', 'v1'], '.git')
    for message in ('automatic update', '# subject\n; body', '# one\n; two\n@ three\n! four\n$ five\n% six\n^ seven\n& eight\n| nine\n: ten'):
        with fixture() as (repo, env):
            staged(repo, env)
            setup(repo, env, 'config', 'core.commentChar', 'auto')
            env['GIT_EDITOR'] = ':'
            compare('audit11-comment-auto-' + message.splitlines()[0], repo, env,
                    ['commit', '-e', '-m', message])
    for source in ('GIT_EDITOR', 'core.editor', 'VISUAL', 'EDITOR'):
        with fixture() as (repo, env):
            staged(repo, env)
            script = editor_script(repo, env)
            env.pop('GIT_EDITOR')
            env['TERM'] = 'xterm'
            env['EDITOR'] = 'false'
            if source == 'core.editor':
                setup(repo, env, 'config', 'core.editor', script)
            else:
                env[source] = script
            if source == 'GIT_EDITOR':
                setup(repo, env, 'config', 'core.editor', 'false')
                env['VISUAL'] = 'false'
            elif source == 'core.editor':
                env['VISUAL'] = 'false'
            compare('editor-precedence-' + source, repo, env, ['commit'])
    for label, args, action, text, config in [
        ('plain', [], 'replace', 'subject\n# comment\n\nbody  \n', None),
        ('empty', [], 'replace', '\n# only a comment\n', None),
        ('failure', [], 'fail', '', None),
        ('colon', [], 'noop', '', None),
        ('empty-editor', [], 'noop', '', None),
        ('edit-message', ['-e', '-m', 'provided'], 'append', '\nextra\n', None),
        ('edit-file', ['-e', '-F', 'message'], 'append', '\nextra\n', None),
        ('no-edit', ['--no-edit', '-m', 'provided'], 'fail', '', None),
        ('no-edit-empty', ['--no-edit'], 'fail', '', None),
        ('amend', ['--amend'], 'replace', 'amended\n', None),
        ('amend-no-edit', ['--amend', '--no-edit'], 'fail', '', None),
        ('template', ['-t', 'message'], 'append', '\ncompleted\n', None),
        ('template-unchanged', ['-t', 'message'], 'noop', '', None),
        ('template-unchanged-allowed', ['--allow-empty-message', '-t', 'message'], 'noop', '', None),
        ('config-template', [], 'append', '\ncompleted\n', ('commit.template', 'message')),
        ('comments', [], 'replace', 'subject\n; comment\n', ('core.commentChar', ';')),
        ('whitespace', ['--cleanup=whitespace'], 'replace', 'subject  \n# keep\n', None),
        ('verbatim', ['--cleanup=verbatim'], 'replace', '\nsubject  \n# keep\n\n', None),
        ('strip-message', ['--cleanup=strip', '-m', 'subject\n# remove'], 'fail', '', None),
        ('scissors', ['--cleanup=scissors'], 'replace', 'subject\n# ------------------------ >8 ------------------------\ncut\n', None),
        ('config-cleanup', [], 'replace', 'subject\n# keep\n', ('commit.cleanup', 'whitespace')),
        ('default-cleanup', ['--cleanup=default', '-m', 'subject\n# keep'], 'fail', '', None),
        ('file-cleanup', ['--cleanup=strip', '-F', 'message'], 'fail', '', None),
    ]:
        with fixture() as (repo, env):
            staged(repo, env)
            write(repo, 'message', 'template subject\n')
            editor_script(repo, env, action, text)
            if label == 'colon':
                env['GIT_EDITOR'] = ':'
            if label == 'empty-editor':
                env['GIT_EDITOR'] = ''
            if config:
                setup(repo, env, 'config', *config)
            compare('commit-' + label, repo, env, ['commit', *args])
    with fixture() as (repo, env):
        staged(repo, env)
        script = editor_script(repo, env)
        # Shell operators and an explicit argument exercise sh -c and "$@".
        env['GIT_EDITOR'] = 'MARK=present ' + shlex.quote(script) + ' "$@"; :'
        compare('editor-shell-arguments', repo, env, ['commit'])
    with fixture() as (repo, env):
        staged(repo, env)
        script = Path(editor_script(repo, env))
        script.write_text(script.read_text().replace('#!/bin/sh\n',
                          '#!/bin/sh\ntest "$1" = "fixed argument" || exit 8\nshift\n', 1))
        env['GIT_EDITOR'] = shlex.quote(str(script)) + ' "fixed argument"'
        compare('editor-fixed-argument', repo, env, ['commit'])
    with fixture() as (repo, env):
        staged(repo, env)
        script = editor_script(repo, env)
        env.pop('GIT_EDITOR')
        env.pop('TERM', None)
        env.update(VISUAL='false', EDITOR=script)
        compare('editor-unset-term', repo, env, ['commit'])
    for term in ('dumb', None):
        with fixture() as (repo, env):
            staged(repo, env)
            ignored_editor = editor_script(repo, env, 'fail')
            env.pop('GIT_EDITOR')
            env['VISUAL'] = ignored_editor
            env.pop('EDITOR', None)
            env.pop('TERM', None)
            if term:
                env['TERM'] = term
            compare('editor-no-editor-' + str(term), repo, env, ['commit'])
    for args in (['-a'], ['--only', 'file'], ['--amend']):
        with fixture() as (repo, env):
            write(repo, 'file', 'working\n')
            script = editor_script(repo, env)
            with open(script, 'a') as out:
                out.write('git ls-files -s > .git/editor-index\n'
                          'printf "%s\\n%s\\n%s\\n" "$GIT_AUTHOR_NAME" "$GIT_AUTHOR_EMAIL" "$GIT_AUTHOR_DATE" > .git/editor-author\n')
            compare('editor-index-' + args[0], repo, env, ['commit', *args])
    with fixture() as (repo, env):
        shutil.copyfile(repo / '.git/index', repo / '.git/alternate-index')
        env['GIT_INDEX_FILE'] = '.git/alternate-index'
        write(repo, 'file', 'working\n')
        script = editor_script(repo, env)
        with open(script, 'a') as out:
            out.write('git ls-files -s > .git/editor-index\n')
        compare('editor-alternate-index', repo, env, ['commit', '-a'])
    for label, args, auto in [('default', [], 'yes'), ('no-edit', ['--no-edit'], 'yes'),
                              ('edit', ['-e', '-m', 'custom'], None), ('auto-no', [], 'no'),
                              ('failure', [], 'yes'), ('empty', [], 'yes')]:
        with fixture() as (repo, env):
            divergent(repo, env)
            editor_script(repo, env, 'fail' if label == 'failure' else 'replace',
                          '\n# empty\n' if label == 'empty' else 'edited merge\n')
            if auto:
                env['GIT_MERGE_AUTOEDIT'] = auto
            compare('merge-editor-' + label, repo, env, ['merge', *args, 'side'])
    for label, args in [('edit', []), ('no-edit', ['--no-edit'])]:
        with fixture() as (repo, env):
            divergent(repo, env)
            setup(repo, env, 'merge', '--no-commit', 'side')
            editor_script(repo, env, text='concluded merge\n')
            compare('commit-merge-' + label, repo, env, ['commit', *args])
    for hook in (False, True):
        with fixture() as (repo, env):
            divergent(repo, env)
            setup(repo, env, 'merge', '--no-commit', 'side')
            write(repo, 'file', 'after merge\n')
            editor_script(repo, env, 'noop')
            if hook:
                path = repo / '.git/hooks/pre-commit'
                path.write_text('#!/bin/sh\nprintf "hook\\n" > hook-file\ngit add hook-file\n')
                path.chmod(0o755)
            compare('commit-merge-all-' + str(hook), repo, env, ['commit', '-a'])
    for cleanup in ('whitespace', 'verbatim', 'scissors'):
        with fixture() as (repo, env):
            divergent(repo, env)
            editor_script(repo, env, 'noop')
            setup(repo, env, 'config', 'commit.cleanup', cleanup)
            setup(repo, env, 'config', 'core.commentString', '//')
            compare('merge-cleanup-' + cleanup, repo, env, ['merge', '-e', 'side'])
        with fixture() as (repo, env):
            divergent(repo, env)
            setup(repo, env, 'merge', '--no-commit', 'side')
            editor_script(repo, env, 'noop')
            setup(repo, env, 'config', 'core.commentString', '//')
            compare('commit-merge-cleanup-' + cleanup, repo, env, ['commit', '--cleanup=' + cleanup])
        for editing in (False, True):
            with fixture() as (repo, env):
                divergent(repo, env)
                editor_script(repo, env, 'noop')
                compare('merge-message-cleanup-' + cleanup + str(editing), repo, env,
                        ['merge', '-e' if editing else '--no-edit', '--cleanup=' + cleanup,
                         '-m', '  subject  \n\nbody \n\n', 'side'])
    for action in ('replace', 'noop', 'fail'):
        with fixture() as (repo, env):
            editor_script(repo, env, action, 'tag subject\n# remove\n')
            compare('tag-editor-' + action, repo, env, ['tag', '-a', 'v1'])
    for annotated in (False, True):
        with fixture() as (repo, env):
            setup(repo, env, 'tag', *(['-a', '-m', 'old message'] if annotated else []), 'old')
            env['GIT_EDITOR'] = ':'
            compare('tag-force-edit-' + str(annotated), repo, env, ['tag', '-a', '-f', 'old'])
    with fixture() as (repo, env):
        write(repo, '.git/TAG_EDITMSG', 'saved draft\n')
        compare('tag-lightweight-preserves-draft', repo, env, ['tag', 'light'])


def identity():
    for label, config, values in [
        ('overrides', None, dict(GIT_AUTHOR_NAME='Author', GIT_AUTHOR_EMAIL='a@test', GIT_COMMITTER_NAME='Committer', GIT_COMMITTER_EMAIL='c@test')),
        ('email', 'email', {}), ('guess-name', 'name', {}), ('guess-all', 'both', {}),
        ('config-only-name', 'name', dict(CONFIG_ONLY='true')),
        ('config-only-email', 'email', dict(CONFIG_ONLY='true')),
        ('committer-missing-name', 'name', dict(CONFIG_ONLY='true', GIT_AUTHOR_NAME='A', GIT_AUTHOR_EMAIL='a@test')),
        ('committer-missing-email', 'email', dict(CONFIG_ONLY='true', GIT_AUTHOR_NAME='A', GIT_AUTHOR_EMAIL='a@test')),
        ('config-only-overrides', 'both', dict(CONFIG_ONLY='true', GIT_AUTHOR_NAME='A', GIT_AUTHOR_EMAIL='a@test', GIT_COMMITTER_NAME='C', GIT_COMMITTER_EMAIL='c@test')),
        ('guess-email', 'email', dict(EMAIL='')),
        ('empty-name', None, dict(GIT_AUTHOR_NAME='')),
        ('empty-committer-name', None, dict(GIT_COMMITTER_NAME='')),
        ('empty-email', None, dict(GIT_AUTHOR_EMAIL='')),
        ('sanitise', None, dict(GIT_AUTHOR_NAME='  A <B>\n ', GIT_AUTHOR_EMAIL=' a<>@test ')),
    ]:
        with fixture() as (repo, env):
            staged(repo, env)
            if config:
                text = '[user]\n'
                if config not in ('name', 'both'):
                    text += 'name = Fixture\n'
                if config not in ('email', 'both'):
                    text += 'email = fixture@example.test\n'
                (Path(env['HOME']) / '.gitconfig').write_text(text)
            values = values.copy()
            if values.pop('CONFIG_ONLY', None):
                setup(repo, env, 'config', 'user.useConfigOnly', 'true')
            env.update(values)
            compare('identity-' + label, repo, env, ['commit', '-m', 'identity'])
    dates = ['Tue, 14 Nov 2023 22:13:20 +0000', '14 Nov 2023 17:13:20 -0500',
             'Tue Nov 14 22:13:20 2023 +0000', 'Tue, 14 Nov 2023 17:13:20 EST',
             'Tue, 14 Nov 2023 22:13:20 +0000 (UTC)', '20231114T221320Z',
             '2023-11-14T22:13:20Z', '2023-11-15T07:13:20+09:00', '2023-11-14 22:13',
             '2023-11-14 22:13:20.123 +0000', '2023.11.14 22:13:20',
             '11/14/2023 22:13:20', '@1700000000 +0530', '1700000000 -0330',
             'now', 'yesterday', '2 days ago', 'last Tuesday', 'not-a-date', '2023-11-14']
    for date in dates:
        for command in ('commit', 'tag'):
            with fixture() as (repo, env):
                staged(repo, env)
                env.update(GIT_AUTHOR_DATE=date, GIT_COMMITTER_DATE=date)
                args = ['commit', '-m', 'dated'] if command == 'commit' else ['tag', '-a', '-m', 'dated', 'dated']
                compare(command + '-date-' + date, repo, env, args)


def paths():
    for opts in (['--only'], ['--include']):
        with fixture() as (repo, env):
            (repo / 'file').unlink()
            write(repo, 'file/local', 'precious\n')
            compare('audit8-path-directory-' + opts[0], repo, env,
                    ['commit', *opts, '-m', 'change', '--', 'file'])
    for label, args, cwd in [
        ('implicit', ['file'], '.'), ('only', ['--only', 'file'], '.'),
        ('short-only', ['-o', 'file'], '.'), ('include', ['--include', 'file'], '.'),
        ('short-include', ['-i', 'file'], '.'), ('all-error', ['-a', 'file'], '.'),
        ('only-all-error', ['-a', '--only', 'file'], '.'),
        ('glob', [':(glob)**/nested'], '.'), ('exclude', ['.', ':(exclude)other'], '.'),
        ('icase', [':(icase)FILE'], '.'), ('literal', [':(literal)file'], '.'),
        ('directory', ['dir'], '.'), ('subdir', ['nested'], 'dir'),
        ('top-subdir', [':(top)file'], 'dir'), ('parent-subdir', ['../file'], 'dir'),
        ('new', ['new'], '.'), ('intent', ['intent'], '.'), ('deleted', ['deleted'], '.'),
        ('untracked-error', ['untracked'], '.'), ('missing-error', ['missing'], '.'),
        ('amend-only-no-path', ['--amend', '--only'], '.'),
        ('only-no-path-error', ['--only'], '.'), ('include-no-path-error', ['--include'], '.'),
        ('only-include-error', ['--only', '--include', 'file'], '.'),
        ('include-all-error', ['--include', '-a', 'file'], '.'),
        ('include-untracked-error', ['--include', 'untracked'], '.'),
        ('allow-empty-only', ['--allow-empty', '--only'], '.'),
        ('ordinary-wildcard', ['d*/nest*'], '.'),
    ]:
        with fixture() as (repo, env):
            write(repo, 'deleted', 'base\n')
            setup(repo, env, 'add', 'deleted')
            setup(repo, env, 'commit', '-qm', 'deletion base')
            for name in ('file', 'other', 'dir/nested'):
                write(repo, name, 'staged\n')
            write(repo, 'new', 'new\n')
            setup(repo, env, 'add', '.')
            for name in ('file', 'other', 'dir/nested'):
                write(repo, name, 'working\n')
            write(repo, 'intent', 'intent\n')
            setup(repo, env, 'add', '-N', 'intent')
            write(repo, 'untracked', 'untracked\n')
            (repo / 'deleted').unlink()
            compare('paths-' + label, repo, env, ['commit', '-m', 'partial', *args], cwd)
    for include in (False, True):
        with fixture() as (repo, env):
            staged(repo, env)
            write(repo, 'other', 'working\n')
            editor_script(repo, env, 'fail')
            compare('paths-editor-abort-' + str(include), repo, env,
                    ['commit', *(['--include'] if include else []), 'other'])
    for include in (False, True):
        with fixture() as (repo, env):
            staged(repo, env)
            write(repo, 'other', 'working\n')
            hook = repo / '.git/hooks/post-commit'
            hook.write_text('#!/bin/sh\ngit ls-files -s > .git/hook-index\nprintf "%s\\n" "$GIT_INDEX_FILE" > .git/hook-index-path\nprintf "hook\\n" > hooked\ngit add hooked\n')
            hook.chmod(0o755)
            compare('paths-post-commit-' + str(include), repo, env,
                    ['commit', '-m', 'hook', *(['--include'] if include else []), 'other'])
    for include in (False, True):
        with fixture() as (repo, env):
            setup(repo, env, 'rm', '--cached', 'file')
            compare('paths-staged-deletion-' + str(include), repo, env,
                    ['commit', '-m', 'partial', *(['--include'] if include else []), 'file'])
    for kind in ('MERGE_HEAD', 'CHERRY_PICK_HEAD', 'REVERT_HEAD'):
        with fixture() as (repo, env):
            staged(repo, env)
            write(repo, '.git/' + kind, setup(repo, env, 'rev-parse', 'HEAD') + '\n')
            compare('paths-replay-' + kind, repo, env, ['commit', '-m', 'partial', 'file'])


def e2e():
    with fixture() as (repo, env):
        divergent(repo, env)
        env['GIT_MERGE_AUTOEDIT'] = 'yes'
        staged(repo, env)
        editor_script(repo, env, text='session commit\n')
        compare('session-commit', repo, env, ['commit'])
        editor_script(repo, env, text='session amend\n')
        compare('session-amend', repo, env, ['commit', '--amend'])
        editor_script(repo, env, text='session merge\n')
        compare('session-merge', repo, env, ['merge', 'side'])
        editor_script(repo, env, text='session tag\n')
        compare('session-tag', repo, env, ['tag', '-a', 'session'])


mode = sys.argv[1] if len(sys.argv) > 1 else 'editor'
try:
    if mode not in ('editor', 'identity', 'paths', 'e2e'):
        raise ValueError('unknown mode ' + mode)
    globals()[mode]()
    if not REPORT['comparisons']:
        raise RuntimeError('zero comparisons')
except Exception as error:
    REPORT['unexpected_errors'] += 1
    print('UNEXPECTED: ' + str(error), file=sys.stderr)
finally:
    REPORT['seconds'] = round(time.monotonic() - START, 3)
    output = Path(os.environ.get('NGIT_REPORT_DIR', ROOT / 'target/ux/reports'))
    output.mkdir(parents=True, exist_ok=True)
    (output / ('ux-commit-' + mode + '.json')).write_text(json.dumps(REPORT, indent=2) + '\n')
print('ux-commit/%s comparisons=%d exact=%d intended=%d failures=%d unexpected_errors=%d' %
      (mode, REPORT['comparisons'], REPORT['exact'], len(REPORT['intended']),
       REPORT['failures'], REPORT['unexpected_errors']))
sys.exit(bool(REPORT['failures'] or REPORT['unexpected_errors'] or not REPORT['comparisons']))
