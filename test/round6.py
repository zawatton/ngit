#!/usr/bin/env python3
"""Exact asymmetric patch names and pull precondition comparisons."""
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
DEADLINE = START + 290
REPORT = dict(comparisons=0, exact=0, intended={}, failures=0, unexpected_errors=0, cases=[])
BAD = re.compile(rb'void-function|void-variable|wrong-type-argument|Lisp error|[Ll]oader error|Cannot open load file|[Oo]ut of memory|resource exhausted')


def run(argv, repo, env, data=None):
    budget = min(35, DEADLINE - time.monotonic())
    if budget <= 0:
        raise RuntimeError('overall deadline')
    p = subprocess.Popen(argv, cwd=repo, env=env, stdin=subprocess.PIPE,
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE, start_new_session=True)
    try:
        out, err = p.communicate(data, timeout=budget)
    except subprocess.TimeoutExpired:
        os.killpg(p.pid, signal.SIGKILL)
        p.communicate()
        raise RuntimeError('command timeout: ' + repr(argv))
    return dict(exit=p.returncode, stdout_b64=base64.b64encode(out).decode(),
                stderr_b64=base64.b64encode(err).decode())


def setup(args, repo, env, data=None):
    result = run(['git', *args], repo, env, data)
    if result['exit']:
        raise RuntimeError('fixture failed: ' + repr((args, result)))
    return base64.b64decode(result['stdout_b64']).decode().strip()


def write(repo, path, text):
    dest = repo / path
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_text(text)


def state(repo, env):
    """Compare all worktree/config/ref/state bytes and the logical index."""
    files = {}
    for p in sorted(repo.rglob('*')):
        rel = p.relative_to(repo)
        if rel.parts[:2] == ('.git', 'objects') or str(rel) == '.git/index':
            continue
        # am's private bookkeeping differs; its public refs/index/worktree do not.
        if rel.parts[:2] == ('.git', 'rebase-apply') and (repo / '.git/rebase-apply/applying').exists():
            continue
        if p.is_symlink():
            files[str(rel)] = ['link', os.readlink(p)]
        elif p.is_file():
            files[str(rel)] = [p.stat().st_mode & 0o777, base64.b64encode(p.read_bytes()).decode()]
    return dict(files=files, index=run(['git', 'ls-files', '--stage'], repo, env),
                refs=run(['git', 'show-ref', '--head'], repo, env))


def protected(repo):
    """Include raw index, AUTO_MERGE, all refs and all worktree bytes."""
    return {str(p.relative_to(repo)): (os.readlink(p) if p.is_symlink()
                                      else base64.b64encode(p.read_bytes()).decode())
            for p in repo.rglob('*') if (p.is_symlink() or p.is_file())
            and p.relative_to(repo).parts[:2] != ('.git', 'objects')}


def without_fetch(files):
    """Allow fetch's own metadata updates on a subsequent merge refusal."""
    return {k: v for k, v in files.items() if k != '.git/FETCH_HEAD'
            and not k.startswith(('.git/refs/remotes/', '.git/logs/refs/remotes/'))}


def compare(name, args, repo, env, data=None, immutable=False):
    if MATCH and MATCH not in name:
        return
    backup = repo.parent / 'snapshot'
    shutil.copytree(repo, backup, symlinks=True)
    before = protected(repo)
    expected = run(['git', *args], repo, env, data)
    expected_state = state(repo, env)
    expected_files = protected(repo)
    expected_unchanged = before == expected_files
    expected_protected = without_fetch(before) == without_fetch(expected_files)
    shutil.rmtree(repo)
    shutil.copytree(backup, repo, symlinks=True)
    # Restore real index stat information before measuring refusal immutability.
    # This does not stage or alter any content, including unresolved entries.
    run(['git', 'update-index', '-q', '--refresh'], repo, env)
    before_actual = protected(repo)
    actual = run([*COMMAND, *args], repo, env, data)
    actual_state = state(repo, env)
    actual_files = protected(repo)
    actual_unchanged = before_actual == actual_files
    actual_protected = without_fetch(before_actual) == without_fetch(actual_files)
    unexpected = actual['exit'] < 0 or bool(BAD.search(base64.b64decode(actual['stderr_b64']) +
                                                       base64.b64decode(actual['stdout_b64'])))
    equal = (expected == actual and expected_state == actual_state and
             (not immutable or (expected_unchanged and actual_unchanged)) and
             (not name.startswith('pull-cherry-resolved-') or
              (expected_protected and actual_protected)))
    REPORT['comparisons'] += 1
    REPORT['exact'] += 1
    REPORT['failures'] += int(not equal or unexpected)
    REPORT['unexpected_errors'] += int(unexpected)
    REPORT['cases'].append(dict(name=name, argv=args, expected=expected, actual=actual,
        equal=equal, state_equal=expected_state == actual_state,
        expected_unchanged=expected_unchanged, actual_unchanged=actual_unchanged,
        expected_protected=expected_protected, actual_protected=actual_protected,
        expected_state=expected_state, actual_state=actual_state))
    if not equal:
        print('FAIL ' + name + ': ' + repr((expected, actual, expected_state == actual_state,
                                           expected_unchanged, actual_unchanged)), flush=True)
    shutil.rmtree(repo)
    shutil.copytree(backup, repo, symlinks=True)
    shutil.rmtree(backup)
    run(['git', 'update-index', '-q', '--refresh'], repo, env)


VARIANTS = ([], ['--check'], ['--index'], ['--index', '--check'],
            ['--cached'], ['--cached', '--check'])


def headers(repo, env):
    for path in ('file', 'dir/file', 'deep/dir/file', 'a/dir/file', 'a/deep/dir/file',
                 'a/moved', 'moved', 'dir/moved'):
        write(repo, path, 'base\n')
    setup(['add', '.'], repo, env)
    setup(['commit', '-qm', 'preimages'], repo, env)
    for p in range(4):
        for reverse in (False, True):
            # At p3 exactly one side still has a name; p2 tests the review case.
            old, new = ('deep/dir/file', 'moved') if p == 3 else ('dir/file', 'new')
            if reverse:
                old, new = new, old.replace('file', 'renamed')
            for kind in ('rename', 'copy'):
                patch = (f'diff --git a/{old} b/{new}\nsimilarity index 100%\n'
                         f'{kind} from {old}\n{kind} to {new}\n').encode()
                for opts in VARIANTS:
                    compare(f'headers-p{p}-{reverse}-{kind}-' + '-'.join(opts),
                            ['apply', f'-p{p}', *opts], repo, env, data=patch,
                            immutable=p >= 2)
    for reverse in (False, True):
        old, new = ('a/dir/file', 'b/new') if not reverse else ('a/new', 'b/dir/moved')
        patch = (f'diff --git {old} {new}\n--- {old}\n+++ {new}\n'
                 '@@ -1 +1 @@\n-base\n+changed\n').encode()
        for opts in VARIANTS:
            compare(f'headers-text-{reverse}-' + '-'.join(opts),
                    ['apply', '-p2', *opts], repo, env, data=patch, immutable=True)
    for deletion in (False, True):
        patch = ('diff --git a/dir/file b/dir/file\n' +
                 ('deleted file mode 100644\n--- a/dir/file\n+++ /dev/null\n@@ -1 +0,0 @@\n-base\n'
                  if deletion else
                  'new file mode 100644\n--- /dev/null\n+++ b/dir/created\n@@ -0,0 +1 @@\n+created\n'))
        # A matching default name must agree with the creation's +++ spelling.
        if not deletion:
            patch = patch.replace('a/dir/file b/dir/file', 'a/dir/created b/dir/created')
        for opts in (['--check'], ['--index', '--check'], ['--cached', '--check']):
            compare(f'headers-allowed-{deletion}-' + '-'.join(opts),
                    ['apply', '-p2', *opts], repo, env, data=patch.encode())
    for kind in ('rename', 'copy'):
        for missing in ('from', 'to'):
            patch = (f'diff --git a/dir/file b/new\nsimilarity index 100%\n'
                     f'{kind} from ' + ('' if missing == 'from' else 'dir/file') + '\n' +
                     f'{kind} to ' + ('' if missing == 'to' else 'new') + '\n')
            mail = ('From ' + '0' * 40 + ' Mon Sep 17 00:00:00 2001\n'
                    'From: Fixture <fixture@example.test>\n'
                    'Date: Tue, 14 Nov 2023 22:13:20 +0000\n'
                    'Subject: [PATCH] missing name\n\n' + patch)
            compare(f'headers-am-{kind}-{missing}', ['am'], repo, env, data=mail.encode())
    patch = (b'diff --git a/dir/other.txt b/dir/other.txt\nold mode 100644\nnew mode 100755\n'
             b'diff --git a/dir/file b/new\nsimilarity index 100%\n'
             b'rename from dir/file\nrename to new\n')
    for opts in VARIANTS:
        compare('headers-atomic-' + '-'.join(opts), ['apply', '-p2', *opts],
                repo, env, data=patch, immutable=True)
    for kind in ('rename', 'copy'):
        patch = (f'diff --git a/dir/file b/new\nsimilarity index 100%\n'
                 f'{kind} from dir/file\n{kind} to new\n'
                 'Binary files a/dir/file and b/new differ\n').encode()
        compare('headers-binary-' + kind, ['apply', '-p2', '--check'],
                repo, env, data=patch, immutable=True)


def mail_starts(repo, env):
    traditional = '--- /dev/null\n+++ b/one\n@@ -0,0 +1 @@\n+first\n'
    git = ('diff --git a/two b/two\nnew file mode 100644\n'
           '--- /dev/null\n+++ b/two\n@@ -0,0 +1 @@\n+second\n')
    header = ('From: Auditor <audit@example.invalid>\n'
              'Date: Mon, 1 Jan 2024 00:00:00 +0000\n'
              'Subject: [PATCH] mixed patch\n\n')
    envelope = 'From ' + '0' * 40 + ' Mon Sep 17 00:00:00 2001\n'
    # Keep the review's mail bytes as a separate control as well as mbox input.
    compare('mail-review-raw', ['am'], repo, env,
            data=(header + traditional + git).encode())
    shapes = dict(traditional=traditional, git=git,
                  traditional_git=traditional + git, git_traditional=git + traditional)
    for name, patch in shapes.items():
        for separator in ('', '---\n'):
            for body in ('', 'Explanation.\nSubject: body text\nX-Note: ---\n\n'):
                label = f'mail-{name}-sep{bool(separator)}-body{bool(body)}'
                compare(label, ['am'], repo, env,
                        data=(envelope + header + body + separator + patch).encode())
    for name, prefix in (('diff-u', 'diff -u /dev/null b/one\n'),
                         ('index', 'Index: one\n'),
                         ('space-separator', '--- \t\n')):
        compare('mail-' + name, ['am'], repo, env,
                data=(envelope + header + prefix + traditional + git).encode())
    for name, prefix in (('traditional-header-text', '--- prose\nnot a patch header\n'),
                         ('diff-header-text', 'diff - prose\n'),
                         ('index-header-text', 'Index: prose\n')):
        # mailinfo starts the patch even when apply skips this initial line.
        malformed = ('diff --git a/dir/file b/new\nsimilarity index 100%\n'
                     'rename from dir/file\nrename to \n')
        compare('mail-diagnostic-' + name, ['am'], repo, env,
                data=(envelope + header + prefix + malformed).encode())


    malformed = '--- prose\nnot a patch header\n'
    for name, patch in (('only', malformed),
                        ('then-valid', malformed + traditional + git),
                        ('after-valid', traditional + git + malformed)):
        compare('mail-malformed-' + name, ['am'], repo, env,
                data=(envelope + header + patch).encode())
    broken_git = ('diff --git a/dir/file b/new\nsimilarity index 100%\n'
                  'rename from dir/file\nrename to \n')
    # patchbreak uses byte-sensitive prefixes, including each letter's case.
    prefixes = ('diff -', 'Diff -', 'DIFF -', 'dIfF -',
                'Index: ', 'index: ', 'INDEX: ', 'iNdEx: ',
                '--- ', '---\t')
    for prefix in prefixes:
        prose = prefix + ('\n' if prefix == '---\t' else 'this is explanatory text.\n\n')
        for name, patch in (('message', '---\n' + traditional + git),
                            ('diagnostic', '---\n' + broken_git)):
            compare('mail-patchbreak-' + repr(prefix) + '-' + name, ['am'], repo, env,
                    data=(envelope + header + prose + patch).encode())


def pull(repo, env):
    remote = repo.parent / 'remote'
    setup(['clone', '-q', str(repo), str(remote)], repo.parent, env)
    setup(['remote', 'add', 'origin', '../remote'], repo, env)
    setup(['config', 'branch.main.remote', 'origin'], repo, env)
    setup(['config', 'branch.main.merge', 'refs/heads/main'], repo, env)
    write(remote, 'incoming', 'remote\n')
    setup(['add', '.'], remote, env)
    setup(['commit', '-qm', 'upstream'], remote, env)
    head = setup(['rev-parse', 'HEAD'], repo, env)
    blob = setup(['rev-parse', 'HEAD:other.txt'], repo, env)
    tree = setup(['rev-parse', 'HEAD^{tree}'], repo, env)
    shapes = ('merge-resolved', 'merge-unmerged', 'unmerged', 'cherry-unmerged',
              'revert-unmerged', 'rebase-merge-unmerged', 'rebase-apply-unmerged',
              'cherry-dirty', 'revert-dirty', 'rebase-merge-dirty', 'rebase-apply-dirty',
              'cherry-resolved')
    forms = (
        ('config-auto', ['pull.rebase=interactive', 'pull.ff=only', 'rebase.autoStash=true'], []),
        ('cli-auto', ['rebase.autoStash=false'], ['--rebase=merges', '--ff-only', '--autostash']),
        ('no-auto', ['pull.rebase=true', 'pull.ff=only'], []),
        ('cli-no-auto', ['rebase.autoStash=true'], ['--rebase=interactive', '--ff-only', '--no-autostash']),
        ('merge-auto', ['merge.autoStash=true'], ['--no-rebase', '--ff-only']),
        ('no-ff-config', ['pull.rebase=true', 'rebase.autoStash=true'], []),
    )
    for shape in shapes:
        save = repo.parent / 'shape'
        shutil.copytree(repo, save, symlinks=True)
        write(repo, '.git/FETCH_HEAD', 'precondition sentinel\n')
        write(repo, '.git/AUTO_MERGE', tree + '\n')
        if shape.startswith('merge-'):
            write(repo, '.git/MERGE_HEAD', head + '\n')
        elif shape.startswith('cherry-'):
            write(repo, '.git/CHERRY_PICK_HEAD', head + '\n')
        elif shape.startswith('revert-'):
            write(repo, '.git/REVERT_HEAD', head + '\n')
        elif shape.startswith('rebase-'):
            directory = 'rebase-merge' if shape.startswith('rebase-merge') else 'rebase-apply'
            write(repo, '.git/' + directory + '/head-name', 'refs/heads/main\n')
            write(repo, '.git/' + directory + '/orig-head', head + '\n')
            write(repo, '.git/REBASE_HEAD', head + '\n')
        if 'unmerged' in shape:
            setup(['update-index', '--index-info'], repo, env,
                  (f'0 {"0" * 40}\tother.txt\n' +
                   ''.join(f'100644 {blob} {stage}\tother.txt\n' for stage in (1, 2, 3))).encode())
            write(repo, 'other.txt', 'conflict\n')
        else:
            write(repo, 'other.txt', 'resolved staged\n')
            setup(['add', 'other.txt'], repo, env)
            write(repo, 'other.txt', 'resolved unstaged\n')
        for name, config, opts in forms:
            # Clean marker files alone are not a pull.c pre-fetch refusal.
            # Dirty sequencer cases without autostash exercise require_clean_work_tree.
            if shape.endswith('dirty') and name not in ('no-auto', 'cli-no-auto'):
                continue
            if shape == 'cherry-resolved' and name not in ('config-auto', 'cli-auto'):
                continue
            argv = [a for value in config for a in ('-c', value)] + ['pull', '-q', *opts]
            compare('pull-' + shape + '-' + name, argv, repo, env,
                    immutable=shape != 'cherry-resolved')
        # Advice configuration must preserve the same refusal ordering.
        if shape in ('merge-resolved', 'merge-unmerged'):
            compare('pull-' + shape + '-no-advice',
                    ['-c', 'advice.resolveConflict=false', '-c', 'pull.rebase=true',
                     'pull', '-q', '--autostash', '--ff-only'], repo, env, immutable=True)
        shutil.rmtree(repo)
        shutil.copytree(save, repo, symlinks=True)
        shutil.rmtree(save)


def main(mode):
    if mode not in ('headers', 'pull', 'mail_starts'):
        raise ValueError('expected headers, pull or mail_starts')
    with tempfile.TemporaryDirectory(prefix='ngit-round6-') as tmp:
        base = Path(tmp)
        repo, home = base / 'repo', base / 'home'
        repo.mkdir()
        home.mkdir()
        env = {k: v for k, v in os.environ.items() if not k.startswith('GIT_')}
        env.update(HOME=str(home), XDG_CONFIG_HOME=str(home / 'xdg'), LC_ALL='C', TZ='UTC',
                   EDITOR='true', GIT_CONFIG_NOSYSTEM='1',
                   GIT_AUTHOR_NAME='Fixture', GIT_AUTHOR_EMAIL='fixture@example.test',
                   GIT_COMMITTER_NAME='Fixture', GIT_COMMITTER_EMAIL='fixture@example.test',
                   GIT_AUTHOR_DATE='1700000000 +0000', GIT_COMMITTER_DATE='1700000000 +0000')
        setup(['init', '-q', '-b', 'main'], repo, env)
        setup(['config', 'user.name', 'Fixture'], repo, env)
        setup(['config', 'user.email', 'fixture@example.test'], repo, env)
        write(repo, 'other.txt', 'base\n')
        setup(['add', '.'], repo, env)
        setup(['commit', '-qm', 'base'], repo, env)
        globals()[mode](repo, env)
    if not REPORT['comparisons']:
        raise RuntimeError('zero selected comparisons')


mode = sys.argv[1] if len(sys.argv) > 1 else 'headers'
try:
    main(mode)
except Exception as error:
    REPORT['unexpected_errors'] += 1
    print('UNEXPECTED: ' + str(error), file=sys.stderr)
finally:
    REPORT['seconds'] = round(time.monotonic() - START, 3)
    out = Path(os.environ.get('NGIT_REPORT_DIR', ROOT / 'target/include-pathspec/reports'))
    out.mkdir(parents=True, exist_ok=True)
    (out / f'round6-{mode}.json').write_text(json.dumps(REPORT, indent=2) + '\n')
print(json.dumps({k: v for k, v in REPORT.items() if k != 'cases'}))
sys.exit(bool(REPORT['failures'] or REPORT['unexpected_errors']))
