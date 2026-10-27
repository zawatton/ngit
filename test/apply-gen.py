#!/usr/bin/env python3
"""Seeded apply differentials, including the F7 review's destructive cases."""
import importlib.util
import json
import os
from pathlib import Path
import random
import shutil
import sys
import tempfile
import time
from concurrent.futures import ThreadPoolExecutor

ROOT = Path(__file__).resolve().parents[1]
# Read selectors before fixture environments discard NGIT_* overrides.
ONLY = tuple(p for p in os.environ.get('NGIT_UX_ONLY', '').split(',') if p)
spec = importlib.util.spec_from_file_location('ux_cmds', ROOT / 'test/ux-cmds.py')
ux = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ux)
REPORT = ux.REPORT
VARIANTS = ([], ['--check'], ['--index'], ['--index', '--check'],
            ['--cached'], ['--cached', '--check'])
SEED = 2473007


def selected(name):
    return (not ONLY or name.startswith(ONLY)) and (not ux.MATCH or ux.MATCH in name)


def case(name, setup, commands):
    if selected(name):
        ux.case(name, setup, commands)


def put(repo, path, data):
    dest = repo / path
    dest.parent.mkdir(parents=True, exist_ok=True)
    if dest.is_symlink():
        dest.unlink()
    dest.write_bytes(data)


def clear_permissions(repo):
    for path in repo.rglob('*'):
        if path.is_dir() and not path.is_symlink():
            path.chmod(0o755)
    repo.chmod(0o755)


def compare(name, repo, env, patch, opts=(), cwd='.', command=None, before=None):
    if not selected(name):
        return
    snapshot = repo.parent / 'snapshot'
    shutil.copytree(repo, snapshot, symlinks=True)
    results = []
    argv = command or ['apply', *opts]
    for executable in (['git'], ux.COMMAND):
        # Rebuild only the stat cache after copying; content and dirty modes stay.
        ux.run(['git', 'update-index', '-q', '--refresh'], repo, env)
        if before:
            before(repo, env)
        result = ux.run([*executable, *argv], repo / cwd, env, patch)
        results.append((result, ux.state(repo, env)))
        clear_permissions(repo)
        shutil.rmtree(repo)
        shutil.copytree(snapshot, repo, symlinks=True)
    clear_permissions(snapshot)
    shutil.rmtree(snapshot)
    (expected, es), (actual, ast) = results
    unexpected = actual['exit'] < 0 or bool(ux.BAD.search(actual['stdout'] + actual['stderr']))
    equal = expected == actual and es == ast
    REPORT['comparisons'] += 1
    REPORT['exact'] += int(equal and not unexpected)
    REPORT['failures'] += int(not equal)
    REPORT['unexpected_errors'] += int(unexpected)
    REPORT['cases'].append(dict(name=name, argv=argv, equal=equal, expected=expected,
                                actual=actual, state_equal=es == ast,
                                expected_state=es, actual_state=ast,
                                patch=patch.decode('utf-8', errors='surrogateescape')))
    if not equal:
        print('FAIL ' + name + ' ' + repr((expected, actual, es == ast)), flush=True)


def fixture(parent):
    repo, home = parent / 'repo', parent / 'home'
    repo.mkdir()
    home.mkdir()
    (home / '.gitconfig').write_text('[user]\n name = Fixture\n email = fixture@example.test\n')
    env = {k: v for k, v in os.environ.items()
           if not k.startswith(('GIT_', 'NGIT_')) and k != 'VISUAL'}
    env.update(HOME=str(home), XDG_CONFIG_HOME=str(home / 'xdg'), TZ='UTC', LC_ALL='C',
               GIT_CONFIG_NOSYSTEM='1', GIT_AUTHOR_DATE='1700000000 +0000',
               GIT_COMMITTER_DATE='1700000000 +0000')
    if ux.NO_ELC:
        env['NGIT_NO_ELC'] = ux.NO_ELC
    ux.git(repo, env, 'init', '-q', '-b', 'main')
    ux.git(repo, env, 'config', 'core.filemode', 'true')
    put(repo, 'sentinel', b'untouched\n')
    return repo, env


def save(repo, env):
    ux.git(repo, env, 'add', '-A')
    ux.git(repo, env, 'commit', '-qm', 'fixture', '--allow-empty')


def text_patch(old=b'old\n', new=b'new\n', name='f'):
    return (f'--- a/{name}\n+++ b/{name}\n@@ -1 +1 @@\n'.encode() + b'-' + old + b'+' + new)


def review():
    for finding in range(1, 10):
        if finding == 1 and os.geteuid() == 0:
            print('SKIP review-1-permissions: running as root', flush=True)
            continue
        with tempfile.TemporaryDirectory(prefix='ngit-apply-review-') as tmp:
            repo, env = fixture(Path(tmp))
            put(repo, 'f', b'old\n')
            if finding == 1:
                put(repo, 'z', b'old\nanchor\n')
            if finding in (2, 3, 7):
                put(repo, 'f', {2: b'one\ntwo\nprecious\n', 3: b'one\n\n',
                                7: b'one\nanchor\nspacer\nfour\nlast\n'}[finding])
            if finding == 5:
                ux.git(repo, env, 'config', 'core.autocrlf', 'false')
                put(repo, 'f', b'old\r\n')
            if finding == 9:
                put(repo, 'd/f', b'old\n')
            save(repo, env)
            patch = text_patch()
            if finding == 1:
                put(repo, 'z', b'staged\nanchor\n')
                put(repo, 'outer/d/f', b'staged\n')
                ux.git(repo, env, 'add', '.')
                put(repo, 'z', b'staged\nanchor\nprecious\n')
                (repo / 'outer').chmod(0o555)
                compare('review-1-stash-precious', repo, env, b'',
                        command=['stash', 'push', '--staged'])
                # Also exercise apply's parent pruning without the stash driver.
                patch = b'--- a/outer/d/f\n+++ /dev/null\n@@ -1 +0,0 @@\n-staged\n'
            elif finding == 2:
                patch = b'--- a/f\n+++ b/f\n@@ -1,3 +1,3 @@\n-one\n+ONE\n two\n'
                for body in (b'@@ -1,2 +0,0 @@\n', b'@@ -1,1 +1,1 @@\n-one\n+ONE',
                             b'@@ -1,2 +1,2 @@\n one\n two\n'):
                    compare('review-2-corrupt-' + repr(body), repo, env,
                            b'--- a/f\n+++ b/f\n' + body)
            elif finding == 3:
                patch = b'--- a/f\n+++ b/f\n@@ -1,2 +1,2 @@\n-one\n+ONE\n\n'
            elif finding == 4:
                (repo / 'f').chmod(0o755)
            elif finding == 5:
                ux.git(repo, env, 'config', 'core.autocrlf', 'true')
                patch = b'diff --git a/f b/f\nold mode 100644\nnew mode 100755\n'
            elif finding == 6:
                patch = b'diff --git a/f b/f\nold mode 100644\nnew mode 120000\n'
                wrong = b'diff --git a/f b/f\nold mode 120000\nnew mode 120000\n'
                compare('review-6-cached-wrong-type', repo, env, wrong, ['--cached', '--check'])
                compare('review-6-cached-preimage-type', repo, env,
                        wrong + text_patch(), ['--cached', '--check'])
            elif finding == 7:
                patch = (b'--- a/f\n+++ b/f\n@@ -1,2 +1,2 @@\n-one\n+ONE\n anchor\n'
                         b'--- a/f\n+++ b/f\n@@ -4,2 +4,2 @@\n-four\n+FOUR\n last\n')
                chain = text_patch(b'one\n', b'two\n') + text_patch(b'two\n', b'three\n')
                put(repo, 'f', b'one\n')
                ux.git(repo, env, 'add', 'f')
                compare('review-7-dependent', repo, env, chain)
                put(repo, 'f', b'one\nanchor\nspacer\nfour\nlast\n')
                ux.git(repo, env, 'add', 'f')
            elif finding == 8:
                oid = ux.git(repo, env, 'rev-parse', 'HEAD')
                patch = (f'diff --git a/sub b/sub\nnew file mode 160000\n'
                         f'index {"0" * 40}..{oid}\n--- /dev/null\n+++ b/sub\n'
                         f'@@ -0,0 +1 @@\n+Subproject commit {oid}\n').encode()
            elif finding == 9:
                patch = b'--- a/d/f\n+++ /dev/null\n@@ -1 +0,0 @@\n-old\n'
                compare('review-9-original-cwd', repo, env, patch, cwd='d',
                        command=['-C', '..', 'apply'])
            for opts in VARIANTS:
                compare(f'review-{finding}-' + ','.join(opts), repo, env, patch, opts)
            clear_permissions(repo)


def index_conflicts(prefix):
    for existing, added in (('a/precious', 'a'), ('a', 'a/f')):
        with tempfile.TemporaryDirectory(prefix='ngit-apply-df-') as tmp:
            repo, env = fixture(Path(tmp))
            save(repo, env)
            oid = ux.git(repo, env, 'hash-object', '-w', '--stdin', data=b'precious staged bytes\n')
            ux.git(repo, env, 'update-index', '--add', '--cacheinfo', f'100644,{oid},{existing}')
            patch = (f'diff --git a/{added} b/{added}\nnew file mode 100644\n'
                     f'--- /dev/null\n+++ b/{added}\n@@ -0,0 +1 @@\n+new\n').encode()
            for opts in (['--cached'], ['--cached', '--check'],
                         ['--index'], ['--index', '--check']):
                compare(f'{prefix}-index-df-{existing}-to-{added}-' + ','.join(opts),
                        repo, env, patch, opts)
            # Removing the conflicting preimage in the same patch is valid.
            put(repo, existing, b'precious staged bytes\n')
            removal = (f'diff --git a/{existing} b/{existing}\ndeleted file mode 100644\n'
                       f'--- a/{existing}\n+++ /dev/null\n@@ -1 +0,0 @@\n'
                       f'-precious staged bytes\n').encode()
            for opts in (['--cached'], ['--cached', '--check'],
                         ['--index'], ['--index', '--check']):
                compare(f'{prefix}-index-df-replaced-{existing}-to-{added}-' + ','.join(opts),
                        repo, env, removal + patch, opts)


def permission_patch(prefix):
    if os.geteuid() == 0:
        print(f'SKIP {prefix}-multi-permissions: running as root', flush=True)
        return
    for failure in ('unlink', 'rmdir'):
        with tempfile.TemporaryDirectory(prefix='ngit-apply-multi-permissions-') as tmp:
            repo, env = fixture(Path(tmp))
            for name in ('a-first', 'outer/d/f', 'z-last'):
                put(repo, name, b'old\n')
            save(repo, env)
            (repo / ('outer/d' if failure == 'unlink' else 'outer')).chmod(0o555)
            patch = (text_patch(name='a-first') +
                     b'--- a/outer/d/f\n+++ /dev/null\n@@ -1 +0,0 @@\n-old\n' +
                     text_patch(name='z-last'))
            for opts in VARIANTS:
                compare(f'{prefix}-multi-{failure}-' + ','.join(opts), repo, env, patch, opts)
            clear_permissions(repo)


def review_f8():
    if os.geteuid() != 0:
        with tempfile.TemporaryDirectory(prefix='ngit-apply-unlink-stash-') as tmp:
            repo, env = fixture(Path(tmp))
            put(repo, 'z', b'old\nanchor\n')
            save(repo, env)
            put(repo, 'z', b'staged\nanchor\n')
            put(repo, 'outer/d/f', b'staged\n')
            ux.git(repo, env, 'add', '.')
            put(repo, 'z', b'staged\nanchor\nprecious\n')
            (repo / 'outer/d').chmod(0o555)
            compare('f8-1-stash-unlink-precious', repo, env, b'',
                    command=['stash', 'push', '--staged'])
            clear_permissions(repo)
    else:
        print('SKIP f8-1-stash-unlink-precious: running as root', flush=True)
    with tempfile.TemporaryDirectory(prefix='ngit-apply-stripped-rename-') as tmp:
        repo, env = fixture(Path(tmp))
        put(repo, 'f', b'old\n')
        save(repo, env)
        patch = (b'diff --git a/old/f b/new/f\nsimilarity index 100%\n'
                 b'rename from old/f\nrename to new/f\n')
        for opts in VARIANTS:
            compare('f8-11-stripped-rename-' + ','.join(opts), repo, env, patch, ['-p2', *opts])
    index_conflicts('f8-2')


def deferred():
    for finding in (3, 6, 7, 8, 9, 10):
        with tempfile.TemporaryDirectory(prefix='ngit-apply-deferred-') as tmp:
            repo, env = fixture(Path(tmp))
            put(repo, 'f', b'old\n')
            if finding == 8:
                (repo / 'a').symlink_to('first-target')
                (repo / 'b').symlink_to('second-target')
            if finding == 10:
                (repo / 'f').unlink()
                (repo / 'f').symlink_to('old')
            save(repo, env)
            patch, opts = text_patch(), ['--cached']
            if finding == 3:
                rows = '0 ' + '0' * 40 + '\tf\n'
                for stage, content in enumerate((b'old\n', b'ours\n', b'theirs\n'), 1):
                    oid = ux.git(repo, env, 'hash-object', '-w', '--stdin', data=content)
                    rows += f'100644 {oid} {stage}\tf\n'
                ux.git(repo, env, 'update-index', '--index-info', data=rows.encode())
            elif finding == 6:
                patch = b'diff --git a/f b/f\n' + text_patch() + b'\n-- \nrename to g\n'
                opts = []
            elif finding == 7:
                (repo / 'f').unlink()
                ux.git(repo, env, 'rm', '--cached', 'f')
                patch = b'--- a/f\n+++ b/f\n@@ -0,0 +1 @@\n+new\n'
                opts = []
            elif finding == 8:
                first = ux.git(repo, env, 'rev-parse', 'HEAD:a')
                second = ux.git(repo, env, 'rev-parse', 'HEAD:b')
                patch = (f'diff --git a/a b/b\nsimilarity index 100%\n'
                         f'rename from a\nrename to b\nindex {first}..{first} 120000\n'
                         f'diff --git a/b b/a\nsimilarity index 100%\n'
                         f'rename from b\nrename to a\nindex {second}..{second} 120000\n').encode()
                opts = ['--index']
            elif finding == 9:
                put(repo, 'new', b'new\n')
                ux.git(repo, env, 'add', '-N', 'new')
                patch = (b'diff --git a/new b/new\nnew file mode 100644\n'
                         b'--- /dev/null\n+++ b/new\n@@ -0,0 +1 @@\n+new\n')
            elif finding == 10:
                ux.git(repo, env, 'config', 'core.symlinks', 'false')
                put(repo, 'f', b'old')
                patch = (b'diff --git a/f b/f\n--- a/f\n+++ b/f\n@@ -1 +1 @@\n'
                         b'-old\n\\ No newline at end of file\n'
                         b'+new\n\\ No newline at end of file\n')
                opts = ['--index']
            compare(f'deferred-{finding}', repo, env, patch, opts)
            if finding == 7:
                put(repo, 'f', b'old\n')
                ux.git(repo, env, 'add', 'f')
                deletion = b'--- a/f\n+++ b/f\n@@ -1 +0,0 @@\n-old\n'
                compare('deferred-7-delete', repo, env, deletion)
                compare('deferred-7-delete-epoch', repo, env,
                        deletion.replace(b'+++ b/f\n',
                                         b'+++ b/f\t1970-01-01 00:00:00 +0000\n'))

    deferred_f9()
    deferred_d2()


def deferred_d2():
    deletion = b'--- a/f\n+++ b/f\n@@ -1 +0,0 @@\n-old\n'
    addition = b'--- a/f\n+++ b/f\n@@ -0,0 +1 @@\n+old\n'
    for reverse in (False, True):
        with tempfile.TemporaryDirectory(prefix='ngit-d2-traditional-') as tmp:
            repo, env = fixture(Path(tmp))
            put(repo, 'f', b'old\n')
            save(repo, env)
            for opts in VARIANTS:
                flags = [*(['-R'] if reverse else []), *opts]
                suffix = f'R{int(reverse)}-' + ','.join(opts)
                compare('deferred-7-del2-' + suffix, repo, env,
                        addition if reverse else deletion, flags)
            # Explicit /dev/null must win over the old name's epoch timestamp.
            if reverse:
                ux.git(repo, env, 'rm', 'f')
            epoch = deletion.replace(b'--- a/f\n',
                                     b'--- a/f\t1970-01-01 00:00:00 +0000\n')
            epoch = epoch.replace(b'+++ b/f\n', b'+++ /dev/null\n')
            for opts in VARIANTS:
                compare(f'd2-reg-1-R{int(reverse)}-' + ','.join(opts), repo, env,
                        epoch, [*(['-R'] if reverse else []), *opts])

    for delete in (False, True):
        for reverse in (False, True):
            with tempfile.TemporaryDirectory(prefix='ngit-d2-mode-') as tmp:
                repo, env = fixture(Path(tmp))
                if delete != reverse:
                    put(repo, 'f', b'old\n')
                save(repo, env)
                patch = (b'diff --git a/f b/f\n' +
                         (b'deleted file mode 100644\n+++ /dev/null\n@@ -1 +0,0 @@\n-old\n'
                          if delete else
                          b'new file mode 100644\n--- /dev/null\n@@ -0,0 +1 @@\n+old\n'))
                for opts in VARIANTS:
                    compare(f'd2-reg-8-del{int(delete)}-R{int(reverse)}-' + ','.join(opts),
                            repo, env, patch, [*(['-R'] if reverse else []), *opts])

    # Exercise the entire extended-header vocabulary in parse_git_diff_header.
    for kind, src, dst in (('legacy', 'rename old', 'rename new'),
                           ('rename', 'rename from', 'rename to'),
                           ('copy', 'copy from', 'copy to')):
        with tempfile.TemporaryDirectory(prefix='ngit-d2-headers-') as tmp:
            repo, env = fixture(Path(tmp))
            put(repo, 'f', b'old\n')
            save(repo, env)
            before = ux.git(repo, env, 'rev-parse', 'HEAD:f')
            after = ux.git(repo, env, 'hash-object', '-w', '--stdin', data=b'new\n')
            patch = (f'diff --git a/f b/g\nold mode 100644\nnew mode 100755\n'
                     f'similarity index 50%\ndissimilarity index 50%\n'
                     f'{src} f\n{dst} g\nindex {before}..{after}\n'
                     f'--- a/f\n+++ b/g\n@@ -1 +1 @@\n-old\n+new\n').encode()
            for opts in VARIANTS:
                compare('d2-reg-9-' + kind + '-' + ','.join(opts), repo, env, patch, opts)

    with tempfile.TemporaryDirectory(prefix='ngit-d2-mail-hunk-') as tmp:
        repo, env = fixture(Path(tmp))
        put(repo, 'f', b'old\nmiddle\nthird\n')
        save(repo, env)
        patch = (b'diff --git a/f b/f\n--- a/f\n+++ b/f\n@@ -1,2 +1,2 @@\n'
                 b'-old\n+new\n middle\n\n-- \nRegards\n@@ -3 +3 @@\n-third\n+TAMPER\n')
        for opts in VARIANTS:
            compare('deferred-6-hunk-signature-' + ','.join(opts), repo, env, patch, opts)
            compare('deferred-6-hunk-unterminated-' + ','.join(opts), repo, env,
                    patch.split(b'@@ -3 +3 @@', 1)[0] + b'@@ -3 +3 @@', opts)

    # These review findings remain outside the D1/D2 acceptance selections.
    with tempfile.TemporaryDirectory(prefix='ngit-d2-deferred-') as tmp:
        repo, env = fixture(Path(tmp))
        put(repo, 'f', b'old\n')
        save(repo, env)
        patch = text_patch().replace(b'--- a/f\n', b'--- a/f 2026-01-01 01:02:03\n')
        patch = patch.replace(b'+++ b/f\n', b'+++ b/f 2026-01-02 01:02:03\n')
        compare('d2-deferred-4-space-timestamp', repo, env, patch)
        put(repo, 'empty', b'')
        ux.git(repo, env, 'add', '-N', 'empty')
        patch = (b'diff --git a/empty b/empty\n--- a/empty\n+++ b/empty\n'
                 b'@@ -0,0 +1 @@\n+new\n')
        compare('d2-deferred-5-empty-ita-index', repo, env, patch, ['--index'])

    def setup_skip(repo, env):
        put(repo, 'f', b'old\n')
        ux.commit(repo, env, 'am base')
        mail = b''
        for number in range(3):
            mail += (f'From {number:040d} Mon Sep 17 00:00:00 2001\n'
                     f'From: Fixture <fixture@example.test>\n'
                     f'Date: Tue, 14 Nov 2023 22:13:20 +0000\n'
                     f'Subject: [PATCH] fails {number}\n\n').encode()
            mail += text_patch(b'missing\n', b'resolved\n') + b'\n'
        result = ux.run(['git', 'am'], repo, env, mail)
        if result['exit'] != 128:
            raise RuntimeError('expected am conflict: ' + repr(result))
        put(repo, 'f', b'resolved\n')
        ux.git(repo, env, 'add', 'f')

    def move_head(repo, env):
        ux.git(repo, env, 'commit', '--allow-empty', '-qm', 'external move')

    case('d2-deferred-6-skip-abort-safety', setup_skip,
         [['am', '--continue'], move_head, ['am', '--skip'], ['am', '--abort']])

    def setup_hook(repo, env):
        put(repo, 'f', b'old\n')
        ux.commit(repo, env, 'am base')
        tree = ux.git(repo, env, 'rev-parse', 'HEAD^{tree}')
        parent = ux.git(repo, env, 'rev-parse', 'HEAD')
        moved = ux.git(repo, env, 'commit-tree', tree, '-p', parent, '-m', 'hook parent')
        failed_am(repo, env)
        put(repo, 'f', b'resolved\n')
        ux.git(repo, env, 'add', 'f')
        put(repo, '.git/hooks/pre-applypatch', f'#!/bin/sh\ngit update-ref HEAD {moved}\n'.encode())
        (repo / '.git/hooks/pre-applypatch').chmod(0o755)

    case('d2-deferred-7-hook-moves-head', setup_hook, [['am', '--continue']])


def rename_record(old, new, before, after):
    return (f'diff --git a/{old} b/{new}\nsimilarity index 50%\n'
            f'rename from {old}\nrename to {new}\n'
            f'--- a/{old}\n+++ b/{new}\n@@ -1 +1 @@\n').encode() + b'-' + before + b'+' + after


def failed_am(repo, env, second=False):
    mail = (b'From ' + b'0' * 40 + b' Mon Sep 17 00:00:00 2001\n'
            b'From: Fixture <fixture@example.test>\n'
            b'Date: Tue, 14 Nov 2023 22:13:20 +0000\n'
            b'Subject: [PATCH] resolve first\n\n' + text_patch(b'missing\n', b'resolved\n'))
    if second:
        mail += (b'\nFrom ' + b'1' * 40 + b' Mon Sep 17 00:00:00 2001\n'
                 b'From: Fixture <fixture@example.test>\n'
                 b'Date: Tue, 14 Nov 2023 22:13:20 +0000\n'
                 b'Subject: [PATCH] fails second\n\n' + text_patch(b'absent\n', b'last\n'))
    result = ux.run(['git', 'am'], repo, env, mail)
    if result['exit'] != 128:
        raise RuntimeError('expected am conflict: ' + repr(result))


def review_f9():
    for reverse in (False, True):
        with tempfile.TemporaryDirectory(prefix='ngit-f9-rename-') as tmp:
            repo, env = fixture(Path(tmp))
            put(repo, 'f', b'one\n')
            save(repo, env)
            patch = b''.join(rename_record('old/f', 'new/f',
                            after if reverse else b'one\n',
                            b'one\n' if reverse else after)
                            for after in (b'two\n', b'three\n'))
            for opts in VARIANTS:
                compare(f'f9-6-stripped-chain-R{int(reverse)}-' + ','.join(opts),
                        repo, env, patch, ['-p2', *(['-R'] if reverse else []), *opts])
        with tempfile.TemporaryDirectory(prefix='ngit-f9-swap-') as tmp:
            repo, env = fixture(Path(tmp))
            for name, target in (('a', 'first-target'), ('b', 'second-target')):
                (repo / name).symlink_to(target)
            save(repo, env)
            patch = b''
            for old, new in (('a', 'b'), ('b', 'a')):
                oid = ux.git(repo, env, 'rev-parse', 'HEAD:' + old)
                patch += (f'diff --git a/{old} b/{new}\nsimilarity index 100%\n'
                          f'rename from {old}\nrename to {new}\nindex {oid}..{oid} 120000\n').encode()
            for opts in (['--cached'], ['--cached', '--check']):
                compare(f'f9-8-cached-swap-R{int(reverse)}-' + ','.join(opts), repo, env,
                        patch, [*(['-R'] if reverse else []), *opts])
    with tempfile.TemporaryDirectory(prefix='ngit-f9-symlinks-') as tmp:
        repo, env = fixture(Path(tmp))
        (repo / 'f').symlink_to('old')
        save(repo, env)
        ux.git(repo, env, 'config', 'core.symlinks', 'false')
        put(repo, 'f', b'old')
        patch = (b'diff --git a/f b/f\nold mode 120000\nnew mode 120000\n' +
                 text_patch(b'old\n\\ No newline at end of file\n',
                            b'new\n\\ No newline at end of file\n'))
        compare('f9-10a-symlinks-false-index-check', repo, env, patch, ['--index', '--check'])
        for opts in VARIANTS:
            addition = (b'diff --git a/link b/link\nnew file mode 120000\n'
                        b'--- /dev/null\n+++ b/link\n@@ -0,0 +1 @@\n'
                        b'+target\n\\ No newline at end of file\n')
            compare('f9-10b-symlinks-false-create-' + ','.join(opts), repo, env, addition, opts)
    for control in ('--abort', '--skip'):
        with tempfile.TemporaryDirectory(prefix='ngit-f9-am-') as tmp:
            repo, env = fixture(Path(tmp))
            put(repo, 'f', b'base-f\n')
            put(repo, 'g', b'base-g\n')
            save(repo, env)
            put(repo, 'g', b'precious\n')
            failed_am(repo, env)
            compare('f9-1-am-unrelated-' + control, repo, env, b'', command=['am', control])


def review_f10():
    for control in ('--abort', '--skip'):
        for finding in ('A-stat', 'B-ita', 'B-ita-present', 'B-ita-dirty'):
            with tempfile.TemporaryDirectory(prefix='ngit-f10-am-') as tmp:
                repo, env = fixture(Path(tmp))
                put(repo, 'f', b'base\n')
                save(repo, env)
                failed_am(repo, env)
                if finding == 'A-stat':
                    put(repo, 'f', b'staged\n')
                    ux.git(repo, env, 'add', 'f')
                else:
                    put(repo, 'new', b'precious\n' if finding == 'B-ita-dirty' else b'')
                    ux.git(repo, env, 'add', '-N', 'new')
                    if finding == 'B-ita':
                        (repo / 'new').unlink()
                # Do this after the copy's stat refresh so the stale timestamp
                # reaches both commands rather than being repaired by the suite.
                compare('f10-' + finding + '-' + control, repo, env, b'',
                        command=['am', control],
                        before=(lambda r, e: os.utime(r / 'f', (1, 1)))
                        if finding == 'A-stat' else None)


def deferred_f9():
    with tempfile.TemporaryDirectory(prefix='ngit-f9-deferred-') as tmp:
        repo, env = fixture(Path(tmp))
        put(repo, 'f', b'old\n')
        put(repo, 'd/f', b'old\n')
        save(repo, env)
        rows = ''
        for stage in (1, 2, 3):
            oid = ux.git(repo, env, 'hash-object', '-w', '--stdin', data=f'precious-{stage}\n'.encode())
            rows += f'100644 {oid} {stage}\ta/precious\n'
        ux.git(repo, env, 'update-index', '--index-info', data=rows.encode())
        patch = (b'diff --git a/a b/a\nnew file mode 100644\n'
                 b'--- /dev/null\n+++ b/a\n@@ -0,0 +1 @@\n+new\n')
        compare('f9-deferred-3-df-conflict-stages', repo, env, patch, ['--cached'])
        # Restore only the fixture index before testing independent findings.
        ux.git(repo, env, 'read-tree', 'HEAD')
        if os.geteuid() != 0:
            (repo / 'd').chmod(0o555)
            compare('f9-deferred-4-unlink-overwrite', repo, env, text_patch(name='d/f'))
            clear_permissions(repo)
        else:
            print('SKIP f9-deferred-4-unlink-overwrite: running as root', flush=True)
        incomplete = (b'diff --git a/f b/f\nsimilarity index 100%\nrename from f\n' +
                      b'diff --git a/d/f b/d/f\n' + text_patch(name='d/f'))
        compare('f9-deferred-5-incomplete-rename', repo, env, incomplete)
        put(repo, 'new', b'new\n')
        ux.git(repo, env, 'add', '-N', 'new')
        addition = (b'diff --git a/new b/new\nnew file mode 100644\n'
                    b'--- /dev/null\n+++ b/new\n@@ -0,0 +1 @@\n+new\n')
        compare('f9-deferred-9-intent-to-add', repo, env, addition, ['--cached', '--check'])
    def setup_am(r, e):
        put(r, 'f', b'old\n')
        ux.commit(r, e, 'am base')
        failed_am(r, e, second=True)
        put(r, 'f', b'resolved\n')
        ux.git(r, e, 'add', 'f')
    case('f9-deferred-7-continue-abort-safety', setup_am,
            [['am', '--continue'], ['am', '--abort']])


def e2e():
    def mailed_series(repo, env):
        for name in ('clean', 'f', 'g', 'h'):
            put(repo, name, b'old\n')
        ux.commit(repo, env, 'series base')
        base = ux.git(repo, env, 'rev-parse', 'HEAD')
        for name in ('clean', 'f', 'g', 'h'):
            put(repo, name, b'new\n')
            ux.commit(repo, env, 'change ' + name)
        mail = ux.run(['git', 'format-patch', '--stdout', base + '..HEAD'], repo, env)
        if mail['exit'] or mail['stdout'].count('Subject: [PATCH ') != 4:
            raise RuntimeError('expected four real mailed commits: ' + repr(mail))
        put(repo, '.git/series.mbox', mail['stdout'].encode('utf-8', errors='surrogateescape'))
        ux.git(repo, env, 'reset', '--hard', base)
        for name in ('f', 'g'):
            put(repo, name, b'target branch\n')
        ux.commit(repo, env, 'target diverges')

    def resolve(repo, env):
        put(repo, 'f', b'resolved\n')
        ux.git(repo, env, 'add', 'f')

    for control in ('--skip', '--abort'):
        case('e2e-am-continue-' + control, mailed_series,
             [['am', '.git/series.mbox'], resolve, ['am', '--continue'], ['am', control]])

    with tempfile.TemporaryDirectory(prefix='ngit-apply-e2e-index-') as tmp:
        repo, env = fixture(Path(tmp))
        put(repo, 'f', b'old\n')
        save(repo, env)
        rows = '0 ' + '0' * 40 + '\tf\n'
        for stage, content in enumerate((b'old\n', b'ours\n', b'theirs\n'), 1):
            oid = ux.git(repo, env, 'hash-object', '-w', '--stdin', data=content)
            rows += f'100644 {oid} {stage}\tf\n'
        ux.git(repo, env, 'update-index', '--index-info', data=rows.encode())
        for opts in VARIANTS[2:]:
            compare('e2e-unmerged-' + ','.join(opts), repo, env, text_patch(), opts)
        ux.git(repo, env, 'read-tree', 'HEAD')
        put(repo, 'new', b'new\n')
        ux.git(repo, env, 'add', '-N', 'new')
        addition = (b'diff --git a/new b/new\nnew file mode 100644\n'
                    b'--- /dev/null\n+++ b/new\n@@ -0,0 +1 @@\n+new\n')
        for opts in VARIANTS[2:]:
            compare('e2e-ita-' + ','.join(opts), repo, env, addition, opts)


def generated_f9(number):
    rng = random.Random(SEED + number)
    with tempfile.TemporaryDirectory(prefix='ngit-f9-gen-') as tmp:
        repo, env = fixture(Path(tmp))
        reverse = bool(number % 2)
        if number < 64:
            strip = rng.randint(2, 4)
            put(repo, 'f', b'one\n')
            save(repo, env)
            old = '/'.join(['old'] * (strip - 1) + ['f'])
            new = '/'.join(['new'] * (strip - 1) + ['f'])
            patch = b''.join(rename_record(old, new,
                            after if reverse else b'one\n',
                            b'one\n' if reverse else after)
                            for after in (f'two-{rng.randrange(999)}\n'.encode(),
                                          f'three-{rng.randrange(999)}\n'.encode()))
            flags = [f'-p{strip}']
        else:
            target = f'target-{rng.randrange(999)}'.encode()
            save(repo, env)
            ux.git(repo, env, 'config', 'core.symlinks', 'false')
            patch = (b'diff --git a/f b/f\nnew file mode 120000\n'
                     b'--- /dev/null\n+++ b/f\n@@ -0,0 +1 @@\n+' + target +
                     b'\n\\ No newline at end of file\n')
            if reverse:
                oid = ux.git(repo, env, 'hash-object', '-w', '--stdin', data=target)
                ux.git(repo, env, 'update-index', '--add', '--cacheinfo', f'120000,{oid},f')
                put(repo, 'f', target)
            flags = []
        for opts in VARIANTS:
            compare(f'gen-{number}-f9-R{int(reverse)}-' + ','.join(opts), repo, env, patch,
                    [*flags, *(['-R'] if reverse else []), *opts])


def generated():
    # Sixty independently generated patches, each on six fresh copies (360).
    # Shape, context, direction and local divergence vary independently.
    def scenario(number):
        rng = random.Random(SEED + number)
        shape = number % 10
        with tempfile.TemporaryDirectory(prefix='ngit-apply-gen-') as tmp:
            repo, env = fixture(Path(tmp))
            lines = [rng.choice([b'\n', b'repeated\n', f'line-{i}-{rng.randrange(99)}\n'.encode()])
                     for i in range(rng.randint(8, 18))]
            lines[0], lines[-1] = b'first\n', b'last\n'
            if number % 4 == 0:
                lines[-1] = b'last without newline'
            old = b''.join(lines)
            attrs = shape == 1
            if attrs:
                put(repo, '.gitattributes', b'f text eol=crlf\n')
                old = old.replace(b'\n', b'\r\n')
            if shape == 2:
                old = b'\x00' + bytes(rng.randrange(256) for _ in range(80))
            put(repo, 'f', old)
            put(repo, 'later', b'later preimage\n')
            save(repo, env)
            before = repo.parent / 'before'
            shutil.copytree(repo, before, symlinks=True)
            changed = list(lines)
            changed[1], changed[-2] = b'NEW first edit\n', b'NEW second edit\n'
            new = b''.join(changed)
            if attrs:
                new = new.replace(b'\n', b'\r\n')
            if shape == 2:
                new = old[:20] + b'\x00binary-change' + old[45:]
            put(repo, 'f', new)
            if shape == 3:
                (repo / 'f').rename(repo / 'renamed')
            elif shape == 4:
                put(repo, 'f', old)
                (repo / 'f').chmod(0o755)
            elif shape == 5:
                (repo / 'f').unlink()
                (repo / 'f').symlink_to('sentinel')
            elif shape == 6:
                put(repo, 'created/deep/file', new)
            elif shape == 7:
                (repo / 'f').unlink()
            elif shape == 8:
                put(repo, 'later', b'later changed\n')
            ux.git(repo, env, 'add', '-A')
            context = (0, 1, 3)[number // 10 % 3]
            patch = ux.run(['git', 'diff', '--cached', '--binary', '-M', f'-U{context}'], repo, env)['stdout'].encode('utf-8', errors='surrogateescape')
            reverse = number // 10 % 2 == 1
            if not reverse:
                shutil.rmtree(repo)
                shutil.copytree(before, repo, symlinks=True)
            shutil.rmtree(before)
            divergence = number // 10
            if divergence in (1, 2, 3, 4) and (repo / 'f').is_file() and not (repo / 'f').is_symlink():
                data = (repo / 'f').read_bytes()
                if divergence == 1:
                    data = b'extra before\n' + data
                elif divergence == 2:
                    data += b'\nextra after\n'
                elif divergence == 3:
                    data = data.replace(b'repeated\n', b'repeated\nextra between\n', 1)
                else:
                    data = data[:len(data)//2] + data
                put(repo, 'f', data)
            if shape == 8:
                put(repo, 'later', b'fails later\n')
            if shape == 9:
                # Truncation and count overflow must never remove unseen bytes.
                if number % 20 == 9:
                    patch = patch[:patch.rfind(b'\n', 0, len(patch)-1)+1]
                else:
                    import re
                    patch = re.sub(rb'(@@ -\d+),?\d*', rb'\1,999', patch, count=1)
            if number % 5 == 0:
                patch = patch.replace(b'\n \n', b'\n\n')
            for opts in VARIANTS:
                compare(f'gen-{number:02d}-shape{shape}-U{context}-R{int(reverse)}-' + ','.join(opts),
                        repo, env, patch, [*(['-R'] if reverse else []), *opts])
    # Independent repositories keep runtime startup below the eight-minute cap.
    with ThreadPoolExecutor(max_workers=4) as pool:
        list(pool.map(scenario, range(60)))
        list(pool.map(generated_f9, range(60, 68)))
    # Real permission failures, independent of the generated content variants.
    if os.geteuid() != 0:
        with tempfile.TemporaryDirectory(prefix='ngit-apply-permissions-') as tmp:
            repo, env = fixture(Path(tmp))
            put(repo, 'outer/d/f', b'old\n')
            save(repo, env)
            (repo / 'outer').chmod(0o555)
            for opts in VARIANTS:
                compare('gen-permission-remove-' + ','.join(opts), repo, env,
                        b'--- a/outer/d/f\n+++ /dev/null\n@@ -1 +0,0 @@\n-old\n', opts)
                compare('gen-permission-write-' + ','.join(opts), repo, env,
                        b'--- /dev/null\n+++ b/outer/new\n@@ -0,0 +1 @@\n+new\n', opts)
            clear_permissions(repo)
    else:
        print('SKIP generated-permissions: running as root', flush=True)
    permission_patch('gen')
    index_conflicts('gen')


if __name__ == '__main__':
    mode = sys.argv[1] if len(sys.argv) == 2 else 'apply-gen'
    try:
        if mode == 'review':
            review()
            review_f8()
            review_f9()
            review_f10()
        else:
            {'apply-gen': generated, 'deferred': deferred, 'e2e': e2e}[mode]()
    except Exception as exc:
        REPORT['unexpected_errors'] += 1
        print('ERROR ' + repr(exc), flush=True)
    REPORT['seconds'] = round(time.monotonic() - ux.START, 3)
    dest = Path(os.environ.get('NGIT_REPORT_DIR', ROOT / 'target/ux/reports'))
    dest.mkdir(parents=True, exist_ok=True)
    (dest / f'apply-gen-{mode}.json').write_text(json.dumps(REPORT, indent=2) + '\n')
    print('apply-gen %s: comparisons=%d exact=%d intended=%d failures=%d unexpected_errors=%d' %
          (mode, REPORT['comparisons'], REPORT['exact'], len(REPORT['intended']),
           REPORT['failures'], REPORT['unexpected_errors']))
    sys.exit(int(bool(REPORT['failures'] or REPORT['unexpected_errors'] or not REPORT['comparisons'])))
