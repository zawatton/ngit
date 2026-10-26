#!/usr/bin/env python3
"""Exact log review comparisons, including state and Git self-calibration.

Git 2.47.3 date.c:get_time honours GIT_TEST_DATE_NOW for relative, human
and approximate dates; all date cases use that hook instead of wall time.
"""
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
COMMAND = shlex.split(os.environ.get('NGIT_UX_COMMAND', str(ROOT / 'bin/ngit')))
NO_ELC = os.environ.get('NGIT_NO_ELC')
MATCH = os.environ.get('NGIT_UX_MATCH', '')
REPORT_DIR = Path(os.environ.get('NGIT_REPORT_DIR', ROOT / 'target/ux/reports'))
START = time.monotonic()
DEADLINE = START + 460
REPORT = dict(comparisons=0, exact=0, intended={}, failures=0, unexpected_errors=0, cases=[])
BAD = re.compile(rb'void-function|void-variable|wrong-type-argument|Lisp error|[Ll]oader error|Cannot open load file|Invalid regexp|[Oo]ut of memory|resource exhausted')


def run(argv, repo, env, data=None):
    budget = min(25, DEADLINE - time.monotonic())
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
    return p.returncode, out, err


def git(args, repo, env, data=None):
    rc, out, err = run(['git', *args], repo, env, data)
    if rc:
        raise RuntimeError('fixture/state: ' + repr((args, rc, out, err)))
    return out.decode().strip()


def write(repo, path, text):
    p = repo / path
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(text)


def commit(repo, env, message, day):
    date = f'2024-01-{day:02d}T12:34:56 +0530'
    git(['add', '.'], repo, env)
    return git(['commit', '-qm', message], repo,
               dict(env, GIT_AUTHOR_DATE=date, GIT_COMMITTER_DATE=date))


def fixture(repo, env):
    git(['init', '-q', '-b', 'main', str(repo)], repo.parent, env)
    write(repo, 'old.txt', ''.join(f'line {i}\n' for i in range(30)))
    write(repo, 'steady.txt', 'never moved\n')
    commit(repo, env, 'Initial subject with several words\n\nFirst body paragraph with enough words to wrap into multiple lines.\n\nReviewed-by: Reviewer <review@example.test>\nSigned-off-by: Alias <alias@example.test>', 1)
    git(['tag', 'base'], repo, env)
    git(['branch', 'side'], repo, env)
    git(['mv', 'old.txt', 'middle.txt'], repo, env)
    commit(repo, env, 'Rename only', 3)
    git(['mv', 'middle.txt', 'final.txt'], repo, env)
    with (repo / 'final.txt').open('a') as f:
        f.write('edited after rename\n')
    commit(repo, env, 'Rename and edit\n\nReviewed-by: One <one@example.test>\nReviewed-by: Two <two@example.test>\n continuation\nAcked-by: Other <other@example.test>', 5)
    git(['tag', 'renamed'], repo, env)
    git(['checkout', '-q', 'side'], repo, env)
    write(repo, 'side.txt', 'side change\n')
    commit(repo, env, 'Side change', 4)
    git(['checkout', '-q', 'main'], repo, env)
    merge_env = dict(env, GIT_AUTHOR_DATE='2024-01-07T12:34:56 +0530',
                     GIT_COMMITTER_DATE='2024-01-07T12:34:56 +0530')
    git(['merge', '-q', '--no-ff', '-m', 'Merge side', 'side'], repo, merge_env)
    git(['tag', 'merged'], repo, env)
    write(repo, 'steady.txt', 'never moved\nupdated\n')
    write(repo, '.mailmap', 'Canonical Person <canonical@example.test> Alias <alias@example.test>\n')
    commit(repo, env, 'A final subject long enough for padding and wrapping\n\nBody with several words to demonstrate wrapping and indentation.\n\nReviewed-by: One <one@example.test>\nReviewed-by: Two <two@example.test>\n continuation\nAcked-by: Other <other@example.test>\nSigned-off-by: Alias <alias@example.test>', 9)
    git(['branch', 'equiv', 'base'], repo, env)
    git(['checkout', '-q', 'equiv'], repo, env)
    git(['cherry-pick', 'side'], repo, dict(env, GIT_COMMITTER_DATE='2024-01-08T12:34:56 +0530'))
    git(['checkout', '-q', 'main'], repo, env)
    write(repo, 'extra.map', 'External Name <external@example.test> <alias@example.test>\n')
    blob = git(['hash-object', '-w', '--stdin'], repo, env,
               b'Blob Name <blob@example.test> <alias@example.test>\n')
    git(['update-ref', 'refs/mailmap', blob], repo, env)
    # A resolved conflict produces actual combined hunks, including a hunk
    # inherited wholesale from a parent (dense --cc suppresses that hunk).
    git(['checkout', '-qb', 'conflict-left', 'base'], repo, env)
    write(repo, 'conflict.txt', 'left\n' + 'context\n' * 10 + 'left tail\n')
    commit(repo, env, 'Left conflict', 10)
    git(['checkout', '-qb', 'conflict-right', 'base'], repo, env)
    write(repo, 'conflict.txt', 'right\n' + 'context\n' * 10 + 'right tail\n')
    commit(repo, env, 'Right conflict', 11)
    git(['checkout', '-q', 'conflict-left'], repo, env)
    rc, _, _ = run(['git', 'merge', '--no-commit', 'conflict-right'], repo, env)
    if rc != 1:
        raise RuntimeError('expected a fixture merge conflict')
    write(repo, 'conflict.txt', 'resolved\n' + 'context\n' * 10 + 'left tail\n')
    commit(repo, env, 'Resolved conflict', 12)
    git(['tag', 'resolved'], repo, env)
    git(['checkout', '-q', 'main'], repo, env)


def state(repo, env):
    result = {}
    for key, args in [('refs', ['show-ref', '--head']), ('head', ['rev-parse', 'HEAD']),
                      ('index', ['ls-files', '-s']),
                      ('status', ['status', '--porcelain=v1', '--ignored']),
                      ('config', ['config', '--local', '--list'])]:
        result[key] = run(['git', *args], repo, env)
    commits = git(['rev-list', '--all'], repo, env).splitlines()
    result['commits'] = {c: run(['git', 'cat-file', '-p', c], repo, env) for c in commits}
    return result


def packed(result, repo):
    rc, out, err = result
    # Only fixture paths are normalised; all other output bytes are exact.
    return dict(exit=rc, stdout_b64=base64.b64encode(out.replace(str(repo).encode(), b'<repo>')).decode(),
                stderr_b64=base64.b64encode(err.replace(str(repo).encode(), b'<repo>')).decode())


def compare(name, args, base, env, config=()):
    if MATCH and MATCH not in name:
        return
    g, n = base.parent / 'git-case', base.parent / 'ngit-case'
    try:
        for repo in (g, n):
            shutil.copytree(base, repo)
            for k, v in config:
                git(['config', k, v], repo, env)
        expected = packed(run(['git', *args], g, env), g)
        actual_raw = run([*COMMAND, *args], n, env)
        actual = packed(actual_raw, n)
        expected_state, actual_state = state(g, env), state(n, env)
        unexpected = actual_raw[0] < 0 or bool(BAD.search(actual_raw[1] + actual_raw[2]))
        equal = expected == actual and expected_state == actual_state
        REPORT['comparisons'] += 1
        REPORT['exact'] += int(equal and not unexpected)
        REPORT['failures'] += int(not equal)
        REPORT['unexpected_errors'] += int(unexpected)
        REPORT['cases'].append(dict(name=name, argv=args, expected=expected, actual=actual,
                                    equal=equal, state_equal=expected_state == actual_state))
        if not equal:
            print('FAIL ' + name + ': ' + repr((expected, actual, expected_state == actual_state)), flush=True)
    finally:
        for repo in (g, n):
            shutil.rmtree(repo, ignore_errors=True)


def review_fixture(base, env, name):
    repo = base.parent / name
    git(['init', '-q', '-b', 'main', str(repo)], repo.parent, env)
    return repo


def review_history(base, env):
    repo = review_fixture(base, env, 'review-history')
    write(repo, 'f', 'initial\n')
    write(repo, 'g', 'initial\n')
    commit(repo, env, 'I', 1)
    git(['tag', 'I'], repo, env)
    git(['checkout', '-qb', 'topic'], repo, env)
    write(repo, 'f', 'topic\n')
    commit(repo, env, 'A', 2)
    git(['checkout', '-q', 'main'], repo, env)
    write(repo, 'g', 'main\n')
    commit(repo, env, 'B', 3)
    git(['merge', '-q', '--no-ff', '-m', 'M', 'topic'], repo, env)
    for output in ('-p', '--stat', '--name-status'):
        compare('review-12-path-merge-' + output,
                ['log', '-m', output, '--format=%s', '--', 'f'], repo, env)
    for command in ('log', 'show'):
        for opts in (['--first-parent', '-m'], ['-m', '--first-parent']):
            compare('review-13-' + command + repr(opts),
                    [command, '-1', *opts, '-p', 'HEAD'], repo, env)
    git(['checkout', '-qb', 'ours', 'I'], repo, env)
    git(['merge', '-q', '--no-ff', '-s', 'ours', '-m', 'Ours', 'topic'], repo, env)
    for opts in ([], ['--parents'], ['--graph']):
        compare('review-14-retain-treesame-' + repr(opts),
                ['log', '--simplify-merges', *opts, '--format=%s:%p', '--', 'f'], repo, env)
    git(['checkout', '-qb', 'patch-base', 'I'], repo, env)
    write(repo, 'lines', ''.join(f'line {i}\n' for i in range(12)))
    commit(repo, env, 'Patch base', 4)
    for branch, offset, day in (('left', 2, 5), ('right', 8, 6)):
        git(['checkout', '-qb', branch, 'patch-base'], repo, env)
        lines = (repo / 'lines').read_text().splitlines(keepends=True)
        lines.insert(offset, 'X\n')
        write(repo, 'lines', ''.join(lines))
        commit(repo, env, branch, day)
    for opt in ('--cherry-pick', '--cherry-mark'):
        compare('review-16-context-' + opt,
                ['log', opt, '--format=%m%s', 'left...right'], repo, env)


def review_follow(base, env):
    repo = review_fixture(base, env, 'review-follow')
    write(repo, 'path0', ''.join(f'line {i}\n' for i in range(30)))
    commit(repo, env, 'Add source', 1)
    write(repo, 'path0', (repo / 'path0').read_text() + 'changed\n')
    commit(repo, env, 'Change source', 2)
    write(repo, 'path1', (repo / 'path0').read_text())
    commit(repo, env, 'Copy source', 3)
    for output in ('--name-status', '-p', '--stat'):
        compare('review-15-copy-' + output,
                ['log', '--follow', output, '--pretty=format:%s', '--', 'path1'], repo, env)
    write(repo, 'path2', (repo / 'path1').read_text() + 'copy edit\n')
    commit(repo, env, 'Copy and edit', 4)
    compare('review-15-edited-copy',
            ['log', '--follow', '--name-status', '--format=%s', '--', 'path2'], repo, env)


def review_follow_candidates(base, env):
    text = ''.join(f'line {i:02d}\n' for i in range(30))
    # All sources must compete in one exact pass, then one inexact pass.
    for name, src_path, decoy_path, dest, exact, deleted_exact, modify in (
            ('exact-copy', 'src', 'decoy', 'dest', True, False, False),
            ('modified-source', 'src', 'decoy', 'dest', True, False, True),
            ('inexact-copy', 'src', 'decoy', 'dest', False, False, False),
            ('basename-decoy', 'src', 'old/dest', 'new/dest', False, False, False),
            ('exact-rename', 'src', 'decoy', 'dest', True, True, False),
            ('exact-copy-basename', 'old/dest', 'decoy', 'new/dest', True, True, False),
            ('exact-rename-basename', 'src', 'old/dest', 'new/dest', True, True, False),
            ('exact-copy-tie', 'a/dest', 'z-decoy', 'new/dest', True, True, False),
            ('inexact-renames', 'src', 'old/dest', 'new/dest', False, False, False)):
        repo = review_fixture(base, env, 'review-10-' + name)
        write(repo, src_path, text)
        write(repo, decoy_path, text if deleted_exact else text.replace('line 04', 'decoy04').replace('line 05', 'decoy05'))
        commit(repo, env, 'Add candidates', 1)
        write(repo, src_path, text + 'source history\n')
        commit(repo, env, 'Change source', 2)
        if deleted_exact:
            write(repo, decoy_path, (repo / src_path).read_text())
            commit(repo, env, 'Change decoy', 3)
        write(repo, dest, (repo / src_path).read_text() + ('' if exact else 'copy edit\n'))
        (repo / decoy_path).unlink()
        if name == 'inexact-renames':
            (repo / src_path).unlink()
        if modify:
            write(repo, src_path, 'replacement source\n')
        commit(repo, env, 'Copy and remove decoy', 4)
        for output in ('--name-status', '-p', '--stat'):
            compare('review-10-' + name + '-' + output,
                    ['log', '--follow', output, '--format=%s', '--', dest], repo, env)
    repo = review_fixture(base, env, 'review-10-matrix-tie')
    for path in ('a', 'b', 'c', 'd', 'e'):
        write(repo, path, text[:len(text) // 2] if path in ('a', 'b') else text)
    commit(repo, env, 'Add five sources', 1)
    write(repo, 'dest', text + 'copy edit\n')
    commit(repo, env, 'Copy with five candidates', 2)
    for output in ('--name-status', '-p', '--stat'):
        compare('review-10-matrix-tie-' + output,
                ['log', '--follow', output, '--format=%s', '--', 'dest'], repo, env)


def review_binary_cherry(base, env):
    for name, old, left, right, attrs in (
            ('binary-new', '\0old\n', '\0new\n', '\0n ew\n', ''),
            ('binary-add', None, '\0new\n', '\0n ew\n', ''),
            ('binary-old-only', '\0old\n', 'new\n', 'n ew\n', ''),
            ('binary-equal', '\0old\n', '\0new\n', '\0new\n', ''),
            ('binary-attribute', 'old\n', 'new\n', 'n ew\n', 'f -diff\n'),
            ('binary-named-attribute', 'old\n', 'new\n', 'n ew\n', 'f diff=binary\n'),
            ('text-attribute', '\0old\n', '\0new\n', '\0n ew\n', 'f diff\n'),
            ('binary-delete', '\0base\n', None, None, '')):
        repo = review_fixture(base, env, 'review-11-' + name)
        write(repo, '.gitattributes', attrs)
        if old is not None:
            write(repo, 'f', old)
        commit(repo, env, 'Binary base', 1)
        git(['tag', 'binary-base'], repo, env)
        for branch, value, day in (('left', left, 2), ('right', right, 4)):
            git(['checkout', '-qb', branch, 'binary-base'], repo, env)
            if name == 'binary-delete':
                write(repo, 'f', '\0old\n' if branch == 'left' else '\0o ld\n')
                commit(repo, env, branch + ' preimage', day)
            if value is None:
                (repo / 'f').unlink()
            else:
                write(repo, 'f', value)
            commit(repo, env, branch + ' change', day + 1)
        for option in ('--cherry-pick', '--cherry-mark'):
            for sides in ([], ['--left-right']):
                compare('review-11-' + name + option + repr(sides),
                        ['log', option, *sides, '--format=%m%s', 'left...right'], repo, env)


def review_driver_cherry(base, env):
    # No textconv is configured: patch IDs use the driver's binary setting
    # and raw blobs, rather than the textconv used for displayed patches.
    for name, attrs, config, nul in (
            ('named-binary', 'diff=review', [('diff.review.binary', 'true')], False),
            ('named-text', 'diff=review', [('diff.review.binary', 'false')], True),
            ('named-auto', 'diff=review', [('diff.review.binary', 'auto')], True),
            ('named-default-text', 'diff=review', [], False),
            ('named-default-binary', 'diff=review', [], True),
            ('missing-default-binary', 'diff=missing', [('diff.default.binary', 'true')], False),
            ('missing-default-text', 'diff=missing', [('diff.default.binary', 'false')], True),
            ('named-case', 'diff=Review', [('diff.Review.binary', 'false')], True),
            ('minus-diff', '-diff', [('diff.default.binary', 'false')], False),
            ('binary-macro', 'binary', [('diff.default.binary', 'false')], False),
            ('named-unset', 'diff=unset', [('diff.unset.binary', 'true')], False),
            ('named-binary-text', 'diff=binary', [('diff.binary.binary', 'false')], True),
            ('forced-text', 'diff', [('diff.default.binary', 'true')], True),
            ('unspecified', '!diff', [('diff.default.binary', 'true')], False)):
        repo = review_fixture(base, env, 'review-f6-' + name)
        write(repo, '.gitattributes', 'f ' + attrs + '\n')
        prefix = '\0' if nul else ''
        write(repo, 'f', prefix + 'old\n')
        commit(repo, env, 'Driver base', 1)
        git(['tag', 'driver-base'], repo, env)
        for branch, value, day in (('left', 'new', 2), ('right', 'n ew', 4)):
            git(['checkout', '-qb', branch, 'driver-base'], repo, env)
            write(repo, 'f', prefix + value + '\n')
            commit(repo, env, branch + ' driver change', day)
        for option in ('--cherry-pick', '--cherry-mark'):
            for sides in ([], ['--left-right']):
                compare('review-f6-' + name + option + repr(sides),
                        ['log', option, *sides, '--format=%m%s', 'left...right'],
                        repo, env, config=config)


def review_formats(base, env):
    for field in ('author', 'committer'):
        for option in ('--use-mailmap', '--no-use-mailmap'):
            for pattern in ('Canonical Person', 'canonical@example.test', 'Alias', 'alias@example.test'):
                compare('review-17-' + field + option + pattern,
                        ['log', option, '--' + field + '=' + pattern, '--format=%s'], base, env)
    repo = base.parent / 'review-mailmap'
    shutil.copytree(base, repo)
    write(repo, 'another', 'another identity\n')
    commit(repo, dict(env, GIT_AUTHOR_NAME='Canonical Person',
                     GIT_AUTHOR_EMAIL='canonical@example.test'), 'Canonical author', 13)
    for opts in (['-sne'], ['-ne'], ['-se']):
        compare('review-shortlog-group-' + repr(opts), ['shortlog', *opts, 'HEAD'], repo, env)
    for command in ('log', 'show'):
        for value in ('true', 'false'):
            compare('review-output-' + command + value, [command, '-1', '--stat'], repo, env,
                    [('log.mailmap', value), ('color.ui', 'always')])
    for value in ('true', 'false'):
        compare('review-bisect-output-' + value, ['bisect', 'start', 'HEAD', 'HEAD^'], repo, env,
                [('log.mailmap', value), ('color.ui', 'always')])
    write(repo, 'another', 'modified identity commit\n')
    commit(repo, env, 'Mapped modification', 14)
    for value in ('true', 'false'):
        compare('review-bisect-mailmap-color-' + value, ['bisect', 'start', 'HEAD', 'HEAD^'], repo, env,
                [('log.mailmap', value), ('color.ui', 'always')])


def review_dates(base, env):
    repo = review_fixture(base, env, 'review-dates')
    write(repo, 'f', 'date boundary\n')
    commit(repo, dict(env, GIT_AUTHOR_DATE='2024-06-01T00:00:00 +0000',
                     GIT_COMMITTER_DATE='2024-06-01T00:00:00 +0000'), 'Dated', 1)
    # commit() sets its own dates, so create the boundary with explicit plumbing.
    git(['commit', '--amend', '-q', '--no-edit', '--date=2024-06-01T00:00:00 +0000'], repo,
        dict(env, GIT_COMMITTER_DATE='2024-06-01T00:00:00 +0000'))
    for value in ('1 year 1 month ago', '1 month 1 year ago', '2 years 3 months ago',
                  '1 year 2 weeks 1 month ago', '1 month 2 days ago'):
        for opt in ('--since', '--until'):
            compare('review-18-' + opt + value, ['log', opt + '=' + value, '--format=%s'],
                    repo, dict(env, GIT_TEST_DATE_NOW='1735689600'))


def follow(base, env):
    review_follow(base, env)
    review_follow_candidates(base, env)
    for path in ('final.txt', 'steady.txt'):
        for opts in (['--format=%h %s'], ['--stat'], ['--name-status'], ['-p'],
                     ['--format=%s', '--stat'], ['--format=%s', '-p']):
            compare('follow-' + path + '-'.join(opts), ['log', '--follow', *opts, '--', path], base, env)
    for rev in ('renamed', 'merged'):
        compare('follow-at-' + rev, ['log', '--follow', '--format=%s', rev, '--', 'final.txt'], base, env)
    compare('follow-two-paths', ['log', '--follow', '--', 'final.txt', 'steady.txt'], base, env)


def dates(base, env):
    review_dates(base, env)
    for mode in ('relative', 'human', 'local', 'short', 'iso', 'iso-strict', 'rfc', 'raw', 'unix',
                 'format:%Y/%m/%d %H:%M:%S %z %a', 'format-local:%Y/%m/%d %H:%M:%S %z'):
        for command in ('log', 'show'):
            compare('dates-' + command + '-' + mode,
                    [command, '-s', '-2', '--format=%ad|%cd', '--date=' + mode], base, env)
    for span in (0, 45, 95, 3600, 86400, 8*86400, 40*86400, 400*86400):
        for mode in ('relative', 'human'):
            compare(f'date-{mode}-{span}', ['log', '-1', '--format=%ad', '--date=' + mode],
                    base, dict(env, GIT_TEST_DATE_NOW=str(1704783896 + span)))
    for opt in ('--since', '--until', '--after', '--before'):
        for value in ('2 weeks ago', 'yesterday', 'last monday', '2024-01-04 00:00:00 +0000'):
            compare('date-select-' + opt + value, ['log', '--format=%s', opt + '=' + value], base,
                    dict(env, GIT_TEST_DATE_NOW='1704870236') if value == 'yesterday' else env)
    compare('date-config', ['log', '-1', '--format=%ad'], base, env, [('log.date', 'relative')])


def formats(base, env):
    review_formats(base, env)
    specs = ['%(trailers)', '%(trailers:only)', '%(trailers:key=Reviewed-by)',
             '%(trailers:key=Reviewed-by,valueonly)', '%(trailers:keyonly)',
             '%(trailers:unfold)', '%(trailers:separator=%x2C,key_value_separator=%x3D)',
             '%(trailers:key=Reviewed-by,separator=%x00,unfold=true,valueonly=true)',
             '%w(32,2,4)%s%n%b', '%w(20)%b%w(0)%n%s',
             '%<(60)%s|', '%>(60)%s|', '%><(60)%s|',
             '%aN|%aE|%cN|%cE|%an|%ae|%cn|%ce', '%aL|%cL']
    for align in ('<', '>', '><'):
        for trunc in ('trunc', 'ltrunc', 'mtrunc'):
            specs.append('%' + align + '(12,' + trunc + ')%s|')
    for spec in specs:
        compare('format-' + spec, ['log', '-1', '--format=' + spec], base, env)
    for color in ('always', 'never', 'auto'):
        for spec in ('%C(red)%s%C(reset)', '%C(auto,red)%s%C(auto,reset)',
                     '%C(always,blue)%h%C(always,reset)', '%C(bold green)%s%C(reset)',
                     '%C(auto)%h%d', '%C(auto)%H%C(reset)%s'):
            compare('color-' + color + spec, ['log', '-1', '--color=' + color, '--format=' + spec], base, env)
    for color in ('always', 'never'):
        for output in ([], ['--stat'], ['-p']):
            compare('color-builtin-' + color + repr(output),
                    ['log', '-1', '--color=' + color, *output], base, env)
    for ui in ('always', 'never', 'auto'):
        compare('color-ui-' + ui, ['log', '-1', '--format=%C(red)%s%C(reset)'], base, env, [('color.ui', ui)])
    for cfg in ([], [('mailmap.file', 'extra.map')], [('mailmap.blob', 'refs/mailmap')],
                [('mailmap.file', 'extra.map'), ('mailmap.blob', 'refs/mailmap')]):
        for option in ([], ['--use-mailmap'], ['--no-use-mailmap']):
            compare('mailmap-' + repr((cfg, option)), ['log', '-1', *option, '--format=%aN|%aE|%cN|%cE|%an|%ae'], base, env, cfg)
        compare('mailmap-builtin-' + repr(cfg), ['log', '-1', '--use-mailmap'], base, env, cfg)
        compare('mailmap-shortlog-' + repr(cfg), ['shortlog', '-sne', 'HEAD'], base, env, cfg)
    for value in ('true', 'false'):
        compare('log-mailmap-' + value, ['log', '-1'], base, env, [('log.mailmap', value)])


def history(base, env):
    review_history(base, env)
    review_binary_cherry(base, env)
    review_driver_cherry(base, env)
    for option in ('-m', '--cc', '-c'):
        for output in ('-p', '--stat', '--name-status', '--format=%h %s'):
            compare('history-' + option + output, ['log', '-3', option, output], base, env)
    for option in ('-m', '--cc', '-c'):
        for output in ('-p', '--stat', '--name-status'):
            compare('resolved-' + option + output,
                    ['log', '-1', option, output, 'resolved'], base, env)
        compare('show-resolved-' + option, ['show', option, 'resolved'], base, env)
    for opts in (['--boundary', '-2', '--oneline', 'main'],
                 ['--left-right', '--oneline', 'renamed...equiv'],
                 ['--left-right', '--cherry-mark', '--oneline', 'renamed...equiv'],
                 ['--left-right', '--cherry-mark', '--graph', '--oneline', 'renamed...equiv']):
        compare('history-builtin-' + repr(opts), ['log', *opts], base, env)
    for opts in (['--cherry-mark', '--left-right', 'renamed...equiv'], ['--simplify-merges'], ['--simplify-merges', '--', 'side.txt'],
                 ['--simplify-merges', '--parents', '--', 'steady.txt'],
                 ['--ancestry-path', 'base..main'], ['--ancestry-path=side', 'base..main'],
                 ['--ancestry-path=renamed', 'base..main'], ['--boundary', 'base..main'],
                 ['--boundary', '-2', 'main'], ['--left-right', 'side...renamed'],
                 ['--left-right', '--boundary', 'side...renamed'],
                 ['--cherry-pick', '--left-right', 'side...equiv'],
                 ['--cherry-mark', '--left-right', 'side...equiv']):
        compare('history-' + repr(opts), ['log', '--format=%m%h %s', *opts], base, env)


def e2e(base, env):
    # Each review step observes the same rename/merge/mailmap repository snapshot.
    for i, args in enumerate((['log', '--graph', '--oneline', '--all'],
                             ['log', '--follow', '--name-status', '--', 'final.txt'],
                             ['show', '--cc', 'merged'],
                             ['log', '-m', '--stat', '-2'],
                             ['log', '--use-mailmap', '--format=%aN <%aE>: %s'],
                             ['shortlog', '-sne', 'HEAD'],
                             ['log', '--date=human', '--format=%h %ad %s'],
                             ['log', '--simplify-merges', '--parents', '--', 'side.txt'])):
        compare('review-step-' + str(i + 1), args, base, env)


def main():
    mode = sys.argv[1] if len(sys.argv) == 2 else ''
    if mode not in ('follow', 'dates', 'format', 'history', 'e2e'):
        raise SystemExit('usage: ux-log.py follow|dates|format|history|e2e')
    env = {k: v for k, v in os.environ.items() if not k.startswith(('GIT_', 'NGIT_')) and k != 'VISUAL'}
    with tempfile.TemporaryDirectory(prefix='ngit-ux-log-') as tmp:
        root = Path(tmp)
        home = root / 'home'
        home.mkdir()
        write(home, '.gitconfig', '[user]\n\tname = Alias\n\temail = alias@example.test\n[init]\n\tdefaultBranch = main\n')
        env.update(HOME=str(home), TZ='UTC', LC_ALL='C', GIT_CONFIG_NOSYSTEM='1',
                   GIT_AUTHOR_NAME='Alias', GIT_AUTHOR_EMAIL='alias@example.test',
                   GIT_COMMITTER_NAME='Alias', GIT_COMMITTER_EMAIL='alias@example.test',
                   GIT_AUTHOR_DATE='2024-01-09T12:34:56 +0530', GIT_COMMITTER_DATE='2024-01-09T12:34:56 +0530',
                   GIT_TEST_DATE_NOW='1705320000')
        if NO_ELC:
            env['NGIT_NO_ELC'] = NO_ELC
        try:
            base = root / 'base'
            fixture(base, env)
            globals()[{'format': 'formats'}.get(mode, mode)](base, env)
        except Exception as exc:
            REPORT['unexpected_errors'] += 1
            REPORT['cases'].append(dict(error=str(exc)))
            print('ERROR ' + str(exc), flush=True)
    REPORT['seconds'] = round(time.monotonic() - START, 3)
    REPORT_DIR.mkdir(parents=True, exist_ok=True)
    (REPORT_DIR / f'ux-log-{mode}.json').write_text(json.dumps(REPORT, indent=2) + '\n')
    print(f"ux-log {mode}: comparisons={REPORT['comparisons']} exact={REPORT['exact']} intended={len(REPORT['intended'])} failures={REPORT['failures']} unexpected_errors={REPORT['unexpected_errors']}")
    return int(bool(REPORT['failures'] or REPORT['unexpected_errors'] or not REPORT['comparisons']))


if __name__ == '__main__':
    sys.exit(main())
