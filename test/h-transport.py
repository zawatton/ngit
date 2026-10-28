#!/usr/bin/env python3
"""Local transport: all, e2e, controls, and deferred pre-existing differences."""
import json
import os
from pathlib import Path
import re
import resource
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
COMMAND = shlex.split(os.environ.get('NGIT_UX_COMMAND', str(ROOT / 'bin/ngit')))
OUT = Path(os.environ.get('NGIT_REPORT_DIR', str(ROOT / 'target/h-transport')))
BASELINE = Path(os.environ.get('NGIT_H_BASELINE', 'target/defects-h/baseline'))
if not BASELINE.is_absolute():
    BASELINE = ROOT / BASELINE
BAD = re.compile(rb'void-function|void-variable|wrong-type-argument|Lisp error|'
                 rb'Cannot open load file|[Oo]ut of memory|resource exhausted')
LIMIT = 8388608 * 1024
resource.setrlimit(resource.RLIMIT_AS, (LIMIT, LIMIT))


def run(command, cwd, env):
    process = subprocess.Popen(command, cwd=cwd, env=env, stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, start_new_session=True)
    try:
        out, err = process.communicate(timeout=40)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGKILL)
        process.communicate()
        raise RuntimeError('command timeout: ' + repr(command))
    return dict(rc=process.returncode, out=out.decode('utf-8', 'surrogateescape'),
                err=err.decode('utf-8', 'surrogateescape'))


def git(root, env, *args, cwd=None):
    result = run(['git', *args], cwd or root, env)
    if result['rc']:
        raise RuntimeError('Git fixture/state failure: ' + repr((args, result)))
    return result


def fixture(root):
    root.mkdir()
    for directory in ('home', 'template', 'outside'):
        (root / directory).mkdir()
    env = {k: v for k, v in os.environ.items() if not k.startswith('GIT_')}
    env.update(HOME=str(root / 'home'), XDG_CONFIG_HOME=str(root / 'home/xdg'),
               GIT_CONFIG_NOSYSTEM='1', GIT_TEMPLATE_DIR=str(root / 'template'),
               GIT_TERMINAL_PROMPT='0', GIT_OPTIONAL_LOCKS='0', LC_ALL='C', TZ='UTC',
               GIT_AUTHOR_NAME='Fixture', GIT_AUTHOR_EMAIL='fixture@example.test',
               GIT_COMMITTER_NAME='Fixture', GIT_COMMITTER_EMAIL='fixture@example.test',
               GIT_AUTHOR_DATE='1700000000 +0000', GIT_COMMITTER_DATE='1700000000 +0000')
    git(root, env, 'init', '-q', '-b', 'main', 'source')
    (root / 'source/file').write_text('base\n')
    git(root, env, '-C', 'source', 'add', 'file')
    git(root, env, '-C', 'source', 'commit', '-qm', 'base')
    git(root, env, 'clone', '-q', '--no-local', 'source', 'dest')
    (root / 'dest/sub/deep').mkdir(parents=True)
    (root / 'source/file').write_text('base\nremote change\n')
    git(root, env, '-C', 'source', 'add', 'file')
    git(root, env, '-C', 'source', 'commit', '-qm', 'remote change')
    git(root, env, 'init', '-q', '--bare', '-b', 'main', 'sink')
    return env


def state(root, env):
    """Compare logical objects, refs, logs, index entries, config and user bytes."""
    result = {}
    for name in ('source', 'dest', 'sink', *(['cloned'] if (root / 'cloned').is_dir() else [])):
        directory = root / name
        gd = directory if name == 'sink' else directory / '.git'
        args = ['--git-dir=' + str(gd)]
        files = {}
        for path in sorted(gd.rglob('*')):
            relative = path.relative_to(gd).as_posix()
            if path.is_file() and (relative in ('HEAD', 'config', 'FETCH_HEAD') or
                                   relative.startswith('logs/')):
                files[relative] = path.read_bytes().hex()
        item = dict(files=files,
                    refs=git(root, env, *args, 'for-each-ref',
                             '--format=%(refname) %(objectname) %(symref)'),
                    objects=git(root, env, *args, 'cat-file', '--batch-all-objects', '--batch'),
                    fsck=git(root, env, *args, 'fsck', '--no-progress', '--connectivity-only'))
        if name != 'sink':
            item['index'] = git(root, env, *args, 'ls-files', '--stage', '-v')
            item['worktree'] = {
                p.relative_to(directory).as_posix(): [p.stat().st_mode & 0o777, p.read_bytes().hex()]
                for p in sorted(directory.rglob('*'))
                if p.is_file() and '.git' not in p.relative_to(directory).parts}
        result[name] = item
    return result


def context(cwd='dest', opts=(), variables=None, relative='..'):
    return dict(cwd=cwd, opts=list(opts), variables=variables or {}, relative=relative)


CONTEXTS = {
    'root': context(),
    'sub': context('dest/sub'),
    'C-sub': context('', opts=['-C', 'dest/sub']),
    'env-rel': context('dest/sub', variables=dict(GIT_DIR='../.git', GIT_WORK_TREE='..')),
    'env-abs': context('dest/sub', variables=dict(
        GIT_DIR='@ROOT@/dest/.git', GIT_WORK_TREE='@ROOT@/dest')),
    'env-abs-gitdir': context('dest/sub', variables=dict(
        GIT_DIR='@ROOT@/dest/.git', GIT_WORK_TREE='..')),
    'env-abs-worktree': context('dest/sub', variables=dict(
        GIT_DIR='../.git', GIT_WORK_TREE='@ROOT@/dest')),
    'option-rel': context('dest/sub', opts=['--git-dir=../.git', '--work-tree=..']),
    'C-env-rel': context('', opts=['-C', 'dest/sub'], variables=dict(
        GIT_DIR='../.git', GIT_WORK_TREE='..')),
    'C-repeat-env': context('', opts=['-C', 'dest', '-C', 'sub/deep'], variables=dict(
        GIT_DIR='../../.git', GIT_WORK_TREE='../..')),
    'C-option-rel': context('', opts=['-C', 'dest/sub', '--git-dir=../.git', '--work-tree=..']),
    'outside-env': context('outside', variables=dict(
        GIT_DIR='@ROOT@/dest/.git', GIT_WORK_TREE='@ROOT@/dest')),
    'gitdir-only': context('dest/sub', variables=dict(GIT_DIR='../.git'), relative='../..'),
}


def step(ctx, args):
    return dict(ctx, args=args)


def all_cases():
    cases = []
    for tag, ctx in CONTEXTS.items():
        forms = ['relative']
        if tag in ('root', 'env-rel', 'C-repeat-env', 'outside-env'):
            forms += ['relative-dot', 'absolute', 'file-url']
        for form in forms:
            for operation, remote in (('fetch', 'source'), ('push', 'sink')):
                if form == 'absolute':
                    url = '@ROOT@/' + remote
                elif form == 'file-url':
                    url = 'file://@ROOT@/' + remote
                else:
                    url = ctx['relative'] + ('/./' if form == 'relative-dot' else '/') + remote
                args = [operation, '-q', url, 'main']
                cases.append(('h-transport-' + '-'.join((operation, tag, form)), [step(ctx, args)]))
    for tag in ('sub', 'env-rel', 'C-repeat-env', 'outside-env'):
        cases.append(('h-transport-ls-remote-' + tag,
                      [step(CONTEXTS[tag], ['ls-remote', '../source', 'refs/heads/main'])]))
    for tag, ctx in (('root', CONTEXTS['root']),
                     ('C-sub', context('', opts=['-C', 'dest/sub']))):
        cases.append(('h-transport-clone-' + tag + '-absolute', [step(ctx,
            ['clone', '-q', '--no-local', '@ROOT@/source', '@ROOT@/cloned'])]))
    assert len(cases) == 56
    return cases


def deferred_cases():
    """Retain exact relative-clone config/reflog spelling comparisons."""
    return [('h-transport-clone-' + tag, [step(ctx,
             ['clone', '-q', '--no-local', url, '@ROOT@/cloned'])])
            for tag, ctx, url in (('root', CONTEXTS['root'], '../source'),
                                 ('C-sub', context('', opts=['-C', 'dest/sub']), '../../source'))]


def e2e_cases():
    cases = []
    for tag in ('env-rel', 'C-repeat-env'):
        ctx = CONTEXTS[tag]
        cases.append(('h-transport-e2e-' + tag, [
            step(ctx, ['fetch', '-q', '../source', 'main']),
            step(CONTEXTS['root'], ['merge', '-q', '--ff-only', '@REMOTE_HEAD@']),
            dict(write='dest/local', content='valuable local work\n'),
            step(CONTEXTS['root'], ['add', 'local']),
            step(CONTEXTS['root'], ['commit', '-q', '-m', 'local work']),
            step(ctx, ['push', '-q', '../sink', 'HEAD:main'])]))
    return cases


def evaluate(command, cases):
    rows = []
    with tempfile.TemporaryDirectory(prefix='ngit-h-transport-') as temporary:
        root = Path(temporary) / 'fixture'
        for name, steps in cases:
            sides = []
            for tool in (['git'], command):
                env = fixture(root)
                remote_head = git(root, env, '-C', 'source', 'rev-parse', 'HEAD')['out'].strip()
                results = []
                try:
                    for item in steps:
                        if 'write' in item:
                            (root / item['write']).write_text(item['content'])
                            continue
                        expand = lambda text: text.replace('@ROOT@', str(root)).replace(
                            '@REMOTE_HEAD@', remote_head)
                        invocation_env = dict(env, **{k: expand(v) for k, v in item['variables'].items()})
                        argv = [*tool, *map(expand, item['opts']), *map(expand, item['args'])]
                        result = run(argv, root / item['cwd'], invocation_env)
                        results.append(dict(result=result, state=state(root, env)))
                    sides.append(results)
                finally:
                    shutil.rmtree(root)
            unexpected = any(entry['result']['rc'] < 0 or BAD.search(
                (entry['result']['out'] + entry['result']['err']).encode('utf-8', 'surrogateescape'))
                for side in sides for entry in side)
            oracle_failed = any(entry['result']['rc'] for entry in sides[0])
            equal = sides[0] == sides[1] and not unexpected and not oracle_failed
            rows.append(dict(name=name, equal=equal, unexpected=bool(unexpected),
                             oracle_failed=bool(oracle_failed), expected=sides[0], actual=sides[1]))
            if not equal:
                print('MISMATCH ' + name, flush=True)
    return dict(comparisons=len(rows), exact=sum(row['equal'] for row in rows),
                failures=sum(not row['equal'] for row in rows),
                unexpected_errors=sum(row['unexpected'] or row['oracle_failed'] for row in rows),
                cases=rows)


def main(mode):
    OUT.mkdir(parents=True, exist_ok=True)
    report = dict(mode=mode)
    start = time.monotonic()
    failed = True
    try:
        if mode not in ('all', 'e2e', 'controls', 'deferred'):
            raise ValueError('expected all, e2e, controls or deferred')
        if mode != 'controls' and COMMAND == [str(ROOT / 'bin/ngit')]:
            build = run(['sh', str(ROOT / 'tools/build-elc.sh')], ROOT, os.environ.copy())
            (OUT / 'build-elc.log').write_text(build['out'] + build['err'])
            if build['rc']:
                raise RuntimeError('build-elc failed; see build-elc.log')
        if mode == 'controls':
            baseline_command = [str(BASELINE.resolve() / 'bin/ngit')]
            if not Path(baseline_command[0]).is_file():
                raise RuntimeError('baseline missing: ' + baseline_command[0])
            cases = all_cases() + e2e_cases()
            calibration = evaluate(['git'], cases)
            baseline = evaluate(baseline_command, cases)
            required = {'h-transport-fetch-env-rel-relative', 'h-transport-push-env-rel-relative',
                        'h-transport-e2e-env-rel'}
            red = {row['name'] for row in baseline['cases'] if not row['equal']}
            report.update(calibration=calibration, baseline=baseline,
                          required_red=sorted(required), red_verified=required <= red)
            failed = bool(calibration['failures'] or calibration['unexpected_errors'] or
                          baseline['unexpected_errors'] or not required <= red)
        else:
            cases = {'all': all_cases, 'e2e': e2e_cases, 'deferred': deferred_cases}[mode]()
            report.update(evaluate(COMMAND, cases))
            failed = bool(report['failures'] or report['unexpected_errors'])
    except Exception as error:
        report['error'] = str(error)
        report['unexpected_errors'] = report.get('unexpected_errors', 0) + 1
    report.update(seconds=round(time.monotonic() - start, 3), passed=not failed)
    (OUT / ('h-transport-' + mode + '.json')).write_text(json.dumps(report, indent=2) + '\n')
    summary = {key: value for key, value in report.items() if key != 'cases'}
    for group in ('calibration', 'baseline'):
        if group in summary:
            summary[group] = {key: value for key, value in summary[group].items() if key != 'cases'}
    print(json.dumps(summary))
    return int(failed)


if __name__ == '__main__':
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else 'all'))
