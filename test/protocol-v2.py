#!/usr/bin/env python3
"""Git protocol differential tests: preserve packet boundaries and request order.

Each comparison includes command output, client-to-server wire bytes and real-Git
repository inspection. Fixtures are copied afresh at the same path for both
clients. No expected diagnostic differences are waived.
"""
import base64
import gzip
import contextlib
import importlib.util
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
COMMAND = shlex.split(os.environ.get('NGIT_PV2_COMMAND', str(ROOT / 'bin/ngit')))
NO_ELC = os.environ.get('NGIT_NO_ELC')
REPORT_DIR = Path(os.environ.get('NGIT_REPORT_DIR', ROOT / 'target/protocol-v2/reports'))
MODES = ('select', 'lsrefs', 'e2e-ssh', 'http-discovery', 'http-fetch',
         'e2e-https', 'negotiate', 'negotiate-http', 'negotiate-v0',
         'e2e-negotiate', 'errors', 'server-option', 'fallback', 'review')
START = time.monotonic()
DEADLINE = START + 450
REPORT = dict(comparisons=0, exact=0, intended={}, failures=0,
              unexpected_errors=0, cases=[])
BASE_ENV = {k: v for k, v in os.environ.items()
            if not k.startswith(('GIT_', 'NGIT_')) and k != 'VISUAL'}
BASE_ENV.update(LC_ALL='C', LANG='C', TZ='UTC', EDITOR='true',
                GIT_CONFIG_NOSYSTEM='1', GIT_TERMINAL_PROMPT='0',
                GIT_AUTHOR_NAME='Fixture', GIT_AUTHOR_EMAIL='fixture@example.test',
                GIT_COMMITTER_NAME='Fixture', GIT_COMMITTER_EMAIL='fixture@example.test',
                GIT_AUTHOR_DATE='1700600000 +0000', GIT_COMMITTER_DATE='1700600000 +0000')
if NO_ELC is not None:
    BASE_ENV['NGIT_NO_ELC'] = NO_ELC
if 'NGIT_VERIFY_RUN' in os.environ:
    BASE_ENV['NGIT_VERIFY_RUN'] = os.environ['NGIT_VERIFY_RUN']
BAD = re.compile(rb'void-function|void-variable|wrong-type-argument|Lisp error|'
                 rb'Cannot open load file|[Oo]ut of memory|resource exhausted|'
                 rb'panicked at|error while loading shared libraries')


def process(argv, cwd=None, env=None, input=None, timeout=35, check=False,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE):
    """All descendants share a killable group, including HTTP backends."""
    budget = min(timeout, DEADLINE - time.monotonic())
    if budget <= 0:
        raise RuntimeError('mode deadline exceeded')
    if env is not None and 'NGIT_VERIFY_RUN' in BASE_ENV:
        env = dict(env, NGIT_VERIFY_RUN=BASE_ENV['NGIT_VERIFY_RUN'])
    p = subprocess.Popen(argv, cwd=cwd, env=env, stdin=subprocess.PIPE,
                         stdout=stdout, stderr=stderr, start_new_session=True)
    try:
        out, err = p.communicate(input, timeout=budget)
    except subprocess.TimeoutExpired:
        os.killpg(p.pid, signal.SIGKILL)
        p.communicate()
        raise RuntimeError('command timeout: ' + repr(argv)) from None
    result = subprocess.CompletedProcess(argv, p.returncode, out, err)
    if check and p.returncode:
        raise RuntimeError('fixture command failed: ' + repr((argv, out, err)))
    return result


def git(args, cwd, env, data=None):
    return process(['git', *map(str, args)], cwd, env, input=data, check=True).stdout.strip()


def load(name):
    spec = importlib.util.spec_from_file_location(name.replace('-', '_'),
                                                 ROOT / 'test' / (name + '.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


HTTP = load('smart-http-fixture').SmartHTTPServer
AUTH_HTTP = load('credential-http-fixture').CredentialHTTPServer


def pkt(data):
    return f'{len(data) + 4:04x}'.encode() + data


def packets(data):
    """Return payloads and special packets; keep invalid/raw tails verbatim."""
    result = []
    while data:
        try:
            size = int(data[:4], 16)
        except ValueError:
            result.append(('raw', data))
            break
        if len(data) < 4 or (size > 2 and (size < 4 or size > len(data))):
            result.append(('raw', data))
            break
        if size <= 2:
            result.append(('special', data[:4]))
            data = data[4:]
        else:
            result.append(('packet', data[4:size]))
            data = data[size:]
    return result


def wire_normalize(data):
    """Only the agent value changes; presence and all other payloads survive.

    Recompute the affected length prefix so differing agent lengths cannot cause
    false differences. v0 capabilities can contain agent alongside other words.
    """
    out = bytearray()
    offset = 0
    for kind, payload in packets(data):
        if kind == 'packet':
            normalized = payload
            if re.fullmatch(rb'agent=[^\x00\x20\n]+\n?', payload):
                normalized = re.sub(rb'agent=[^\n]+', b'agent=<AGENT>', payload)
            elif re.match(rb'^[0-9a-fA-F]{40}(?:[0-9a-fA-F]{24})? '
                          rb'(?:[0-9a-fA-F]{40}(?:[0-9a-fA-F]{24})? )?[^ \0\n]+\0', payload):
                ref, caps = payload.split(b'\0', 1)
                caps = re.sub(rb'(?<![^\x20])agent=[^\x20\n]+',
                              b'agent=<AGENT>', caps)
                normalized = ref + b'\0' + caps
            elif re.match(rb'^want [0-9a-fA-F]{40}(?:[0-9a-fA-F]{24})? ', payload):
                want, oid, caps = payload.split(b' ', 2)
                normalized = want + b' ' + oid + b' ' + re.sub(
                    rb'(?<![^\x20])agent=[^\x20\n]+', b'agent=<AGENT>', caps)
            size = len(payload) + 4
            out.extend(pkt(normalized) if normalized != payload else data[offset:offset+size])
            offset += size
        else:
            out.extend(payload)
            offset += len(payload)
    return bytes(out)


def b64(data):
    return base64.b64encode(data).decode()


def normalize(data, root, server=None):
    data = data.replace(str(root).encode(), b'<FIXTURE>')
    if server:
        data = data.replace(server.url().rstrip('/').encode(), b'https://localhost:<PORT>')
    return data


def encoded(result, root, server=None):
    return dict(exit=result.returncode,
                stdout_b64=b64(normalize(result.stdout, root, server)),
                stderr_b64=b64(normalize(result.stderr, root, server)))


def repository_state(repo, env, root, server=None):
    if not repo.exists() or not ((repo / '.git').exists() or (repo / 'HEAD').exists()):
        return None
    def inspect(args):
        return encoded(process(['git', *args], repo, env), root, server)
    gd = repo / '.git' if (repo / '.git').is_dir() else repo
    reachable = git(['rev-list', '--objects', '--all'], repo, env).splitlines()
    reachable = sorted({line.split(b' ', 1)[0].decode() for line in reachable})
    objects = git(['cat-file', '--batch-all-objects', '--batch-check=%(objectname)'],
                  repo, env).decode().splitlines()
    fetch_head = gd / 'FETCH_HEAD'
    fsck = inspect(['fsck', '--strict'])
    return dict(refs=inspect(['for-each-ref', '--format=%(refname) %(objectname) %(symref)']),
                head=inspect(['symbolic-ref', '-q', 'HEAD']),
                fetch_head_b64=b64(normalize(fetch_head.read_bytes(), root, server))
                if fetch_head.exists() else None,
                reachable=reachable, objects=sorted(objects), fsck=fsck,
                index=inspect(['ls-files', '-s']),
                status=inspect(['status', '--porcelain=v1', '--ignored']),
                clean=fsck['exit'] == 0)


def immutable_state(state):
    if state is None:
        return None
    return {k: state[k] for k in ('refs', 'head', 'reachable', 'objects', 'index', 'status')}


def append_history(repo, env, ref, count, label, parent=None, clock=1700600100):
    """One fast-import process, deterministic identities, dates and object IDs."""
    data = bytearray()
    if parent is None:
        r = process(['git', 'rev-parse', '--verify', ref], repo, env)
        parent = r.stdout.strip().decode() if r.returncode == 0 else None
    for i in range(count):
        content = (label + '-' + str(i) + '\n').encode()
        message = (label + ' ' + str(i) + '\n').encode()
        data.extend(f'commit {ref}\nmark :{i+1}\n'.encode())
        data.extend(f'author Fixture <fixture@example.test> {clock+i} +0000\n'
                    f'committer Fixture <fixture@example.test> {clock+i} +0000\n'
                    f'data {len(message)}\n'.encode() + message)
        if i:
            data.extend(f'from :{i}\n'.encode())
        elif parent:
            data.extend(f'from {parent}\n'.encode())
        data.extend(f'M 100644 inline {label}.txt\ndata {len(content)}\n'.encode() + content + b'\n')
    git(['fast-import', '--quiet'], repo, env, bytes(data))
    return git(['rev-parse', ref], repo, env).decode()


SSH = r'''#!/bin/sh
while [ $# -gt 0 ]; do
  case $1 in -G) exit 0;; -o|-p|-l|-i|-F|-J) shift 2;; -*) shift;; *) break;; esac
done
[ $# -gt 0 ] && shift
base=$(dirname "$0")
log=$base/wire/$(ls "$base/wire" | wc -l)
mkdir "$log"
if [ -f "$base/drop-protocol" ]; then unset GIT_PROTOCOL; fi
if [ "${GIT_PROTOCOL+x}" ]; then printf '%s' "$GIT_PROTOCOL" >"$log/protocol"; fi
printf '%s' "$1" >"$log/command"
if [ -f "$base/fault-command" ]; then set -- "$(cat "$base/fault-command")"; fi
mkfifo "$log/input"
exec 3<&0
tee "$log/stdin" <&3 >"$log/input" &
recorder=$!
sh -c "$1" <"$log/input" 3<&-
status=$?
exec 3<&-
kill "$recorder" 2>/dev/null || :
wait "$recorder" 2>/dev/null || :
exit "$status"
'''


class Suite:
    def __init__(self, root):
        self.root = root
        self.active = root / 'active'
        self.template = root / 'template'
        self.home = root / 'home'
        self.home.mkdir()
        (self.home / '.gitconfig').write_text(
            '[user]\nname = Fixture\nemail = fixture@example.test\n'
            '[init]\ndefaultBranch = main\n[pack]\nthreads = 1\n'
            '[gc]\nauto = 0\n[fetch]\nnegotiationAlgorithm = default\n')
        self.env = dict(BASE_ENV, HOME=str(self.home))
        self.template.mkdir()
        seed = self.template / 'seed'
        git(['init', '-q', '--template=', '-b', 'main', seed], root, self.env)
        append_history(seed, self.env, 'refs/heads/main', 35, 'main')
        fork = git(['rev-parse', 'main~10'], seed, self.env).decode()
        append_history(seed, self.env, 'refs/heads/topic', 8, 'topic', fork, 1700600200)
        append_history(seed, self.env, 'refs/heads/feature', 4, 'feature', fork, 1700600300)
        git(['read-tree', '--reset', '-u', 'main'], seed, self.env)
        git(['merge', '-q', '--no-ff', '-m', 'merge topic', 'topic'], seed, self.env)
        git(['tag', 'lightweight', 'main~2'], seed, self.env)
        git(['tag', '-a', 'annotated', '-m', 'annotated fixture', 'main'], seed, self.env)
        git(['clone', '-q', '--bare', '--template=', seed, self.template / 'remote.git'], root, self.env)
        (self.template / 'ssh').write_text(SSH)
        (self.template / 'ssh').chmod(0o755)

    def reset(self, template):
        if self.active.exists():
            shutil.rmtree(self.active)
        shutil.copytree(template, self.active)
        (self.active / 'wire').mkdir(exist_ok=True)

    def prepare(self, transport, client='empty', shape=None, version=None, config=None):
        self.reset(self.template)
        repo = self.active / 'client'
        remote = self.active / 'remote.git'
        env = dict(self.env, GIT_SSH_COMMAND=shlex.quote(str(self.active / 'ssh')))
        shutil.copytree(self.home, self.active / 'home')
        env['HOME'] = str(self.active / 'home')
        if client == 'clone':
            git(['clone', '-q', '--template=', remote, repo], self.active, env)
        else:
            git(['init', '-q', '--template=', '-b', 'main', repo], self.active, env)
        if shape:
            # Tags and remote-tracking tips at the shared base would let the
            # negotiator ACK it immediately instead of walking local history.
            for ref in git(['for-each-ref', '--format=%(refname)', 'refs/tags'], repo, env).splitlines():
                git(['update-ref', '-d', ref.decode()], repo, env)
            if shape == 'many-branches':
                for i in range(48):
                    append_history(repo, env, f'refs/heads/local-{i:02}', 3,
                                   f'branch-{i}', 'HEAD', 1700601000 + i*10)
            elif shape == 'max-in-vain':
                # An early ACK for main, then >256 unacknowledged candidates
                # before the base of the other wanted branch is found.
                tip = git(['rev-parse', 'HEAD'], repo, env).decode()
                git(['update-ref', 'refs/heads/early-common', tip], repo, env)
                append_history(remote, env, 'refs/heads/main', 1, 'shared', tip, 1700610000)
                shared = git(['rev-parse', 'main'], remote, env).decode()
                git(['fetch', '-q', str(remote), shared], repo, env)
                git(['update-ref', 'refs/heads/early-common', shared], repo, env)
                append_history(repo, env, 'refs/heads/local-long', 650, 'in-vain',
                               'refs/remotes/origin/feature', 1700601000)
                # The local feature branch must not provide its base as a tip.
                for ref in ('refs/remotes/origin/feature', 'refs/remotes/origin/topic'):
                    git(['update-ref', '--no-deref', '-d', ref], repo, env)
            else:
                count = {'small': 8, 'second-flush': 24, 'no-common-256': 600,
                         'late-common': 90, 'divergent': 24}[shape]
                append_history(repo, env, 'refs/heads/main', count, 'local',
                               clock=1700601000)
                git(['read-tree', '--reset', '-u', 'main'], repo, env)
                # Remove advertised remote tips which would shortcut the walk.
                for line in git(['for-each-ref', '--format=%(refname)', 'refs/remotes'], repo, env).splitlines():
                    git(['update-ref', '--no-deref', '-d', line.decode()], repo, env)
                if shape == 'late-common':
                    for i in range(6):
                        append_history(repo, env, f'refs/heads/unrelated-{i}', 20,
                                       f'unrelated-{i}', '', 1700603000+i*100)
            append_history(remote, env, 'refs/heads/main', 4, 'remote', clock=1700620000)
            append_history(remote, env, 'refs/heads/feature', 2, 'remote-feature', clock=1700620100)
        if version is not None:
            git(['config', '--global', 'protocol.version', str(version)], repo, env)
        for key, value in config or []:
            git(['config', '--add', key, value], repo, env)
        if transport == 'fallback-ssh':
            (self.active / 'drop-protocol').touch()
        # Origin is set after a URL has been allocated by pair().
        return env

    def transcript(self, transport, server):
        if server:
            result = []
            for method, path, headers, body, raw in server.requests:
                wire_body = body
                # Preserve spelling, order, absent vs empty and duplicates.
                selected = [(k, v) for k, v in raw if k.lower() in
                            ('git-protocol', 'content-type', 'accept', 'content-encoding')]
                # A gzip body is compared after decompression so the agent value
                # inside it is normalised; Content-Encoding itself stays compared.
                if any(k.lower() == 'content-encoding' and v.strip() == 'gzip'
                       for k, v in selected):
                    body = gzip.decompress(body)
                record = dict(method=method, path=path, headers=selected,
                              body_b64=b64(wire_normalize(body)))
                if method == 'POST':
                    record.update(content_length=[v for k, v in raw if k.lower() == 'content-length'],
                                  transfer_encoding=[(k, v) for k, v in raw
                                                     if k.lower() == 'transfer-encoding'],
                                  wire_body_b64=b64(wire_body))
                result.append(record)
            return result
        result = []
        for path in sorted((self.active / 'wire').iterdir(), key=lambda p: int(p.name)):
            protocol = path / 'protocol'
            result.append(dict(protocol=protocol.read_text() if protocol.exists() else None,
                               command=normalize((path / 'command').read_bytes(), self.root).decode(),
                               stdin_b64=b64(wire_normalize((path / 'stdin').read_bytes()))))
        return result

    def pair(self, name, operation='fetch', options=(), transport='ssh', client='empty',
             shape=None, version=2, source='cli', config=None, flow=False, error=None,
             immutable=False, expected_exit=None):
        env = self.prepare(transport, client, shape,
                           version if source == 'config' else None, config)
        if error == 'sha256':
            remote = self.active / 'remote.git'
            shutil.rmtree(remote)
            git(['init', '-q', '--bare', '--template=', '--object-format=sha256',
                 '-b', 'main', remote], self.active, env)
            append_history(remote, env, 'refs/heads/main', 2, 'sha256')
        if error and transport == 'ssh':
            (self.active / 'fault-command').write_text(shlex.join([
                sys.executable, str(Path(__file__).resolve()), '--peer',
                str(self.active / 'remote.git'), error]))
        snapshot = self.root / 'snapshot'
        if snapshot.exists():
            shutil.rmtree(snapshot)
        shutil.copytree(self.active, snapshot)
        server = None
        is_http = transport in ('http', 'auth', 'fallback-http')
        if is_http:
            cls = AUTH_HTTP if transport == 'auth' else HTTP
            server = cls(self.active, forward_protocol=transport != 'fallback-http',
                         rewrite_response=rewriter(error) if error else None,
                         backend_env=env, run_process=process)
        with server if server else contextlib.nullcontext():
            url = server.url('remote.git') if server else 'fake:' + str(self.active / 'remote.git')
            if server:
                env['GIT_SSL_CAINFO'] = server.ca_file
            if transport == 'auth':
                helper = self.root / 'credential-helper'
                helper.write_text('#!/bin/sh\nif [ "$1" = get ]; then\n'
                                  "printf 'username=alice\\npassword=password\\n'\nfi\n")
                helper.chmod(0o755)
            runs = []
            for command in (['git'], COMMAND):
                self.reset(snapshot)
                repo = self.active / 'client'
                git(['config', 'remote.origin.url', url], repo, env)
                git(['config', 'remote.origin.fetch', '+refs/heads/*:refs/remotes/origin/*'], repo, env)
                if transport == 'auth':
                    git(['config', 'credential.helper', str(helper)], repo, env)
                    # clone reads its helper from HOME, before it has a config.
                    extra = ['-c', 'credential.helper=' + str(helper)]
                else:
                    extra = []
                if source == 'cli' and version is not None:
                    extra += ['-c', 'protocol.version=' + str(version)]
                steps = [('initial', operation, list(options))]
                if flow:
                    steps += [('fetch', 'fetch', []), ('ls-remote', 'ls-remote', ['--symref']),
                              ('pull', 'pull', ['--no-rebase', '--no-edit'])]
                records = []
                for step, op, opts in steps:
                    if server:
                        server.requests.clear()
                    for p in (self.active / 'wire').iterdir():
                        shutil.rmtree(p)
                    if step in ('fetch', 'pull') and flow:
                        append_history(self.active / 'remote.git', env, 'refs/heads/main',
                                       2, 'advance-' + step, clock=1700630000 + 100*(step == 'pull'))
                    destination = self.active / 'destination' if operation == 'clone' else repo
                    work = destination if step != 'initial' else repo
                    if op == 'clone':
                        argv = [*extra, 'clone', '--template=', *opts, url, str(destination)]
                    elif op == 'ls-remote':
                        # Patterns are supplied after the URL, flags before it.
                        flags, patterns = [], []
                        words = iter(opts)
                        for word in words:
                            if word.startswith('-'):
                                flags.append(word)
                                if word in ('-o', '--server-option', '--upload-pack'):
                                    flags.append(next(words))
                            else:
                                patterns.append(word)
                        argv = [*extra, op, *flags, 'origin', *patterns]
                    elif op == 'push':
                        argv = [*extra, op, *([] if error and error.startswith('push-') else ['-q']),
                                *opts, 'origin', *(['--delete', 'main'] if error == 'push-delete'
                                                  else ['main:refs/heads/pushed'])]
                    else:
                        argv = [*extra, op, *opts]
                    before = repository_state(destination, env, self.root, server)
                    command_error = None
                    result = None
                    if not work.exists():
                        result = None
                        command_error = 'prior clone did not create destination'
                    else:
                        try:
                            result = process([*command, *argv], work, env)
                        except RuntimeError as exc:
                            command_error = str(exc)
                    after = repository_state(destination, env, self.root, server)
                    remote = repository_state(self.active / 'remote.git', env, self.root, server)
                    unexpected = result is not None and (result.returncode < 0 or result.returncode in (126, 127, 134, 139) or bool(BAD.search(result.stderr)))
                    unexpected = unexpected or bool(command_error and work.exists())
                    records.append(dict(result=encoded(result, self.root, server) if result else dict(not_run=command_error),
                                        wire=self.transcript(transport, server), state=after,
                                        remote=remote, unexpected_error=unexpected,
                                        unchanged=immutable_state(before) == immutable_state(after)
                                        if immutable else None))
                runs.append(records)
            for i, (expected, actual) in enumerate(zip(*runs)):
                for left, right in zip(expected['wire'], actual['wire']):
                    if 'wire_body_b64' in left and 'wire_body_b64' in right:
                        if left['wire_body_b64'] != right['wire_body_b64']:
                            for record in (left, right):
                                record['content_length'] = [True for _ in record['content_length']]
                        del left['wire_body_b64'], right['wire_body_b64']
                label = name + ('/' + steps[i][0] if flow else '')
                # Git 2.47.3's HTTP remote helper ignores server options even
                # on v0; the native SSH transport rejects them on v0. Keep
                # comparing the complete result and wire in either case.
                rejects_options = version == 0 and not is_http and any(
                    o.startswith('--server-option') or
                    (o == '-o' and operation != 'clone') for o in options)
                wanted_exit = expected_exit if expected_exit is not None else (128 if immutable or version in (3, 'foo') or rejects_options else 0)
                reference_ok = (expected['result'].get('exit') == wanted_exit and
                                (wanted_exit != 0 or bool(expected['wire'])))
                equal = expected == actual and not actual['unexpected_error']
                equal = equal and reference_ok
                if immutable:
                    equal = equal and expected['unchanged'] and actual['unchanged']
                    equal = equal and expected['result'].get('exit', 0) != 0 and actual['result'].get('exit', 0) != 0
                for r in (expected, actual):
                    equal = equal and all(s is None or s['clean'] for s in (r['state'], r['remote']))
                REPORT['comparisons'] += 1
                REPORT['exact'] += int(bool(equal))
                REPORT['failures'] += int(not equal)
                REPORT['unexpected_errors'] += int(actual['unexpected_error']) + int(not reference_ok)
                REPORT['cases'].append(dict(name=label, expected=expected, actual=actual,
                                            expected_exit=wanted_exit, reference_ok=reference_ok,
                                            equal=bool(equal)))
                print(('PASS ' if equal else 'FAIL ') + label, flush=True)
        shutil.rmtree(snapshot)


def rewriter(error):
    def rewrite(handler, code, headers, response):
        if error in ('bare-object-format', 'no-ls-refs') and handler.command == 'GET':
            response = b''.join(pkt(b'object-format') if error == 'bare-object-format'
                                and k == 'packet' and p.startswith(b'object-format=')
                                else b'' if error == 'no-ls-refs' and k == 'packet'
                                and p.startswith(b'ls-refs') else pkt(p) if k == 'packet' else p
                                for k, p in packets(response))
        elif error == 'sha256' and handler.command == 'GET':
            response = b''.join(pkt(p.replace(b'object-format=sha1', b'object-format=sha256'))
                                if k == 'packet' else p for k, p in packets(response))
        elif error == 'err-discovery' and handler.command == 'GET':
            response = pkt(b'ERR protocol-v2 fixture failure\n') + b'0000'
        elif handler.command == 'POST':
            if b'command=fetch' in handler.request_body:
                response = fault_response(error, response)
            elif b'command=ls-refs' in handler.request_body:
                response = refs_response(error, response)
        return code, headers, response
    return rewrite


def fault_response(error, response):
    if error == 'empty-sections-no-lf':
        return pkt(b'shallow-info') + b'0001' + pkt(b'wanted-refs') + b'0001' + fault_response('no-lf', response)
    if error in ('wanted-flush', 'wanted-response-end', 'shallow-response-end'):
        return pkt(b'wanted-refs' if error.startswith('wanted-') else b'shallow-info') + (
            b'0000' if error == 'wanted-flush' else b'0002') + response
    if error == 'no-lf':
        return b''.join(pkt(p[:-1] if p.endswith(b'\n') and not p.startswith((b'\1', b'\2', b'\3')) else p)
                        if k == 'packet' else p for k, p in packets(response))
    section, sep, line = error.partition(':')
    if sep and section in ('shallow-info', 'wanted-refs'):
        return pkt(section.encode()) + pkt(line.encode()) + b'0001' + response
    return {
        'err-fetch': pkt(b'ERR protocol-v2 fixture failure\n') + b'0000',
        'unknown-section': pkt(b'unknown-section\n') + b'0000',
        'missing-packfile': pkt(b'acknowledgments\n') + pkt(b'ready\n') + b'00010000',
        'bad-ack': pkt(b'acknowledgments\n') + pkt(b'not-an-ack\n') + b'0000',
        'ack-flush': b'0000', 'ack-delim': b'0001',
        'ack-response-end': b'0002',
        'pack-flush': b'0000', 'pack-delim': b'0001',
    }.get(error, response)


def refs_response(error, response):
    oid = b'1' * 40
    return {
        'refs-delim': b'0001',
        'refs-no-name': pkt(oid) + b'0000',
        'refs-bad-oid': pkt(b'xyz refs/heads/main') + b'0000',
        'refs-bad-peeled': pkt(oid + b' refs/tags/test peeled:xyz') + b'0000',
    }.get(error, response)


def push_peer(repo, error):
    """A receive-pack fixture that reports successful rewritten updates."""
    oid = git(['rev-parse', 'main'], repo, os.environ)
    caps = b'report-status report-status-v2 delete-refs'
    sys.stdout.buffer.write(pkt(oid + b' refs/heads/main\0' + caps + b'\n') + b'0000')
    sys.stdout.buffer.flush()
    header = sys.stdin.buffer.read(4)
    size = int(header, 16)
    command = sys.stdin.buffer.read(size - 4).split(b'\0', 1)[0]
    sys.stdin.buffer.read(4)  # command-list flush
    dst = command.split(b' ')[2]
    values = {
        'push-refname': [b'refname refs/tags/rewritten'],
        'push-old-oid': [b'old-oid ' + git(['rev-parse', 'main~1'], repo, os.environ)],
        'push-new-oid': [b'new-oid ' + git(['rev-parse', 'topic'], repo, os.environ)],
        'push-forced': [b'old-oid ' + oid, b'new-oid ' + git(['rev-parse', 'main~1'], repo, os.environ),
                        b'forced-update'],
        'push-unknown-option': [b'future-option ignored', b'old-oid invalid', b'new-oid'],
        'push-delete': [b'refname refs/heads/rewritten', b'old-oid ' + oid,
                        b'new-oid ' + b'0' * 40, b'forced-update'],
        'push-multiple': [b'refname refs/heads/rewritten'],
    }[error]
    reply = pkt(b'unpack ok\n') + pkt(b'ok ' + dst + b'\n')
    reply += b''.join(pkt(b'option ' + value + b'\n') for value in values)
    if error == 'push-multiple':
        reply += pkt(b'ok ' + dst + b'\n') + pkt(b'option refname refs/tags/second\n')
    sys.stdout.buffer.write(reply + b'0000')
    sys.stdout.buffer.flush()
    # Drain the pack after replying so neither client can hit a broken pipe.
    sys.stdin.buffer.read()


def fault_peer(repo, error):
    """Socket-free adversarial v2 SSH server, backed by real upload-pack."""
    if error.startswith('push-'):
        return push_peer(repo, error)
    env = dict(os.environ, GIT_PROTOCOL='version=2')
    ad = process(['git', 'upload-pack', '--advertise-refs', repo], repo, env,
                 check=True).stdout
    handler = type('Handler', (), dict(command='GET'))()
    ad = rewriter(error)(handler, 200, {}, ad)[2]
    sys.stdout.buffer.write(ad)
    sys.stdout.buffer.flush()
    if error == 'err-discovery':
        return
    while True:
        request = bytearray()
        while True:
            header = sys.stdin.buffer.read(4)
            if not header:
                return
            if len(header) != 4:
                raise RuntimeError('truncated request header')
            request.extend(header)
            length = int(header, 16)
            if length > 4:
                request.extend(sys.stdin.buffer.read(length - 4))
            if length == 0:
                break
        reply = process(['git', 'upload-pack', '--stateless-rpc', repo], repo,
                        env, input=bytes(request), check=True).stdout
        if b'command=fetch' in request:
            reply = fault_response(error, reply)
        else:
            reply = refs_response(error, reply)
        sys.stdout.buffer.write(reply)
        sys.stdout.buffer.flush()


def discovery_cases(s, transport='ssh', versions=(2,)):
    for version in versions:
        for name, opts in [('all', []), ('heads', ['--heads']), ('tags', ['--tags']),
                           ('refs', ['--refs']), ('pattern', ['main', 'annot*']),
                           ('symref', ['--symref'])]:
            s.pair(f'{transport}-v{version}-ls-{name}', 'ls-remote', opts,
                   transport, version=version)


def fetch_cases(s, transport='ssh', versions=(2,)):
    for version in versions:
        for name, opts in [('default', []), ('branch', ['--branch', 'topic']),
                           ('no-tags', ['--no-tags']), ('single', ['--single-branch']),
                           ('bare', ['--bare'])]:
            s.pair(f'{transport}-v{version}-clone-{name}', 'clone', opts,
                   transport, version=version)
        for name, opts, config in [
            ('default', [], None),
            ('explicit', ['origin', 'main:refs/remotes/origin/copied',
                          'topic:refs/remotes/origin/topic'], None),
            ('negative', ['origin', '+refs/heads/*:refs/remotes/origin/*', '^refs/heads/topic'], None),
            ('tags', ['--tags'], None), ('no-tags', ['--no-tags'], None),
            ('tag-following', ['origin', 'main'], None),
            ('branch-name', ['origin', 'topic'], None)]:
            s.pair(f'{transport}-v{version}-fetch-{name}', options=opts,
                   transport=transport, version=version, config=config)


def main(mode, s):
    if mode == 'select':
        for source in ('cli', 'config'):
            for version in (None, 0, 1, 2, 3, 'foo'):
                for op in ('ls-remote', 'clone', 'fetch'):
                    s.pair(f'{source}-{version}-{op}', op, version=version, source=source)
        for version in (None, 0, 1, 2):
            s.pair(f'push-v{version}', 'push', client='clone', version=version)
    elif mode == 'lsrefs':
        discovery_cases(s)
        fetch_cases(s)
    elif mode == 'e2e-ssh':
        s.pair('ssh-flow', 'clone', flow=True)
    elif mode == 'http-discovery':
        discovery_cases(s, 'http', (2, 0))
    elif mode == 'http-fetch':
        fetch_cases(s, 'http', (2, 0))
    elif mode == 'e2e-https':
        s.pair('authenticated-https-flow', 'clone', transport='auth', flow=True)
    elif mode in ('negotiate', 'negotiate-http', 'negotiate-v0'):
        transport = 'http' if mode == 'negotiate-http' else 'ssh'
        for shape in ('small', 'second-flush', 'no-common-256', 'many-branches',
                      'late-common', 'max-in-vain'):
            s.pair(shape, client='clone', shape=shape, transport=transport,
                   version=0 if mode == 'negotiate-v0' else 2)
    elif mode == 'e2e-negotiate':
        for transport in ('ssh', 'auth'):
            for op in ('fetch', 'pull'):
                opts = ['--no-rebase', '--no-edit'] if op == 'pull' else []
                s.pair(transport + '-divergent-' + op, op, opts, transport,
                       client='clone', shape='divergent')
    elif mode == 'errors':
        for error in ('err-discovery', 'err-fetch', 'unknown-section', 'missing-packfile',
                      'bad-ack', 'sha256'):
            s.pair(error, transport='ssh', client='clone', shape='small',
                   error=error, immutable=True)
        s.pair('invalid-version', version='foo', client='clone', immutable=True)
    elif mode == 'review':
        for error in ('push-refname', 'push-old-oid', 'push-new-oid', 'push-forced',
                      'push-unknown-option', 'push-delete', 'push-multiple'):
            opts = ['--no-verify'] + (['--set-upstream'] if error == 'push-multiple' else [])
            s.pair(error, 'push', opts, client='clone', error=error)
        for transport in ('ssh', 'http'):
            for error in ('bare-object-format', 'no-ls-refs', 'refs-delim',
                          'refs-no-name', 'refs-bad-oid', 'refs-bad-peeled'):
                s.pair(transport + '-' + error, 'ls-remote', transport=transport,
                       error=error, immutable=error != 'bare-object-format')
            s.pair(transport + '-no-lf-pack', transport=transport, error='no-lf')
            s.pair(transport + '-no-lf-acks', transport=transport,
                   client='clone', shape='small', error='no-lf')
            s.pair(transport + '-no-lf-nak', transport=transport,
                   client='clone', shape='second-flush', error='no-lf')
            for error in ('bare-object-format', 'empty-sections-no-lf'):
                s.pair(transport + '-fetch-' + error, transport=transport, error=error)
            for error in ('ack-flush', 'ack-delim', 'ack-response-end'):
                s.pair(transport + '-' + error, transport=transport, client='clone',
                       shape='small', error=error, immutable=True)
            for error in ('pack-flush', 'pack-delim', 'shallow-info:nonsense',
                          'shallow-info:shallow xyz', 'shallow-info:unshallow xyz',
                          'shallow-info:unshallow ' + '1' * 40,
                          'wanted-refs:nonsense', 'wanted-refs:' + '1' * 40 + ' refs/heads/unknown',
                          'wanted-flush', 'wanted-response-end', 'shallow-response-end'):
                s.pair(transport + '-' + error, transport=transport, error=error,
                       immutable=True)
            s.pair(transport + '-wanted-unrequested', options=['origin', 'main'],
                   transport=transport, error='wanted-refs:' + '1' * 40 + ' refs/heads/topic',
                   immutable=True)
    elif mode == 'server-option':
        for transport in ('ssh', 'http'):
            for version in (2, 0):
                for op in ('fetch', 'clone', 'ls-remote'):
                    for spelling in ('short', 'long', 'config'):
                        # Git 2.47.3 clone -o selects the origin name; only its
                        # long form sends server options. This Git also ignores
                        # remote.*.serverOption. Compare those behaviors too.
                        opts = ['-o', 'one', '-o', 'two=2'] if spelling == 'short' else ['--server-option=one', '--server-option=two=2']
                        config = None
                        if spelling == 'config':
                            opts = []
                            config = [('remote.origin.serverOption', 'one'),
                                      ('remote.origin.serverOption', 'two=2')]
                        s.pair(f'{transport}-v{version}-{op}-{spelling}', op, opts,
                               transport, version=version, config=config)
    elif mode == 'fallback':
        for transport in ('fallback-ssh', 'fallback-http'):
            for op in ('ls-remote', 'clone', 'fetch'):
                s.pair(transport + '-' + op, op, transport=transport)


if __name__ == '__main__':
    if len(sys.argv) == 4 and sys.argv[1] == '--peer':
        fault_peer(Path(sys.argv[2]), sys.argv[3])
        sys.exit(0)
    if len(sys.argv) != 2 or sys.argv[1] not in MODES:
        sys.exit('usage: protocol-v2.sh {' + '|'.join(MODES) + '}')
    mode = sys.argv[1]
    try:
        with tempfile.TemporaryDirectory(prefix='ngit-protocol-v2-') as tmp:
            main(mode, Suite(Path(tmp)))
    except Exception as error:
        REPORT['unexpected_errors'] += 1
        REPORT['error'] = f'{type(error).__name__}: {error}'
        print('HARNESS ERROR ' + REPORT['error'], file=sys.stderr, flush=True)
    REPORT['seconds'] = round(time.monotonic() - START, 3)
    REPORT_DIR.mkdir(parents=True, exist_ok=True)
    path = REPORT_DIR / f'protocol-v2-{mode}.json'
    path.write_text(json.dumps(REPORT, indent=2) + '\n')
    print(f'protocol-v2 {mode}: comparisons={REPORT["comparisons"]} exact={REPORT["exact"]} '
          f'failures={REPORT["failures"]} unexpected_errors={REPORT["unexpected_errors"]}', flush=True)
    sys.exit(int(bool(REPORT['failures'] or REPORT['unexpected_errors'] or not REPORT['comparisons'])))
