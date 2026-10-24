#!/bin/sh
# Semantic maintenance comparisons: pack selection need not match git.
set -eu
root=$(cd "$(dirname "$0")/.." && pwd)
ngit="$root/bin/ngit"
# Byte code makes ngit several times faster; without Emacs, sources run.
sh "$root/tools/build-elc.sh" || :
fx=${NGIT_FIXTURE_DIR:-${TMPDIR:-/tmp}/ngit-fixtures}
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
export GIT_CONFIG_NOSYSTEM=1 GIT_OPTIONAL_LOCKS=0
export GIT_AUTHOR_NAME='Test Author' GIT_AUTHOR_EMAIL=test@example.com
export GIT_COMMITTER_NAME='Test Author' GIT_COMMITTER_EMAIL=test@example.com
export GIT_AUTHOR_DATE='1700600000 +0900' GIT_COMMITTER_DATE='1700600000 +0900'
fail=0 tests=0

same() {
  tests=$((tests + 1))
  if ! cmp -s "$2" "$3"; then
    echo "MISMATCH: $1"
    diff -u "$2" "$3" | head -25 || :
    fail=1
  fi
}

state() {
  (
    cd "$1"
    git show-ref --head --dereference || test "$?" = 1
    if [ -f .git/packed-refs ]; then cat .git/packed-refs; fi
    find .git/refs .git/logs -type f | LC_ALL=C sort | while read -r path; do
      echo "== $path"; cat "$path"
    done
    git rev-list --all --objects | LC_ALL=C sort
    git fsck --no-progress
  ) >"$2" 2>&1
}

maintenance() { # source, setup, command...
  src=$1 setup=$2; shift 2
  rm -rf "$out/w" "$out/snapshot"
  cp -a "$src" "$out/w"
  (cd "$out/w" && eval "$setup")
  cp -a "$out/w/.git" "$out/snapshot"
  (cd "$out/w" && git "$@") >"$out/git.out" 2>"$out/git.err" && gs=0 || gs=$?
  state "$out/w" "$out/git.state"
  rm -rf "$out/w/.git"; cp -a "$out/snapshot" "$out/w/.git"
  (cd "$out/w" && "$ngit" "$@") >"$out/ngit.out" 2>"$out/ngit.err" && ns=0 || ns=$?
  state "$out/w" "$out/ngit.state"
  echo "$gs" >"$out/git.status"; echo "$ns" >"$out/ngit.status"
  same "$* status" "$out/git.status" "$out/ngit.status"
  same "$* stdout" "$out/git.out" "$out/ngit.out"
  same "$* state" "$out/git.state" "$out/ngit.state"
  if [ "$ns" != 0 ]; then cat "$out/ngit.err"; fi
}

fsck_check() {
  (cd "$out/w" && git fsck --no-progress "$@") >"$out/git.out" 2>"$out/git.err" && gs=0 || gs=$?
  (cd "$out/w" && "$ngit" fsck --no-progress "$@") >"$out/ngit.out" 2>"$out/ngit.err" && ns=0 || ns=$?
  echo "$gs" >"$out/git.status"; echo "$ns" >"$out/ngit.status"
  same "fsck $* stdout" "$out/git.out" "$out/ngit.out"
  same "fsck $* stderr" "$out/git.err" "$out/ngit.err"
  same "fsck $* status" "$out/git.status" "$out/ngit.status"
}

case $1 in
pack-write)
  cp -a "$fx/loose" "$out/w"
  cd "$out/w"
  git cat-file --batch-all-objects --batch-check='%(objectname)' >"$out/oids"
  "$ngit" pack-objects --stdout <"$out/oids" >"$out/new.pack"
  git index-pack --strict "$out/new.pack" >"$out/hash"
  git verify-pack -v "$out/new.idx" >"$out/verify"
  # A loose source must actually exercise the delta search.
  if ! grep -q 'chain length = [1-9]' "$out/verify"; then
    echo 'MISMATCH: pack writer did not find deltas'; fail=1
  fi
  awk 'length($1) == 40 {print $1}' "$out/verify" | LC_ALL=C sort >"$out/got"
  same 'pack object set' "$out/oids" "$out/got"
  "$ngit" verify-pack -v "$out/new.idx" >"$out/ngit.verify"
  same 'verify-pack -v' "$out/verify" "$out/ngit.verify"
  cp "$out/new.idx" "$out/expected.idx"
  rm "$out/new.idx"
  "$ngit" index-pack "$out/new.pack" >"$out/ngit.hash"
  same 'index-pack checksum' "$out/hash" "$out/ngit.hash"
  same 'index-pack idx bytes' "$out/expected.idx" "$out/new.idx"
  # Test a git-produced REF_DELTA pack as well as the OFS_DELTA output.
  git pack-objects --stdout <"$out/oids" >"$out/git.pack"
  git index-pack "$out/git.pack" >"$out/hash"
  rm "$out/expected.idx"
  cp "$out/git.idx" "$out/expected.idx"; rm "$out/git.idx"
  "$ngit" index-pack "$out/git.pack" >"$out/ngit.hash"
  same 'git pack idx bytes' "$out/expected.idx" "$out/git.idx"
  same 'git pack checksum' "$out/hash" "$out/ngit.hash"
  "$ngit" index-pack --stdin <"$out/git.pack" >"$out/stdin.hash"
  git verify-pack .git/objects/pack/*.idx
  printf 'pack\t%s\n' "$(cat "$out/hash")" >"$out/stdin.expected"
  same 'index-pack --stdin' "$out/stdin.expected" "$out/stdin.hash"
  "$ngit" pack-objects --revs --all "$out/named" </dev/null >"$out/named.hash"
  named="$out/named-$(cat "$out/named.hash")"
  git verify-pack -v "$named.idx" >"$out/named.verify"
  awk 'length($1) == 40 {print $1}' "$out/named.verify" | LC_ALL=C sort >"$out/got"
  git rev-list --all --objects | cut -d' ' -f1 | LC_ALL=C sort >"$out/expected"
  same '--all pack set' "$out/expected" "$out/got"
  printf 'HEAD\n^HEAD~2\n' >"$out/revs"
  "$ngit" pack-objects --stdout --revs --window=0 --depth=0 <"$out/revs" >"$out/range.pack"
  git index-pack --strict "$out/range.pack" >/dev/null
  git verify-pack -v "$out/range.idx" >"$out/range.verify"
  awk 'length($1) == 40 {print $1}' "$out/range.verify" | LC_ALL=C sort >"$out/got"
  git rev-list --objects HEAD ^HEAD~2 | cut -d' ' -f1 | LC_ALL=C sort >"$out/expected"
  same 'revision exclusion set' "$out/expected" "$out/got"
  # Repacking already packed deltas must preserve the complete object set.
  "$ngit" -C "$fx/packed" pack-objects --stdout <"$out/oids" >"$out/reuse.pack"
  git index-pack --strict "$out/reuse.pack" >/dev/null
  git verify-pack -v "$out/reuse.idx" >"$out/reuse.verify"
  awk 'length($1) == 40 {print $1}' "$out/reuse.verify" | LC_ALL=C sort >"$out/got"
  same 'source pack reuse set' "$out/oids" "$out/got"
  python3 "$root/test/make-pack-fixture.py" "$out/mixed.pack"
  git index-pack "$out/mixed.pack" >"$out/hash"
  cp "$out/mixed.idx" "$out/mixed.expected"; rm "$out/mixed.idx"
  "$ngit" index-pack "$out/mixed.pack" >"$out/ngit.hash"
  same 'mixed REF/OFS chain idx' "$out/mixed.expected" "$out/mixed.idx"
  same 'mixed REF/OFS checksum' "$out/hash" "$out/ngit.hash"
  (cd "$out" && git verify-pack -v mixed.idx) >"$out/relative.git"
  (cd "$out" && "$ngit" verify-pack -v mixed.idx) >"$out/relative.ngit"
  same 'verify-pack outside repo, relative path' "$out/relative.git" "$out/relative.ngit"
  printf unreferenced | git hash-object -w --stdin >/dev/null
  "$ngit" pack-objects --all --stdout </dev/null >"$out/all.pack"
  git index-pack --strict "$out/all.pack" >/dev/null
  git verify-pack -v "$out/all.idx" >"$out/all.verify"
  awk 'length($1) == 40 {print $1}' "$out/all.verify" | LC_ALL=C sort >"$out/got"
  same '--all implies revisions and excludes dangling' "$out/oids" "$out/got"
  # Failed stdin indexing must preserve an already installed valid pack.
  python3 - "$out/git.pack" "$out/bad.pack" <<'PY'
import pathlib, sys
b = bytearray(pathlib.Path(sys.argv[1]).read_bytes())
b[20] ^= 255
pathlib.Path(sys.argv[2]).write_bytes(b)
PY
  if "$ngit" index-pack --stdin <"$out/bad.pack" >/dev/null 2>&1; then
    echo 'MISMATCH: corrupt stdin pack accepted'; fail=1
  fi
  installed=".git/objects/pack/pack-$(cat "$out/stdin.expected" | cut -f2).pack"
  same 'failed index-pack preserves installed pack' "$out/git.pack" "$installed"
  chmod u+w "$out/mixed.idx"
  printf x | dd of="$out/mixed.idx" bs=1 seek=8 conv=notrunc 2>/dev/null
  if "$ngit" verify-pack "$out/mixed.idx" >/dev/null 2>&1; then
    echo 'MISMATCH: corrupt index accepted'; fail=1
  fi
  # Ordinary index-pack accepts duplicate entries; equal OIDs retain their
  # physical order in the index's offset table.
  python3 - "$out/duplicate.pack" <<'PY'
import hashlib, pathlib, struct, sys, zlib
entry = b'\x33' + zlib.compress(b'dup')
body = b'PACK' + struct.pack('>II', 2, 2) + entry + entry
pathlib.Path(sys.argv[1]).write_bytes(body + hashlib.sha1(body).digest())
PY
  git index-pack "$out/duplicate.pack" >"$out/hash"
  cp "$out/duplicate.idx" "$out/duplicate.expected"; rm "$out/duplicate.idx"
  "$ngit" index-pack "$out/duplicate.pack" >"$out/ngit.hash"
  same 'duplicate OID index ordering' "$out/duplicate.expected" "$out/duplicate.idx"
  same 'duplicate OID pack checksum' "$out/hash" "$out/ngit.hash"
  # Repacking a source pack with duplicate OIDs must emit the reachable
  # object once and must not bulk-copy both physical entries.
  mkdir "$out/duplicate-source"
  git -C "$out/duplicate-source" init -q -b main
  mkdir -p "$out/duplicate-source/.git/objects/pack"
  duplicate_hash=$(cat "$out/hash")
  cp "$out/duplicate.pack" \
    "$out/duplicate-source/.git/objects/pack/pack-$duplicate_hash.pack"
  cp "$out/duplicate.idx" \
    "$out/duplicate-source/.git/objects/pack/pack-$duplicate_hash.idx"
  duplicate_oid=$(printf dup | git hash-object --stdin)
  git -C "$out/duplicate-source" update-ref refs/tags/blobtag "$duplicate_oid"
  cd "$out"
  maintenance "$out/duplicate-source" : repack -a -d -q
  # A duplicate chosen as a forward REF delta can depend on an OFS delta
  # whose physical base is the duplicate's later full entry.  Repacking must
  # materialize this source rather than turn the valid chain into a cycle.
  python3 - "$out/duplicate-cycle.pack" <<'PY'
import hashlib, pathlib, struct, sys, zlib
def oid(data):
    return hashlib.sha1(b'blob ' + str(len(data)).encode() + b'\0' + data).digest()
def varint(n):
    out = bytearray()
    while n >= 128:
        out.append((n & 127) | 128); n >>= 7
    return bytes(out + bytes([n]))
def header(kind, size):
    out = bytearray([(kind << 4) | (size & 15)]); size >>= 4
    while size:
        out[-1] |= 128; out.append(size & 127); size >>= 7
    return bytes(out)
def ofs(n):
    out = bytearray([n & 127])
    while n >> 7:
        n = (n >> 7) - 1; out.insert(0, 128 | (n & 127))
    return bytes(out)
def delta(base, target):
    return varint(len(base)) + varint(len(target)) + bytes([len(target)]) + target
a, b = b'duplicate A\n', b'duplicate B\n'
body = b'PACK' + struct.pack('>II', 2, 3)
d = delta(b, a)
body += header(7, len(d)) + oid(b) + zlib.compress(d)
aoff = len(body)
body += header(3, len(a)) + zlib.compress(a)
d = delta(a, b)
body += header(6, len(d)) + ofs(len(body) - aoff) + zlib.compress(d)
pathlib.Path(sys.argv[1]).write_bytes(body + hashlib.sha1(body).digest())
PY
  git index-pack "$out/duplicate-cycle.pack" >"$out/cycle.hash"
  mkdir "$out/duplicate-cycle-source"
  git -C "$out/duplicate-cycle-source" init -q -b main
  mkdir -p "$out/duplicate-cycle-source/.git/objects/pack"
  cycle_hash=$(cat "$out/cycle.hash")
  cp "$out/duplicate-cycle.pack" \
    "$out/duplicate-cycle-source/.git/objects/pack/pack-$cycle_hash.pack"
  cp "$out/duplicate-cycle.idx" \
    "$out/duplicate-cycle-source/.git/objects/pack/pack-$cycle_hash.idx"
  a_oid=$(printf 'duplicate A\n' | git hash-object --stdin)
  b_oid=$(printf 'duplicate B\n' | git hash-object --stdin)
  git -C "$out/duplicate-cycle-source" update-ref refs/tags/a "$a_oid"
  git -C "$out/duplicate-cycle-source" update-ref refs/tags/b "$b_oid"
  maintenance "$out/duplicate-cycle-source" : repack -a -d -q
  # Large incompressible loose data takes the native inflate/recompress path;
  # large highly compressible data stays below the compressed-size gate and
  # exercises the ordinary fallback.  Build these in a fresh repository so
  # the shared fixture object set cannot be changed by the regression.
  git -C "$fx/loose" cat-file --batch-all-objects --batch-check='%(objectname)' |
    LC_ALL=C sort >"$out/base.before"
  mkdir "$out/large-loose"
  git -C "$out/large-loose" init -q -b main
  python3 - "$out/random.payload" "$out/compressible.payload" <<'PY'
import hashlib, pathlib, sys
parts = []
seed = b'ngit native loose regression'
while sum(map(len, parts)) < 256 * 1024:
    seed = hashlib.sha256(seed).digest()
    parts.append(seed)
pathlib.Path(sys.argv[1]).write_bytes(b''.join(parts)[:256 * 1024])
pathlib.Path(sys.argv[2]).write_bytes(b'z' * (512 * 1024))
PY
  random_oid=$(git -C "$out/large-loose" hash-object -w "$out/random.payload")
  compressible_oid=$(git -C "$out/large-loose" hash-object -w "$out/compressible.payload")
  random_path="$out/large-loose/.git/objects/$(printf '%s' "$random_oid" | cut -c1-2)/$(printf '%s' "$random_oid" | cut -c3-)"
  compressible_path="$out/large-loose/.git/objects/$(printf '%s' "$compressible_oid" | cut -c1-2)/$(printf '%s' "$compressible_oid" | cut -c3-)"
  if [ "$(wc -c <"$random_path")" -lt 65536 ] ||
     [ "$(wc -c <"$compressible_path")" -ge 65536 ]; then
    echo 'MISMATCH: large loose native/fallback size gates'; fail=1
  fi
  printf '%s\n%s\n' "$random_oid" "$compressible_oid" | LC_ALL=C sort >"$out/large.oids"
  "$ngit" -C "$out/large-loose" pack-objects --stdout \
    <"$out/large.oids" >"$out/large.pack"
  git index-pack --strict "$out/large.pack" >/dev/null
  git verify-pack -v "$out/large.idx" >"$out/large.verify"
  awk 'length($1) == 40 {print $1}' "$out/large.verify" | LC_ALL=C sort >"$out/large.got"
  same 'large loose native/fallback pack set' "$out/large.oids" "$out/large.got"
  git init -q --bare "$out/large-roundtrip.git"
  cp "$out/large.pack" "$out/large.idx" "$out/large-roundtrip.git/objects/pack/"
  git --git-dir="$out/large-roundtrip.git" cat-file blob "$random_oid" >"$out/random.got"
  git --git-dir="$out/large-roundtrip.git" cat-file blob "$compressible_oid" >"$out/compressible.got"
  same 'large native loose roundtrip' "$out/random.payload" "$out/random.got"
  same 'large fallback loose roundtrip' "$out/compressible.payload" "$out/compressible.got"
  git -C "$fx/loose" cat-file --batch-all-objects --batch-check='%(objectname)' |
    LC_ALL=C sort >"$out/base.after"
  same 'large loose regression leaves fixture unchanged' "$out/base.before" "$out/base.after"
  ;;
gc)
  maintenance "$fx/loose" : pack-refs --all
  maintenance "$fx/loose" : pack-refs --all --no-prune
  maintenance "$fx/loose" : reflog expire --expire=never --all
  maintenance "$fx/loose" : reflog expire --expire=now --all
  maintenance "$fx/loose" 'git reset -q --soft HEAD~1' \
    reflog expire --expire=never --expire-unreachable=now --all
  maintenance "$fx/loose" : reflog delete 'HEAD@{1}'
  maintenance "$fx/loose" : reflog delete --rewrite --updateref 'refs/heads/main@{0}'
  maintenance "$fx/loose" : gc --auto
  maintenance "$fx/loose" : repack -a -d -l -q
  maintenance "$fx/packed" : repack -a -d -q
  maintenance "$fx/loose" : gc --prune=now
  maintenance "$fx/packed" : gc --prune=now
  maintenance "$fx/loose" 'oid=$(printf packed-dangling | git hash-object -w --stdin); printf "%s\n" "$oid" | git pack-objects .git/objects/pack/pack >/dev/null; git prune-packed' gc -q
  maintenance "$fx/packed" 'for p in .git/objects/pack/*.pack; do : >"${p%.pack}.keep"; done' repack -a -d -q
  if ! ls "$out/w"/.git/objects/pack/*.keep >/dev/null 2>&1; then
    echo 'MISMATCH: repack removed kept packs'; fail=1
  fi
  maintenance "$fx/loose" 'git repack -a -d -q; git for-each-ref --format="delete %(refname)" | git update-ref --stdin; git reflog expire --expire=now --all; rm -f .git/index' gc -q --prune=now
  git -C "$out/w" cat-file --batch-all-objects --batch-check='%(objectname)' >"$out/empty.objects"
  if [ -s "$out/empty.objects" ]; then
    echo 'MISMATCH: gc retained expired-only pack'; fail=1
  fi
  # Prune compares the entire remaining loose set, not only reachable refs.
  setup='printf "unreferenced\n" | git hash-object -w --stdin >/dev/null'
  for flags in '-n -v' '-v'; do
    # shellcheck disable=SC2086
    maintenance "$fx/loose" "$setup" prune --expire=now $flags
    git -C "$out/w" cat-file --batch-all-objects --batch-check='%(objectname)' >"$out/remain"
    if [ "$flags" = '-v' ]; then
      git -C "$fx/loose" cat-file --batch-all-objects --batch-check='%(objectname)' >"$out/expected"
      same 'prune remaining object set' "$out/expected" "$out/remain"
    fi
  done
  for r in loose packed; do
    git -C "$fx/$r" count-objects >"$out/git.count"
    "$ngit" -C "$fx/$r" count-objects >"$out/ngit.count"
    same "count-objects $r" "$out/git.count" "$out/ngit.count"
    git -C "$fx/$r" count-objects -v >"$out/git.count"
    "$ngit" -C "$fx/$r" count-objects -v >"$out/ngit.count"
    same "count-objects -v $r" "$out/git.count" "$out/ngit.count"
  done
  # Historical trees span many windows; insertions shift entry boundaries
  # while source packs supply shared OFS delta bases.
  mkdir "$out/history"
  git -C "$out/history" init -q -b main
  python3 - "$out/history" <<'PY'
import subprocess, sys
repo = sys.argv[1]
def git(*args, data=None):
    return subprocess.check_output(['git', '-C', repo, *args], input=data).strip()
entries = {}
for i in range(200):
    name = ('item-%03d-' % i) + ('x' * (30 + i % 70))
    entries[name] = git('hash-object', '-w', '--stdin', data=('payload %d\n' % i).encode())
parent = None
for version in range(12):
    name = ('item-%03d-new-' % (version * 17)) + ('y' * version)
    entries[name] = git('hash-object', '-w', '--stdin', data=('new %d\n' % version).encode())
    if version:
        del entries[sorted(entries)[version * 7]]
    tree = git('mktree', data=b''.join(b'100644 blob ' + oid + b'\t' + name.encode() + b'\n'
                                     for name, oid in sorted(entries.items())))
    args = ['commit-tree', tree.decode()]
    if parent:
        args += ['-p', parent.decode()]
    parent = git(*args, data=('history %d\n' % version).encode())
git('update-ref', 'refs/heads/main', parent.decode())
git('repack', '-a', '-d', '-q')
PY
  maintenance "$out/history" : gc --prune=now
  # A malformed textual edge must fail before publishing a pack, even when
  # permissive hex decoding could alias a separately reachable valid tree.
  cp -a "$fx/loose" "$out/bad-edge"
  python3 - "$out/bad-edge" <<'PY'
import hashlib, pathlib, subprocess, sys, zlib
repo = pathlib.Path(sys.argv[1])
def git(*args):
    return subprocess.check_output(['git', '-C', str(repo), *args]).strip()
def store(kind, data):
    raw = kind + b' ' + str(len(data)).encode() + b'\0' + data
    oid = hashlib.sha1(raw).hexdigest()
    path = repo / '.git/objects' / oid[:2] / oid[2:]
    path.parent.mkdir(exist_ok=True)
    path.write_bytes(zlib.compress(raw))
    return oid
blob = bytes.fromhex(git('rev-parse', 'HEAD:a.txt').decode())
for i in range(10000):
    tree = store(b'tree', b'100644 f' + str(i).encode() + b'\0' + blob)
    if '0' in tree:
        break
else:
    raise RuntimeError('could not build zero-nibble fixture')
bad = tree.replace('0', 'g', 1)
commit = git('cat-file', 'commit', 'HEAD').split(b'\n', 1)[1]
oid = store(b'commit', b'tree ' + bad.encode() + b'\n' + commit + b'\n')
head = (repo / '.git/HEAD').read_text().strip().removeprefix('ref: ')
(repo / '.git' / head).write_text(oid + '\n')
(repo / '.git/refs/tags/canonical-tree').write_text(tree + '\n')
PY
  find "$out/bad-edge/.git/objects/pack" -type f | LC_ALL=C sort >"$out/before.pack"
  "$ngit" -C "$out/bad-edge" gc --prune=now >"$out/bad.out" 2>"$out/bad.err" && ns=0 || ns=$?
  find "$out/bad-edge/.git/objects/pack" -type f | LC_ALL=C sort >"$out/after.pack"
  tests=$((tests + 1))
  if [ "$ns" = 0 ] || ! grep -q 'fatal: bad object' "$out/bad.err" ||
     ! cmp -s "$out/before.pack" "$out/after.pack"; then
    echo 'MISMATCH: gc published a pack for an invalid commit edge'; fail=1
  fi
  ;;
fsck)
  cp -a "$fx/loose" "$out/w"
  fsck_check
  fsck_check --connectivity-only
  # An unreachable commit, tree, and blob exercise dangling edge marking.
  (
    cd "$out/w"
    blob=$(printf 'dangling payload\n' | git hash-object -w --stdin)
    tree=$(printf '100644 blob %s\tfile\n' "$blob" | git mktree)
    printf 'unreachable\n' | git commit-tree "$tree" >/dev/null
    printf 'standalone\n' | git hash-object -w --stdin >/dev/null
  )
  fsck_check
  fsck_check --unreachable
  fsck_check --no-dangling
  fsck_check --connectivity-only
  fsck_check --no-reflogs --unreachable
  for kind in blob tree commit; do
    rm -rf "$out/w"; cp -a "$fx/loose" "$out/w"
    case $kind in
      blob) oid=$(git -C "$out/w" rev-parse HEAD:a.txt) ;;
      tree) oid=$(git -C "$out/w" rev-parse HEAD^{tree}) ;;
      commit) oid=$(git -C "$out/w" rev-parse HEAD~1) ;;
    esac
    path="$out/w/.git/objects/$(printf '%s' "$oid" | cut -c1-2)/$(printf '%s' "$oid" | cut -c3-)"
    rm "$path"
    fsck_check
    fsck_check --connectivity-only
  done
  for damage in empty zlib hash; do
    rm -rf "$out/w"; cp -a "$fx/loose" "$out/w"
    oid=$(git -C "$out/w" rev-parse HEAD:a.txt)
    path="$out/w/.git/objects/$(printf '%s' "$oid" | cut -c1-2)/$(printf '%s' "$oid" | cut -c3-)"
    chmod u+w "$path"
    case $damage in
      empty) : >"$path" ;;
      zlib) printf 'not zlib\n' >"$path" ;;
      hash) python3 - "$path" <<'PY'
import pathlib, sys, zlib
p = pathlib.Path(sys.argv[1])
p.write_bytes(zlib.compress(b'blob 5\0wrong'))
PY
        ;;
    esac
    fsck_check
  done
  rm -rf "$out/w"; cp -a "$fx/packed" "$out/w"
  fsck_check
  fsck_check --connectivity-only
  # Exercise object-table collisions and growth, rather than only sorting
  # names whose low hash buckets happen to differ.
  rm -rf "$out/w"; mkdir "$out/w"
  git -C "$out/w" init -q -b main
  n=0
  while [ "$n" -lt 96 ]; do
    printf 'dangling %s' "$n" | git -C "$out/w" hash-object -w --stdin >/dev/null
    n=$((n + 1))
  done
  fsck_check
  # A correctly hashed commit can still contain a pointer of the wrong type.
  blob=$(printf payload | git -C "$out/w" hash-object -w --stdin)
  bad=$(printf 'tree %s\nauthor T <t@e> 1700000000 +0000\ncommitter T <t@e> 1700000000 +0000\n\nbad\n' "$blob" |
    git -C "$out/w" hash-object --literally -w -t commit --stdin)
  git -C "$out/w" update-ref refs/heads/main "$bad"
  fsck_check
  fsck_check --connectivity-only
  path="$out/w/.git/objects/$(printf '%s' "$bad" | cut -c1-2)/$(printf '%s' "$bad" | cut -c3-)"
  chmod u+w "$path"; printf 'not zlib' >"$path"
  fsck_check --connectivity-only
  # Physical pack corruption must be found even with a good loose duplicate.
  rm -rf "$out/w"; cp -a "$fx/loose" "$out/w"
  oid=$(git -C "$out/w" rev-parse HEAD:a.txt)
  pack=$(printf '%s\n' "$oid" | git -C "$out/w" pack-objects .git/objects/pack/pack)
  path="$out/w/.git/objects/pack/pack-$pack.pack"
  python3 - "$path" <<'PY'
import pathlib, sys
p = pathlib.Path(sys.argv[1])
b = bytearray(p.read_bytes())
b[13] ^= 255
p.chmod(0o644)
p.write_bytes(b)
PY
  fsck_check
  rm -rf "$out/w"; mkdir "$out/w"
  git -C "$out/w" init -q -b main
  n=0
  while [ "$n" -lt 96 ]; do
    printf 'dangling %s' "$n" | git -C "$out/w" hash-object -w --stdin >/dev/null
    n=$((n + 1))
  done
  git -C "$out/w" cat-file --batch-all-objects --batch-check='%(objectname)' |
    git -C "$out/w" pack-objects .git/objects/pack/pack >/dev/null
  git -C "$out/w" prune-packed
  fsck_check
  fsck_check --connectivity-only
  python3 - "$out/w" <<'PY'
import pathlib, sys
p = next((pathlib.Path(sys.argv[1]) / '.git/objects/pack').glob('*.idx'))
b = bytearray(p.read_bytes())
b[-1] ^= 1
p.chmod(0o644)
p.write_bytes(b)
PY
  fsck_check
  ;;
*) echo "unknown maintenance suite $1"; exit 2 ;;
esac
echo "$1: $tests comparisons, status $fail"
exit "$fail"
