#!/bin/sh
# Real repository transport comparison. Usage: test/real-transport.sh REPO LIMIT
set -u
root=$(cd "$(dirname "$0")/.." && pwd); ngit="$root/bin/ngit"
# Byte code makes ngit several times faster; without Emacs, sources run.
sh "$root/tools/build-elc.sh" || :
repo=${1:?repository required}; limit=${2:?time limit required}
fx=${NGIT_FIXTURE_DIR:-${TMPDIR:-/tmp}/ngit-fixtures}/real-transport
rm -rf "$fx" && mkdir -p "$fx/empty-template"
trap 'rm -rf "$fx"' EXIT HUP INT TERM
export LC_ALL=C TZ=UTC GIT_CONFIG_NOSYSTEM=1 GIT_TEMPLATE_DIR="$fx/empty-template" GIT_OPTIONAL_LOCKS=0
export GIT_AUTHOR_NAME='Test Author' GIT_AUTHOR_EMAIL=test@example.com GIT_COMMITTER_NAME='Test Author' GIT_COMMITTER_EMAIL=test@example.com
export GIT_AUTHOR_DATE='1700600000 +0000' GIT_COMMITTER_DATE='1700600000 +0000'
url="file://$(cd "$repo" && pwd)"; spent=0
timed() { # All ngit invocations share LIMIT, including local preparation.
  if [ -n "${NGIT_MAX_LOAD:-}" ]; then
    while :; do
      load=$(awk '{print $1}' /proc/loadavg)
      awk -v current="$load" -v ceiling="$NGIT_MAX_LOAD" 'BEGIN {exit !(current <= ceiling)}' && break
      sleep 5
    done
  fi
  remaining=$(echo "$limit - $spent" | bc)
  [ "$(echo "$remaining > 0" | bc)" = 1 ] || return 124
  start=$(date +%s.%N); timeout "$remaining" "$ngit" "$@"; st=$?; end=$(date +%s.%N)
  spent=$(echo "$spent + $end - $start" | bc)
  echo "ngit $*: $(echo "$end - $start" | bc)s (total ${spent}s)" >&2
  return $st
}
snapshot() { d=$1; { git -C "$d" rev-parse HEAD; git -C "$d" symbolic-ref -q HEAD || :; git -C "$d" config --local --list | sed "s|$fx/[gn]-clone|@CLONE@|g" | sort; cat "$d/.git/FETCH_HEAD" 2>/dev/null | sed "s|$fx/[gn]\.git|@REMOTE@|g; s|$fx/[gn]$|@REMOTE@|g" || :; (cd "$d/.git" && find logs -type f 2>/dev/null | sort | while IFS= read -r f; do echo "== $f"; sed "s|$fx/[gn]\.git|@REMOTE@|g; s|$fx/[gn]-clone|@CLONE@|g" "$f"; done); git -C "$d" for-each-ref --format='%(refname) %(objectname) %(symref)' | sort; git -C "$d" ls-files --stage; git -C "$d" rev-list --objects --all | sort; python3 - "$d" <<'PY'
import hashlib, json, os, stat, sys
root = sys.argv[1]
for base, dirs, files in os.walk(root, topdown=True, followlinks=False):
    if base == root and '.git' in dirs:
        dirs.remove('.git')
    dirs.sort()
    for name in sorted(dirs + files):
        path = os.path.join(base, name)
        rel = os.path.relpath(path, root)
        mode = stat.S_IMODE(os.lstat(path).st_mode)
        if os.path.islink(path): value = ['link', os.readlink(path)]
        elif os.path.isdir(path): value = ['dir']
        else:
            with open(path, 'rb') as stream: value = ['file', hashlib.sha256(stream.read()).hexdigest()]
        print(json.dumps([rel, mode] + value, ensure_ascii=True))
PY
  } >"$2"; }
bare_state() { d=$1; { git --git-dir="$d" for-each-ref --format='%(refname) %(objectname) %(symref)' | sort; git --git-dir="$d" rev-list --objects --all | sort; git --git-dir="$d" fsck --no-progress --connectivity-only --no-dangling 2>&1; } >"$2"; }
advance_remote() { # Create the same commit in each bare without exposing it to either client.
  d=$1 parent=$(git --git-dir="$1" rev-parse refs/heads/transport)
  blob=$(printf 'remote-only transport object\n' | git --git-dir="$d" hash-object -w --stdin) || return
  rm -f "$fx/remote-index"
  GIT_INDEX_FILE="$fx/remote-index" git --git-dir="$d" read-tree "$parent" || return
  GIT_INDEX_FILE="$fx/remote-index" git --git-dir="$d" update-index --add --cacheinfo "100644,$blob,remote-only-transport.txt" || return
  tree=$(GIT_INDEX_FILE="$fx/remote-index" git --git-dir="$d" write-tree) || return
  commit=$(printf 'remote-only transport commit\n' | git --git-dir="$d" commit-tree "$tree" -p "$parent") || return
  git --git-dir="$d" update-ref refs/heads/transport "$commit" "$parent"
}
git clone -q --template= "$url" "$fx/g-clone" || exit 2
timed clone --template= "$url" "$fx/n-clone" >/dev/null || { echo "ngit clone failed or timed out"; exit 1; }
snapshot "$fx/g-clone" "$fx/g.state"; snapshot "$fx/n-clone" "$fx/n.state"; cmp -s "$fx/g.state" "$fx/n.state" || { echo 'MISMATCH: real clone state'; diff -u "$fx/g.state" "$fx/n.state" | head -30; exit 1; }
# Local bare clones may share immutable object files; refs and new writes
# remain independent, without repacking each large source pack again.
git clone -q --template= --bare "$fx/g-clone" "$fx/g.git" || exit 2
git clone -q --template= --bare "$fx/n-clone" "$fx/n.git" || exit 2
printf 'transport\n' >"$fx/g-clone/ngit-real-transport.txt"; cp "$fx/g-clone/ngit-real-transport.txt" "$fx/n-clone/"
git -C "$fx/g-clone" add ngit-real-transport.txt && git -C "$fx/g-clone" commit -qm transport
timed -C "$fx/n-clone" add ngit-real-transport.txt >/dev/null || { echo 'ngit add failed or timed out'; exit 1; }
timed -C "$fx/n-clone" commit -q -m transport >/dev/null || { echo 'ngit commit failed or timed out'; exit 1; }
git -C "$fx/g-clone" push -q "$fx/g.git" HEAD:refs/heads/transport || exit 2
timed -C "$fx/n-clone" push "$fx/n.git" HEAD:refs/heads/transport >/dev/null || { echo 'ngit push failed or timed out'; exit 1; }
# Neither client has this commit or its blob, so the following fetches must
# receive a pack instead of merely updating a ref to an existing object.
advance_remote "$fx/g.git" || exit 2
advance_remote "$fx/n.git" || exit 2
git -C "$fx/g-clone" fetch "$fx/n.git" refs/heads/transport:refs/remotes/check/transport >/dev/null || exit 2
timed -C "$fx/n-clone" fetch "$fx/g.git" refs/heads/transport:refs/remotes/check/transport >/dev/null || { echo 'ngit fetch failed or timed out'; exit 1; }
snapshot "$fx/g-clone" "$fx/g.final"; snapshot "$fx/n-clone" "$fx/n.final"; cmp -s "$fx/g.final" "$fx/n.final" || { echo 'MISMATCH: real transport final state'; diff -u "$fx/g.final" "$fx/n.final" | head -30; exit 1; }
bare_state "$fx/g.git" "$fx/g.bare" || exit 1
bare_state "$fx/n.git" "$fx/n.bare" || exit 1
cmp -s "$fx/g.bare" "$fx/n.bare" || { echo 'MISMATCH: real transport remote refs/objects'; diff -u "$fx/g.bare" "$fx/n.bare" | head -30; exit 1; }
echo "ngit total: ${spent}s (limit ${limit}s)"
[ "$(echo "$spent <= $limit" | bc)" = 1 ]
