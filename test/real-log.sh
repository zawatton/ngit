#!/bin/sh
# Read-only log and branch commands on the real repository REPO, against git.
#   walk   - full-history log --oneline, and log --format=fuller -n 300
#   diff   - log -n 50 --stat, log --oneline -- FILE and -- DIR
#   graph  - log --graph --oneline -n 500
#   branch - branch -a -vv and branch --merged
#   show   - tip patch, five earlier commit headers/stats, and a raw blob
#   inspect - three blames (including an old path), three tracked searches,
#             describe, shortlog and the count of all reachable commits
# Every ngit run's stdout and exit status must equal git's, and ngit's runs
# must take at most LIMIT seconds in total.
# Usage: test/real-log.sh REPO MODE LIMIT
set -u
root=$(cd "$(dirname "$0")/.." && pwd)
ngit="$root/bin/ngit"
# Byte code makes ngit several times faster; without Emacs, sources run.
sh "$root/tools/build-elc.sh" || :
repo=$1 mode=$2 limit=$3
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
export GIT_OPTIONAL_LOCKS=0
spent=0 fail=0 n=0
run() { # run ARGS...: compare one command, add ngit's time to $spent
  n=$((n + 1))
  echo "checking $*"
  git -C "$repo" "$@" >"$out/git.$n" 2>/dev/null; gs=$?
  s=$(date +%s.%N)
  remaining=$(echo "$limit - $spent" | bc)
  if [ "$(echo "$remaining > 0" | bc)" = 1 ]; then
    timeout --kill-after=2s "${remaining}s" "$ngit" -C "$repo" "$@" >"$out/ngit.$n" 2>"$out/err"; ns=$?
  else
    : >"$out/ngit.$n"; : >"$out/err"; ns=124
  fi
  e=$(date +%s.%N)
  spent=$(echo "$spent + $e - $s" | bc)
  echo "ngit $*: $(echo "$e - $s" | bc)s"
  if [ $gs -ne $ns ] || ! cmp -s "$out/git.$n" "$out/ngit.$n"; then
    echo "MISMATCH: $* [git=$gs ngit=$ns]"
    diff "$out/git.$n" "$out/ngit.$n" | head -6; head -3 "$out/err"; fail=1
  fi
}
# Pin the history so commits made while the test runs do not matter.
head=$(git -C "$repo" rev-parse HEAD) || exit 2
case $mode in
  walk)
    run log --oneline "$head"
    run log --format=fuller -n 300 "$head" ;;
  diff)
    file=$(git -C "$repo" log --format= --name-only -n 200 "$head" | sort | uniq -c | sort -rn | awk 'NR==1 {print $2}')
    dir=$(dirname "$file")
    run log -n 50 --stat "$head"
    run log --oneline "$head" -- "$file"
    run log --oneline "$head" -- "$dir" ;;
  graph)
    run log --graph --oneline -n 500 "$head" ;;
  branch)
    run branch -a -vv
    run branch --merged "$head" ;;
  show)
    run show "$head"
    for i in 1 2 3 4 5; do
      run show -s "$head~$i"
      run show --stat "$head~$i"
    done
    # Choose a small top-level blob from the pinned tree, not the work tree.
    file=$(git -C "$repo" ls-tree -l "$head" | awk '
      $2 == "blob" && $4 <= 8192 {
        sub(/^[^\t]*\t/, ""); print; exit
      }')
    [ -n "$file" ] || { echo "no small top-level blob"; exit 2; }
    run show "$head:$file" ;;
  inspect)
    # Select text paths up to 8 KiB, including the oldest surviving path.  The two
    # others have the most changes among the recent history's small files.
    git -C "$repo" ls-tree -rl "$head" >"$out/tree"
    git -C "$repo" log --no-renames --reverse --diff-filter=A --format= --name-only "$head" >"$out/added"
    git -C "$repo" log --no-renames -n 500 --format= --name-only "$head" >"$out/changed"
    python3 - "$out" <<'PY'
import collections, pathlib, sys
out = pathlib.Path(sys.argv[1])
eligible = set()
for line in (out / 'tree').read_text().splitlines():
    meta, path = line.split('\t', 1)
    mode, kind, oid, size = meta.split()
    if kind == 'blob' and mode in ('100644', '100755') and 0 < int(size) <= 8192:
        if path.endswith(('.org', '.el', '.sh', '.py', '.md')) or path in ('AGENTS.md', '.gitignore'):
            eligible.add(path)
added = (out / 'added').read_text().splitlines()
old = next((p for p in added if p in eligible), None)
counts = collections.Counter((out / 'changed').read_text().splitlines())
paths = [old] if old else []
for path, _ in counts.most_common():
    if path in eligible and path not in paths:
        paths.append(path)
    if len(paths) == 3:
        break
if len(paths) != 3:
    raise SystemExit('need three small text files')
(out / 'files').write_text('\n'.join(paths) + '\n')
PY
    [ $? = 0 ] || exit 2
    while IFS= read -r file; do run blame "$head" -- "$file"; done <"$out/files"
    run grep TODO
    run grep defun
    run grep '電気'
    run describe --always "$head"
    run shortlog -sn "$head"
    run rev-list --count --all ;;
  *) echo "unknown mode $mode"; exit 2 ;;
esac
echo "ngit total: ${spent}s (limit ${limit}s)"
[ $fail = 0 ] && [ "$(echo "$spent <= $limit" | bc)" = 1 ]
