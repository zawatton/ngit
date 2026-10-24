#!/bin/sh
# Object deletion must not leave commit-graphs referring to removed commits.
# Usage: test/gc-commit-graph.sh monolithic|split [gc|prune|repack]
set -eu
root=$(cd "$(dirname "$0")/.." && pwd)
graph=${1:?commit-graph mode required}
operation=${2:-gc}
case $graph in monolithic|split) ;; *) exit 2 ;; esac
case $operation in gc|prune|repack) ;; *) exit 2 ;; esac
sh "$root/tools/build-elc.sh"
fx=${NGIT_FIXTURE_DIR:-${TMPDIR:-/tmp}/ngit-fixtures}
mkdir -p "$fx"
out=$(mktemp -d "$fx/.commit-graph-XXXXXXXX")
trap 'rm -rf "$out"' EXIT
export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null GIT_OPTIONAL_LOCKS=0
export GIT_AUTHOR_NAME='Test Author' GIT_AUTHOR_EMAIL=test@example.com
export GIT_COMMITTER_NAME='Test Author' GIT_COMMITTER_EMAIL=test@example.com
export GIT_AUTHOR_DATE='1700600000 +0900' GIT_COMMITTER_DATE='1700600000 +0900'
repo=$out/repo
git init -q "$repo"
git -C "$repo" config gc.auto 0
git -C "$repo" config maintenance.auto false
git -C "$repo" config core.logAllRefUpdates true
echo base >"$repo/file"
git -C "$repo" add file
git -C "$repo" commit -qm base
base=$(git -C "$repo" rev-parse HEAD)
if [ "$graph" = split ]; then
  git -C "$repo" commit-graph write --reachable --split
fi
echo discarded >>"$repo/file"
git -C "$repo" commit -qam discarded
discarded=$(git -C "$repo" rev-parse HEAD)
if [ "$graph" = split ]; then
  git -C "$repo" commit-graph write --reachable --split=no-merge
  test "$(find "$repo/.git/objects/info/commit-graphs" -name 'graph-*.graph' | wc -l)" -eq 2
else
  git -C "$repo" commit-graph write --reachable
  test -s "$repo/.git/objects/info/commit-graph"
fi
git -C "$repo" fsck --full --strict --no-progress >"$out/before-fsck" 2>&1
# Packed objects exercise the deleting-repack path; prune needs loose objects.
if [ "$operation" != prune ]; then git -C "$repo" repack -adq; fi
git -C "$repo" reset -q --hard "$base"
git -C "$repo" reflog expire --expire=now --expire-unreachable=now --all
git -C "$repo" rev-list --objects --all | LC_ALL=C sort >"$out/before"
case $operation in
  gc) "$root/bin/ngit" -C "$repo" gc --prune=now ;;
  prune)
    # A dry run must neither remove objects nor invalidate optional caches.
    graph_files() {
      (cd "$repo/.git/objects/info"
       for path in commit-graph commit-graphs/commit-graph-chain commit-graphs/graph-*.graph; do
         if [ -f "$path" ]; then sha256sum "$path"; fi
       done)
    }
    graph_files >"$out/before-graph"
    test -s "$out/before-graph"
    "$root/bin/ngit" -C "$repo" prune --dry-run >"$out/dry"
    graph_files >"$out/after-graph"
    cmp "$out/before-graph" "$out/after-graph"
    git -C "$repo" cat-file -e "$discarded"
    git -C "$repo" commit-graph verify
    "$root/bin/ngit" -C "$repo" prune --expire=now ;;
  repack) "$root/bin/ngit" -C "$repo" repack -a -d -q ;;
esac
if git -C "$repo" cat-file -e "$discarded" 2>/dev/null; then
  echo "FAIL: $graph $operation retained the unreachable commit"
  exit 1
fi
git -C "$repo" rev-list --objects --all | LC_ALL=C sort >"$out/after"
cmp "$out/before" "$out/after"
if ! git -C "$repo" fsck --full --strict --no-progress >"$out/fsck" 2>&1; then
  cat "$out/fsck"
  echo "FAIL: $graph $operation left an invalid object database"
  exit 1
fi
git -C "$repo" commit-graph verify
echo "PASS: $graph $operation removed unreachable objects and preserved fsck validity"
