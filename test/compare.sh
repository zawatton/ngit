#!/bin/sh
# Compare ngit against git for one suite of commands; exit 0 iff all match.
# Usage: test/compare.sh SUITE   (SUITE: basic, rev, cat, all-objects,
#        deltas, worktree, ls-tree, log, index, attr-config, diff, status,
#        status-long, real, real-diff)
# Fixtures are rebuilt into $NGIT_FIXTURE_DIR (default: $TMPDIR/ngit-fixtures)
# when missing.  The `real` and `real-diff` suites run against
# $NGIT_REAL_REPO.  git must not refresh the index while comparing, or
# stat-dirty entries would look clean to ngit afterwards.
set -u
root=$(cd "$(dirname "$0")/.." && pwd)
ngit="$root/bin/ngit"
fx=${NGIT_FIXTURE_DIR:-${TMPDIR:-/tmp}/ngit-fixtures}
[ -d "$fx/wt" ] || sh "$root/test/make-fixture.sh" "$fx" >/dev/null 2>&1 || {
  echo "fixture build failed"; exit 2; }
[ -d "$fx/st-staged-only" ] || sh "$root/test/make-wt-fixture.sh" "$fx" >/dev/null 2>&1 || {
  echo "work tree fixture build failed"; exit 2; }
export GIT_OPTIONAL_LOCKS=0
fail=0
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT

check() { # check REPO ARGS...
  repo=$1; shift
  git -C "$repo" "$@" >"$out/git" 2>/dev/null; gs=$?
  "$ngit" -C "$repo" "$@" >"$out/ngit" 2>"$out/err"; ns=$?
  if [ $gs -ne $ns ] || ! cmp -s "$out/git" "$out/ngit"; then
    echo "MISMATCH ($(basename "$repo")): git $* [git=$gs ngit=$ns]"
    diff "$out/git" "$out/ngit" | head -8
    head -3 "$out/err"
    fail=1
  fi
}

objects() { # all object names in REPO
  git -C "$1" cat-file --batch-all-objects --batch-check='%(objectname)'
}

batch_cat() { # cat every object of REPO with -t and -p in one ngit process
  repo=$1
  objects "$repo" | while read -r o; do
    git -C "$repo" cat-file -t "$o"; git -C "$repo" cat-file -p "$o"; done >"$out/git"
  objects "$repo" >"$out/list"
  NGIT_BATCH_LIST="$out/list" "$ngit" -C "$repo" test-cat-all >"$out/ngit" 2>"$out/err"
  if ! cmp -s "$out/git" "$out/ngit"; then
    echo "MISMATCH ($(basename "$repo")): cat-file -t/-p over all objects"
    diff "$out/git" "$out/ngit" | head -8; head -3 "$out/err"; fail=1
  fi
}

case $1 in
  basic)
    check "$fx/loose" cat-file -p HEAD
    check "$fx/loose" rev-parse HEAD ;;
  rev)
    for r in "$fx/loose" "$fx/packed"; do
      for rev in HEAD main side v1.0 light HEAD~1 HEAD~2 HEAD^2 HEAD^^2 \
                 HEAD~1^1 v1.0^{} v1.0^{commit} v1.0^{tree} HEAD^{tree} \
                 "HEAD:docs/long.txt" "HEAD:src" @ refs/heads/main \
                 HEAD~2^2 "HEAD~2^2^{tree}" HEAD~2^1~1; do
        check "$r" rev-parse "$rev"
      done
      check "$r" rev-parse --abbrev-ref HEAD
      check "$r" rev-parse --short HEAD
      check "$r" rev-parse --short=5 HEAD
      check "$r" rev-parse "$(git -C "$r" rev-parse HEAD | cut -c1-9)"
      check "$r" symbolic-ref HEAD
      check "$r" symbolic-ref --short HEAD
      check "$r" rev-parse nosuchref
    done ;;
  cat)
    for r in "$fx/loose" "$fx/packed"; do
      for o in HEAD HEAD^{tree} v1.0 HEAD:a.txt HEAD:link HEAD:src; do
        for m in -t -s -p; do check "$r" cat-file $m "$o"; done
      done
      check "$r" cat-file commit v1.0
      check "$r" cat-file -e HEAD
      check "$r" cat-file -e 0000000000000000000000000000000000000001
    done ;;
  all-objects) batch_cat "$fx/loose"; batch_cat "$fx/packed" ;;
  deltas)
    # The packed fixture must really contain delta entries, then read them.
    git -C "$fx/packed" verify-pack -v "$fx"/packed/.git/objects/pack/*.idx \
      | grep -q 'chain length = [1-9]' || { echo "no deltas in fixture"; exit 1; }
    batch_cat "$fx/packed" ;;
  worktree)
    check "$fx/wt" rev-parse HEAD
    check "$fx/wt" rev-parse --abbrev-ref HEAD
    check "$fx/wt" rev-parse main
    check "$fx/wt" rev-parse --show-toplevel
    check "$fx/wt" log
    check "$fx/wt" cat-file -p HEAD ;;
  ls-tree)
    for r in "$fx/loose" "$fx/packed"; do
      check "$r" ls-tree HEAD
      check "$r" ls-tree -r HEAD
      check "$r" ls-tree -r -t HEAD
      check "$r" ls-tree --name-only -r HEAD
      check "$r" ls-tree HEAD~3
    done ;;
  log)
    for r in "$fx/loose" "$fx/packed"; do
      check "$r" log
      check "$r" log -n 3
      check "$r" log -2
      check "$r" log --format=raw
      check "$r" log side
      check "$r" log main side
    done ;;
  real)
    r=${NGIT_REAL_REPO:?set NGIT_REAL_REPO}
    check "$r" rev-parse HEAD
    check "$r" log -n 30
    check "$r" log --format=raw -n 10
    check "$r" ls-tree HEAD
    check "$r" cat-file -p HEAD ;;
  index)
    for r in "$fx/loose" "$fx/packed" "$fx/dirty" "$fx/v4" "$fx/wt"; do
      check "$r" ls-files
      check "$r" ls-files -s
    done ;;
  attr-config)
    r=$fx/dirty
    for k in core.autocrlf core.filemode remote.origin.url branch.main.merge \
             submodule.sub1.url user.name nosuch.key; do
      check "$r" config --get "$k"
    done
    check "$r" check-attr text eol diff merge binary -- bin.dat crlf.txt eol.crlf \
      a.txt sub/x.txt sub/deep/y.txt "日本語ディレクトリ/ファイル.txt" ;;
  diff)
    for r in "$fx/dirty" "$fx/v4"; do
      check "$r" diff
      check "$r" diff --cached
      check "$r" diff HEAD
      check "$r" diff --name-only
      check "$r" diff --name-status HEAD
      check "$r" diff --cached --name-status
      check "$r" diff -U1
      check "$r" diff -U0 HEAD
      check "$r" diff --no-renames --cached
    done
    for r in "$fx/loose" "$fx/packed"; do
      check "$r" diff HEAD~2 HEAD
      check "$r" diff main side
      check "$r" diff v1.0 HEAD
      check "$r" diff HEAD~5 HEAD
      check "$r" diff --name-status HEAD~5 HEAD
      check "$r" diff -U5 HEAD~4..HEAD
    done
    check "$fx/dirty" diff HEAD~2 HEAD
    check "$fx/dirty" diff origin/main HEAD ;;
  status)
    for r in "$fx/dirty" "$fx/v4" "$fx/loose" "$fx/wt" "$fx/st-empty" "$fx/st-ahead"; do
      check "$r" status --porcelain
      check "$r" status -s
    done ;;
  status-long)
    for r in "$fx"/st-* "$fx/dirty" "$fx/v4" "$fx/loose" "$fx/wt"; do
      check "$r" status
    done ;;
  real-diff)
    r=${NGIT_REAL_REPO:?set NGIT_REAL_REPO}
    check "$r" diff HEAD~3 HEAD
    check "$r" diff --cached
    check "$r" diff ;;
  *) echo "unknown suite $1"; exit 2 ;;
esac
exit $fail
