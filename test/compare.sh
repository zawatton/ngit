#!/bin/sh
# Compare ngit against git for one suite of commands; exit 0 iff all match.
# Usage: test/compare.sh SUITE   (SUITE: basic, rev, cat, all-objects,
#        deltas, worktree, ls-tree, log, index, attr-config, diff, status,
#        status-long, real, real-diff, write-objects, index-write,
#        write-tree, add, add-all, commit, commit-output)
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

# Write commands run on a copy of a fixture: git and ngit each start from
# the same snapshot of .git (the work tree is shared and never written),
# and the resulting index bytes, refs, reflogs, loose objects, fsck
# report, stdout and exit status must be identical.
export GIT_AUTHOR_NAME="Test Author" GIT_AUTHOR_EMAIL=test@example.com
export GIT_COMMITTER_NAME="Test Author" GIT_COMMITTER_EMAIL=test@example.com
export GIT_AUTHOR_DATE="1700600000 +0900" GIT_COMMITTER_DATE="1700600000 +0900"
WENV=

wstate() { # wstate DIR TAG
  d=$1 t=$2
  if [ -f "$d/.git/index" ]; then cp "$d/.git/index" "$out/$t.index"; else : >"$out/$t.index"; fi
  {
    git -C "$d" rev-parse -q --verify HEAD; git -C "$d" symbolic-ref -q HEAD
    for f in "$d/.git/logs/HEAD" "$d/.git/COMMIT_EDITMSG"; do
      [ -f "$f" ] && { echo "== $f"; cat "$f"; }
    done
    find "$d/.git/logs/refs" -type f 2>/dev/null | sort | while read -r f; do echo "== $f"; cat "$f"; done
    (cd "$d/.git" && find objects -type f ! -path '*/pack/*' ! -path '*/info/*' | sort | xargs -r md5sum)
    find "$d/.git" -maxdepth 3 -name '*.lock'
    git -C "$d" fsck --no-progress 2>&1
  } >"$out/$t.state"
}

wrun() { # wrun SRC SETUP ARGS...
  src=$1 setup=$2; shift 2
  w="$out/w"; rm -rf "$w"; cp -a "$src" "$w"
  (cd "$w" && eval "$setup") >/dev/null 2>&1 || { echo "SETUP FAILED: $setup"; fail=1; return; }
  rm -rf "$out/snap"; cp -a "$w/.git" "$out/snap"
  # shellcheck disable=SC2086
  (cd "$w" && env $WENV git -c gc.auto=0 "$@") >"$out/git.out" 2>/dev/null
  echo "exit $?" >>"$out/git.out"
  wstate "$w" git
  rm -rf "$w/.git"; cp -a "$out/snap" "$w/.git"
  # shellcheck disable=SC2086
  (cd "$w" && env $WENV "$ngit" "$@") >"$out/ngit.out" 2>"$out/err"
  echo "exit $?" >>"$out/ngit.out"
  wstate "$w" ngit
  if ! cmp -s "$out/git.out" "$out/ngit.out" || ! cmp -s "$out/git.state" "$out/ngit.state" ||
     ! cmp -s "$out/git.index" "$out/ngit.index"; then
    echo "MISMATCH ($(basename "$src")${setup:+; $setup}): $*"
    diff "$out/git.out" "$out/ngit.out" | head -10
    diff "$out/git.state" "$out/ngit.state" | head -10
    if ! cmp -s "$out/git.index" "$out/ngit.index"; then
      echo "index differs:"
      cp "$out/git.index" "$w/.git/index"; git -C "$w" ls-files -s --debug >"$out/git.lsd" 2>&1
      cp "$out/ngit.index" "$w/.git/index"; git -C "$w" ls-files -s --debug >"$out/ngit.lsd" 2>&1
      diff "$out/git.lsd" "$out/ngit.lsd" | head -16
    fi
    head -3 "$out/err"; fail=1
  fi
}

R='git update-index -q --refresh'
NOREPO='rm -rf nested'

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
  write-objects)
    wrun "$fx/dirty" "" hash-object a.txt code.c bin.dat crlf.txt "space name.txt" link
    wrun "$fx/dirty" "echo fresh object > f.txt" hash-object -w f.txt
    wrun "$fx/dirty" "head -c 300000 /dev/urandom > big.bin; seq 1 50000 > big.txt" hash-object -w big.bin big.txt
    wrun "$fx/dirty" "git config core.autocrlf input" hash-object -w crlf.txt eol.crlf
    wrun "$fx/dirty" "git config core.looseCompression 9; seq 1 2000 > s.txt" hash-object -w s.txt
    wrun "$fx/dirty" "git config core.autocrlf input" hash-object -w --no-filters crlf.txt ;;
  index-write)
    for r in "$fx/dirty" "$fx/v4" "$fx/loose" "$fx/st-empty" "$fx/st-staged-only"; do
      wrun "$r" "" update-index --force-write-index
      wrun "$r" "$R" update-index --force-write-index
    done
    # Racily clean: same size and second as the index, different content.
    wrun "$fx/st-uptodate" "printf 'aaaa\n' > a.txt && git add a.txt && printf 'bbbb\n' > a.txt" \
      update-index --force-write-index
    # Racy entries whose mode changed are stat-dirty, not smudged.
    wrun "$fx/st-uptodate" "printf 'aaaa\\n' > a.txt && git add a.txt && chmod +x a.txt" \
      update-index --force-write-index ;;
  write-tree)
    for r in "$fx/dirty" "$fx/v4" "$fx/loose" "$fx/st-empty" "$fx/st-staged-only" "$fx/st-uptodate"; do
      wrun "$r" "" write-tree
    done
    wrun "$fx/st-uptodate" "mkdir -p n/m && echo x > n/m/x && git add n && git rm -q --cached b.txt" write-tree
    wrun "$fx/st-uptodate" "echo y > y && git add -N y && git rm -q -r --cached docs" write-tree
    wrun "$fx/dirty" "git rm -q -r --cached sub1 docs" write-tree ;;
  add)
    for s in "$NOREPO" "$NOREPO; $R"; do
      wrun "$fx/dirty" "$s" add a.txt
      wrun "$fx/dirty" "$s" add -v new-then-mod.txt b.txt
      wrun "$fx/dirty" "$s" add -v docs
      wrun "$fx/dirty" "$s" add -v link src/run.sh src
      wrun "$fx/dirty" "$s" add -v crlf.txt eol.crlf bin.dat code.c
      wrun "$fx/dirty" "$s" add -v ita.txt
      wrun "$fx/dirty" "$s" add -v sub1 sub2
      wrun "$fx/dirty" "$s" add -n -v .
      wrun "$fx/dirty" "$s" add -v "日本語ディレクトリ" "space name.txt" newdir
      wrun "$fx/dirty" "$s" add -N untracked.txt "untracked space.txt"
      wrun "$fx/dirty" "$s" add -v docs/x keep.log
    done
    wrun "$fx/dirty" "$NOREPO; git config core.autocrlf input" add -v crlf.txt
    wrun "$fx/dirty" "$NOREPO; git config core.filemode false" add -v docs
    wrun "$fx/v4" "$R" add -v a.txt extra.txt docs
    wrun "$fx/st-uptodate" "printf 'aaaa\n' > a.txt && git add a.txt && printf 'bbbb\n' > a.txt" add -v b.txt
    wrun "$fx/st-uptodate" "printf 'aaaa\n' > a.txt && git add a.txt && printf 'bbbb\n' > a.txt" add -v a.txt
    wrun "$fx/st-uptodate" "rm -r src && echo f > src" add -v src ;;
  add-all)
    for s in "$NOREPO" "$NOREPO; $R"; do
      wrun "$fx/dirty" "$s" add -A
      wrun "$fx/dirty" "$s" add -v -A
      wrun "$fx/dirty" "$s" add -u
      wrun "$fx/dirty" "$s" add -v -u
      wrun "$fx/dirty" "$s" add -v .
      wrun "$fx/dirty" "$s" add -v -u docs src
    done
    wrun "$fx/dirty" "git -C nested add x.txt && git -C nested commit -q -m n" add -v -A
    wrun "$fx/dirty" "" add -A
    wrun "$fx/dirty" "$NOREPO" add -v foo.log
    wrun "$fx/dirty" "$NOREPO" add -v build/out.txt
    wrun "$fx/dirty" "$NOREPO" add -v build
    wrun "$fx/dirty" "$NOREPO" add -v foo.log a.txt
    wrun "$fx/dirty" "$NOREPO" add -f -v foo.log build
    wrun "$fx/dirty" "$NOREPO" add nope a.txt
    wrun "$fx/dirty" "$NOREPO" add
    wrun "$fx/v4" "" add -A
    wrun "$fx/st-empty" "" add -A
    wrun "$fx/st-empty-clean" "echo x > x && mkdir d && echo y > d/y" add -v . ;;
  commit)
    wrun "$fx/st-staged-only" "" commit -m "staged only"
    wrun "$fx/st-staged-only" "" commit -q -m quiet
    wrun "$fx/st-empty" "" commit -m "first commit"
    wrun "$fx/st-empty-clean" "" commit --allow-empty -m "empty root"
    wrun "$fx/st-uptodate" "" commit --allow-empty -m "nothing changed"
    wrun "$fx/st-detached-at" "echo d > d.txt && git add d.txt" commit -m "on detached head"
    wrun "$fx/st-uptodate" "git checkout -q --orphan fresh" commit -m "orphan branch"
    wrun "$fx/st-staged-only" "" commit -m "  subject line  " -m "" -m "$(printf 'body\n\n\n\nmore  \n\n')"
    wrun "$fx/st-staged-only" "" commit -m "日本語の件名" -m "本文"
    wrun "$fx/st-staged-only" "printf '\n\nfrom file\n\n  x\n' > ../msg.txt" commit -F ../msg.txt
    wrun "$fx/st-staged-only" "" commit -m "$(printf 'two\nline subject\n\nbody')"
    wrun "$fx/dirty" "$NOREPO" commit -m "dirty index"
    wrun "$fx/v4" "" commit -m "v4 index"
    wrun "$fx/st-staged-only" "$R" commit -m refreshed
    wrun "$fx/st-staged-only" "touch -d 2002-02-02 b.txt" commit -m "stat refresh" ;;
  commit-output)
    wrun "$fx/dirty" "$NOREPO" commit -a -m "all changes"
    wrun "$fx/dirty" "$NOREPO; $R" commit -a -m "all changes"
    wrun "$fx/st-uptodate" "git mv src/run.sh src/bin-run.sh && mkdir docs/x && git mv docs/long.txt docs/x/long.txt && chmod +x b.txt && git add b.txt" commit -m renames
    wrun "$fx/st-uptodate" "git rm -q a.txt && git mv b.txt c.txt" commit -m "delete and rename"
    wrun "$fx/st-uptodate" "" commit -m nothing
    wrun "$fx/st-ahead" "" commit -m "untracked only"
    wrun "$fx/st-ahead" "" commit -a -m "untracked only"
    wrun "$fx/dirty" "git checkout -q -- a.txt" commit -a -m x
    wrun "$fx/st-staged-only" "" commit -m ""
    wrun "$fx/st-ahead" "" commit --amend --no-edit
    wrun "$fx/st-ahead" "" commit --amend -m "amended"
    wrun "$fx/st-staged-only" "" commit --amend -m "amend with staged"
    WENV="GIT_AUTHOR_NAME=Someone GIT_AUTHOR_EMAIL=someone@example.com"
    wrun "$fx/st-staged-only" "" commit -m "other author"
    wrun "$fx/st-ahead" "" commit --amend --no-edit
    WENV="GIT_COMMITTER_NAME="
    wrun "$fx/st-staged-only" "" commit -a -m "no identity"
    WENV=
    wrun "$fx/st-uptodate" "seq 1 100 > a.txt && seq 1 2 100 > b.txt && git add a.txt b.txt" commit -m stats ;;
  *) echo "unknown suite $1"; exit 2 ;;
esac
exit $fail
