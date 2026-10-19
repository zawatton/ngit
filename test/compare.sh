#!/bin/sh
# Compare ngit against git for one suite of commands; exit 0 iff all match.
# Usage: test/compare.sh SUITE   (SUITE: basic, rev, cat, all-objects,
#        deltas, worktree, ls-tree, log, index, attr-config, diff, status,
#        status-long, real, real-diff, write-objects, index-write,
#        write-tree, add, add-all, commit, commit-output, log-format,
#        log-walk, log-diff, log-path, log-graph, branch-list, branch-filter,
#        branch-create, branch-modify, branch-upstream)
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
[ -d "$fx/work" ] || sh "$root/test/make-log-fixture.sh" "$fx" >/dev/null 2>&1 || {
  echo "log fixture build failed"; exit 2; }
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
WENV= WREFS= WSTDERR=

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
    if [ -n "$WREFS" ]; then
      for f in "$d/.git/config" "$d/.git/packed-refs"; do
        [ -f "$f" ] && { echo "== $f"; cat "$f"; }
      done
      (cd "$d/.git" && find refs -type f | sort)
      git -C "$d" for-each-ref
    fi
  } >"$out/$t.state"
}

wrun() { # wrun SRC SETUP ARGS...
  src=$1 setup=$2; shift 2
  w="$out/w"; rm -rf "$w"; cp -a "$src" "$w"
  (cd "$w" && eval "$setup") >/dev/null 2>&1 || { echo "SETUP FAILED: $setup"; fail=1; return; }
  rm -rf "$out/snap"; cp -a "$w/.git" "$out/snap"
  # shellcheck disable=SC2086
  if [ -n "$WSTDERR" ]; then
    # shellcheck disable=SC2086
    (cd "$w" && env $WENV git -c gc.auto=0 "$@") >"$out/git.out" 2>&1
  else
    # shellcheck disable=SC2086
    (cd "$w" && env $WENV git -c gc.auto=0 "$@") >"$out/git.out" 2>/dev/null
  fi
  echo "exit $?" >>"$out/git.out"
  wstate "$w" git
  rm -rf "$w/.git"; cp -a "$out/snap" "$w/.git"
  # shellcheck disable=SC2086
  if [ -n "$WSTDERR" ]; then
    # shellcheck disable=SC2086
    (cd "$w" && env $WENV "$ngit" "$@") >"$out/ngit.out" 2>&1
  else
    # shellcheck disable=SC2086
    (cd "$w" && env $WENV "$ngit" "$@") >"$out/ngit.out" 2>"$out/err"
  fi
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
  log-format)
    h="$fx/hist"
    for f in medium short full fuller raw oneline reference med ful; do
      check "$h" log --pretty=$f
    done
    check "$h" log --oneline --all
    check "$h" log --abbrev-commit -3
    check "$h" log --abbrev=10 --oneline -5
    check "$h" log --no-abbrev --pretty=medium -12
    check "$h" log --format=%H%n%h%n%T%n%t%n%P%n%p%n%an%n%ae%n%al%n%ad%n%at%n%ai%n%aI%n%as%n%aD -6
    check "$h" log --format=%cn%x09%ce%x09%cl%x09%cd%x09%ct%x09%ci%x09%cI%x09%cs%x09%cD -6
    check "$h" log "--format=[%s]%n[%f]%n[%b]%n[%B]" --all
    check "$h" log --format=format:%h%x00%s -5
    check "$h" log "--format=tformat:%h%+b%-b% s|" --all
    check "$h" log "--format=%h %d%n%D" --all
    check "$h" log --format= -3
    check "$h" log --pretty=format: -3
    check "$h" log "--format=%m %h" main...c2
    for d in default iso iso8601 iso-strict rfc short raw unix "format:%Y/%m/%d %H:%M:%S %z %a %b %e %j %A %B %I" default-local iso-local short-local; do
      check "$h" log --date="$d" -4
    done
    check "$h" log --decorate --oneline --all
    check "$h" log --decorate=full --all
    check "$h" log --decorate=short -3
    check "$h" log --decorate --no-decorate -3
    check "$h" log --decorate --format=raw -2
    check "$h" log --pretty=raw --decorate -2
    check "$h" log --parents --oneline --all
    check "$h" log --parents -3
    check "$h" log --pretty=oneline -4
    check "$h" log --pretty=fuller --date=iso -4
    check "$h" log --pretty=bogus
    check "$h" log --date=bogus ;;
  log-walk)
    h="$fx/hist" w="$fx/work"
    for r in main topic c1 c2 v1.0 v2.0 light old mid docs tie; do
      check "$h" log --oneline $r
    done
    check "$h" log --oneline main..topic
    check "$h" log --oneline topic..main
    check "$h" log --oneline main...topic
    check "$h" log --oneline c1...c2
    check "$h" log --oneline c1..c2 c2..c1
    check "$h" log --oneline ^v1.0 main
    check "$h" log --oneline main ^c1 ^c2
    check "$h" log --oneline main --not c1 c2
    check "$h" log --oneline ..topic
    check "$h" log --oneline topic..
    check "$h" log --oneline 'c1^@'
    check "$h" log --oneline 'c2^!'
    check "$h" log --oneline 'main^2'
    check "$h" log --oneline --all
    check "$h" log --oneline --branches
    check "$h" log --oneline --branches='c*'
    check "$h" log --oneline --tags
    check "$h" log --oneline --tags=v1
    check "$w" log --oneline --remotes
    check "$w" log --oneline --remotes='*/c*'
    check "$w" log --oneline --all
    check "$h" log --oneline --first-parent
    check "$h" log --oneline --first-parent main~3
    check "$h" log --oneline --merges
    check "$h" log --oneline --no-merges
    check "$h" log --oneline --min-parents=3
    check "$h" log --oneline --max-parents=0
    check "$h" log --oneline --reverse
    check "$h" log --oneline --reverse -n 5
    check "$h" log --oneline --skip=4 -n 6
    check "$h" log --oneline -n 0
    check "$h" log --oneline --max-count=3 --skip=100
    check "$h" log --oneline --since=1710030000
    check "$h" log --oneline --until=1710040000
    check "$h" log --oneline --after="2024-03-10 12:00:00 +0900" --before="2024-03-10 20:00:00 +0900"
    check "$h" log --oneline --author=Other
    check "$h" log --oneline --author='^Log' --all
    check "$h" log --oneline --committer=Committer -3
    check "$h" log --oneline --grep=c1
    check "$h" log --oneline -i --grep=MERGE
    check "$h" log --oneline --grep=c1 --grep=merge
    check "$h" log --oneline --grep=c1 --grep=merge --all-match
    check "$h" log --oneline --grep=c1 --invert-grep
    check "$h" log --oneline -E --grep='c(1|2): (start|more)'
    check "$h" log --oneline -F --grep='(criss-cross)'
    check "$h" log --oneline --grep='t.p'
    check "$h" log --oneline --topo-order --all
    check "$h" log --oneline --date-order --all
    check "$h" log --oneline --topo-order main...topic
    check "$h" log --oneline --all --since=1710040000
    check "$h" log --oneline --all ^light
    check "$h" log --oneline nope
    check "$h" log --oneline main..nope
    check "$fx/st-empty" log
    check "$fx/loose" log --oneline --all
    check "$fx/packed" log --oneline --all --topo-order ;;
  log-diff)
    h="$fx/hist" r="$fx/ren"
    for f in -p --stat --shortstat --numstat --name-only --name-status --summary; do
      check "$h" log $f
      check "$r" log $f
    done
    check "$h" log --oneline --stat
    check "$h" log --stat -p -5
    check "$h" log --stat --summary --all
    check "$h" log --format=%h --name-status --all
    check "$h" log --format=format:%s --shortstat
    check "$h" log -U1 -3
    check "$h" log -p --first-parent
    check "$h" log --stat --no-renames
    check "$h" log -s --stat -3
    check "$r" log -p -M
    check "$r" log --no-renames --name-status
    check "$fx/ren-limit" log --name-status
    check "$fx/ren-limit" log --stat
    check "$fx/loose" log -p --all
    check "$fx/packed" log --stat --all
    COLUMNS=40 check "$h" log --stat -4
    COLUMNS=200 check "$r" log --stat ;;
  log-path)
    h="$fx/hist"
    for p in src/main.c doc doc/ src lib README README.md data.bin x.txt c1.txt '*.txt' 'doc/*' nonexistent; do
      check "$h" log --oneline -- "$p"
      check "$h" log --oneline --full-history -- "$p"
    done
    check "$h" log --oneline -- c1.txt c2.txt
    check "$h" log --oneline --all -- tie.txt
    check "$h" log --oneline --sparse -- doc
    check "$h" log --oneline --first-parent -- doc
    check "$h" log --oneline --parents -- doc
    check "$h" log --oneline --topo-order -- src
    check "$h" log --oneline main -- README.md
    check "$h" log --oneline src/main.c
    check "$h" log --oneline --stat -- doc
    check "$h" log --oneline --full-diff --stat -- x.txt
    check "$h" log -p -- data.bin
    check "$h" log --oneline v1.0..main -- doc
    check "$h" log --oneline main...c2 -- c1.txt c2.txt
    check "$fx/ren" log --oneline --stat -- b
    check "$fx/ren" log --oneline --name-status -- b/numbers2.txt ;;
  log-graph)
    h="$fx/hist" w="$fx/work"
    check "$h" log --graph
    check "$h" log --graph --oneline
    check "$h" log --graph --oneline --all
    check "$w" log --graph --oneline --all
    check "$h" log --graph --oneline --decorate --all
    check "$h" log --graph --date-order --oneline --all
    check "$h" log --graph --oneline main...c2
    check "$h" log --graph --oneline c1 c2 ^main~6
    check "$h" log --graph --oneline --first-parent
    check "$h" log --graph --oneline -- doc
    check "$h" log --graph --oneline --full-history -- doc
    check "$h" log --graph --oneline -- c1.txt c2.txt
    check "$h" log --graph --oneline --merges
    check "$h" log --graph --oneline --no-merges
    check "$h" log --graph --oneline --grep=c1
    check "$h" log --graph --oneline -n 7
    check "$h" log --graph --oneline --skip=3
    check "$h" log --graph --stat -6
    check "$h" log --graph -p -4
    check "$h" log --graph --name-status --all
    check "$h" log --graph --format=%h%n%s%n
    check "$h" log --graph --format=format:%h
    check "$h" log --graph --pretty=fuller -5
    check "$h" log --graph --parents --oneline
    check "$h" log --graph --reverse
    check "$fx/ren" log --graph --stat
    check "$fx/packed" log --graph --oneline --all ;;
  branch-list)
    wk="$fx/work" h="$fx/hist"
    for a in "" -a -r -v -vv "-a -v" "-a -vv" "-r -vv" "-a --no-abbrev -v" "-v --abbrev=10" \
             --show-current "--list n*" "--list *o*" "-a --list origin/c*" "-r --list *main" \
             "-l -i N*" "-a --sort=-refname" "--sort=committerdate" "--sort=-authordate -v" \
             "-a --sort=objectname"; do
      # shellcheck disable=SC2086
      check "$wk" branch $a
      # shellcheck disable=SC2086
      check "$fx/work-wt" branch $a
    done
    check "$h" branch -v
    check "$fx/st-empty" branch
    check "$fx/st-empty" branch --show-current
    WREFS=1
    wrun "$wk" "git checkout -q --detach main~1" branch -v
    wrun "$wk" "git checkout -q --detach v1.0" branch
    wrun "$wk" "git checkout -q --detach origin/c2 && git commit -q --allow-empty -m moved" branch -vv
    wrun "$wk" "git checkout -q --detach main" branch --show-current
    WREFS= ;;
  branch-filter)
    wk="$fx/work" h="$fx/hist"
    for a in --merged --no-merged "--merged main~2" "--no-merged notrack" "-a --merged" \
             "--contains main~4" "--contains c1" "-a --no-contains main" "-r --contains origin/c2-base" \
             "--points-at main" "-a --points-at origin/main" "--merged main --contains main~6" \
             "-v --merged" "--sort=-committerdate --merged" "--contains nope"; do
      # shellcheck disable=SC2086
      check "$wk" branch $a
    done
    for a in "--contains c1" "--merged c2" "--no-merged" "-v --contains v1.0"; do
      # shellcheck disable=SC2086
      check "$h" branch $a
    done ;;
  branch-create)
    wk="$fx/work" WREFS=1 WSTDERR=1
    wrun "$wk" "" branch new1
    wrun "$wk" "" branch new2 main~3
    wrun "$wk" "" branch new3 origin/topic
    wrun "$wk" "" branch new4 origin/c2-base
    wrun "$wk" "" branch --no-track new5 origin/topic
    wrun "$wk" "" branch -t new6 main
    wrun "$wk" "" branch --track new7 origin/docs
    wrun "$wk" "" branch -q new8 origin/topic
    wrun "$wk" "" branch deep/name/x
    wrun "$wk" "" branch tagged v1.0
    wrun "$wk" "" branch -f notrack main
    wrun "$wk" "" branch -f ahead origin/topic
    wrun "$wk" "" branch notrack
    wrun "$wk" "" branch -f main main~1
    wrun "$wk" "" branch -f wtb main
    wrun "$wk" "" branch 'bad..name'
    wrun "$wk" "" branch 'bad name'
    wrun "$wk" "" branch -- -dash
    wrun "$wk" "" branch nostart nope
    wrun "$wk" "" branch -t badtrack main~1
    wrun "$wk" "git config branch.autoSetupMerge false" branch auto1 origin/topic
    wrun "$wk" "git config branch.autoSetupMerge always" branch auto2 main
    wrun "$wk" "git checkout -q --detach main~2" branch fromdetached
    wrun "$fx/st-empty" "" branch first
    wrun "$wk" "printf '# comment\n' >> .git/config" branch -t withcomment origin/topic
    wrun "$wk" "git config --unset-all remote.origin.fetch" branch nofetch origin/topic
    WREFS= WSTDERR= ;;
  branch-modify)
    wk="$fx/work" WREFS=1 WSTDERR=1
    wrun "$wk" "" branch -d behind
    wrun "$wk" "" branch -d notrack
    wrun "$wk" "" branch -D notrack
    wrun "$wk" "" branch -d gone
    wrun "$wk" "" branch -d ahead
    wrun "$wk" "" branch -D ahead loose-only
    wrun "$wk" "" branch -d loose-only
    wrun "$wk" "" branch -d main
    wrun "$wk" "" branch -d wtb
    wrun "$wk" "" branch -d nope
    wrun "$wk" "" branch -d c2
    wrun "$wk" "" branch -d
    wrun "$wk" "" branch -q -D notrack
    wrun "$wk" "" branch -d -r origin/c2
    wrun "$wk" "" branch -r -d origin/nope
    wrun "$wk" "" branch -d local-up
    wrun "$wk" "git branch -q --track up2 notrack" branch -d up2
    wrun "$wk" "" branch -m ahead renamed
    wrun "$wk" "" branch -m notrack deep/renamed
    wrun "$wk" "" branch -m main trunk
    wrun "$wk" "" branch -m renamedcurrent
    wrun "$wk" "" branch -m ahead behind
    wrun "$wk" "" branch -M ahead behind
    wrun "$wk" "" branch -m nope x
    wrun "$wk" "" branch -m ahead 'bad..'
    wrun "$wk" "" branch -m wtb wtb2
    wrun "$wk" "" branch -m loose-only loose-moved
    wrun "$wk" "" branch -c ahead copied
    wrun "$wk" "" branch -c ahead behind
    wrun "$wk" "" branch -C ahead behind
    wrun "$wk" "" branch -c currentcopy
    wrun "$wk" "" branch -c local-up local-copy
    wrun "$wk" "" branch -M main main
    wrun "$wk" "git checkout -q --detach main" branch -m x
    wrun "$fx/st-empty" "" branch -m newmain
    wrun "$fx/hist" "" branch -d c1
    wrun "$fx/hist" "" branch -D old c1 c2
    WREFS= WSTDERR= ;;
  branch-upstream)
    wk="$fx/work" WREFS=1 WSTDERR=1
    wrun "$wk" "" branch -u origin/topic
    wrun "$wk" "" branch -u origin/docs notrack
    wrun "$wk" "" branch --set-upstream-to=origin/c2 ahead
    wrun "$wk" "" branch --set-upstream-to origin/c2 ahead
    wrun "$wk" "" branch -u notrack ahead
    wrun "$wk" "" branch -u ahead ahead
    wrun "$wk" "" branch -u origin/nope
    wrun "$wk" "" branch -u v1.0 ahead
    wrun "$wk" "" branch -u origin/topic nope
    wrun "$wk" "" branch --unset-upstream ahead
    wrun "$wk" "" branch --unset-upstream
    wrun "$wk" "" branch --unset-upstream notrack
    wrun "$wk" "" branch --unset-upstream local-up
    wrun "$wk" "git checkout -q --detach main" branch -u origin/main
    wrun "$wk" "git checkout -q --detach main" branch --unset-upstream
    wrun "$wk" "git config branch.ahead.rebase true" branch --unset-upstream ahead
    wrun "$wk" "git config --add branch.ahead.merge refs/heads/docs" branch --unset-upstream ahead
    WREFS= WSTDERR= ;;
  *) echo "unknown suite $1"; exit 2 ;;
esac
exit $fail
