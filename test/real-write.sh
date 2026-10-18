#!/bin/sh
# Write commands on a clone of the real repository REPO, against git.
#   tree   - update-index --force-write-index, then write-tree after git
#            has staged the modifications
#   add    - add -A after modifying the clone
#   commit - commit -a -m after modifying the clone (fixed dates)
# git and ngit start from the same index (and refs, for commit); the
# resulting index bytes, stdout and HEAD must be identical, and ngit's
# runs must take at most LIMIT seconds in total.  The clone lives in
# $NGIT_FIXTURE_DIR/real-clone and is reset to the cloned commit first.
# Usage: test/real-write.sh REPO MODE LIMIT
set -u
root=$(cd "$(dirname "$0")/.." && pwd)
ngit="$root/bin/ngit"
repo=$1 mode=$2 limit=$3
fx=${NGIT_FIXTURE_DIR:-${TMPDIR:-/tmp}/ngit-fixtures}
clone="$fx/real-clone"
mkdir -p "$fx"
if [ ! -f "$clone/.git/ngit-base" ]; then
  rm -rf "$clone"
  # A plain local clone hardlinks the objects; -s would need alternates,
  # which ngit does not read.
  git clone -q "$repo" "$clone" || { echo "clone failed"; exit 2; }
  git -C "$clone" config gc.auto 0
  git -C "$clone" config maintenance.auto false
  git -C "$clone" rev-parse HEAD >"$clone/.git/ngit-base"
fi
cd "$clone" || exit 2
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
export GIT_OPTIONAL_LOCKS=0
export GIT_AUTHOR_NAME="Test Author" GIT_AUTHOR_EMAIL=test@example.com
export GIT_COMMITTER_NAME="Test Author" GIT_COMMITTER_EMAIL=test@example.com
export GIT_AUTHOR_DATE="1700600000 +0900" GIT_COMMITTER_DATE="1700600000 +0900"

git reset -q --hard "$(cat .git/ngit-base)" && git clean -fdq || exit 2

# Deterministic modifications: two appended files, a deleted one, an
# exec bit, a touched-only file and new files in old and new directories.
git ls-files -- '*.org' '*.md' | sed -n '10p;200p;400p;600p;800p' >"$out/picks"
f1=$(sed -n 1p "$out/picks") f2=$(sed -n 2p "$out/picks") f3=$(sed -n 3p "$out/picks")
f4=$(sed -n 4p "$out/picks") f5=$(sed -n 5p "$out/picks")
echo "ngit real-write line" >>"$f1"
echo "ngit real-write line" >>"$f2"
rm -f "$f3"
chmod +x "$f4"
touch "$f5"
echo new >"$(dirname "$f1")/ngit-real-write-new.txt"
mkdir -p ngit-real-write-dir/sub && echo new >ngit-real-write-dir/sub/n.txt

snap="$out/snap"
save() { rm -rf "$snap"; mkdir -p "$snap"
  for p in index HEAD refs logs packed-refs COMMIT_EDITMSG; do
    [ -e ".git/$p" ] && cp -a ".git/$p" "$snap/"; done; }
restore() { for p in index HEAD refs logs packed-refs COMMIT_EDITMSG; do
    rm -rf ".git/$p"; [ -e "$snap/$p" ] && cp -a "$snap/$p" ".git/$p"; done; }
spent=0
timed() { # timed OUTFILE ARGS...: run ngit, add the elapsed time to $spent
  o=$1; shift
  s=$(date +%s.%N)
  "$ngit" "$@" >"$o" 2>"$out/err"; st=$?
  e=$(date +%s.%N)
  spent=$(echo "$spent + $e - $s" | bc)
  echo "exit $st" >>"$o"
  echo "ngit $*: $(echo "$e - $s" | bc)s"
}
fail=0
same() { # same WHAT FILE1 FILE2
  cmp -s "$2" "$3" || { echo "MISMATCH: $1"; diff "$2" "$3" | head -6; head -3 "$out/err"; fail=1; }
}
run_both() { # run_both TAG ARGS...: git then ngit from the saved state
  t=$1; shift
  restore; git -c gc.auto=0 "$@" >"$out/$t.git" 2>/dev/null; echo "exit $?" >>"$out/$t.git"
  cp .git/index "$out/$t.git.index"; git rev-parse HEAD >"$out/$t.git.head"
  restore; timed "$out/$t.ngit" "$@"
  cp .git/index "$out/$t.ngit.index"; git rev-parse HEAD >"$out/$t.ngit.head"
  same "$t stdout" "$out/$t.git" "$out/$t.ngit"
  same "$t index" "$out/$t.git.index" "$out/$t.ngit.index"
  same "$t HEAD" "$out/$t.git.head" "$out/$t.ngit.head"
}

case $mode in
  tree)
    save; run_both rewrite update-index --force-write-index
    git add -A; save; run_both write-tree write-tree ;;
  add)
    save; run_both add add -A ;;
  commit)
    save; run_both commit commit -a -m "ngit real-write commit" ;;
  *) echo "unknown mode $mode"; exit 2 ;;
esac
echo "ngit total: ${spent}s (limit ${limit}s)"
[ $fail = 0 ] && [ "$(echo "$spent <= $limit" | bc)" = 1 ]
