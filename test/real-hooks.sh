#!/bin/sh
# Real-repository hook comparison.  The provided repository is only cloned.
# Usage: test/real-hooks.sh REPO LIMIT
set -u
# awk sums the timings; a locale decimal comma would break them.
export LC_NUMERIC=C
repo=$1 limit=$2
root=$(cd "$(dirname "$0")/.." && pwd)
ngit="$root/bin/ngit"
fx=${NGIT_FIXTURE_DIR:-${TMPDIR:-/tmp}/ngit-fixtures}
clone="$fx/real-hooks-clone"
mkdir -p "$fx"
out=$(mktemp -d "$fx/.real-hooks.XXXXXXXX")
trap 'rm -rf "$out"' EXIT HUP INT TERM

# Never borrow object files from the caller: receiver writes and maintenance
# in this disposable clone must remain independent of the source repository.
rm -rf "$clone"
git clone -q --no-hardlinks "$repo" "$clone" || {
  echo 'real hooks clone failed'; exit 2;
}

export GIT_CONFIG_NOSYSTEM=1 GIT_OPTIONAL_LOCKS=0
export GIT_AUTHOR_NAME='Test Author' GIT_AUTHOR_EMAIL=test@example.com
export GIT_COMMITTER_NAME='Test Author' GIT_COMMITTER_EMAIL=test@example.com
export GIT_AUTHOR_DATE='1700600000 +0900' GIT_COMMITTER_DATE='1700600000 +0900'
export NGIT_HOOK_LOG="$clone/.git/hooks.log"

install_hooks() { # install_hooks GITDIR PREFIX HOOK...
  hook_gitdir=$1 hook_prefix=$2
  shift 2
  mkdir -p "$hook_gitdir/hooks"
  printf '%s\n' "$hook_prefix" >"$hook_gitdir/hooks/.ngit-hook-prefix"
  for hook_name in "$@"; do
    cp "$root/test/hook-recorder.sh" "$hook_gitdir/hooks/$hook_name"
    chmod +x "$hook_gitdir/hooks/$hook_name"
  done
}

cd "$clone" || exit 2
git config gc.auto 0
git config maintenance.auto false
git config user.name 'Test Author'
git config user.email test@example.com
base=$(git rev-parse HEAD) || exit 2
git switch -q --detach "$base" || exit 2
git branch -D ngit-real-hooks-main ngit-real-hooks-side >/dev/null 2>&1 || :
git switch -qc ngit-real-hooks-main "$base" || exit 2
git switch -qc ngit-real-hooks-side "$base" || exit 2
printf 'side\n' >ngit-real-hooks-side.txt
git add ngit-real-hooks-side.txt
git commit -qm 'real hooks side' || exit 2
git switch -q ngit-real-hooks-main || exit 2

rm -rf .git/hooks-remote.git
git clone -q --bare --local . .git/hooks-remote.git || exit 2
git remote remove ngit-real-hooks >/dev/null 2>&1 || :
git remote add ngit-real-hooks .git/hooks-remote.git
install_hooks .git local pre-commit prepare-commit-msg commit-msg post-commit \
  post-checkout pre-merge-commit post-merge pre-push
install_hooks .git/hooks-remote.git remote pre-receive update post-receive post-update

# Save only mutable administrative data.  Objects are immutable and may be
# reused by the second run; the receiver snapshot hard-links its object store.
snap="$out/snap"
mkdir -p "$snap"
for path in HEAD refs logs config packed-refs hooks; do
  [ ! -e ".git/$path" ] || cp -a ".git/$path" "$snap/"
done
cp -al .git/hooks-remote.git "$snap/hooks-remote.git" || exit 2

restore() {
  for path in HEAD refs logs config packed-refs hooks; do
    rm -rf ".git/$path"
    [ ! -e "$snap/$path" ] || cp -a "$snap/$path" .git/
  done
  rm -rf .git/hooks-remote.git
  cp -al "$snap/hooks-remote.git" .git/hooks-remote.git || exit 2
  git reset -q --hard ngit-real-hooks-main || exit 2
  git clean -fdq || exit 2
  rm -f .git/hooks.log
  printf 'pending commit\n' >ngit-real-hooks-commit.txt
  git add ngit-real-hooks-commit.txt || exit 2
}

normalize() {
  sed "s|$clone|@CLONE@|g" "$1" >"$2"
}

state() { # state OUTPUT
  state_out=$1
  {
    echo '== HEAD'
    git rev-parse HEAD
    git symbolic-ref -q HEAD || :
    echo '== refs'
    git for-each-ref --format='%(refname) %(objectname) %(symref)' | LC_ALL=C sort
    echo '== reflogs'
    (cd .git && find logs -type f | LC_ALL=C sort | while IFS= read -r file; do
      echo "-- $file"; cat "$file"
    done)
    echo '== index'
    git ls-files -s
    echo '== status'
    git status --porcelain=v1
    echo '== hook log'
    cat .git/hooks.log 2>/dev/null || :
    echo '== receiver refs'
    git --git-dir=.git/hooks-remote.git for-each-ref \
      --format='%(refname) %(objectname) %(symref)' | LC_ALL=C sort
    echo '== receiver fsck'
    git --git-dir=.git/hooks-remote.git fsck --no-progress --connectivity-only 2>&1
  } >"$state_out.raw"
  normalize "$state_out.raw" "$state_out"
}

same() { # same LABEL LEFT RIGHT
  cmp -s "$2" "$3" || {
    echo "MISMATCH: $1"
    diff -u "$2" "$3" | head -30
    fail=1
  }
}

spent=0
fail=0
run_step() { # run_step TOOL LABEL ARGS...
  tool=$1 label=$2
  shift 2
  if [ "$tool" = git ]; then
    git -c gc.auto=0 "$@" >"$out/$tool.$label.out.raw" 2>"$out/$tool.$label.err.raw"
    status=$?
  else
    started=$(date +%s.%N)
    "$ngit" "$@" >"$out/$tool.$label.out.raw" 2>"$out/$tool.$label.err.raw"
    status=$?
    ended=$(date +%s.%N)
    elapsed=$(awk -v start="$started" -v end="$ended" 'BEGIN {printf "%.6f", end-start}')
    spent=$(awk -v total="$spent" -v step="$elapsed" 'BEGIN {printf "%.6f", total+step}')
    printf 'ngit %s: %ss\n' "$label" "$elapsed"
  fi
  normalize "$out/$tool.$label.out.raw" "$out/$tool.$label.out"
  normalize "$out/$tool.$label.err.raw" "$out/$tool.$label.err"
  printf '%s\n' "$status" >"$out/$tool.$label.status"
  state "$out/$tool.$label.state"
  if [ "$tool" = ngit ]; then
    for part in out err status state; do
      same "$label $part" "$out/git.$label.$part" "$out/ngit.$label.$part"
    done
  fi
  [ "$status" -eq 0 ] || fail=1
}

for tool in git ngit; do
  restore
  run_step "$tool" commit commit -m 'real hooks commit'
  run_step "$tool" switch switch ngit-real-hooks-side
  run_step "$tool" merge merge --no-ff -m 'real hooks merge' ngit-real-hooks-main
  run_step "$tool" push push ngit-real-hooks HEAD:refs/heads/ngit-real-hooks-result
  [ "$fail" -eq 0 ] || break
done

echo "ngit hooks total: ${spent}s (limit ${limit}s)"
[ "$fail" -eq 0 ] && awk -v spent="$spent" -v limit="$limit" 'BEGIN {exit !(spent <= limit)}'
