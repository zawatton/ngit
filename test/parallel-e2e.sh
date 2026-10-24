#!/bin/sh
# Cross-lane Git oracle: nested paths, rename+edit merge, lease+atomic push,
# bounded file transport, index/worktree/ref equivalence, and full fsck.
set -eu
ulimit -v 8388608
root=$(cd "$(dirname "$0")/.." && pwd)
sh "$root/tools/build-elc.sh"
mkdir -p "${NGIT_FIXTURE_DIR:-${TMPDIR:-/tmp}}"
fx=$(mktemp -d "${NGIT_FIXTURE_DIR:-${TMPDIR:-/tmp}}/ngit-parallel-e2e.XXXXXXXX")
trap 'rm -rf "$fx"' EXIT HUP INT TERM
export LC_ALL=C TZ=UTC GIT_CONFIG_NOSYSTEM=1 GIT_OPTIONAL_LOCKS=0
export GIT_AUTHOR_NAME='Parallel Test' GIT_AUTHOR_EMAIL=parallel@example.com
export GIT_COMMITTER_NAME='Parallel Test' GIT_COMMITTER_EMAIL=parallel@example.com
export GIT_AUTHOR_DATE='1700600000 +0000' GIT_COMMITTER_DATE='1700600000 +0000'
export NGIT_MEMORY_BUDGET_MIB=${NGIT_MEMORY_BUDGET_MIB:-8192}
mkdir "$fx/empty"
export GIT_TEMPLATE_DIR="$fx/empty"
git init -q --template= -b main "$fx/seed"
mkdir -p "$fx/seed/nested/deep"
printf 'nested initial\n' >"$fx/seed/nested/deep/note.txt"
printf 'line one\nline two\nline three\nline four\nline five\n' >"$fx/seed/old.txt"
git -C "$fx/seed" add .
git -C "$fx/seed" commit -q -m seed
base=$(git -C "$fx/seed" rev-parse HEAD)
run() { timeout 90 "$tool" "$@" || exit 1; }
for kind in git ngit; do
  tool=git
  [ "$kind" = git ] || tool="$root/bin/ngit"
  cp -a "$fx/seed" "$fx/$kind-work"
  git clone -q --bare --template= "$fx/seed" "$fx/$kind-remote.git"
  work="$fx/$kind-work"
  remote="file://$fx/$kind-remote.git"
  (
    printf 'nested changed\n' >>"$work/nested/deep/note.txt" || exit 1
    run -C "$work" -C nested/deep add note.txt
    run -C "$work/nested/deep" commit -q -m 'nested commit'
    run -C "$work" switch -q -c topic
    run -C "$work" mv old.txt renamed.txt
    run -C "$work" commit -q -m rename
    run -C "$work" switch -q main
    printf 'line one\nline two EDITED\nline three\nline four\nline five\n' >"$work/old.txt" || exit 1
    run -C "$work" add old.txt
    run -C "$work" commit -q -m 'opposite edit'
    run -C "$work" merge --no-edit topic
    [ ! -e "$work/old.txt" ] && test -f "$work/renamed.txt" && grep -q 'line two EDITED' "$work/renamed.txt" || exit 1
    run -C "$work" push --atomic "--force-with-lease=refs/heads/main:$base" "$remote" main:refs/heads/main topic:refs/heads/topic
    timeout 180 "$tool" clone -q --template= "$remote" "$fx/$kind-clone" || exit 1
    merged=$(git -C "$work" rev-parse HEAD) || exit 1
    printf 'after clone\n' >>"$work/nested/deep/note.txt" || exit 1
    run -C "$work/nested/deep" add note.txt
    run -C "$work/nested/deep" commit -q -m 'after clone'
    run -C "$work" push --atomic "--force-with-lease=refs/heads/main:$merged" "$remote" main:refs/heads/main
    timeout 180 "$tool" -C "$fx/$kind-clone" fetch origin || exit 1
    run -C "$fx/$kind-clone" reset --hard origin/main
  ) >"$fx/$kind.log" 2>&1 || { tail -30 "$fx/$kind.log"; exit 1; }
done
snapshot() {
  directory=$1
  git -C "$directory" for-each-ref --format='%(refname) %(objectname) %(symref)' | sort
  git -C "$directory" rev-parse HEAD
  git -C "$directory" ls-files --stage
  (cd "$directory" && find . -path ./.git -prune -o -type f -print | sort | while IFS= read -r file; do printf '%s ' "$file"; git hash-object "$file"; done)
  git -C "$directory" fsck --full --no-progress --no-dangling
}
for part in work clone; do
  snapshot "$fx/git-$part" >"$fx/git-$part.state"
  snapshot "$fx/ngit-$part" >"$fx/ngit-$part.state"
  cmp "$fx/git-$part.state" "$fx/ngit-$part.state" || exit 1
done
for kind in git ngit; do
  git --git-dir="$fx/$kind-remote.git" for-each-ref --format='%(refname) %(objectname)' | sort >"$fx/$kind-remote.refs"
  git --git-dir="$fx/$kind-remote.git" fsck --full --no-progress --no-dangling
done
cmp "$fx/git-remote.refs" "$fx/ngit-remote.refs"
echo "parallel-e2e: PASS (nested add/commit, rename+edit merge, matching lease+atomic push, clone/fetch, refs/index/worktree, full fsck; budget ${NGIT_MEMORY_BUDGET_MIB} MiB)"
