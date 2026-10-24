#!/bin/sh
# Build deterministic repositories whose indexes contain every unmerged stage
# mask.  A real merge creates the ordinary conflicts; stage-only masks that a
# merge resolves automatically (AU, UA and DD) are added with --index-info.
# Usage: test/make-unmerged-fixture.sh DIR
set -eu
dir=$(cd "$1" && pwd)

export GIT_AUTHOR_NAME="Fixture Author" GIT_AUTHOR_EMAIL=author@example.com
export GIT_COMMITTER_NAME="Fixture Committer" GIT_COMMITTER_EMAIL=committer@example.com
export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL="$dir/unmerged-build/.git/empty-global-config"
export GIT_AUTHOR_DATE="1700700000 +0900" GIT_COMMITTER_DATE="1700700000 +0900"

# Only remove repositories owned by this builder.
rm -rf "$dir/unmerged-build" "$dir/unmerged-v2" "$dir/unmerged-v3" \
  "$dir/unmerged-v4" "$dir/unmerged-resolved" "$dir/unmerged-no-merge"
mkdir -p "$dir/unmerged-build/nested"
cd "$dir/unmerged-build"
git init -q -b main
git config gc.auto 0
git config merge.renames false

printf 'anchor\n' > anchor.txt
printf 'base one\ncommon\nbase three\n' > nested/uu.txt
printf 'base one\ncommon\nbase three\n' > 'nested/quo"ted name.txt'
printf 'base one\ncommon\nbase three\n' > nested/missing.txt
printf 'base one\ncommon\nbase three\n' > nested/equal-ours.txt
printf 'base one\ncommon\nbase three\n' > nested/equal-theirs.txt
printf 'base one\ncommon\nbase three\n' > nested/reuc.txt
printf 'base\000binary\001\n' > nested/binary.bin
printf 'base\n' > nested/delete-us.txt
printf 'base\n' > nested/delete-them.txt
printf 'base\n' > synthetic-deleted-both.txt
git add -A
git commit -qm base
git branch theirs

printf 'ours one\ncommon\nours three\n' > nested/uu.txt
printf 'ours one\ncommon\nours three\n' > 'nested/quo"ted name.txt'
printf 'ours one\ncommon\nours three\n' > nested/missing.txt
printf 'ours one\ncommon\nours three\n' > nested/equal-ours.txt
printf 'ours one\ncommon\nours three\n' > nested/equal-theirs.txt
printf 'ours one\ncommon\nours three\n' > nested/reuc.txt
printf 'ours\000binary\002\n' > nested/binary.bin
rm nested/delete-us.txt
printf 'ours changed\n' > nested/delete-them.txt
printf 'ours add/add\n' > 'nested/add "both".txt'
git add -A
git commit -qm ours

git checkout -q theirs
printf 'theirs one\ncommon\ntheirs three\n' > nested/uu.txt
printf 'theirs one\ncommon\ntheirs three\n' > 'nested/quo"ted name.txt'
printf 'theirs one\ncommon\ntheirs three\n' > nested/missing.txt
printf 'theirs one\ncommon\ntheirs three\n' > nested/equal-ours.txt
printf 'theirs one\ncommon\ntheirs three\n' > nested/equal-theirs.txt
printf 'theirs one\ncommon\ntheirs three\n' > nested/reuc.txt
printf 'theirs\000binary\003\n' > nested/binary.bin
printf 'theirs changed\n' > nested/delete-us.txt
rm nested/delete-them.txt
printf 'theirs add/add\n' > 'nested/add "both".txt'
git add -A
git commit -qm theirs

git checkout -q main
if git merge -q --no-commit theirs; then
  echo 'unmerged fixture merge unexpectedly succeeded' >&2
  exit 1
fi

# Exercise absent and stage-matching work trees while retaining the conflicts.
rm nested/missing.txt
git checkout -q --ours -- nested/equal-ours.txt
git checkout -q --theirs -- nested/equal-theirs.txt

# AU, UA, and DD cannot survive an ordinary merge.  Install those stage masks
# explicitly, using objects that came from the real merge's three commits.
mkdir -p synthetic
ours=$(printf 'synthetic ours\n' | git hash-object -w --stdin)
theirs_obj=$(printf 'synthetic theirs\n' | git hash-object -w --stdin)
base=$(git rev-parse HEAD^:synthetic-deleted-both.txt)
printf 'synthetic ours\n' > 'synthetic/added by us.txt'
printf 'synthetic theirs\n' > 'synthetic/added by them.txt'
{
  printf '100644 %s 2\tsynthetic/added by us.txt\n' "$ours"
  printf '100644 %s 3\tsynthetic/added by them.txt\n' "$theirs_obj"
  printf '100644 %s 1\tsynthetic/deleted-both.txt\n' "$base"
} | git update-index --index-info

# Synthetic add/add conflicts for combined-diff edge cases.  Keeping these in
# the existing fixture lets the normal diff/-U0/-U1 matrix cover them.
add_aa() { # add_aa PATH STAGE2 STAGE3
  printf '100644 %s 2\t%s\n100644 %s 3\t%s\n' "$2" "$1" "$3" "$1" \
    | git update-index --index-info
}
multi_body() { # multi_body PREFIX RESULT-WORD
  printf '%s() {\n' "$1"
  i=0; while [ "$i" -lt 8 ]; do printf '\n'; i=$((i + 1)); done
  printf '%s first\n' "$2"
  i=0; while [ "$i" -lt 12 ]; do printf '\n'; i=$((i + 1)); done
  printf '%s last\n' "$2"
}
u0_ours=$(printf 'a\nb\n} xx\nx\n} xx\n    body;\n' | git hash-object -w --stdin)
u0_theirs=$(printf 'x\nx\n} xx\n    body;\n' | git hash-object -w --stdin)
add_aa nested/u0-underflow.txt "$u0_ours" "$u0_theirs"
: > nested/u0-underflow.txt

multi_ours=$(multi_body function_name ours | git hash-object -w --stdin)
multi_theirs=$(multi_body function_name theirs | git hash-object -w --stdin)
add_aa nested/multi-hunk.txt "$multi_ours" "$multi_theirs"
multi_body function_name result > nested/multi-hunk.txt

dir_ours=$(multi_body directory_case_unique ours | git hash-object -w --stdin)
dir_theirs=$(multi_body directory_case_unique theirs | git hash-object -w --stdin)
add_aa nested/wt-directory.txt "$dir_ours" "$dir_theirs"
mkdir nested/wt-directory.txt

stages() { git ls-files -u -- "$1" | awk '{printf "%s", $3}'; }
check_stages() {
  got=$(stages "$1")
  [ "$got" = "$2" ] || {
    echo "bad unmerged stages for $1: got $got, want $2" >&2
    exit 1
  }
}
check_stages nested/uu.txt 123
check_stages 'nested/add "both".txt' 23
check_stages nested/delete-us.txt 13
check_stages nested/delete-them.txt 12
check_stages 'synthetic/added by us.txt' 2
check_stages 'synthetic/added by them.txt' 3
check_stages synthetic/deleted-both.txt 1
check_stages nested/u0-underflow.txt 23
check_stages nested/multi-hunk.txt 23
check_stages nested/wt-directory.txt 23

# Seed a resolve-undo record, then restore only stages 2 and 3 for the same
# path.  Resolving it again must preserve the prior stage-1 record in REUC.
reuc1=$(git rev-parse ':1:nested/reuc.txt')
reuc2=$(git rev-parse ':2:nested/reuc.txt')
reuc3=$(git rev-parse ':3:nested/reuc.txt')
git add nested/reuc.txt
{
  printf '0 %040d\tnested/reuc.txt\n' 0
  printf '100644 %s 2\tnested/reuc.txt\n' "$reuc2"
  printf '100644 %s 3\tnested/reuc.txt\n' "$reuc3"
} | git update-index --index-info
check_stages nested/reuc.txt 23
git ls-files --resolve-undo -- nested/reuc.txt | grep -q "^100644 $reuc1 1" || {
  echo 'resolve-undo record was not retained' >&2
  exit 1
}

cd "$dir"
cp -a unmerged-build unmerged-v2
git -C unmerged-v2 update-index --index-version 2 --force-write-index

cp -a unmerged-build unmerged-v3
# An extended stage-zero entry makes Git retain index version 3.
git -C unmerged-v3 update-index --skip-worktree anchor.txt
git -C unmerged-v3 update-index --index-version 3 --force-write-index

cp -a unmerged-build unmerged-v4
git -C unmerged-v4 update-index --index-version 4 --force-write-index

cp -a unmerged-v2 unmerged-resolved
git -C unmerged-resolved add nested/uu.txt

cp -a unmerged-v2 unmerged-no-merge
rm unmerged-no-merge/.git/MERGE_HEAD

for spec in unmerged-v2:2 unmerged-v3:3 unmerged-v4:4; do
  repo=${spec%:*} want=${spec#*:}
  got=$(od -An -tu1 -j7 -N1 "$repo/.git/index" | tr -d ' ')
  [ "$got" = "$want" ] || {
    echo "bad index version for $repo: got $got, want $want" >&2
    exit 1
  }
done
