#!/bin/sh
# Build deterministic histories used by the bisect comparison suite.
set -eu
base=$1

export GIT_AUTHOR_NAME='Test Author' GIT_AUTHOR_EMAIL=test@example.com
export GIT_COMMITTER_NAME='Test Author' GIT_COMMITTER_EMAIL=test@example.com

linear=$base/bisect-linear
rm -rf "$linear"
git init -q -b main "$linear"
git -C "$linear" config gc.auto 0
i=0
while [ "$i" -le 8 ]; do
  printf '%s\n' "$i" >"$linear/value"
  printf 'line %s\n' "$i" >>"$linear/history"
  git -C "$linear" add value history
  stamp=$((1700700000 + i))
  GIT_AUTHOR_DATE="$stamp +0900" GIT_COMMITTER_DATE="$stamp +0900" \
    git -C "$linear" commit -qm "commit $i"
  git -C "$linear" tag "c$i"
  i=$((i + 1))
done

branch=$base/bisect-branch
rm -rf "$branch"
git init -q -b main "$branch"
git -C "$branch" config gc.auto 0
printf 'base\n' >"$branch/base"
git -C "$branch" add base
GIT_AUTHOR_DATE='1700710000 +0900' GIT_COMMITTER_DATE='1700710000 +0900' \
  git -C "$branch" commit -qm base
git -C "$branch" tag base
git -C "$branch" switch -qc side
printf 'side one\n' >"$branch/side"
git -C "$branch" add side
GIT_AUTHOR_DATE='1700710003 +0900' GIT_COMMITTER_DATE='1700710003 +0900' \
  git -C "$branch" commit -qm 'side one'
printf 'side two\n' >>"$branch/side"
git -C "$branch" add side
GIT_AUTHOR_DATE='1700710001 +0900' GIT_COMMITTER_DATE='1700710001 +0900' \
  git -C "$branch" commit -qm 'side two'
git -C "$branch" switch -q main
printf 'main one\n' >"$branch/main"
git -C "$branch" add main
GIT_AUTHOR_DATE='1700710001 +0900' GIT_COMMITTER_DATE='1700710001 +0900' \
  git -C "$branch" commit -qm 'main one'
GIT_AUTHOR_DATE='1700710000 +0900' GIT_COMMITTER_DATE='1700710000 +0900' \
  git -C "$branch" merge -q --no-ff -m merge side
printf 'tip\n' >>"$branch/main"
git -C "$branch" add main
GIT_AUTHOR_DATE='1700710005 +0900' GIT_COMMITTER_DATE='1700710005 +0900' \
  git -C "$branch" commit -qm tip

tie=$base/bisect-tie
rm -rf "$tie"
git init -q -b main "$tie"
git -C "$tie" config gc.auto 0
printf 'base\n' >"$tie/base"
git -C "$tie" add base
GIT_AUTHOR_DATE='1700720000 +0900' GIT_COMMITTER_DATE='1700720000 +0900' \
  git -C "$tie" commit -qm case2-c0
git -C "$tie" tag base
git -C "$tie" switch -qc left
printf 'c1\n' >"$tie/left"
git -C "$tie" add left
GIT_AUTHOR_DATE='1700720000 +0900' GIT_COMMITTER_DATE='1700720000 +0900' \
  git -C "$tie" commit -qm case2-c1
printf 'c2\n' >>"$tie/left"
git -C "$tie" add left
GIT_AUTHOR_DATE='1700720000 +0900' GIT_COMMITTER_DATE='1700720000 +0900' \
  git -C "$tie" commit -qm case2-c2
git -C "$tie" switch -q main
printf 'c3\n' >"$tie/right"
git -C "$tie" add right
GIT_AUTHOR_DATE='1700720000 +0900' GIT_COMMITTER_DATE='1700720000 +0900' \
  git -C "$tie" commit -qm case2-c3
printf 'c4\n' >>"$tie/right"
git -C "$tie" add right
GIT_AUTHOR_DATE='1700720000 +0900' GIT_COMMITTER_DATE='1700720000 +0900' \
  git -C "$tie" commit -qm case2-c4
git -C "$tie" switch -q left
GIT_AUTHOR_DATE='1700720000 +0900' GIT_COMMITTER_DATE='1700720000 +0900' \
  git -C "$tie" merge -q --no-ff -m case2-c5 main
git -C "$tie" tag tip
