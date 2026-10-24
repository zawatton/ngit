#!/bin/sh
# Deterministic replay histories, including a conflict in the middle of a range.
set -eu
dir=$1/${2:-sequencer}
rm -rf "$dir"
mkdir -p "$dir"
cd "$dir"
export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null
export GIT_AUTHOR_NAME='Fixture Author' GIT_AUTHOR_EMAIL=author@example.com
export GIT_COMMITTER_NAME='Fixture Committer' GIT_COMMITTER_EMAIL=committer@example.com
export GIT_AUTHOR_DATE='1700800000 +0900' GIT_COMMITTER_DATE='1700800000 +0900'
git init -q -b main
git config gc.auto 0
git config merge.renames false
git config core.autocrlf false
commit() { git add -A; git commit -qm "$1"; }
printf 'base\ncommon\nlast\n' >text.txt
printf 'keep\n' >keep.txt
commit base
git tag base
printf 'main\ncommon\nlast\n' >text.txt
printf 'main\n' >main.txt
commit main
git switch -qc side base
printf 'first\n' >first.txt
commit 'first subject'
git tag first
printf 'side\ncommon\nlast\n' >text.txt
commit 'second subject'
git tag second
printf 'third\n' >third.txt
commit 'third subject'
git switch -qc clean base
printf 'clean\n' >clean.txt
commit clean
git switch -qc topic base
printf 'first\n' >first.txt
commit 'first subject'
printf 'third\n' >third.txt
commit 'third subject'
git switch -qc applied clean
git cherry-pick first >/dev/null
git switch -qc merged clean
git merge -qm 'merge main' main
git switch -qc symlink-base base
mkdir backing
printf 'precious\n' >backing/file
ln -s backing link
commit 'symlink base'
git switch -qc symlink-next
rm link
mkdir link
printf 'replacement\n' >link/file
commit 'replace symlink with directory'
git switch -q main
