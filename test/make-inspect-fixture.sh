#!/bin/sh
# Deterministic line provenance, grep and naming corner cases.
set -eu
dir=$1/inspect
rm -rf "$dir"
mkdir -p "$dir"
cd "$dir"
export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null
export GIT_AUTHOR_NAME='First Author' GIT_AUTHOR_EMAIL=first@example.org
export GIT_COMMITTER_NAME='Test Committer' GIT_COMMITTER_EMAIL=committer@example.org
export GIT_AUTHOR_DATE='1700000000 +0900' GIT_COMMITTER_DATE='1700000000 +0900'
git init -q -b main
git config gc.auto 0
mkdir -p dir
printf 'alpha one\nbeta two\nALPHA beta\nalphabet\nfoo+bar\n(foo|bar)\nfoo bar\nlast' > original
printf 'alpha\nbeta\n\nfoo_bar\nalpha beta\n' > dir/text
printf '\000alpha\nbeta\n' > binary
printf 'alpha\n' > 'space name'
printf 'alpha\n' > 'dir/quoted"name'
git add -A
git commit -qm 'initial lines'
git tag -a -m release v1
git checkout -qb side
printf 'side\n' > side-file
git add -A
GIT_AUTHOR_DATE='1700000100 -0330' GIT_COMMITTER_DATE='1700000100 -0330' git commit -qm 'side commit'
git checkout -q main
git mv original renamed
sed -i 's/beta two/beta  two/; s/ALPHA beta/ALPHA new/' renamed
git add -A
GIT_AUTHOR_NAME='A Longer Author' GIT_AUTHOR_EMAIL=long@example.org \
GIT_AUTHOR_DATE='1700000200 +0530' GIT_COMMITTER_DATE='1700000200 +0530' \
  git commit -qm '[PATCH] change and rename'
GIT_AUTHOR_DATE='1700000300 +0000' GIT_COMMITTER_DATE='1700000300 +0000' \
  git merge -q --no-ff -m 'merge side' side
printf 'work tree alpha\n' >> dir/text
printf 'index only alpha\n' > staged
git add staged
printf 'untracked alpha\n' > untracked
ln -s dir/text link
git add link
git pack-refs --all
