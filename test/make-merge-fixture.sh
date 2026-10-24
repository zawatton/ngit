#!/bin/sh
# Build deterministic histories for tree merging and stash content merging.
# Usage: test/make-merge-fixture.sh DIR
set -eu
dir=$(cd "$1" && pwd)
export GIT_AUTHOR_NAME="Fixture Author" GIT_AUTHOR_EMAIL=author@example.com
export GIT_COMMITTER_NAME="Fixture Committer" GIT_COMMITTER_EMAIL=committer@example.com
export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null
export GIT_AUTHOR_DATE="1700800000 +0900" GIT_COMMITTER_DATE="1700800000 +0900"

# Only remove repositories owned by this builder.
for name in merge-ff merge-clean merge-conflicts merge-mode merge-df \
            merge-df-reverse merge-criss-cross stash-merge; do
  rm -rf "$dir/$name"
done

init_repo() {
  mkdir "$dir/$1"
  cd "$dir/$1"
  git init -q -b main
  git config gc.auto 0
  git config merge.renames false
  git config core.autocrlf false
  git config commit.gpgsign false
  git config tag.gpgsign false
}
commit() { git add -A; git commit -qm "$1"; }
body() {
  printf '%s\n' one two three four five six seven eight nine ten eleven twelve
}

init_repo merge-ff
body > text.txt
printf 'keep\n' > keep.txt
commit base
git tag base
git branch side
git checkout -q side
printf 'side\n' >> text.txt
mkdir -p new-dir
printf 'new\n' > new-dir/leaf.txt
commit 'side change'
git tag side-tag
git checkout -q main

init_repo merge-clean
body > text.txt
body | sed 's/$/\r/' > crlf.txt
printf 'crlf.txt -text\n' > .gitattributes
printf '#!/bin/sh\necho base\n' > mode.sh
printf 'keep\n' > keep.txt
commit base
git tag base
git branch side
sed 's/^two$/ours two/' text.txt > tmp; mv tmp text.txt
body | sed 's/^two$/ours two/; s/$/\r/' > crlf.txt
printf '#!/bin/sh\necho ours\n' > mode.sh
printf 'ours\n' > ours.txt
commit ours
git checkout -q side
sed 's/^eleven$/theirs eleven/' text.txt > tmp; mv tmp text.txt
body | sed 's/^eleven$/theirs eleven/; s/$/\r/' > crlf.txt
chmod +x mode.sh
mkdir -p new-dir
printf 'new\n' > new-dir/leaf.txt
commit theirs
git tag side-tag
git checkout -q main

init_repo merge-conflicts
printf 'base\ncommon\nlast\n' > content.txt
printf 'base\n' > delete-ours.txt
printf 'base\n' > delete-theirs.txt
printf 'base\000binary\n' > binary.bin
printf 'base\r\ncommon\r\n' > crlf.txt
printf 'crlf.txt -text\n' > .gitattributes
printf 'keep\n' > keep.txt
commit base
git tag base
git branch side
printf 'ours\ncommon\nlast\n' > content.txt
rm delete-ours.txt
printf 'ours\n' > delete-theirs.txt
printf 'ours\000binary\n' > binary.bin
printf 'ours\r\ncommon\r\n' > crlf.txt
printf 'ours\n' > add.txt
commit ours
git checkout -q side
printf 'theirs\ncommon\nlast\n' > content.txt
printf 'theirs\n' > delete-ours.txt
rm delete-theirs.txt
printf 'theirs\000binary\n' > binary.bin
printf 'theirs\r\ncommon\r\n' > crlf.txt
printf 'theirs\n' > add.txt
commit theirs
git checkout -q main

init_repo merge-mode
printf 'base\n' > mode.txt
commit base
git branch side
printf 'ours\n' > mode.txt
chmod +x mode.txt
commit ours
git checkout -q side
rm mode.txt
ln -s target mode.txt
commit theirs
git checkout -q main

init_repo merge-df
printf 'keep\n' > keep.txt
commit base
git branch side
printf 'ours file\n' > collision
commit ours
git checkout -q side
mkdir collision
printf 'theirs leaf\n' > collision/leaf.txt
commit theirs
git checkout -q main
cp -a "$dir/merge-df" "$dir/merge-df-reverse"
git -C "$dir/merge-df-reverse" checkout -q side

# Construct both merge commits explicitly so that their two incomparable
# parents remain the merge bases.  Their tips need a recursive virtual base.
init_repo merge-criss-cross
body > text.txt
printf 'keep\n' > keep.txt
commit base
base=$(git rev-parse HEAD)
sed 's/^two$/A two/' text.txt > tmp; mv tmp text.txt
commit A
a=$(git rev-parse HEAD)
git checkout -q --detach "$base"
sed 's/^eleven$/B eleven/' text.txt > tmp; mv tmp text.txt
commit B
b=$(git rev-parse HEAD)
body | sed 's/^two$/A two/; s/^eleven$/B eleven/' > text.txt
git add text.txt
tree=$(git write-tree)
left=$(printf 'merge A and B\n' | git commit-tree "$tree" -p "$a" -p "$b")
right=$(printf 'merge B and A\n' | git commit-tree "$tree" -p "$b" -p "$a")
git branch -f main "$left"
git branch side "$right"
git checkout -q main
sed 's/^A two$/two/; s/^four$/ours four/' text.txt > tmp; mv tmp text.txt
commit 'left tip'
git checkout -q side
sed 's/^B eleven$/eleven/; s/^nine$/theirs nine/' text.txt > tmp; mv tmp text.txt
commit 'right tip'
git checkout -q main
[ "$(git merge-base --all main side | wc -l)" -eq 2 ] || {
  echo 'criss-cross fixture does not have two merge bases' >&2
  exit 1
}

init_repo stash-merge
body > text.txt
printf 'base\r\ncommon\r\nlast\r\n' > crlf.txt
printf 'crlf.txt -text\n' > .gitattributes
printf 'base\000binary\n' > binary.bin
printf 'base\n' > delete.txt
printf 'base\n' > mode.txt
printf 'keep\n' > keep.txt
commit base
