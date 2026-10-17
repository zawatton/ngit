#!/bin/sh
# Build deterministic fixture repositories under DIR:
#   DIR/loose   - all objects loose, loose refs
#   DIR/packed  - same history after `git gc --aggressive` (deltas, packed-refs)
#   DIR/wt      - a linked worktree of DIR/packed on branch `side`
# Usage: test/make-fixture.sh DIR
set -eu
dir=$1
rm -rf "$dir"
mkdir -p "$dir/loose"
cd "$dir/loose"

export GIT_AUTHOR_NAME="Fixture Author" GIT_AUTHOR_EMAIL=author@example.com
export GIT_COMMITTER_NAME="Fixture Committer" GIT_COMMITTER_EMAIL=committer@example.com
export GIT_CONFIG_NOSYSTEM=1 HOME="$dir"
n=0
commit() {
  n=$((n + 1))
  export GIT_AUTHOR_DATE="$((1700000000 + n * 3600)) +0900"
  export GIT_COMMITTER_DATE="$((1700000000 + n * 3600)) $2"
  git add -A
  git commit -q -m "$1"
}

git init -q -b main
git config gc.auto 0
mkdir -p src docs "日本語ディレクトリ"
i=0
while [ $i -lt 400 ]; do echo "line $i of a long text file used to create deltas"; i=$((i + 1)); done > docs/long.txt
echo hello > a.txt
printf '#!/bin/sh\necho run\n' > src/run.sh && chmod +x src/run.sh
ln -s a.txt link
echo 'quote "me"' > 'we"ird name.txt'
echo tab > "$(printf 'tab\tname.txt')"
commit "initial commit" "+0900"

echo "line appended" >> docs/long.txt
echo "日本語の内容" > "日本語ディレクトリ/ファイル.txt"
commit "日本語のコミットメッセージ

本文の二行目。

    indented line
" "-0500"

git checkout -q -b side
sed 's/line 10 /LINE TEN /' docs/long.txt > docs/long.tmp && mv docs/long.tmp docs/long.txt
commit "side change" "+0000"
git tag -a -m "annotated tag message" v1.0
git checkout -q main
echo "main change" > b.txt
commit "main change" "+0530"
GIT_AUTHOR_DATE="1700100000 +0900" GIT_COMMITTER_DATE="1700100000 +0900" \
  git merge -q --no-ff -m "Merge branch 'side'" side
git tag light
echo more >> docs/long.txt
commit "after merge" "+0900"
# Same committer timestamp as the previous commit, to pin queue tie order.
GIT_AUTHOR_DATE="$((1700000000 + n * 3600)) +0900" \
GIT_COMMITTER_DATE="$((1700000000 + n * 3600)) +0900" \
  git commit -q --allow-empty -m "same timestamp"

cd "$dir"
git clone -q --no-local "$dir/loose" packed
cd packed
git config gc.auto 0
git branch -q side origin/side
git gc -q --aggressive
git worktree add -q "$dir/wt" side
