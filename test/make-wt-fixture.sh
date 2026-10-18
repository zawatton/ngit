#!/bin/sh
# Build work-tree fixtures for status/diff under DIR (which must already
# hold DIR/loose from make-fixture.sh):
#   DIR/dirty      - every index/work tree state status and diff report
#   DIR/v4         - DIR/dirty with an index in version 4
#   DIR/st-*       - small clones in each branch-tracking state
# Usage: test/make-wt-fixture.sh DIR
set -eu
dir=$(cd "$1" && pwd)
export GIT_AUTHOR_NAME="Fixture Author" GIT_AUTHOR_EMAIL=author@example.com
export GIT_COMMITTER_NAME="Fixture Committer" GIT_COMMITTER_EMAIL=committer@example.com
export GIT_CONFIG_NOSYSTEM=1 HOME="$dir"
export GIT_AUTHOR_DATE="1700500000 +0900" GIT_COMMITTER_DATE="1700500000 +0900"
rm -rf "$dir/dirty" "$dir/v4" "$dir"/st-* "$dir/subsrc"
clone() { git clone -q --no-local "$dir/loose" "$dir/$1"; }

# Submodule source repository.
mkdir "$dir/subsrc" && cd "$dir/subsrc"
git init -q -b main
echo sub > s.txt && git add s.txt && git commit -q -m "sub one"
echo sub2 >> s.txt && git commit -q -am "sub two"

clone dirty
cd "$dir/dirty"
printf '*.log\nbuild/\n!keep.log\n/rootonly.txt\ndocs/**/gen-*\nonlyign/*.tmp\n' > .gitignore
printf '*.dat binary\n*.crlf text eol=lf\nsub/*.txt -text\n' > .gitattributes
printf 'bin\000ary\001data\n' > bin.dat
printf 'line one\nline two\n' > crlf.txt
printf 'attr one\nattr two\n' > eol.crlf
echo "space" > "space name.txt"
i=0
while [ $i -lt 60 ]; do echo "func_$i() {"; echo "    body $i"; echo "}"; i=$((i + 1)); done > code.c
echo gone > staged-del.txt
echo keep > rename-me.txt
git add -A
git -c protocol.file.allow=always submodule -q add "$dir/subsrc" sub1
git -c protocol.file.allow=always submodule -q add "$dir/subsrc" sub2
git -C sub2 checkout -q HEAD~1
git add sub2
git commit -q -m "work tree fixture base"
git -C sub2 checkout -q main
git add sub2
git commit -q -m "advance sub2"
git -C sub2 checkout -q HEAD~1

# Staged changes.
echo "staged change" >> a.txt && git add a.txt
echo "unstaged change" >> a.txt
echo new > new.txt && git add new.txt
echo new2 > new-then-mod.txt && git add new-then-mod.txt && echo more >> new-then-mod.txt
git rm -q staged-del.txt
git mv rename-me.txt renamed.txt
git mv "we\"ird name.txt" "moved name.txt"
echo ita > ita.txt && git add -N ita.txt
# Unstaged changes.
rm b.txt
chmod +x docs/long.txt
rm link && ln -s b.txt link
rm src/run.sh && ln -s ../a.txt src/run.sh
printf 'line one\r\nline two\r\n' > crlf.txt
printf 'attr one\r\nattr two changed\r\n' > eol.crlf
printf 'bin\000ary\002data\n' > bin.dat
sed 's/body 30$/body thirty/; s/body 45$/body 45 changed/' code.c > code.tmp && mv code.tmp code.c
touch -d '2001-01-01' "日本語ディレクトリ/ファイル.txt"
echo dirt >> sub1/s.txt && echo junk > sub1/untracked.txt
# Untracked and ignored paths.
echo u > untracked.txt
echo u > "untracked space.txt"
mkdir -p newdir/deeper emptydir build onlyign docs/x sub
echo n > newdir/deeper/n.txt
echo l > foo.log && echo k > keep.log && echo b > build/out.txt
echo r > rootonly.txt && echo r > sub/rootonly.txt
echo t > onlyign/a.tmp
echo g > docs/x/gen-file && echo h > docs/x/hand-file
mkdir nested && git -C nested init -q && echo x > nested/x.txt

cd "$dir"
git clone -q --no-local dirty v4 2>/dev/null
cd v4
echo extra > extra.txt && git add extra.txt && echo m >> README 2>/dev/null || true
git update-index --index-version 4
echo v4 >> a.txt

# Branch tracking states.
clone st-uptodate
clone st-ahead && (cd "$dir/st-ahead" && echo x > x.txt && git add x.txt && git commit -q -m ahead && echo u > u.txt)
clone st-behind && git -C "$dir/st-behind" reset -q --hard HEAD~1
clone st-diverged && (cd "$dir/st-diverged" && git reset -q --hard HEAD~2 &&
  echo y > y.txt && git add y.txt && git commit -q -m diverge)
clone st-gone && git -C "$dir/st-gone" config branch.main.merge refs/heads/nonexistent
clone st-detached-at && git -C "$dir/st-detached-at" checkout -q v1.0 2>/dev/null
clone st-detached-from && (cd "$dir/st-detached-from" && git checkout -q v1.0 2>/dev/null &&
  git commit -q --allow-empty -m "detached work")
mkdir "$dir/st-empty" && (cd "$dir/st-empty" && git init -q -b main && echo e > e.txt &&
  git add e.txt && echo u > u.txt)
mkdir "$dir/st-empty-clean" && git -C "$dir/st-empty-clean" init -q -b main
clone st-staged-only && (cd "$dir/st-staged-only" && echo s >> a.txt && git add a.txt)
