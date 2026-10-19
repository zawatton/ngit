#!/bin/sh
# Build deterministic fixtures for log and branch under DIR:
#   DIR/hist       - merges (TREESAME, criss-cross, octopus), renames, mode
#                    changes, a binary file, tags, equal commit timestamps
#   DIR/origin.git - bare copy of hist, later advanced and pruned
#   DIR/work       - clone of origin.git with tracking branches (ahead,
#                    behind, gone, local upstream) and a linked worktree
#   DIR/work-wt    - linked worktree of work on branch `wtb`
#   DIR/ren        - renames: inexact, same basename, competing candidates,
#                    mode changes, binary files, CRLF
#   DIR/ren-limit  - clone of ren with diff.renameLimit 2
#   DIR/tags       - copy of hist with loose/packed tags on all object types
# Usage: test/make-log-fixture.sh DIR
set -eu
dir=$1
rm -rf "$dir/hist" "$dir/origin.git" "$dir/work" "$dir/work-wt" "$dir/ren"
mkdir -p "$dir/hist"
export GIT_CONFIG_NOSYSTEM=1 HOME="$dir"
export GIT_AUTHOR_NAME="Log Author" GIT_AUTHOR_EMAIL=log-author@example.com
export GIT_COMMITTER_NAME="Log Committer" GIT_COMMITTER_EMAIL=log-committer@example.com
n=0
stamp() { # stamp TZ: advance the clock by one hour
  n=$((n + 1))
  export GIT_AUTHOR_DATE="$((1710000000 + n * 3600 - 600)) +0900"
  export GIT_COMMITTER_DATE="$((1710000000 + n * 3600)) $1"
}
commit() { # commit MESSAGE [TZ]
  stamp "${2:-+0900}"
  git add -A
  git commit -q --allow-empty-message -m "$1"
}
merge() { # merge MESSAGE BRANCH...
  m=$1; shift
  stamp +0900
  git merge -q --no-ff -m "$m" "$@"
}

cd "$dir/hist"
git init -q -b main
git config gc.auto 0
mkdir -p src doc lib
seq 1 60 > src/main.c
printf 'readme\n' > README
printf 'doc one\n' > doc/one.txt
commit "root: initial import

The first commit of the log fixture.
It has a body of two lines." +0000
seq 1 3 60 > lib/util.c
printf '\000\001\002binary\n' > data.bin
commit "add util and a binary file" -0700
sed 's/^7$/seven/' src/main.c > t && mv t src/main.c
commit "main: rename line seven"
git tag -a -m "release one" v1.0
git checkout -q -b topic
printf 'topic\n' >> doc/one.txt
commit "topic: extend doc one" +0530
printf '\000\003binary changed\n' > data.bin
commit "topic: change binary"
git checkout -q main
git mv README README.md
commit "rename README"
merge "Merge branch 'topic'" topic
git tag light
# A branch that touches only doc/ gives a TREESAME merge for src/.
git checkout -q -b docs
printf 'doc two\n' > doc/two.txt
commit "docs: add doc two"
git checkout -q main
chmod +x lib/util.c
commit "make util executable"
merge "Merge branch 'docs'" docs
# Criss-cross: c1 and c2 merge each other's tips.
git checkout -q -b c1
printf 'c1\n' > c1.txt
commit "c1: start"
git checkout -q main
git checkout -q -b c2
printf 'c2\n' > c2.txt
commit "c2: start"
git branch -q c2-base
git checkout -q c1
merge "c1: merge c2" c2-base
git checkout -q c2
merge "c2: merge c1" c1~1
git checkout -q c1
printf 'c1 more\n' >> c1.txt
commit "c1: more"
git checkout -q c2
printf 'c2 more\n' >> c2.txt
commit "c2: more"
git checkout -q main
merge "Merge c1 and c2 (criss-cross)" c1 c2
# Octopus with three branches.
for b in oa ob oc; do
  git checkout -q -b $b main
  printf '%s\n' $b > $b.txt
  commit "$b: add $b.txt"
done
git checkout -q main
merge "Octopus merge" oa ob oc
# Equal committer timestamps on two branches pin the queue tie order.
git checkout -q -b tie main
printf 'tie\n' > tie.txt
git add -A
GIT_AUTHOR_DATE="1710500000 +0900" GIT_COMMITTER_DATE="1710500000 +0900" \
  git commit -q -m "tie: same time A"
git checkout -q main
seq 1 70 > src/main.c
git add -A
GIT_AUTHOR_DATE="1710500000 +0900" GIT_COMMITTER_DATE="1710500000 +0900" \
  git commit -q -m "main: same time B"
merge "Merge branch 'tie'" tie
# Messages: an empty one, a long subject, trailing blank lines, a
# multi-line subject, and a different author.
printf 'x\n' > x.txt
commit ""
printf 'y\n' > y.txt
commit "a very long subject line that keeps going well past the usual seventy two column limit for subjects"
printf 'z\n' > z.txt
commit "$(printf 'two line\nsubject\n\nbody after\n\n\n')"
printf 'w\n' > w.txt
stamp -0330
GIT_AUTHOR_NAME="Other Person" GIT_AUTHOR_EMAIL=other@example.org \
GIT_AUTHOR_DATE="1600000000 -0330" git add -A
GIT_AUTHOR_NAME="Other Person" GIT_AUTHOR_EMAIL=other@example.org \
GIT_AUTHOR_DATE="1600000000 -0330" git commit -q -m "commit by another author"
git rm -q -r lib
commit "delete lib"
git tag -a -m "release two" v2.0
git tag mid HEAD~3
git branch -q old v1.0

# Remote side: a bare copy, and a clone with tracking branches.
cd "$dir"
git clone -q --bare hist origin.git
git clone -q origin.git work
cd work
git config gc.auto 0
git checkout -q -b ahead origin/topic
printf 'ahead\n' > ahead.txt
commit "ahead: local work"
git checkout -q -b behind origin/docs
git reset -q --hard HEAD~1
git checkout -q -b gone origin/c1
git checkout -q -b local-up --track main
git checkout -q -b notrack main
printf 'nt\n' > nt.txt
commit "notrack: own commit"
git checkout -q main
git worktree add -q -b wtb ../work-wt main~2
# Advance and prune the remote so `ahead` is also behind and `gone` is gone.
cd "$dir/hist"
git checkout -q topic
printf 'remote topic\n' >> doc/one.txt
commit "topic: remote change"
git checkout -q main
git push -q "$dir/origin.git" topic
git push -q "$dir/origin.git" :c1
cd "$dir/work"
git fetch -q --prune origin
git pack-refs --all
git update-ref refs/heads/loose-only main~4

# Rename detection.
mkdir -p "$dir/ren" && cd "$dir/ren"
git init -q -b main
git config gc.auto 0
mkdir -p a b deep/dir
seq 1 100 > a/numbers.txt
seq 1 50 | sed 's/$/ text line/' > a/text.txt
seq 1 30 > a/same-base.txt
printf 'line %s\r\n' $(seq 1 40) > a/crlf.txt
head -c 3000 /dev/zero | tr '\0' 'x' > a/long-line.txt
printf '\000\001binary %s\n' $(seq 1 50) > a/blob.bin
seq 1 20 > a/cand.txt
seq 1 20 | sed 's/$/!/' > a/other.txt
: > a/empty
commit "rename fixture base"
git mv a/numbers.txt b/numbers2.txt && seq 1 90 > b/numbers2.txt
git mv a/text.txt deep/dir/text.txt && sed -i 's/^1\(.\) text/X\1 text/' deep/dir/text.txt
git mv a/same-base.txt b/same-base.txt && echo extra >> b/same-base.txt
git mv a/crlf.txt b/crlf.txt && printf 'more\r\n' >> b/crlf.txt
git mv a/long-line.txt b/long.txt && printf 'tail\n' >> b/long.txt
git mv a/blob.bin b/blob2.bin && printf '\000end\n' >> b/blob2.bin
git rm -q a/cand.txt
seq 1 21 > b/cand-copy1.txt && seq 0 20 > b/cand-copy2.txt
git mv a/other.txt b/other.txt && chmod +x b/other.txt
git mv a/empty b/empty
commit "inexact renames"
seq 1 200 > c1.txt; seq 201 400 > c2.txt; seq 401 600 > c3.txt
commit "three files"
git rm -q c1.txt c2.txt c3.txt
seq 1 199 > d1.txt; seq 201 399 > d2.txt; seq 401 599 > d3.txt
commit "renames over the limit"
cd "$dir" && rm -rf ren-limit && git clone -q ren ren-limit
git -C ren-limit config diff.renameLimit 2

# Keep non-commit tags out of the history-walking fixtures.
rm -rf "$dir/tags"
cp -a "$dir/hist" "$dir/tags"
cd "$dir/tags"
export GIT_COMMITTER_DATE="1710800000 +0530"
git tag v1.2 main~2
git tag v1.9 main~1
git tag v1.10 main
git tag V1.3 main~3
for version in v1.0-rc1 v1.0+build v1.0.0 v1.01 v1.001 v1.00 v1.000 v1.09 v1.090; do
  git tag "$version" main
done
git tag -a -m "$(printf 'annotation first\nsecond line\n\nbody line\nlast line')" annotated main~2
git tag -a -m "a much longer tag name" a-tag-name-longer-than-fifteen main~4
git tag -a -m "日本語の注釈" 日本語 main~3
git tag -a -m "" empty-message main
git tag -a -m "$(printf 'signed-looking message\n\n-----BEGIN PGP SIGNATURE-----\nopaque\n-----END PGP SIGNATURE-----')" signed-message main
git tag tree main^{tree}
git tag blob main:README.md
git tag -a -m "annotated tree" annotated-tree main^{tree}
git tag -a -m "annotated blob" annotated-blob main:README.md
git -c advice.nestedTag=false tag -a -m "nested annotation" nested annotated
git tag tag-object annotated
git pack-refs --all
git tag loose-tag main~5

# Keep conflict resolutions separate from the history-walking fixtures.
rm -rf "$dir/show"
cp -a "$dir/hist" "$dir/show"
cd "$dir/show"
git checkout -q -b cc-base main
seq 1 45 > conflict.txt
printf 'alpha\nold\nomega\n' > 'space name'
printf 'old' > no-newline
printf 'delete base\n' > delete.txt
printf 'header\nbase\ntail\n' > coalesce.txt
printf '\000base\n' > combined.bin
commit "combined base"
git checkout -q -b cc-left
sed -i 's/^6$/left six/; s/^25$/left twenty five/' conflict.txt
printf 'alpha\nleft\nomega\n' > 'space name'
printf 'left' > no-newline
printf 'delete left\n' > delete.txt
printf 'header\nshared\nleft\nshared again\ntail\n' > coalesce.txt
printf 'left\n' > both-new
printf '\000left\n' > combined.bin
commit "combined left"
git checkout -q -b cc-right cc-base
sed -i 's/^6$/right six/; s/^25$/right twenty five/' conflict.txt
printf 'alpha\nright\nomega\n' > 'space name'
printf 'right' > no-newline
printf 'delete right\n' > delete.txt
printf 'header\nright\nshared\nshared again\ntail\n' > coalesce.txt
printf 'right\n' > both-new
printf '\000right\n' > combined.bin
commit "combined right"
git merge -q --no-ff --no-commit cc-left >/dev/null 2>&1 || :
test -f .git/MERGE_HEAD
seq 1 45 | sed 's/^6$/resolved six/; s/^25$/right twenty five/' > conflict.txt
printf 'alpha\nresolved\nomega\n' > 'space name'
printf 'resolved' > no-newline
printf 'header\nresolved\ntail\n' > coalesce.txt
printf 'resolved\n' > both-new
chmod +x both-new
git rm -q -f delete.txt
printf '\000resolved\n' > combined.bin
commit "combined resolution"
git branch cc-merge
stamp +0900
tree=$(git rev-parse cc-merge^{tree})
left=$(git rev-parse cc-left) right=$(git rev-parse cc-right) base=$(git rev-parse cc-base)
octopus=$(printf 'combined three parents\n' | git commit-tree "$tree" -p "$left" -p "$right" -p "$base")
git branch cc-octopus "$octopus"
