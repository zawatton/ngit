#!/bin/sh
# S11.4 mail patch, strict apply and am comparisons.  Sourced by compare.sh.

WTREE=1 WREFS=1 WSTDERR=1
patch_repo="$out/patch-repo"
mkdir -p "$patch_repo/dir"
(
  cd "$patch_repo" || exit 1
  git init -q -b main
  git config user.name 'Patch Author'
  git config user.email patch@example.com
  printf 'one\ntwo\nthree\n' >alpha.txt
  printf 'kept\n' >dir/kept.txt
  git add -A
  GIT_AUTHOR_NAME='Patch Author' GIT_AUTHOR_EMAIL=patch@example.com \
    GIT_AUTHOR_DATE='1700000000 +0900' GIT_COMMITTER_DATE='1700000100 +0900' git commit -qm base
  printf 'one\nTWO\nthree\nfour\n' >alpha.txt
  GIT_AUTHOR_NAME='Patch Author' GIT_AUTHOR_EMAIL=patch@example.com \
    GIT_AUTHOR_DATE='1700100000 +0900' GIT_COMMITTER_DATE='1700100100 +0900' \
    git commit -qam 'UP Case + punctuation'
  printf 'new file\n' >beta.txt
  chmod +x beta.txt
  git add beta.txt
  GIT_AUTHOR_NAME='Patch Author' GIT_AUTHOR_EMAIL=patch@example.com \
    GIT_AUTHOR_DATE='1700200000 +0900' GIT_COMMITTER_DATE='1700200100 +0900' \
    git commit -qm 'second patch'
  git mv dir/kept.txt dir/renamed.txt
  printf 'body\n' >>dir/renamed.txt
  GIT_AUTHOR_NAME='Patch Author' GIT_AUTHOR_EMAIL=patch@example.com \
    GIT_AUTHOR_DATE='1700300000 +0900' GIT_COMMITTER_DATE='1700300100 +0900' \
    git commit -qam 'rename patch'
) >/dev/null 2>&1 || { echo 'patch fixture build failed'; fail=1; return; }

root_repo="$out/root-repo"
mkdir -p "$root_repo"
(
  cd "$root_repo" || exit 1
  git init -q -b main
  git config user.name 'Root Author'
  git config user.email root@example.com
  printf 'root\n' >root.txt
  git add root.txt
  GIT_AUTHOR_DATE='1700000000 +0900' GIT_COMMITTER_DATE='1700000100 +0900' \
    git commit -qm root
) >/dev/null 2>&1 || { echo 'root patch fixture build failed'; fail=1; return; }

# Generation: ranges, stdout, numbered and explicitly unnumbered subjects,
# output directories, one-patch defaults and numeric count shorthand.
wrun "$patch_repo" '' format-patch --stdout HEAD~3..HEAD
wrun "$patch_repo" '' format-patch --stdout -N HEAD~3..HEAD
wrun "$patch_repo" '' format-patch --stdout -n HEAD~1..HEAD
wrun "$patch_repo" '' format-patch --stdout -1 HEAD
wrun "$patch_repo" '' format-patch --stdout HEAD~1
wrun "$root_repo" '' format-patch --stdout -1 HEAD
wrun "$root_repo" '' format-patch --stdout HEAD..HEAD
ten_patch_setup='i=1; while [ $i -le 7 ]; do echo extra$i >>series-ten.txt; git add series-ten.txt; git commit -qm "extra $i"; i=$((i + 1)); done'
wrun "$patch_repo" "$ten_patch_setup" format-patch --stdout HEAD~10..HEAD
wrun "$patch_repo" '' format-patch -o outgoing HEAD~3..HEAD
wrun "$patch_repo" '' format-patch HEAD~1..HEAD

# apply validates the entire input before writing.  Exercise work-tree only,
# index coupling, index only, reverse, stats, offsets and refusals.
one_patch='git format-patch -1 --stdout >series.patch; git reset -q --hard HEAD~1'
wrun "$patch_repo" "$one_patch" apply --check series.patch
wrun "$patch_repo" "$one_patch" apply series.patch
wsteps "$patch_repo" "$one_patch" 'apply stdin' 'g apply <series.patch'
wrun "$patch_repo" "$one_patch" apply --index series.patch
wrun "$patch_repo" "$one_patch" apply --cached series.patch
wrun "$patch_repo" "$one_patch; git apply --index series.patch" apply -R --check series.patch
wrun "$patch_repo" "$one_patch; git apply --index series.patch" apply -R --index series.patch
wrun "$patch_repo" "$one_patch" apply --stat series.patch
large_stat_setup='long=directory-with-a-name-that-is-deliberately-more-than-fifty-characters/long.txt; mkdir -p "$(dirname "$long")"; seq 1 150 >short.txt; seq 1 20 >"$long"; git add short.txt "$long"; git commit -qm stat-base; seq 201 210 >short.txt; seq 1 130 >"$long"; git diff >multi-stat.patch; git checkout -q -- short.txt "$long"'
wrun "$patch_repo" "$large_stat_setup" apply --stat multi-stat.patch
wrun "$patch_repo" "$one_patch; printf 'occupied\n' >dir/renamed.txt" apply series.patch
wrun "$patch_repo" "$one_patch; printf 'dirty\n' >>dir/kept.txt" apply --index series.patch

offset_setup='seq 1 10 >offset.txt; git add offset.txt; git commit -qm offset-base; sed -i "s/^6$/SIX/" offset.txt; git commit -qam offset-change; git format-patch -1 --stdout >series.patch; git reset -q --hard HEAD~1; sed -i "1i leading" offset.txt'
wrun "$patch_repo" "$offset_setup" apply series.patch
bad_context='git format-patch -1 --stdout HEAD~2 >series.patch; git reset -q --hard HEAD~3; sed -i "s/two/not-context/" alpha.txt'
wrun "$patch_repo" "$bad_context" apply --check series.patch

# Reverse the exact -U1 binary diff used by stash_staged.  These cases also
# verify refusal leaves precious work-tree bytes and directory modes intact.
f5_diff='git diff-tree -p -U1 --binary HEAD "$(git write-tree)" >f5.patch'
for local in empty precious; do
  f5_setup=": >f; git add f; $f5_diff"
  [ "$local" = empty ] || f5_setup="$f5_setup; printf precious >f"
  wrun "$patch_repo" "$f5_setup" apply -R f5.patch
  wrun "$patch_repo" "$f5_setup" apply -R --check f5.patch
done
for edge in beginning end; do
  if [ "$edge" = beginning ]; then
    f5_base='old\nanchor\n'; f5_index='staged\nanchor\n'
    f5_local='precious\nanchor\nstaged\nanchor\n'
  else
    f5_base='anchor\nold\n'; f5_index='anchor\nstaged\n'
    f5_local='anchor\nstaged\nanchor\nprecious\n'
  fi
  f5_setup="printf '$f5_base' >f; git add f; git commit -qm f5-base; printf '$f5_index' >f; git add f; $f5_diff"
  wrun "$patch_repo" "$f5_setup" apply -R f5.patch
  wrun "$patch_repo" "$f5_setup; printf '$f5_local' >f" apply -R f5.patch
done
f5_setup="printf '*.txt text eol=crlf\\n' >.gitattributes; printf 'old\\r\\nanchor\\r\\n' >f.txt; git add .gitattributes f.txt; git commit -qm f5-crlf; printf 'staged\\r\\nanchor\\r\\n' >f.txt; git add f.txt; $f5_diff"
wrun "$patch_repo" "$f5_setup" apply -R f5.patch
wrun "$patch_repo" "$f5_setup" apply -R --index f5.patch
for content in empty text binary; do
  case "$content" in empty) bytes='' ;; text) bytes='old\n' ;; binary) bytes='old\000bytes\n' ;; esac
  f5_setup="printf '$bytes' >f; git add f; git commit -qm f5-deletion; git rm -q f; $f5_diff"
  wrun "$patch_repo" "$f5_setup" apply -R f5.patch
  wrun "$patch_repo" "$f5_setup; printf precious >f" apply -R f5.patch
done
for kind in mode binary delete; do
  f5_setup="mkdir -p d; chmod 700 d; printf 'old\\000bytes\\n' >d/f; git add d/f; git commit -qm f5-binary"
  case "$kind" in
    mode) f5_setup="$f5_setup; chmod +x d/f; git add d/f" ;;
    binary) f5_setup="$f5_setup; printf 'new\\000bytes\\n' >d/f; git add d/f" ;;
    delete) f5_setup="$f5_setup; git rm -q d/f; mkdir -p d; chmod 700 d" ;;
  esac
  wrun "$patch_repo" "$f5_setup; $f5_diff; printf kept >d/other" apply -R f5.patch
done
f5_setup="mkdir -p d; chmod 700 d; : >d/f; git add d/f; $f5_diff"
wrun "$patch_repo" "$f5_setup" apply -R f5.patch
wrun "$patch_repo" "$f5_setup; printf kept >d/other" apply -R f5.patch
# find_pos checks the higher line number first when distances are equal.
f5_setup="printf 'a\\nanchor\\nold\\nanchor\\nz\\n' >f; git add f; git commit -qm f5-search; printf 'a\\nanchor\\nstaged\\nanchor\\nz\\n' >f; git add f; $f5_diff; printf 'anchor\\nstaged\\nanchor\\nstaged\\nanchor\\n' >f"
wrun "$patch_repo" "$f5_setup" apply -R f5.patch
# Isolate the end constraint in a hunk which does not start at line one.
f5_setup="printf 'prefix\\npadding\\nanchor\\nold\\n' >f; git add f; git commit -qm f5-end; printf 'prefix\\npadding\\nanchor\\nstaged\\n' >f; git add f; $f5_diff"
wrun "$patch_repo" "$f5_setup; printf 'prefix\\npadding\\nanchor\\nstaged\\nanchor\\nprecious\\n' >f" apply -R f5.patch
# Reverse a nonempty addition with a sibling which must survive pruning.
f5_setup="mkdir -p d; chmod 700 d; printf 'added\\n' >d/f; git add d/f; $f5_diff; printf kept >d/other"
wrun "$patch_repo" "$f5_setup" apply -R f5.patch
f5_setup="printf 'old\\000bytes\\n' >f; git add f; git commit -qm f5-dirty-binary; printf 'new\\000bytes\\n' >f; git add f; $f5_diff; printf 'precious\\000bytes\\n' >f"
wrun "$patch_repo" "$f5_setup" apply -R f5.patch

whitespace_setup='printf "bad trailing   \n" >>alpha.txt; git diff >whitespace.patch; git checkout -q -- alpha.txt'
wrun "$patch_repo" "$whitespace_setup" apply --check whitespace.patch
wrun "$patch_repo" "$whitespace_setup" apply whitespace.patch
whitespace_all_setup='printf " \tindent\nbad1 \nbad2 \nbad3 \nbad4 \nbad5 \nbad6 \n\n\n" >>alpha.txt; git diff >whitespace-all.patch; git checkout -q -- alpha.txt'
wrun "$patch_repo" "$whitespace_all_setup" apply whitespace-all.patch

symlink_setup='git format-patch -1 --stdout HEAD~2 >series.patch; git reset -q --hard HEAD~3; mv alpha.txt outside; ln -s outside alpha.txt'
wrun "$patch_repo" "$symlink_setup" apply series.patch
untyped_symlink_setup='git reset -q --hard HEAD~3; mv alpha.txt outside; ln -s outside alpha.txt; printf "%b" "--- a/alpha.txt\n+++ b/alpha.txt\n@@ -1 +1 @@\n-one\n+ONE\n" >untyped.patch'
wrun "$patch_repo" "$untyped_symlink_setup" apply untyped.patch
admin_setup='printf "diff --git a/.git/audit-proof b/.git/audit-proof\nnew file mode 100644\n--- /dev/null\n+++ b/.git/audit-proof\n@@ -0,0 +1 @@\n+blocked\n" >admin.patch'
wrun "$patch_repo" "$admin_setup" apply admin.patch

# The first hunk inserts lines.  The second hunk's line number is still in
# original-file coordinates, and repeated context must not move it forward.
multihunk_setup=': >a; i=0; while [ $i -lt 10 ]; do echo unique$i >>a; i=$((i + 1)); done; r=0; while [ $r -lt 4 ]; do i=0; while [ $i -lt 10 ]; do echo repeat$i >>a; i=$((i + 1)); done; r=$((r + 1)); done; git add a; git commit -qm hunk-base; printf "diff --git a/a b/a\nindex 23efff4..9774ad7 100644\n--- a/a\n+++ b/a\n@@ -1,5 +1,15 @@\n unique0\n unique1\n+insert0\n+insert1\n+insert2\n+insert3\n+insert4\n+insert5\n+insert6\n+insert7\n+insert8\n+insert9\n unique2\n unique3\n unique4\n@@ -22,7 +32,7 @@ repeat0\n repeat1\n repeat2\n repeat3\n-repeat4\n+CHANGED\n repeat5\n repeat6\n repeat7\n" >input.patch'
wrun "$patch_repo" "$multihunk_setup" apply input.patch

# Apply a clean series through am, then preserve and control a conflict.
series_setup='git format-patch --stdout HEAD~3..HEAD >series.mbox; git reset -q --hard HEAD~3'
wrun "$patch_repo" "$series_setup" am series.mbox

conflict_setup='git format-patch -1 --stdout HEAD~2 >series.mbox; git reset -q --hard HEAD~3; printf "one\nmain\nthree\n" >alpha.txt; git add alpha.txt; git commit -qm main-conflict'
wrun "$patch_repo" "$conflict_setup" am series.mbox
for control in --abort --skip; do
  wsteps "$patch_repo" "$conflict_setup" "am conflict $control" \
    "g am series.mbox || [ \$? -eq 128 ]; g am $control"
done
wsteps "$patch_repo" "$conflict_setup" 'am conflict continue' \
  'g am series.mbox || [ $? -eq 128 ]; printf "one\nresolved\nthree\nfour\n" >alpha.txt; g add alpha.txt; g am --continue'
wsteps "$patch_repo" "$conflict_setup" 'am conflict continue without changes' \
  'g am series.mbox || [ $? -eq 128 ]; g am --continue'
wsteps "$patch_repo" "$conflict_setup" 'am refuses new mailbox while stopped' \
  'g am series.mbox || [ $? -eq 128 ]; g am series.mbox'

# A later conflict must leave the first applied commit recoverable by abort.
partial_abort_setup='printf "a-old\n" >am-a; printf "b-old\n" >am-b; git add am-a am-b; git commit -qm am-base; base=$(git rev-parse HEAD); printf "a-new\n" >am-a; git commit -qam first; printf "b-new\n" >am-b; git commit -qam second; git format-patch --stdout "$base..HEAD" >partial.mbox; git reset -q --hard "$base"; printf "conflict\n" >am-b; git commit -qam conflict'
wsteps "$patch_repo" "$partial_abort_setup" 'am partial series abort' \
  'g am partial.mbox || [ $? -eq 128 ]; g am --abort'

# am must never fold an unrelated staged change into the mailed commit.
dirty_index_setup='printf "mail-old\n" >mail-a; printf "private-old\n" >private-b; git add mail-a private-b; git commit -qm dirty-base; base=$(git rev-parse HEAD); printf "mail-new\n" >mail-a; git commit -qam mailed; git format-patch -1 --stdout >dirty.mbox; git reset -q --hard "$base"; printf "staged-private\n" >private-b; git add private-b'
wrun "$patch_repo" "$dirty_index_setup" am dirty.mbox
wsteps "$patch_repo" "$dirty_index_setup" 'am dirty index abort' \
  'g am dirty.mbox || [ $? -eq 128 ]; g am --abort'
wsteps "$patch_repo" "$conflict_setup" 'am moved head abort safety' \
  'g am series.mbox || [ $? -eq 128 ]; g commit --allow-empty -q -m moved; g am --abort'

WENV= WREFS= WSTDERR= WTREE= WMODES= WAFTER_REFRESH= WSEQUENCE=
