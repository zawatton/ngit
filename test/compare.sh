#!/bin/sh
# Compare ngit against git for one suite of commands; exit 0 iff all match.
# Usage: test/compare.sh SUITE   (SUITE: basic, rev, cat, all-objects,
#        deltas, worktree, ls-tree, log, index, attr-config, diff, status,
#        status-long, real, real-diff, write-objects, index-write,
#        write-tree, add, add-all, commit, commit-output, log-format,
#        log-walk, log-diff, log-path, log-graph, branch-list, branch-filter,
#        branch-create, branch-modify, branch-upstream, reset-index, tag-list,
#        tag-write, reset-hard, checkout, switch, worktree-overlay, show, modes,
#        rm, mv, restore, stash-push, stash-apply, unmerged, merge-file,
#        merge, cherry-pick, rebase, stash-merge, blame, grep, describe, pack-write,
#        gc, fsck, patch, misc, bisect, hooks, hooks-extra, clone, fetch, push,
#        remote, ssh)
# Fixtures are rebuilt into $NGIT_FIXTURE_DIR (default: $TMPDIR/ngit-fixtures)
# when missing.  The `real` and `real-diff` suites run against
# $NGIT_REAL_REPO.  git must not refresh the index while comparing, or
# stat-dirty entries would look clean to ngit afterwards.
# NGIT_TEST_MATCH selects cases by a literal substring of setup/arguments;
# no matching comparisons is a failure.  NGIT_TEST_TRACE shows case names,
# and NGIT_TEST_KEEP_OUTPUT retains the comparison artifacts.
set -u
case $1 in
  clone|fetch|push|remote|ssh) exec sh "$(dirname "$0")/transport.sh" "$1" ;;
esac
root=$(cd "$(dirname "$0")/.." && pwd)
ngit="$root/bin/ngit"
# Byte code makes ngit several times faster; without Emacs, sources run.
sh "$root/tools/build-elc.sh" || :
fx=${NGIT_FIXTURE_DIR:-${TMPDIR:-/tmp}/ngit-fixtures}
# Rebuild every fixture when a builder script changes; each builder only
# removes the repositories it creates.
stamp=$(cat "$root"/test/make-fixture.sh "$root"/test/make-wt-fixture.sh \
  "$root"/test/make-log-fixture.sh "$root"/test/make-unmerged-fixture.sh \
  "$root"/test/make-merge-fixture.sh "$root"/test/make-inspect-fixture.sh | cksum)
if [ "$1" != "merge-file" ] && [ "$(cat "$fx/.ngit-stamp" 2>/dev/null)" != "$stamp" ]; then
  mkdir -p "$fx"
  sh "$root/test/make-fixture.sh" "$fx" >/dev/null 2>&1 || {
    echo "fixture build failed"; exit 2; }
  sh "$root/test/make-wt-fixture.sh" "$fx" >/dev/null 2>&1 || {
    echo "work tree fixture build failed"; exit 2; }
  sh "$root/test/make-log-fixture.sh" "$fx" >/dev/null 2>&1 || {
    echo "log fixture build failed"; exit 2; }
  sh "$root/test/make-unmerged-fixture.sh" "$fx" >/dev/null 2>&1 || {
    echo "unmerged fixture build failed"; exit 2; }
  sh "$root/test/make-merge-fixture.sh" "$fx" >/dev/null 2>&1 || {
    echo "merge fixture build failed"; exit 2; }
  sh "$root/test/make-inspect-fixture.sh" "$fx" >/dev/null 2>&1 || {
    echo "inspection fixture build failed"; exit 2; }
  echo "$stamp" >"$fx/.ngit-stamp"
fi
export GIT_OPTIONAL_LOCKS=0
fail=0
checks=0
out=$(mktemp -d)
cleanup() {
  if [ -n "${NGIT_TEST_KEEP_OUTPUT:-}" ]; then
    echo "Comparison artifacts: $out"
  else
    rm -rf "$out"
  fi
}
trap cleanup EXIT

selected() {
  [ -n "${NGIT_TEST_MATCH:-}" ] || return 0
  case "$*" in *"$NGIT_TEST_MATCH"*) return 0 ;; *) return 1 ;; esac
}

check() { # check REPO ARGS...
  repo=$1; shift
  selected "$repo $*" || return 0
  checks=$((checks + 1))
  [ -z "${NGIT_TEST_TRACE:-}" ] || printf 'CHECK %s: %s\n' "$checks" "$*"
  git -C "$repo" "$@" >"$out/git" 2>/dev/null; gs=$?
  "$ngit" -C "$repo" "$@" >"$out/ngit" 2>"$out/err"; ns=$?
  if [ $gs -ne $ns ] || ! cmp -s "$out/git" "$out/ngit"; then
    echo "MISMATCH ($(basename "$repo")): git $* [git=$gs ngit=$ns]"
    diff "$out/git" "$out/ngit" | head -8
    head -3 "$out/err"
    fail=1
  fi
}

# Write commands run on a copy of a fixture: git and ngit each start from
# the same snapshot of .git (the work tree is shared and never written),
# and the resulting index bytes, refs, reflogs, loose objects, fsck
# report, stdout and exit status must be identical.
export GIT_AUTHOR_NAME="Test Author" GIT_AUTHOR_EMAIL=test@example.com
export GIT_COMMITTER_NAME="Test Author" GIT_COMMITTER_EMAIL=test@example.com
export GIT_AUTHOR_DATE="1700600000 +0900" GIT_COMMITTER_DATE="1700600000 +0900"
WENV= WREFS= WSTDERR= WTREE= WMODES= WAFTER_REFRESH= WSEQUENCE= WHOOKS=

wstate() { # wstate DIR TAG
  d=$1 t=$2
  if [ -f "$d/.git/index" ]; then cp "$d/.git/index" "$out/$t.index"; else : >"$out/$t.index"; fi
  if [ -n "$WTREE" ]; then
    # Ignore stat data, index encoding and extensions; retain every entry's
    # object, mode, stage, assume-valid and extended flags.
    python3 - "$out/$t.index" <<'PY'
import pathlib, struct, sys
p = pathlib.Path(sys.argv[1])
b = p.read_bytes()
records = []
if b:
    version, count = struct.unpack_from('>II', b, 4)
    off, prev = 12, b''
    for _ in range(count):
        start = off
        flags = struct.unpack_from('>H', b, off + 60)[0]
        off += 62
        ext = b[off:off + 2] if flags & 0x4000 else b'\0\0'
        off += 2 if flags & 0x4000 else 0
        if version == 4:
            drop = b[off] & 127
            while b[off] & 128:
                off += 1
                drop = ((drop + 1) << 7) + (b[off] & 127)
            off += 1
            end = b.index(0, off)
            name = prev[:len(prev) - drop] + b[off:end]
            off = end + 1
        else:
            end = b.index(0, off)
            name = b[off:end]
            off = start + ((end + 1 - start + 7) & ~7)
        records.append(b[start + 24:start + 28] + b[start + 40:start + 60]
                       + struct.pack('>H', flags & 0xb000) + ext + name + b'\0')
        prev = name
p.write_bytes(b''.join(records))
PY
  fi
  {
    git -C "$d" rev-parse -q --verify HEAD; git -C "$d" symbolic-ref -q HEAD
    for f in "$d/.git/logs/HEAD" "$d/.git/COMMIT_EDITMSG"; do
      [ -f "$f" ] && { echo "== $f"; cat "$f"; }
    done
    find "$d/.git/logs/refs" -type f 2>/dev/null | sort | while read -r f; do echo "== $f"; cat "$f"; done
    (cd "$d/.git" && find objects -type f ! -path '*/pack/*' ! -path '*/info/*' | sort | xargs -r md5sum)
    find "$d/.git" -maxdepth 3 -name '*.lock'
    if [ -n "$WMODES" ]; then
      echo '== modes'
      (cd "$d" && find . -path './.git/objects/pack' -prune -o -print0 |
        LC_ALL=C sort -z | xargs -0 -r stat -c '%a %n')
    fi
    git -C "$d" fsck --no-progress 2>&1
    if [ -n "$WTREE" ]; then
      python3 - "$d" <<'PY'
import hashlib, json, os, stat, sys
root = sys.argv[1]
def walk(base):
    for name in sorted(os.listdir(os.path.join(root, base))):
        if not base and name == '.git':
            continue
        rel = os.path.join(base, name)
        path = os.path.join(root, rel)
        mode = os.lstat(path).st_mode
        kind = 'link' if stat.S_ISLNK(mode) else 'dir' if stat.S_ISDIR(mode) else 'file'
        target = os.readlink(path) if kind == 'link' else None
        digest = hashlib.md5(open(path, 'rb').read()).hexdigest() if kind == 'file' else None
        print(json.dumps([rel, kind, stat.S_IMODE(mode), target, digest], ensure_ascii=True))
        if kind == 'dir':
            walk(rel)
walk('')
PY
      echo '== index entries'
      git -C "$d" ls-files -s
      echo '== diff-files (without refresh)'
      git -C "$d" diff-files --name-status
      echo '== status'
      git -C "$d" status --porcelain=v1
    fi
    if [ -n "$WREFS" ]; then
      for f in "$d/.git/config" "$d/.git/packed-refs" "$d/.git/ORIG_HEAD" \
               "$d/.git/MERGE_HEAD" "$d/.git/MERGE_MSG" "$d/.git/MERGE_MODE" \
               "$d/.git/SQUASH_MSG" "$d/.git/AUTO_MERGE" "$d/.git/CHERRY_PICK_HEAD" \
               "$d/.git/REVERT_HEAD" "$d/.git/REBASE_HEAD"; do
        [ -f "$f" ] && { echo "== $f"; cat "$f"; }
      done
      for f in "$d"/.git/BISECT_*; do
        [ ! -f "$f" ] || { echo "== $f"; cat "$f"; }
      done
      (cd "$d/.git" && find worktrees -print 2>/dev/null | sort)
      (cd "$d/.git" && find sequencer rebase-merge rebase-apply worktrees -type f ! -name index 2>/dev/null | sort |
        while IFS= read -r f; do echo "== $f"; cat "$f"; done)
      for f in "$d"/.git/worktrees/*/index; do
        if [ -f "$f" ]; then
          echo "== ${f%/index}/index entries"
          GIT_INDEX_FILE="$f" git -C "$d" ls-files -s
        fi
      done
      (cd "$d/.git" && find refs -type f | sort)
      git -C "$d" for-each-ref
    fi
    if [ -n "$WHOOKS" ]; then
      echo '== hook log'
      cat "$d/.git/hooks.log" 2>/dev/null || :
      if [ -d "$d/.git/receiver.git" ]; then
        echo '== receiver HEAD'
        cat "$d/.git/receiver.git/HEAD" 2>/dev/null || :
        echo '== receiver refs'
        git --git-dir="$d/.git/receiver.git" for-each-ref \
          --format='%(refname) %(objectname) %(symref)' | LC_ALL=C sort
      fi
    fi
  } >"$out/$t.state"
}

wcommand() {
  hook_env=
  if [ -n "$WHOOKS" ]; then
    hook_dir=$(git rev-parse --absolute-git-dir 2>/dev/null) || hook_dir="$PWD/.git"
    hook_env="NGIT_HOOK_LOG=$hook_dir/hooks.log"
  fi
  if [ -n "$WSEQUENCE" ]; then
    # A sequence runs each command through the implementation under test.
    # shellcheck disable=SC2086
    env $WENV $hook_env NGIT_TEST_COMMAND="$1" sh -c \
      'set -e; g() { "$NGIT_TEST_COMMAND" "$@"; }; '"$WSEQUENCE"
  else
    # shellcheck disable=SC2086
    env $WENV $hook_env "$@"
  fi
}

wgit() {
  # Command-line config overrides are themselves visible to config --list.
  if [ "${1:-}" = config ] || { [ -n "$WHOOKS" ] && [ "${1:-}" = gc ]; }; then
    wcommand git "$@"
  else
    wcommand git -c gc.auto=0 "$@"
  fi
}

wrun() { # wrun SRC SETUP ARGS...
  src=$1 setup=$2; shift 2
  selected "$src $setup $*" || return 0
  checks=$((checks + 1))
  [ -z "${NGIT_TEST_TRACE:-}" ] || printf 'WRITE %s: %s\n' "$checks" "$*"
  w="$out/w"; rm -rf "$w"; cp -a "$src" "$w"
  (cd "$w" && eval "$setup") >/dev/null 2>&1 || { echo "SETUP FAILED: $setup"; fail=1; return; }
  rm -rf "$out/snap"
  if [ -n "$WTREE" ]; then
    cp -a "$w" "$out/snap"
    # Both commands must see copied inodes, including when SETUP committed
    # and refreshed the original work tree's index stat data.
    rm -rf "$w"; cp -a "$out/snap" "$w"
  else
    cp -a "$w/.git" "$out/snap"
  fi
  # Restore clean entries' stat data after copying; otherwise inode reuse
  # can make one run clean and the other dirty.  Dirty entries and flags
  # remain intact.  The post-command state checks never refresh the index.
  if [ -n "$WTREE" ]; then
    git -C "$w" update-index -q --refresh >/dev/null 2>&1 || :
    [ -z "$WAFTER_REFRESH" ] || (cd "$w" && eval "$WAFTER_REFRESH")
  fi
  # shellcheck disable=SC2086
  if [ -n "$WSTDERR" ]; then
    # shellcheck disable=SC2086
    (cd "$w" && wgit "$@") >"$out/git.stdout" 2>"$out/git.stderr"
    gs=$?
    {
      echo '== stdout'; cat "$out/git.stdout"
      echo '== stderr'; cat "$out/git.stderr"
      echo "exit $gs"
    } >"$out/git.out"
  else
    # shellcheck disable=SC2086
    (cd "$w" && wgit "$@") >"$out/git.out" 2>/dev/null
    echo "exit $?" >>"$out/git.out"
  fi
  wstate "$w" git
  if [ -n "$WTREE" ]; then
    rm -rf "$w"; cp -a "$out/snap" "$w"
  else
    rm -rf "$w/.git"; cp -a "$out/snap" "$w/.git"
  fi
  if [ -n "$WTREE" ]; then
    git -C "$w" update-index -q --refresh >/dev/null 2>&1 || :
    [ -z "$WAFTER_REFRESH" ] || (cd "$w" && eval "$WAFTER_REFRESH")
  fi
  # shellcheck disable=SC2086
  if [ -n "$WSTDERR" ]; then
    # shellcheck disable=SC2086
    (cd "$w" && wcommand "$ngit" "$@") >"$out/ngit.stdout" 2>"$out/ngit.stderr"
    ns=$?
    {
      echo '== stdout'; cat "$out/ngit.stdout"
      echo '== stderr'; cat "$out/ngit.stderr"
      echo "exit $ns"
    } >"$out/ngit.out"
  else
    # shellcheck disable=SC2086
    (cd "$w" && wcommand "$ngit" "$@") >"$out/ngit.out" 2>"$out/err"
    echo "exit $?" >>"$out/ngit.out"
  fi
  wstate "$w" ngit
  if ! cmp -s "$out/git.out" "$out/ngit.out" || ! cmp -s "$out/git.state" "$out/ngit.state" ||
     ! cmp -s "$out/git.index" "$out/ngit.index"; then
    echo "MISMATCH ($(basename "$src")${setup:+; $setup}): $*"
    diff "$out/git.out" "$out/ngit.out" | head -10
    diff "$out/git.state" "$out/ngit.state" | head -10
    if [ -z "$WTREE" ] && ! cmp -s "$out/git.index" "$out/ngit.index"; then
      echo "index differs:"
      cp "$out/git.index" "$w/.git/index"; git -C "$w" ls-files -s --debug >"$out/git.lsd" 2>&1
      cp "$out/ngit.index" "$w/.git/index"; git -C "$w" ls-files -s --debug >"$out/ngit.lsd" 2>&1
      diff "$out/git.lsd" "$out/ngit.lsd" | head -16
    fi
    [ ! -f "$out/err" ] || head -3 "$out/err"; fail=1
  fi
}

wsteps() { # wsteps SRC SETUP LABEL SCRIPT
  WSEQUENCE=$4
  wrun "$1" "$2" "$3"
  WSEQUENCE=
}

checkout_fixture() {
  wt="$out/${1:-write-wt}"
  mkdir -p "$wt/dir" "$wt/back"
  (
    cd "$wt" || exit 1
    git init -q -b main
    git config gc.auto 0
    printf 'base\n' > a.txt
    printf 'one\n' > dir/one.txt
    printf 'two\n' > dir/two.txt
    printf '#!/bin/sh\necho base\n' > exec.sh; chmod +x exec.sh
    ln -s a.txt link
    printf 'auto\nlines\n' > auto.txt
    printf 'attr\nlines\n' > attr.txt
    printf 'attr.txt text eol=crlf\n' > .gitattributes
    printf 'file\n' > swap
    printf 'leaf\n' > back/leaf
    printf 'removed\n' > removed.txt
    git add -A; git commit -qm base
    git update-index --add --cacheinfo "160000,$(git rev-parse HEAD),module"
    git commit -qm 'add gitlink'; git tag v1.0
    printf 'third\n' > a.txt; printf 'third\n' > dir/one.txt
    rm dir/two.txt removed.txt swap back/leaf
    mkdir swap; printf 'new leaf\n' > swap/leaf
    rmdir back; printf 'file again\n' > back
    chmod -x exec.sh; rm link; ln -s dir/one.txt link
    printf 'head only\n' > new-head.txt
    git rm -q --cached module
    git add -A; git commit -qm third
    printf 'tip\n' > a.txt; printf 'tip\n' > dir/one.txt
    git add -A; git commit -qm tip
  ) >/dev/null 2>&1 || { echo 'checkout fixture build failed'; exit 2; }
}

R='git update-index -q --refresh'
NOREPO='rm -rf nested'

objects() { # all object names in REPO
  git -C "$1" cat-file --batch-all-objects --batch-check='%(objectname)'
}

batch_cat() { # cat every object of REPO with -t and -p in one ngit process
  repo=$1
  objects "$repo" | while read -r o; do
    git -C "$repo" cat-file -t "$o"; git -C "$repo" cat-file -p "$o"; done >"$out/git"
  objects "$repo" >"$out/list"
  NGIT_BATCH_LIST="$out/list" "$ngit" -C "$repo" test-cat-all >"$out/ngit" 2>"$out/err"
  if ! cmp -s "$out/git" "$out/ngit"; then
    echo "MISMATCH ($(basename "$repo")): cat-file -t/-p over all objects"
    diff "$out/git" "$out/ngit" | head -8; head -3 "$out/err"; fail=1
  fi
}

case $1 in
  hooks|hooks-extra) . "$root/test/hooks-suite.sh" ;;
  patch) . "$root/test/patch-suite.sh" ;;
  misc) . "$root/test/misc-suite.sh"; . "$root/test/misc-audit-suite.sh" ;;
  bisect) . "$root/test/bisect-suite.sh" ;;
  pack-write|gc|fsck)
    sh "$root/test/maintenance.sh" "$1" || fail=1 ;;
  merge-file)
    python3 "$root/test/merge-file-check.py" || fail=1
    sh "$root/test/merge-fuzz.sh" 30 || fail=1 ;;
  cherry-pick)
    WTREE=1 WREFS=1 WSTDERR=1
    export GIT_EDITOR=true
    sh "$root/test/make-sequencer-fixture.sh" "$fx" sequencer-cherry-pick || exit 2
    r=$fx/sequencer-cherry-pick
    for options in '' '-x' '-n' '--no-commit'; do
      wrun "$r" '' cherry-pick $options first
      wrun "$r" '' cherry-pick $options base..side
      if [ "$options" != '-x' ]; then wrun "$r" '' revert $options HEAD; fi
    done
    wrun "$r" '' cherry-pick first side
    wrun "$r" 'git switch -q symlink-base' cherry-pick symlink-next
    WAFTER_REFRESH='touch -d @1700000000 keep.txt'
    wrun "$r" '' cherry-pick first
    wrun "$r" '' revert HEAD
    WAFTER_REFRESH=
    wrun "$r" '' cherry-pick -m 1 merged
    wrun "$r" '' cherry-pick -m 2 merged
    wrun "$r" '' cherry-pick merged
    wrun "$r" '' revert -m 1 merged
    wrun "$r" 'git switch -q merged' revert -m 1 HEAD
    wrun "$r" 'git switch -q merged' revert -m 2 HEAD
    wrun "$r" 'git switch -q side' revert base..side
    wrun "$r" 'git switch -q side' revert -n base..side
    for action in cherry-pick revert; do
      for control in --abort --skip --quit --continue; do
        wrun "$r" '' "$action" "$control"
        wsteps "$r" '' "$action range $control" \
          "g $action base..side || [ \$? -eq 1 ]; g $action $control"
      done
    done
    for prefix in '' '-x' '-n'; do
      for control in --abort --skip --quit; do
        wsteps "$r" '' "cherry-pick $prefix range $control" \
          "g cherry-pick $prefix base..side || [ \$? -eq 1 ]; g cherry-pick $control"
      done
    done
    wsteps "$r" '' 'resolved single pick' \
      'g cherry-pick second || [ $? -eq 1 ]; printf "resolved\n" >text.txt; g add text.txt; g cherry-pick --continue'
    wsteps "$r" '' 'resolved range pick' \
      'g cherry-pick -x base..side || [ $? -eq 1 ]; printf "resolved\n" >text.txt; g add text.txt; g cherry-pick --continue'
    wsteps "$r" '' 'single pick abort preserves unrelated changes' \
      'printf "dirty\n" >>keep.txt; g cherry-pick second || [ $? -eq 1 ]; g cherry-pick --abort'
    wsteps "$r" '' 'range abort preserves unrelated changes' \
      'printf "dirty\n" >>keep.txt; g cherry-pick base..side || [ $? -eq 1 ]; g cherry-pick --abort'
    wsteps "$r" '' 'manual commit and continue range' \
      'g cherry-pick base..side || [ $? -eq 1 ]; printf "resolved\n" >text.txt; g add text.txt; g commit -m resolved; g cherry-pick --continue'
    wsteps "$r" '' 'empty resolved pick' \
      'g cherry-pick second || [ $? -eq 1 ]; g checkout HEAD -- text.txt; g cherry-pick --continue'
    for action in cherry-pick revert; do
      for control in --abort --skip --quit --continue; do
        wsteps "$r" '' "$action single conflict $control" \
          "g $action second || [ \$? -eq 1 ]; g $action $control"
      done
    done
    revert_setup='git switch -q side; printf "later\n" >text.txt; git add text.txt; git commit -qm later'
    for control in --abort --skip --quit; do
      wsteps "$r" "$revert_setup" "revert range conflict $control" \
        "g revert base..side || [ \$? -eq 1 ]; g revert $control"
    done
    wsteps "$r" "$revert_setup" 'resolved revert range' \
      'g revert base..side || [ $? -eq 1 ]; printf "resolved\n" >text.txt; g add text.txt; g revert --continue'
    ;;
  rebase)
    WTREE=1 WREFS=1 WSTDERR=1
    export GIT_EDITOR=true
    sh "$root/test/make-sequencer-fixture.sh" "$fx" sequencer-rebase || exit 2
    r=$fx/sequencer-rebase
    wrun "$r" '' rebase base
    wrun "$r" '' rebase clean
    wrun "$r" '' rebase side
    wrun "$r" 'git switch -q topic' rebase clean
    wrun "$r" '' rebase --onto clean base topic
    wrun "$r" 'git switch -q topic' rebase applied
    wrun "$r" 'git switch -qc behind base' rebase main
    wrun "$r" 'git switch -q --detach topic' rebase clean
    for control in --abort --skip --continue; do
      wrun "$r" '' rebase "$control"
      wsteps "$r" '' "rebase conflict $control" \
        "g rebase side || [ \$? -eq 1 ]; g rebase $control"
    done
    wsteps "$r" '' 'resolved rebase' \
      'g rebase side || [ $? -eq 1 ]; printf "resolved\n" >text.txt; g add text.txt; g rebase --continue'
    wsteps "$r" 'git switch -q side' 'resolved rebase range' \
      'g rebase main || [ $? -eq 1 ]; printf "resolved\n" >text.txt; g add text.txt; g rebase --continue'
    wrun "$r" '' rebase --onto clean base side
    wrun "$r" '' rebase --onto side base
    wsteps "$r" '' 'abort rebase after branch moved' \
      'g rebase side || [ $? -eq 1 ]; git branch -f main clean; g rebase --abort'
    ;;
  merge)
    WTREE=1 WREFS=1 WSTDERR=1
    WENV='GIT_MERGE_AUTOEDIT=no GIT_EDITOR=:'
    ff=$fx/merge-ff clean=$fx/merge-clean conflicts=$fx/merge-conflicts
    for options in '' '-q' '--ff' '--ff-only' '--stat' '-n' '--log' '--no-commit'; do
      # shellcheck disable=SC2086
      wrun "$ff" "" merge $options side
    done
    wrun "$ff" "" merge --no-ff side
    wrun "$ff" "" merge --no-ff -m 'merge subject' -m 'merge body' side
    wrun "$ff" "" merge --squash side
    wrun "$ff" "" merge --squash --no-commit side
    wrun "$ff" "" merge main
    wrun "$clean" "" merge base
    wrun "$clean" "" merge --ff-only side
    for options in '' '-q' '--ff' '--no-ff' '--stat' '-n' '--log'; do
      # shellcheck disable=SC2086
      wrun "$clean" "" merge $options side
    done
    wrun "$clean" "" merge -m 'custom subject' -m 'custom body' side
    wrun "$clean" "git branch -m topic" merge side
    side=$(git -C "$clean" rev-parse side)
    wrun "$clean" "" merge "$side"
    wrun "$clean" "" merge "$(printf '%s' "$side" | cut -c1-9)"
    wrun "$clean" "" merge --no-commit side
    wrun "$clean" "" merge --squash side
    wrun "$clean" "" merge --squash --no-ff side
    wrun "$clean" "" merge --squash --commit side
    wrun "$clean" "" merge --commit --squash side
    wrun "$clean" "" merge --squash --ff-only side
    wrun "$ff" "printf 'dirty\n' >> text.txt" merge side
    wrun "$ff" "printf 'staged\n' >> text.txt; git add text.txt" merge side
    wrun "$ff" "printf 'intent\n' > intent.txt; git add -N intent.txt" merge --no-ff side
    wrun "$ff" "mkdir new-dir; printf 'untracked\n' > new-dir/leaf.txt" merge side
    wrun "$ff" "mkdir new-dir; printf 'unrelated\n' > new-dir/unrelated.txt" merge side
    wrun "$ff" "printf 'untracked file\n' > new-dir" merge side
    wrun "$ff" "printf 'unrelated\n' >> keep.txt" merge side
    wrun "$clean" "printf 'dirty\n' >> text.txt" merge side
    wrun "$clean" "printf 'staged\n' >> keep.txt; git add keep.txt" merge side
    wrun "$clean" "printf 'unrelated\n' >> keep.txt" merge side
    wrun "$clean" "mkdir new-dir; printf 'untracked\n' > new-dir/leaf.txt" merge side
    wrun "$clean" "mkdir new-dir; printf 'unrelated\n' > new-dir/unrelated.txt" merge side
    wrun "$conflicts" "" merge side
    wrun "$conflicts" "" merge -q side
    wrun "$conflicts" "" merge --squash side
    wrun "$fx/merge-mode" "" merge side
    wrun "$fx/merge-df" "" merge side
    wrun "$fx/merge-df" "mkdir collision~HEAD; printf 'keep\n' > collision~HEAD/keep; git add collision~HEAD; git commit -qm occupied-directory" merge side
    wrun "$fx/merge-df-reverse" "" merge main
    wrun "$fx/merge-criss-cross" "" merge side
    wrun "$fx/merge-criss-cross" "" merge --no-commit side
    wrun "$clean" "" merge --abort
    wrun "$clean" "" merge --continue
    wsteps "$conflicts" "" 'merge and abort' \
      'g merge side || [ $? -eq 1 ]; g merge --abort'
    wsteps "$conflicts" "printf 'unrelated\n' >> keep.txt" 'dirty merge and abort' \
      'g merge side || [ $? -eq 1 ]; g merge --abort'
    wsteps "$conflicts" "git switch -q side; git rm -q keep.txt; git commit -qm delete-keep; git switch -q main" 'abort protects recreated untracked file' \
      'g merge side || [ $? -eq 1 ]; printf "precious\n" > keep.txt; g merge --abort'
    wsteps "$clean" "" 'no-commit merge and abort' \
      'g merge --no-commit side; g merge --abort'
    wsteps "$conflicts" "" 'continue unresolved merge' \
      'g merge side || [ $? -eq 1 ]; g merge --continue'
    wsteps "$conflicts" "" 'resolve merge and commit' \
      'g merge side || [ $? -eq 1 ]; printf "resolved\n" > content.txt; g add -A; g status; g commit -m resolved'
    wsteps "$conflicts" "" 'resolve merge and commit --no-edit' \
      'g merge side || [ $? -eq 1 ]; g add -A; g commit --no-edit'
    wsteps "$conflicts" "" 'resolve merge and continue' \
      'g merge side || [ $? -eq 1 ]; g add -A; g merge --continue'
    wsteps "$clean" "" 'no-commit merge and commit' \
      'g merge --no-commit side; g status; g commit --no-edit'
    wsteps "$clean" "" 'squash merge and commit' \
      'g merge --squash side; g commit --no-edit'
    WTREE= WREFS= WSTDERR= WENV= ;;
  stash-merge)
    WTREE=1 WREFS=1 WSTDERR=1
    r=$fx/stash-merge
    saved="sed 's/^two$/saved two/' text.txt > tmp; mv tmp text.txt; git stash push -qm saved"
    clean="$saved; sed 's/^eleven$/upstream eleven/' text.txt > tmp; mv tmp text.txt; git add text.txt; git commit -qm upstream"
    conflict="$saved; sed 's/^two$/upstream two/' text.txt > tmp; mv tmp text.txt; git add text.txt; git commit -qm upstream"
    for command in apply pop; do
      wrun "$r" "$clean" stash "$command"
      wrun "$r" "$clean" stash "$command" --index
      wrun "$r" "$clean" stash "$command" -q
      wrun "$r" "$conflict" stash "$command"
      wrun "$r" "$conflict" stash "$command" --index
      wrun "$r" "$conflict" stash "$command" -q
    done
    # A staged saved change and a disjoint upstream change exercise restoring
    # the saved index separately from merging the work tree.
    staged="sed 's/^two$/saved two/' text.txt > tmp; mv tmp text.txt; git add text.txt; sed 's/^four$/saved four/' text.txt > tmp; mv tmp text.txt; git stash push -qm saved; sed 's/^eleven$/upstream eleven/' text.txt > tmp; mv tmp text.txt; git add text.txt; git commit -qm upstream"
    wrun "$r" "$staged" stash apply
    wrun "$r" "$staged" stash apply --index
    repeated="printf 'a\nb\nc\nX\nd\ne\nf\na\nb\nc\nX\nd\ne\nf\ntail\n' > text.txt; git add text.txt; git commit -qm repeated; sed '11s/^X$/Y/' text.txt > tmp; mv tmp text.txt; git add text.txt; git stash push -qm repeated; sed 's/^tail$/new tail/' text.txt > tmp; mv tmp text.txt; git add text.txt; git commit -qm upstream"
    wrun "$r" "$repeated" stash apply --index
    wrun "$r" "$repeated" stash pop --index
    wrun "$r" "$saved; printf 'unrelated\n' >> keep.txt" stash apply
    wrun "$r" "$clean; printf 'dirty\n' >> text.txt" stash apply
    wrun "$r" "printf 'saved\000binary\n' > binary.bin; git stash push -qm saved; printf 'upstream\000binary\n' > binary.bin; git add binary.bin; git commit -qm upstream" stash apply
    wrun "$r" "printf 'saved\r\ncommon\r\nlast\r\n' > crlf.txt; git stash push -qm saved; printf 'upstream\r\ncommon\r\nlast\r\n' > crlf.txt; git add crlf.txt; git commit -qm upstream" stash apply
    wrun "$r" "printf 'saved\n' > delete.txt; git stash push -qm saved; git rm -q delete.txt; git commit -qm upstream" stash apply
    wrun "$r" "git rm -q delete.txt; git stash push -qm saved; printf 'upstream\n' > delete.txt; git add delete.txt; git commit -qm upstream" stash apply
    wrun "$r" "printf 'saved\n' > mode.txt; git stash push -qm saved; chmod +x mode.txt; git add mode.txt; git commit -qm executable" stash apply
    wrun "$r" "printf 'saved\n' > mode.txt; git stash push -qm saved; rm mode.txt; ln -s target mode.txt; git add mode.txt; git commit -qm symlink" stash apply
    wrun "$r" "printf 'saved file\n' > collision; git add collision; git stash push -qm saved; mkdir collision; printf 'upstream leaf\n' > collision/leaf; git add collision; git commit -qm directory" stash apply
    wrun "$r" "mkdir collision; printf 'saved leaf\n' > collision/leaf; git add collision; git stash push -qm saved; printf 'upstream file\n' > collision; git add collision; git commit -qm file" stash apply
    wsteps "$r" "$conflict" 'stash pop retains conflicts' \
      'g stash pop || [ $? -eq 1 ]; g stash list; g ls-files -u; g status -s'
    WTREE= WREFS= WSTDERR= ;;
  modes)
    for mask in 002 022; do
      (
        umask "$mask"
        fail=0
        WTREE=1 WREFS=1 WSTDERR=1 WMODES=1
        checkout_fixture "modes-$mask"
        wrun "$wt" "git checkout -q --orphan modes/root; rm .git/COMMIT_EDITMSG; rm -rf .git/logs" commit -m modes
        wrun "$wt" "chmod 600 .git/logs/HEAD .git/logs/refs/heads/main .git/COMMIT_EDITMSG" commit --allow-empty -m modes
        wrun "$wt" "" branch modes/deep/new HEAD~1
        wrun "$wt" "" branch -m main renamed/deep/main
        wrun "$wt" "chmod 600 .git/config .git/logs/refs/heads/main" branch -m main renamed/private
        wrun "$wt" "chmod 600 .git/config .git/logs/refs/heads/main" branch -c main copied/private
        wrun "$wt" "chmod 644 .git/config .git/logs/refs/heads/main" branch -m main renamed/old-mask
        wrun "$wt" "git config branch.main.description private; chmod 400 .git/config" branch -m main renamed/readonly
        wrun "$wt" "" tag -a modes/deep/tag -m modes
        wrun "$wt" "" reset --hard v1.0
        wrun "$wt" "git branch modes/other v1.0; chmod 600 .git/HEAD .git/index" switch modes/other
        wrun "$wt" "rm a.txt exec.sh; rm -rf dir" checkout -- a.txt exec.sh dir
        wrun "$wt" "mkdir -p fresh/deep; printf 'new file\n' > fresh/deep/file; rm .git/index" add fresh
        [ "$fail" -ne 0 ] || echo "modes: umask $mask PASS"
        exit "$fail"
      ) || fail=1
    done ;;
  basic)
    check "$fx/loose" cat-file -p HEAD
    check "$fx/loose" rev-parse HEAD ;;
  rev)
    for r in "$fx/loose" "$fx/packed"; do
      for rev in HEAD main side v1.0 light HEAD~1 HEAD~2 HEAD^2 HEAD^^2 \
                 HEAD~1^1 v1.0^{} v1.0^{commit} v1.0^{tree} HEAD^{tree} \
                 "HEAD:docs/long.txt" "HEAD:src" @ refs/heads/main \
                 HEAD~2^2 "HEAD~2^2^{tree}" HEAD~2^1~1; do
        check "$r" rev-parse "$rev"
      done
      check "$r" rev-parse --abbrev-ref HEAD
      check "$r" rev-parse --short HEAD
      check "$r" rev-parse --short=5 HEAD
      check "$r" rev-parse "$(git -C "$r" rev-parse HEAD | cut -c1-9)"
      check "$r" symbolic-ref HEAD
      check "$r" symbolic-ref --short HEAD
      check "$r" rev-parse nosuchref
    done ;;
  cat)
    for r in "$fx/loose" "$fx/packed"; do
      for o in HEAD HEAD^{tree} v1.0 HEAD:a.txt HEAD:link HEAD:src; do
        for m in -t -s -p; do check "$r" cat-file $m "$o"; done
      done
      check "$r" cat-file commit v1.0
      check "$r" cat-file -e HEAD
      check "$r" cat-file -e 0000000000000000000000000000000000000001
    done ;;
  all-objects) batch_cat "$fx/loose"; batch_cat "$fx/packed" ;;
  deltas)
    # The packed fixture must really contain delta entries, then read them.
    git -C "$fx/packed" verify-pack -v "$fx"/packed/.git/objects/pack/*.idx \
      | grep -q 'chain length = [1-9]' || { echo "no deltas in fixture"; exit 1; }
    batch_cat "$fx/packed" ;;
  worktree)
    check "$fx/wt" rev-parse HEAD
    check "$fx/wt" rev-parse --abbrev-ref HEAD
    check "$fx/wt" rev-parse main
    check "$fx/wt" rev-parse --show-toplevel
    check "$fx/wt" log
    check "$fx/wt" cat-file -p HEAD ;;
  ls-tree)
    for r in "$fx/loose" "$fx/packed"; do
      check "$r" ls-tree HEAD
      check "$r" ls-tree -r HEAD
      check "$r" ls-tree -r -t HEAD
      check "$r" ls-tree --name-only -r HEAD
      check "$r" ls-tree HEAD~3
    done ;;
  log)
    for r in "$fx/loose" "$fx/packed"; do
      check "$r" log
      check "$r" log -n 3
      check "$r" log -2
      check "$r" log --format=raw
      check "$r" log side
      check "$r" log main side
    done ;;
  real)
    r=${NGIT_REAL_REPO:?set NGIT_REAL_REPO}
    check "$r" rev-parse HEAD
    check "$r" log -n 30
    check "$r" log --format=raw -n 10
    check "$r" ls-tree HEAD
    check "$r" cat-file -p HEAD ;;
  index)
    for r in "$fx/loose" "$fx/packed" "$fx/dirty" "$fx/v4" "$fx/wt"; do
      check "$r" ls-files
      check "$r" ls-files -s
    done ;;
  unmerged)
    WREFS=1 WSTDERR=1 WTREE=1
    for r in "$fx/unmerged-v2" "$fx/unmerged-resolved"; do
      check "$r" ls-files
      check "$r" ls-files -s
      check "$r" ls-files -u
      check "$r" ls-files --unmerged
      check "$r" status
      check "$r" status -s
      check "$r" status --porcelain
      check "$r" diff
      check "$r" diff --cached
      check "$r" diff HEAD
      check "$r" diff --name-status
      check "$r" diff --cached --name-status
      check "$r" diff --name-only
      check "$r" diff --cached --name-only
      check "$r" diff -U0
      check "$r" diff -U1
      check "$r" diff --cached -U0
      check "$r" diff --cached -U1
    done
    # Exercise the other index encodings without multiplying the expensive
    # combined-diff matrix.
    check "$fx/unmerged-v3" ls-files -s
    check "$fx/unmerged-v4" ls-files -s
    # Outside a merge, long status uses the generic conflict-resolution hint.
    check "$fx/unmerged-no-merge" status
    # Keep MERGE_HEAD after resolving every path: long status must report that
    # the merge is still in progress even though no unmerged entries remain.
    wrun "$fx/unmerged-v2" "git add -A" status

    # The normalized WTREE comparison covers rejection without being sensitive
    # to index encoding or stat data.
    wrun "$fx/unmerged-v2" "" commit -m unresolved
    wrun "$fx/unmerged-resolved" "" commit -m unresolved
    wrun "$fx/unmerged-v2" "" write-tree
    wrun "$fx/unmerged-resolved" "" write-tree

    # Rewrites and a one-path resolution must preserve the actual index bytes,
    # including versions 2, 3 and 4.  Disable WTREE normalization temporarily.
    WTREE=
    for r in "$fx/unmerged-v2" "$fx/unmerged-v3" "$fx/unmerged-v4"; do
      wrun "$r" "" update-index --force-write-index
      wrun "$r" "" add -v nested/uu.txt
      wrun "$r" "" add -N nested/uu.txt
      wrun "$r" "" add nested/reuc.txt
    done
    WTREE=1
    WREFS= WSTDERR= WTREE= ;;
  attr-config)
    r=$fx/dirty
    for k in core.autocrlf core.filemode remote.origin.url branch.main.merge \
             submodule.sub1.url user.name nosuch.key; do
      check "$r" config --get "$k"
    done
    check "$r" check-attr text eol diff merge binary -- bin.dat crlf.txt eol.crlf \
      a.txt sub/x.txt sub/deep/y.txt "日本語ディレクトリ/ファイル.txt" ;;
  diff)
    for r in "$fx/dirty" "$fx/v4"; do
      check "$r" diff
      check "$r" diff --cached
      check "$r" diff HEAD
      check "$r" diff --name-only
      check "$r" diff --name-status HEAD
      check "$r" diff --cached --name-status
      check "$r" diff -U1
      check "$r" diff -U0 HEAD
      check "$r" diff --no-renames --cached
    done
    for r in "$fx/loose" "$fx/packed"; do
      check "$r" diff HEAD~2 HEAD
      check "$r" diff main side
      check "$r" diff v1.0 HEAD
      check "$r" diff HEAD~5 HEAD
      check "$r" diff --name-status HEAD~5 HEAD
      check "$r" diff -U5 HEAD~4..HEAD
    done
    check "$fx/dirty" diff HEAD~2 HEAD
    check "$fx/dirty" diff origin/main HEAD ;;
  status)
    for r in "$fx/dirty" "$fx/v4" "$fx/loose" "$fx/wt" "$fx/st-empty" "$fx/st-ahead"; do
      check "$r" status --porcelain
      check "$r" status -s
    done ;;
  status-long)
    for r in "$fx"/st-* "$fx/dirty" "$fx/v4" "$fx/loose" "$fx/wt"; do
      check "$r" status
    done ;;
  real-diff)
    r=${NGIT_REAL_REPO:?set NGIT_REAL_REPO}
    check "$r" diff HEAD~3 HEAD
    check "$r" diff --cached
    check "$r" diff ;;
  write-objects)
    wrun "$fx/dirty" "" hash-object a.txt code.c bin.dat crlf.txt "space name.txt" link
    wrun "$fx/dirty" "echo fresh object > f.txt" hash-object -w f.txt
    wrun "$fx/dirty" "head -c 300000 /dev/urandom > big.bin; seq 1 50000 > big.txt" hash-object -w big.bin big.txt
    wrun "$fx/dirty" "git config core.autocrlf input" hash-object -w crlf.txt eol.crlf
    wrun "$fx/dirty" "git config core.looseCompression 9; seq 1 2000 > s.txt" hash-object -w s.txt
    wrun "$fx/dirty" "git config core.autocrlf input" hash-object -w --no-filters crlf.txt ;;
  index-write)
    for r in "$fx/dirty" "$fx/v4" "$fx/loose" "$fx/st-empty" "$fx/st-staged-only"; do
      wrun "$r" "" update-index --force-write-index
      wrun "$r" "$R" update-index --force-write-index
    done
    # Racily clean: same size and second as the index, different content.
    wrun "$fx/st-uptodate" "printf 'aaaa\n' > a.txt && git add a.txt && printf 'bbbb\n' > a.txt" \
      update-index --force-write-index
    # Racy entries whose mode changed are stat-dirty, not smudged.
    wrun "$fx/st-uptodate" "printf 'aaaa\\n' > a.txt && git add a.txt && chmod +x a.txt" \
      update-index --force-write-index ;;
  write-tree)
    for r in "$fx/dirty" "$fx/v4" "$fx/loose" "$fx/st-empty" "$fx/st-staged-only" "$fx/st-uptodate"; do
      wrun "$r" "" write-tree
    done
    wrun "$fx/st-uptodate" "mkdir -p n/m && echo x > n/m/x && git add n && git rm -q --cached b.txt" write-tree
    wrun "$fx/st-uptodate" "echo y > y && git add -N y && git rm -q -r --cached docs" write-tree
    wrun "$fx/dirty" "git rm -q -r --cached sub1 docs" write-tree ;;
  add)
    for s in "$NOREPO" "$NOREPO; $R"; do
      wrun "$fx/dirty" "$s" add a.txt
      wrun "$fx/dirty" "$s" add -v new-then-mod.txt b.txt
      wrun "$fx/dirty" "$s" add -v docs
      wrun "$fx/dirty" "$s" add -v link src/run.sh src
      wrun "$fx/dirty" "$s" add -v crlf.txt eol.crlf bin.dat code.c
      wrun "$fx/dirty" "$s" add -v ita.txt
      wrun "$fx/dirty" "$s" add -v sub1 sub2
      wrun "$fx/dirty" "$s" add -n -v .
      wrun "$fx/dirty" "$s" add -v "日本語ディレクトリ" "space name.txt" newdir
      wrun "$fx/dirty" "$s" add -N untracked.txt "untracked space.txt"
      wrun "$fx/dirty" "$s" add -v docs/x keep.log
    done
    wrun "$fx/dirty" "$NOREPO; git config core.autocrlf input" add -v crlf.txt
    wrun "$fx/dirty" "$NOREPO; git config core.filemode false" add -v docs
    wrun "$fx/v4" "$R" add -v a.txt extra.txt docs
    wrun "$fx/st-uptodate" "printf 'aaaa\n' > a.txt && git add a.txt && printf 'bbbb\n' > a.txt" add -v b.txt
    wrun "$fx/st-uptodate" "printf 'aaaa\n' > a.txt && git add a.txt && printf 'bbbb\n' > a.txt" add -v a.txt
    wrun "$fx/st-uptodate" "rm -r src && echo f > src" add -v src ;;
  add-all)
    for s in "$NOREPO" "$NOREPO; $R"; do
      wrun "$fx/dirty" "$s" add -A
      wrun "$fx/dirty" "$s" add -v -A
      wrun "$fx/dirty" "$s" add -u
      wrun "$fx/dirty" "$s" add -v -u
      wrun "$fx/dirty" "$s" add -v .
      wrun "$fx/dirty" "$s" add -v -u docs src
    done
    wrun "$fx/dirty" "git -C nested add x.txt && git -C nested commit -q -m n" add -v -A
    wrun "$fx/dirty" "" add -A
    wrun "$fx/dirty" "$NOREPO" add -v foo.log
    wrun "$fx/dirty" "$NOREPO" add -v build/out.txt
    wrun "$fx/dirty" "$NOREPO" add -v build
    wrun "$fx/dirty" "$NOREPO" add -v foo.log a.txt
    wrun "$fx/dirty" "$NOREPO" add -f -v foo.log build
    wrun "$fx/dirty" "$NOREPO" add nope a.txt
    wrun "$fx/dirty" "$NOREPO" add
    wrun "$fx/v4" "" add -A
    wrun "$fx/st-empty" "" add -A
    wrun "$fx/st-empty-clean" "echo x > x && mkdir d && echo y > d/y" add -v . ;;
  commit)
    wrun "$fx/st-staged-only" "" commit -m "staged only"
    wrun "$fx/st-staged-only" "" commit -q -m quiet
    wrun "$fx/st-empty" "" commit -m "first commit"
    wrun "$fx/st-empty-clean" "" commit --allow-empty -m "empty root"
    wrun "$fx/st-uptodate" "" commit --allow-empty -m "nothing changed"
    wrun "$fx/st-detached-at" "echo d > d.txt && git add d.txt" commit -m "on detached head"
    wrun "$fx/st-uptodate" "git checkout -q --orphan fresh" commit -m "orphan branch"
    wrun "$fx/st-staged-only" "" commit -m "  subject line  " -m "" -m "$(printf 'body\n\n\n\nmore  \n\n')"
    wrun "$fx/st-staged-only" "" commit -m "日本語の件名" -m "本文"
    wrun "$fx/st-staged-only" "printf '\n\nfrom file\n\n  x\n' > ../msg.txt" commit -F ../msg.txt
    wrun "$fx/st-staged-only" "" commit -m "$(printf 'two\nline subject\n\nbody')"
    wrun "$fx/dirty" "$NOREPO" commit -m "dirty index"
    wrun "$fx/v4" "" commit -m "v4 index"
    wrun "$fx/st-staged-only" "$R" commit -m refreshed
    wrun "$fx/st-staged-only" "touch -d 2002-02-02 b.txt" commit -m "stat refresh" ;;
  reset-index)
    WREFS=1 WSTDERR=1
    for mode in --soft --mixed; do
      for rev in HEAD~1 HEAD~2 old v1.0; do
        wrun "$fx/hist" "" reset "$mode" "$rev"
      done
      wrun "$fx/st-detached-at" "" reset "$mode" HEAD~1
      wrun "$fx/hist" "git reset -q --soft HEAD~1" reset "$mode" HEAD~1
    done
    wrun "$fx/st-staged-only" "" reset
    wrun "$fx/st-staged-only" "" reset --soft
    wrun "$fx/dirty" "$NOREPO" reset --mixed HEAD~1
    wrun "$fx/v4" "" reset --mixed HEAD~1
    wrun "$fx/st-staged-only" "touch -d 2002-02-02 b.txt" reset
    wrun "$fx/hist" "" reset -q HEAD~2
    wrun "$fx/hist" "" reset --soft -q HEAD~2
    wrun "$fx/hist" "echo changed >> src/main.c; git add src/main.c; echo unstaged >> doc/one.txt; rm w.txt" reset
    wrun "$fx/hist" "echo changed >> src/main.c; git add src/main.c; echo unstaged >> doc/one.txt; rm w.txt" reset -q
    wrun "$fx/hist" "echo changed >> src/main.c; git add src/main.c" reset src/main.c
    wrun "$fx/hist" "echo changed >> src/main.c; git add src/main.c" reset -- src/main.c
    wrun "$fx/hist" "echo changed >> src/main.c; git add src/main.c; git update-index --skip-worktree src/main.c" reset -- src/main.c
    wrun "$fx/hist" "" reset v1.0 -- src
    wrun "$fx/hist" "" reset v1.0 src doc
    wrun "$fx/hist" "" reset HEAD~2 -- w.txt
    wrun "$fx/hist" "git rm -q src/main.c" reset -- src/main.c
    wrun "$fx/hist" "echo new > new.txt; git add new.txt" reset -- new.txt
    wrun "$fx/hist" "echo new > new.txt; git add -N new.txt" reset -- new.txt
    wrun "$fx/hist" "echo new > new.txt; git add -N new.txt" reset -- src
    wrun "$fx/hist" "echo new > new.txt; git add new.txt" reset -- '*.txt'
    wrun "$fx/hist" "ln -s missing broken" reset broken
    wrun "$fx/hist" "git rm -q -r doc; echo blocker > doc; git add doc; git commit -q -m replace" reset HEAD~1 -- doc/one.txt
    wrun "$fx/hist" "mv doc elsewhere; ln -s elsewhere doc" reset -- src
    wrun "$fx/hist" "" reset --mixed HEAD~1 -- w.txt
    wrun "$fx/hist" "" reset -q v1.0 -- src
    wrun "$fx/hist" "" reset -- nonexistent
    wrun "$fx/st-empty" "" reset
    wrun "$fx/st-empty-clean" "" reset --soft
    wrun "$fx/hist" "git symbolic-ref HEAD refs/heads/unborn; git rev-parse main > .git/ORIG_HEAD" reset --soft main
    wrun "$fx/hist" "git rev-parse HEAD > .git/MERGE_HEAD; echo merge > .git/MERGE_MSG" reset
    wrun "$fx/hist" "git rev-parse HEAD > .git/MERGE_HEAD; echo merge > .git/MERGE_MSG" reset --soft
    wrun "$fx/hist" "git rev-parse HEAD > .git/CHERRY_PICK_HEAD" reset --soft HEAD~1
    wrun "$fx/hist" "" reset no-such-revision
    wrun "$fx/hist" "" reset no-such-revision --
    wrun "$fx/hist" "" reset no-such-revision -- src/main.c
    wrun "$fx/hist" "" reset --soft -- src/main.c
    wrun "$fx/hist" "" reset --soft HEAD~1 -- src
    wrun "$fx/st-staged-only" "" reset -- ''
    wrun "$fx/hist" "git config core.bare true" reset --mixed HEAD~1
    wrun "$fx/hist" "git config core.bare true" reset --soft HEAD~1
    WENV="GIT_REFLOG_ACTION=custom"
    wrun "$fx/hist" "" reset --soft HEAD~1
    WENV=
    # These modes change the work tree, so both runs start from a copied tree.
    WTREE=1
    for option in --merge --keep; do
      wrun "$fx/hist" "" reset "$option" HEAD~1
      wrun "$fx/hist" "printf 'local\\n' >> a.txt" reset "$option" HEAD~1
      wrun "$fx/hist" "printf 'staged\\n' >> a.txt; git add a.txt" reset "$option" HEAD~1
    done
    WTREE=
    # The patch mode deliberately differs from git; verify rejection
    # and that no index, ref, reflog or ORIG_HEAD changes occurred.
    for option in -p; do
      w="$out/w"; rm -rf "$w"; cp -a "$fx/hist" "$w"
      wstate "$w" git
      "$ngit" -C "$w" reset "$option" >"$out/reject.out" 2>&1; ns=$?
      printf 'fatal: reset %s is not supported\n' "$option" >"$out/reject.want"
      wstate "$w" ngit
      if [ "$ns" -ne 128 ] || ! cmp -s "$out/reject.out" "$out/reject.want" ||
         ! cmp -s "$out/git.state" "$out/ngit.state" || ! cmp -s "$out/git.index" "$out/ngit.index"; then
        echo "MISMATCH: reset $option rejection"; fail=1
      fi
    done
    WREFS= WSTDERR= ;;
  stash-push)
    WTREE=1 WREFS=1 WSTDERR=1
    checkout_fixture
    dirty="printf 'staged\n' > a.txt; git add a.txt; printf 'unstaged\n' >> a.txt; rm dir/one.txt; printf 'added\n' > added.txt; git add added.txt; printf 'keep\n' > untracked.txt; chmod +x exec.sh; rm link; ln -s missing link"
    wrun "$wt" "" stash
    wrun "$wt" "" stash push -q
    wrun "$wt" "$dirty" stash
    wrun "$wt" "$dirty" stash push -m 'save this work'
    wrun "$wt" "$dirty" stash push --keep-index
    wrun "$wt" "$dirty" stash push --include-untracked
    wrun "$wt" "$dirty; mkdir -p extra/deep; printf 'new\n' > extra/deep/file; printf '*.ignored\n' > .git/info/exclude; printf 'ignored\n' > x.ignored" stash push -u -k -q
    wrun "$wt" "$dirty" stash push -- a.txt
    wrun "$wt" "$dirty" stash push -k -- dir a.txt
    wrun "$wt" "$dirty" stash push -u -- untracked.txt a.txt
    wrun "$wt" "$dirty" stash push -- '*.txt'
    wrun "$wt" "$dirty" stash push -- .
    wrun "$wt" "printf 'staged\n' > a.txt; git add a.txt; git show HEAD:a.txt > a.txt" stash push
    wrun "$wt" "$dirty; git update-index --index-version 4" stash push -u
    wrun "$wt" "printf 'outside\n' >> auto.txt" stash push -- a.txt
    wrun "$wt" "git config core.logAllRefUpdates false; $dirty" stash push
    wrun "$wt" "git switch -q --detach; $dirty" stash push
    wrun "$wt" "printf 'only untracked\n' > untracked.txt" stash push -u
    wrun "$wt" "printf 'first\n' >> a.txt; git stash push -qm first; $dirty" stash push -m second
    wrun "$wt" "$dirty" stash push -- missing
    WTREE= WREFS= WSTDERR= ;;
  stash-apply)
    WTREE=1 WREFS=1 WSTDERR=1
    checkout_fixture
    stash_setup="printf 'staged\n' > a.txt; git add a.txt; printf 'unstaged\n' >> a.txt; rm dir/one.txt; printf 'added\n' > added.txt; git add added.txt; chmod +x exec.sh; rm link; ln -s missing link; printf 'loose\n' > untracked.txt; git stash push -qu -m saved"
    multi="$stash_setup; printf 'second\n' >> dir/one.txt; git stash push -qm second; printf 'third\n' >> auto.txt; git stash push -qm third"
    wrun "$wt" "$multi" stash list
    wrun "$wt" "$stash_setup" stash show
    wrun "$wt" "$stash_setup" stash show -p
    wrun "$wt" "$multi" stash show --stat 'stash@{1}'
    wrun "$wt" "$stash_setup" stash apply
    wrun "$wt" "$stash_setup" stash apply --index
    wrun "$wt" "$stash_setup" stash apply -q
    wrun "$wt" "$stash_setup; printf 'upstream\n' >> auto.txt; git add auto.txt; git commit -qm upstream" stash apply
    wrun "$wt" "$stash_setup; printf 'local\n' >> auto.txt; git add auto.txt" stash apply
    wrun "$wt" "$stash_setup; printf 'local\n' >> a.txt" stash apply
    wrun "$wt" "$stash_setup; printf 'blocker\n' > added.txt" stash apply
    wrun "$wt" "$stash_setup; printf 'blocker\n' > untracked.txt" stash apply
    wrun "$wt" "$stash_setup; printf 'local\n' >> a.txt" stash pop
    wrun "$wt" "$stash_setup; printf 'local\n' >> a.txt" stash apply --index
    wrun "$wt" "$stash_setup; printf 'upstream\n' >> auto.txt; git add auto.txt; git commit -qm upstream" stash apply --index
    wrun "$wt" "$stash_setup; git update-index --index-version 4" stash apply --index
    wrun "$wt" "printf 'staged\n' > a.txt; git add a.txt; git show HEAD:a.txt > a.txt; git stash push -q" stash apply --index
    [ "$(git -C "$w" write-tree)" = "$(git -C "$w" rev-parse 'stash^2^{tree}')" ] || {
      echo 'MISMATCH: stash apply --index cache tree'; fail=1; }
    wrun "$wt" "mkdir -p added-dir; printf 'staged\n' > added-dir/new; git add added-dir; git stash push -q; mkdir -p added-dir; printf 'keep\n' > added-dir/keep" stash apply
    wrun "$wt" "rm a.txt; mkdir a.txt; printf 'child\n' > a.txt/child; git add -A; git stash push -q" stash apply
    wrun "$wt" "rm -rf dir; printf 'file\n' > dir; git add -A; git stash push -q" stash apply
    wrun "$wt" "printf 'stashed\n' >> a.txt; git stash push -q; chmod +x a.txt; git add a.txt; git commit -qm executable" stash apply
    wrun "$wt" "chmod +x a.txt; git stash push -q; printf 'upstream\n' >> a.txt; git add a.txt; git commit -qm content" stash apply
    wrun "$wt" "$stash_setup" stash pop
    wrun "$wt" "$multi" stash pop -q 'stash@{1}'
    wrun "$wt" "$multi" stash drop
    wrun "$wt" "$multi" stash drop 'stash@{1}'
    wrun "$wt" "$multi" stash drop -q 2
    wrun "$wt" "$stash_setup" stash drop
    wrun "$wt" "$multi" stash clear
    wrun "$wt" "" stash clear
    wrun "$wt" "$stash_setup" stash branch stash-branch
    wrun "$wt" "$multi" stash branch stash-branch 'stash@{1}'
    wrun "$wt" "$stash_setup; printf 'blocker\n' > untracked.txt" stash branch stash-branch
    WSTDERR=
    wrun "$wt" "$multi; touch .git/refs/stash.lock" stash drop
    wrun "$wt" "$stash_setup; touch .git/refs/stash.lock" stash drop
    wrun "$wt" "$multi; touch .git/refs/stash.lock" stash clear
    WSTDERR=1
    wrun "$wt" "" stash list
    wrun "$wt" "" stash apply
    WTREE= WREFS= WSTDERR= ;;
  reset-hard)
    WTREE=1 WREFS=1 WSTDERR=1
    checkout_fixture
    dirty="printf 'staged\n' > a.txt; git add a.txt; printf 'unstaged\n' >> a.txt; rm dir/one.txt; printf 'added\n' > added.txt; git add added.txt; printf 'keep\n' > untracked.txt; chmod +x exec.sh; rm link; ln -s missing link"
    for rev in HEAD~1 HEAD~2 v1.0; do
      wrun "$wt" "$dirty" reset --hard "$rev"
    done
    wrun "$wt" "$dirty" reset --hard
    wrun "$wt" "$dirty" reset --hard -q HEAD~2
    wrun "$wt" "git reset -q --hard v1.0" reset --hard main
    wrun "$wt" "git config core.autocrlf true; rm auto.txt attr.txt" reset --hard
    wrun "$wt" "rm attr.txt" reset --hard
    wrun "$wt" "git config core.eol crlf; printf 'auto.txt text\n' >> .gitattributes; git add .gitattributes; git commit -qm eol; rm auto.txt" reset --hard
    wrun "$wt" "git update-index --skip-worktree a.txt; printf 'dirty\n' > a.txt" reset --hard
    wrun "$wt" "git update-index --skip-worktree a.txt; printf 'dirty\n' > a.txt" reset --hard HEAD~2
    wrun "$wt" "git update-index --assume-unchanged a.txt; printf 'dirty\n' > a.txt" reset --hard
    wrun "$wt" "git update-index --skip-worktree exec.sh; chmod +x exec.sh" reset --hard HEAD~2
    wrun "$wt" "git update-index --skip-worktree dir/one.txt; rm -rf dir; printf 'blocker\n' > dir" reset --hard HEAD~2
    WSTDERR=
    wrun "$wt" "$dirty; touch .git/index.lock" reset --hard HEAD~2
    WSTDERR=1
    wrun "$wt" "mkdir -p gone/deep; printf 'tracked\n' > gone/deep/file; git add gone; printf 'keep\n' > gone/untracked" reset --hard
    wrun "$wt" "git config core.filemode false; chmod +x exec.sh; rm link; git config core.symlinks false" reset --hard
    wrun "$wt" "git update-index --index-version 4; $dirty" reset --hard HEAD~2
    wrun "$wt" "" reset --hard -- a.txt
    WTREE= WREFS= WSTDERR= ;;
  rm)
    WTREE=1 WREFS=1 WSTDERR=1
    checkout_fixture
    wrun "$wt" "" rm a.txt
    wrun "$wt" "" rm --cached a.txt
    wrun "$wt" "" rm -r dir
    wrun "$wt" "mkdir dir-extra; printf 'sibling\n' > dir-extra/file; git add dir-extra; git commit -qm sibling" rm -r dir
    wrun "$wt" "" rm -rn dir
    wrun "$wt" "" rm -rq dir
    wrun "$wt" "" rm .
    wrun "$wt" "" rm dir
    wrun "$wt" "" rm nonexistent
    wrun "$wt" "" rm --ignore-unmatch nonexistent
    wrun "$wt" "" rm --ignore-unmatch a.txt nonexistent
    wrun "$wt" "printf 'local\n' > a.txt" rm a.txt
    wrun "$wt" "printf 'local\n' > a.txt" rm --cached a.txt
    wrun "$wt" "printf 'local\n' > a.txt" rm -f a.txt
    wrun "$wt" "printf 'staged\n' > a.txt; git add a.txt" rm a.txt
    wrun "$wt" "printf 'staged\n' > a.txt; git add a.txt" rm --cached a.txt
    wrun "$wt" "printf 'staged\n' > a.txt; git add a.txt; rm a.txt" rm a.txt
    wrun "$wt" "printf 'staged\n' > a.txt; git add a.txt; rm a.txt; mkdir a.txt" rm a.txt
    wrun "$wt" "printf 'staged\n' > a.txt; git add a.txt; printf 'local\n' > a.txt" rm a.txt
    wrun "$wt" "printf 'staged\n' > a.txt; git add a.txt; printf 'local\n' > a.txt" rm --cached a.txt
    wrun "$wt" "printf 'staged\n' > a.txt; git add a.txt; printf 'local\n' > a.txt" rm -f a.txt
    wrun "$wt" "git rm -q --cached a.txt; git add -N a.txt; printf 'local\n' > a.txt" rm --cached a.txt
    wrun "$wt" "rm a.txt" rm a.txt
    wrun "$wt" "rm a.txt; mkdir a.txt; printf 'keep\n' > a.txt/untracked" rm a.txt
    wrun "$wt" "rm a.txt; mkdir a.txt" rm a.txt exec.sh
    wrun "$wt" "rm a.txt; mkdir a.txt; printf 'keep\n' > a.txt/untracked" rm a.txt exec.sh
    wrun "$wt" "git config core.filemode false; chmod +x a.txt" rm a.txt
    wrun "$wt" "git config core.autocrlf true" rm a.txt
    wrun "$wt" "printf 'odd\tname\n' > 'odd	name'; git add 'odd	name'; git commit -qm odd" rm 'odd	name'
    wrun "$wt" "git rm -q --cached a.txt; b=\$(git rev-parse HEAD~2:a.txt); o=\$(git rev-parse HEAD~1:a.txt); t=\$(git rev-parse HEAD:a.txt); printf '100644 %s 1\ta.txt\n100644 %s 2\ta.txt\n100644 %s 3\ta.txt\n' \"\$b\" \"\$o\" \"\$t\" | git update-index --index-info" rm -f a.txt
    wrun "$wt" "" rm -r -- 'dir/*.txt' a.txt
    wrun "$wt" "" rm -r a.txt dir/one.txt
    WTREE= WREFS= WSTDERR= ;;
  mv)
    WTREE=1 WREFS=1 WSTDERR=1
    checkout_fixture
    wrun "$wt" "" mv a.txt renamed.txt
    wrun "$wt" "printf 'dirty\n' > a.txt" mv a.txt much-longer-name.txt
    wrun "$wt" "git update-index --assume-unchanged a.txt" mv a.txt assumed-name.txt
    wrun "$wt" "mkdir target" mv a.txt exec.sh target
    wrun "$wt" "mkdir target" mv -v a.txt exec.sh target
    wrun "$wt" "mkdir target" mv -n a.txt exec.sh target
    wrun "$wt" "mkdir target" mv -n dir a.txt target
    wrun "$wt" "mkdir target" mv -v dir a.txt target
    wrun "$wt" "" mv dir renamed-dir
    wrun "$wt" "printf 'untracked\n' > dir/keep" mv dir renamed-dir
    wrun "$wt" "" mv -v dir renamed-dir
    wrun "$wt" "" mv -n dir renamed-dir
    wrun "$wt" "" mv dir renamed-dir/
    wrun "$wt" "git config core.ignorecase true" mv a.txt A.txt
    wrun "$wt" "printf 'untracked\n' > untracked" mv untracked moved
    wrun "$wt" "" mv missing moved
    wrun "$wt" "mkdir empty" mv empty moved
    wrun "$wt" "printf 'existing\n' > existing" mv a.txt existing
    wrun "$wt" "printf 'existing\n' > existing" mv -f a.txt existing
    wrun "$wt" "printf 'existing\n' > existing" mv -fv a.txt existing
    wrun "$wt" "mkdir existing" mv -f a.txt existing
    wrun "$wt" "" mv a.txt missing/child
    wrun "$wt" "" mv -k a.txt missing/child
    wrun "$wt" "mkdir target; printf 'untracked\n' > untracked" mv -k missing a.txt untracked target
    wrun "$wt" "" mv a.txt exec.sh missing-destination
    wrun "$wt" "" mv a.txt missing-dir/
    wrun "$wt" "git rm -q --cached a.txt; b=\$(git rev-parse HEAD~2:a.txt); o=\$(git rev-parse HEAD~1:a.txt); t=\$(git rev-parse HEAD:a.txt); printf '100644 %s 1\ta.txt\n100644 %s 2\ta.txt\n100644 %s 3\ta.txt\n' \"\$b\" \"\$o\" \"\$t\" | git update-index --index-info" mv a.txt moved
    WTREE= WREFS= WSTDERR= ;;
  checkout)
    WTREE=1 WREFS=1 WSTDERR=1
    checkout_fixture
    wk="$fx/work"
    # Branch-mode checkout shares the switch machinery but keeps checkout's
    # detached-HEAD advice and its branch/path disambiguation rules.
    wrun "$wk" "" checkout notrack
    wrun "$wk" "" checkout main
    WAFTER_REFRESH="git ls-files -z | xargs -0 rm -f; rm .git/index"
    wrun "$wt" "" checkout main
    WAFTER_REFRESH=
    wrun "$wk" "printf 'dirty\n' >> c1.txt" checkout HEAD
    wrun "$wk" "printf 'dirty\n' >> c1.txt" checkout -f HEAD
    wrun "$wk" "" checkout --detach
    wrun "$wk" "" checkout -q notrack
    wrun "$wk" "" checkout -b checkout-new
    wrun "$fx/st-empty-clean" "" checkout -b checkout-new
    wrun "$fx/st-empty-clean" "printf 'staged\n' > local-add; git add local-add" checkout -b checkout-new
    wrun "$wk" "" checkout -b checkout-from main~3
    wrun "$wk" "git branch replace main~3" checkout -B replace main~1
    wrun "$wk" "" checkout -B main main~1
    wrun "$wk" "" checkout --detach main~2
    wrun "$wk" "" checkout HEAD~2
    wrun "$wk" "git checkout -q notrack" checkout -
    wrun "$wk" "printf 'dirty\n' >> c1.txt" checkout notrack
    wrun "$wk" "printf 'staged\n' >> c1.txt; git add c1.txt" checkout notrack
    wrun "$wk" "printf 'added\n' > local-add; git add local-add" checkout notrack
    wrun "$wk" "git rm -q c1.txt" checkout notrack
    wrun "$wk" "rm c1.txt" checkout notrack
    wrun "$wk" "printf 'dirty\n' >> doc/one.txt" checkout old
    wrun "$wk" "printf 'staged\n' >> doc/one.txt; git add doc/one.txt" checkout old
    wrun "$wk" "printf 'untracked\n' > nt.txt" checkout notrack
    wrun "$wk" "printf 'dirty\n' >> doc/one.txt; printf 'untracked\n' > README" checkout -f old
    wrun "$wk" "" checkout --orphan checkout-orphan
    wrun "$wk" "" checkout --orphan main
    wrun "$wk" "" checkout --orphan notrack
    wrun "$wk" "" checkout -t origin/topic
    wrun "$wk" "" checkout --track origin/docs
    wrun "$wk" "" checkout topic
    wrun "$wk" "git checkout -q -b track-ahead --track origin/main; git commit -qm ahead --allow-empty; git checkout -q main" checkout track-ahead
    wrun "$wk" "" checkout behind
    wrun "$wk" "" checkout ahead
    wrun "$wk" "" checkout gone
    wrun "$wk" "" checkout --no-guess topic
    wrun "$wk" "git remote add other ../origin.git; git config remote.other.fetch '+refs/heads/*:refs/remotes/other/*'; git update-ref refs/remotes/other/topic refs/remotes/origin/topic" checkout topic
    wrun "$wk" "git remote add other ../origin.git; git config remote.other.fetch '+refs/heads/*:refs/remotes/other/*'; git update-ref refs/remotes/other/topic refs/remotes/origin/topic; git config checkout.defaultRemote origin" checkout topic
    wrun "$wk" "git checkout -q --detach main; git commit -q --allow-empty -m orphaned" checkout notrack
    wrun "$wk" "git checkout -q --detach main; for n in 1 2 3 4 5 6; do git commit -q --allow-empty -m orphan-\$n; done" checkout notrack
    wrun "$wk" "git checkout -q --detach main; git commit -q --allow-empty -m retained; git branch retained" checkout notrack
    WAFTER_REFRESH="touch -d '10 seconds ago' c1.txt"
    wrun "$wk" "" checkout notrack
    WAFTER_REFRESH=
    wrun "$wk" "printf 'collision\n' > notrack; git add notrack; git commit -qm collision" checkout notrack
    wrun "$wk" "printf 'collision\n' > notrack; git add notrack; git commit -qm collision; printf 'changed\n' > notrack" checkout -- notrack
    wrun "$wk" "printf 'collision\n' > topic; git add topic; git commit -qm collision" checkout topic
    wrun "$wt" "git branch oldtree v1.0" checkout oldtree
    wrun "$wt" "git branch oldtree v1.0; printf 'keep\n' > swap/keep" checkout oldtree
    wrun "$wt" "git branch oldtree v1.0; printf 'keep\n' > swap/keep" checkout -f oldtree
    WSTDERR=
    wrun "$wt" "git branch oldtree v1.0; printf 'keep\n' > swap/keep; touch .git/index.lock" checkout -f oldtree
    WSTDERR=1
    wrun "$wt" "git branch oldtree v1.0; git checkout -q oldtree; printf 'keep\n' > back/keep" checkout main
    prefix_setup="git checkout -q -b prefix-target; mkdir blocker; printf 'target\\n' > blocker/sub; git add blocker; git commit -qm prefix; git checkout -q main"
    wrun "$wt" "$prefix_setup; printf 'precious\n' > blocker" checkout prefix-target
    wrun "$wt" "$prefix_setup; printf 'staged\n' > blocker; git add blocker; printf 'precious\n' > blocker" checkout prefix-target
    wrun "$wt" "$prefix_setup; printf 'precious\n' > blocker" checkout -f prefix-target
    wrun "$wt" "$prefix_setup; mkdir outside; printf 'precious\n' > outside/keep; ln -s outside blocker" checkout prefix-target
    wrun "$wt" "$prefix_setup; mkdir outside; printf 'precious\n' > outside/keep; ln -s outside blocker" checkout -f prefix-target
    wrun "$wt" "$prefix_setup; mkdir -p outside/sub; printf 'precious\n' > outside/sub/keep; ln -s outside blocker" checkout -f prefix-target
    wrun "$wt" "$prefix_setup; mkdir -p blocker/sub outside; printf 'safe\n' > outside/safe; ln -s ../../outside blocker/sub/link" checkout -f prefix-target
    wrun "$wt" "$prefix_setup; printf 'blocker/\n' >> .git/info/exclude; mkdir -p blocker/sub outside; printf 'safe\n' > outside/safe; ln -s ../../outside blocker/sub/link" checkout prefix-target
    wrun "$wt" "$prefix_setup; git checkout -q prefix-target; git update-index --skip-worktree blocker/sub; rm blocker/sub; mkdir blocker/sub; printf 'precious\n' > blocker/sub/keep" checkout -f prefix-target
    wrun "$wt" "printf 'dirty\n' > a.txt" checkout -- a.txt
    wrun "$wt" "printf 'dirty\n' > a.txt" checkout -f -- a.txt
    wrun "$wt" "printf 'dirty\n' > a.txt" checkout -f a.txt
    wrun "$wt" "rm a.txt" checkout a.txt
    wrun "$wt" "printf 'staged\n' > a.txt; git add a.txt; printf 'dirty\n' >> a.txt" checkout -- a.txt
    wrun "$wt" "rm dir/one.txt; printf 'untracked\n' > dir/keep" checkout -- dir
    wrun "$wt" "" checkout HEAD~1 -- a.txt
    wrun "$wt" "" checkout v1.0 -- dir
    wrun "$wt" "" checkout v1.0 dir
    wrun "$wt" "" checkout v1.0 -- swap back
    wrun "$wt" "rm a.txt; rm dir/one.txt" checkout -- '*.txt'
    wrun "$wt" "" checkout -- nonexistent
    wrun "$wt" "printf 'dirty\n' > a.txt" checkout -- a.txt nonexistent
    wrun "$wt" "" checkout v1.0 -- nonexistent
    wrun "$wt" "git config core.autocrlf true; rm auto.txt attr.txt" checkout -- auto.txt attr.txt
    wrun "$wt" "rm attr.txt" checkout -- attr.txt
    wrun "$wt" "rm .gitattributes attr.txt" checkout -- attr.txt
    wrun "$wt" "printf 'attr.txt -text\n' > .gitattributes; rm attr.txt" checkout -- attr.txt
    WSTDERR=
    wrun "$wt" "printf 'dirty\n' > a.txt; touch .git/index.lock" checkout -- a.txt
    WSTDERR=1
    wrun "$wt" "chmod +x exec.sh" checkout -- exec.sh
    wrun "$wt" "" checkout v1.0 -- exec.sh
    wrun "$wt" "rm link; ln -s missing link" checkout -- link
    wrun "$wt" "rm link; git config core.symlinks false" checkout -- link
    wrun "$wt" "git update-index --assume-unchanged a.txt; printf 'dirty\n' > a.txt" checkout -- a.txt
    wrun "$wt" "git update-index --skip-worktree a.txt; printf 'dirty\n' > a.txt" checkout HEAD -- a.txt
    wrun "$wt" "git update-index --skip-worktree a.txt; printf 'dirty\n' > a.txt" checkout v1.0 -- a.txt
    wrun "$wt" "" checkout -- a.txt
    WTREE= WREFS= WSTDERR= ;;
  restore)
    WTREE=1 WREFS=1 WSTDERR=1
    checkout_fixture restore
    rt="$out/restore"
    dirty="printf 'staged\n' > a.txt; git add a.txt; printf 'unstaged\n' >> a.txt"
    wrun "$rt" "printf 'dirty\n' > a.txt" restore -- a.txt
    wrun "$rt" "rm a.txt; mkdir a.txt; printf 'untracked\n' > a.txt/keep" restore -- a.txt
    wrun "$rt" "mkdir outside; printf 'safe\n' > outside/keep; rm a.txt; ln -s outside a.txt" restore -- a.txt
    wrun "$rt" "mkdir -p outside/one.txt; printf 'safe\n' > outside/one.txt/keep; rm -rf dir; ln -s outside dir" restore -- dir/one.txt
    wrun "$rt" "$dirty" restore --staged -- a.txt
    wrun "$rt" "$dirty" restore --staged --worktree -- a.txt
    wrun "$rt" "$dirty" restore -SW -- a.txt
    wrun "$rt" "$dirty" restore -WS -q -- a.txt
    wrun "$rt" "$dirty" restore -s HEAD~2 -- a.txt
    wrun "$rt" "printf 'dirty\n' > dir/one.txt" restore --source=HEAD~2 -- dir
    wrun "$rt" "$dirty" restore -s HEAD~2 -S -- a.txt
    wrun "$rt" "$dirty" restore -s HEAD~2 -SW -- a.txt
    wrun "$rt" "printf 'local\n' > new-head.txt" restore -s HEAD~2 -W -- new-head.txt
    wrun "$rt" "printf 'local\n' > new-head.txt" restore -s HEAD~2 -S -- new-head.txt
    wrun "$rt" "printf 'local\n' > new-head.txt" restore -s HEAD~2 -SW -- new-head.txt
    wrun "$rt" "printf 'keep\n' > swap/keep; printf 'keep\n' > back" restore -s HEAD~2 -SW -- swap back
    wrun "$rt" "rm a.txt dir/one.txt" restore -- '*.txt'
    wrun "$rt" "git config core.autocrlf true; rm auto.txt attr.txt" restore -- auto.txt attr.txt
    wrun "$rt" "chmod +x exec.sh; rm link; ln -s missing link" restore -- exec.sh link
    wrun "$rt" "git update-index --assume-unchanged a.txt exec.sh; printf 'dirty\n' > a.txt; chmod +x exec.sh" restore -s HEAD~2 -S -- a.txt exec.sh
    wrun "$rt" "git update-index --skip-worktree a.txt" restore -- a.txt
    wrun "$rt" "printf 'dirty\n' > a.txt" restore -- a.txt no-one no-two
    wrun "$rt" "" restore -s HEAD~2 -- no-one no-two
    wrun "$rt" "printf 'dirty\n' > a.txt" restore --worktree -- a.txt
    WSTDERR=
    wrun "$rt" "printf 'dirty\n' > a.txt; touch .git/index.lock" restore -- a.txt
    WSTDERR=1
    wrun "$rt" "" restore
    wrun "$rt" "" restore -s no-such -- a.txt
    WTREE= WREFS= WSTDERR= ;;
  worktree-overlay)
    WTREE=1 WREFS=1 WSTDERR=1
    checkout_fixture
    wrun "$wt" "" checkout v1.0 -- 'dir/*.txt' a.txt
    wrun "$wt" "" checkout v1.0 -- dir dir/one.txt
    wrun "$wt" "printf 'keep\n' > swap-extra; printf 'keep\n' > swap.txt; git add swap-extra swap.txt" checkout v1.0 -- swap
    wrun "$wt" "git checkout -q v1.0" checkout main -- swap
    wrun "$wt" "git branch carried HEAD; git update-index --assume-unchanged dir/two.txt; printf 'dirty\n' >> dir/two.txt" switch carried
    wrun "$wt" "git branch carried HEAD; git clone -q . module; git -C module checkout -q \$(git rev-parse HEAD:module); printf 'dirty\n' >> module/a.txt" switch carried
    WTREE= WREFS= WSTDERR= ;;
  switch)
    WTREE=1 WREFS=1 WSTDERR=1
    checkout_fixture
    wk="$fx/work"
    wrun "$wk" "" switch notrack
    wrun "$wk" "" switch main
    WAFTER_REFRESH="git ls-files -z | xargs -0 rm -f; rm .git/index"
    wrun "$wt" "" switch main
    WAFTER_REFRESH=
    wrun "$wk" "" switch -q notrack
    wrun "$wk" "" switch -c switch-new
    wrun "$fx/st-empty-clean" "" switch -c switch-new
    wrun "$fx/st-empty-clean" "printf 'staged\n' > local-add; git add local-add" switch -c switch-new
    wrun "$wk" "" switch -c switch-from main~3
    wrun "$wk" "git branch replace main~3" switch -C replace main~1
    wrun "$wk" "" switch -C main main~1
    wrun "$wk" "" switch --detach main~2
    wrun "$wk" "" switch --detach
    wrun "$wk" "git switch -q notrack" switch -
    wrun "$wk" "printf 'dirty\n' >> c1.txt" switch notrack
    wrun "$wk" "printf 'staged\n' >> c1.txt; git add c1.txt" switch notrack
    wrun "$wk" "printf 'added\n' > local-add; git add local-add" switch notrack
    wrun "$wk" "git rm -q c1.txt" switch notrack
    wrun "$wk" "rm c1.txt" switch notrack
    wrun "$wk" "printf 'dirty\n' >> doc/one.txt" switch old
    wrun "$wk" "printf 'staged\n' >> doc/one.txt; git add doc/one.txt" switch old
    wrun "$wk" "printf 'untracked\n' > nt.txt" switch notrack
    wrun "$wk" "printf 'dirty\n' >> doc/one.txt; printf 'untracked\n' > README" switch -f old
    wrun "$wk" "printf 'dirty\n' >> doc/one.txt; printf 'untracked\n' > README" switch --discard-changes old
    wrun "$wk" "" switch --orphan switch-orphan
    wrun "$wk" "" switch --orphan main
    wrun "$wk" "" switch --orphan notrack
    wrun "$wk" "" switch -t origin/topic
    wrun "$wk" "" switch --track origin/docs
    wrun "$wk" "" switch topic
    wrun "$wk" "git switch -q -c track-ahead --track origin/main; git commit -qm ahead --allow-empty; git switch -q main" switch track-ahead
    wrun "$wk" "" switch behind
    wrun "$wk" "" switch ahead
    wrun "$wk" "" switch gone
    wrun "$wk" "" switch --no-guess topic
    wrun "$wk" "git remote add other ../origin.git; git config remote.other.fetch '+refs/heads/*:refs/remotes/other/*'; git update-ref refs/remotes/other/topic refs/remotes/origin/topic" switch topic
    wrun "$wk" "git remote add other ../origin.git; git config remote.other.fetch '+refs/heads/*:refs/remotes/other/*'; git update-ref refs/remotes/other/topic refs/remotes/origin/topic; git config checkout.defaultRemote origin" switch topic
    wrun "$wk" "git switch -q --detach main; git commit -q --allow-empty -m orphaned" switch notrack
    wrun "$wk" "git checkout -q --detach main; for n in 1 2 3 4 5 6; do git commit -q --allow-empty -m orphan-\$n; done" switch notrack
    wrun "$wk" "git checkout -q --detach main; git commit -q --allow-empty -m retained; git branch retained" switch notrack
    WAFTER_REFRESH="touch -d '10 seconds ago' c1.txt"
    wrun "$wk" "" switch notrack
    WAFTER_REFRESH=
    wrun "$wt" "git branch oldtree v1.0" switch oldtree
    wrun "$wt" "git branch oldtree v1.0; touch .git/index.lock" switch oldtree
    wrun "$wt" "git branch oldtree v1.0; printf 'keep\n' > swap/keep" switch oldtree
    wrun "$wt" "git branch oldtree v1.0; printf 'keep\n' > swap/keep" switch -f oldtree
    WSTDERR=
    wrun "$wt" "git branch oldtree v1.0; printf 'keep\n' > swap/keep; touch .git/index.lock" switch -f oldtree
    WSTDERR=1
    wrun "$wt" "git branch oldtree v1.0; git switch -q oldtree; printf 'keep\n' > back/keep" switch main
    prefix_setup="git checkout -q -b prefix-target; mkdir blocker; printf 'target\\n' > blocker/sub; git add blocker; git commit -qm prefix; git checkout -q main"
    wrun "$wt" "$prefix_setup; printf 'precious\n' > blocker" switch prefix-target
    wrun "$wt" "$prefix_setup; printf 'staged\n' > blocker; git add blocker; printf 'precious\n' > blocker" switch prefix-target
    wrun "$wt" "$prefix_setup; printf 'precious\n' > blocker" switch -f prefix-target
    wrun "$wt" "$prefix_setup; mkdir outside; printf 'precious\n' > outside/keep; ln -s outside blocker" switch prefix-target
    wrun "$wt" "$prefix_setup; mkdir outside; printf 'precious\n' > outside/keep; ln -s outside blocker" switch -f prefix-target
    wrun "$wt" "$prefix_setup; mkdir -p outside/sub; printf 'precious\n' > outside/sub/keep; ln -s outside blocker" switch -f prefix-target
    wrun "$wt" "$prefix_setup; mkdir -p blocker/sub outside; printf 'safe\n' > outside/safe; ln -s ../../outside blocker/sub/link" switch -f prefix-target
    wrun "$wt" "$prefix_setup; printf 'blocker/\n' >> .git/info/exclude; mkdir -p blocker/sub outside; printf 'safe\n' > outside/safe; ln -s ../../outside blocker/sub/link" switch prefix-target
    wrun "$wt" "$prefix_setup; git switch -q prefix-target; git update-index --skip-worktree blocker/sub; rm blocker/sub; mkdir blocker/sub; printf 'precious\n' > blocker/sub/keep" switch -f prefix-target
    WTREE= WREFS= WSTDERR= ;;
  commit-output)
    wrun "$fx/dirty" "$NOREPO" commit -a -m "all changes"
    wrun "$fx/dirty" "$NOREPO; $R" commit -a -m "all changes"
    wrun "$fx/st-uptodate" "git mv src/run.sh src/bin-run.sh && mkdir docs/x && git mv docs/long.txt docs/x/long.txt && chmod +x b.txt && git add b.txt" commit -m renames
    wrun "$fx/st-uptodate" "git rm -q a.txt && git mv b.txt c.txt" commit -m "delete and rename"
    wrun "$fx/st-uptodate" "" commit -m nothing
    wrun "$fx/st-ahead" "" commit -m "untracked only"
    wrun "$fx/st-ahead" "" commit -a -m "untracked only"
    wrun "$fx/dirty" "git checkout -q -- a.txt" commit -a -m x
    wrun "$fx/st-staged-only" "" commit -m ""
    wrun "$fx/st-ahead" "" commit --amend --no-edit
    wrun "$fx/st-ahead" "" commit --amend -m "amended"
    wrun "$fx/st-staged-only" "" commit --amend -m "amend with staged"
    WENV="GIT_AUTHOR_NAME=Someone GIT_AUTHOR_EMAIL=someone@example.com"
    wrun "$fx/st-staged-only" "" commit -m "other author"
    wrun "$fx/st-ahead" "" commit --amend --no-edit
    WENV="GIT_COMMITTER_NAME="
    wrun "$fx/st-staged-only" "" commit -a -m "no identity"
    WENV=
    wrun "$fx/st-uptodate" "seq 1 100 > a.txt && seq 1 2 100 > b.txt && git add a.txt b.txt" commit -m stats ;;
  log-format)
    h="$fx/hist"
    for f in medium short full fuller raw oneline reference med ful; do
      check "$h" log --pretty=$f
    done
    check "$h" log --oneline --all
    check "$h" log --abbrev-commit -3
    check "$h" log --abbrev=10 --oneline -5
    check "$h" log --no-abbrev --pretty=medium -12
    check "$h" log --format=%H%n%h%n%T%n%t%n%P%n%p%n%an%n%ae%n%al%n%ad%n%at%n%ai%n%aI%n%as%n%aD -6
    check "$h" log --format=%cn%x09%ce%x09%cl%x09%cd%x09%ct%x09%ci%x09%cI%x09%cs%x09%cD -6
    check "$h" log "--format=[%s]%n[%f]%n[%b]%n[%B]" --all
    check "$h" log --format=format:%h%x00%s -5
    check "$h" log "--format=tformat:%h%+b%-b% s|" --all
    check "$h" log "--format=%h %d%n%D" --all
    check "$h" log --format= -3
    check "$h" log --pretty=format: -3
    check "$h" log "--format=%m %h" main...c2
    for d in default iso iso8601 iso-strict rfc short raw unix "format:%Y/%m/%d %H:%M:%S %z %a %b %e %j %A %B %I" default-local iso-local short-local; do
      check "$h" log --date="$d" -4
    done
    check "$h" log --decorate --oneline --all
    check "$h" log --decorate=full --all
    check "$h" log --decorate=short -3
    check "$h" log --decorate --no-decorate -3
    check "$h" log --decorate --format=raw -2
    check "$h" log --pretty=raw --decorate -2
    check "$h" log --parents --oneline --all
    check "$h" log --parents -3
    check "$h" log --pretty=oneline -4
    check "$h" log --pretty=fuller --date=iso -4
    check "$h" log --pretty=bogus
    check "$h" log --date=bogus ;;
  blame)
    h="$fx/hist" r="$fx/ren" i="$fx/inspect" c="$fx/show"
    for p in src/main.c README.md doc/one.txt data.bin; do
      check "$h" blame HEAD -- "$p"
      check "$h" blame --porcelain HEAD -- "$p"
    done
    check "$h" blame README.md
    check "$h" blame -s HEAD -- src/main.c
    check "$h" blame -e HEAD -- src/main.c
    check "$h" blame -L 5,15 -L 20,+3 HEAD -- src/main.c
    check "$h" blame --porcelain -L 7,7 HEAD -- src/main.c
    check "$h" blame v1.0 -- src/main.c
    for p in b/numbers2.txt deep/dir/text.txt b/crlf.txt b/other.txt b/long.txt; do
      check "$r" blame HEAD -- "$p"
    done
    check "$r" blame --porcelain HEAD -- deep/dir/text.txt
    check "$i" blame HEAD -- renamed
    check "$i" blame --porcelain HEAD -- renamed
    check "$i" blame -w HEAD -- renamed
    check "$i" blame -w --porcelain HEAD -- renamed
    check "$i" blame -s -- dir/text
    check "$i" blame -L '/beta/,+2' HEAD -- renamed
    check "$i" blame -L 4,-2 HEAD -- renamed
    check "$i" blame --abbrev=12 -e HEAD -- renamed
    check "$c" blame cc-merge -- conflict.txt
    check "$c" blame --porcelain cc-merge -- coalesce.txt
    check "$c" blame cc-octopus -- conflict.txt
    check "$h" blame HEAD -- missing ;;
  grep)
    h="$fx/hist" i="$fx/inspect"
    check "$i" grep alpha
    check "$i" grep -n -i alpha
    check "$i" grep -w alpha
    check "$i" grep -v alpha
    check "$i" grep -l alpha
    check "$i" grep -c alpha
    check "$i" grep -n --cached alpha
    check "$i" grep -n alpha HEAD
    check "$i" grep -n alpha v1 HEAD
    check "$i" grep -n alpha -- dir
    check "$i" grep alpha HEAD -- 'dir/*'
    check "$i" grep -e alpha --and -e beta
    check "$i" grep -e alpha --or -e beta
    check "$i" grep -e alpha --and --not -e beta
    check "$i" grep '(' -e alpha --or -e beta ')' --and -e two
    check "$i" grep -E '(alpha|beta) (one|two)'
    check "$i" grep -F '(foo|bar)'
    check "$i" grep 'foo+bar'
    check "$i" grep 'foo\+bar'
    check "$i" grep 'alpha\|beta'
    check "$i" grep -E '[()]'
    check "$i" grep -n '^$'
    check "$i" grep -n 'last$'
    check "$i" grep -n 'alpha[[:space:]]beta'
    check "$i" grep -e alpha HEAD -- dir
    check "$i" grep -n ''
    check "$i" grep no-such-pattern
    check "$i" grep alpha -- untracked
    check "$h" grep -n '^[1-9][0-9]*$' HEAD -- src
    check "$fx/loose" grep 日本語
    check "$fx/loose" grep -ni 日本語
    check "$fx/loose" grep -E '日本.*内容'
    check "$fx/loose" grep 日本語 -- '日本語ディレクトリ' ;;
  describe)
    h="$fx/hist" t="$fx/tags" i="$fx/inspect"
    for rev in HEAD HEAD~1 HEAD~6 topic docs c1 c2 old; do
      check "$h" describe --always "$rev"
      check "$h" describe --tags --long "$rev"
      check "$h" name-rev "$rev"
      check "$h" name-rev --name-only --tags "$rev"
    done
    check "$h" describe --long --abbrev=12 HEAD
    check "$h" describe --tags --abbrev=0 HEAD~2
    check "$h" describe --dirty
    check "$i" describe --dirty
    check "$i" describe --dirty=+changed
    check "$t" describe --tags HEAD
    check "$t" name-rev HEAD HEAD~1 v1.0 v1.0^0
    check "$t" rev-list --objects --all
    check "$h" shortlog HEAD
    check "$h" shortlog -w HEAD
    check "$h" shortlog -sne --all
    check "$h" shortlog -sn
    check "$h" rev-list --count
    check "$h" shortlog -n v1.0..HEAD
    check "$i" shortlog -e HEAD
    for opts in '--count --all' '--all' '--parents HEAD' '--reverse --max-count=5 HEAD' \
                '--count v1.0..HEAD' '--count c1...c2' '--objects HEAD' \
                '--objects v1.0..HEAD' '--objects --all' '--objects --max-count=1 v1.0'; do
      check "$h" rev-list $opts
    done ;;
  log-walk)
    h="$fx/hist" w="$fx/work"
    for r in main topic c1 c2 v1.0 v2.0 light old mid docs tie; do
      check "$h" log --oneline $r
    done
    check "$h" log --oneline main..topic
    check "$h" log --oneline topic..main
    check "$h" log --oneline main...topic
    check "$h" log --oneline c1...c2
    check "$h" log --oneline c1..c2 c2..c1
    check "$h" log --oneline ^v1.0 main
    check "$h" log --oneline main ^c1 ^c2
    check "$h" log --oneline main --not c1 c2
    check "$h" log --oneline ..topic
    check "$h" log --oneline topic..
    check "$h" log --oneline 'c1^@'
    check "$h" log --oneline 'c2^!'
    check "$h" log --oneline 'main^2'
    check "$h" log --oneline --all
    check "$h" log --oneline --branches
    check "$h" log --oneline --branches='c*'
    check "$h" log --oneline --tags
    check "$h" log --oneline --tags=v1
    check "$w" log --oneline --remotes
    check "$w" log --oneline --remotes='*/c*'
    check "$w" log --oneline --all
    check "$h" log --oneline --first-parent
    check "$h" log --oneline --first-parent main~3
    check "$h" log --oneline --merges
    check "$h" log --oneline --no-merges
    check "$h" log --oneline --min-parents=3
    check "$h" log --oneline --max-parents=0
    check "$h" log --oneline --reverse
    check "$h" log --oneline --reverse -n 5
    check "$h" log --oneline --skip=4 -n 6
    check "$h" log --oneline -n 0
    check "$h" log --oneline --max-count=3 --skip=100
    check "$h" log --oneline --since=1710030000
    check "$h" log --oneline --until=1710040000
    check "$h" log --oneline --after="2024-03-10 12:00:00 +0900" --before="2024-03-10 20:00:00 +0900"
    check "$h" log --oneline --author=Other
    check "$h" log --oneline --author='^Log' --all
    check "$h" log --oneline --committer=Committer -3
    check "$h" log --oneline --grep=c1
    check "$h" log --oneline -i --grep=MERGE
    check "$h" log --oneline --grep=c1 --grep=merge
    check "$h" log --oneline --grep=c1 --grep=merge --all-match
    check "$h" log --oneline --grep=c1 --invert-grep
    check "$h" log --oneline -E --grep='c(1|2): (start|more)'
    check "$h" log --oneline -F --grep='(criss-cross)'
    check "$h" log --oneline --grep='t.p'
    check "$h" log --oneline --topo-order --all
    check "$h" log --oneline --date-order --all
    check "$h" log --oneline --topo-order main...topic
    check "$h" log --oneline --all --since=1710040000
    check "$h" log --oneline --all ^light
    check "$h" log --oneline nope
    check "$h" log --oneline main..nope
    check "$fx/st-empty" log
    check "$fx/loose" log --oneline --all
    check "$fx/packed" log --oneline --all --topo-order ;;
  show)
    h="$fx/hist" t="$fx/tags" r="$fx/ren" c="$fx/show"
    check "$h" show
    check "$h" show HEAD~2
    for f in -s --no-patch -q --quiet --stat --oneline --name-only --name-status --abbrev-commit; do
      check "$h" show "$f"
    done
    check "$h" show --format=%h:%s
    check "$h" show --pretty=fuller --date=iso-strict
    check "$h" show --stat -p HEAD
    check "$h" show --no-patch --stat HEAD
    check "$h" show --stat -q HEAD
    check "$h" show -p --quiet HEAD
    check "$h" show -p --name-status HEAD
    for m in main~5 main~6 main~7; do
      for f in '' -s --stat --first-parent --name-status --oneline; do
        if [ -n "$f" ]; then check "$h" show "$f" "$m"; else check "$h" show "$m"; fi
      done
    done
    for f in '' -s --stat --name-status --name-only --first-parent --oneline -U0 -U1; do
      if [ -n "$f" ]; then check "$c" show "$f" cc-merge; else check "$c" show cc-merge; fi
    done
    check "$c" show --stat -p cc-merge
    check "$c" show cc-merge -- conflict.txt
    for f in '' -s --stat --name-status; do
      if [ -n "$f" ]; then check "$c" show "$f" cc-octopus; else check "$c" show cc-octopus; fi
    done
    check "$h" show v1.0
    for tag in annotated nested annotated-tree annotated-blob tree blob empty-message signed-message; do
      check "$t" show "$tag"
    done
    check "$t" show --oneline v1.0
    check "$t" show --pretty=fuller -s nested
    check "$t" show --format=%s -s annotated
    check "$t" show -s v1.0 v1.0^{}
    check "$h" show HEAD^{tree}
    check "$h" show HEAD:doc
    check "$h" show HEAD:README.md
    check "$h" show "$(git -C "$h" rev-parse HEAD:README.md)"
    check "$h" show HEAD:data.bin
    check "$h" show HEAD~2 HEAD
    check "$h" show -s HEAD HEAD~2 HEAD
    check "$h" show --oneline HEAD~2 HEAD
    check "$h" show --format=format:%s -s HEAD HEAD~2
    check "$t" show annotated-tree annotated-blob HEAD v1.0
    check "$h" show HEAD~3..HEAD
    check "$h" show -s HEAD~6..HEAD
    check "$h" show "$(git -C "$h" rev-list --max-parents=0 HEAD)"
    check "$h" show topic~1
    check "$r" show HEAD~2
    check "$h" show nonexistent-revision
    check "$h" show 0000000000000000000000000000000000000001
    check "$h" show HEAD nonexistent-revision
    check "$h" show HEAD:missing-path ;;
  log-diff)
    h="$fx/hist" r="$fx/ren"
    for f in -p --stat --shortstat --numstat --name-only --name-status --summary; do
      check "$h" log $f
      check "$r" log $f
    done
    check "$h" log --oneline --stat
    check "$h" log --stat -p -5
    check "$h" log --stat --summary --all
    check "$h" log --format=%h --name-status --all
    check "$h" log --format=format:%s --shortstat
    check "$h" log -U1 -3
    check "$h" log -p --first-parent
    check "$h" log --stat --no-renames
    check "$h" log -s --stat -3
    check "$r" log -p -M
    check "$r" log --no-renames --name-status
    check "$fx/ren-limit" log --name-status
    check "$fx/ren-limit" log --stat
    check "$fx/loose" log -p --all
    check "$fx/packed" log --stat --all
    COLUMNS=40 check "$h" log --stat -4
    COLUMNS=200 check "$r" log --stat ;;
  log-path)
    h="$fx/hist"
    for p in src/main.c doc doc/ src lib README README.md data.bin x.txt c1.txt '*.txt' 'doc/*' nonexistent; do
      check "$h" log --oneline -- "$p"
      check "$h" log --oneline --full-history -- "$p"
    done
    check "$h" log --oneline -- c1.txt c2.txt
    check "$h" log --oneline --all -- tie.txt
    check "$h" log --oneline --sparse -- doc
    check "$h" log --oneline --first-parent -- doc
    check "$h" log --oneline --parents -- doc
    check "$h" log --oneline --topo-order -- src
    check "$h" log --oneline main -- README.md
    check "$h" log --oneline src/main.c
    check "$h" log --oneline --stat -- doc
    check "$h" log --oneline --full-diff --stat -- x.txt
    check "$h" log -p -- data.bin
    check "$h" log --oneline v1.0..main -- doc
    check "$h" log --oneline main...c2 -- c1.txt c2.txt
    check "$fx/ren" log --oneline --stat -- b
    check "$fx/ren" log --oneline --name-status -- b/numbers2.txt ;;
  log-graph)
    h="$fx/hist" w="$fx/work"
    check "$h" log --graph
    check "$h" log --graph --oneline
    check "$h" log --graph --oneline --all
    check "$w" log --graph --oneline --all
    check "$h" log --graph --oneline --decorate --all
    check "$h" log --graph --date-order --oneline --all
    check "$h" log --graph --oneline main...c2
    check "$h" log --graph --oneline c1 c2 ^main~6
    check "$h" log --graph --oneline --first-parent
    check "$h" log --graph --oneline -- doc
    check "$h" log --graph --oneline --full-history -- doc
    check "$h" log --graph --oneline -- c1.txt c2.txt
    check "$h" log --graph --oneline --merges
    check "$h" log --graph --oneline --no-merges
    check "$h" log --graph --oneline --grep=c1
    check "$h" log --graph --oneline -n 7
    check "$h" log --graph --oneline --skip=3
    check "$h" log --graph --stat -6
    check "$h" log --graph -p -4
    check "$h" log --graph --name-status --all
    check "$h" log --graph --format=%h%n%s%n
    check "$h" log --graph --format=format:%h
    check "$h" log --graph --pretty=fuller -5
    check "$h" log --graph --parents --oneline
    check "$h" log --graph --reverse
    check "$fx/ren" log --graph --stat
    check "$fx/packed" log --graph --oneline --all ;;
  tag-list)
    t="$fx/tags"
    check "$fx/st-empty" tag
    for a in "" -l --list -n -n0 -n1 -n2 -n5 -n20 \
             "-l v1.*" "-l v1.* *blob" "-l *nothing*" "-i -l V*" \
             --sort=refname --sort=-refname --sort=version:refname \
             --sort=-version:refname --sort=v:refname \
             --sort=creatordate --sort=-creatordate --sort=taggerdate \
             "--sort=taggerdate --sort=-creatordate -n2" "-i --sort=refname" \
             --contains --no-contains --merged --no-merged \
             "--contains v1.0" "--no-contains main~3" "--merged c1" \
             "--no-merged c2" "--contains c1 --contains c2" \
             "--no-contains c1 --no-contains c2" \
             "--merged main --contains v1.0 -n2" \
             "--points-at main" "--points-at annotated" \
             "--points-at main~2" "--points-at main^{tree}" \
             "--points-at main:README.md" "--points-at annotated-blob" \
             "--points-at tree --points-at blob" \
             --contains=nope --contains=blob --merged=tree --points-at=nope --sort=nope; do
      # shellcheck disable=SC2086
      check "$t" tag $a
    done
    check "$t" tag -n10 'v*' '*tree' '日本*'
    check "$t" tag --list --contains=main~3 --no-merged=main~1
    check "$t" tag --list --points-at=nested
    check "$t" tag --list --no-contains=main --sort=-refname ;;
  tag-write)
    tagrepo="$fx/tags" WREFS=1 WSTDERR=1
    WENV="GIT_COMMITTER_NAME=Tagger GIT_COMMITTER_EMAIL=tagger@example.com GIT_COMMITTER_DATE=2023-11-22T12:00:00+0900"
    wrun "$tagrepo" "" tag new-tag
    wrun "$tagrepo" "" tag deep/name main~2
    wrun "$tagrepo" "" tag new-tree 'main^{tree}'
    wrun "$tagrepo" "" tag new-blob main:README.md
    wrun "$tagrepo" "" tag new-tag-object annotated
    wrun "$tagrepo" "" tag -a annotated-new -m "release message"
    wrun "$tagrepo" "" tag -am "combined short options" combined
    wrun "$tagrepo" "" tag implied -m "$(printf '  subject  \n\n\nbody  \n\n')" -m "next paragraph"
    wrun "$tagrepo" "" tag -m "" -m "" -m paragraph -m "" empty-paragraphs
    wrun "$tagrepo" "" tag -m "" empty-annotation
    wrun "$tagrepo" "" tag -m "日本語の件名" -m "本文" 日本語タグ
    wrun "$tagrepo" "printf '\n# comment\nfrom file  \n\n\n  body\n# end\n' > tag-message" tag -F tag-message from-file main~3
    wrun "$tagrepo" "git config core.commentChar ';'; printf '; comment\n# kept\nmessage\n' > tag-message" tag -F tag-message comments
    wrun "$tagrepo" "" tag -a -m "tree tag" ann-tree 'main^{tree}'
    wrun "$tagrepo" "" tag -a -m "blob tag" ann-blob main:README.md
    wrun "$tagrepo" "" tag -a -m "tag of a tag" ann-tag annotated
    wrun "$tagrepo" "git config advice.nestedTag false" tag -m "nested silently" ann-tag annotated
    wrun "$tagrepo" "" tag -f v1.2 main
    wrun "$tagrepo" "" tag -f v1.2 v1.2
    wrun "$tagrepo" "" tag -f loose-tag main
    wrun "$tagrepo" "" tag -f annotated main
    wrun "$tagrepo" "" tag -f -a -m replacement annotated main~1
    wrun "$tagrepo" "" tag -fa -m replacement annotated main~1
    wrun "$tagrepo" "" tag v1.2
    wrun "$tagrepo" "" tag 日本語
    wrun "$tagrepo" "" tag -f 日本語 main
    wrun "$tagrepo" "" tag -a -m exists annotated
    wrun "$tagrepo" "" tag new-tag no-such-revision
    wrun "$tagrepo" "" tag missing-object 0000000000000000000000000000000000000001
    wrun "$tagrepo" "" tag -m message missing-object 0000000000000000000000000000000000000001
    wrun "$tagrepo" "" tag 'bad..name'
    wrun "$tagrepo" "" tag 'bad name'
    wrun "$tagrepo" "" tag 'bad.lock'
    wrun "$tagrepo" "" tag 'bad//name'
    wrun "$tagrepo" "" tag ''
    wrun "$tagrepo" "" tag -- -dash
    wrun "$tagrepo" "" tag HEAD
    wrun "$tagrepo" "" tag @
    wrun "$tagrepo" "" tag -m one -F missing both
    wrun "$tagrepo" "" tag -F no-such-file from-file
    wrun "$tagrepo" "" tag tree/child
    wrun "$tagrepo" "" tag -m message tree/child
    wrun "$tagrepo" "git tag parent/child; git pack-refs --all" tag parent
    wrun "$tagrepo" "" tag too many arguments
    wrun "$fx/st-empty" "" tag no-head
    wrun "$tagrepo" "" tag -d loose-tag
    wrun "$tagrepo" "" tag -d annotated nested tag-object tree blob
    wrun "$tagrepo" "" tag -d no-such-tag
    wrun "$tagrepo" "" tag -d loose-tag no-such-tag annotated
    wrun "$tagrepo" "" tag -d no-such-tag blob other-missing
    wrun "$tagrepo" "" tag -d annotated annotated
    wrun "$tagrepo" "" tag -d 日本語
    wrun "$tagrepo" "" tag -d ../../heads/main
    wrun "$tagrepo" "" tag -d
    WENV= WREFS= WSTDERR= ;;
  branch-list)
    wk="$fx/work" h="$fx/hist"
    for a in "" -a -r -v -vv "-a -v" "-a -vv" "-r -vv" "-a --no-abbrev -v" "-v --abbrev=10" \
             --show-current "--list n*" "--list *o*" "-a --list origin/c*" "-r --list *main" \
             "-l -i N*" "-a --sort=-refname" "--sort=committerdate" "--sort=-authordate -v" \
             "-a --sort=objectname"; do
      # shellcheck disable=SC2086
      check "$wk" branch $a
      # shellcheck disable=SC2086
      check "$fx/work-wt" branch $a
    done
    check "$h" branch -v
    check "$fx/st-empty" branch
    check "$fx/st-empty" branch --show-current
    WREFS=1
    wrun "$wk" "git checkout -q --detach main~1" branch -v
    wrun "$wk" "git checkout -q --detach v1.0" branch
    wrun "$wk" "git checkout -q --detach origin/c2 && git commit -q --allow-empty -m moved" branch -vv
    wrun "$wk" "git checkout -q --detach main" branch --show-current
    WREFS= ;;
  branch-filter)
    wk="$fx/work" h="$fx/hist"
    for a in --merged --no-merged "--merged main~2" "--no-merged notrack" "-a --merged" \
             "--contains main~4" "--contains c1" "-a --no-contains main" "-r --contains origin/c2-base" \
             "--points-at main" "-a --points-at origin/main" "--merged main --contains main~6" \
             "-v --merged" "--sort=-committerdate --merged" "--contains nope"; do
      # shellcheck disable=SC2086
      check "$wk" branch $a
    done
    for a in "--contains c1" "--merged c2" "--no-merged" "-v --contains v1.0"; do
      # shellcheck disable=SC2086
      check "$h" branch $a
    done ;;
  branch-create)
    wk="$fx/work" WREFS=1 WSTDERR=1
    wrun "$wk" "" branch new1
    wrun "$wk" "" branch new2 main~3
    wrun "$wk" "" branch new3 origin/topic
    wrun "$wk" "" branch new4 origin/c2-base
    wrun "$wk" "" branch --no-track new5 origin/topic
    wrun "$wk" "" branch -t new6 main
    wrun "$wk" "" branch --track new7 origin/docs
    wrun "$wk" "" branch -q new8 origin/topic
    wrun "$wk" "" branch deep/name/x
    wrun "$wk" "" branch tagged v1.0
    wrun "$wk" "" branch -f notrack main
    wrun "$wk" "" branch -f ahead origin/topic
    wrun "$wk" "" branch notrack
    wrun "$wk" "" branch -f main main~1
    wrun "$wk" "" branch -f wtb main
    wrun "$wk" "" branch 'bad..name'
    wrun "$wk" "" branch 'bad name'
    wrun "$wk" "" branch -- -dash
    wrun "$wk" "" branch nostart nope
    wrun "$wk" "" branch -t badtrack main~1
    wrun "$wk" "git config branch.autoSetupMerge false" branch auto1 origin/topic
    wrun "$wk" "git config branch.autoSetupMerge always" branch auto2 main
    wrun "$wk" "git checkout -q --detach main~2" branch fromdetached
    wrun "$fx/st-empty" "" branch first
    wrun "$wk" "printf '# comment\n' >> .git/config" branch -t withcomment origin/topic
    wrun "$wk" "git config --unset-all remote.origin.fetch" branch nofetch origin/topic
    WREFS= WSTDERR= ;;
  branch-modify)
    wk="$fx/work" WREFS=1 WSTDERR=1
    wrun "$wk" "" branch -d behind
    wrun "$wk" "" branch -d notrack
    wrun "$wk" "" branch -D notrack
    wrun "$wk" "" branch -d gone
    wrun "$wk" "" branch -d ahead
    wrun "$wk" "" branch -D ahead loose-only
    wrun "$wk" "" branch -d loose-only
    wrun "$wk" "" branch -d main
    wrun "$wk" "" branch -d wtb
    wrun "$wk" "" branch -d nope
    wrun "$wk" "" branch -d c2
    wrun "$wk" "" branch -d
    wrun "$wk" "" branch -q -D notrack
    wrun "$wk" "" branch -d -r origin/c2
    wrun "$wk" "" branch -r -d origin/nope
    wrun "$wk" "" branch -d local-up
    wrun "$wk" "git branch -q --track up2 notrack" branch -d up2
    wrun "$wk" "" branch -m ahead renamed
    wrun "$wk" "" branch -m notrack deep/renamed
    wrun "$wk" "" branch -m main trunk
    wrun "$wk" "" branch -m renamedcurrent
    wrun "$wk" "" branch -m ahead behind
    wrun "$wk" "" branch -M ahead behind
    wrun "$wk" "" branch -m nope x
    wrun "$wk" "" branch -m ahead 'bad..'
    wrun "$wk" "" branch -m wtb wtb2
    wrun "$wk" "" branch -m loose-only loose-moved
    wrun "$wk" "" branch -c ahead copied
    wrun "$wk" "" branch -c ahead behind
    wrun "$wk" "" branch -C ahead behind
    wrun "$wk" "" branch -c currentcopy
    wrun "$wk" "" branch -c local-up local-copy
    wrun "$wk" "" branch -M main main
    wrun "$wk" "git checkout -q --detach main" branch -m x
    wrun "$fx/st-empty" "" branch -m newmain
    wrun "$fx/hist" "" branch -d c1
    wrun "$fx/hist" "" branch -D old c1 c2
    WREFS= WSTDERR= ;;
  branch-upstream)
    wk="$fx/work" WREFS=1 WSTDERR=1
    wrun "$wk" "" branch -u origin/topic
    wrun "$wk" "" branch -u origin/docs notrack
    wrun "$wk" "" branch --set-upstream-to=origin/c2 ahead
    wrun "$wk" "" branch --set-upstream-to origin/c2 ahead
    wrun "$wk" "" branch -u notrack ahead
    wrun "$wk" "" branch -u ahead ahead
    wrun "$wk" "" branch -u origin/nope
    wrun "$wk" "" branch -u v1.0 ahead
    wrun "$wk" "" branch -u origin/topic nope
    wrun "$wk" "" branch --unset-upstream ahead
    wrun "$wk" "" branch --unset-upstream
    wrun "$wk" "" branch --unset-upstream notrack
    wrun "$wk" "" branch --unset-upstream local-up
    wrun "$wk" "git checkout -q --detach main" branch -u origin/main
    wrun "$wk" "git checkout -q --detach main" branch --unset-upstream
    wrun "$wk" "git config branch.ahead.rebase true" branch --unset-upstream ahead
    wrun "$wk" "git config --add branch.ahead.merge refs/heads/docs" branch --unset-upstream ahead
    WREFS= WSTDERR= ;;
  *) echo "unknown suite $1"; exit 2 ;;
esac
if [ "$checks" -eq 0 ]; then
  if [ -n "${NGIT_TEST_MATCH:-}" ]; then
    echo "No comparisons matched: $NGIT_TEST_MATCH"
    fail=1
  else
    case $1 in
      hooks|hooks-extra) echo "No comparisons selected for $1"; fail=1 ;;
    esac
  fi
fi
if [ "$fail" -eq 0 ]; then
  echo "PASS $1: $checks comparisons"
else
  echo "FAIL $1: $checks comparisons"
fi
exit $fail
