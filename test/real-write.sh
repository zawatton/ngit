#!/bin/sh
# Write commands on a clone of the real repository REPO, against git.
#   tree   - update-index --force-write-index, then write-tree after git
#            has staged the modifications
#   add    - add -A after modifying the clone
#   commit - commit -a -m after modifying the clone (fixed dates)
#   branch - a create/track/rename/copy/upstream/delete sequence; config,
#            packed-refs, refs, reflogs and output must match
#   reset  - soft/mixed/hard resets and return to the cloned commit
#   tag    - create, list, filter and delete lightweight/annotated tags
#   switch - branch switching, previous branch, path checkout and reset
#   rm-mv  - cached/work-tree removals and file/directory renames
#   restore - restore the index and work tree, including an older tree
#   stash  - save tracked/untracked changes, list/show, then pop
#   merge  - merge branches, cherry-pick, revert and rebase --onto
#   gc     - pack maintenance; refs, packed-refs, logs, object set and fsck
#   transport - file URL clone, new commit, push and cross-fetch
#   hooks  - commit, switch, merge and local push with recording hooks
# git and ngit start from the same index (and refs, for commit); the
# resulting index bytes, stdout and HEAD must be identical, and ngit's
# runs must take at most LIMIT seconds in total.  The clone lives in
# $NGIT_FIXTURE_DIR/real-clone and is reset to the cloned commit first.
# Usage: test/real-write.sh REPO MODE LIMIT
# NGIT_MAX_LOAD optionally waits for this one-minute load before gc or
# each measured sequence step.  Waiting is excluded from ngit's elapsed time.
set -u
if [ "$2" = transport ]; then
  exec sh "$(dirname "$0")/real-transport.sh" "$1" "$3"
fi
if [ "$2" = hooks ]; then
  exec sh "$(dirname "$0")/real-hooks.sh" "$1" "$3"
fi
root=$(cd "$(dirname "$0")/.." && pwd)
ngit="$root/bin/ngit"
# Byte code makes ngit several times faster; without Emacs, sources run.
sh "$root/tools/build-elc.sh" || :
repo=$1 mode=$2 limit=$3
fx=${NGIT_FIXTURE_DIR:-${TMPDIR:-/tmp}/ngit-fixtures}
clone="$fx/real-clone"
mkdir -p "$fx"
if [ ! -f "$clone/.git/ngit-base" ]; then
  rm -rf "$clone"
  # Keep the source's object files independent from maintenance tests.
  # A shared clone would need alternates, which ngit does not read.
  git clone -q --no-hardlinks "$repo" "$clone" || { echo "clone failed"; exit 2; }
  git -C "$clone" config gc.auto 0
  git -C "$clone" config maintenance.auto false
  git -C "$clone" rev-parse HEAD >"$clone/.git/ngit-base"
fi
cd "$clone" || exit 2
if [ "$mode" = gc ]; then
  out=$(mktemp -d "$fx/.gc-XXXXXXXX")
else
  out=$(mktemp -d)
fi
trap 'rm -rf "$out"' EXIT
export GIT_OPTIONAL_LOCKS=0
export GIT_AUTHOR_NAME="Test Author" GIT_AUTHOR_EMAIL=test@example.com
export GIT_COMMITTER_NAME="Test Author" GIT_COMMITTER_EMAIL=test@example.com
export GIT_AUTHOR_DATE="1700600000 +0900" GIT_COMMITTER_DATE="1700600000 +0900"

git reset -q --hard "$(cat .git/ngit-base)" && git clean -fdq || exit 2

# Deterministic modifications: two appended files, a deleted one, an
# exec bit, a touched-only file and new files in old and new directories.
case $mode in
tree|add|commit|branch)
git ls-files -- '*.org' '*.md' | sed -n '10p;200p;400p;600p;800p' >"$out/picks"
f1=$(sed -n 1p "$out/picks") f2=$(sed -n 2p "$out/picks") f3=$(sed -n 3p "$out/picks")
f4=$(sed -n 4p "$out/picks") f5=$(sed -n 5p "$out/picks")
echo "ngit real-write line" >>"$f1"
echo "ngit real-write line" >>"$f2"
rm -f "$f3"
chmod +x "$f4"
touch "$f5"
echo new >"$(dirname "$f1")/ngit-real-write-new.txt"
mkdir -p ngit-real-write-dir/sub && echo new >ngit-real-write-dir/sub/n.txt
;;
esac

snap="$out/snap"
save() { rm -rf "$snap"; mkdir -p "$snap"
  for p in index HEAD refs logs packed-refs COMMIT_EDITMSG config ORIG_HEAD \
           AUTO_MERGE REBASE_HEAD CHERRY_PICK_HEAD REVERT_HEAD MERGE_MSG \
           MERGE_HEAD MERGE_MODE sequencer rebase-merge; do
    [ -e ".git/$p" ] && cp -a ".git/$p" "$snap/"; done; }
restore() { for p in index HEAD refs logs packed-refs COMMIT_EDITMSG config ORIG_HEAD \
                    AUTO_MERGE REBASE_HEAD CHERRY_PICK_HEAD REVERT_HEAD MERGE_MSG \
                    MERGE_HEAD MERGE_MODE sequencer rebase-merge; do
    rm -rf ".git/$p"; [ -e "$snap/$p" ] && cp -a "$snap/$p" ".git/$p"; done; }
spent=0
timed() { # timed OUTFILE ARGS...: run ngit, add the elapsed time to $spent
  o=$1; shift
  s=$(date +%s.%N)
  "$ngit" "$@" >"$o" 2>"$out/err"; st=$?
  e=$(date +%s.%N)
  spent=$(echo "$spent + $e - $s" | bc)
  echo "exit $st" >>"$o"
  echo "ngit $*: $(echo "$e - $s" | bc)s"
}
fail=0
same() { # same WHAT FILE1 FILE2
  cmp -s "$2" "$3" || { echo "MISMATCH: $1"; diff "$2" "$3" | head -6
    [ ! -f "$out/err" ] || head -3 "$out/err"; fail=1; }
}
run_both() { # run_both TAG ARGS...: git then ngit from the saved state
  t=$1; shift
  restore; git -c gc.auto=0 "$@" >"$out/$t.git" 2>/dev/null; echo "exit $?" >>"$out/$t.git"
  cp .git/index "$out/$t.git.index"; git rev-parse HEAD >"$out/$t.git.head"
  restore; timed "$out/$t.ngit" "$@"
  cp .git/index "$out/$t.ngit.index"; git rev-parse HEAD >"$out/$t.ngit.head"
  same "$t stdout" "$out/$t.git" "$out/$t.ngit"
  same "$t index" "$out/$t.git.index" "$out/$t.ngit.index"
  same "$t HEAD" "$out/$t.git.head" "$out/$t.ngit.head"
}

sequence_state() { # sequence_state FILE: semantic index plus refs and work tree
  {
    git rev-parse -q --verify HEAD
    git symbolic-ref -q HEAD || :
    for p in HEAD ORIG_HEAD config packed-refs; do
      if [ -f ".git/$p" ]; then echo "== $p"; cat ".git/$p"; fi
    done
    (cd .git && find refs logs -type f | sort | while IFS= read -r p; do
      echo "== $p"; cat "$p"
    done)
    git for-each-ref
    echo '== index'; git ls-files -s
    echo '== diff-files'; git diff-files
    echo '== status'; git status --porcelain
    echo '== locks'; find .git -name '*.lock' | sort
    if [ "$mode" = stash ]; then
      echo '== stash work tree'; git diff --binary
      for p in ngit-real-stash-new ngit-real-stash-dir/deep/new; do
        if [ -f "$p" ]; then echo "$p"; git hash-object "$p"; fi
      done
    fi
    if [ "$mode" = rm-mv ] || [ "$mode" = restore ]; then
      echo '== selected work tree'
      python3 - "$f1" "$f2" "$f3" "$real_dir" \
        ngit-real-moved-file ngit-real-moved-dir <<'PY'
import hashlib, json, os, stat, sys
def snapshot(path):
    if not os.path.lexists(path):
        print(json.dumps([path, 'missing']))
        return
    mode = os.lstat(path).st_mode
    if stat.S_ISLNK(mode):
        data = ['link', os.readlink(path)]
    elif stat.S_ISDIR(mode):
        data = ['dir']
    else:
        with open(path, 'rb') as stream:
            data = ['file', hashlib.sha256(stream.read()).hexdigest()]
    print(json.dumps([path, stat.S_IMODE(mode)] + data))
    if stat.S_ISDIR(mode):
        for name in sorted(os.listdir(path)):
            snapshot(os.path.join(path, name))
for path in sys.argv[1:]:
    snapshot(path)
PY
    fi
  } >"$1"
}

sequence_step() { # sequence_step LABEL ARGS...
  label=$1; shift
  if [ "$tool" = git ]; then
    git -c gc.auto=0 "$@" >"$out/git.$label.out" 2>"$out/git.$label.err"; st=$?
  else
    if [ -n "${NGIT_MAX_LOAD:-}" ]; then
      while :; do
        load=$(LC_ALL=C uptime | awk -F 'load average: ' '{split($2, a, ","); print a[1]+0}')
        awk -v current="$load" -v ceiling="$NGIT_MAX_LOAD" 'BEGIN {exit !(current <= ceiling)}' && break
        sleep 5
      done
      echo "load before $label: $load"
    fi
    s=$(date +%s.%N)
    "$ngit" "$@" >"$out/ngit.$label.out" 2>"$out/ngit.$label.err"; st=$?
    e=$(date +%s.%N)
    spent=$(echo "$spent + $e - $s" | bc)
    echo "ngit $*: $(echo "$e - $s" | bc)s"
    if [ -n "${NGIT_MAX_LOAD:-}" ]; then
      LC_ALL=C uptime | awk -F 'load average: ' '{split($2, a, ","); print "load after: " a[1]+0}'
    fi
    if [ "${NGIT_PROFILE:-}" = 1 ]; then
      # Keep diagnostics visible without including them in git's stderr.
      sed -n '/^profile\//p' "$out/ngit.$label.err"
      sed '/^profile\//d' "$out/ngit.$label.err" >"$out/profile.err"
      mv "$out/profile.err" "$out/ngit.$label.err"
    fi
  fi
  echo "exit $st" >>"$out/$tool.$label.out"
  [ "$st" = 0 ] || fail=1
  sequence_state "$out/$tool.$label.state"
  if [ "$tool" = ngit ]; then
    for part in out err state; do
      same "$mode $label $part" "$out/git.$label.$part" "$out/ngit.$label.$part"
    done
  fi
  # A mismatch already answers the comparison.  Avoid spending minutes on
  # subsequent full-tree writes after a failed step.
  if [ "$fail" != 0 ]; then
    echo "ngit total before failure: ${spent}s"
    exit 1
  fi
}

run_sequence() {
  base=$(cat .git/ngit-base)
  # These names belong only to this disposable clone's checks.
  git branch -D ngit-real-switch >/dev/null 2>&1 || :
  if [ "$mode" = merge ]; then
    git switch -q --detach "$base" || exit 2
    git branch -D ngit-real-left ngit-real-right ngit-real-pick >/dev/null 2>&1 || :
    merge_base=$(git rev-parse "$base~6")
  fi
  git tag -d ngit-real-light ngit-real-annotated >/dev/null 2>&1 || :
  if [ "$mode" = stash ]; then git stash clear; fi
  # Prefer a file HEAD modified or deleted; otherwise any top-level file of
  # HEAD~1 (restoring an unchanged file is still a valid path checkout).
  pick=$(git diff-tree --no-commit-id --name-only --diff-filter=MD -r HEAD~1 HEAD | head -1)
  [ -n "$pick" ] || pick=$(git ls-tree HEAD~1 | awk '$2 == "blob" { print $4; exit }')
  if [ "$mode" = switch ] && [ -z "$pick" ]; then
    echo 'switch check needs a file in HEAD~1'; exit 2
  fi
  if [ "$mode" = rm-mv ] || [ "$mode" = restore ]; then
    # Select a small existing directory and regular files outside it.  Use
    # paths in both revisions so an older-source restore is meaningful.
    git ls-files -z >"$out/current-paths"
    # The older restore source is the nearest HEAD~N (N >= 2) that modified a
    # file, since recent commits of a live repository may only add files.
    src=HEAD~2 n=2
    while [ $n -le 50 ]; do
      if [ -n "$(git diff --name-only --diff-filter=M HEAD~$n HEAD 2>/dev/null | head -1)" ]; then
        src=HEAD~$n; break
      fi
      n=$((n + 1))
    done
    git ls-tree -rz --name-only "$src" >"$out/older-paths"
    git diff --name-only -z --diff-filter=M "$src" HEAD >"$out/changed-paths"
    python3 - "$out/current-paths" "$out/older-paths" "$out/changed-paths" "$mode" >"$out/write-paths" <<'PY'
import collections, os, pathlib, sys
current = pathlib.Path(sys.argv[1]).read_bytes().split(b'\0')[:-1]
older = set(pathlib.Path(sys.argv[2]).read_bytes().split(b'\0')[:-1])
changed = set(os.fsdecode(p) for p in pathlib.Path(sys.argv[3]).read_bytes().split(b'\0')[:-1])
paths = [os.fsdecode(p) for p in current if p in older
         and os.path.isfile(os.fsdecode(p)) and not os.path.islink(os.fsdecode(p))]
if sys.argv[4] == 'restore' and not changed.intersection(paths):
    raise SystemExit('restore check needs a regular file changed since the source revision')
counts = collections.Counter()
for path in current:
    parent = os.path.dirname(os.fsdecode(path))
    while parent:
        counts[parent] += 1
        parent = os.path.dirname(parent)
directory = next((d for d in sorted(counts) if 2 <= counts[d] <= 12
                  and any(p.startswith(d + '/') for p in paths)), None)
if directory is None:
    raise SystemExit('real write check needs a directory with 2-12 files')
outside = sorted((p for p in paths if not p.startswith(directory + '/')),
                 key=lambda p: (p not in changed, p))
if len(outside) < 3:
    raise SystemExit('real write check needs three regular files')
for path in outside[:3] + [directory, next(p for p in paths if p.startswith(directory + '/'))]:
    print(path)
PY
    [ $? = 0 ] || exit 2
    f1=$(sed -n 1p "$out/write-paths") f2=$(sed -n 2p "$out/write-paths")
    f3=$(sed -n 3p "$out/write-paths") real_dir=$(sed -n 4p "$out/write-paths")
    dir_file=$(sed -n 5p "$out/write-paths")
  fi
  sequence_base=$base
  if [ "$mode" = merge ]; then
    # Seed identical histories outside the timed replay sequence, just as
    # the small comparison suites build their input histories with Git.
    git switch -qc ngit-real-left "$merge_base" || exit 2
    printf 'left\n' >ngit-real-left.txt
    git add ngit-real-left.txt && git commit -qm 'real left' || exit 2
    git switch -qc ngit-real-pick "$merge_base" || exit 2
    printf 'pick\n' >ngit-real-pick.txt
    git add ngit-real-pick.txt && git commit -qm 'real pick' || exit 2
    git switch -qc ngit-real-right "$merge_base" || exit 2
    printf 'right\n' >ngit-real-right.txt
    git add ngit-real-right.txt && git commit -qm 'real right' || exit 2
    sequence_base=$(git rev-parse HEAD)
  fi
  save
  for tool in git ngit; do
    restore
    # Recreate the initial work tree before restoring the identical ref/log
    # snapshot.  Only the clone is ever reset or cleaned.
    git reset -q --hard "$sequence_base" && git clean -fdq || exit 2
    restore
    git update-index -q --refresh || exit 2
    case $mode in
      merge)
        sequence_step merge merge -m 'real merge' ngit-real-left
        sequence_step cherry-pick cherry-pick ngit-real-pick
        sequence_step revert revert HEAD
        sequence_step rebase rebase --onto "$base" "$merge_base"
        ;;
      reset)
        sequence_step soft reset --soft HEAD~1
        sequence_step mixed reset --mixed HEAD~1
        sequence_step hard reset --hard HEAD~1
        sequence_step back reset --hard "$base" ;;
      tag)
        sequence_step light tag ngit-real-light
        sequence_step annotated tag -a ngit-real-annotated -m 'ngit real-write tag'
        sequence_step notes tag -n
        sequence_step sorted tag -n --sort=-creatordate
        sequence_step contains tag --contains HEAD~3
        sequence_step delete tag -d ngit-real-light ngit-real-annotated ;;
      switch)
        sequence_step create branch ngit-real-switch HEAD~3
        sequence_step away switch ngit-real-switch
        sequence_step previous switch -
        sequence_step path checkout HEAD~1 -- "$pick"
        sequence_step hard reset --hard ;;
      rm-mv)
        sequence_step cached rm --cached -- "$f1"
        sequence_step remove rm -- "$f2"
        sequence_step file mv -- "$f3" ngit-real-moved-file
        sequence_step directory mv -- "$real_dir" ngit-real-moved-dir ;;
      restore)
        printf '\nstaged restore check\n' >>"$f1"; git add -- "$f1"
        sequence_step staged restore --staged -- "$f1"
        printf '\nworktree restore check\n' >>"$f2"
        sequence_step worktree restore -- "$f1" "$f2"
        sequence_step file-source restore -s "$src" -- "$f1"
        printf '\ndirectory restore check\n' >>"$dir_file"; git add -- "$dir_file"
        sequence_step directory-source restore -s "$src" -- "$real_dir" ;;
      stash)
        git ls-files -- '*.org' '*.md' | sed -n '10p;200p' >"$out/stash-picks"
        f1=$(sed -n 1p "$out/stash-picks") f2=$(sed -n 2p "$out/stash-picks")
        [ -n "$f1" ] && [ -n "$f2" ] || exit 2
        printf 'ngit stash staged\n' >>"$f1"
        git add -- "$f1"
        printf 'ngit stash unstaged\n' >>"$f1"
        printf 'ngit stash modified\n' >>"$f2"
        printf 'ngit stash untracked\n' >ngit-real-stash-new
        mkdir -p ngit-real-stash-dir/deep
        printf 'ngit stash nested\n' >ngit-real-stash-dir/deep/new
        sequence_step push stash push -u -m 'ngit real-write stash'
        sequence_step list stash list
        sequence_step show stash show
        sequence_step pop stash pop ;;
    esac
  done
  git branch -D ngit-real-switch >/dev/null 2>&1 || :
}

case $mode in
  gc)
    save
    # Objects are immutable.  Keeping this snapshot inside the fixture
    # mount permits hard links even under the sandbox's separate mounts.
    cp -al .git/objects "$out/gc-objects" || exit 2
    gc_state() {
      {
        git show-ref --head --dereference
        if [ -f .git/packed-refs ]; then cat .git/packed-refs; fi
        find .git/refs .git/logs -type f | LC_ALL=C sort | while read -r f; do
          echo "== $f"; cat "$f"
        done
        git rev-list --all --objects | LC_ALL=C sort
      } >"$1"
    }
    git gc --prune=now >"$out/git.gc.out" 2>"$out/git.gc.err"; gs=$?
    echo "exit $gs" >>"$out/git.gc.out"
    gc_state "$out/git.gc.state"
    git fsck --no-progress --connectivity-only >"$out/git.gc.fsck" 2>&1; gfs=$?
    restore
    rm -rf .git/objects; cp -al "$out/gc-objects" .git/objects || exit 2
    if [ -n "${NGIT_MAX_LOAD:-}" ]; then
      while :; do
        load=$(LC_ALL=C uptime | awk -F 'load average: ' '{split($2, a, ","); print a[1]+0}')
        awk -v current="$load" -v ceiling="$NGIT_MAX_LOAD" 'BEGIN {exit !(current <= ceiling)}' && break
        sleep 5
      done
      echo "load before gc: $load"
    fi
    s=$(date +%s.%N)
    timeout "$limit" "$ngit" gc --prune=now >"$out/ngit.gc.out" 2>"$out/err"; ns=$?
    e=$(date +%s.%N); spent=$(echo "$e - $s" | bc)
    echo "exit $ns" >>"$out/ngit.gc.out"
    if [ "$ns" != 0 ]; then
      echo "ngit gc: exit $ns"
      tail -8 "$out/err"
      echo "ngit total: ${spent}s (limit ${limit}s)"
      exit 1
    fi
    gc_state "$out/ngit.gc.state"
    git fsck --no-progress >"$out/ngit.gc.fsck" 2>&1; nfs=$?
    same 'gc stdout/status' "$out/git.gc.out" "$out/ngit.gc.out"
    same 'gc refs, packed-refs, reflogs and reachable objects' "$out/git.gc.state" "$out/ngit.gc.state"
    same 'gc fsck' "$out/git.gc.fsck" "$out/ngit.gc.fsck"
    [ "$gs" = 0 ] && [ "$ns" = 0 ] && [ "$gfs" = 0 ] && [ "$nfs" = 0 ] || fail=1
    ;;
  reset|tag|switch|rm-mv|restore|stash|merge) run_sequence ;;
  tree)
    save; run_both rewrite update-index --force-write-index
    git add -A; save; run_both write-tree write-tree ;;
  add)
    save; run_both add add -A ;;
  commit)
    save; run_both commit commit -a -m "ngit real-write commit" ;;
  branch)
    save
    upstream=$(git for-each-ref --format='%(refname:short)' refs/remotes | grep -v HEAD | head -1)
    branch_state() { # branch_state FILE: refs, packed-refs, config and reflogs
      { cat .git/config; cat .git/packed-refs 2>/dev/null
        (cd .git && find refs logs -type f | sort | while read -r f; do echo "== $f"; cat "$f"; done)
        git for-each-ref; } >"$1"; }
    for tool in git ngit; do
      restore
      for cmd in "branch nb1" "branch nb2 HEAD~5" "branch -t nb3 HEAD~2" "branch nb4 $upstream" \
                 "branch -a -vv" "branch -m nb1 nb1r" "branch -c nb2 nb2c" "branch -u $upstream nb2c" \
                 "branch --unset-upstream nb4" "branch -d nb1r" "branch -D nb2" "branch -vv"; do
        # shellcheck disable=SC2086
        if [ $tool = git ]; then
          git -c gc.auto=0 $cmd; echo "exit $?"
        else
          s=$(date +%s.%N); "$ngit" $cmd; st=$?; e=$(date +%s.%N)
          spent=$(echo "$spent + $e - $s" | bc); echo "exit $st"
        fi
      done >"$out/$tool.out" 2>&1
      branch_state "$out/$tool.state"
    done
    same "branch output" "$out/git.out" "$out/ngit.out"
    same "branch state" "$out/git.state" "$out/ngit.state" ;;
  *) echo "unknown mode $mode"; exit 2 ;;
esac
echo "ngit total: ${spent}s (limit ${limit}s)"
[ $fail = 0 ] && [ "$(echo "$spent <= $limit" | bc)" = 1 ]
