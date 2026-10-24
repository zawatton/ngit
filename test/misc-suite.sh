# Sourced by test/compare.sh's misc case.  Keep mutations inside wrun's copy.

WTREE=1 WREFS=1 WSTDERR=1
checkout_fixture misc-clean

clean_files="printf 'loose\n' > loose.tmp; mkdir -p loose-dir/deep mixed; printf 'nested\n' > loose-dir/deep/nested.tmp; printf 'plain\n' > mixed/plain; printf '*.ignored\n' > .git/info/exclude; printf 'ignored\n' > mixed/held.ignored; printf 'ignored\n' > root.ignored"
wrun "$wt" "$clean_files" clean -n
wrun "$wt" "$clean_files" clean -f
wrun "$wt" "$clean_files" clean -fd
wrun "$wt" "$clean_files" clean -fdx
wrun "$wt" "$clean_files" clean -fdX
wrun "$wt" "$clean_files; printf 'keep\n' > loose.keep" clean -n -d -e '*.keep'
wrun "$wt" "$clean_files" clean -n -- '*.tmp'
wrun "$wt" "$clean_files" clean -n -- loose-dir/deep
wrun "$wt" "$clean_files; mkdir nested-repo; git -C nested-repo init -q; printf 'private\n' > nested-repo/file" clean -n -d
wrun "$wt" "$clean_files; mkdir nested-repo; git -C nested-repo init -q; printf 'private\n' > nested-repo/file" clean -n -d -f -f
wrun "$wt" "$clean_files" clean
wrun "$wt" "$clean_files; git config clean.requireForce false" clean

# Add exercises both symbolic and detached HEAD layouts.  Sequences include
# porcelain inspection and removal while the linked tree is still present.
wrun "$wt" "" worktree add -b misc-topic linked HEAD~1
wrun "$wt" "" worktree add --detach detached HEAD~1
wrun "$wt" "" worktree add implicit
wrun "$wt" "git branch existing HEAD~1" worktree add existing-tree existing
wrun "$wt" "git worktree add -q --detach linked HEAD~1" worktree list
wrun "$wt" "git worktree add -q --detach linked HEAD~1" worktree list --porcelain
wrun "$wt" "git worktree add -q --detach linked HEAD~1" -C linked worktree list
wrun "$wt" "git worktree add -q --detach linked HEAD~1" -C linked worktree list --porcelain
wsteps "$wt" "" 'worktree add branch/list/remove' \
  'g worktree add -b misc-topic linked HEAD~1; g worktree list --porcelain; g worktree remove linked'
wsteps "$wt" "" 'worktree add detached/list/remove' \
  'g worktree add --detach detached HEAD~1; g worktree list --porcelain; g worktree remove detached'
wrun "$wt" "git worktree add -q --detach linked HEAD~1; printf 'dirty\n' > linked/untracked" worktree remove linked
wrun "$wt" "git worktree add -q --detach linked HEAD~1; printf 'dirty\n' > linked/untracked" worktree remove --force linked
wrun "$wt" "git worktree add -q --detach linked HEAD~1; git worktree lock linked" worktree remove linked
wrun "$wt" "git worktree add -q --detach linked HEAD~1; git worktree lock --reason retained linked" worktree remove -f linked
wrun "$wt" "git worktree add -q --detach linked HEAD~1; git worktree lock --reason retained linked" worktree remove -ff linked
wrun "$wt" "git worktree add -q --detach linked HEAD~1; git worktree lock linked" worktree remove --force --force linked
wrun "$wt" "git worktree add -q --detach linked HEAD~1; printf 'gitdir: ../.git\n' > linked/.git" worktree remove --force linked
wrun "$wt" "git worktree add -q --detach linked HEAD~1; rm linked/.git" worktree remove --force linked
wrun "$wt" "git worktree add -q --detach linked HEAD~1; rm -rf linked" worktree prune
wrun "$wt" "git worktree add -q --detach linked HEAD~1; git worktree lock linked; rm -rf linked" worktree prune
wrun "$wt" "mkdir -p .git/worktrees/incomplete; git rev-parse HEAD > .git/worktrees/incomplete/HEAD" worktree prune

# Local writes compare config bytes through WREFS.  The duplicate cases prove
# that a failed single-value edit does not partially rewrite the file.
wrun "$wt" "" config misc.one value
wrun "$wt" "" config misc.word list
wrun "$wt" "" config --add misc.multi one
wrun "$wt" "git config misc.one value" config --unset misc.one
wrun "$wt" "git config --add misc.multi one; git config --add misc.multi two" config misc.multi replacement
wrun "$wt" "git config --add misc.multi one; git config --add misc.multi two" config --unset misc.multi
wsteps "$wt" "" 'config local set/add/get/list/unset' \
  'g config misc.seq one; g config --add misc.seq two; g config --get misc.seq; g config --list; g config --unset misc.seq || [ $? -eq 5 ]'

WENV="HOME=$out/w/home"
wsteps "$wt" "mkdir home" 'config global set/add/list/unset' \
  'g config --global user.name Global; g config --global --add misc.multi one; g config --global --add misc.multi two; g config --global --list; g config --global --unset user.name'
WENV=

WTREE= WREFS= WSTDERR=
