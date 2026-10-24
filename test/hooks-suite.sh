#!/bin/sh
# Git hook differential coverage.  Sourced by compare.sh.

WTREE=1 WREFS=1 WSTDERR=1 WHOOKS=1

hook_driver="$root/test/hook-recorder.sh"
hook_mode=$1
hook_section=core

hook_extra_case() {
  case $1 in
    cherry-continue-pre-fails|cherry-continue-prepare-fails|\
    commit-continue-merge-source|commit-continue-message-source|\
    rebase-continue-prepare-fails|rebase-continue-message-bypassed|\
    commit-post-failure-ignored|commit-amend-post-failure-ignored|\
    merge-post-failure-ignored|rebase-post-failure-ignored|\
    am-post-failure-ignored|push-post-failure-ignored|\
    push-post-update-failure-ignored|commit-pre-a-fails|\
    commit-pre-bypassed|commit-message-bypassed|merge-hook-stages|\
    cherry-continue-message-fails|am-hook-stages|\
    push-rejected-tag-input|push-fetch-first-input|\
    refs-cherry-pick|refs-rebase|refs-am) return 0 ;;
    *) return 1 ;;
  esac
}

hook_case_enabled() {
  case $hook_mode in
    hooks-extra)
      [ "$hook_section" = extra ] || hook_extra_case "$1" ;;
    hooks)
      [ "$hook_section" != extra ] && ! hook_extra_case "$1" ;;
  esac
}

hooks_install_path() { # hooks_install_path DIRECTORY PREFIX HOOK...
  hook_path=$1 hook_prefix=$2
  shift 2
  mkdir -p "$hook_path"
  printf '%s\n' "$hook_prefix" >"$hook_path/.ngit-hook-prefix"
  for hook_name in "$@"; do
    cp "$hook_driver" "$hook_path/$hook_name"
    chmod +x "$hook_path/$hook_name"
  done
}

hooks_install() { # hooks_install GITDIR PREFIX HOOK...
  hook_gitdir=$1 hook_prefix=$2
  shift 2
  hooks_install_path "$hook_gitdir/hooks" "$hook_prefix" "$@"
}

hook_wrun() { # hook_wrun SRC SETUP LABEL ARGS...
  hook_src=$1 hook_setup=$2 hook_label=$3
  shift 3
  hook_case_enabled "$hook_label" || return 0
  wrun "$hook_src" ": hook-case-$hook_label; $hook_setup" "$@"
}

commit_setup='printf "commit hook change\n" >>keep.txt; git add keep.txt; hooks_install .git local pre-commit prepare-commit-msg commit-msg post-commit post-rewrite'
commit_repo="$fx/merge-ff"
hook_wrun "$commit_repo" "$commit_setup" commit-hooks commit -m 'hook commit'
hook_wrun "$commit_repo" "$commit_setup" commit-no-verify commit --no-verify -m 'hook no verify'
hook_wrun "$commit_repo" "$commit_setup" commit-n commit -n -m 'hook short no verify'
hook_wrun "$commit_repo" "$commit_setup" commit-source-C commit -C HEAD
WENV='NGIT_HOOK_FAIL=pre-commit'
hook_wrun "$commit_repo" "$commit_setup" commit-pre-fails commit -m failure
hook_wrun "$commit_repo" "$commit_setup" commit-pre-a-fails commit -a -m failure
hook_wrun "$commit_repo" "$commit_setup" commit-pre-bypassed commit --no-verify -m bypass
WENV='NGIT_HOOK_FAIL=prepare-commit-msg'
hook_wrun "$commit_repo" "$commit_setup" commit-prepare-fails commit -m failure
hook_wrun "$commit_repo" "$commit_setup" commit-prepare-no-verify-fails commit --no-verify -m failure
WENV='NGIT_HOOK_FAIL=commit-msg'
hook_wrun "$commit_repo" "$commit_setup" commit-message-fails commit -m failure
hook_wrun "$commit_repo" "$commit_setup" commit-message-bypassed commit -n -m bypass
WENV='NGIT_HOOK_FAIL=post-commit'
hook_wrun "$commit_repo" "$commit_setup" commit-post-failure-ignored commit -m post-failure
WENV=
amend_setup='printf "amend hook change\n" >>keep.txt; git add keep.txt; hooks_install .git local pre-commit prepare-commit-msg commit-msg post-commit post-rewrite'
hook_wrun "$commit_repo" "$amend_setup" commit-amend commit --amend --no-edit
WENV='NGIT_HOOK_FAIL=post-rewrite'
hook_wrun "$commit_repo" "$amend_setup" commit-amend-post-failure-ignored commit --amend --no-edit
WENV='NGIT_HOOK_EDIT=prepare-commit-msg'
hook_wrun "$commit_repo" "$commit_setup" commit-message-edit commit -m edited
WENV='NGIT_HOOK_STAGE=pre-commit NGIT_HOOK_STAGE_FILE=keep.txt'
hook_wrun "$commit_repo" "$commit_setup" commit-hook-stages commit -m staged
hook_wrun "$commit_repo" "$commit_setup" commit-a-hook-stages commit -a -m staged
WENV='NGIT_HOOK_STAGE=pre-commit NGIT_HOOK_STAGE_FILE=keep.txt NGIT_HOOK_FAIL=pre-commit'
hook_wrun "$commit_repo" "$commit_setup" commit-hook-stages-then-fails commit -m staged
hook_wrun "$commit_repo" "$commit_setup" commit-a-hook-stages-then-fails commit -a -m staged
WENV=

# A relative hooksPath is resolved from the work tree, and a non-executable
# hook is warned about and skipped without changing the commit.
relative_setup='printf "relative hook\n" >>keep.txt; git add keep.txt; git config core.hooksPath .githooks; hooks_install_path .githooks relative pre-commit post-commit'
hook_wrun "$commit_repo" "$relative_setup" relative-hooks-path commit -m relative
nonexec_setup='printf "nonexec hook\n" >>keep.txt; git add keep.txt; hooks_install .git local pre-commit post-commit; chmod -x .git/hooks/pre-commit'
hook_wrun "$commit_repo" "$nonexec_setup" non-executable-hook commit -m nonexec
hook_wrun "$commit_repo" "$nonexec_setup; git config advice.ignoredHook false" non-executable-advice-disabled commit -m nonexec
absolute_setup='printf "absolute hook\n" >>keep.txt; git add keep.txt; git config core.hooksPath "$PWD/.git/custom-hooks"; hooks_install_path .git/custom-hooks absolute pre-commit post-commit'
hook_wrun "$commit_repo" "$absolute_setup" absolute-hooks-path commit -m absolute
WSEQUENCE='g -C subdir commit -m subdir'
hook_wrun "$commit_repo" "$commit_setup; mkdir subdir" subdirectory-hooks commit -m subdir
WSEQUENCE=

# Merge hooks cover true merges, fast-forwards, squash state, --no-verify,
# commit-message source arguments, and failures before and after the update.
merge_setup='hooks_install .git local pre-merge-commit prepare-commit-msg commit-msg post-commit post-merge'
hook_wrun "$fx/merge-clean" "$merge_setup" merge-hooks merge side
hook_wrun "$fx/merge-clean" "$merge_setup" merge-no-verify merge --no-verify side
hook_wrun "$fx/merge-ff" "$merge_setup" merge-fast-forward merge side
hook_wrun "$fx/merge-ff" "$merge_setup" merge-squash merge --squash side
WENV='NGIT_HOOK_FAIL=pre-merge-commit'
hook_wrun "$fx/merge-clean" "$merge_setup" merge-pre-fails merge side
WENV='NGIT_HOOK_FAIL=prepare-commit-msg'
hook_wrun "$fx/merge-clean" "$merge_setup" merge-prepare-fails merge side
WENV='NGIT_HOOK_FAIL=commit-msg'
hook_wrun "$fx/merge-clean" "$merge_setup" merge-message-fails merge side
WENV='NGIT_HOOK_FAIL=post-merge'
hook_wrun "$fx/merge-clean" "$merge_setup" merge-post-failure-ignored merge side
WENV='NGIT_HOOK_STAGE=pre-merge-commit NGIT_HOOK_STAGE_FILE=keep.txt'
hook_wrun "$fx/merge-clean" "$merge_setup" merge-hook-stages merge side
WENV=
merge_commit_setup='hooks_install .git local prepare-commit-msg commit-msg post-commit; git merge --no-commit side'
hook_wrun "$fx/merge-clean" "$merge_commit_setup" merge-source-commit commit --no-edit
squash_commit_setup='hooks_install .git local prepare-commit-msg commit-msg post-commit; git merge --squash side'
hook_wrun "$fx/merge-clean" "$squash_commit_setup" squash-source-commit commit -m squash

# Sequencer hooks: cherry-pick and revert run commit hooks; rebase consults
# pre-rebase and reports every rewritten pair to post-rewrite on stdin.
seq_fixture="hooks-sequencer-$hook_mode"
sh "$root/test/make-sequencer-fixture.sh" "$fx" "$seq_fixture" || return
seq_repo="$fx/$seq_fixture"
seq_hooks='hooks_install .git local pre-commit prepare-commit-msg commit-msg post-commit pre-rebase post-rewrite post-checkout'
hook_wrun "$seq_repo" "$seq_hooks" cherry-pick-hooks cherry-pick first
hook_wrun "$seq_repo" "git switch -q side; $seq_hooks" revert-hooks revert HEAD
hook_wrun "$seq_repo" "$seq_hooks" rebase-hooks rebase clean
WENV='NGIT_HOOK_FAIL=pre-rebase'
hook_wrun "$seq_repo" "$seq_hooks" rebase-pre-fails rebase clean
WENV='NGIT_HOOK_FAIL=prepare-commit-msg'
hook_wrun "$seq_repo" "$seq_hooks" cherry-pick-prepare-fails cherry-pick first
hook_wrun "$seq_repo" "git switch -q side; $seq_hooks" revert-prepare-fails revert HEAD
hook_wrun "$seq_repo" "$seq_hooks" rebase-prepare-fails rebase clean
WENV='NGIT_HOOK_FAIL=post-rewrite'
hook_wrun "$seq_repo" "$seq_hooks" rebase-post-failure-ignored rebase clean
WENV='NGIT_HOOK_FAIL=post-checkout'
hook_wrun "$seq_repo" "$seq_hooks" rebase-checkout-failure-ignored rebase clean
WENV=
cherry_continue_setup="git cherry-pick second >/dev/null 2>&1 || :; $seq_hooks"
revert_continue_setup="git revert second >/dev/null 2>&1 || :; $seq_hooks"
rebase_continue_setup="git rebase side >/dev/null 2>&1 || :; $seq_hooks"
WSEQUENCE='printf "resolved\n" >text.txt; g add text.txt; g cherry-pick --continue'
hook_wrun "$seq_repo" "$cherry_continue_setup" cherry-continue-hooks cherry-pick --continue
WENV='NGIT_HOOK_FAIL=pre-commit'
hook_wrun "$seq_repo" "$cherry_continue_setup" cherry-continue-pre-fails cherry-pick --continue
WENV='NGIT_HOOK_FAIL=prepare-commit-msg'
hook_wrun "$seq_repo" "$cherry_continue_setup" cherry-continue-prepare-fails cherry-pick --continue
WENV='NGIT_HOOK_FAIL=commit-msg'
hook_wrun "$seq_repo" "$cherry_continue_setup" cherry-continue-message-fails cherry-pick --continue
WENV=
WSEQUENCE='printf "resolved\n" >text.txt; g add text.txt; g commit --no-edit'
hook_wrun "$seq_repo" "$cherry_continue_setup" commit-continue-merge-source commit --no-edit
WSEQUENCE='printf "resolved\n" >text.txt; g add text.txt; g commit -m resolved'
hook_wrun "$seq_repo" "$cherry_continue_setup" commit-continue-message-source commit -m resolved
WSEQUENCE='printf "resolved\n" >text.txt; g add text.txt; g revert --continue'
hook_wrun "$seq_repo" "$revert_continue_setup" revert-continue-hooks revert --continue
WSEQUENCE='printf "resolved\n" >text.txt; g add text.txt; g rebase --continue'
hook_wrun "$seq_repo" "$rebase_continue_setup" rebase-continue-hooks rebase --continue
WENV='NGIT_HOOK_FAIL=prepare-commit-msg'
hook_wrun "$seq_repo" "$rebase_continue_setup" rebase-continue-prepare-fails rebase --continue
WENV='NGIT_HOOK_FAIL=commit-msg'
hook_wrun "$seq_repo" "$rebase_continue_setup" rebase-continue-message-bypassed rebase --continue
WENV= WSEQUENCE=

# post-checkout distinguishes branch/file checkout with flag 1/0.  switch
# and checkout share the hook.  A failing post-checkout makes the command
# fail after the checkout has already updated the work tree and index.
checkout_fixture hook-checkout
checkout_repo="$out/hook-checkout"
checkout_setup='git branch hook-side HEAD~1; hooks_install .git local post-checkout'
hook_wrun "$checkout_repo" "$checkout_setup" switch-post-checkout switch hook-side
hook_wrun "$checkout_repo" "$checkout_setup" checkout-post-checkout checkout hook-side
hook_wrun "$checkout_repo" 'hooks_install .git local post-checkout' checkout-path-post-checkout checkout HEAD~1 -- a.txt
WENV='NGIT_HOOK_FAIL=post-checkout'
hook_wrun "$checkout_repo" "$checkout_setup" checkout-post-failure switch hook-side
WENV='NGIT_HOOK_FAIL=post-checkout NGIT_HOOK_FAIL_STATUS=7'
hook_wrun "$checkout_repo" "$checkout_setup" switch-post-failure-seven switch hook-side
hook_wrun "$checkout_repo" 'hooks_install .git local post-checkout' checkout-path-post-failure-seven checkout HEAD~1 -- a.txt
WENV=

# am has its own message, pre-apply and post-apply hook chain.
am_repo="$out/hook-am"
mkdir -p "$am_repo"
(
  cd "$am_repo" || exit 1
  git init -q -b main
  git config user.name 'Patch Author'
  git config user.email patch@example.com
  printf 'base\n' >mail.txt
  git add mail.txt
  git commit -qm base
  printf 'mailed\n' >>mail.txt
  git commit -qam mailed
) || { echo 'hook am fixture build failed'; fail=1; return; }
am_setup='git format-patch -1 --stdout >change.mbox; git reset -q --hard HEAD~1; hooks_install .git local applypatch-msg pre-applypatch post-applypatch'
hook_wrun "$am_repo" "$am_setup" am-hooks am change.mbox
WENV='NGIT_HOOK_EDIT=applypatch-msg'
hook_wrun "$am_repo" "$am_setup" am-message-edit am change.mbox
WENV='NGIT_HOOK_STAGE=pre-applypatch NGIT_HOOK_STAGE_FILE=mail.txt'
hook_wrun "$am_repo" "$am_setup" am-hook-stages am change.mbox
WENV='NGIT_HOOK_FAIL=applypatch-msg'
hook_wrun "$am_repo" "$am_setup" am-message-fails am change.mbox
WENV='NGIT_HOOK_FAIL=pre-applypatch'
hook_wrun "$am_repo" "$am_setup" am-pre-fails am change.mbox
WENV='NGIT_HOOK_FAIL=post-applypatch'
hook_wrun "$am_repo" "$am_setup" am-post-failure-ignored am change.mbox
WENV='NGIT_HOOK_FAIL=applypatch-msg'
hook_wrun "$am_repo" "$am_setup" am-no-verify am --no-verify change.mbox
WENV=

# Local and fake-SSH pushes share the same central log.  Receiver hooks live
# in the bare repository while their log remains in the client's .git.
push_repo="$out/hook-push"
mkdir -p "$push_repo"
(
  cd "$push_repo" || exit 1
  git init -q -b main
  git config user.name 'Push Author'
  git config user.email push@example.com
  printf '.git-receiver-placeholder\n' >.gitignore
  printf 'base\n' >push.txt
  git add -A
  git commit -qm base
  git clone -q --bare --no-hardlinks . .git/receiver.git
  git --git-dir=.git/receiver.git config receive.advertisePushOptions true
  printf 'next\n' >>push.txt
  git commit -qam next
  git remote add receiver .git/receiver.git
  cat >fake-ssh <<'EOF'
#!/bin/sh
while [ $# -gt 0 ]; do
  case $1 in -G) exit 0 ;; -o|-p|-l|-i|-F|-J) shift 2 ;; -*) shift ;; *) break ;; esac
done
[ $# -gt 0 ] && shift
exec sh -c "$1"
EOF
  chmod +x fake-ssh
) || { echo 'hook push fixture build failed'; fail=1; return; }
push_hooks='hooks_install .git local pre-push; hooks_install .git/receiver.git remote pre-receive update post-receive post-update'
hook_wrun "$push_repo" "$push_hooks" push-local-hooks push --push-option=one receiver main:refs/heads/pushed
WENV='NGIT_HOOK_FAIL=pre-push'
hook_wrun "$push_repo" "$push_hooks" push-pre-fails push receiver main:refs/heads/pushed
hook_wrun "$push_repo" "$push_hooks" push-pre-bypassed push --no-verify receiver main:refs/heads/pushed
WENV='NGIT_HOOK_FAIL=pre-receive'
hook_wrun "$push_repo" "$push_hooks" push-receive-fails push receiver main:refs/heads/pushed
WENV='NGIT_HOOK_FAIL=update'
hook_wrun "$push_repo" "$push_hooks" push-update-fails push receiver main:refs/heads/pushed
WENV='NGIT_HOOK_FAIL=post-receive'
hook_wrun "$push_repo" "$push_hooks" push-post-failure-ignored push receiver main:refs/heads/pushed
WENV='NGIT_HOOK_FAIL=post-update'
hook_wrun "$push_repo" "$push_hooks" push-post-update-failure-ignored push receiver main:refs/heads/pushed
WENV=
ssh_setup="$push_hooks; git remote set-url receiver fake:.git/receiver.git; git config core.sshCommand ./fake-ssh"
hook_wrun "$push_repo" "$ssh_setup" push-ssh-hooks push receiver main:refs/heads/pushed
hook_wrun "$push_repo" "git --git-dir=.git/receiver.git branch obsolete main; $push_hooks" push-multiple-and-delete push receiver main:refs/heads/one :refs/heads/obsolete
hook_wrun "$push_repo" "git push -q receiver main; $push_hooks" push-up-to-date-hook push receiver main
tag_setup="git tag existing; git --git-dir=.git/receiver.git tag existing; $push_hooks"
hook_wrun "$push_repo" "$tag_setup" push-rejected-tag-input push receiver refs/tags/existing
fetch_first_setup='old=$(git --git-dir=.git/receiver.git rev-parse main); tree=$(git --git-dir=.git/receiver.git rev-parse "main^{tree}"); remote_commit=$(printf "remote-only\n" | git --git-dir=.git/receiver.git commit-tree "$tree" -p "$old"); git --git-dir=.git/receiver.git update-ref refs/heads/main "$remote_commit"'
hook_wrun "$push_repo" "$fetch_first_setup; $push_hooks" push-fetch-first-input push receiver main

# reference-transaction runs alone so its prepared/committed/aborted order
# and stdin batches are not hidden by unrelated hook cleanup.
ref_setup='hooks_install .git refs reference-transaction'
hook_wrun "$commit_repo" "$ref_setup" refs-branch-create branch hook-ref HEAD
hook_wrun "$fx/merge-ff" "$ref_setup" refs-switch switch side
hook_wrun "$fx/merge-clean" "$ref_setup" refs-merge merge side
hook_wrun "$seq_repo" "$ref_setup" refs-cherry-pick cherry-pick first
hook_wrun "$seq_repo" "$ref_setup" refs-rebase rebase clean
hook_wrun "$am_repo" "$am_setup; hooks_install .git refs reference-transaction" refs-am am change.mbox
hook_wrun "$commit_repo" 'git branch hook-ref HEAD; hooks_install .git refs reference-transaction' refs-branch-delete branch -d hook-ref
WENV='NGIT_HOOK_FAIL=reference-transaction:prepared NGIT_HOOK_RECORD_LOCKS=1'
hook_wrun "$commit_repo" "$ref_setup" refs-prepared-fails branch hook-ref HEAD
WENV=
ref_commit_setup='printf "reference commit\n" >>keep.txt; git add keep.txt; hooks_install .git refs reference-transaction'
hook_wrun "$commit_repo" "$ref_commit_setup" refs-commit-order commit -m reference
WENV='NGIT_HOOK_FAIL=reference-transaction:prepared NGIT_HOOK_RECORD_LOCKS=1'
hook_wrun "$commit_repo" "$ref_commit_setup" refs-commit-prepared-fails commit -m reference
WENV=

# Ref updates include backend-specific rename, packed deletion and the
# internal reset performed when restoring a stash's staged changes.
hook_section=extra
hook_wrun "$commit_repo" "$ref_setup" refs-branch-rename branch -m hook-renamed
hook_wrun "$commit_repo" "git branch hook-force HEAD; $ref_setup" refs-unchecked-branch-force branch -f hook-force HEAD~1
hook_wrun "$commit_repo" "$ref_setup" refs-unchecked-switch-force switch -C main HEAD~1
WSEQUENCE='printf "first stash\n" >>keep.txt; g stash push -q; printf "second stash\n" >>keep.txt; g stash push -q'
hook_wrun "$commit_repo" "$ref_setup" refs-unchecked-stash-store stash push -q
WSEQUENCE=
packed_delete_setup='git branch hook-packed HEAD; git pack-refs --all; hooks_install .git refs reference-transaction'
hook_wrun "$commit_repo" "$packed_delete_setup" refs-packed-delete branch -d hook-packed
WENV='NGIT_HOOK_FAIL_PREPARED_N=2 NGIT_HOOK_RECORD_LOCKS=1'
hook_wrun "$commit_repo" "$packed_delete_setup" refs-packed-delete-second-prepared-fails branch -d hook-packed
WENV=
tag_same_setup='git tag hook-tag HEAD; hooks_install .git refs reference-transaction'
hook_wrun "$commit_repo" "$tag_same_setup" refs-tag-same-object tag -f hook-tag HEAD
WENV='NGIT_HOOK_FAIL=reference-transaction:prepared'
hook_wrun "$commit_repo" "$tag_same_setup" refs-tag-same-object-rejected tag -f hook-tag HEAD
WENV=
remote_refs_setup='git remote add hook-origin .; git update-ref refs/remotes/hook-origin/main HEAD; git symbolic-ref refs/remotes/hook-origin/HEAD refs/remotes/hook-origin/main; hooks_install .git refs reference-transaction'
hook_wrun "$commit_repo" "$remote_refs_setup" refs-remote-rename remote rename hook-origin hook-other
hook_wrun "$commit_repo" "$remote_refs_setup" refs-remote-remove remote remove hook-origin
remote_packed_setup='git remote add hook-origin .; git update-ref refs/remotes/hook-origin/main HEAD; git symbolic-ref refs/remotes/hook-origin/HEAD refs/remotes/hook-origin/main; git pack-refs --all; hooks_install .git refs reference-transaction'
hook_wrun "$commit_repo" "$remote_packed_setup" refs-remote-packed-rename remote rename hook-origin hook-other
hook_wrun "$commit_repo" "$remote_packed_setup" refs-remote-packed-remove remote remove hook-origin
stash_refs_setup='printf "staged stash\n" >>keep.txt; git add keep.txt; printf "unstaged stash\n" >>keep.txt; git stash push -qm hook-stash; hooks_install .git refs reference-transaction'
hook_wrun "$commit_repo" "$stash_refs_setup" refs-stash-clear stash clear
hook_wrun "$commit_repo" "$stash_refs_setup" refs-stash-drop-last stash drop
hook_wrun "$commit_repo" "$stash_refs_setup" refs-stash-pop-last stash pop
hook_wrun "$commit_repo" "$stash_refs_setup" refs-stash-apply-index stash apply --index
WENV='NGIT_HOOK_FAIL=reference-transaction:prepared'
hook_wrun "$commit_repo" "$stash_refs_setup" refs-stash-clear-rejected stash clear
hook_wrun "$commit_repo" "$stash_refs_setup" refs-stash-drop-rejected stash drop
WENV='NGIT_HOOK_FAIL_PREPARED_N=1'
hook_wrun "$commit_repo" "$stash_refs_setup" refs-stash-reset-rejected stash apply --index
WENV=
hook_wrun "$commit_repo" "$ref_setup" refs-reflog-update-bypasses-hook reflog delete --updateref 'refs/heads/main@{0}'

worktree_hook_setup='hooks_install .git refs reference-transaction post-checkout'
hook_wrun "$commit_repo" "$worktree_hook_setup" refs-worktree-add worktree add -b hook-wt wt HEAD
WENV='NGIT_HOOK_FAIL=post-checkout NGIT_HOOK_FAIL_STATUS=7'
hook_wrun "$commit_repo" "$worktree_hook_setup" refs-worktree-checkout-fails worktree add -b hook-wt wt HEAD
WENV='NGIT_HOOK_FAIL_PREPARED_N=1'
hook_wrun "$commit_repo" "$worktree_hook_setup" refs-worktree-head-rejected worktree add --detach wt HEAD
hook_wrun "$commit_repo" "$worktree_hook_setup" refs-worktree-branch-rejected worktree add -b hook-wt wt HEAD
WENV='NGIT_HOOK_FAIL_PREPARED_N=3'
hook_wrun "$commit_repo" "$worktree_hook_setup" refs-worktree-reset-rejected worktree add -b hook-wt wt HEAD
WENV=
am_control_setup='git format-patch -1 --stdout >change.mbox; git reset -q --hard HEAD~1; printf "local\n" >mail.txt; git add mail.txt; git commit -qm local; git am change.mbox >/dev/null 2>&1 || :; hooks_install .git refs reference-transaction'
hook_wrun "$am_repo" "$am_control_setup" refs-am-skip am --skip
hook_wrun "$am_repo" "$am_control_setup" refs-am-abort-unchanged-head am --abort
WENV='NGIT_HOOK_FAIL=reference-transaction:prepared NGIT_HOOK_RECORD_LOCKS=1'
WSEQUENCE='printf "resolved\n" >mail.txt; g add mail.txt; g am --continue'
hook_wrun "$am_repo" "$am_control_setup" refs-am-commit-rejected am --continue
WENV= WSEQUENCE=
hook_section=core

# pre-auto-gc is conditional on Git deciding that auto maintenance is due.
# A low threshold plus several loose unreachable objects forces that path.
gc_setup='hooks_install .git local pre-auto-gc; git config gc.auto 1; git config gc.autoDetach false; i=0; found=0; while [ $found -lt 3 ]; do oid=$(printf "loose-%s\n" "$i" | git hash-object --stdin); case $oid in 17*) printf "loose-%s\n" "$i" | git hash-object -w --stdin >/dev/null; found=$((found + 1));; esac; i=$((i + 1)); done'
hook_wrun "$commit_repo" "$gc_setup" pre-auto-gc gc --auto
WENV='NGIT_HOOK_FAIL=pre-auto-gc'
hook_wrun "$commit_repo" "$gc_setup" pre-auto-gc-failure gc --auto
WENV=

# Clone installs post-checkout from a template.  Use the same destination
# path for both tools so argv and hook $0 remain byte-comparable.
hook_clone_case() { # hook_clone_case LABEL [--no-checkout]
  [ "$hook_mode" = hooks ] || return 0
  clone_label=$1
  shift
  clone_src="$out/clone-source"
  clone_template="$out/clone-template"
  clone_dest="$out/clone-destination"
  if [ ! -d "$clone_src/.git" ]; then
    mkdir -p "$clone_src" "$clone_template/hooks"
    git -C "$clone_src" init -q -b main
    printf 'clone\n' >"$clone_src/clone.txt"
    git -C "$clone_src" add clone.txt
    git -C "$clone_src" commit -qm clone
    hooks_install_path "$clone_template/hooks" clone post-checkout
  fi
  selected "clone $clone_label $*" || return 0
  checks=$((checks + 1))
  rm -rf "$clone_dest"
  # shellcheck disable=SC2086
  env $WENV NGIT_HOOK_LOG="$clone_dest/.git/hooks.log" git clone --template="$clone_template" \
    "$@" "$clone_src" "$clone_dest" >"$out/git.clone.out" 2>"$out/git.clone.err"
  echo "exit $?" >>"$out/git.clone.out"
  {
    git -C "$clone_dest" rev-parse HEAD
    git -C "$clone_dest" ls-files -s
    cat "$clone_dest/.git/hooks.log" 2>/dev/null || :
  } >"$out/git.clone.state"
  rm -rf "$clone_dest"
  # shellcheck disable=SC2086
  env $WENV NGIT_HOOK_LOG="$clone_dest/.git/hooks.log" "$ngit" clone --template="$clone_template" \
    "$@" "$clone_src" "$clone_dest" >"$out/ngit.clone.out" 2>"$out/ngit.clone.err"
  echo "exit $?" >>"$out/ngit.clone.out"
  {
    git -C "$clone_dest" rev-parse HEAD
    git -C "$clone_dest" ls-files -s
    cat "$clone_dest/.git/hooks.log" 2>/dev/null || :
  } >"$out/ngit.clone.state"
  for clone_part in out err state; do
    if ! cmp -s "$out/git.clone.$clone_part" "$out/ngit.clone.$clone_part"; then
      echo "MISMATCH (clone $clone_label): $clone_part"
      diff "$out/git.clone.$clone_part" "$out/ngit.clone.$clone_part" | head -12
      fail=1
    fi
  done
}
if [ "$hook_mode" = hooks ]; then
  hook_clone_case post-checkout
  hook_clone_case no-checkout --no-checkout
  WENV='NGIT_HOOK_FAIL=post-checkout NGIT_HOOK_FAIL_STATUS=7'
  hook_clone_case post-checkout-failure
  WENV=
  hooks_install_path "$clone_template/hooks" clone post-checkout reference-transaction
  hook_clone_case reference-transactions
fi

WENV= WREFS= WSTDERR= WTREE= WMODES= WAFTER_REFRESH= WSEQUENCE= WHOOKS=
