;;; ngit-merge.el --- Merge commits and manage merge state -*- lexical-binding: t; -*-

;;; Commentary:

;; Command policy lives here; the tree merge API in ngit-merge-tree can
;; also be used by sequencer commands without changing HEAD or merge state.

;;; Code:

(require 'ngit-merge-tree)

(defun ngit--merge-file (repo name)
  (expand-file-name name (ngit-repo-gitdir repo)))

(defun ngit--merge-write (repo name data)
  (if (member name '("AUTO_MERGE" "CHERRY_PICK_HEAD" "REVERT_HEAD" "REBASE_HEAD" "ORIG_HEAD"))
      (ngit-ref-write repo name (ngit--trim-eol data))
    (ngit-write-file (ngit--merge-file repo name) data)))

(defun ngit--merge-orig-head (repo head)
  (ngit-ref-write repo "ORIG_HEAD" head))

(defun ngit--merge-snapshot-commit (repo tree parents message)
  (ngit-hex
   (ngit-write-object
    repo "commit"
    (concat "tree " (ngit-hex tree) "\n"
            (mapconcat (lambda (p) (concat "parent " p "\n")) parents "")
            "author " (ngit-ident repo "AUTHOR") "\ncommitter " (ngit-ident repo "COMMITTER")
            "\n\n" message))))

(defun ngit--merge-save-state (repo head staged)
  "Write merge.c's temporary stash objects, without changing refs or files."
  (let* ((wt (ngit-wt-changes repo t)) (index (ngit-read-index repo))
         (old (aref index 0)))
    (when (or wt staged)
      (let* ((raw (ngit-read-ref repo "HEAD"))
             (branch (if (and raw (string-prefix-p "ref: refs/heads/" raw))
                         (substring raw 16) "(no branch)"))
             (label (concat branch ": " (ngit-abbrev repo head) " " (ngit--subject-of repo head)))
             (itree (ngit-ct-update repo (ngit-ct-new) old))
             (icommit (ngit--merge-snapshot-commit repo itree (list head) (concat "index on " label "\n")))
             (targets nil) (dropped (make-hash-table :test 'equal)))
        (let ((intent (seq-find (lambda (e) (memq 'ita (aref e 5))) old)))
          (when intent
            (ngit--die "error: Entry '%s' not uptodate. Cannot merge.\nCannot save the current worktree state\nfatal: stash failed"
                       (aref intent 0))))
        (dolist (c wt)
          (let ((path (aref c 0)))
            (puthash path t dropped)
            (when (/= (aref c 4) 0)
              (push (ngit--switch-entry
                     path (aref c 4)
                     (if (aref c 7) (ngit-write-object repo "blob" (aref c 7)) (aref c 5)) nil)
                    targets))))
        (let ((tree (ngit-ct-update repo (ngit-ct-new) (ngit-index-overlay old targets dropped))))
          (ngit--merge-snapshot-commit repo tree (list head icommit) (concat "WIP on " label))
          tree)))))

(defun ngit--merge-incoming (repo head remote)
  "Return REMOTE's commits unreachable from HEAD, in revision order."
  (let ((excluded (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal)) (stack (list head))
        (queue nil) (out nil))
    (while stack
      (let ((hex (pop stack)))
        (unless (gethash hex excluded)
          (puthash hex t excluded)
          (setq stack (append (ngit-commit-parents repo hex) stack)))))
    (setq queue (list (ngit-parse-commit-object repo (ngit-commit remote))))
    (while queue
      (let* ((c (pop queue)) (hex (ngit--c-hex c)))
        (unless (or (gethash hex seen) (gethash hex excluded))
          (puthash hex t seen)
          (push c out)
          (dolist (p (ngit--c-parents c))
            (ngit-parse-commit-object repo p)
            (setq queue (ngit--insert-by-date p queue))))))
    (nreverse out)))

(defun ngit--merge-title (repo name)
  "Format fmt-merge-msg's title for a single NAME."
  (let* ((raw (ngit-read-ref repo "HEAD"))
         (branch (if (and raw (string-prefix-p "ref: refs/heads/" raw))
                     (substring raw 16) "HEAD"))
         (kind (cond ((ngit-resolve-ref repo (concat "refs/heads/" name)) "branch")
                     ((string-prefix-p "refs/heads/" name) "branch")
                     ((ngit-resolve-ref repo (concat "refs/tags/" name)) "tag")
                     ((ngit-resolve-ref repo (concat "refs/remotes/" name)) "remote-tracking branch")
                     (t "commit"))))
    (concat "Merge " kind " '" name "'"
            (if (member branch '("main" "master")) "" (concat " into " branch)) "\n")))

(defun ngit--merge-message (repo name head remote messages log-count)
  (let ((msg (if messages (ngit-stripspace (mapconcat #'identity messages "\n\n"))
               (ngit--merge-title repo name))))
    (when (> log-count 0)
      (let ((commits nil) (i 0))
        (dolist (c (ngit--merge-incoming repo head remote))
          (unless (cdr (ngit--c-parents c)) (push c commits)))
        (setq commits (nreverse commits))
        (setq msg (concat msg "\n* " name ":"
                          (if (> (length commits) log-count)
                              (format " (%d commits)" (length commits)) "") "\n"))
        (dolist (c commits)
          (when (< i log-count)
            (setq msg (concat msg "  " (ngit--subject (ngit--c-message c)) "\n")))
          (setq i (1+ i)))
        (when (> i log-count) (setq msg (concat msg "  ...\n")))))
    (if (multibyte-string-p msg) (encode-coding-string msg 'utf-8) msg)))

(defun ngit--merge-squash-message (repo head remote)
  (let ((msg "Squashed commit of the following:\n")
        (ngit--rv-repo repo))
    (dolist (c (ngit--merge-incoming repo head remote))
      (setq msg (concat msg "\ncommit " (ngit--c-hex c) "\n"
                        (ngit-pretty-builtin c 'medium '(normal)))))
    (ngit--merge-write repo "SQUASH_MSG" msg)
    (ngit--emit "Squash commit -- not updating HEAD\n")))

(defun ngit--merge-state (repo remote message no-ff conflicts squash &optional conflicted)
  "Save the state used by commit and append the conflict hint."
  (unless squash
    (ngit--merge-write repo "MERGE_HEAD" (concat remote "\n"))
    (ngit--merge-write repo "MERGE_MODE" (if no-ff "no-ff" ""))
    (ngit--merge-write repo "MERGE_MSG" message))
  (when (or conflicts conflicted)
    (ngit--merge-write
     repo "MERGE_MSG"
     (concat (if squash "" message) "\n# Conflicts:\n"
             (mapconcat (lambda (p) (concat "#\t" p "\n")) conflicts "")))))

(defun ngit--merge-stat (repo old new)
  (let ((changes (ngit-detect-renames (ngit-diff-trees repo old new))))
    (ngit--emit-stat (ngit--stat-entries repo changes))
    (ngit--emit-summary changes)))

(defun ngit--merge-commit (repo head remote tree message name no-ff no-verify)
  "Write an automatic two-parent merge commit and update HEAD."
  (let* ((message (ngit-stripspace message))
         (committer (ngit-ident repo "COMMITTER"))
         (author (ngit-ident repo "AUTHOR"))
         (index-file (ngit-index-file repo))
         (message-file (ngit--merge-file repo "MERGE_MSG"))
         (action (or (getenv "GIT_REFLOG_ACTION") (concat "merge " name)))
         (pre-ran (and (not no-verify)
                       (ngit-hook-exists-p repo "pre-merge-commit")))
         (failed nil))
    ;; Merge hooks see the in-progress state and may update the real index.
    (ngit--merge-state repo remote message no-ff nil nil)
    (when (and pre-ran
               (/= (ngit-commit-run-hook repo "pre-merge-commit" index-file nil
                                          nil nil action) 0))
      (setq failed t))
    (when pre-ran
      ;; Git re-reads the hook-updated index, but the result tree for an
      ;; automatic merge was written before prepare_to_commit runs.
      (ngit-commit-read-hook-index repo index-file))
    (unless failed
      (when (/= (ngit-commit-run-hook repo "prepare-commit-msg" index-file nil
                                      (list message-file "merge") nil action) 0)
        (setq failed t)))
    (unless (or failed no-verify)
      (when (/= (ngit-commit-run-hook repo "commit-msg" index-file nil
                                      (list message-file) nil action) 0)
        (setq failed t)))
    (if failed nil
      (setq message (ngit-commit-clean-hook-message
                     repo (ngit--read-file message-file) nil))
      (let* (
         (data (concat "tree " (ngit-hex tree) "\nparent " head "\nparent " remote
                       "\nauthor " author "\ncommitter " committer "\n\n" message))
             (hex (ngit-hex (ngit-write-object repo "commit" data))))
        (ngit-update-head repo hex head committer
                          (concat action ": Merge made by the 'ort' strategy."))
        (ngit-commit-clear-merge-state repo)
        hex))))

(defun ngit--merge-abort (repo &optional target)
  "Reset the merging index like reset --merge, preserving unrelated edits."
  (unless (or target (file-exists-p (ngit--merge-file repo "MERGE_HEAD")))
    (ngit--die "fatal: There is no merge to abort (MERGE_HEAD missing)."))
  (let* ((head (ngit-resolve-ref repo "HEAD"))
         (tree (ngit--switch-tree repo (or target head)))
         (index (ngit-read-index repo)) (old (aref index 0))
         (targets nil) (deletes nil) (dropped (make-hash-table :test 'equal))
         (i 0) (root (ngit-ct-new)))
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (while (< i (length old))
      (let* ((e (aref old i)) (path (aref e 0))
             (group (ngit-index-group old i))
             (target (ngit--literal-entry repo tree path))
             (same (and target (= (aref e 3) 0)
                        (= (aref e 2) (car target)) (equal (aref e 1) (cdr target)))))
        (unless same
          (when (and (= (aref e 3) 0)
                     (not (ngit--switch-worktree-clean-p repo index e)))
            (ngit--die (concat "error: Entry '%s' not uptodate. Cannot merge.\n"
                               "fatal: Could not reset index file to revision 'HEAD'.") path))
          (puthash path t dropped)
          (push e deletes))
        (setq i (car group))))
    (ngit--flatten-tree
     repo tree ""
     (lambda (path mode bin)
       (let ((e (ngit-index-lookup old path)))
         (unless (ngit--switch-same-entry-p e mode bin)
           (puthash path t dropped)
           (push (ngit--switch-entry path mode bin nil) targets)))))
    ;; reset --merge protects files created after a merge deleted their
    ;; indexed counterparts.  Check all targets before removing any path.
    (let ((untracked (car (ngit-add-untracked repo '("") (ngit--tracked-set old)))))
      (dolist (e targets)
        (when (and (not (ngit-index-lookup old (aref e 0)))
                   (ngit--switch-untracked-conflicts untracked (aref e 0)))
          (ngit--die
           (concat "error: Untracked working tree file '%s' would be overwritten by merge.\n"
                   "fatal: Could not reset index file to revision 'HEAD'.") (aref e 0)))))
    (let ((lock (ngit-lock (ngit-index-file repo)))
          (checked (make-hash-table :test 'eql)) (fresh nil))
      (unwind-protect
          (progn
            (dolist (e deletes) (ngit--unlink-entry repo e))
            (dolist (e (nreverse targets)) (push (ngit-checkout-entry repo e) fresh))
            (let ((entries (ngit-index-overlay old (nreverse fresh) dropped)))
              (aset index 0 entries)
              ;; reset --merge discards resolve-undo rather than recording
              ;; the aborted stages as newly resolved paths.
              (aset index 8 nil)
              (aset index 9 nil)
              (ngit--reset-refresh repo index entries checked t)
              (ngit-ct-update repo root entries)
              (ngit-commit-lock lock (ngit-index-bytes repo index entries root checked))
              (setq lock nil)))
        (when lock (ngit-unlock lock))))
    (aset repo 6 nil)
    (ngit--merge-orig-head repo head)
    (if (and target (not (equal target head)))
        (ngit-update-head repo target head (ngit-ident repo "COMMITTER")
                          (concat "reset: moving to " target))
      (ngit--append-reflog repo "HEAD" head head (ngit-ident repo "COMMITTER")
                           (concat "reset: moving to " (or target "HEAD"))))
    (ngit-commit-clear-merge-state repo)
    0))

;;;###autoload
(defun ngit-cmd-merge (repo args)
  (let ((quiet nil) (messages nil) (ff 'yes) (squash nil) (no-commit nil)
        (stat (ngit-config-bool repo "merge.stat" t)) (log-count 0)
        (abort nil) (continue nil) (explicit-commit nil) (snapshot nil) (names nil)
        (no-verify nil))
    (let ((config (ngit-config-get repo "merge.ff")))
      (cond ((equal config "false") (setq ff 'no))
            ((equal config "only") (setq ff 'only))))
    (while args
      (let ((a (pop args)))
        (cond
         ((member a '("-q" "--quiet")) (setq quiet t stat nil))
         ((string= a "-m") (push (or (pop args) (ngit--die-with 129 "error: switch `m' requires a value")) messages))
         ((string-prefix-p "--message=" a) (push (substring a 10) messages))
         ((and (string-prefix-p "-m" a) (> (length a) 2)) (push (substring a 2) messages))
         ((string= a "--ff") (setq ff 'yes))
         ((string= a "--no-ff") (setq ff 'no))
         ((string= a "--ff-only") (setq ff 'only))
         ((string= a "--squash") (setq squash t no-commit t))
         ((string= a "--no-commit") (setq no-commit t))
         ((string= a "--commit") (setq no-commit nil explicit-commit t))
         ((string= a "--stat") (setq stat t))
         ((member a '("-n" "--no-stat")) (setq stat nil))
         ((string= a "--log") (setq log-count 20))
         ((string-prefix-p "--log=" a) (setq log-count (string-to-number (substring a 6))))
         ((string= a "--no-log") (setq log-count 0))
         ((string= a "--abort") (setq abort t))
         ((string= a "--continue") (setq continue t))
         ((string= a "--no-verify") (setq no-verify t))
         ((string= a "--no-edit") nil)
         ((string= a "--") (setq names (append (nreverse names) args) args nil))
         ((string-prefix-p "-" a) (ngit--die-with 129 "error: merge option '%s' is not supported" a))
         (t (push a names)))))
    (setq names (nreverse names) messages (nreverse messages))
    (cond
     (abort (ngit--merge-abort repo))
     (continue
      (unless (file-exists-p (ngit--merge-file repo "MERGE_HEAD"))
        (ngit--die "fatal: There is no merge in progress (MERGE_HEAD missing)."))
      (ngit-cmd-commit repo nil))
     (t
      (when (aref (ngit-read-index repo) 9)
        (ngit--die
         (concat "error: Merging is not possible because you have unmerged files.\n"
                 (if (ngit-config-bool repo "advice.resolveConflict" t)
                     "hint: Fix them up in the work tree, and then use 'git add/rm <file>'\nhint: as appropriate to mark resolution and make a commit.\n" "")
                 "fatal: Exiting because of an unresolved conflict.")))
      (when (file-exists-p (ngit--merge-file repo "MERGE_HEAD"))
        (ngit--die (concat "fatal: You have not concluded your merge (MERGE_HEAD exists)."
                           (if (ngit-config-bool repo "advice.resolveConflict" t)
                               "\nPlease, commit your changes before you merge." ""))))
      (when (and squash (eq ff 'no)) (ngit--die "fatal: options '--squash' and '--no-ff.' cannot be used together"))
      (when (and squash explicit-commit) (ngit--die "fatal: options '--squash' and '--commit.' cannot be used together"))
      (unless names (ngit--die "fatal: No remote for the current branch."))
      (when (cdr names) (ngit--die "ngit: octopus merges are not supported"))
      (let* ((name (car names)) (head (ngit-resolve-ref repo "HEAD"))
             (remote (condition-case nil (ngit-peel repo (ngit-rev-parse repo name) 'commit)
                       (error (ngit--die "merge: %s - not something we can merge" name))))
             (bases (and head (ngit-merge-bases repo (ngit-commit head) (list (ngit-commit remote)))))
             (ours (ngit--switch-tree repo head)) (theirs (ngit--switch-tree repo remote))
             (action (or (getenv "GIT_REFLOG_ACTION") (concat "merge " name))))
        (ngit-hook-setenv "GIT_REFLOG_ACTION" action)
        (when head (ngit--merge-orig-head repo head))
        (cond
         ((and head (or (equal head remote)
                        (and (= (length bases) 1) (equal remote (ngit--c-hex (car bases))))))
          (unless quiet (ngit--emit (if squash "Already up to date. (nothing to squash)\n" "Already up to date.\n")))
          0)
         ((and head (not bases)) (ngit--die "fatal: refusing to merge unrelated histories"))
         ((and (not (eq ff 'no))
               (or (null head) (and (= (length bases) 1) (equal head (ngit--c-hex (car bases))))))
          (ngit-ident repo "COMMITTER")
          (when (and head (not quiet))
            (ngit--emit "Updating " (ngit-abbrev repo head) ".." (ngit-abbrev repo remote) "\n"))
          (ngit--merge-fast-forward repo head remote ours theirs action messages squash quiet stat)
          0)
         ((eq ff 'only)
          (ngit--die (concat
                      (if (ngit-config-bool repo "advice.diverging" t)
                          "hint: Diverging branches can't be fast-forwarded, you need to either:\nhint:\nhint: \tgit merge --no-ff\nhint:\nhint: or:\nhint:\nhint: \tgit rebase\nhint:\nhint: Disable this message with \"git config advice.diverging false\"\n" "")
                      "fatal: Not possible to fast-forward, aborting.")))
         (t
          (ngit-ident repo "COMMITTER")
          (unless no-commit (ngit-ident repo "AUTHOR"))
          (let ((staged (ngit-diff-tree-index repo ours)))
            (setq snapshot (ngit--merge-save-state repo head staged))
            (when staged
              (when snapshot (ngit--merge-write repo "AUTO_MERGE" (concat (ngit-hex snapshot) "\n")))
              (ngit--append-reflog repo "HEAD" head head (ngit-ident repo "COMMITTER")
                                   (concat action ": updating HEAD"))
              (ngit--die-with
               2 "error: Your local changes to the following files would be overwritten by merge:\n  %s\nMerge with strategy ort failed."
               (mapconcat (lambda (c) (ngit-quote-path (aref c 0))) staged " "))))
          (let* ((message (ngit--merge-message repo name head remote messages log-count))
                 (base (ngit-merge-base-tree repo head remote))
                 (ngit-merge-ancestor-label
                  (if (= (length bases) 1) (ngit-abbrev repo (ngit--c-hex (car bases)))
                    "merged common ancestors"))
                 (result (ngit-merge-trees repo base ours theirs "HEAD" name))
                 (conflicts (plist-get result :conflicts))
                 (conflicted (or conflicts (plist-get result :conflicted))))
            (condition-case err
                (ngit-merge-apply repo result ours quiet)
              (ngit-fatal
               (if (equal (cadr err) "Aborting")
                   (progn
                     (when snapshot (ngit--merge-write repo "AUTO_MERGE" (concat (ngit-hex snapshot) "\n")))
                     (ngit--die-with 2 "Aborting\nMerge with strategy ort failed."))
                 (signal (car err) (cdr err)))))
            (when squash (ngit--merge-squash-message repo head remote))
            (if (or conflicted no-commit)
                (progn
                  (ngit--merge-state repo remote message (eq ff 'no) conflicts squash conflicted)
                  (if conflicted
                      (progn (ngit--emit "Automatic merge failed; fix conflicts and then commit the result.\n") 1)
                    (ngit-native-write-stderr "Automatic merge went well; stopped before committing as requested\n")
                    (when squash (ngit-hook-run repo "post-merge" '("1")))
                    0))
              (let ((tree (plist-get result :tree)))
                (if (not (ngit--merge-commit repo head remote tree message name
                                             (eq ff 'no) no-verify))
                    (progn
                      (ngit-native-write-stderr "Not committing merge; use 'git commit' to complete the merge.\n")
                      1)
                  (unless quiet (ngit--emit "Merge made by the 'ort' strategy.\n"))
                  (when stat (ngit--merge-stat repo ours tree))
                  (ngit-hook-run repo "post-merge" '("0"))
                  0)))))))))))

(defun ngit--merge-fast-forward (repo head remote ours theirs action messages squash quiet stat)
  "Run unpack-trees' fast-forward without creating a merge commit."
  (ngit--switch-twoway repo ours theirs "merge")
  (unless quiet
    (ngit--emit (if messages "Fast-forward (no commit created; -m option ignored)\n" "Fast-forward\n")))
  (if squash
      (ngit--merge-squash-message repo head remote)
    (ngit-update-head repo remote head (ngit-ident repo "COMMITTER") (concat action ": Fast-forward")))
  (when stat (ngit--merge-stat repo ours theirs))
  (unless squash (ngit-commit-clear-merge-state repo))
  (ngit-hook-run repo "post-merge" (list (if squash "1" "0"))))

(provide 'ngit-merge)

;;; ngit-merge.el ends here
