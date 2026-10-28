;;; ngit-sequencer.el --- Commit replay and the merge rebase backend -*- lexical-binding: t; -*-

;;; Commentary:

;; State files follow Git 2.47's sequencer.c.  Replay shares the tree merge
;; engine with merge; command state never becomes a second source of refs.

;;; Code:

(require 'ngit-merge)

(defun ngit-rebase-action ()
  (or (getenv "GIT_REFLOG_ACTION") "rebase"))

(defun ngit-seq-read (repo name)
  (let ((path (ngit--merge-file repo name)))
    (and (file-exists-p path) (ngit--read-file path))))

(defun ngit-seq-value (repo name)
  (let ((value (ngit-seq-read repo name)))
    (and value (string-trim-right value))))

(defun ngit-seq-remove (repo name)
  (if (member name '("AUTO_MERGE" "CHERRY_PICK_HEAD" "REVERT_HEAD" "REBASE_HEAD"))
      (ngit-ref-delete-state repo name)
    (let ((path (ngit--merge-file repo name)))
    (when (file-exists-p path)
      (if (file-directory-p path) (ngit--switch-delete-directory-tree path)
        (ngit-delete-file path))))))

(defun ngit-seq-clear (repo)
  (ngit-commit-clear-merge-state repo))

(defun ngit-seq-commit-object (repo hex)
  (ngit-parse-commit-object repo (ngit-commit hex)))

(defun ngit-seq-resolve (repo name)
  (condition-case nil (ngit-peel repo (ngit-rev-parse repo name) 'commit)
    (error (ngit--die "fatal: bad revision '%s'" name))))

(defun ngit-seq-tree (repo hex)
  (and hex (ngit--commit-tree-of repo hex)))

(defun ngit-seq-local-boundary (repo start boundary)
  "Whether every ancestry path from START reaches BOUNDARY within 256 nodes."
  (let ((stack (list start)) (seen (make-hash-table :test 'eq))
        (count 0) (closed t))
    (while (and stack closed)
      (let ((c (pop stack)))
        (unless (or (eq c boundary) (gethash c seen))
          (puthash c t seen)
          (setq count (1+ count))
          (ngit-parse-commit-object repo c)
          (if (or (> count 256) (null (ngit--c-parents c)))
              (setq closed nil)
            (setq stack (append (ngit--c-parents c) stack))))))
    closed))

(defun ngit-seq-commits (repo revisions revert)
  "Resolve REVISIONS, walking ranges once in replay order."
  (let ((pending nil) (range nil) (out nil) (empty-range nil)
        (ngit--rv-repo repo) (ngit--commit-table (make-hash-table :test 'equal)))
    (dolist (rev revisions)
      (when (or (string-search ".." rev) (string-prefix-p "^" rev)
                (string-suffix-p "^!" rev)) (setq range t))
      (let ((next (ngit--handle-revision repo rev 0 pending)))
        (when (eq next 'not-a-rev) (ngit--die "fatal: bad revision '%s'" rev))
        (setq pending next)))
    (when (and range (null (cdr revisions)) (= (length pending) 2)
               (string-search ".." (car revisions)) (not (string-search "..." (car revisions))))
      (let ((a (car (cadr pending))) (b (car (car pending))))
        (cond
         ((ngit-seq-local-boundary repo b a)
          ;; These commit vectors are private to this walk.  No path can
          ;; escape below A, so the excluded boundary need not load older
          ;; ancestors just because their dates are newer than the tips.
          (ngit-parse-commit-object repo a)
          (aset a 2 nil))
         ((ngit-seq-local-boundary repo a b) (setq empty-range t)))))
    (if empty-range nil
     (if (not range)
        (mapcar (lambda (p) (ngit--c-hex (car p))) (nreverse pending))
      (let ((ngit--rv-commits nil) (ngit--rv-limited nil) (ngit--rv-first-parent nil)
            (ngit--rv-paths nil) (ngit--rv-topo t) (ngit--rv-max-age nil)
            (ngit--rv-min-age nil) (ngit--rv-max-parents -1) (ngit--rv-min-parents 0)
            (ngit--rv-max-count -1) (ngit--rv-skip 0) (ngit--rv-grep nil)
            (ngit--rv-no-walk nil) (ngit--rv-reverse nil) (ngit--rv-rewrite-parents nil)
            (ngit--rv-simplify-history nil) (ngit--rv-dense t) c)
        (ngit-walk-prepare repo pending)
        (while (setq c (ngit-walk-next)) (push (ngit--c-hex c) out)))
      (if revert (nreverse out) out)))))

(defun ngit-seq-todo (repo commits action &optional full)
  (mapconcat (lambda (hex)
               (concat (if (equal action "revert") "revert" "pick") " "
                       (if full hex (ngit-abbrev repo hex)) " "
                       (ngit--subject-of repo hex) "\n")) commits ""))

(defun ngit-seq-parse-todo (repo text)
  (let ((out nil))
    (dolist (line (split-string (or text "") "\n" t))
      (unless (string-prefix-p "#" line)
        (let ((words (split-string line " " t)))
          (when (cadr words) (push (ngit-seq-resolve repo (cadr words)) out)))))
    (nreverse out)))

(defun ngit-seq-hint (repo message advice)
  (when (ngit-config-bool repo (concat "advice." advice) t)
    (dolist (line (split-string message "\n"))
      (ngit-native-write-stderr (concat "hint: " line "\n")))
    (ngit-native-write-stderr
     (concat "hint: Disable this message with \"git config advice." advice " false\"\n"))))

(defun ngit-seq-conflict-hint (repo action no-commit rebase)
  (ngit-seq-hint
   repo
   (cond (rebase
          (concat "Resolve all conflicts manually, mark them as resolved with\n"
                  "\"git add/rm <conflicted_files>\", then run \"git rebase --continue\".\n"
                  "You can instead skip this commit: run \"git rebase --skip\".\n"
                  "To abort and get back to the state before \"git rebase\", run \"git rebase --abort\"."))
         (no-commit (concat "after resolving the conflicts, mark the corrected paths\n"
                            "with 'git add <paths>' or 'git rm <paths>'"))
         (t (concat "After resolving the conflicts, mark them with\n"
                    "\"git add/rm <pathspec>\", then run\n"
                    "\"git " action " --continue\".\n"
                    "You can instead skip this commit with \"git " action " --skip\".\n"
                    "To abort and get back to the state before \"git " action "\",\n"
                    "run \"git " action " --abort\"."))) "mergeConflict"))

(defun ngit-seq-unmerged (repo action)
  (when (aref (ngit-read-index repo) 9)
    (ngit-native-write-stderr
     (concat "error: " (if (equal action "revert") "Reverting" "Cherry-picking")
             " is not possible because you have unmerged files.\n"))
    (when (ngit-config-bool repo "advice.resolveConflict" t)
      (ngit-native-write-stderr
       "hint: Fix them up in the work tree, and then use 'git add/rm <file>'\nhint: as appropriate to mark resolution and make a commit.\n"))
    (ngit--die-with 128 "fatal: %s failed" action)))

(defun ngit-seq-message (repo c parent action record)
  (let* ((hex (ngit--c-hex c)) (subject (ngit--subject (ngit--c-message c)))
         (message (ngit--c-message c)))
    (if (equal action "revert")
        (concat (if (and (string-prefix-p "Revert \"" subject)
                         (not (string-prefix-p "Revert \"Revert \"" subject)))
                    (concat "Reapply \"" (substring subject 8))
                  (concat "Revert \"" subject "\""))
                "\n\nThis reverts commit " hex
                (if (cdr (ngit--c-parents c))
                    (concat ", reversing\nchanges made to " parent) "") ".\n")
      (if (not record) message
        (concat (string-trim-right message) "\n\n(cherry picked from commit " hex ")\n")))))

(defun ngit-seq-clean-message (message)
  (ngit-stripspace
   (mapconcat #'identity
              (seq-remove (lambda (s) (string-prefix-p "#" s)) (split-string message "\n")) "\n")))

(defun ngit-seq-run-rebase-commit-hook (repo name index-file author &optional args input)
  "Run rebase continuation commit hook NAME with git-commit's child environment."
  (if (not (ngit-hook-exists-p repo name)) 0
    (when (and args (member name '("prepare-commit-msg" "commit-msg")))
      (setq args (cons (ngit-hook-path-arg repo (car args)) (cdr args))))
    (let ((env (ngit-commit-hook-env
                (if (string-suffix-p ".lock" index-file)
                    index-file
                  (ngit-hook-path-arg repo index-file))
                author ngit-commit-replay-action)))
      ;; `git commit -e' inherits GIT_EDITOR instead of forcing the no-editor
      ;; value used by ordinary noninteractive commit hooks.
      (setq env (delete "GIT_EDITOR=:" env))
      (ngit-hook--run repo name args input env))))

(defun ngit-seq-commit (repo hex action message &optional quiet rebase continued template replay-tree)
  "Commit the current index, preserving the picked author and date."
  (catch 'ngit-seq-commit-result
   (let* ((head (ngit-resolve-ref repo "HEAD"))
         ;; A clean automatic replay already installed this exact tree.
         (index (unless replay-tree (ngit-read-index repo)))
         (entries (and index (aref index 0)))
         (root (and index (or (ngit-ct-root index) (ngit-ct-new))))
         (checked (make-hash-table :test 'eql))
         (author (if (equal action "revert") (ngit-ident repo "AUTHOR")
                   (ngit--c-author (ngit-seq-commit-object repo hex))))
         (index-file (ngit-index-file repo))
         (message-file (ngit--merge-file repo "COMMIT_EDITMSG"))
         (ngit-commit-replay-action
          (cond (rebase (concat (ngit-rebase-action) (if continued " (continue)" " (pick)")))
                (continued (if (equal action "cherry-pick") "commit (cherry-pick)" "commit"))
                (t action)))
         (ngit-commit-replay-date (and (not rebase) (or (not continued) (equal action "cherry-pick"))))
         ;; Automatic replay refreshed the index before its sequence.
         ;; A continuation follows commit.c and refreshes the user's index.
         (refresh (and continued (ngit-refresh-entries repo index entries checked)))
         (valid (or replay-tree (ngit-ct-fully-valid-p root)))
         (auto (ngit-seq-read repo "AUTO_MERGE"))
         (tree (or replay-tree (ngit-ct-update repo root entries)))
         (pre-ran (and continued (not rebase)
                       (ngit-hook-exists-p repo "pre-commit"))))
    (when (and continued (aref (ngit--hook-root-info repo) 0))
      (dolist (entry (ngit-commit-author-env author))
        (let ((eq (string-search "=" entry)))
          (ngit-hook-setenv (substring entry 0 eq) (substring entry (1+ eq))))))
    (when (or refresh (not valid))
      (let ((lock (ngit-lock index-file)))
        (ngit-commit-lock lock (ngit-index-bytes repo index entries root checked))))
    (when (and pre-ran
               (/= (ngit-commit-run-hook repo "pre-commit" index-file
                                          (and continued author)) 0))
      (ngit--die-with 1 ""))
    (when pre-ran
      (setq index (ngit-commit-read-hook-index repo index-file)
            entries (aref index 0)
            root (or (ngit-ct-root index) (ngit-ct-new))
            tree (ngit-ct-update repo root entries)))
    (when (or continued (ngit-hook-exists-p repo "prepare-commit-msg"))
      (ngit--merge-write repo "COMMIT_EDITMSG" (if continued (or template message) message))
      (when (/= (if (and continued rebase)
                    (ngit-seq-run-rebase-commit-hook
                     repo "prepare-commit-msg" index-file author
                     (list message-file "message"))
                  (ngit-commit-run-hook
                   repo "prepare-commit-msg" index-file (and continued author)
                   (if continued
                       (list message-file "merge")
                     (list message-file "message")))) 0)
        (cond
         (continued
          (throw 'ngit-seq-commit-result 1))
         (rebase
          (let ((line (ngit-seq-todo repo (list hex) "cherry-pick" t)))
            (ngit-native-write-stderr
             (concat "error: 'prepare-commit-msg' hook failed\n"
                     "hint: Could not execute the todo command\n"
                     "hint:\n"
                     "hint:     " (string-trim-right line) "\n"
                     "hint:\n"
                     "hint: It has been rescheduled; To edit the command before continuing, please\n"
                     "hint: edit the todo list first:\n"
                     "hint:\n"
                     "hint:     git rebase --edit-todo\n"
                     "hint:     git rebase --continue\n"))
            (ngit--merge-write repo "REBASE_HEAD" (concat hex "\n"))
            (ngit-rebase-write
             repo "git-rebase-todo"
             (concat line (or (ngit-seq-read repo "rebase-merge/git-rebase-todo") "")))
            (throw 'ngit-seq-commit-result 2)))
         (t
          (ngit--die-with 128 "error: 'prepare-commit-msg' hook failed\nfatal: %s failed"
                          action))))
      (when (and continued (not rebase)
                 (/= (ngit-commit-run-hook repo "commit-msg" index-file
                                          (and continued author)
                                          (list message-file)) 0))
        (ngit--die-with 1 ""))
      (setq message (ngit-commit-clean-hook-message
                     repo (ngit--read-file message-file) (and template t))))
    (if (and continued (equal tree (ngit-seq-tree repo head)))
        (progn
          (ngit-cmd-status repo nil)
          (when (equal action "cherry-pick")
            (ngit-native-write-stderr
             "The previous cherry-pick is now empty, possibly due to conflict resolution.\nIf you wish to commit it anyway, use:\n\n    git commit --allow-empty\n\nOtherwise, please use 'git cherry-pick --skip'\n"))
          1)
      (ngit--finish-commit repo head (list head) tree (if continued (ngit-seq-clean-message message) message)
                           nil quiet nil index-file (ngit-ident repo "COMMITTER") author
                           nil (and continued rebase) (not continued) (not continued))
      (when (and continued rebase)
        (ngit-seq-run-rebase-commit-hook repo "post-commit" index-file author))
      (if continued
          (ngit-seq-clear repo)
        (ngit-ref-delete-state repo "CHERRY_PICK_HEAD")
        (ngit-seq-remove repo "MERGE_MSG"))
      0))))

(defun ngit-seq-pick (repo hex action opts &optional rebase)
  "Replay HEX through the tree merger; return nonzero on a conflict."
  (let* ((c (ngit-seq-commit-object repo hex)) (parents (ngit-commit-parents repo hex))
         (mainline (or (plist-get opts :mainline) 0))
         (no-commit (plist-get opts :no-commit)) (record (plist-get opts :record))
         (parent (if (> mainline 0) (nth (1- mainline) parents) (car parents)))
         (head (ngit-resolve-ref repo "HEAD"))
         (ours (if no-commit (ngit-ct-update repo (ngit-ct-new) (ngit-index-entries repo))
                 (ngit-seq-tree repo head)))
         (revert (equal action "revert"))
         (base (ngit-seq-tree repo (if revert hex parent)))
         (next (ngit-seq-tree repo (if revert parent hex)))
         (label (concat (ngit-abbrev repo hex) " (" (ngit--subject (ngit--c-message c)) ")"))
         (ngit-merge-ancestor-label (if revert label (concat "parent of " label)))
         (message (ngit-seq-message repo c parent action record))
         (result nil))
    (when (and (cdr parents) (= mainline 0))
      (ngit--die-with 128 "error: commit %s is a merge but no -m option was given.\nfatal: %s failed" hex action))
    (when (and (> mainline 0) (not parent))
      (ngit--die-with 128 "error: commit %s does not have parent %d\nfatal: %s failed" hex mainline action))
    (setq result (ngit-merge-trees repo base ours next "HEAD"
                                  (if revert (concat "parent of " label) label)))
    (ngit-merge-apply repo result ours (and rebase 'rebase))
    (when (or (plist-get result :conflicted) (plist-get result :conflicts))
      (setq message (concat message "\n# Conflicts:\n"
                            (mapconcat (lambda (p) (concat "#\t" p "\n"))
                                       (plist-get result :conflicts) ""))))
    (ngit--merge-write repo "MERGE_MSG" message)
    (cond ((and (not revert) (not no-commit))
           (ngit--merge-write repo "CHERRY_PICK_HEAD" (concat hex "\n")))
          ((and revert (or no-commit (plist-get result :conflicted) (plist-get result :conflicts)))
           (ngit--merge-write repo "REVERT_HEAD" (concat hex "\n"))))
    (cond
     ((or (plist-get result :conflicted) (plist-get result :conflicts))
      (when rebase (ngit-ref-delete-state repo "CHERRY_PICK_HEAD"))
      (ngit-native-write-stderr (concat "error: could not " (if revert "revert " "apply ")
                                       (ngit-abbrev repo hex) "... " (ngit--subject (ngit--c-message c)) "\n"))
      (ngit-seq-conflict-hint repo action no-commit rebase)
      1)
     (no-commit
      0)
     ((and (equal (plist-get result :tree) ours) (not (and rebase (equal base next))))
      (if rebase
          (progn
            (ngit-native-write-stderr
             (concat "dropping " hex " " (ngit--subject (ngit--c-message c)) " -- patch contents already upstream\n"))
            (ngit-seq-remove repo "MERGE_MSG") 0)
        (ngit--merge-write repo "COMMIT_EDITMSG" message)
        (ngit-cmd-status repo nil)
        (when revert (ngit-seq-remove repo "REVERT_HEAD"))
        (unless revert
          (ngit-native-write-stderr
           "The previous cherry-pick is now empty, possibly due to conflict resolution.\nIf you wish to commit it anyway, use:\n\n    git commit --allow-empty\n\nOtherwise, please use 'git cherry-pick --skip'\n"))
        1))
     (t (ngit-seq-commit repo hex action message rebase rebase nil nil
                         (plist-get result :tree))))))

(defun ngit-seq-refresh (repo)
  "Refresh carried entries once, as sequencer.c read_and_refresh_cache does."
  (let* ((index (ngit-read-index repo)) (entries (aref index 0))
         (checked (make-hash-table :test 'eql)))
    (when (ngit-refresh-entries repo index entries checked)
      (ngit-write-index repo index entries (ngit-ct-root index) checked))))

(defun ngit-seq-run (repo commits action opts directory)
  (ngit-seq-refresh repo)
  (let ((status 0))
    (while (and commits (= status 0))
      (when directory (ngit--merge-write repo "sequencer/todo" (ngit-seq-todo repo commits action)))
      (setq status (ngit-seq-pick repo (car commits) action opts))
      (when directory
        (ngit--merge-write repo "sequencer/abort-safety" (concat (ngit-resolve-ref repo "HEAD") "\n")))
      (setq commits (cdr commits)))
    (when (and directory (= status 0)) (ngit-seq-remove repo "sequencer"))
    status))

(defun ngit-seq-options (repo)
  (let ((text (or (ngit-seq-read repo "sequencer/opts") "")))
    (list :no-commit (and (string-search "no-commit = true" text) t)
          :record (and (string-search "record-origin = true" text) t)
          :mainline (let ((pos (string-search "mainline = " text)))
                      (and pos (string-to-number (substring text (+ pos 11))))))))

(defun ngit-seq-reset (repo target)
  "Reset replay changes, retaining unrelated unstaged work like reset --merge."
  ;; The merge abort installer already implements reset --merge's protection.
  (ngit--merge-abort repo target)
  (ngit-seq-clear repo))

(defun ngit-seq-control (repo action control)
  (let* ((directory (file-directory-p (ngit--merge-file repo "sequencer")))
         (pick (or (ngit-seq-value repo "CHERRY_PICK_HEAD") (ngit-seq-value repo "REVERT_HEAD")))
         (todo (and directory (ngit-seq-parse-todo repo (ngit-seq-read repo "sequencer/todo"))))
         (opts (ngit-seq-options repo)))
    (cond
     ((equal control "--quit") (ngit-seq-clear repo) (ngit-seq-remove repo "sequencer") 0)
     ((not (or directory pick))
      (ngit--die-with 128 "error: no %s in progress\nfatal: %s failed"
                      (if (equal control "--skip") action "cherry-pick or revert") action))
     ((equal control "--abort")
      (let ((target (or (ngit-seq-value repo "sequencer/head") (ngit-resolve-ref repo "HEAD"))))
        (if (and directory (not (equal (ngit-seq-value repo "sequencer/abort-safety")
                                      (ngit-resolve-ref repo "HEAD"))))
            (ngit-native-write-stderr "warning: You seem to have moved HEAD. Not rewinding, check your HEAD!\n")
          (ngit-seq-reset repo target))
        (ngit-seq-remove repo "sequencer") 0))
     ((equal control "--skip")
      (when (and directory (not pick)
                 (not (equal (ngit-seq-value repo "sequencer/abort-safety") (ngit-resolve-ref repo "HEAD"))))
        (ngit--die-with 128 "error: there is nothing to skip\nhint: have you committed already?\nhint: try \"git %s --continue\"\nfatal: %s failed" action action))
      (ngit-seq-reset repo (ngit-resolve-ref repo "HEAD"))
      (if directory (ngit-seq-run repo (cdr todo) action opts t) 0))
     (t
      (if pick (ngit--commit-check-unmerged repo (ngit-index-entries repo))
        (ngit-seq-unmerged repo action))
      (when (and directory (not pick)
                 (ngit-diff-tree-index repo (ngit-seq-tree repo (ngit-resolve-ref repo "HEAD"))))
        (ngit--die-with 128 "error: your local changes would be overwritten by %s.\nhint: commit your changes or stash them to proceed.\nfatal: %s failed" action action))
      (when pick
        (when (/= (ngit-seq-commit repo pick action (ngit-seq-read repo "MERGE_MSG") nil nil t) 0)
          (ngit--die-with 1 "")))
      (if directory (ngit-seq-run repo (cdr todo) action opts t) 0)))))

(defun ngit-seq-command (repo args action)
  (let ((opts nil) (control nil) (revisions nil))
    (while args
      (let ((arg (pop args)))
        (cond
         ((member arg '("--continue" "--abort" "--skip" "--quit")) (setq control arg))
         ((member arg '("-n" "--no-commit")) (setq opts (plist-put opts :no-commit t)))
         ((equal arg "-x") (setq opts (plist-put opts :record t)))
         ((member arg '("-m" "--mainline")) (setq opts (plist-put opts :mainline (string-to-number (or (pop args) "0")))))
         ((string-prefix-p "--mainline=" arg) (setq opts (plist-put opts :mainline (string-to-number (substring arg 11)))))
         ((equal arg "--no-edit") nil)
         ((string-prefix-p "-" arg) (ngit--die "ngit: %s: unsupported argument %s" action arg))
         (t (push arg revisions)))))
    (if control (ngit-seq-control repo action control)
      (ngit-seq-unmerged repo action)
      (unless revisions (ngit--die-with 129 "error: no commits specified"))
      (let* ((commits (ngit-seq-commits repo (nreverse revisions) (equal action "revert")))
             (directory (or (cdr commits) (seq-some (lambda (s) (string-search ".." s)) revisions))))
        (unless commits (ngit--die-with 128 "error: empty commit set passed\nfatal: %s failed" action))
        (when directory
          (when (file-exists-p (ngit--merge-file repo "sequencer"))
            (ngit--die-with 128 "error: %s is already in progress\nfatal: %s failed" action action))
          (ngit-make-directory (ngit--merge-file repo "sequencer") t)
          (ngit--merge-write repo "sequencer/head" (concat (ngit-resolve-ref repo "HEAD") "\n"))
          (let ((lines nil))
            (when (plist-get opts :no-commit) (push "\tno-commit = true\n" lines))
            (when (plist-get opts :record) (push "\trecord-origin = true\n" lines))
            (when (plist-get opts :mainline) (push (format "\tmainline = %d\n" (plist-get opts :mainline)) lines))
            (when lines (ngit--merge-write repo "sequencer/opts" (concat "[options]\n" (apply #'concat (nreverse lines)))))))
        (ngit-seq-run repo commits action opts directory)))))

;;;###autoload
(defun ngit-cmd-cherry-pick (repo args) (ngit-seq-command repo args "cherry-pick"))

;;;###autoload
(defun ngit-cmd-revert (repo args) (ngit-seq-command repo args "revert"))

(defun ngit-seq-without-space (text)
  (dolist (space '(" " "\t" "\r" "\n" "\v" "\f"))
    (setq text (string-replace space "" text)))
  text)

(defun ngit-seq-patch-id (repo hex)
  "Return diff.c's stable, whitespace-insensitive patch id for HEX."
  (let* ((c (ngit-seq-commit-object repo hex))
         (parent (car (ngit--c-parents c)))
         (sum (make-vector 20 0))
         (changes (unless (cdr (ngit--c-parents c))
                    (ngit-diff-trees repo (and parent (ngit-seq-tree repo (ngit--c-hex parent)))
                                     (ngit-seq-tree repo hex)))))
    (when changes
      (dolist (change changes)
        (let* ((path (ngit-seq-without-space (aref change 0)))
               (om (aref change 2)) (nm (aref change 4))
               (old (ngit--diff-blob repo om (aref change 3)))
               (new (ngit--diff-blob repo nm (aref change 5)))
               (parts (list (concat "diff--gita/" path "b/" path)))
               (carry 0) (i 0))
          (cond ((= om 0) (push (format "newfilemode%06o" nm) parts))
                ((= nm 0) (push (format "deletedfilemode%06o" om) parts))
                ((/= om nm) (push (format "oldmode%06onewmode%06o" om nm) parts)))
          (if (or (ngit--binary-p repo (aref change 0) old)
                  (ngit--binary-p repo (aref change 0) new))
              (push (concat (if (aref change 3) (ngit-hex (aref change 3)) (make-string 40 ?0))
                            (if (aref change 5) (ngit-hex (aref change 5)) (make-string 40 ?0))) parts)
            (push (concat (if (= om 0) "---/dev/null" (concat "---a/" path))
                          (if (= nm 0) "+++/dev/null" (concat "+++b/" path))) parts)
            (let* ((lines nil) (a (ngit-xdiff--split old)) (b (ngit-xdiff--split new))
                   (script (ngit-xdiff--diff-script a b nil)))
              (dolist (line (split-string (if script (ngit-xdiff--emit a b script 3) "") "\n"))
                (unless (or (string-prefix-p "@@" line) (string-prefix-p "\\ " line))
                  (push line lines)))
              (push (ngit-seq-without-space (apply #'concat (nreverse lines))) parts)))
          (let ((digest (ngit-unhex (secure-hash 'sha1 (apply #'concat (nreverse parts))))))
            ;; Git sums the twenty hash octets with little-endian carry.
            (while (< i 20)
              (setq carry (+ carry (aref sum i) (aref digest i)))
              (aset sum i (logand carry 255))
              (setq carry (ash carry -8) i (1+ i))))))
      (apply #'unibyte-string (append sum nil)))))

(defconst ngit-rebase-todo-help
  (concat "#\n# Commands:\n"
          "# p, pick <commit> = use commit\n"
          "# r, reword <commit> = use commit, but edit the commit message\n"
          "# e, edit <commit> = use commit, but stop for amending\n"
          "# s, squash <commit> = use commit, but meld into previous commit\n"
          "# f, fixup [-C | -c] <commit> = like \"squash\" but keep only the previous\n"
          "#                    commit's log message, unless -C is used, in which case\n"
          "#                    keep only this commit's message; -c is same as -C but\n"
          "#                    opens the editor\n"
          "# x, exec <command> = run command (the rest of the line) using shell\n"
          "# b, break = stop here (continue rebase later with 'git rebase --continue')\n"
          "# d, drop <commit> = remove commit\n"
          "# l, label <label> = label current HEAD with a name\n"
          "# t, reset <label> = reset HEAD to a label\n"
          "# m, merge [-C <commit> | -c <commit>] <label> [# <oneline>]\n"
          "#         create a merge commit using the original merge commit's\n"
          "#         message (or the oneline, if no original merge commit was\n"
          "#         specified); use -c <commit> to reword the commit message\n"
          "# u, update-ref <ref> = track a placeholder for the <ref> to be updated\n"
          "#                       to this position in the new commits. The <ref> is\n"
          "#                       updated at the end of the rebase\n"
          "#\n# These lines can be re-ordered; they are executed from top to bottom.\n"
          "#\n# If you remove a line here THAT COMMIT WILL BE LOST.\n"
          "#\n# However, if you remove everything, the rebase will be aborted.\n#\n"))

(defun ngit-rebase-write (repo name value)
  (ngit--merge-write repo (concat "rebase-merge/" name) value))

(defun ngit-rebase-value (repo name)
  (ngit-seq-value repo (concat "rebase-merge/" name)))

(defun ngit-seq-sq-quote (string)
  "Quote STRING like Git's sq_quote_buf."
  (concat "'" (string-replace "'" "'\\''" string) "'"))

(defun ngit-rebase-author-script (author)
  (let* ((close (string-search "> " author))
         (open (string-search " <" author)))
    (concat "GIT_AUTHOR_NAME=" (ngit-seq-sq-quote (substring author 0 open)) "\n"
            "GIT_AUTHOR_EMAIL=" (ngit-seq-sq-quote (substring author (+ open 2) close)) "\n"
            "GIT_AUTHOR_DATE=" (ngit-seq-sq-quote (concat "@" (substring author (+ close 2)))) "\n")))

(defun ngit-rebase-finish (repo)
  (let* ((ref (ngit-rebase-value repo "head-name"))
         (onto (ngit-rebase-value repo "onto"))
         (original (ngit-rebase-value repo "orig-head"))
         (auto (ngit-seq-read repo "AUTO_MERGE"))
         (head (ngit-resolve-ref repo "HEAD")))
    (unless (equal ref "detached HEAD")
      (let* ((file (ngit--ref-file repo ref)) (lock (ngit-lock file)) input)
        (unwind-protect
            (let ((actual (ngit-resolve-ref repo ref)))
              (unless (equal actual original)
                (ngit--die-with 1 "error: update_ref failed for ref '%s': cannot lock ref '%s': is at %s but expected %s"
                                ref ref actual original))
              (setq input (ngit-ref-prepare repo (list (list original head ref)) nil (list lock)))
              (ngit-commit-lock lock (concat head "\n"))
              (setq lock nil)
              (ngit--append-reflog repo ref original head (ngit-ident repo "COMMITTER")
                                   (concat (ngit-rebase-action) " (finish): " ref " onto " onto))
              (aset repo 4 nil)
              (ngit-ref-finish repo input))
          (when lock (ngit-unlock lock))))
      (ngit--switch-write-head repo ref head head (concat (ngit-rebase-action) " (finish): returning to " ref)))
    (let ((rewritten (ngit-seq-read repo "rebase-merge/rewritten-list")))
      (when (and rewritten (not (string= rewritten "")))
        (ngit-hook-run repo "post-rewrite" '("rebase") rewritten)))
    (ngit-seq-remove repo "rebase-merge")
    (ngit-native-write-stderr
     (concat "Successfully rebased and updated " (if (equal ref "detached HEAD") "detached HEAD" ref) ".\n"))
    0))

(defun ngit-rebase-run (repo commits)
  (let ((number (string-to-number (or (ngit-rebase-value repo "msgnum") "0")))
        (total (string-to-number (ngit-rebase-value repo "end")))
        (status 0))
    (while (and commits (= status 0))
      (let* ((hex (pop commits)) (c (ngit-seq-commit-object repo hex))
             (line (ngit-seq-todo repo (list hex) "cherry-pick" t))
             (head (ngit-resolve-ref repo "HEAD")))
        (setq number (1+ number))
        (ngit-rebase-write repo "git-rebase-todo" (ngit-seq-todo repo commits "cherry-pick" t))
        (ngit-write-file (ngit--merge-file repo "rebase-merge/done") line nil t)
        (ngit-rebase-write repo "msgnum" (format "%d\n" number))
        (ngit-native-write-stderr (format "Rebasing (%d/%d)\r" number total))
        (dolist (name '("AUTO_MERGE" "REBASE_HEAD" "rebase-merge/message"
                        "rebase-merge/stopped-sha" "rebase-merge/patch"))
          (ngit-seq-remove repo name))
        (ngit-rebase-write repo "author-script" (ngit-rebase-author-script (ngit--c-author c)))
        (if (equal head (car (ngit-commit-parents repo hex)))
            (progn
              (ngit--switch-protect-untracked repo (ngit-seq-tree repo head) (ngit-seq-tree repo hex) "rebase")
              (ngit--reset-hard repo (ngit-seq-tree repo hex) hex)
              (ngit-update-head repo hex head (ngit-ident repo "COMMITTER")
                                (concat (ngit-rebase-action) " (pick): " (ngit--subject-of repo hex))))
          (setq status (ngit-seq-pick repo hex "cherry-pick" nil t)))
        (if (= status 2)
            ;; A commit hook failure reschedules the command without
            ;; converting it into a patch-conflict stop.
            (setq status 1)
          (when (/= status 0)
            (ngit-rebase-write repo "stopped-sha" (concat hex "\n"))
            (ngit--merge-write repo "REBASE_HEAD" (concat hex "\n"))
            (ngit-rebase-write repo "message" (ngit-seq-read repo "MERGE_MSG"))
            (let ((ngit--out nil))
              (dolist (change (ngit-diff-trees repo
                                             (ngit-seq-tree repo (car (ngit-commit-parents repo hex)))
                                             (ngit-seq-tree repo hex)))
                ;; The sequencer's saved patch uses a nonrecursive tree diff.
                ;; Nested entries are represented by tree changes, not blobs.
                (unless (string-search "/" (aref change 0))
                  (ngit--emit-patch repo change 3)))
              (ngit-rebase-write repo "patch" (apply #'concat (nreverse ngit--out))))
            (ngit-native-write-stderr
             (concat "Could not apply " (ngit-abbrev repo hex) "... " (ngit--subject-of repo hex) "\n"))))
        (when (= status 0)
          (ngit-write-file (ngit--merge-file repo "rebase-merge/rewritten-list")
                           (concat hex " " (ngit-resolve-ref repo "HEAD") "\n") nil t))))
    (if (= status 0) (ngit-rebase-finish repo) status)))

(defun ngit-rebase-continue-template (repo message author)
  "Build commit.c's unchanged editor template for a resolved rebase."
  (let* ((done (ngit-seq-parse-todo repo (ngit-seq-read repo "rebase-merge/done")))
         (todo (ngit-seq-parse-todo repo (ngit-seq-read repo "rebase-merge/git-rebase-todo")))
         (ref (ngit-rebase-value repo "head-name"))
         (onto (ngit-abbrev repo (ngit-rebase-value repo "onto")))
         (count (length done)) (n (length todo)) (lines nil)
         (st (ngit-status-collect repo))
         (a (car (ngit--split-ident author)))
         (committer (car (ngit--split-ident (ngit-ident repo "COMMITTER")))))
    (push (concat "interactive rebase in progress; onto " onto "\n"
                  (if (= count 1) "Last command done (1 command done):\n"
                    (format "Last commands done (%d commands done):\n" count))) lines)
    (dolist (hex (last done (min count 2)))
      (push (concat "   pick " (ngit-abbrev repo hex) " " (ngit--subject-of repo hex) "\n") lines))
    (when (> count 2) (push "  (see more in file .git/rebase-merge/done)\n" lines))
    (if (= n 0) (push "No commands remaining.\n" lines)
      (push (format "Next command%s to do (%d remaining command%s):\n"
                    (if (= n 1) "" "s") n (if (= n 1) "" "s")) lines)
      (dolist (hex (seq-take todo 2))
        (push (concat "   pick " (ngit-abbrev repo hex) " " (ngit--subject-of repo hex) "\n") lines)))
    (push (concat "You are currently rebasing branch '" (if (string-prefix-p "refs/heads/" ref) (substring ref 11) ref)
                  "' on '" onto "'.\n\n") lines)
    (when (plist-get st :staged)
      (push "Changes to be committed:\n" lines)
      (dolist (change (plist-get st :staged))
        (push (ngit--status-line (aref change 1) (aref change 0) (aref change 8) "") lines))
      (push "\n" lines))
    (concat message "\n# Please enter the commit message for your changes. Lines starting\n"
            "# with '#' will be ignored, and an empty message aborts the commit.\n#\n"
            (if (equal a committer) "" (concat "# Author:    " a "\n#\n"))
            (mapconcat (lambda (line) (concat "#" (if (or (equal line "") (string-prefix-p "\t" line)) "" " ") line "\n"))
                       (butlast (split-string (apply #'concat (nreverse lines)) "\n")) ""))))

(defun ngit-rebase-control (repo control)
  (unless (file-directory-p (ngit--merge-file repo "rebase-merge"))
    (ngit--die "fatal: no rebase in progress"))
  (let ((head (ngit-resolve-ref repo "HEAD"))
        (original (ngit-rebase-value repo "orig-head"))
        (ref (ngit-rebase-value repo "head-name")))
    (cond
     ((equal control "--abort")
      (ngit--switch-protect-untracked repo nil (ngit-seq-tree repo original) "rebase")
      (ngit--reset-hard repo (ngit-seq-tree repo original) original)
      (unless (or (equal ref "detached HEAD") (equal (ngit-resolve-ref repo ref) original))
        (ngit--write-ref repo ref original (ngit-resolve-ref repo ref) (concat "rebase (abort): returning to " ref)))
      (ngit--switch-write-head repo (unless (equal ref "detached HEAD") ref) original head
                               (concat "rebase (abort): returning to " ref))
      (ngit-seq-clear repo) (ngit-seq-remove repo "REBASE_HEAD")
      (ngit-seq-remove repo "rebase-merge") 0)
     (t
      (if (equal control "--skip")
          (progn
            (ngit--switch-protect-untracked repo nil (ngit-seq-tree repo head) "rebase")
            (ngit--reset-hard repo (ngit-seq-tree repo head) head) (ngit-seq-clear repo))
        (when (or (aref (ngit-read-index repo) 9) (ngit-wt-changes repo t))
          (dolist (e (append (ngit-index-entries repo) nil))
            (when (= (aref e 3) 1) (ngit--emit (aref e 0) ": needs merge\n")))
          (ngit--emit "You must edit all merge conflicts and then\nmark them as resolved using git add\n")
          (ngit--die-with 1 ""))
        (let ((hex (ngit-rebase-value repo "stopped-sha")))
          (when (and hex (ngit-diff-tree-index repo (ngit-seq-tree repo head)))
            (let* ((message (ngit-seq-read repo "rebase-merge/message"))
                   (author (ngit--c-author (ngit-seq-commit-object repo hex)))
                   (template (ngit-rebase-continue-template repo message author))
                   (hook-root (aref (ngit--hook-root-info repo) 0))
                   (saved-author-env
                    (and hook-root
                         (mapcar (lambda (name) (cons name (getenv name)))
                                 '("GIT_AUTHOR_NAME" "GIT_AUTHOR_EMAIL" "GIT_AUTHOR_DATE"))))
                   (status (ngit-seq-commit repo hex "cherry-pick" message nil t t template)))
              (dolist (entry saved-author-env)
                (ngit-hook-setenv (car entry) (cdr entry)))
              (unless (= status 0)
                (ngit--die-with 1 "error: could not commit staged changes."))
              (ngit-write-file
               (ngit--merge-file repo "rebase-merge/rewritten-list")
               (concat hex " " (ngit-resolve-ref repo "HEAD") "\n") nil t)))))
      (ngit-rebase-run repo (ngit-seq-parse-todo repo (ngit-seq-read repo "rebase-merge/git-rebase-todo")))))))

;;;###autoload
(defun ngit-cmd-rebase (repo args)
  (let ((onto-name nil) (control nil) (names nil) (no-verify nil))
    (while args
      (let ((arg (pop args)))
        (cond ((equal arg "--onto") (setq onto-name (pop args)))
              ((string-prefix-p "--onto=" arg) (setq onto-name (substring arg 7)))
              ((member arg '("--continue" "--skip" "--abort")) (setq control arg))
              ((string= arg "--no-verify") (setq no-verify t))
              ((string= arg "--verify") (setq no-verify nil))
              ((string-prefix-p "-" arg) (ngit--die "ngit: rebase: unsupported argument %s" arg))
              (t (push arg names)))))
    (if control (ngit-rebase-control repo control)
      (when (file-directory-p (ngit--merge-file repo "rebase-merge"))
        (ngit--die "fatal: It seems that there is already a rebase-merge directory"))
      (setq names (nreverse names))
      (unless (car names) (ngit--die "ngit: rebase requires an upstream"))
      (let* ((upstream-name (car names)) (branch-name (cadr names))
             (upstream (ngit-seq-resolve repo upstream-name))
             (onto-name (or onto-name upstream-name))
             (onto (ngit-seq-resolve repo onto-name))
             (old (ngit-resolve-ref repo "HEAD"))
             (original (if branch-name (ngit-seq-resolve repo branch-name) old))
             (target-ref (and branch-name
                              (if (string-prefix-p "refs/heads/" branch-name) branch-name
                                (concat "refs/heads/" branch-name))))
             (raw (if branch-name
                      (if (ngit-resolve-ref repo target-ref) (concat "ref: " target-ref) original)
                    (ngit-read-ref repo "HEAD")))
             (ref (if (string-prefix-p "ref: " raw) (substring raw 5) "detached HEAD"))
             (branch (if (string-prefix-p "refs/heads/" ref) (substring ref 11) "HEAD"))
             (commits nil) (upstream-commits nil) (ids (make-hash-table :test 'equal))
             (skipped nil))
        (when (or (ngit-diff-tree-index repo (ngit-seq-tree repo old)) (ngit-wt-changes repo t))
          (ngit--die-with 1 "error: cannot rebase: You have unstaged changes.\nerror: Please commit or stash them."))
        (unless (equal ref "detached HEAD")
          (let ((path (ngit--branch-checked-out repo ref)))
            (when (and path (not (equal (file-name-as-directory path)
                                       (file-name-as-directory (ngit-repo-toplevel repo)))))
              (ngit--die "fatal: '%s' is already used by worktree at '%s'" branch path))))
        (setq commits (seq-remove
                       (lambda (hex) (cdr (ngit-commit-parents repo hex)))
                       (ngit-seq-commits repo (list (concat upstream ".." original)) nil)))
        (if (and (equal onto upstream)
                 (ngit--in-merge-bases repo (ngit-commit upstream) (ngit-commit original))
                 (not (seq-some (lambda (hex) (cdr (ngit-commit-parents repo hex)))
                                (mapcar (lambda (c) (ngit--c-hex c)) (ngit--merge-incoming repo upstream original)))))
            (progn
              (when branch-name
                (ngit--switch-protect-untracked repo (ngit-seq-tree repo old) (ngit-seq-tree repo original) "rebase")
                (ngit--reset-hard repo (ngit-seq-tree repo original) original)
                (ngit--switch-write-head repo (unless (equal ref "detached HEAD") ref) original old (concat "rebase: checkout " branch-name)))
              (ngit--emit (if (equal branch "HEAD") "HEAD is up to date.\n"
                            (concat "Current branch " branch " is up to date.\n"))) 0)
          (setq upstream-commits (ngit-seq-commits repo (list (concat original ".." upstream)) nil))
          (dolist (hex upstream-commits)
            (let ((id (ngit-seq-patch-id repo hex))) (when id (puthash id t ids))))
          (setq commits
                (seq-remove
                 (lambda (hex)
                   (let ((id (ngit-seq-patch-id repo hex)))
                     (when (and id (gethash id ids))
                       (setq skipped t)
                       (ngit-native-write-stderr (concat "warning: skipped previously applied commit " (ngit-abbrev repo hex) "\n")) t)))
                 commits))
          (when skipped (ngit-seq-hint repo "use --reapply-cherry-picks to include skipped commits" "skippedCherryPicks"))
          (when (and (not no-verify)
                     (/= (ngit-hook-run repo "pre-rebase"
                                        (delq nil (list upstream-name branch-name))
                                        nil)
                         0))
            (ngit--die-with 1 "error: The pre-rebase hook refused to rebase."))
          (ngit--switch-protect-untracked repo (ngit-seq-tree repo old) (ngit-seq-tree repo onto) "rebase")
          (ngit-make-directory (ngit--merge-file repo "rebase-merge") t)
          (dolist (name '("interactive" "no-reschedule-failed-exec" "drop_redundant_commits"))
            (ngit-rebase-write repo name ""))
          (ngit-rebase-write repo "head-name" (concat ref "\n"))
          (ngit-rebase-write repo "onto" (concat onto "\n"))
          (ngit-rebase-write repo "orig-head" (concat original "\n"))
          (ngit-rebase-write repo "end" (format "%d\n" (length commits)))
          (let ((todo (if commits (ngit-seq-todo repo commits "cherry-pick" t) "noop\n")))
            (ngit-rebase-write repo "git-rebase-todo" todo)
            (ngit-rebase-write repo "git-rebase-todo.backup"
                               (concat todo "\n# Rebase " (ngit-abbrev repo upstream) ".." (ngit-abbrev repo original)
                                       " onto " (ngit-abbrev repo onto) (format " (%d command%s)\n" (length commits) (if (= (length commits) 1) "" "s"))
                                       ngit-rebase-todo-help)))
          (ngit--reset-hard repo (ngit-seq-tree repo onto) onto)
          (ngit--write-ref repo "ORIG_HEAD" original (ngit-resolve-ref repo "ORIG_HEAD") nil)
          (ngit--switch-write-head repo nil onto old (concat (ngit-rebase-action) " (start): checkout " onto-name) old)
          (ngit-hook-run repo "post-checkout" (list old onto "1"))
          (ngit-rebase-run repo commits))))))

(provide 'ngit-sequencer)
;;; ngit-sequencer.el ends here
