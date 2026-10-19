;;; ngit-reset.el --- soft, mixed and hard reset -*- lexical-binding: t; -*-

;;; Commentary:

;; Follow builtin/reset.c's read_from_tree: replace only differing
;; entries, invalidate their cache-tree paths, then refresh the entire
;; index.  Path resets use the same operation without updating refs.

;;; Code:

(require 'ngit-branch)
(require 'ngit-commit)
(require 'ngit-checkout)

(defun ngit--reset-covered-p (items path)
  (let ((ngit--rv-paths items))
    (eq (ngit--ps-class path nil) 'full)))

(defun ngit--reset-invalidate (root replaced path)
  (puthash path t replaced)
  (when root (ngit-ct-invalidate root path)))

(defun ngit--reset-leading-symlink-p (path cache)
  "Like has_symlink_leading_path, caching shared directory prefixes."
  (let ((start 0) (slash nil) (found nil))
    (while (and (not found) (setq slash (ngit--byte-pos path ?/ start)))
      (let* ((prefix (substring path 0 slash))
             (kind (gethash prefix cache)))
        (unless kind
          (let ((st (ngit-wt-lstat prefix)))
            (setq kind (if (and st (= (logand (aref st 0) ngit-s-ifmt) ngit-s-iflnk))
                           'symlink 'other))
            (puthash prefix kind cache)))
        (setq found (eq kind 'symlink) start (1+ slash))))
    found))

(defun ngit--reset-refresh (repo index entries checked quiet &optional candidates)
  "Refresh ENTRIES, including new entries with zero stat data.
Keep dirty entries' stat data, as read-cache.c refresh_cache_ent does.
If CANDIDATES is a cons, collect unverified paths in its car for a later diff."
  (let ((raw (aref index 1)) (ts (aref index 4))
        (filemode (ngit-config-bool repo "core.filemode" t))
        (prefixes (make-hash-table :test 'equal))
        (first t) (i 0))
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (while (< i (length entries))
      (let* ((e (aref entries i)) (pos (aref e 4)) (mode (aref e 2))
             (valid (or (memq 'skip-worktree (aref e 5))
                        (and pos (/= 0 (logand (aref raw (+ pos 60)) #x80)))))
             (skipped valid) (status nil))
        (unless valid
          (let* ((leading (ngit--reset-leading-symlink-p (aref e 0) prefixes))
                 (st (unless leading (ngit-entry-lstat index e))))
            (if (null st)
                (setq status
                      (if (or leading
                              (= (syscall-direct ngit-wt--sys-lstat
                                                 (ngit-wt--set-path (aref e 0))
                                                 (nl-ffi-memory-address ngit-wt--stat) 0 0 0 0)
                                 -2))
                          "D" "M"))
              (let ((wmode (ngit-wt-mode st)))
                (when (and (not filemode) (= (logand mode #o170000) #o100000)
                           (= (logand wmode #o170000) #o100000))
                  (setq wmode mode))
                (cond
                 ((memq 'ita (aref e 5)) (setq status "A"))
                 ((= mode ngit-mode-gitlink)
                  (let ((head (and (= wmode ngit-mode-tree)
                                   (ngit--gitlink-head repo (aref e 0)))))
                    (unless (and (= wmode ngit-mode-tree)
                                 (or (null head) (equal head (aref e 1))))
                      (setq status "M"))))
                 ((/= wmode mode)
                  (setq status (if (eq (ngit--type-class mode) (ngit--type-class wmode))
                                   "M" "T")))
                 ((and pos (ngit--stat-match-p raw pos st)
                       (> ts (ngit--be32 raw (+ pos 8))))
                  (setq valid t))
                 ((and pos (/= (ngit--be32 raw (+ pos 36)) 0)
                       (/= (ngit--be32 raw (+ pos 36)) (logand (aref st 2) #xffffffff)))
                  (setq status "M"))
                 ((equal (ngit-hash-blob
                          (prog1 (ngit-wt-content repo (aref e 0) st)
                            (ngit-entry-lstat index e)))
                         (aref e 1))
                  (if (and pos (ngit--stat-match-p raw pos st))
                      (setq valid t)
                    (aset entries i (ngit-make-entry (aref e 0) (aref e 1) mode
                                                    (ngit-wt-stat-block mode) (aref e 5)))))
                 (t (setq status "M")))))))
        (when (and valid pos) (puthash pos t checked))
        ;; A switch can reuse this scan when displaying local changes.  Flags
        ;; that bypassed refresh and submodules still need diff's full check.
        (when (and candidates (or skipped status (= mode ngit-mode-gitlink)))
          (push (aref e 0) (car candidates)))
        (when (and status (not quiet))
          (when first
            (ngit--emit "Unstaged changes after reset:\n")
            (setq first nil))
          ;; refresh_index prints raw names, without diff's path quoting.
          (ngit--emit status "\t" (aref e 0) "\n")))
      (setq i (1+ i)))))

(defun ngit--reset-index (repo tree paths quiet)
  "Reset the index to TREE, limited to PATHS when non-nil."
  (let* ((index (ngit-read-index repo)) (old (aref index 0))
         (root (ngit-ct-root index))
         (items (mapcar (lambda (p) (ngit--ps-item (ngit--pathspec-item repo p))) paths))
         (changes (ngit-diff-tree-index repo tree))
         (replaced (make-hash-table :test 'equal))
         (checked (make-hash-table :test 'eql))
         (entries nil) (i 0))
    (dolist (c changes)
      (let ((path (aref c 0)))
        (when (or (not paths) (ngit--reset-covered-p items path))
          (ngit--reset-invalidate root replaced path)
          (unless (= (aref c 2) 0)
            (let* ((pos (ngit-index-find old path))
                   (e (and (< pos (length old)) (aref old pos)))
                   (flags (and e (string= (aref e 0) path)
                               (memq 'skip-worktree (aref e 5)) '(skip-worktree))))
              ;; add_index_entry removes conflicting ancestors and descendants,
              ;; including entries outside the selected pathspec.
              (let ((start 0) (slash nil)
                    (j (ngit-index-find old (concat path "/")))
                    (end (ngit-index-find old (concat path "0"))))
                (while (setq slash (ngit--byte-pos path ?/ start))
                  (let* ((prefix (substring path 0 slash))
                         (p (ngit-index-find old prefix)))
                    (when (and (< p (length old)) (string= (aref (aref old p) 0) prefix))
                      (ngit--reset-invalidate root replaced prefix)))
                  (setq start (1+ slash)))
                (while (< j end)
                  (ngit--reset-invalidate root replaced (aref (aref old j) 0))
                  (setq j (1+ j))))
              (push (ngit-make-entry path (aref c 3) (aref c 2)
                                     (concat (make-string 24 0) (ngit--be32-bytes (aref c 2))
                                             (make-string 12 0)) flags)
                    entries))))))
    (while (< i (length old))
      (let* ((e (aref old i)) (path (aref e 0)))
        (when (and (not (gethash path replaced)) (memq 'ita (aref e 5))
                   (or (not paths) (ngit--reset-covered-p items path)))
          (puthash path t replaced)
          (when root (ngit-ct-invalidate root path))))
      (setq i (1+ i)))
    (setq entries (ngit-index-overlay old entries replaced))
    (ngit--reset-refresh repo index entries checked quiet)
    (ngit-write-index repo index entries root checked)))

(defun ngit--reset-unknown (rev)
  (ngit--die-with 128
                  (concat "fatal: ambiguous argument '%s': unknown revision or path not in the working tree.\n"
                          "Use '--' to separate paths from revisions, like this:\n"
                          "'git <command> [<revision>...] -- [<file>...]'") rev))

(defun ngit--reset-hard-canonical-clean-p (repo entry)
  "Non-nil when regular-file ENTRY matches canonical work-tree content.
This is intentionally used only for an index entry unchanged by hard reset:
Git keeps such a file even when today's checkout filter would smudge it to a
different byte representation."
  (let* ((path (aref entry 0)) (mode (aref entry 2))
         (filemode (ngit-config-bool repo "core.filemode" t)))
    (and (= (logand mode #o170000) #o100000)
         (ngit--checkout-leading-clear-p path)
         (let ((st (ngit-wt-lstat path)))
           (and st
                (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifreg)
                (let ((wmode (ngit-wt-mode st)))
                  (when (and (not filemode)
                             (= (logand wmode #o170000) #o100000))
                    (setq wmode mode))
                  (and (= wmode mode)
                       (equal (ngit-hash-blob
                               (prog1 (ngit-wt-content repo path st)
                                 (ngit-wt-lstat path)))
                              (aref entry 1)))))))))

(defun ngit--reset-hard (repo tree rev &optional before-write)
  "Merge TREE onto the index and work tree, as oneway_merge does.
Call BEFORE-WRITE with the final entries vector under the index lock, after all
protected entry checks and before the first work-tree deletion or write."
  (let* ((index (ngit-read-index repo)) (old (aref index 0))
         (root (ngit-ct-root index))
         (changes (ngit-diff-tree-index repo tree))
         (dropped (make-hash-table :test 'equal))
         (targets-by-path (make-hash-table :test 'equal))
         (removed-by-path (make-hash-table :test 'equal))
         (targets nil) (removed nil)
         (entries nil) (stats nil) (i 0)
         (stat-options (vector (ngit-config-bool repo "core.filemode" t)
                               (ngit-config-bool repo "core.symlinks" t)))
         (ngit--checkout-attr-direction 'index-first)
         (ngit--checkout-attr-cache (make-hash-table :test 'equal))
         (ngit--checkout-leading-cache (make-hash-table :test 'equal))
         (checked (make-hash-table :test 'eql))
         (lock (ngit-lock (ngit-index-file repo))))
    (unwind-protect
        (progn
          (ngit-wt-setup (ngit-repo-toplevel repo))
          ;; ngit-diff-tree-index describes TARGET -> INDEX.  Its old side is
          ;; therefore the reset target.  Overlay only those paths so equal
          ;; cache-tree subtrees never need to be flattened.
          (dolist (c changes)
            (let* ((path (aref c 0))
                   (mode (aref c 2)) (bin (aref c 3))
                   (prev (ngit-index-lookup old path)))
              (puthash path t dropped)
              (when root (ngit-ct-invalidate root path))
              (when (/= mode 0)
                (let ((e (ngit-make-entry
                          path bin mode
                          (concat (make-string 24 0) (ngit--be32-bytes mode)
                                  (make-string 12 0))
                          (and prev (memq 'skip-worktree (aref prev 5))
                               '(skip-worktree)))))
                  (puthash path e targets-by-path)
                  (push e targets)))))
          ;; Tree/index diff deliberately ignores unmerged and intent-to-add
          ;; entries.  Hard reset removes them even when the target is empty.
          (while (< i (length old))
            (let ((e (aref old i)))
              (when (or (/= (aref e 3) 0) (memq 'ita (aref e 5)))
                (puthash (aref e 0) t dropped)
                (when root (ngit-ct-invalidate root (aref e 0)))))
            (setq i (1+ i)))
          (maphash
           (lambda (path _)
             (unless (or (gethash path targets-by-path)
                         (gethash path removed-by-path))
               (let ((e (ngit-index-lookup old path)))
                 (when e
                   (puthash path t removed-by-path)
                   (push e removed)))))
           dropped)
          (setq removed (sort removed #'ngit--entry-less)
                entries (ngit-index-overlay old targets dropped))
          ;; Protected entries still undergo verify_uptodate when the
          ;; target changes them, even with UNPACK_RESET_OVERWRITE_UNTRACKED.
          (let ((raw (aref index 1)) (rejected nil))
            (setq i 0)
            (while (< i (length old))
              (let* ((e (aref old i)) (pos (aref e 4)))
                (when (and pos (gethash (aref e 0) dropped)
                           (/= (aref e 2) ngit-mode-gitlink)
                           (or (memq 'skip-worktree (aref e 5))
                               (/= 0 (logand (aref raw (+ pos 60)) #x80))))
                  (unless (or (ngit--checkout-stat-clean-p repo index e stat-options)
                              (= (syscall-direct ngit-wt--sys-lstat
                                                 (ngit-wt--set-path (aref e 0))
                                                 (nl-ffi-memory-address ngit-wt--stat) 0 0 0 0)
                                 -2))
                      (ngit-native-write-stderr
                       (format "error: Entry '%s' not uptodate. Cannot merge.\n" (aref e 0)))
                      (setq rejected t))))
              (setq i (1+ i)))
            (when rejected
              (ngit--die "fatal: Could not reset index file to revision '%s'." rev)))
          (setq i 0)
          (when before-write (funcall before-write entries))
          (clrhash ngit--checkout-leading-cache)
          ;; Remove tracked paths first, so file/directory replacements can be
          ;; materialized without removing unrelated untracked siblings.
          (dolist (e removed) (ngit--unlink-entry repo e))
          (aset index 0 entries)
          (aset repo 7 nil)
          (setq i 0)
          (while (< i (length entries))
            (let* ((e (aref entries i)) (pos (aref e 4)))
              (unless (and pos (memq 'skip-worktree (aref e 5)))
                (if (and pos (ngit--checkout-stat-clean-p repo index e stat-options))
                    (progn
                      (puthash pos t checked)
                      (unless (ngit--checkout-stat-identical-p index e)
                        (push (cons pos (ngit-wt-stat-block (aref e 2))) stats)))
                  (let ((fresh (if (or (ngit--checkout-clean-p repo index e)
                                       (and pos
                                            (ngit--reset-hard-canonical-clean-p repo e)))
                                   (ngit-make-entry (aref e 0) (aref e 1) (aref e 2)
                                                    (ngit-wt-stat-block (aref e 2)) (aref e 5))
                                 (ngit-checkout-entry repo e))))
                    (if pos
                        (progn
                          ;; Keep the raw flags (including CE_VALID) of unchanged
                          ;; entries while refreshing their stat blocks below.
                          (push (cons pos (aref fresh 6)) stats)
                          (puthash pos t checked))
                      (aset entries i fresh))))))
            (setq i (1+ i)))
          (when stats
            (let ((raw (aref index 1)) (parts nil) (start 0))
              (dolist (patch (nreverse stats))
                (push (substring raw start (car patch)) parts)
                (push (cdr patch) parts)
                (setq start (+ (car patch) 40)))
              (push (substring raw start) parts)
              (aset index 1 (apply #'concat (nreverse parts)))))
          ;; Preserve valid cache-tree subtrees and rebuild only invalidated
          ;; paths.  A missing or previously invalid tree still gets primed.
          (unless root (setq root (ngit-ct-new)))
          (if tree
              (unless (ngit-ct-fully-valid-p root)
                (ngit-ct-update repo root entries))
            (aset root 0 0)
            (aset root 1 ngit-empty-tree-bin)
            (aset root 2 nil))
          (ngit-commit-lock lock (ngit-index-bytes repo index entries root checked))
          (aset repo 6 nil))
      (ngit-unlock lock))))

(defun ngit-cmd-reset (repo args)
  (let ((mode nil) (quiet nil) (rest nil) (rev "HEAD") (paths nil) (explicit nil))
    (while args
      (let ((a (pop args)))
        (cond
         ((string= a "--") (setq rest (append (nreverse rest) (cons a args)) args nil explicit t))
         ((member a '("-q" "--quiet")) (setq quiet t))
         ((member a '("--soft" "--mixed" "--hard")) (setq mode a))
         ((member a '("--merge" "--keep" "-p" "--patch"))
          (ngit--die "fatal: reset %s is not supported" a))
         ((string-prefix-p "-" a) (ngit--die-with 129 "error: reset option '%s' is not supported" a))
         (t (push a rest)))))
    (unless explicit (setq rest (nreverse rest)))
    (cond
     ((equal (car rest) "--") (setq paths (cdr rest)))
     ((equal (cadr rest) "--") (setq rev (car rest) paths (cddr rest)))
     (rest
      (let ((hex (condition-case nil
                     (ngit-peel repo (ngit-rev-parse repo (car rest))
                                (if (cdr rest) 'tree 'commit))
                   (error nil)))
            (file (let ((name (expand-file-name (car rest))))
                    (or (file-exists-p name) (file-symlink-p name)))))
        (cond
         (hex
          (when file
            (ngit--die (concat "fatal: ambiguous argument '%s': both revision and filename\n"
                               "Use '--' to separate paths from revisions, like this:\n"
                               "'git <command> [<revision>...] -- [<file>...]'") (car rest)))
          (setq rev (car rest) paths (cdr rest)))
         ((or file (ngit--glob-special-p (car rest) 0)) (setq paths rest))
         (t (ngit--reset-unknown (car rest)))))))
    (when (string= rev "@") (setq rev "HEAD"))
    (when (member "" paths)
      (ngit--die "fatal: empty string is not a valid pathspec. please use . instead if you meant to match all paths"))
    (let* ((head (ngit-resolve-ref repo "HEAD"))
           (unborn (and (string= rev "HEAD") (not head)))
           (hex (unless unborn
                  (condition-case nil
                      (ngit-peel repo (ngit-rev-parse repo rev) (if paths 'tree 'commit))
                    (error (ngit--die "fatal: Failed to resolve '%s' as a valid %s."
                                      rev (if paths "tree" "revision")))))))
      (when paths
        (cond ((equal mode "--soft") (ngit--die "fatal: Cannot do soft reset with paths."))
              ((equal mode "--hard") (ngit--die "fatal: Cannot do hard reset with paths."))
              ((equal mode "--mixed")
               (ngit-native-write-stderr
                "warning: --mixed with paths is deprecated; use 'git reset -- <paths>' instead.\n"))))
      (if (equal mode "--soft")
          (when (or (file-exists-p (expand-file-name "MERGE_HEAD" (ngit-repo-gitdir repo)))
                    (seq-some (lambda (e) (/= (aref e 3) 0)) (aref (ngit-read-index repo) 0)))
            (ngit--die "fatal: Cannot do a soft reset in the middle of a merge."))
        (when (ngit-config-bool repo "core.bare" (null (ngit-repo-toplevel repo)))
          (ngit--die (if (equal mode "--hard")
                         "fatal: this operation must be run in a work tree"
                       "fatal: mixed reset is not allowed in a bare repository")))
        (let ((tree (and hex (ngit-unhex (ngit-peel repo hex 'tree)))))
          (if (equal mode "--hard") (ngit--reset-hard repo tree rev)
            (ngit--reset-index repo tree paths quiet))))
      (unless (or paths unborn)
        (let* ((action (getenv "GIT_REFLOG_ACTION"))
               (orig (ngit-resolve-ref repo "ORIG_HEAD"))
               (msg (if action (concat action ": updating HEAD")
                      (concat "reset: moving to " rev))))
          (if head
              (ngit--write-ref repo "ORIG_HEAD" head orig
                               (unless (equal head orig)
                                 (concat (or action "reset") ": updating ORIG_HEAD")))
            (when orig (delete-file (ngit--ref-file repo "ORIG_HEAD"))))
          ;; A no-op ref update still logs HEAD, but not its branch.
          (if (equal hex head)
              (ngit--append-reflog repo "HEAD" head hex (ngit-ident repo "COMMITTER") msg)
            (ngit-update-head repo hex head (ngit-ident repo "COMMITTER") msg))))
      (unless paths
        (dolist (name '("MERGE_HEAD" "MERGE_RR" "MERGE_MSG" "MERGE_MODE"
                        "SQUASH_MSG" "AUTO_MERGE" "CHERRY_PICK_HEAD" "REVERT_HEAD"))
          (let ((file (expand-file-name name (ngit-repo-gitdir repo))))
            (when (file-exists-p file) (delete-file file)))))
      (when (and (equal mode "--hard") hex (not quiet))
        (let ((subject (ngit--subject-of repo hex)))
          (ngit--emit "HEAD is now at " (ngit-abbrev repo hex)
                      (if (string= subject "") "" (concat " " (decode-coding-string subject 'utf-8)))
                      "\n")))
      0)))

(provide 'ngit-reset)

;;; ngit-reset.el ends here
