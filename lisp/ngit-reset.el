;;; ngit-reset.el --- soft, mixed and hard reset -*- lexical-binding: t; -*-

;;; Commentary:

;; Follow builtin/reset.c's read_from_tree: replace only differing
;; entries, invalidate their cache-tree paths, then refresh the entire
;; index.  Path resets use the same operation without updating refs.

;;; Code:

(require 'ngit-command-paths)
(require 'ngit-branch)
(require 'ngit-commit)
(require 'ngit-checkout)

(defun ngit--reset-covered-p (items path)
  (eq (ngit-command-path-class items path nil) 'full))

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
             (valid (or (/= (aref e 3) 0)
                        (memq 'skip-worktree (aref e 5))
                        (and pos (/= 0 (logand (aref raw (+ pos 60)) #x80)))))
             (skipped valid) (status nil))
        (unless valid
          (let* ((leading (ngit--reset-leading-symlink-p (aref e 0) prefixes))
                 (st (unless leading (ngit-entry-lstat index e))))
            (if (null st)
                (setq status
                      (if (or leading
                              (= (ngit-wt-lstat-result (aref e 0))
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
         (items (ngit-command-path-items repo paths))
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
         (_ (progn (ngit-win-validate-worktree-records old)
                   (ngit-win-validate-worktree-records changes)
                   (when (ngit-windows-p)
                     (ngit-win-validate-worktree-records (ngit-diff-trees repo nil tree)))))
         (lock (unless (ngit-windows-p) (ngit-lock (ngit-index-file repo)))))
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
          (ngit-win-validate-worktree-records entries)
          (when (ngit-windows-p) (setq lock (ngit-lock (ngit-index-file repo))))
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
                              (= (ngit-wt-lstat-result (aref e 0))
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
          (aset index 8 nil)
          (aset index 9 nil)
          (ngit-commit-lock lock (ngit-index-bytes repo index entries root checked))
          (aset repo 6 nil))
      (when lock (ngit-unlock lock)))))

(defun ngit--reset-uptodate-p (repo index entry)
  "Check unpack-trees' stat contract without refreshing the index first."
  (or (not (ngit-wt-lstat (aref entry 0)))
      (let* ((pos (aref entry 4)) (mode (aref entry 2))
             (filemode (ngit-config-bool repo "core.filemode" t))
             (st (and pos (ngit-entry-lstat index entry))))
        ;; The reference Git build does not enable USE_NSEC.  A refresh can
        ;; retain old ctime nanoseconds when every checked stat field agrees.
        (and st (ngit--checkout-leading-clear-p (aref entry 0))
             (let ((wmode (ngit-wt-mode st)))
               (when (or (and (not filemode) (= (logand mode #o170000) #o100000)
                              (= (logand wmode #o170000) #o100000))
                         (and (= mode ngit-mode-symlink) (= wmode #o100644)
                              (not (ngit-config-bool repo "core.symlinks" t)))
                         (and (= mode ngit-mode-gitlink) (= wmode ngit-mode-tree)))
                 (setq wmode mode))
               (and (= wmode mode)
                    (ngit--stat-match-p (aref index 1) pos st)
                    (not (ngit--racily-modified-p repo index entry filemode))))))))

(defun ngit--reset-original-cwd ()
  "Read the inherited process cwd, unaffected by the CLI's Lisp-only -C."
  (if (ngit-windows-p)
      (or (getenv "NGIT_ORIGINAL_CWD") (ngit-win-current-directory))
   (let* ((buffer (ngit-native--buffer 3 4096))
         (address (nl-ffi-memory-address buffer))
         ;; Linux getcwd includes the terminating NUL in its return value.
         (size (syscall-direct 79 address 4096 0 0 0 0)))
    (unless (> size 0)
      (ngit--die "fatal: Unable to read current working directory"))
    (directory-file-name
     (file-truename (decode-coding-string (ptr-read-bytes address (1- size)) 'utf-8))))))

(defun ngit--reset-protect-untracked (repo changes old rev)
  "Preflight every destination and its leading components before reset writes."
  (let ((untracked nil) (collisions nil) (dirs nil)
        (tracked (ngit--tracked-set old)))
    ;; An unmerged index pathname can now be a directory containing entirely
    ;; untracked files.  Do not let its stages hide that directory from the walk.
    (dotimes (i (length old))
      (let* ((entry (aref old i)) (path (aref entry 0))
             (st (and (/= (aref entry 3) 0) (ngit-wt-lstat path))))
        (when (and st (= (ngit-wt-mode st) ngit-mode-tree))
          (remhash path tracked))))
    (ngit--walk-untracked
     repo (file-name-as-directory (ngit-repo-toplevel repo))
     tracked "" (ngit-ignore-global-patterns repo)
     (lambda (path) (push path untracked)) nil t)
    (dolist (c changes)
      (let ((path (aref c 0)))
        (dolist (name untracked)
          (when (string-prefix-p (concat path "/") name) (push path dirs)))
        (dolist (name (ngit--switch-untracked-conflicts untracked path))
          (push (cons name (if (= (aref c 2) 0) "removed" "overwritten")) collisions))))
    (cond
     (dirs
      (ngit--die "error: Updating '%s' would lose untracked files in it\nfatal: Could not reset index file to revision '%s'."
                 (car (sort dirs #'string<)) rev))
     ((seq-some
       (lambda (c)
         (let* ((path (aref c 0)) (st (ngit-wt-lstat path))
                (file (expand-file-name (decode-coding-string path 'utf-8)
                                        (ngit-repo-toplevel repo))))
           (when (and st (= (ngit-wt-mode st) ngit-mode-tree)
                      (equal (ngit--reset-original-cwd)
                             (directory-file-name (file-truename file))))
             (ngit--die "error: Refusing to remove '%s' since it is the current working directory.\nfatal: Could not reset index file to revision '%s'."
                        path rev)))) changes))
     (collisions
      (let ((first (car (sort collisions (lambda (a b) (string< (car a) (car b)))))))
        (ngit--die "error: Untracked working tree file '%s' would be %s by merge.\nfatal: Could not reset index file to revision '%s'."
                   (car first) (cdr first) rev))))))

(defun ngit--reset-preserving (repo tree head-tree rev keep)
  "Reset to TREE with unpack-trees merge/keep safety checks before writes."
  (let* ((index (ngit-read-index repo)) (old (aref index 0))
         (changes (ngit-diff-tree-index repo tree))
         (head-changes (and keep (ngit-diff-trees repo head-tree tree)))
         (head-diff (make-hash-table :test 'equal))
         (destinations (make-hash-table :test 'equal))
         (parents (make-hash-table :test 'equal))
         (write-changes nil)
         (drop (make-hash-table :test 'equal))
         (targets nil) (writes nil) (deletes nil)
         (root (ngit-ct-root index)) (i 0))
    ;; The ordinary diff omits ITA additions.  Two-way unpack-trees must
    ;; still verify/remove them, even when both trees lack their paths.
    ;; Fold this into the existing stage cleanup scan to avoid another pass
    ;; over ordinary indexes.
    (let ((added nil) (known nil))
      (while (< i (length old))
        (let ((e (aref old i)))
          (when (/= (aref e 3) 0) (puthash (aref e 0) t drop))
          (when (memq 'ita (aref e 5))
            (unless known
              (setq known (make-hash-table :test 'equal))
              (dolist (c changes) (puthash (aref c 0) t known)))
            (unless (gethash (aref e 0) known)
              (puthash (aref e 0) t known)
              (push (ngit-change (aref e 0) ?A 0 nil (aref e 2) (aref e 1) 'ita) changes)
              (setq added t))))
        (setq i (1+ i)))
      (when added
        (setq changes (sort changes (lambda (a b) (string< (aref a 0) (aref b 0)))))))
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (when keep
      (when (or (file-exists-p (expand-file-name "MERGE_HEAD" (ngit-repo-gitdir repo)))
                (seq-some (lambda (e) (/= (aref e 3) 0)) old))
        (ngit--die "fatal: Cannot do a keep reset in the middle of a merge."))
      (dolist (c head-changes) (puthash (aref c 0) c head-diff)))
    (setq write-changes (if keep
                            (seq-filter (lambda (c) (gethash (aref c 0) head-diff)) changes)
                          changes))
    (ngit--reset-protect-untracked
     repo (append write-changes
                  (and keep
                       ;; --keep also checks HEAD paths absent from the index,
                       ;; including deletions with no destination to write.
                       (mapcar (lambda (c)
                                 (vector (aref c 0) (aref c 1)
                                         (aref c 4) (aref c 5) (aref c 2) (aref c 3)))
                               head-changes))) old rev)
    (dolist (c write-changes)
      (when (/= (aref c 2) 0)
        (let ((path (aref c 0)) (start 0) (slash nil))
          (puthash path t destinations)
          (while (setq slash (ngit--byte-pos path ?/ start))
            (puthash (substring path 0 slash) t parents)
            (setq start (1+ slash))))))
    ;; Check even entries that already equal the target: --keep first merges
    ;; HEAD -> target, then resets the remaining staged changes without writing.
    (let ((check (lambda (path current mode bin changed)
                   (when changed
                     (let ((error-kind nil))
                       (cond
                        ((and keep (null current) (/= mode 0)
                              (let ((h (ngit--literal-entry repo head-tree path)))
                                (and h (/= (car h) ngit-mode-tree))))
                         (setq error-kind "would be overwritten by merge. Cannot merge"))
                        ((and keep current
                              (not (ngit--switch-same-entry-p current mode bin))
                              (let ((h (ngit--literal-entry repo head-tree path)))
                                (not (and h (ngit--switch-same-entry-p current (car h) (cdr h))))))
                         (setq error-kind "would be overwritten by merge. Cannot merge"))
                        ((and current (= (aref current 3) 0)
                              (not (ngit--switch-same-entry-p current mode bin))
                              (not (ngit--reset-uptodate-p repo index current)))
                         (setq error-kind "not uptodate. Cannot merge")))
                       (when error-kind
                         (ngit--die "error: Entry '%s' %s.\nfatal: Could not reset index file to revision '%s'."
                                    path error-kind rev)))))))
      (when keep
        (dolist (c head-changes)
          (funcall check (aref c 0) (ngit-index-lookup old (aref c 0)) (aref c 4) (aref c 5) t)))
      (dolist (c changes)
        (let* ((path (aref c 0)) (current (ngit-index-lookup old path))
               (unmerged (and current (/= (aref current 3) 0)))
               (write (or unmerged (not keep) (gethash path head-diff)))
               (target (and (/= (aref c 2) 0)
                            (ngit--switch-entry path (aref c 2) (aref c 3) nil))))
          (unless keep (funcall check path current (aref c 2) (aref c 3) t))
          (when keep
            (let ((topology (gethash path parents)) (start 0) (slash nil))
              (while (and (not topology) (setq slash (ngit--byte-pos path ?/ start)))
                (setq topology (gethash (substring path 0 slash) destinations)
                      start (1+ slash)))
              (when (and topology current (not (ngit--reset-uptodate-p repo index current)))
                (ngit--die "error: Entry '%s' not uptodate. Cannot merge.\nfatal: Could not reset index file to revision '%s'."
                           path rev))))
          (puthash path t drop)
          (when root (ngit-ct-invalidate root path))
          (when target (push target targets))
          (when write
            (if target (push target writes) (when current (push current deletes)))))))
    (let* ((entries (ngit-index-overlay old targets drop))
           (_ (progn (ngit-win-validate-worktree-records old)
                     (ngit-win-validate-worktree-records deletes)
                     (ngit-win-validate-worktree-records writes)
                     (ngit-win-validate-worktree-records entries)))
           (lock (ngit-lock (ngit-index-file repo)))
           (checked (make-hash-table :test 'eql))
           (ngit--checkout-attr-cache (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            (aset index 0 entries)
            (dolist (e deletes) (ngit--unlink-entry repo e))
            (ngit--switch-clear-target-directories repo (vconcat writes))
            (dolist (e writes)
              (let ((fresh (ngit-checkout-entry repo e)))
                (aset entries (ngit-index-find entries (aref e 0)) fresh)))
            (ngit--reset-refresh repo index entries checked t)
            (aset index 8 nil)
            (aset index 9 nil)
            (ngit-commit-lock lock (ngit-index-bytes repo index entries root checked))
            (setq lock nil) (aset repo 6 nil) (aset repo 7 nil))
        (when lock (ngit-unlock lock))))))

(defun ngit-cmd-reset (repo args)
  (let ((mode nil) (quiet nil) (rest nil) (rev "HEAD") (paths nil) (explicit nil))
    (while args
      (let ((a (pop args)))
        (cond
         ((string= a "--") (setq rest (append (nreverse rest) (cons a args)) args nil explicit t))
         ((member a '("-q" "--quiet")) (setq quiet t))
         ((member a '("--soft" "--mixed" "--hard" "--merge" "--keep")) (setq mode a))
         ((member a '("-p" "--patch"))
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
         ((or file (string-prefix-p ":(" (car rest))
              (ngit--glob-special-p (car rest) 0)) (setq paths rest))
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
              ((member mode '("--merge" "--keep"))
               (ngit--die "fatal: Cannot do %s reset with paths." (substring mode 2)))
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
          (cond ((equal mode "--hard") (ngit--reset-hard repo tree rev))
                ((member mode '("--merge" "--keep"))
                 (require 'ngit-switch)
                 (ngit--reset-preserving repo tree (and head (ngit-unhex (ngit-peel repo head 'tree))) rev (equal mode "--keep")))
                (t (ngit--reset-index repo tree paths quiet)))))
      (unless (or paths unborn)
        (let* ((action (getenv "GIT_REFLOG_ACTION"))
               (orig (ngit-resolve-ref repo "ORIG_HEAD"))
               (msg (if action (concat action ": updating HEAD")
                      (concat "reset: moving to " rev))))
          (if head
              (ngit--write-ref repo "ORIG_HEAD" head orig
                               (unless (equal head orig)
                                 (concat (or action "reset") ": updating ORIG_HEAD")))
            (when orig (ngit-ref-delete-state repo "ORIG_HEAD")))
          (ngit-update-head repo hex head (ngit-ident repo "COMMITTER") msg)))
      (unless paths
        (dolist (name '("CHERRY_PICK_HEAD" "REVERT_HEAD"))
          (when (ngit-read-ref repo name) (ngit-ref-delete-state repo name)))
        (ngit-ref-delete-state repo "AUTO_MERGE")
        (dolist (name '("MERGE_HEAD" "MERGE_RR" "MERGE_MSG" "MERGE_MODE"
                        "SQUASH_MSG"))
          (let ((file (expand-file-name name (ngit-repo-gitdir repo))))
            (when (file-exists-p file) (ngit-delete-file file))))
        (ngit-ref-delete-state repo "AUTO_MERGE"))
      (when (and (equal mode "--hard") hex (not quiet))
        (let ((subject (ngit--subject-of repo hex)))
          (ngit--emit "HEAD is now at " (ngit-abbrev repo hex)
                      (if (string= subject "") "" (concat " " (decode-coding-string subject 'utf-8)))
                      "\n")))
      0)))

(provide 'ngit-reset)

;;; ngit-reset.el ends here
