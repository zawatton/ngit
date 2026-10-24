;;; ngit-restore.el --- restore index and work-tree paths -*- lexical-binding: t; -*-

;;; Commentary:

;; Restore is checkout_paths with restore's non-overlay defaults.  A source
;; tree patches only the selected index runs, while the index-to-work-tree
;; form writes refreshed stat data back to the index.

;;; Code:

(require 'ngit-checkout)

(defun ngit--restore-parse (repo args)
  "Return (TREE SOURCE STAGED WORKTREE QUIET PATHS) for restore ARGS."
  (let ((source nil) (staged nil) (worktree nil) (selected nil)
        (quiet nil) (paths nil))
    (while args
      (let ((a (pop args)))
        (cond
         ((string= a "--") (setq paths (append (reverse args) paths) args nil))
         ((member a '("-q" "--quiet")) (setq quiet t))
         ((member a '("-S" "--staged"))
          (unless selected (setq staged nil worktree nil selected t))
          (setq staged t))
         ((member a '("-W" "--worktree"))
          (unless selected (setq staged nil worktree nil selected t))
          (setq worktree t))
         ((member a '("-SW" "-WS"))
          (setq selected t staged t worktree t))
         ((member a '("-s" "--source"))
          (unless args
            (ngit--die-with 129 "error: switch `%s' requires a value" a))
          (setq source (pop args)))
         ((string-prefix-p "--source=" a) (setq source (substring a 9)))
         ((string-prefix-p "-" a)
          (ngit--die-with 129 "error: unknown option `%s'" a))
         (t (push a paths)))))
    (setq paths (nreverse paths))
    (unless selected (setq worktree t))
    (when (and staged (null source)) (setq source "HEAD"))
    (when (member "" paths)
      (ngit--die
       "fatal: empty string is not a valid pathspec. please use . instead if you meant to match all paths"))
    (unless paths (ngit--die "fatal: you must specify path(s) to restore"))
    (let ((tree nil))
      (when source
        (let ((hex (condition-case nil (ngit-rev-parse repo source)
                     (error (ngit--die "fatal: could not resolve %s" source)))))
          (setq tree
                (condition-case nil
                    (ngit-unhex (ngit-peel repo hex 'tree))
                  (error (ngit--die "fatal: reference is not a tree: %s" source))))))
      (list tree source staged worktree quiet paths))))

(defun ngit--restore-index-targets (entries items matched)
  "Return selected stage-zero ENTRIES, using direct ranges for literals."
  (let ((out nil) (seen (make-hash-table :test 'equal)))
    (let ((emit
           (lambda (e)
             (let ((path (aref e 0)))
               (when (and (= (aref e 3) 0)
                          (not (memq 'skip-worktree (aref e 5))))
                 (ngit--checkout-item-matches items path matched)
                 (unless (gethash path seen)
                   (puthash path t seen)
                   (push e out)))))))
      (if (seq-some #'cdr items)
          (let ((i 0))
            (while (< i (length entries))
              (let ((e (aref entries i)))
                (when (ngit--checkout-match-p items (aref e 0))
                  (funcall emit e)))
              (setq i (1+ i))))
        (dolist (item items)
          (let* ((path (car item))
                 (lo (if (string= path "") 0 (ngit-index-find entries path)))
                 (hi (if (string= path "") (length entries)
                       (ngit-index-find entries (concat path "0"))))
                 (i lo))
            ;; The half-open [PATH, PATH0) range contains PATH and PATH/...
            ;; without examining unrelated index entries.
            (while (< i hi)
              (let ((e (aref entries i)))
                (when (ngit--item-covers-p path (aref e 0))
                  (funcall emit e)))
              (setq i (1+ i)))))))
    (sort out #'ngit--entry-less)))

(defun ngit--restore-preserve-same (old targets)
  "Use OLD entry vectors for TARGETS whose object and mode are unchanged."
  (let ((out nil))
    (dolist (target targets)
      (let ((e (ngit-index-lookup old (aref target 0))))
        (push (if (and e (= (aref e 3) 0) (not (memq 'ita (aref e 5)))
                       (= (aref e 2) (aref target 2))
                       (equal (aref e 1) (aref target 1)))
                  e target)
              out)))
    (nreverse out)))

(defun ngit--restore-report-misses (paths matched)
  "Report all unmatched PATHS like report_path_error."
  (let ((i 0) (miss nil))
    (dolist (path paths)
      (unless (aref matched i)
        (ngit-native-write-stderr
         (format "error: pathspec '%s' did not match any file(s) known to git\n" path))
        (setq miss t))
      (setq i (1+ i)))
    miss))

(defun ngit--restore-drop-set (selected targets)
  "Return selected index paths absent from TARGETS."
  (let ((keep (make-hash-table :test 'equal))
        (drop (make-hash-table :test 'equal)))
    (dolist (e targets) (puthash (aref e 0) t keep))
    (dolist (e selected)
      (unless (gethash (aref e 0) keep) (puthash (aref e 0) t drop)))
    drop))

(defun ngit--restore-add-removed (entry removed seen)
  (let ((path (aref entry 0)))
    (unless (gethash path seen)
      (puthash path t seen)
      (setcar removed (cons entry (car removed))))))

(defun ngit--restore-force-target-shape (repo target)
  "Remove a directory blocking non-directory TARGET, without following links."
  (let ((path (aref target 0))
        (fmt (logand (aref target 2) #o170000)))
    ;; lstat only protects the final component.  A leading symlink must be
    ;; replaced by checkout's leading-dir logic, never traversed recursively.
    (when (ngit--checkout-leading-clear-p path)
      (let ((st (ngit-wt-lstat path)))
        (when (and st (/= fmt ngit-mode-gitlink)
                   (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir))
          (delete-directory
           (expand-file-name (decode-coding-string path 'utf-8)
                             (ngit-repo-toplevel repo))
           t)
          (when ngit--checkout-leading-cache
            (clrhash ngit--checkout-leading-cache)))))))

(defun ngit--restore-worktree (repo index old entries targets removed write-index root)
  "Install TARGETS and REMOVED in the work tree, optionally writing ENTRIES."
  (let ((checked (make-hash-table :test 'eql))
        (updates nil)
        (ngit--checkout-attr-cache (make-hash-table :test 'equal)))
    (dolist (e (sort removed #'ngit--entry-less)) (ngit--unlink-entry repo e))
    (when write-index
      (aset index 0 entries)
      (aset repo 7 nil))
    (dolist (target targets)
      (let* ((name (aref target 0))
             (old-pos (ngit-index-find old name))
             (old-e (and (< old-pos (length old)) (aref old old-pos)))
             (same (and old-e (= (aref old-e 3) 0)
                        (not (memq 'ita (aref old-e 5)))
                        (string= (aref old-e 0) name)
                        (= (aref old-e 2) (aref target 2))
                        (equal (aref old-e 1) (aref target 1))))
             (stat-clean (and same (ngit--checkout-stat-clean-p repo index old-e)))
             (clean (or stat-clean (ngit--checkout-clean-p repo index target)))
             (fresh (if clean
                        (ngit-make-entry name (aref target 1) (aref target 2)
                                         (ngit-wt-stat-block (aref target 2))
                                         (aref target 5))
                      (progn
                        (ngit--restore-force-target-shape repo target)
                        (ngit-checkout-entry repo target)))))
        (when write-index
          (let ((pos (ngit-index-find entries name)))
            (if (and same (aref old-e 4))
                (progn
                  (push (cons (aref old-e 4) (aref fresh 6)) updates)
                  (puthash (aref old-e 4) t checked)
                  (aset entries pos old-e))
              (aset entries pos fresh))))))
    (when write-index (ngit--checkout-update-index-stat index updates))
    checked))

(defun ngit-cmd-restore (repo args)
  (let* ((parsed (ngit--restore-parse repo args))
         (tree (nth 0 parsed)) (staged (nth 2 parsed))
         (worktree (nth 3 parsed)) (paths (nth 5 parsed))
         (magic (ngit-pathspec-magic-p (ngit-pathspec-items repo paths)))
         (expanded (ngit-pathspec-write-items repo paths t tree))
         (items (mapcar (lambda (path) (if magic (cons path nil) (ngit--ps-item path))) expanded))
         (paths (if magic expanded paths))
         (matched (make-vector (length items) nil))
         (lock (ngit-lock (ngit-index-file repo))))
    (unwind-protect
        (let* ((index (ngit-read-index repo)) (old (aref index 0))
               (selected (ngit--restore-index-targets old items matched))
               (targets (if tree
                            (ngit--checkout-source-entries repo tree items matched)
                          selected)))
          (if (ngit--restore-report-misses paths matched)
              1
            (setq targets (ngit--restore-preserve-same old targets))
            ;; Tree entries have no index stat block.  Stage-only restores
            ;; still need their mode in the otherwise-zero stat template.
            (dolist (e targets)
              (unless (aref e 4)
                (aset e 6 (concat (make-string 24 0)
                                  (ngit--be32-bytes (aref e 2))
                                  (make-string 12 0)))))
            (let* ((drop (and tree (ngit--restore-drop-set selected targets)))
                   ;; Default index-to-work-tree restore also commits fresh
                   ;; stat data, and must retain the existing cache-tree.
                   (root (and (or staged (and worktree (null tree)))
                              (ngit-ct-root index)))
                   (removed-box (list nil))
                   (removed-seen (make-hash-table :test 'equal))
                   (entries
                    (if staged
                        (ngit-index-overlay
                         old targets drop
                         (lambda (e)
                           (ngit--restore-add-removed e removed-box removed-seen)))
                      old)))
              (when drop
                (dolist (e selected)
                  (when (gethash (aref e 0) drop)
                    (when root (ngit-ct-invalidate root (aref e 0)))
                    (when worktree
                      (ngit--restore-add-removed e removed-box removed-seen)))))
              (when staged
                (dolist (e targets)
                  (let ((old-e (ngit-index-lookup old (aref e 0))))
                    (unless (eq e old-e)
                      (when root (ngit-ct-invalidate root (aref e 0)))))))
              ;; Work-tree-only source restores still need topology removals,
              ;; but their temporary overlay must never reach the index.
              (when (and worktree tree (not staged))
                (ngit-index-overlay
                 old targets nil
                 (lambda (e)
                   (ngit--restore-add-removed e removed-box removed-seen))))
              (let ((checked
                     (and worktree
                          (ngit--restore-worktree
                           repo index old entries targets (car removed-box)
                           (or staged (null tree)) root))))
                (when (or staged (and worktree (null tree)))
                  (ngit-commit-lock lock
                                    (ngit-index-bytes repo index entries root checked))
                  (setq lock nil)
                  (aset repo 6 nil))
                0))))
      (when lock (ngit-unlock lock)))))

(provide 'ngit-restore)

;;; ngit-restore.el ends here
