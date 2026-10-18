;;; ngit-diff.el --- the diff command -*- lexical-binding: t; -*-

;;; Commentary:

;; `diff' between the index and the work tree, a tree and the index
;; (--cached), a tree and the work tree, or two trees, printed as git's
;; default patch output (diff.c builtin_diff / fill_metainfo) or with
;; --name-only / --name-status.  Hunks come from the xdiff port.  Exact
;; renames are detected; inexact (similarity) renames are not.

;;; Code:

(require 'ngit-status)
(require 'ngit-xdiff)

(defun ngit--diff-blob (repo mode bin)
  "Return the content of side (MODE BIN) as git diffs it."
  (cond ((null bin) "")
        ((= mode ngit-mode-gitlink) (concat "Subproject commit " (ngit-hex bin) "\n"))
        (t (cdr (ngit-read-object repo bin)))))

(defun ngit--binary-p (repo path data)
  (let ((diff (cdr (assoc "diff" (ngit-attrs repo path '("diff"))))))
    (cond ((eq diff 'unset) t)
          ((equal diff "binary") t)
          ((eq diff 'set) nil)
          (t (let ((head (if (> (length data) 8000) (substring data 0 8000) data)))
               (and (string-match-p "\0" head) t))))))

(defun ngit--abbrev-bin (repo bin)
  (if bin (ngit-abbrev repo (ngit-hex bin)) (make-string (ngit-default-abbrev repo) ?0)))

(defun ngit--emit-patch-part (repo path-a path-b old-mode old-bin new-mode new-bin
                                  new-data dirty rename ctx)
  "Emit one file pair; a nil BIN means that side is absent."
  (let* ((a-name (ngit-quote-path (concat "a/" path-a)))
         (b-name (ngit-quote-path (concat "b/" path-b)))
         (lbl-a (if old-bin a-name "/dev/null"))
         (lbl-b (if (or new-bin new-data) b-name "/dev/null"))
         (header (list (concat "diff --git " a-name " " b-name "\n"))))
    (cond ((null old-bin)
           (push (concat "new file mode " (ngit--octal6 new-mode) "\n") header))
          ((not (or new-bin new-data))
           (push (concat "deleted file mode " (ngit--octal6 old-mode) "\n") header))
          ((/= old-mode new-mode)
           (push (concat "old mode " (ngit--octal6 old-mode) "\nnew mode "
                         (ngit--octal6 new-mode) "\n")
                 header)))
    (when rename
      (push (concat "similarity index 100%\nrename from " (ngit-quote-path path-a)
                    "\nrename to " (ngit-quote-path path-b) "\n")
            header))
    (unless (equal old-bin new-bin)
      (push (concat "index " (ngit--abbrev-bin repo old-bin) ".."
                    (ngit--abbrev-bin repo new-bin)
                    (if (= old-mode new-mode) (concat " " (ngit--octal6 old-mode)) "")
                    "\n")
            header))
    (let* ((old (ngit--diff-blob repo old-mode old-bin))
           (new (cond ((and new-bin (= new-mode ngit-mode-gitlink))
                       (concat "Subproject commit " (ngit-hex new-bin) (if dirty "-dirty" "") "\n"))
                      (new-data new-data)
                      (t (ngit--diff-blob repo new-mode new-bin)))))
      (apply #'ngit--emit (nreverse header))
      (cond
       ((and (or (and old-bin (/= old-mode ngit-mode-gitlink))
                 (and new-bin (/= new-mode ngit-mode-gitlink)))
             (or (ngit--binary-p repo path-a old) (ngit--binary-p repo path-b new)))
        (unless (and (equal old-bin new-bin) (not dirty))
          (ngit--emit "Binary files " lbl-a " and " lbl-b " differ\n")))
       (t
        (let ((hunks (ngit-xdiff-unified old new ctx)))
          (unless (string= hunks "")
            ;; diff.c appends a tab to a name containing a space.
            (ngit--emit "--- " lbl-a (if (and old-bin (string-search " " path-a)) "\t" "")
                        "\n+++ " lbl-b
                        (if (and (or new-bin new-data) (string-search " " path-b)) "\t" "")
                        "\n" hunks))))))))

(defun ngit--emit-patch (repo c ctx)
  (let* ((path (aref c 0))
         (old-path (or (aref c 8) path))
         (old-mode (aref c 2)) (new-mode (aref c 4))
         (flags (aref c 6))
         (dirty (and (listp flags) (or (memq 'modified flags) (memq 'untracked flags))))
         (new-bin (aref c 5)))
    (if (and (aref c 3) (or new-bin (aref c 7))
             (/= (logand old-mode #o170000) (logand new-mode #o170000)))
        (progn
          (ngit--emit-patch-part repo old-path path old-mode (aref c 3) 0 nil nil nil nil ctx)
          (ngit--emit-patch-part repo old-path path 0 nil new-mode new-bin (aref c 7) nil nil ctx))
      (ngit--emit-patch-part repo old-path path old-mode (aref c 3) new-mode new-bin
                             (aref c 7) dirty (= (aref c 1) ?R) ctx))))

(defun ngit--compose-tree-wt (tree-changes wt-changes)
  "Combine tree->index and index->work tree changes into tree->work tree."
  (let ((wt (make-hash-table :test 'equal)) (out nil) (seen (make-hash-table :test 'equal)))
    (dolist (w wt-changes) (puthash (aref w 0) w wt))
    (dolist (c tree-changes)
      (let ((w (gethash (aref c 0) wt)))
        (puthash (aref c 0) t seen)
        (cond
         ((null w) (push c out))
         ((= (aref w 1) ?D)
          (when (aref c 3)
            (push (ngit-change (aref c 0) ?D (aref c 2) (aref c 3) 0 nil) out)))
         ((null (aref c 3))
          (let ((n (copy-sequence w))) (aset n 1 ?A) (aset n 2 0) (aset n 3 nil) (push n out)))
         (t
          (unless (and (equal (aref c 3) (aref w 5)) (= (aref c 2) (aref w 4)) (null (aref w 6)))
            (let ((n (copy-sequence w)))
              (aset n 2 (aref c 2)) (aset n 3 (aref c 3))
              (aset n 1 (if (= (logand (aref c 2) #o170000) (logand (aref w 4) #o170000)) ?M ?T))
              (push n out)))))))
    (dolist (w wt-changes)
      (unless (gethash (aref w 0) seen) (push w out)))
    (sort out (lambda (x y) (string< (aref x 0) (aref y 0))))))

(defun ngit--tree-of (repo rev)
  (ngit-unhex (ngit-peel repo (ngit-rev-parse repo rev) 'tree)))

(defun ngit-cmd-diff (repo args)
  (let ((cached nil) (ctx 3) (out 'patch) (renames (ngit-config-get repo "diff.renames"))
        (revs nil))
    (setq renames (if (null renames) t
                    (or (equal renames "copies") (equal renames "copy")
                        (ngit-config-bool repo "diff.renames" t))))
    (while args
      (let ((a (pop args)))
        (cond ((member a '("--cached" "--staged")) (setq cached t))
              ((string-match "\\`\\(?:-U\\|--unified=\\)\\([0-9]+\\)\\'" a)
               (setq ctx (string-to-number (match-string 1 a))))
              ((string= a "--name-only") (setq out 'name-only))
              ((string= a "--name-status") (setq out 'name-status))
              ((string= a "--no-renames") (setq renames nil))
              ((member a '("-M" "--find-renames")) (setq renames t))
              ((string= a "--") nil)
              ((string-prefix-p "-" a) (ngit--die "unsupported diff option %s" a))
              ((string-match "\\`\\(.*\\)\\.\\.\\(.*\\)\\'" a)
               (push (if (string= (match-string 1 a) "") "HEAD" (match-string 1 a)) revs)
               (push (if (string= (match-string 2 a) "") "HEAD" (match-string 2 a)) revs))
              (t (push a revs)))))
    (setq revs (nreverse revs))
    (let ((changes
           (cond
            ((= (length revs) 2)
             (ngit-diff-trees repo (ngit--tree-of repo (car revs)) (ngit--tree-of repo (cadr revs))))
            (cached
             (let ((head (ngit-resolve-ref repo "HEAD")))
               (ngit-diff-tree-index repo (cond (revs (ngit--tree-of repo (car revs)))
                                                (head (ngit--tree-of repo "HEAD"))))))
            (revs
             (ngit--compose-tree-wt (ngit-diff-tree-index repo (ngit--tree-of repo (car revs)))
                                    (ngit-wt-changes repo t)))
            (t (ngit-wt-changes repo t)))))
      (when renames (setq changes (ngit-detect-renames changes)))
      (dolist (c changes)
        (pcase out
          ('name-only (ngit--emit (ngit-quote-path (aref c 0)) "\n"))
          ('name-status
           (ngit--emit (if (= (aref c 1) ?R) "R100" (string (if (eq (aref c 6) 'ita) ?A (aref c 1))))
                       "\t"
                       (if (aref c 8) (concat (ngit-quote-path (aref c 8)) "\t") "")
                       (ngit-quote-path (aref c 0)) "\n"))
          (_ (ngit--emit-patch repo c ctx)))))
    0))

(provide 'ngit-diff)

;;; ngit-diff.el ends here
