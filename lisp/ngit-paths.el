;;; ngit-paths.el --- invocation path selection and display -*- lexical-binding: t; -*-

;;; Commentary:

;; Repository, index and tree paths remain relative to the work-tree root.
;; Only command arguments and human status output use the invocation cwd.

;;; Code:

(defvar ngit--invocation-directory nil
  "Directory after the command line's repeated -C options were applied.")

(defun ngit-paths-items (repo paths)
  "Normalize literal PATHS using the existing add/restore pathspec API."
  (let ((default-directory (or ngit--invocation-directory default-directory)))
    (mapcar (lambda (path)
              (when (string= path "")
                (ngit--die "fatal: empty string is not a valid pathspec. please use . instead if you meant to match all paths"))
              (let ((item (ngit--pathspec-item repo path)))
                ;; The shared add/restore normalizer removes trailing
                ;; separators.  Read pathspecs retain directory-only intent.
                (if (and (not (string= item "")) (string-suffix-p "/" path))
                    (concat item "/") item)))
            paths)))

(defun ngit-paths-covers-p (item path)
  "Whether root-relative literal ITEM selects PATH (the root is empty)."
  (or (string= item "") (string= item path)
      (and (string-prefix-p item path)
           (> (length path) (length item))
           (or (string-suffix-p "/" item)
               (= (aref path (length item)) ?/)))))

(defun ngit-paths-match-p (items path)
  "Whether root-relative PATH is selected by ITEMS, or ITEMS is nil."
  (or (null items)
      (seq-some (lambda (item) (ngit-paths-covers-p item path)) items)))

(defun ngit-paths-filter-changes (items changes)
  "Select root-relative CHANGES before rename detection."
  (if (null items) changes
    (seq-filter (lambda (change)
                  (ngit-paths-match-p
                   items
                   (if (or (= (aref change 2) #o160000) (= (aref change 4) #o160000))
                       (concat (aref change 0) "/")
                     (aref change 0))))
                changes)))

(defun ngit-paths-prefix (repo)
  "Return the invocation directory relative to REPO's work-tree root."
  (car (ngit-paths-items repo '("."))))

(defun ngit-paths-relative (prefix path)
  "Display root-relative PATH from root-relative directory PREFIX."
  (if (string= prefix "") path
    (let ((dirs (split-string prefix "/" t))
          (parts (split-string path "/" t))
          (trailing (string-suffix-p "/" path)))
      (while (and dirs parts (string= (car dirs) (car parts)))
        (setq dirs (cdr dirs) parts (cdr parts)))
      (let ((out (concat (apply #'concat (mapcar (lambda (_) "../") dirs))
                         (mapconcat #'identity parts "/"))))
        (cond ((string= out "") "./")
              ((and trailing parts) (concat out "/"))
              (t out))))))

(provide 'ngit-paths)

;;; ngit-paths.el ends here
