;;; ngit-paths.el --- invocation path selection and display -*- lexical-binding: t; -*-

;;; Commentary:

;; Repository, index and tree paths remain relative to the work-tree root.
;; Only command arguments and human status output use the invocation cwd.

;;; Code:

(unless (featurep 'ngit-pathspec)
  (load (expand-file-name "ngit-pathspec.el"
                          (file-name-directory (or load-file-name buffer-file-name))) nil t))

(defvar ngit--invocation-directory nil
  "Directory after the command line's repeated -C options were applied.")

(defun ngit-paths-items (repo paths &optional magic)
  "Normalize literal PATHS using the existing add/restore pathspec API."
  (let ((default-directory (or ngit--invocation-directory default-directory)))
    (let ((items (mapcar (lambda (path)
              (when (string= path "")
                (ngit--die "fatal: empty string is not a valid pathspec. please use . instead if you meant to match all paths"))
              (let ((item (or (and magic (ngit-pathspec-compile repo path))
                              (ngit--pathspec-item repo path))))
                ;; The shared add/restore normalizer removes trailing
                ;; separators.  Read pathspecs retain directory-only intent.
                (if (and (stringp item) (not (string= item "")) (string-suffix-p "/" path))
                    (concat item "/") item)))
            paths)))
      (when (and magic items (seq-every-p (lambda (item) (and (vectorp item) (aref item 4))) items))
        (setq items (cons "" items)))
      items)))

(defun ngit-pathspec-items (repo paths)
  "Normalize PATHS and explicitly compile magic for pathspec-taking commands."
  (ngit-paths-items repo paths t))

(defun ngit-paths-covers-p (item path)
  "Whether root-relative literal ITEM selects PATH (the root is empty)."
  (or (string= item "") (string= item path)
      (and (string-prefix-p item path)
           (> (length path) (length item))
           (or (string-suffix-p "/" item)
               (= (aref path (length item)) ?/)))))

(defun ngit-paths-match-p (items path)
  "Whether root-relative PATH is selected by ITEMS, or ITEMS is nil."
  (or (null items) (ngit-pathspec-match-p items path)))

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
