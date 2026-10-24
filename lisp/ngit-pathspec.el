;;; ngit-pathspec.el --- shared Git pathspec magic -*- lexical-binding: t; -*-

;;; Commentary:
;; Ordinary path APIs retain their string representation.  Magic items use
;; [PATH LITERAL GLOB ICASE EXCLUDE DIRECTORY ORIGINAL PREFIX-LENGTH], with root-relative
;; unibyte PATH.  Compilation completes before selection or mutation.

;;; Code:

(defun ngit-pathspec-magic-p (items)
  "Whether ITEMS contains an explicitly compiled magic selector."
  (seq-some #'vectorp items))

(defun ngit-pathspec-compile (repo arg)
  "Compile explicit magic in ARG, or return nil for an ordinary path."
  (when (and (string-prefix-p ":" arg) (not (string= arg ":")))
    (let ((rest (substring arg 1)) (flags nil) (names nil))
      (if (string-prefix-p "(" rest)
          (let ((end (string-search ")" rest)))
            (unless end (ngit--die "fatal: Missing ')' at the end of pathspec magic in '%s'" arg))
            (setq names (split-string (substring rest 1 end) "," t)
                  rest (substring rest (1+ end))))
        (while (and (> (length rest) 0) (memq (aref rest 0) '(?/ ?! ?^)))
          (push (if (= (aref rest 0) ?/) "top" "exclude") names)
          (setq rest (substring rest 1)))
        (when (string-prefix-p ":" rest) (setq rest (substring rest 1))))
      (dolist (name names)
        (unless (member name '("top" "literal" "glob" "icase" "exclude"))
          (ngit--die "fatal: Invalid pathspec magic '%s' in '%s'" name arg))
        (push name flags))
      (when (and (member "glob" flags) (member "literal" flags))
        (ngit--die "fatal: %s: 'literal' and 'glob' are incompatible" arg))
      (let* ((default-directory
              (if (member "top" flags)
                  (file-name-as-directory (ngit-repo-toplevel repo))
                (or ngit--invocation-directory default-directory)))
             (path (ngit--pathspec-item repo (if (string= rest "") "." rest))))
        (vector path (and (member "literal" flags) t)
                (and (member "glob" flags) t) (and (member "icase" flags) t)
                (and (member "exclude" flags) t) (string-suffix-p "/" rest) arg
                (ngit-pathspec-prefix-length repo rest path (member "top" flags)))))))

(defun ngit-pathspec-prefix-length (repo rest path top)
  "Count the invocation prefix surviving lexical REST normalization in PATH."
  (if (or top (file-name-absolute-p rest)) 0
    (let ((prefix (split-string (ngit--pathspec-item repo ".") "/" t)) (depth 0))
      (dolist (part (split-string rest "/" t))
        (cond ((string= part "."))
              ((string= part "..")
               (if (> depth 0) (setq depth (1- depth))
                 (setq prefix (butlast prefix))))
              (t (setq depth (1+ depth)))))
      (min (length path) (if prefix (1+ (length (mapconcat #'identity prefix "/"))) 0)))))

(defun ngit-pathspec-item-match-p (item path)
  "Whether root-relative PATH is selected by ITEM, ignoring exclusion."
  (if (not (vectorp item))
      (ngit-paths-covers-p item path)
    (let* ((pattern (aref item 0))
           (directory (aref item 5))
           (trailing (string-suffix-p "/" path))
           (path (if trailing (substring path 0 -1) path))
           (prefix (aref item 7))
           (fixed (substring pattern 0 prefix)))
      (when (aref item 3)
        (setq pattern (concat fixed (downcase (substring pattern prefix))))
        (when (string-prefix-p fixed path)
          (setq path (concat fixed (downcase (substring path prefix))))))
      (or (string= pattern "")
          (and (or (not directory) trailing (not (string= pattern path)))
               (if (or (aref item 1) (not (ngit--glob-special-p pattern 0)))
                   (ngit-paths-covers-p pattern path)
                 (ngit-wildmatch (if directory (concat pattern "/") pattern)
                                 (if trailing (concat path "/") path)
                                 (aref item 2))))))))

(defun ngit-pathspec-match-p (items path)
  "Match the union of positive ITEMS, then subtract all exclusions."
  (let ((positive nil) (selected nil) (excluded nil))
    (dolist (item items)
      (if (and (vectorp item) (aref item 4))
          (when (ngit-pathspec-item-match-p item path) (setq excluded t))
        (setq positive t)
        (when (ngit-pathspec-item-match-p item path) (setq selected t))))
    (and (or selected (not positive)) (not excluded))))

(defun ngit-pathspec-class (items path dirp)
  "Classify PATH for scoped magic tree walks; directories descend conservatively."
  (if dirp 'partial
    (and (ngit-pathspec-match-p items path) 'full)))

(defun ngit-pathspec-expand (repo items candidates &optional restore)
  "Return literal CANDIDATES selected by compiled ITEMS, validating positives."
  (dolist (item items)
    (unless (or (and (stringp item) (string= item ""))
                (and (vectorp item) (or (aref item 4) (string= (aref item 0) "")))
                (seq-some (lambda (path) (ngit-pathspec-item-match-p item path)) candidates))
      (let ((name (if (vectorp item) (aref item 6) (decode-coding-string item 'utf-8))))
        (if restore
            (ngit--die-with 1 "error: pathspec '%s' did not match any file(s) known to git" name)
          (ngit--die "fatal: pathspec '%s' did not match any files" name)))))
  (seq-filter (lambda (path) (ngit-pathspec-match-p items path)) candidates))

(defun ngit-pathspec-write-items (repo paths &optional restore tree)
  "Expand magic PATHS before add/restore writes; ordinary contracts stay literal."
  (let ((items (ngit-pathspec-items repo paths)))
    (if (not (ngit-pathspec-magic-p items))
        (mapcar (lambda (path) (ngit--pathspec-item repo path)) paths)
      (let ((candidates nil) (entries (ngit-index-entries repo)))
        (dotimes (i (length entries))
          (let ((entry (aref entries i)))
            (push (if (= (aref entry 2) #o160000)
                      (concat (aref entry 0) "/") (aref entry 0)) candidates)))
        (if restore
            (when tree
              (ngit--flatten-tree repo tree ""
                                  (lambda (path mode _bin)
                                    (push (if (= mode #o160000) (concat path "/") path) candidates))))
          (let ((top (file-name-as-directory (ngit-repo-toplevel repo))))
            (ngit-wt-setup top)
            (ngit--walk-untracked repo top (ngit--tracked-set entries) ""
                                  (ngit-ignore-global-patterns repo)
                                  (lambda (path) (push path candidates)) nil t)
            ;; Explicit existing literals must reach add's ignored-path
            ;; advisory; wildcard discovery continues to skip ignored paths.
            (dolist (item items)
              (when (and (vectorp item) (not (aref item 4))
                         (or (aref item 1) (not (ngit--glob-special-p (aref item 0) 0)))
                         (ngit-wt-lstat (aref item 0)))
                (push (aref item 0) candidates)))))
        (setq candidates (sort (delete-dups candidates) #'string<))
        (let ((selected (ngit-pathspec-expand repo items candidates restore)))
          ;; Git's write-side exact icase lookup resolves the first index
          ;; spelling when case-colliding files coexist on a sensitive disk.
          (dolist (item items)
            (when (and (vectorp item) (aref item 3) (not (aref item 4))
                       (or (aref item 1) (not (ngit--glob-special-p (aref item 0) 0))))
              (let ((first (seq-find (lambda (path)
                                       (and (ngit-pathspec-item-match-p item path)
                                            (string= (downcase path) (downcase (aref item 0))))) candidates)))
                (when first
                  (setq selected
                        (seq-filter (lambda (path)
                                      (or (string= path first)
                                          (not (string= (downcase path) (downcase first)))
                                          (seq-some (lambda (other)
                                                      (and (not (eq other item))
                                                           (not (and (vectorp other) (aref other 4)))
                                                           (ngit-pathspec-item-match-p other path))) items)))
                                    selected))))))
          (mapcar (lambda (path) (if (string-suffix-p "/" path) (substring path 0 -1) path)) selected))))))

(defun ngit-pathspec-ls-tree-item (item)
  "Convert supported ls-tree magic ITEM to its literal API representation."
  (if (not (vectorp item)) item
    (let ((unsupported nil))
      (when (aref item 2) (push "'glob'" unsupported))
      (when (aref item 3) (push "'icase'" unsupported))
      (when (aref item 4) (push "'exclude' (mnemonic: '!')" unsupported))
      (when unsupported
        (ngit--die "fatal: %s: pathspec magic not supported by this command: %s"
                   (aref item 6) (mapconcat #'identity (nreverse unsupported) ", ")))
      (concat (aref item 0) (if (and (aref item 5) (not (string= (aref item 0) ""))) "/" "")))))

(provide 'ngit-pathspec)
;;; ngit-pathspec.el ends here
