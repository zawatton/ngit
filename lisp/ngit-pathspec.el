;;; ngit-pathspec.el --- shared Git pathspec magic -*- lexical-binding: t; -*-

;;; Commentary:
;; Ordinary path APIs retain their string representation.  Magic items use
;; [PATH LITERAL GLOB ICASE EXCLUDE DIRECTORY ORIGINAL PREFIX-LENGTH ATTR FIXED], with root-relative
;; unibyte PATH.  Compilation completes before selection or mutation.
;; ATTR holds the repository and compiled requirements, so matching callers
;; need no extra repository argument.  Its implementation is loaded on demand.

;;; Code:

(defun ngit-pathspec-magic-p (items)
  "Whether ITEMS contains an explicitly compiled magic selector."
  (seq-some #'vectorp items))

(defun ngit-pathspec-mixed-items (repo paths items)
  "Compile ordinary PATHS when sharing the magic matcher with ITEMS."
  (let ((originals paths))
    (when (> (length items) (length paths))
      ;; Keep the implicit positive last to align original arguments.
      (setq items (append (cdr items) (list (car items)))))
    (mapcar (lambda (item)
              (let ((arg (pop originals)))
                (if (and arg (not (vectorp item)))
                    (let ((compiled (ngit-pathspec-compile repo (concat ":()" arg))))
                      (aset compiled 6 arg)
                      compiled)
                  item))) items)))

(defun ngit-pathspec-compile (repo arg)
  "Compile explicit magic in ARG, or return nil for an ordinary path."
  (when (and (string-prefix-p ":" arg) (not (string= arg ":")))
    (let ((rest (substring arg 1)) (flags nil) (names nil) (attrs nil)
          (attr-seen nil) (attr-flag nil) (missing-end nil))
      (if (string-prefix-p "(" rest)
          (let* ((closing (ngit-pathspec-magic-delimiter rest 1 ")"))
                 (end (or closing (length rest))))
            (setq missing-end (null closing))
            (let ((start 1) next)
              (while (< start end)
                (setq next (or (ngit-pathspec-magic-delimiter rest start ",)") end))
                (when (> next start) (push (substring rest start next) names))
                (setq start (1+ next)))
              (setq names (nreverse names)
                    rest (if closing (substring rest (1+ end)) ""))))
        (while (and (> (length rest) 0) (memq (aref rest 0) '(?/ ?! ?^)))
          (push (if (= (aref rest 0) ?/) "top" "exclude") names)
          (setq rest (substring rest 1)))
        (when (string-prefix-p ":" rest) (setq rest (substring rest 1))))
      (dolist (name names)
        (cond ((string= name "attr") (setq attr-flag t))
         ((string-prefix-p "attr:" name)
            (progn
              (when attr-seen (ngit--die "fatal: Only one 'attr:' specification is allowed."))
              (setq attr-seen t)
              (unless (featurep 'ngit-pathspec-attr) (require 'ngit-pathspec-attr))
              (setq attrs (ngit-pathspec-attr-compile repo (substring name 5)))))
         (t
          (unless (member name '("top" "literal" "glob" "icase" "exclude"))
            (ngit--die "fatal: Invalid pathspec magic '%s' in '%s'" name arg))
          (push name flags))))
      (when missing-end
        (ngit--die "fatal: Missing ')' at the end of pathspec magic in '%s'" arg))
      (when (and attr-flag (not attrs))
        (unless (featurep 'ngit-pathspec-attr) (require 'ngit-pathspec-attr))
        (setq attrs (ngit-pathspec-attr-compile repo " ")))
      (when (and (member "glob" flags) (member "literal" flags))
        (ngit--die "fatal: %s: 'literal' and 'glob' are incompatible" arg))
      (let* ((default-directory
              (if (member "top" flags)
                  (file-name-as-directory (ngit-repo-toplevel repo))
                (or ngit--invocation-directory default-directory)))
             (path (ngit--pathspec-item repo (if (string= rest "") "." rest)))
             (prefix (ngit-pathspec-prefix-length repo rest path (member "top" flags)))
             (wild (and (not (member "literal" flags))
                        (string-match "[*?\\\\[]" path prefix))))
        (vector path (and (member "literal" flags) t)
                (and (member "glob" flags) t) (and (member "icase" flags) t)
                (and (member "exclude" flags) t) (string-suffix-p "/" rest) arg
                prefix attrs
                (if wild (substring path 0 wild) path))))))

(defun ngit-pathspec-magic-delimiter (text start delimiters)
  "Find the first unescaped DELIMITERS character in TEXT after START."
  (let ((pattern (concat "\\\\.\\|[" delimiters "]")) found)
    (while (and (not found) (string-match pattern text start))
      (setq start (match-end 0))
      (unless (= (aref text (match-beginning 0)) ?\\)
        (setq found (match-beginning 0))))
    found))

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
           (attr-path path)
           (prefix (aref item 7))
           (fixed (substring pattern 0 prefix)))
      ;; The invocation directory is always literal, even when its name
      ;; contains wildmatch syntax.  Case folding also starts after it.
      (and (string-prefix-p fixed (if trailing (concat path "/") path))
           ;; A normalized . may consume the entire pattern without a
           ;; trailing slash; its descendants still need a component boundary.
           (or (< prefix (length pattern)) (= prefix 0)
               (= prefix (length path))
               (and (> (length path) prefix) (= (aref path prefix) ?/)))
           (or (<= (length item) 8) (null (aref item 8))
               (ngit-pathspec-attr-match-p (aref item 8) attr-path))
           (let ((suffix-trailing (and trailing (>= (length path) prefix)))
                 (pattern (substring pattern prefix))
                 (path (if (< (length path) prefix) "" (substring path prefix))))
             (when (aref item 3)
               (setq pattern (downcase pattern) path (downcase path)))
             (or (string= pattern "")
                 (and (or (not directory) trailing (not (string= pattern path)))
                      ;; Git checks a literal exact/recursive match before
                      ;; wildcard matching, including explicit glob items.
                      (or (ngit-paths-covers-p pattern path)
                          (and (not (aref item 1)) (ngit--glob-special-p pattern 0)
                               (ngit-wildmatch (if directory (concat pattern "/") pattern)
                                               (if suffix-trailing (concat path "/") path)
                                               (aref item 2)))))))))))

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

(defun ngit-pathspec-attr-p (items)
  "Whether any of ITEMS carries attribute requirements."
  (seq-some (lambda (item) (and (vectorp item) (aref item 8))) items))

(defun ngit-pathspec-untracked-class (items path)
  "Classify an untracked directory PATH, including leading pathspec matches.
Unlike tracked tree walks, Git tests attributes on the directory itself
before descending into an untracked directory."
  (let ((directory (concat path "/")) (selected nil) (excluded nil))
    (dolist (item items)
      (if (and (vectorp item) (aref item 4))
          (when (ngit-pathspec-item-match-p item directory) (setq excluded t))
        (if (ngit-pathspec-item-match-p item directory)
            (setq selected 'full)
          (when (and (not (eq selected 'full))
                     (or (not (vectorp item)) (not (aref item 8))
                         (ngit-pathspec-attr-match-p (aref item 8) path)))
            (let* ((pattern (if (vectorp item) (aref item 0) item))
                   (name path)
                   (prefix (if (vectorp item) (aref item 9) item))
                   (fixed (and (vectorp item)
                               (substring pattern 0 (aref item 7)))))
              (when (and fixed (aref item 3)
                         (string-prefix-p fixed (concat name "/")))
                (setq pattern (downcase pattern) name (downcase name) prefix (downcase prefix)))
              (let ((leading (concat name "/")))
                (when (and (or (not fixed) (string-prefix-p fixed directory)
                               (string-prefix-p directory fixed))
                           (or (string-prefix-p leading pattern)
                               (and (< (length prefix) (length pattern))
                                    (or (string-prefix-p prefix leading)
                                        (string-prefix-p leading prefix)))))
                  (setq selected 'partial))))))))
    (and (not excluded) selected)))

(defun ngit-pathspec-restore-expand (items candidates)
  "Select CANDIDATES and report all unmatched original ITEMS like checkout.
Once any positive matches, checkout considers all exclusions satisfied."
  (let ((seen (make-vector (length items) nil)) selected misses matched-positive)
    (dolist (path candidates)
      (let ((i 0) positives negatives)
        (dolist (item items)
          (when (ngit-pathspec-item-match-p item path)
            (if (and (vectorp item) (aref item 4))
                (push i negatives)
              (push i positives)))
          (setq i (1+ i)))
        (when positives
          (setq matched-positive t)
          (dolist (position positives) (aset seen position t))
          (unless negatives (push path selected)))))
    (let ((i 0))
      (dolist (item items)
        (unless (or (aref seen i) (and (stringp item) (string= item ""))
                    (and matched-positive (vectorp item) (aref item 4)))
          (push (concat "error: pathspec '"
                        (if (vectorp item) (aref item 6) (decode-coding-string item 'utf-8))
                        "' did not match any file(s) known to git") misses))
        (setq i (1+ i))))
    (when misses (ngit--die-with 1 "%s" (mapconcat #'identity (nreverse misses) "\n")))
    (nreverse selected)))

(defun ngit-pathspec-expand (repo items candidates &optional restore)
  "Return literal CANDIDATES selected by compiled ITEMS, validating positives."
  (if restore (ngit-pathspec-restore-expand items candidates)
  (dolist (item items)
    (unless (or (and (stringp item) (string= item ""))
                (and (vectorp item)
                     (or (aref item 4)
                         (and (not restore) (string= (aref item 0) ""))
                         ;; Add's exact-path existence fallback ignores attr
                         ;; requirements; restore validates actual matches.
                         (and (not restore) (not (aref item 2)) (not (aref item 3))
                              (ngit-wt-lstat (aref item 0)))))
                (seq-some (lambda (path) (ngit-pathspec-item-match-p item path)) candidates))
      (let ((name (if (vectorp item) (aref item 6) (decode-coding-string item 'utf-8))))
        (ngit--die "fatal: pathspec '%s' did not match any files" name))))
  (seq-filter (lambda (path) (ngit-pathspec-match-p items path)) candidates)))

(defun ngit-pathspec-write-items (repo paths &optional restore tree)
  "Expand magic PATHS before add/restore writes; ordinary contracts stay literal."
  (let ((items (ngit-pathspec-items repo paths)))
    (if (not (ngit-pathspec-magic-p items))
        (mapcar (lambda (path) (ngit--pathspec-item repo path)) paths)
      ;; Ordinary items need their wildcard and literal invocation prefix
      ;; information too when they enter the shared magic matcher.
      (setq items (ngit-pathspec-mixed-items repo paths items))
      (let ((candidates nil) (entries (ngit-index-entries repo))
            (ngit--untracked-pathspecs (and (ngit-pathspec-attr-p items) items)))
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
                         (let* ((path (aref item 0)) (st (ngit-wt-lstat path))
                                (dir (or (file-name-directory path) ""))
                                (ctx (ngit--ignore-context repo dir)))
                           (and st
                                (or (cdr ctx)
                                    (ngit-excluded-p (car ctx) path (length dir)
                                                     (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir)))
                                ;; Attribute requirements select leaves.  A
                                ;; directory placeholder must not turn the
                                ;; expanded result back into a recursive add.
                                (not (and (aref item 8)
                                          (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir))))))
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
      (when (and (> (length item) 8) (aref item 8)) (push "'attr'" unsupported))
      (when unsupported
        (ngit--die "fatal: %s: pathspec magic not supported by this command: %s"
                   (aref item 6) (mapconcat #'identity (nreverse unsupported) ", ")))
      (concat (aref item 0) (if (and (aref item 5) (not (string= (aref item 0) ""))) "/" "")))))

(provide 'ngit-pathspec)
;;; ngit-pathspec.el ends here
