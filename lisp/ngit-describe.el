;;; ngit-describe.el --- names and revision inspection -*- lexical-binding: t; -*-

;;; Commentary:
;; Revision inspection using the same commit parser and walker as log.

;;; Code:

(require 'ngit-log)

(defvar ngit-inspect--pending nil)
(defvar ngit-inspect--object-directories nil)

(defun ngit-inspect--walk (repo args &optional require-revision)
  "Return commits selected by revision ARGS, preserving revision.c order."
  (setq ngit--rv-repo repo ngit--rv-commits nil ngit--rv-limited nil
        ngit--rv-first-parent nil ngit--rv-paths nil ngit--rv-dense t
        ngit--rv-simplify-history t ngit--rv-rewrite-parents nil ngit--rv-topo nil
        ngit--rv-max-age nil ngit--rv-min-age nil ngit--rv-min-parents 0
        ngit--rv-max-parents -1 ngit--rv-grep nil ngit--rv-max-count -1
        ngit--rv-skip 0 ngit--rv-reverse nil ngit--rv-no-walk nil)
  (let ((pending nil) (seen nil) (flags 0) (commits nil) (c nil))
    (while args
      (let* ((a (pop args)) (v nil))
        (cond
         ((string= a "--all")
          (setq seen t pending (ngit--add-refs repo "refs/" nil flags pending))
          (let ((h (ngit-resolve-ref repo "HEAD")))
            (when h (push (cons (ngit-commit h) flags) pending))))
         ((string= a "--not") (setq flags (logxor flags (logior ngit-uninteresting ngit-bottom))))
         ((string= a "--reverse") (setq ngit--rv-reverse t))
         ((string= a "--parents") (setq ngit--rv-rewrite-parents t))
         ((string= a "--first-parent") (setq ngit--rv-first-parent t))
         ((string= a "--topo-order") (setq ngit--rv-topo 'graph))
         ((string= a "--date-order") (setq ngit--rv-topo 'date))
         ((string= a "--no-merges") (setq ngit--rv-max-parents 1))
         ((string= a "--merges") (setq ngit--rv-min-parents 2))
         ((setq v (or (ngit--log-opt-value a "--max-count" args)
                      (ngit--log-opt-value a "-n" args)))
          (setq ngit--rv-max-count (string-to-number (car v)) args (cdr v)))
         ((string-match "\\`-\\([0-9]+\\)\\'" a)
          (setq ngit--rv-max-count (string-to-number (match-string 1 a))))
         ((string= a "--")
          (setq ngit--rv-paths (ngit--log-pathspec repo args) args nil))
         (t
          (let ((r (ngit--handle-revision repo a flags pending)))
            (when (eq r 'not-a-rev) (ngit--bad-revision a))
            (setq seen t pending r))))))
    (unless seen
      (when require-revision (ngit--die-with 129 "usage: git rev-list [options] <commit>..."))
      (let ((h (ngit-resolve-ref repo "HEAD")))
        (unless h (ngit--bad-revision "HEAD"))
        (push (cons (ngit-commit h) 0) pending)))
    (setq ngit-inspect--pending (nreverse pending))
    (ngit-walk-prepare repo ngit-inspect--pending)
    (while (setq c (ngit-walk-next)) (push c commits))
    (if ngit--rv-reverse commits (nreverse commits))))

(defun ngit-inspect--directory-selected (path)
  "Match PATH as a nonrecursive tree-walk entry, as in tree-walk.c.
History simplification descends conservatively for wildcard matches;
list-objects instead requires a matching tree name or a literal prefix."
  (let ((selected nil) (excluded nil)
        (items (if (eq (cdar ngit--rv-paths) 'magic)
                   ngit-inspect--object-directories ngit--rv-paths)))
    (dolist (item items)
      (let* ((magic (vectorp item))
             (pattern (cond (magic (aref item 0)) ((consp item) (car item)) (t item)))
             (name path)
             (fixed (and magic (substring pattern 0 (aref item 7))))
             (prefix-ok (or (not fixed) (string-prefix-p fixed (concat path "/"))
                            (string-prefix-p (concat path "/") fixed)))
             (negative (and magic (aref item 4)))
             (matched (if magic
                          (ngit-pathspec-item-match-p
                           item (if (aref item 5) (concat path "/") path))
                        (or (ngit-paths-covers-p pattern path)
                            (and (consp item) (eq (cdr item) t) (ngit-wildmatch pattern path))))))
        (when (and magic (aref item 3))
          (setq pattern (downcase pattern) name (downcase name)))
        (if negative
            ;; An exclusion matching this tree's name is entry_interesting:
            ;; emit the tree and test its children individually. Only a
            ;; literal ancestor is all_entries_interesting and prunes it.
            (when (and prefix-ok
                       (or (equal pattern "")
                           (string-prefix-p (concat pattern "/") name)))
              (setq excluded t))
          (when (or matched (and prefix-ok (string-prefix-p (concat name "/") pattern)))
            (setq selected t)))))
    (and selected (not excluded))))

(defun ngit-inspect--objects (repo hex path seen emit)
  "Visit a tree and its blobs in list-objects.c order, once per object."
  (unless (gethash hex seen)
    (puthash hex t seen)
    (when emit
      (let ((nl (string-search "\n" path)))
        (ngit--emit hex " " (if nl (substring path 0 nl) path) "\n")))
    (let ((obj (ngit-read-object repo (ngit-unhex hex))))
      (when (eq (car obj) 'tree)
        (dolist (e (ngit-parse-tree (cdr obj)))
          (let ((name (concat path (if (string= path "") "" "/") (nth 1 e))))
            (when (and (not (string= (car e) "160000"))
                       (or (not emit) (not ngit--rv-paths)
                           (if (string= (car e) "40000")
                               (ngit-inspect--directory-selected name)
                             (ngit--log-path-class name nil nil))))
              (ngit-inspect--objects repo (ngit-hex (nth 2 e)) name seen emit))))))))

(defun ngit-cmd-rev-list (repo args)
  (let* ((count (member "--count" args)) (objects (member "--objects" args))
         (parents (member "--parents" args))
         (walk-args (delete "--objects" (delete "--count" (copy-sequence args))))
         (commits (ngit-inspect--walk repo walk-args t))
         (ngit-inspect--object-directories
          (when (and objects (eq (cdar ngit--rv-paths) 'magic))
            ;; Tree entries use the name pattern, but cannot be rejected by
            ;; attributes of the directory: descendants may match instead.
            ;; Attribute exclusions likewise cannot exclude an entire tree.
            (delq nil
                  (mapcar (lambda (item)
                            (cond ((not (and (vectorp item) (aref item 8))) item)
                                  ((aref item 4) nil)
                                  (t (let ((copy (copy-sequence item)))
                                       (aset copy 8 nil) copy))))
                          (caar ngit--rv-paths))))))
    (if count
        (ngit--emit (number-to-string (length commits)) "\n")
      (dolist (c commits)
        (ngit--emit (ngit--c-hex c))
        (when parents
          (dolist (p (ngit--c-parents c)) (ngit--emit " " (ngit--c-hex p))))
        (ngit--emit "\n"))
      (when objects
        (let ((seen (make-hash-table :test 'equal)) (excluded nil)
              (cs (make-hash-table :test 'equal)))
          ;; Uninteresting commits exclude their entire object closure.
          (dolist (s ngit-inspect--pending)
            (when (/= 0 (logand (cdr s) ngit-uninteresting)) (push (car s) excluded)))
          (while excluded
            (let ((c (pop excluded)))
              (unless (gethash (ngit--c-hex c) cs)
                (puthash (ngit--c-hex c) t cs)
                (ngit--parse c)
                (ngit-inspect--objects repo (ngit--c-tree c) "" seen nil)
                (setq excluded (append (ngit--c-parents c) excluded)))))
          ;; Pending annotated tags precede the trees queued by commits.
          (dolist (r (if (member "--all" args) (ngit-list-refs repo)
                       (let ((rs nil))
                         (dolist (a args)
                           (unless (string-prefix-p "-" a)
                             (let ((d (ngit-dwim-ref repo a)))
                               (when d (push (list a (cdr d)) rs)))))
                         (nreverse rs))))
            (let ((hex (nth 1 r))
                  (name (if (string-prefix-p "refs/tags/" (car r)) (substring (car r) 10) (car r)))
                  (obj nil))
              (setq obj (ngit-read-object repo (ngit-unhex hex)))
              (while (eq (car obj) 'tag)
                (unless (gethash hex seen)
                  (puthash hex t seen) (ngit--emit hex " " name "\n"))
                (setq hex (ngit-header (ngit-parse-commit (cdr obj)) "object")
                      obj (ngit-read-object repo (ngit-unhex hex))))
              (when (memq (car obj) '(tree blob))
                (ngit-inspect--objects repo hex "" seen t))))
          (dolist (c commits) (ngit-inspect--objects repo (ngit--c-tree c) "" seen t)))))
    0))

(defun ngit-inspect--ident (ident)
  "Return (NAME MAIL STAMP ZONE) from a commit identity."
  (let* ((parts (ngit--split-ident ident)) (who (car parts))
         (lt (string-search "<" who)) (gt (string-search ">" who)))
    (list (if lt (string-trim-right (substring who 0 lt)) who)
          (if (and lt gt) (substring who lt (1+ gt)) "<>")
          (nth 1 parts) (nth 2 parts))))

(defun ngit-inspect--wrap (text)
  "Wrap a shortlog subject at column 76, with six/nine column indents."
  (let ((words (split-string text "[ \t\r\n]+" t)) (line "      ") (width 6) (have nil))
    (dolist (word words)
      (let ((w (ngit--display-width word)))
        (if (and have (> (+ width 1 w) 76))
            (progn (ngit--emit line "\n") (setq line (concat "         " word) width (+ 9 w)))
          (let ((space (if have " " "")))
            (setq line (concat line space word) width (+ width (length space) w))))
        (setq have t)))
    (ngit--emit line "\n")))

(defun ngit-inspect--stdin-tty-p ()
  "Test stdin with Linux TCGETS, without consuming any input."
  (ngit-native--with-memory (memory 64)
    (= (syscall-direct 16 0 #x5401 (nl-ffi-memory-address memory) 0 0 0) 0)))

(defun ngit-cmd-shortlog (repo args)
  (let ((summary nil) (number nil) (email nil) (wrap nil) (revs nil)
        (groups (make-hash-table :test 'equal)) (names nil))
    (dolist (a args)
      (if (string= a "-w") (setq wrap t)
        (if (and (string-prefix-p "-" a) (not (string-prefix-p "--" a))
               (string-match-p "\\`-[sne]+\\'" a))
          (progn (when (string-search "s" a) (setq summary t))
                 (when (string-search "n" a) (setq number t))
                 (when (string-search "e" a) (setq email t)))
        (push a revs))))
    (dolist (c (and (or revs (ngit-inspect--stdin-tty-p))
                   (ngit-inspect--walk repo (nreverse revs))))
      (let* ((ident (ngit-inspect--ident (ngit--c-author c)))
             (name (concat (car ident) (if email (concat " " (nth 1 ident)) "")))
             (g (gethash name groups)))
        (unless g (push name names) (setq g (cons 0 nil)) (puthash name g groups))
        (setcar g (1+ (car g)))
        (unless summary (setcdr g (cons (ngit--subject (ngit--c-message c)) (cdr g))))))
    (setq names (sort names (lambda (a b)
                             (let ((ac (car (gethash a groups))) (bc (car (gethash b groups))))
                               (if (and number (/= ac bc)) (> ac bc) (string< a b))))))
    (dolist (name names)
      (let ((g (gethash name groups)))
        (if summary (ngit--emit (format "%6d\t%s\n" (car g) name))
          (ngit--emit name " (" (number-to-string (car g)) "):\n")
          (dolist (subject (cdr g))
            (let ((s (if (string= subject "") "<none>" subject)))
              (when (string-match "\\`\\[PATCH[^]]*\\] *" s) (setq s (substring s (match-end 0))))
              (if wrap (ngit-inspect--wrap s) (ngit--emit "      " s "\n"))))
          (ngit--emit "\n"))))
    0))

(defun ngit-inspect--tag (repo ref)
  "Return [COMMIT NAME ANNOTATED TAGGER-DATE OBJECT] for tag REF."
  (let* ((hex (nth 1 ref)) (obj (ngit-read-object repo (ngit-unhex hex))))
    (if (eq (car obj) 'tag)
        (let ((parsed (ngit-parse-commit (cdr obj))))
          (vector (condition-case nil (ngit-peel repo hex 'commit) (error nil))
                  (or (ngit-header parsed "tag") (substring (car ref) 10)) t
                  (nth 1 (ngit--split-ident (or (ngit-header parsed "tagger") ""))) hex))
      (vector (and (eq (car obj) 'commit) hex) (substring (car ref) 10) nil 0 hex))))

(defun ngit-inspect--reachable (repo start)
  "Return the set of commits reachable from START without changing walk flags."
  (let ((seen (make-hash-table :test 'equal)) (todo (list start)))
    (while todo
      (let ((c (pop todo)))
        (unless (gethash (ngit--c-hex c) seen)
          (puthash (ngit--c-hex c) t seen)
          (ngit-parse-commit-object repo c)
          (setq todo (append (ngit--c-orig-parents c) todo)))))
    seen))

(defun ngit-cmd-describe (repo args)
  (let ((tags nil) (always nil) (long nil) (dirty nil) (abbrev nil) (revs nil)
        (known (make-hash-table :test 'equal)))
    (while args
      (let* ((a (pop args)) (v nil))
        (cond ((string= a "--tags") (setq tags t))
              ((string= a "--always") (setq always t))
              ((string= a "--long") (setq long t))
              ((string= a "--dirty") (setq dirty "-dirty"))
              ((string-prefix-p "--dirty=" a) (setq dirty (substring a 8)))
              ((setq v (ngit--log-opt-value a "--abbrev" args))
               (setq abbrev (max 0 (min 40 (string-to-number (car v)))) args (cdr v)))
              ((string-prefix-p "-" a) (ngit--die "unsupported describe option %s" a))
              (t (push a revs)))))
    (when (and long (eq abbrev 0)) (ngit--die "fatal: --long is incompatible with --abbrev=0"))
    (when (and dirty revs) (ngit--die "fatal: option '--dirty' and commit-ishes cannot be used together"))
    (dolist (r (ngit-list-refs repo "refs/tags/"))
      (let* ((tag (ngit-inspect--tag repo r)) (hex (aref tag 0)) (old (and hex (gethash hex known))))
        (when (and hex (or (aref tag 2) tags)
                   (or (null old) (and (aref tag 2) (not (aref old 2)))
                       (and (aref tag 2) (aref old 2) (> (aref tag 3) (aref old 3)))))
          (puthash hex tag known))))
    (let ((suffix (if (and dirty (or (ngit-wt-changes repo)
                                      (ngit-diff-tree-index repo (ngit--tree-of repo "HEAD")))) dirty "")))
      (dolist (rev (if revs (nreverse revs) '("HEAD")))
        (let* ((hex (ngit-peel repo (ngit-rev-parse repo rev) 'commit))
               (start (ngit-commit hex)) (exact (gethash hex known))
               (q nil) (seen (make-hash-table :test 'equal)) (ordered nil) (candidates nil))
          (if exact
              (ngit--emit (aref exact 1)
                          (if long (concat "-0-g" (ngit-abbrev repo hex abbrev)) "") suffix "\n")
            ;; describe considers the first ten names in date-priority traversal.
            (ngit-parse-commit-object repo start)
            (setq q (and (> (hash-table-count known) 0) (list start))) (puthash hex t seen)
            (while q
              (let* ((c (pop q)) (tag (gethash (ngit--c-hex c) known)))
                (push c ordered)
                (when (and tag (< (length candidates) 10)) (push tag candidates))
                (dolist (p (ngit--c-orig-parents c))
                  (unless (gethash (ngit--c-hex p) seen)
                    (puthash (ngit--c-hex p) t seen)
                    (ngit-parse-commit-object repo p)
                    (setq q (ngit--insert-by-date p q))))))
            (let ((best nil) (depth most-positive-fixnum))
              (dolist (tag (nreverse candidates))
                (let ((reachable (ngit-inspect--reachable repo (ngit-commit (aref tag 0)))) (n 0))
                  (dolist (c ordered) (unless (gethash (ngit--c-hex c) reachable) (setq n (1+ n))))
                  (when (< n depth) (setq best tag depth n))))
              (cond (best (ngit--emit (aref best 1)
                                     (if (eq abbrev 0) ""
                                       (concat "-" (number-to-string depth) "-g" (ngit-abbrev repo hex abbrev)))
                                     suffix "\n"))
                    (always (ngit--emit (ngit-abbrev repo hex (and abbrev (max 4 abbrev))) suffix "\n"))
                    (t (ngit--die "fatal: No tags can describe '%s'." hex))))))))
    0))

(defun ngit-inspect--name-better (new old)
  (or (null old)
      (if (and (aref new 4) (aref old 4))
          (< (+ (aref new 2) (if (> (aref new 1) 0) 65535 0))
             (+ (aref old 2) (if (> (aref old 1) 0) 65535 0)))
        (if (not (eq (aref new 4) (aref old 4))) (aref new 4)
          (let ((nd (+ (aref new 2) (if (> (aref new 1) 0) 65535 0)))
                (od (+ (aref old 2) (if (> (aref old 1) 0) 65535 0))))
            (or (< nd od) (and (= nd od) (< (aref new 3) (aref old 3)))))))))

(defun ngit-inspect--name-string (name)
  (let ((tip (aref name 0)) (gen (aref name 1)))
    (if (= gen 0) tip
      (concat (if (string-suffix-p "^0" tip) (substring tip 0 -2) tip)
              "~" (number-to-string gen)))))

(defun ngit-inspect--name-refs (repo)
  "Sort refs by tag preference and age before propagating their names."
  (let ((refs nil))
    (dolist (r (ngit-list-refs repo))
      (let* ((hex (nth 1 r)) (obj (ngit-read-object repo (ngit-unhex hex)))
             (date (if (eq (car obj) 'tag)
                       (nth 1 (ngit--split-ident
                               (or (ngit-header (ngit-parse-commit (cdr obj)) "tagger") "")))
                     (if (eq (car obj) 'commit)
                         (ngit--commit-date (ngit-parse-commit (cdr obj))) most-positive-fixnum))))
        (push (list r (and (string-prefix-p "refs/tags/" (car r)) t) date) refs)))
    (mapcar #'car
            (sort (nreverse refs)
                  (lambda (a b) (if (not (eq (nth 1 a) (nth 1 b))) (nth 1 a)
                                  (< (nth 2 a) (nth 2 b))))))))

(defun ngit-cmd-name-rev (repo args)
  (let ((name-only nil) (tags-only nil) (revs nil) (cutoff most-positive-fixnum)
        (names (make-hash-table :test 'equal)) (exact (make-hash-table :test 'equal)))
    (dolist (a args)
      (cond ((string= a "--name-only") (setq name-only t))
            ((string= a "--tags") (setq tags-only t))
            ((string-prefix-p "-" a) (ngit--die "unsupported name-rev option %s" a))
            (t (push a revs))))
    (dolist (rev revs)
      (let ((hex (condition-case nil (ngit-peel repo (ngit-rev-parse repo rev) 'commit) (error nil))))
        (when hex
          (let ((c (ngit-commit hex)))
            (ngit-parse-commit-object repo c) (setq cutoff (min cutoff (ngit--c-date c)))))))
    (setq cutoff (max 0 (- cutoff 86400)))
    (dolist (r (ngit-inspect--name-refs repo))
      (let* ((tagp (string-prefix-p "refs/tags/" (car r)))
             (hex (nth 1 r)) (obj (ngit-read-object repo (ngit-unhex hex)))
             (deref (eq (car obj) 'tag))
             (date (if deref (nth 1 (ngit--split-ident
                                     (or (ngit-header (ngit-parse-commit (cdr obj)) "tagger") "")))
                     (if (eq (car obj) 'commit)
                         (ngit--commit-date (ngit-parse-commit (cdr obj))) most-positive-fixnum)))
             (tip (cond ((and tagp tags-only name-only) (substring (car r) 10))
                        (tagp (substring (car r) 5))
                        ((string-prefix-p "refs/heads/" (car r)) (substring (car r) 11))
                        (t (substring (car r) 5)))))
        (when (or tagp (not tags-only))
          (puthash hex tip exact)
          (let ((commit (condition-case nil (ngit-peel repo hex 'commit) (error nil))))
            (when commit
              (let ((todo (list (cons (ngit-commit commit)
                                      (vector (concat tip (if deref "^0" "")) 0 0 date tagp)))))
                (while todo
                  (let* ((entry (pop todo)) (c (car entry)) (name (cdr entry))
                         (old (gethash (ngit--c-hex c) names)))
                    (ngit-parse-commit-object repo c)
                    (when (and (>= (ngit--c-date c) cutoff) (ngit-inspect--name-better name old))
                      (puthash (ngit--c-hex c) name names)
                      (ngit-parse-commit-object repo c)
                      (let ((i 0) (next nil))
                        (dolist (p (ngit--c-orig-parents c))
                          (setq i (1+ i))
                          (push (cons p
                                      (if (= i 1)
                                          (vector (aref name 0) (1+ (aref name 1))
                                                  (1+ (aref name 2)) date tagp)
                                        (vector (concat (let ((s (ngit-inspect--name-string name)))
                                                          (if (string-suffix-p "^0" s) (substring s 0 -2) s))
                                                        "^" (number-to-string i))
                                                0 (+ (aref name 2) 65535) date tagp))) next))
                        (setq todo (append (nreverse next) todo))))))))))))
    (dolist (rev (nreverse revs))
      (let* ((hex (ngit-rev-parse repo rev)) (n (gethash hex names)))
        (unless name-only (ngit--emit rev " "))
        (ngit--emit (if n (ngit-inspect--name-string n) (or (gethash hex exact) "undefined")) "\n")))
    0))

(provide 'ngit-describe)
;;; ngit-describe.el ends here
