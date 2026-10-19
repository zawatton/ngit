;;; ngit-rev.el --- refs and revision parsing -*- lexical-binding: t; -*-

;;; Commentary:

;; Loose refs, packed-refs, symbolic refs and a subset of gitrevisions(7):
;; full and abbreviated object names, "@", ref DWIM rules, and the suffixes
;; ^, ^N, ~N, ^{}, ^{TYPE} and :PATH.

;;; Code:

(require 'ngit-store)

(defconst ngit--ref-rules
  '("%s" "refs/%s" "refs/tags/%s" "refs/heads/%s" "refs/remotes/%s"
    "refs/remotes/%s/HEAD")
  "Ref lookup rules, in git's `ref_rev_parse_rules' order.")

(defun ngit--packed-refs (repo)
  "Return an alist (REFNAME . HEX) from REPO's packed-refs (cached)."
  (or (aref repo 4)
      (aset repo 4
            (let ((path (expand-file-name "packed-refs" (ngit-repo-commondir repo)))
                  (refs nil))
              (when (file-exists-p path)
                (dolist (line (split-string (ngit--read-file path) "\n" t))
                  (unless (or (string-prefix-p "#" line) (string-prefix-p "^" line))
                    (when (string-match "\\`\\([0-9a-f]\\{40\\}\\) \\(.+\\)\\'" line)
                      (push (cons (match-string 2 line) (match-string 1 line)) refs)))))
              (or (nreverse refs) (list nil))))))

(defun ngit--ref-file (repo name)
  "Return the loose file that would hold ref NAME."
  (let ((per-worktree (or (not (string-prefix-p "refs/" name))
                          (string-prefix-p "refs/bisect/" name)
                          (string-prefix-p "refs/worktree/" name))))
    (expand-file-name name (if per-worktree
                               (ngit-repo-gitdir repo)
                             (ngit-repo-commondir repo)))))

(defun ngit-read-ref (repo name)
  "Return the raw value of ref NAME: \"ref: X\" or a hex name, or nil."
  (let ((file (ngit--ref-file repo name)))
    (if (file-regular-p file)
        (ngit--trim-eol (ngit--read-file file))
      (cdr (assoc name (ngit--packed-refs repo))))))

(defun ngit-resolve-ref (repo name &optional depth)
  "Return the hex object name ref NAME points to, following symrefs."
  (let ((val (ngit-read-ref repo name))
        (depth (or depth 0)))
    (cond ((null val) nil)
          ((> depth 5) (error "ngit: symref loop at %s" name))
          ((string-prefix-p "ref: " val)
           (ngit-resolve-ref repo (substring val 5) (1+ depth)))
          ((ngit-hex-p val 40) (downcase val)))))

(defun ngit-dwim-ref (repo name)
  "Resolve NAME by the ref rules; return (REFNAME . HEX) or nil."
  (let ((rules ngit--ref-rules) (found nil))
    (while (and rules (not found))
      (let* ((full (format (car rules) name))
             (hex (and (or (string-prefix-p "refs/" full)
                           (let ((case-fold-search nil))
                             (string-match-p "\\`[A-Z_]+\\'" full)))
                       (ngit-resolve-ref repo full))))
        (when hex (setq found (cons full hex))))
      (setq rules (cdr rules)))
    found))

;;;; Ref listing

(defun ngit--loose-ref-names (dir prefix acc)
  "Push the names of loose refs below DIR (named PREFIX...) onto ACC."
  (dolist (f (directory-files dir nil nil t))
    (unless (member f '("." ".."))
      (let ((path (concat dir "/" f)))
        (if (file-directory-p path)
            (setq acc (ngit--loose-ref-names path (concat prefix f "/") acc))
          (unless (string-suffix-p ".lock" f)
            (push (concat prefix f) acc))))))
  acc)

(defun ngit-list-refs (repo &optional prefix)
  "Return the refs of REPO below PREFIX (default \"refs/\"), sorted by name.
Each element is (REFNAME HEX SYMREF-TARGET); loose refs override packed
ones and refs that do not resolve are left out."
  (let* ((prefix (or prefix "refs/"))
         (common (ngit-repo-commondir repo))
         (dir (directory-file-name (expand-file-name prefix common)))
         (names (make-hash-table :test 'equal))
         (all nil)
         (out nil))
    ;; `maphash' is slow in NeLisp; collect the distinct names as we go.
    (dolist (r (ngit--packed-refs repo))
      (when (and r (string-prefix-p prefix (car r)) (not (gethash (car r) names)))
        (puthash (car r) t names)
        (push (car r) all)))
    (when (file-directory-p dir)
      (dolist (n (ngit--loose-ref-names dir prefix nil))
        (unless (gethash n names)
          (puthash n t names)
          (push n all))))
    (progn
      (dolist (name (sort all #'string<))
        (let* ((raw (ngit-read-ref repo name))
               (target (and raw (string-prefix-p "ref: " raw) (substring raw 5)))
               (hex (and raw (ngit-resolve-ref repo name))))
          (when hex (push (list name hex target) out)))))
    (nreverse out)))

;;;; Peeling

(defun ngit--object-type (repo hex)
  (car (ngit-read-object repo (ngit-unhex hex))))

(defun ngit-peel (repo hex want)
  "Peel object HEX to type WANT (a symbol), or to a non-tag when WANT is nil."
  (let ((obj (ngit-read-object repo (ngit-unhex hex))))
    (cond
     ((and want (eq (car obj) want)) hex)
     ((eq (car obj) 'tag)
      (ngit-peel repo (ngit-header (ngit-parse-commit (cdr obj)) "object") want))
     ((null want) hex)
     ((and (eq want 'tree) (eq (car obj) 'commit))
      (ngit-header (ngit-parse-commit (cdr obj)) "tree"))
     (t (error "fatal: object %s is a %s, not a %s" hex (car obj) want)))))

(defun ngit-commit-parents (repo hex)
  (ngit-header-all (ngit-parse-commit (cdr (ngit-read-object repo (ngit-unhex hex))))
                   "parent"))

(defun ngit-tree-lookup (repo tree-hex path)
  "Return the hex name of PATH inside tree TREE-HEX, or nil."
  (let ((hex tree-hex))
    (dolist (part (split-string path "/" t))
      (when hex
        (let ((entry (assoc part (mapcar (lambda (e) (cons (nth 1 e) e))
                                         (ngit-parse-tree
                                          (cdr (ngit-read-object repo (ngit-unhex hex))))))))
          (setq hex (and entry (ngit-hex (nth 3 entry)))))))
    hex))

;;;; Revision parsing

(defun ngit--resolve-base (repo name)
  (cond
   ((member name '("@" "")) (ngit-resolve-ref repo "HEAD"))
   ((ngit-hex-p name 40) (downcase name))
   (t (or (cdr (ngit-dwim-ref repo name))
          (and (ngit-hex-p name) (>= (length name) 4)
               (let ((hits (ngit-find-prefix repo name)))
                 (cond ((null hits) nil)
                       ((cdr hits) (error "error: short object ID %s is ambiguous" name))
                       (t (car hits)))))))))

(defun ngit-rev-parse (repo rev)
  "Return the hex object name for revision REV, or signal an error."
  (let* ((colon (and (not (string-prefix-p ":" rev)) (string-search ":" rev)))
         (path (and colon (substring rev (1+ colon))))
         (spec (if colon (substring rev 0 colon) rev))
         (base-end (or (string-match "[~^]" spec) (length spec)))
         (hex (ngit--resolve-base repo (substring spec 0 base-end)))
         (pos base-end))
    (unless hex
      (error "fatal: ambiguous argument '%s': unknown revision or path not in the working tree." rev))
    (while (< pos (length spec))
      (cond
       ((and (string-match "\\^{\\([a-z]*\\)}" spec pos)
             (= (match-beginning 0) pos))
        (let ((type (match-string 1 spec)))
          (setq pos (match-end 0)
                hex (if (string= type "")
                        (ngit-peel repo hex nil)
                      (ngit-peel repo hex (intern type))))))
       ((string-match "\\([~^]\\)\\([0-9]*\\)" spec pos)
        (let* ((op (match-string 1 spec))
               (digits (match-string 2 spec))
               (n (if (string= digits "") 1 (string-to-number digits))))
          (unless (= (match-beginning 0) pos)
            (error "fatal: invalid revision syntax: %s" rev))
          (setq pos (match-end 0))
          (setq hex (ngit-peel repo hex 'commit))
          (if (string= op "~")
              (dotimes (_ n)
                (setq hex (or (car (ngit-commit-parents repo hex))
                              (error "fatal: ambiguous argument '%s': unknown revision or path not in the working tree." rev))))
            (unless (= n 0)
              (setq hex (or (nth (1- n) (ngit-commit-parents repo hex))
                            (error "fatal: ambiguous argument '%s': unknown revision or path not in the working tree." rev)))))))
       (t (error "fatal: invalid revision syntax: %s" rev))))
    (when path
      (let ((tree (ngit-peel repo hex 'tree)))
        (setq hex (if (string= path "")
                      tree
                    (or (ngit-tree-lookup repo tree path)
                        (error "fatal: path '%s' does not exist in '%s'" path spec))))))
    hex))

(provide 'ngit-rev)

;;; ngit-rev.el ends here
