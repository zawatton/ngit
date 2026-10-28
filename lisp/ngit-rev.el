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
        (let ((value (ngit--trim-eol (ngit--read-file file))))
          (if (and (equal name "FETCH_HEAD") (>= (length value) 40))
              (substring value 0 40) value))
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

;;;; Reference transactions

(defvar ngit-ref-inhibit-hooks nil
  "Non-nil for backend operations which do not use reference transactions.")

(defvar ngit-ref-hook-env nil
  "Additional environment exported by the current reference operation.")

(defvar ngit-ref-hook-cwd nil
  "Working directory for the current reference operation's hooks.")

(defun ngit-ref-prepare (repo updates &optional abort-first locks abort-inputs abort-updates abort-locks)
  "Prepare a reference transaction for UPDATES, returning its hook input.
Each update is (OLD NEW REF); nil object names mean the null object.
ABORT-FIRST represents the empty packed-refs transaction used by the
files backend when deleting an unpacked ref.  ABORT-INPUTS are already
prepared nested transactions to abort if this preparation fails.
ABORT-UPDATES overrides UPDATES for the empty packed transaction abort.
ABORT-LOCKS remain held until the nested transactions have been aborted.
Skip all serialization
when no executable reference-transaction hook exists."
  (when (and (not ngit-ref-inhibit-hooks)
             (let ((ngit-hook-working-directory ngit-ref-hook-cwd))
               (ngit-hook-exists-p repo "reference-transaction")))
    (let* ((ngit-hook-working-directory ngit-ref-hook-cwd)
           (zero (make-string 40 ?0))
           (input (mapconcat
                   (lambda (update)
                     (concat (or (nth 0 update) zero) " "
                             (or (nth 1 update) zero) " "
                             (nth 2 update) "\n"))
                   updates "")))
      (when abort-first
        (ngit-hook-run repo "reference-transaction" '("aborted")
                       (if (null abort-updates) input
                         (mapconcat (lambda (update)
                                      (concat (or (nth 0 update) zero) " "
                                              (or (nth 1 update) zero) " "
                                              (nth 2 update) "\n"))
                                    abort-updates ""))
                       ngit-ref-hook-env))
      (unless (= 0 (ngit-hook-run repo "reference-transaction" '("prepared") input ngit-ref-hook-env))
        (dolist (lock locks) (ngit-unlock lock))
        (dolist (prior abort-inputs) (ngit-ref-finish repo prior t))
        (dolist (lock abort-locks) (ngit-unlock lock))
        (ngit-hook-run repo "reference-transaction" '("aborted") input ngit-ref-hook-env)
        (ngit--die "fatal: ref updates aborted by hook"))
      input)))

(defun ngit-ref-finish (repo input &optional aborted)
  "Notify the reference hook that INPUT committed, or ABORTED."
  (when input
    (let ((ngit-hook-working-directory ngit-ref-hook-cwd))
      (ngit-hook-run repo "reference-transaction"
                     (list (if aborted "aborted" "committed")) input ngit-ref-hook-env))))

(defun ngit-ref-write (repo name value &optional old)
  "Write raw reference VALUE to NAME, including transaction notifications."
  (let* ((file (ngit--ref-file repo name))
         (new (if (string-prefix-p "ref: " value)
                  (concat "ref:" (substring value 5)) value))
         lock input)
    (ngit-make-directory (file-name-directory file) t)
    (setq lock (ngit-lock file))
    (unwind-protect
        (progn
          (setq input (ngit-ref-prepare repo (list (list old new name)) nil (list lock)))
          (ngit-commit-lock lock (concat value "\n"))
          (setq lock nil)
          (aset repo 4 nil)
          (ngit-ref-finish repo input))
      (when lock (ngit-unlock lock)))))

(defun ngit-ref-delete-state (repo name)
  "Delete an unpacked state reference NAME, including when it is absent."
  (let* ((file (ngit--ref-file repo name))
         (lock (ngit-lock file)) packed-lock input)
    (unwind-protect
        (progn
          (setq packed-lock (ngit-lock (expand-file-name "packed-refs" (ngit-repo-commondir repo))))
          (setq input (ngit-ref-prepare repo (list (list nil nil name)) t
                                        (list lock packed-lock)))
          (when (file-exists-p file) (ngit-delete-file file))
          (ngit-unlock lock)
          (ngit-unlock packed-lock)
          (ngit-ref-finish repo input))
      (ngit-unlock lock)
      (when packed-lock (ngit-unlock packed-lock)))))

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
