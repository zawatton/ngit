;;; ngit-mailmap.el --- canonical identities for log and shortlog -*- lexical-binding: t; -*-

;;; Code:

(defvar ngit--mailmap-cache nil)

(defun ngit-mailmap--read (text table)
  "Merge Git mailmap TEXT into TABLE; specific name entries beat defaults."
  (dolist (line (split-string text "\n"))
    (unless (string-prefix-p "#" line)
      (when (string-match "\\`[ \t]*\\([^<]*\\)<\\([^>]+\\)>\\(.*\\)" line)
        (let* ((name (string-trim (match-string 1 line)))
               (mail (match-string 2 line))
               (rest (match-string 3 line))
               (old-name nil) (old-mail mail) (new-mail nil))
          (if (string-match "\\`[ \t]*\\([^<]*\\)<\\([^>]+\\)>" rest)
              (setq old-name (string-trim (match-string 1 rest))
                    old-mail (match-string 2 rest) new-mail mail)
            (setq new-mail nil))
          (when (or new-mail (not (string= name "")))
            (let* ((key (cons (downcase old-mail)
                              (and old-name (not (string= old-name "")) (downcase old-name))))
                   (old (gethash key table)))
              (puthash key (cons (if (string= name "") (car old) name)
                                 (or new-mail (cdr old))) table))))))))

(defun ngit-mailmap-load (repo)
  "Load .mailmap, mailmap.blob, then mailmap.file in Git's precedence order."
  (or ngit--mailmap-cache
      (let* ((table (make-hash-table :test 'equal))
             (root (ngit-repo-toplevel repo))
             (file (and root (expand-file-name ".mailmap" root)))
             (blob (or (ngit-config-get repo "mailmap.blob") (and (not root) "HEAD:.mailmap")))
             (extra (ngit-config-get repo "mailmap.file")))
        ;; Git never follows a worktree .mailmap symlink.
        (when (and file (file-regular-p file) (not (file-symlink-p file)))
          (ngit-mailmap--read (ngit--read-file file) table))
        (when blob
          (let ((hex (condition-case nil (ngit-rev-parse repo blob) (error nil))))
            (when hex
              (let ((obj (ngit-read-object repo (ngit-unhex hex))))
                (if (eq (car obj) 'blob) (ngit-mailmap--read (cdr obj) table)
                  (ngit-native-write-stderr (format "error: mailmap is not a blob: %s\n" blob)))))))
        (when extra
          (let ((path (expand-file-name (if (string-prefix-p "~/" extra)
                                           (expand-file-name extra) extra)
                                       (or root default-directory))))
            (when (file-regular-p path) (ngit-mailmap--read (ngit--read-file path) table))))
        (setq ngit--mailmap-cache table))))

(defun ngit-mailmap-person (repo name mail)
  "Canonical (NAME . MAIL) for an identity; match names and emails case insensitively."
  (let* ((table (ngit-mailmap-load repo))
         (entry (or (gethash (cons (downcase mail) (downcase name)) table)
                    (gethash (cons (downcase mail) nil) table))))
    (cons (or (car entry) name) (or (cdr entry) mail))))

(defun ngit-mailmap-ident (repo ident)
  "Apply REPO's mailmap while preserving IDENT's timestamp and zone."
  (if (not (string-match "\\`\\(.*?\\) *<\\([^>]*\\)>\\(.*\\)\\'" ident)) ident
    (let* ((name (match-string 1 ident)) (mail (match-string 2 ident))
           (tail (match-string 3 ident)) (person (ngit-mailmap-person repo name mail)))
      (concat (car person) " <" (cdr person) ">" tail))))

(defun ngit-mailmap-shortlog (repo args)
  "Run shortlog with mapped identities, scoped to its grouping accessor."
  (let ((original (symbol-function 'ngit-inspect--ident))
        (ngit--mailmap-cache nil))
    (unwind-protect
        (progn
          (fset 'ngit-inspect--ident
                (lambda (ident) (funcall original (ngit-mailmap-ident repo ident))))
          (ngit-cmd-shortlog repo args))
      (fset 'ngit-inspect--ident original))))

(provide 'ngit-mailmap)
;;; ngit-mailmap.el ends here
