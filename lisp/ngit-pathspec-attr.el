;;; ngit-pathspec-attr.el --- attribute pathspec requirements -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded only for attr magic.  Keep Git's four attribute states distinct;
;; attributes come from the work tree, with index fallback for missing files,
;; even when selecting tree paths.

;;; Code:

(defun ngit-pathspec-attr-value (value)
  "Unescape VALUE and validate Git's pathspec attribute value alphabet."
  (let ((start 0) pieces)
    ;; Scan only escapes and invalid characters, preserving Git's first
    ;; diagnostic when a bad character precedes a terminal escape.
    (while (string-match "[^A-Za-z0-9,_-]" value start)
      (let* ((pos (match-beginning 0)) (next (1+ pos))
             (char (substring value pos next)))
        (push (substring value start pos) pieces)
        (when (string= char "\\")
          (when (= next (length value))
            (ngit--die "fatal: Escape character '\\' not allowed as last character in attr value"))
          (setq char (substring value next (1+ next)) next (1+ next)))
        (unless (string-match-p "\\`[A-Za-z0-9,_-]\\'" char)
          ;; Git reports the offending byte, including a lone UTF-8 lead
          ;; byte.  Preserve it as a raw character through stderr encoding.
          (ngit--die "fatal: cannot use '%s' for value matching"
                     (decode-coding-string
                      (substring (encode-coding-string char 'utf-8) 0 1) 'utf-8)))
        (push char pieces)
        (setq start next)))
    (push (substring value start) pieces)
    (apply #'concat (nreverse pieces))))

(defun ngit-pathspec-attr-compile (repo spec)
  "Compile space-separated SPEC into a repository-bound attribute selector."
  (when (string= spec "") (ngit--die "fatal: attr spec must not be empty"))
  (let (requirements names)
    (dolist (field (split-string spec " " t))
      (let ((name field) (state 'set) equal)
        (cond ((string-prefix-p "!" field)
               (setq name (substring field 1) state 'unspecified))
              ((string-prefix-p "-" field)
               (setq name (substring field 1) state 'unset))
              ((setq equal (string-search "=" field))
               (setq name (substring field 0 equal)
                     state (ngit-pathspec-attr-value (substring field (1+ equal))))))
        (unless (and (not (string-prefix-p "-" name))
                     (string-match-p "\\`[A-Za-z0-9_.-]+\\'" name))
          (ngit--die "fatal: invalid attribute name %s" name))
        (push (cons name state) requirements)
        (push name names)))
    (vector repo (nreverse requirements) (delete-dups (nreverse names))
            (make-hash-table :test 'equal))))

(defun ngit-pathspec-attr-match-p (compiled path)
  "Whether PATH satisfies all requirements in COMPILED."
  (let* ((cache (aref compiled 3)) (cached (gethash path cache 'missing)))
    (if (not (eq cached 'missing)) cached
      (let ((states (ngit-attrs (aref compiled 0) path (aref compiled 2))))
        (puthash path
                 (seq-every-p (lambda (requirement)
                                (equal (cdr requirement)
                                       (cdr (assoc (car requirement) states))))
                              (aref compiled 1)) cache)))))

(provide 'ngit-pathspec-attr)
;;; ngit-pathspec-attr.el ends here
