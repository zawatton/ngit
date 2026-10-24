;;; takeover-ref.el --- Checked reference write regression -*- lexical-binding: t; -*-
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(require 'ngit-native)
(require 'ngit-cli)
(require 'ngit-write)
(require 'ngit-commit)
(require 'ngit-branch)
(condition-case err
    (let ((repo (ngit-discover (getenv "NGIT_AUDIT_REPO")))
          (old (getenv "NGIT_AUDIT_OLD")) (new (getenv "NGIT_AUDIT_NEW")))
      (cond
       ((equal (getenv "NGIT_AUDIT_CASE") "controls")
        (ngit--write-ref repo "refs/heads/main" new old nil t)
        (ngit--write-ref repo "refs/heads/audit" new nil nil)
        (ngit--write-ref repo "refs/heads/audit" new new nil)
        (ngit-update-head repo new nil (ngit--branch-ident repo) "audit unchecked"))
       (t (ngit--write-ref repo "refs/heads/main"
                           (if (equal (getenv "NGIT_AUDIT_CASE") "noop") new old)
                           old "audit stale ref")))
      (kill-emacs 0))
  (error (princ (format "%s\n" (error-message-string err))) (kill-emacs 1)))
