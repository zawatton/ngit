;;; ngit-main.el --- command-line entry point for ngit -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage: nelisp --load ngit-main.el -- COMMAND ARGS...
;; Exits with the command's status via `kill-emacs'.

;;; Code:

(defconst ngit-main--lazy-modules
  '(("log" "ngit-rename" "ngit-log")
    ("show" "ngit-rename" "ngit-log" "ngit-show")
    ("branch" "ngit-rename" "ngit-log" "ngit-branch")
    ("reset" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset")
    ("switch" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch")
    ("checkout" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch")
    ("rm" "ngit-rename" "ngit-log" "ngit-checkout" "ngit-rm")
    ("mv" "ngit-rename" "ngit-log" "ngit-checkout" "ngit-rm")
    ("restore" "ngit-rename" "ngit-log" "ngit-checkout" "ngit-restore")
    ("stash" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-stash")
    ("tag" "ngit-rename" "ngit-log" "ngit-branch" "ngit-tag"))
  "Commands whose large modules load only when an argument names them.
A false hit (a path argument spelled like a command) only costs time.")

(let ((dir (file-name-directory (or load-file-name buffer-file-name)))
      (args (cdr (member "--" command-line-args)))
      (files (list "ngit-native" "ngit-store" "ngit-rev" "ngit-cli" "ngit-config"
                   "ngit-match" "ngit-index" "ngit-worktree" "ngit-changes"
                   "ngit-status" "ngit-xdiff" "ngit-diff" "ngit-write" "ngit-add"
                   "ngit-commit")))
  (dolist (entry ngit-main--lazy-modules)
    (when (or (member (car entry) args) (member (concat "@" (car entry)) args))
      (dolist (f (cdr entry))
        (unless (member f files) (setq files (append files (list f)))))))
  (dolist (f files)
    (unless (or (featurep (intern f))
                (not (file-exists-p (expand-file-name (concat f ".el") dir))))
      (load (expand-file-name (concat f ".el") dir) nil t)))
  (when (equal (getenv "NGIT_PROFILE") "1")
    (load (expand-file-name "ngit-profile.el" dir) nil t)))

(let ((args (cdr (member "--" command-line-args))))
  ;; bin/ngit prefixes each argument with "@" (see there).
  (when (equal (car args) "--@")
    (setq args (mapcar (lambda (a) (substring a 1)) (cdr args))))
  (kill-emacs (ngit-main args)))

;;; ngit-main.el ends here
