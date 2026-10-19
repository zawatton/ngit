;;; ngit-main.el --- command-line entry point for ngit -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage: nelisp --load ngit-main.el -- COMMAND ARGS...
;; Exits with the command's status via `kill-emacs'.

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name)))
      (args (cdr (member "--" command-line-args))))
  ;; The log and branch modules are large; load them only when an
  ;; argument names one of those commands (a false hit only costs time).
  (dolist (f (append '("ngit-native" "ngit-store" "ngit-rev" "ngit-cli" "ngit-config"
                       "ngit-match" "ngit-index" "ngit-worktree" "ngit-changes"
                       "ngit-status" "ngit-xdiff" "ngit-diff" "ngit-write" "ngit-add"
                       "ngit-commit")
                     (and (or (member "@log" args) (member "@branch" args)
                              (member "log" args) (member "branch" args))
                          '("ngit-rename" "ngit-log" "ngit-branch"))))
    (unless (or (featurep (intern f))
                (not (file-exists-p (expand-file-name (concat f ".el") dir))))
      (load (expand-file-name (concat f ".el") dir) nil t))))

(let ((args (cdr (member "--" command-line-args))))
  ;; bin/ngit prefixes each argument with "@" (see there).
  (when (equal (car args) "--@")
    (setq args (mapcar (lambda (a) (substring a 1)) (cdr args))))
  (kill-emacs (ngit-main args)))

;;; ngit-main.el ends here
