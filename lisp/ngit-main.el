;;; ngit-main.el --- command-line entry point for ngit -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage: nelisp --load ngit-main.el -- COMMAND ARGS...
;; Exits with the command's status via `kill-emacs'.

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (f '("ngit-native" "ngit-store" "ngit-rev" "ngit-cli" "ngit-config"
               "ngit-match" "ngit-index" "ngit-worktree" "ngit-changes"
               "ngit-status" "ngit-xdiff" "ngit-diff" "ngit-write" "ngit-add"
               "ngit-commit"))
    (unless (or (featurep (intern f))
                (not (file-exists-p (expand-file-name (concat f ".el") dir))))
      (load (expand-file-name (concat f ".el") dir) nil t))))

(let ((args (cdr (member "--" command-line-args))))
  ;; bin/ngit prefixes each argument with "@" (see there).
  (when (equal (car args) "--@")
    (setq args (mapcar (lambda (a) (substring a 1)) (cdr args))))
  (kill-emacs (ngit-main args)))

;;; ngit-main.el ends here
