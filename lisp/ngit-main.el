;;; ngit-main.el --- command-line entry point for ngit -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage: nelisp --load ngit-main.el -- COMMAND ARGS...
;; Exits with the command's status via `kill-emacs'.

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (f '("ngit-native" "ngit-store" "ngit-rev" "ngit-cli" "ngit-config"
               "ngit-match" "ngit-index" "ngit-worktree" "ngit-changes"
               "ngit-status" "ngit-xdiff" "ngit-diff"))
    (unless (or (featurep (intern f))
                (not (file-exists-p (expand-file-name (concat f ".el") dir))))
      (load (expand-file-name (concat f ".el") dir) nil t))))

(kill-emacs (ngit-main (cdr (member "--" command-line-args))))

;;; ngit-main.el ends here
