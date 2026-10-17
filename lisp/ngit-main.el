;;; ngit-main.el --- command-line entry point for ngit -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage: nelisp --load ngit-main.el -- COMMAND ARGS...
;; Exits with the command's status via `kill-emacs'.

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (f '("ngit-native" "ngit-store" "ngit-rev" "ngit-cli"))
    (unless (featurep (intern f))
      (load (expand-file-name (concat f ".el") dir) nil t))))

(kill-emacs (ngit-main (cdr (member "--" command-line-args))))

;;; ngit-main.el ends here
