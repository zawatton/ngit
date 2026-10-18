;;; xdiff-driver.el --- Run ngit-xdiff-unified from the command line  -*- lexical-binding: t; -*-

;; Usage:
;;   nelisp --load test/xdiff-driver.el -- OLD NEW CONTEXT
;;     prints the hunks for one pair.
;;   nelisp --load test/xdiff-driver.el -- --list FILE
;;     FILE holds one "OLD NEW CONTEXT" triple per line; for each case the
;;     driver prints a separator line "@@@NGIT-XDIFF-CASE@@@" followed by
;;     the hunks.

(load (expand-file-name "../lisp/ngit-xdiff.el"
                        (file-name-directory load-file-name))
      nil t)

(defun ngit-xdiff-driver--read (f)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally f)
    (buffer-string)))

(defun ngit-xdiff-driver--run (a b ctx)
  (princ (ngit-xdiff-unified (ngit-xdiff-driver--read a)
                             (ngit-xdiff-driver--read b)
                             ctx)))

(let ((args (cdr (member "--" command-line-args))))
  (condition-case err
      (if (equal (car args) "--list")
          (dolist (line (split-string (ngit-xdiff-driver--read (nth 1 args))
                                      "\n" t))
            (let ((f (split-string line " " t)))
              (princ "@@@NGIT-XDIFF-CASE@@@\n")
              (ngit-xdiff-driver--run (nth 0 f) (nth 1 f)
                                      (string-to-number (nth 2 f)))))
        (ngit-xdiff-driver--run (nth 0 args) (nth 1 args)
                                (string-to-number (nth 2 args))))
    (error (princ (format "ERROR: %S\n" err))
           (kill-emacs 2)))
  (kill-emacs 0))
