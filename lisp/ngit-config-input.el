;;; ngit-config-input.el --- config standard input -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded only for a selected config source of "-".

;;; Code:

(defun ngit-config-input--lines (lines)
  "Restart line numbering for each parser opened on the remaining stream."
  (let ((offset (if lines (1- (caar lines)) 0)))
    (mapcar (lambda (line) (cons (- (car line) offset) (cdr line))) lines)))

(defun ngit-config-input--read ()
  "Read config text from standard input in bounded chunks."
  (let* ((owner (ngit-native--buffer 0 65536))
         (address (nl-ffi-memory-address owner)) parts done)
    (while (not done)
      (let ((size (syscall-direct 0 0 address 65536 0 0 0)))
        (cond ((< size 0) (ngit--die "fatal: unable to read standard input"))
              ((= size 0) (setq done t))
              (t (push (ptr-read-bytes address size) parts)))))
    (decode-coding-string (apply #'concat (nreverse parts)) 'utf-8)))

(provide 'ngit-config-input)
;;; ngit-config-input.el ends here
