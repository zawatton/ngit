;;; merge-driver.el --- Batched content merge comparison -*- lexical-binding: t; -*-

(let ((dir (file-name-directory load-file-name)))
  (load (expand-file-name "../lisp/ngit-xdiff.el" dir) nil t)
  (load (expand-file-name "../lisp/ngit-merge-file.el" dir) nil t))

(defun ngit-merge-driver--read (path)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(let* ((args (cdr (member "--" command-line-args)))
       (cases (read (ngit-merge-driver--read (car args))))
       (coding-system-for-write 'no-conversion))
  (condition-case err
      (dolist (item cases)
        (let* ((dir (nth 0 item))
               (result (ngit-merge-file
                        (ngit-merge-driver--read (concat dir "/base"))
                        (ngit-merge-driver--read (concat dir "/ours"))
                        (ngit-merge-driver--read (concat dir "/theirs"))
                        :labels '("base" "ours" "theirs")
                        :style (intern (nth 1 item))
                        :favor (intern (nth 2 item))
                        :marker-size (nth 3 item) :level (nth 4 item))))
          (write-region (car result) nil (concat dir "/actual") nil 'silent)
          (write-region (number-to-string (cdr result)) nil
                        (concat dir "/count") nil 'silent)))
    (error (princ (format "ERROR: %S\n" err)) (kill-emacs 2)))
  (kill-emacs 0))
