;;; ngit-lines.el --- bounded native line scans -*- lexical-binding: t; -*-

;;; Commentary:
;; NeLisp's search primitives may convert the complete input on each call.
;; Scan small chunks so line splitting stays linear even for large sources.

;;; Code:

(defun ngit-lines (data)
  "Split DATA at LF, retaining empty fields, including the final field."
  (let ((offset 0) (size (length data)) (parts nil) (lines nil))
    (while (< offset size)
      (let* ((end (min size (+ offset 1024))) (chunk (substring data offset end))
             (start 0) (nl nil))
        (while (setq nl (string-search "\n" chunk start))
          (let ((last (substring chunk start nl)))
            (push (if parts (apply #'concat (nreverse (cons last parts))) last) lines))
          (setq parts nil start (1+ nl)))
        (when (< start (length chunk)) (push (substring chunk start) parts))
        (setq offset end)))
    (push (if parts (apply #'concat (nreverse parts)) "") lines)
    (nreverse lines)))

(provide 'ngit-lines)
;;; ngit-lines.el ends here
