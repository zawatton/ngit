;;; ngit-diff-stat.el --- command diffstat rendering -*- lexical-binding: t; -*-

;;; Commentary:

;; History shares the ordinary stat layout.  Work-tree diff additionally
;; needs unstored content and unmerged placeholders that do not count as files.

;;; Code:

(require 'ngit-log)

(defun ngit-diff--stat-summary (entries adds dels)
  (let ((files 0))
    (dolist (e entries) (unless (eq (aref e 3) 'unmerged) (setq files (1+ files))))
    (if (= files 0) " 0 files changed\n" (ngit--stat-summary-line files adds dels))))

(defun ngit-diff--stat-entries (repo changes)
  "Build diffstat entries using unstored work-tree bytes when available."
  (let (out)
    (dolist (c changes)
      (if (ngit--unmerged-stages c)
          (push (vector c 0 0 'unmerged) out)
       (let* ((path (aref c 0)) (old-path (or (aref c 8) path))
             (old (ngit--diff-blob repo (aref c 2) (aref c 3)))
             (new (or (aref c 7) (ngit--diff-blob repo (aref c 4) (aref c 5))))
             (binary (or (ngit--binary-p repo old-path old) (ngit--binary-p repo path new)))
             (counts (unless binary (ngit--count-changed-lines old new)))
             (display (copy-sequence c)))
        (aset display 0 (ngit-diff--display path))
        (when (aref c 8) (aset display 8 (ngit-diff--display (aref c 8))))
        (push (vector display (if binary (length new) (car counts))
                      (if binary (length old) (cdr counts)) binary) out))))
    (nreverse out)))

(defun ngit-diff--emit-stat (entries)
  "git's show_stats for ENTRIES."
  (when entries
    (let ((max-change 0) (max-len 0) (bin-width 0) (number-width 0)
          (names (make-hash-table :test 'eq)))
      (dolist (e entries)
        (let ((name (ngit--stat-print-name (aref e 0))))
          (puthash e name names)
          (setq max-len (max max-len (ngit--display-width name)))
          (if (eq (aref e 3) 'unmerged)
              nil
           (if (aref e 3)
              (setq bin-width (max bin-width (+ 14 (ngit--decimal-width (aref e 1))
                                                (ngit--decimal-width (aref e 2))))
                    number-width 3)
            (setq max-change (max max-change (+ (aref e 1) (aref e 2))))))))
      (let* ((width (ngit--stat-width))
             (number-width (max number-width (ngit--decimal-width max-change)))
             (width (max width (+ 16 6 number-width)))
             (graph-width (if (> (+ max-change 4) bin-width) max-change (- bin-width 4)))
             (name-width max-len))
        (when (> (+ name-width number-width 6 graph-width) width)
          (when (> graph-width (- (/ (* width 3) 8) number-width 6))
            (setq graph-width (max 6 (- (/ (* width 3) 8) number-width 6))))
          (if (> name-width (- width number-width 6 graph-width))
              (setq name-width (- width number-width 6 graph-width))
            (setq graph-width (- width number-width 6 name-width))))
        (let ((adds 0) (dels 0))
          (dolist (e entries)
            (let* ((name (gethash e names))
                   (len name-width)
                   (prefix "")
                   (added (aref e 1)) (deleted (aref e 2))
                   (name-len (ngit--display-width name)))
              (when (< name-width name-len)
                (setq prefix "..." len (- len 3))
                ;; As show_stats: skip NAME-LEN - LEN bytes, then cut at a slash.
                (setq name (substring name (min (length name) (max 0 (- name-len len)))))
                (let ((slash (ngit--byte-pos name ?/ 0)))
                  (when slash (setq name (substring name slash)))))
              (let ((padding (make-string (max 0 (- len (ngit--display-width name))) ?\s)))
                (if (eq (aref e 3) 'unmerged)
                    (ngit--emit " " prefix name padding " | Unmerged\n")
                 (if (aref e 3)
                    (ngit--emit " " prefix name padding " | "
                                (format (format "%%%ds" number-width) "Bin")
                                (if (and (= added 0) (= deleted 0))
                                    "\n"
                                  (format " %d -> %d bytes\n" deleted added)))
                  (let ((add added) (del deleted))
                    (when (<= graph-width max-change)
                      (let ((total (ngit--scale-linear (+ add del) graph-width max-change)))
                        (when (and (< total 2) (> add 0) (> del 0)) (setq total 2))
                        (if (< add del)
                            (setq add (ngit--scale-linear add graph-width max-change)
                                  del (- total add))
                          (setq del (ngit--scale-linear del graph-width max-change)
                                add (- total del)))))
                    (ngit--emit " " prefix name padding " | "
                                (format (format "%%%dd" number-width) (+ added deleted))
                                (if (> (+ added deleted) 0) " " "")
                                (make-string (max 0 add) ?+) (make-string (max 0 del) ?-) "\n")))))
              (unless (aref e 3)
                (setq adds (+ adds added) dels (+ dels deleted)))))
          (ngit--emit (ngit-diff--stat-summary entries adds dels)))))))

(defun ngit-diff--emit-stats (repo changes formats)
  "Emit requested FORMATS in Git's numstat, stat, shortstat order."
  (let ((entries (ngit-diff--stat-entries repo changes)))
    (when (memq 'numstat formats)
      (dolist (e entries)
        (ngit--emit (cond ((eq (aref e 3) 'unmerged) "0\t0\t")
                         ((aref e 3) "-\t-\t")
                         (t (format "%d\t%d\t" (aref e 1) (aref e 2))))
                    (ngit--stat-print-name (aref e 0)) "\n")))
    (when (memq 'stat formats) (ngit-diff--emit-stat entries))
    (when (and (memq 'shortstat formats) entries)
      (let ((adds 0) (dels 0))
        (dolist (e entries)
          (unless (aref e 3)
            (setq adds (+ adds (aref e 1)) dels (+ dels (aref e 2)))))
        (ngit--emit (ngit-diff--stat-summary entries adds dels))))))

(provide 'ngit-diff-stat)
;;; ngit-diff-stat.el ends here
