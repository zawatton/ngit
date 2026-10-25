;;; ngit-command-paths.el --- write-command pathspec adapters -*- lexical-binding: t; -*-

;;; Commentary:

;; Keep literal index ranges fast while sharing invocation and magic parsing.

;;; Code:

(require 'ngit-paths)

(defun ngit-command-path-items (repo paths &optional scoped)
  "Compile PATHS into the range representation used by write commands."
  (let* ((items (ngit-pathspec-items repo paths))
         (originals (copy-sequence paths))
         (shared (seq-some
                  (lambda (item)
                    (and (vectorp item)
                         (or (aref item 2) (aref item 3) (aref item 4) (aref item 8))))
                  items)))
    (when (> (length items) (length paths))
      ;; Keep the implicit positive last to preserve argument indices.
      (setq items (append (cdr items) (list (if scoped (ngit-paths-prefix repo) "")))))
    (mapcar
     (lambda (item)
       (let* ((arg (pop originals))
              ;; Recompile ordinary arguments from their original spelling,
              ;; so ../ retains only the prefix that survives normalization.
              (compiled (if (vectorp item) item
                          (and arg (ngit-pathspec-compile repo (concat ":()" arg)))))
              (path (if compiled (aref compiled 0) item))
              (prefix (if compiled (aref compiled 7) 0))
              (wild (and compiled (not (aref compiled 1))
                         (ngit--glob-special-p path prefix))))
         (cond
          ((or shared
               (and compiled
                    (or (aref compiled 2) (aref compiled 3)
                        (aref compiled 4) (aref compiled 8)
                        (and wild (ngit--glob-special-p (substring path 0 prefix) 0)))))
           (if compiled (cons compiled 'magic) (cons path nil)))
          ((not wild) (cons path nil))
          (t (ngit--ps-item path)))))
     items)))

(defun ngit-command-path-origin (repo arg)
  "Return ARG with Git's prefix-origin magic for stash diagnostics."
  (let* ((item (or (ngit-pathspec-compile repo arg)
                   (ngit-pathspec-compile repo (concat ":()" arg))))
         (magic (or (aref item 1) (aref item 2) (aref item 3)
                    (aref item 4) (aref item 8)
                    (string-prefix-p ":/" arg)
                    (and (string-prefix-p ":(" arg)
                         (member "top" (split-string (substring arg 2 (string-search ")" arg)) ",")))))
         (head (cond
                ((not magic) ":(")
                ((string-prefix-p ":(" arg) (substring arg 0 (string-search ")" arg)))
                (t (concat ":(" (if (string-prefix-p ":/" arg) "top" "exclude"))))))
    (concat head (if magic "," "")
            (format "prefix:%d)" (aref item 7)) (aref item 0))))

(defun ngit-command-path-class (items path dirp)
  "Classify PATH using literal ranges or the shared magic matcher."
  (if (seq-some (lambda (item) (eq (cdr item) 'magic)) items)
      (ngit-pathspec-class (mapcar #'car items) path dirp)
    (let ((ngit--rv-paths items)) (ngit--ps-class path dirp))))

(provide 'ngit-command-paths)
;;; ngit-command-paths.el ends here
