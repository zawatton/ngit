;;; transport-memory-cache.el --- reject needless direct copies -*- lexical-binding: t; -*-
(let ((directory (expand-file-name "lisp/" (getenv "NGIT_MEMORY_CACHE_SOURCE"))))
  (dolist (module '("ngit-native" "ngit-store" "ngit-rev" "ngit-cli"
                    "ngit-config" "ngit-match" "ngit-index" "ngit-worktree"
                    "ngit-changes" "ngit-status" "ngit-xdiff" "ngit-diff"
                    "ngit-write" "ngit-add" "ngit-commit" "ngit-objects"
                    "ngit-pack" "ngit-transport-kernel" "ngit-transport-delta"
                    "ngit-transport-index"))
    (load (expand-file-name (concat module ".el") directory) nil t)))
(condition-case err
    (let ((original (symbol-function 'ngit-transport-index--native-copy))
          (ngit-transport-index--native-threshold 1)
          oversized)
      ;; The direct-only 2 MiB fixture cannot benefit from a copy retained in
      ;; a 1 MiB cache (8 MiB aggregate planning budget, one worker + parent).
      (fset 'ngit-transport-index--native-copy
            (lambda (address size)
              (when (> size 1048576) (setq oversized t))
              (funcall original address size)))
      (ngit-transport-index-pack (getenv "NGIT_TRANSPORT_PACK")
                                 (getenv "NGIT_TRANSPORT_IDX"))
      (when oversized (error "uncacheable direct native copy"))
      (princ "transport-memory direct cache: PASS\n")
      (kill-emacs 0))
  (error (ngit-native-write-stderr (concat (error-message-string err) "\n"))
         (kill-emacs 1)))
