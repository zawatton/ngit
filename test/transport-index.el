;;; transport-index.el --- isolated received-pack index probe -*- lexical-binding: t; -*-

;; NGIT_TRANSPORT_PACK and NGIT_TRANSPORT_IDX select disposable test files.
;; Keep the command entry point out of this probe so it can test the received
;; pack indexer independently of protocol negotiation and work-tree checkout.

(let ((directory (expand-file-name "../lisp/" (file-name-directory load-file-name))))
  (dolist (module '("ngit-native" "ngit-store" "ngit-rev" "ngit-cli"
                    "ngit-config" "ngit-match" "ngit-index" "ngit-worktree"
                    "ngit-changes" "ngit-status" "ngit-xdiff" "ngit-diff"
                    "ngit-write" "ngit-add" "ngit-commit" "ngit-objects"
                    "ngit-pack" "ngit-transport-kernel" "ngit-transport-delta" "ngit-transport-index"))
    (load (expand-file-name (concat module ".el") directory) nil t)))

(condition-case error
    (let* ((ngit-transport-index--workers
            (if (getenv "NGIT_TRANSPORT_WORKERS")
                (string-to-number (getenv "NGIT_TRANSPORT_WORKERS"))
              ngit-transport-index--workers))
           (ngit-transport-index--parallel-threshold
            (if (getenv "NGIT_TRANSPORT_WORKERS") 1
              ngit-transport-index--parallel-threshold))
           (ngit-transport-index--native-threshold
            (if (getenv "NGIT_TRANSPORT_NATIVE") 1
              ngit-transport-index--native-threshold))
           (ngit-transport-index--cache-count
            (if (getenv "NGIT_TRANSPORT_CACHE_COUNT")
                (string-to-number (getenv "NGIT_TRANSPORT_CACHE_COUNT"))
              ngit-transport-index--cache-count))
           (ngit-transport-delta--io-byte-limit
            (if (getenv "NGIT_TRANSPORT_IO_LIMIT")
                (string-to-number (getenv "NGIT_TRANSPORT_IO_LIMIT"))
              ngit-transport-delta--io-byte-limit))
           (start (float-time))
           (result (ngit-transport-index-pack (getenv "NGIT_TRANSPORT_PACK")
                                               (getenv "NGIT_TRANSPORT_IDX"))))
      (princ (format "%s %.3fs\n" (car result) (- (float-time) start)))
      (kill-emacs 0))
  (error (ngit-native-write-stderr (concat (error-message-string error) "\n"))
         (kill-emacs 1)))
