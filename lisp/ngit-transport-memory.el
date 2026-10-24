;;; ngit-transport-memory.el --- transport memory planning -*- lexical-binding: t; -*-

;;; Commentary:
;; The launcher supervises aggregate process-tree PSS, including its overhead,
;; with an 8192 MiB default.  These conservative planning allowances reserve
;; space for the parent, delta buffers and checkout state.  They are heuristics,
;; not allocation limits: the supervisor remains the final guard.

;;; Code:

(defun ngit-transport-memory-budget ()
  "Return the validated aggregate transport budget in MiB."
  (let ((value (or (getenv "NGIT_MEMORY_BUDGET_MIB") "8192")))
    (unless (and (string-match-p "\\`[0-9]+\\'" value)
                 (> (string-to-number value) 0))
      (error "fatal: NGIT_MEMORY_BUDGET_MIB must be a positive integer"))
    (string-to-number value)))

(defun ngit-transport-memory-workers (requested)
  "Limit REQUESTED workers using a 768 MiB planning allowance per worker."
  (max 1 (min requested (/ (ngit-transport-memory-budget) 768))))

(defun ngit-transport-memory-cache-bytes (workers requested)
  "Reserve at most one quarter of the budget for all WORKERS and parent caches."
  (min requested (max 1 (/ (* (ngit-transport-memory-budget) 1048576)
                           (* 4 (1+ workers))))))

(provide 'ngit-transport-memory)
;;; ngit-transport-memory.el ends here
