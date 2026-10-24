;;; ngit-transport-checkout.el --- parallel initial clone checkout -*- lexical-binding: t; -*-

;;; Commentary:

;; A clone starts with an empty index and work tree.  Its entries have no
;; merge conflicts or local files to protect, so independent contiguous runs
;; can be written by forked workers.  The parent alone constructs and commits
;; the index after collecting each entry's exact 40-byte stat block.

;;; Code:

(require 'ngit-reset)
(require 'ngit-transport-memory
         (expand-file-name "ngit-transport-memory.el"
                           (file-name-directory
                            (or (and (boundp 'byte-compile-current-file)
                                     byte-compile-current-file)
                                load-file-name buffer-file-name))))

(defvar ngit-transport-checkout-threshold 4096
  "Minimum initial-checkout entry count for parallel workers.
Tests may bind this to 1 to force the parallel path.")

(defvar ngit-transport-checkout-workers 8)

(defconst ngit-transport-checkout--sys-fork 57)
(defconst ngit-transport-checkout--sys-exit 60)
(defconst ngit-transport-checkout--sys-wait4 61)
(defconst ngit-transport-checkout--sys-kill 62)

(defun ngit-transport-checkout--single-thread-p ()
  (= (length (directory-files
              (concat "/proc/" (number-to-string (emacs-pid)) "/task")
              nil "\\`[0-9]")) 1))

(defun ngit-transport-checkout--empty-worktree-p (repo)
  (let ((files (directory-files (ngit-repo-toplevel repo) nil nil t)) found)
    (while (and files (not found))
      (unless (member (car files) '("." ".." ".git")) (setq found t))
      (setq files (cdr files)))
    (not found)))

(defun ngit-transport-checkout--mkdir (path &optional parents)
  "Create PATH safely when another checkout worker creates it first."
  (condition-case err
      (make-directory path parents)
    (file-already-exists
     (unless (file-directory-p path) (signal (car err) (cdr err))))))

(defun ngit-transport-checkout--child (repo entries begin end file)
  (let ((exit-code 1))
    (unwind-protect
        (condition-case err
            (let ((i begin) blocks)
              ;; Adjacent sorted runs can share a leading directory at their
              ;; boundary.  Treat a concurrent successful mkdir as success.
              (fset 'ngit-make-directory #'ngit-transport-checkout--mkdir)
              (while (< i end)
                (let ((fresh (ngit-checkout-entry repo (aref entries i))))
                  (push (aref fresh 6) blocks))
                (setq i (1+ i)))
              (ngit-write-file file (apply #'concat (nreverse blocks)))
              (setq exit-code 0))
          (error
           (ngit-native-write-stderr
            (concat (error-message-string err) "\n"))))
      (syscall-direct ngit-transport-checkout--sys-exit exit-code 0 0 0 0 0))))

(defun ngit-transport-checkout--parallel (repo index entries lock)
  "Write ENTRIES in workers, then commit INDEX through LOCK."
  (let* ((count (length entries))
         (workers (min ngit-transport-checkout-workers count))
         (width (/ (+ count workers -1) workers))
         (status-owner (nl-ffi-memory-allocate 8))
         (status-address (nl-ffi-memory-address status-owner))
         children success)
    (unwind-protect
        (progn
          (let ((begin 0))
            (while (< begin count)
              (let* ((end (min count (+ begin width)))
                     (file (make-temp-file "ngit-checkout-stat-"))
                     (pid (syscall-direct ngit-transport-checkout--sys-fork
                                          0 0 0 0 0 0)))
                (when (< pid 0)
                  (delete-file file)
                  (error "fatal: cannot fork checkout worker"))
                (when (= pid 0)
                  (ngit-transport-checkout--child repo entries begin end file))
                (push (vector pid file begin end nil nil) children)
                (setq begin end))))
          ;; Reap every child before interpreting any result.  A failed child
          ;; must not leave its siblings modifying the work tree after return.
          (dolist (child children)
            (let ((waited -4))
              (while (= waited -4)
                (setq waited
                      (syscall-direct ngit-transport-checkout--sys-wait4
                                      (aref child 0) status-address 0 0 0 0)))
              (unless (= waited (aref child 0))
                (error "fatal: cannot wait for initial checkout worker"))
              (aset child 4 t)
              (aset child 5 (= (ptr-read-u32 status-address 0) 0))))
          (dolist (child children)
            (unless (aref child 5)
              (error "fatal: initial checkout worker failed")))
          (dolist (child children)
            (let* ((begin (aref child 2)) (end (aref child 3))
                   (expected (* 40 (- end begin)))
                   (bytes (ngit--read-file (aref child 1)))
                   (i begin) (at 0))
              (unless (= (length bytes) expected)
                (error "fatal: truncated initial checkout worker result"))
              (while (< i end)
                (aset (aref entries i) 6 (substring bytes at (+ at 40)))
                (setq i (1+ i) at (+ at 40)))))
          (let ((root (ngit-ct-new))
                (checked (make-hash-table :test 'eql)))
            (ngit-ct-update repo root entries)
            (aset index 8 nil)
            (aset index 9 nil)
            (ngit-commit-lock lock
                              (ngit-index-bytes repo index entries root checked))
            (aset repo 6 nil))
          (setq success t))
      (dolist (child children)
        (unless (aref child 4)
          (syscall-direct ngit-transport-checkout--sys-kill
                          (aref child 0) 15 0 0 0 0)))
      (dolist (child children)
        (unless (aref child 4)
          (let ((waited -4))
            (while (= waited -4)
              (setq waited
                    (syscall-direct ngit-transport-checkout--sys-wait4
                                    (aref child 0) status-address 0 0 0 0))))))
      (dolist (child children)
        (when (file-exists-p (aref child 1)) (delete-file (aref child 1))))
      (nl-ffi-memory-release status-owner)
      (unless success (ngit-unlock lock)))))

(defun ngit-transport-checkout (repo tree)
  "Check out TREE into a freshly initialized REPO.
Use the ordinary hard-reset path unless the empty initial checkout is large
enough and the runtime is safe to fork."
  (let* ((index (ngit-read-index repo))
         (ngit-transport-checkout-workers
          (ngit-transport-memory-workers ngit-transport-checkout-workers))
         (old (aref index 0))
         (changes (ngit-diff-tree-index repo tree))
         (count (length changes)))
    (if (or (< count ngit-transport-checkout-threshold)
            (not (ngit-transport-checkout--single-thread-p))
            (> (length old) 0)
            (not (ngit-transport-checkout--empty-worktree-p repo)))
        (ngit--reset-hard repo tree "HEAD")
      (let (targets invalid)
        (dolist (change changes)
          (if (= (aref change 2) 0)
              (setq invalid t)
            (push (ngit-make-entry
                   (aref change 0) (aref change 3) (aref change 2)
                   (concat (make-string 24 0)
                           (ngit--be32-bytes (aref change 2))
                           (make-string 12 0)))
                  targets)))
        (if invalid
            (ngit--reset-hard repo tree "HEAD")
          ;; The empty-index diff walks the canonical tree depth first, which
          ;; is already index path order.  TARGETS was built by pushing.
          (let* ((entries (vconcat (nreverse targets)))
                 (ngit--checkout-attr-direction 'index-first)
                 (ngit--checkout-attr-cache (make-hash-table :test 'equal))
                 (ngit--checkout-leading-cache (make-hash-table :test 'equal)))
            (aset index 0 entries)
            (aset repo 7 nil)
            (ngit-wt-setup (ngit-repo-toplevel repo))
            (let ((lock (ngit-lock (ngit-index-file repo))))
              (ngit-transport-checkout--parallel repo index entries lock))))))))

(provide 'ngit-transport-checkout)

;;; ngit-transport-checkout.el ends here
