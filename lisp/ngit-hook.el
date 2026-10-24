;;; ngit-hook.el --- shared Git hook runner -*- lexical-binding: t; -*-

;;; Commentary:

;; Hooks are discovered lazily by ngit-config.el.  This file is loaded only
;; when the requested hook exists and is executable.  Execution uses the raw
;; transport process substrate so stdout and stderr can share fd 2 without an
;; Emacs process filter changing bytes or output order.

;;; Code:

(require 'ngit-config)
(unless (featurep 'ngit-transport-process)
  (load (expand-file-name "ngit-transport-process.el" ngit-native--dir) nil t))

(defun ngit-hook--env-name (entry)
  (let ((equal (string-search "=" entry)))
    (if equal (substring entry 0 equal) entry)))

(defun ngit-hook--env-apply (base changes)
  "Apply CHANGES to complete environment BASE.
An entry `KEY=value' sets KEY and a bare `KEY' removes it."
  (dolist (change changes)
    (let ((name (ngit-hook--env-name change)) kept)
      (dolist (entry base)
        (unless (equal name (ngit-hook--env-name entry))
          (push entry kept)))
      (setq base (nreverse kept))
      (when (string-search "=" change)
        (setq base (append base (list change))))))
  base)

(defun ngit-hook--env-value (environment name)
  "Return NAME's value in complete ENVIRONMENT, or nil when absent."
  (let ((prefix (concat name "=")) value)
    (dolist (entry environment)
      (when (string-prefix-p prefix entry)
        (setq value (substring entry (length prefix)))))
    value))

(defun ngit-hook--prefix (repo)
  "Return GIT_PREFIX for REPO at the current invocation directory."
  (let ((worktree (ngit--hook-worktree repo)))
    (if (null worktree) ""
      (let ((cwd (file-name-as-directory (expand-file-name default-directory)))
            (worktree (file-name-as-directory (expand-file-name worktree))))
        (cond
         ((equal cwd worktree) "")
         ((string-prefix-p worktree cwd)
          (file-name-as-directory (substring cwd (length worktree))))
         (t ""))))))

(defun ngit-hook--repository-env (repo)
  "Return repository context changes Git exports for REPO's hooks."
  (let* ((top (ngit-repo-toplevel repo))
         (gitdir (directory-file-name (expand-file-name (ngit-repo-gitdir repo))))
         (explicit (getenv "GIT_DIR"))
         (invocation (file-name-as-directory (expand-file-name default-directory)))
         (hook-cwd (ngit--hook-cwd repo))
         (changes
          (list
           (cond
            ((and explicit (not (string= explicit "")))
             (concat "GIT_DIR="
                     (if (or (file-name-absolute-p explicit)
                             (equal invocation hook-cwd))
                         explicit
                       (directory-file-name
                        (expand-file-name explicit invocation)))))
            ((null top) "GIT_DIR=.")
            ((equal gitdir
                    (directory-file-name (expand-file-name ".git" top)))
             "GIT_DIR")
            (t (concat "GIT_DIR=" gitdir)))
           (concat "GIT_PREFIX=" (ngit-hook--prefix repo)))))
    (when (getenv "GIT_WORK_TREE")
      (setq changes (append changes (list "GIT_WORK_TREE=."))))
    changes))

(defun ngit-hook--run (repo name args input env)
  "Implementation for the lazy `ngit-hook-run' entry point."
  (let* ((absolute (ngit--hook-path repo name))
         (environment (ngit-hook--env-apply
                       (ngit-hook--env-apply process-environment
                                             (ngit-hook--repository-env repo))
                       env))
         (effective-gitdir (ngit-hook--env-value environment "GIT_DIR"))
         (path (ngit--hook-command-path
                repo name absolute
                (and effective-gitdir
                     (file-name-absolute-p effective-gitdir))))
         (args (or args nil))
         (argv (cons path args))
         (fallback (append (list "/bin/sh" path) args)))
    (if (null path) 0
      (ngit-transport-run-command argv environment (ngit--hook-cwd repo)
                                  input fallback (equal name "pre-push")))))

(provide 'ngit-hook)

;;; ngit-hook.el ends here
