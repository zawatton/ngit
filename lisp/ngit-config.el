;;; ngit-config.el --- git config files -*- lexical-binding: t; -*-

;;; Commentary:

;; Reads the system, global and repository config files in git's order and
;; answers last-one-wins lookups.  Keys are normalized like git: section and
;; variable names lowercased, subsection kept verbatim.  `include' and
;; `includeIf' are not followed.

;;; Code:

(require 'ngit-store)

(defun ngit--config-files (repo)
  "Return the config files of REPO, lowest precedence first."
  (let ((home (getenv "HOME"))
        (xdg (getenv "XDG_CONFIG_HOME"))
        (global (getenv "GIT_CONFIG_GLOBAL"))
        (files nil))
    (unless (member (getenv "GIT_CONFIG_NOSYSTEM") '("1" "true" "yes"))
      (push (or (getenv "GIT_CONFIG_SYSTEM") "/etc/gitconfig") files))
    (if global
        (push global files)
      (push (expand-file-name "git/config"
                              (if (and xdg (not (string= xdg "")))
                                  xdg
                                (expand-file-name ".config" home)))
            files)
      (when home (push (expand-file-name ".gitconfig" home) files)))
    (when repo
      (push (expand-file-name "config" (ngit-repo-commondir repo)) files))
    (nreverse files)))

(defun ngit--config-value (line pos)
  "Parse a config value in LINE from POS: quotes, escapes, comments."
  (let ((n (length line)) (out nil) (quoted nil) (done nil) (pending-space nil))
    (while (and (< pos n) (not done))
      (let ((c (aref line pos)))
        (cond
         ((and (not quoted) (memq c '(?\; ?#))) (setq done t))
         ((= c ?\") (setq quoted (not quoted)))
         ((= c ?\\)
          (setq pos (1+ pos))
          (when (< pos n)
            (when pending-space (push pending-space out) (setq pending-space nil))
            (push (pcase (aref line pos) (?n "\n") (?t "\t") (?b "\b")
                         (c2 (string c2)))
                  out)))
         ((and (not quoted) (memq c '(?\s ?\t)))
          (when out (setq pending-space (concat pending-space (string c)))))
         (t
          (when pending-space (push pending-space out) (setq pending-space nil))
          (push (string c) out)))
        (setq pos (1+ pos))))
    (apply #'concat (nreverse out))))

(defun ngit--config-parse (text)
  "Parse config TEXT into a list of (KEY . VALUE), in file order."
  (let ((section nil) (entries nil)
        (lines (split-string (replace-regexp-in-string "\\\\\r?\n" "" text) "\n")))
    (dolist (raw lines)
      (let ((line (string-trim-left raw)))
        (cond
         ((or (string= line "") (memq (aref line 0) '(?# ?\;))))
         ((string-match "\\`\\[\\([-A-Za-z0-9.]+\\)\\(?:[ \t]+\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"\\)?\\][ \t]*\\(.*\\)\\'" line)
          (let ((name (downcase (match-string 1 line)))
                (sub (match-string 2 line))
                (rest (match-string 3 line)))
            (setq section
                  (cond (sub (concat name "." (replace-regexp-in-string "\\\\\\(.\\)" "\\1" sub)))
                        ((string-match "\\`\\([^.]+\\)\\.\\(.*\\)\\'" name)
                         (concat (match-string 1 name) "." (match-string 2 name)))
                        (t name)))
            (setq line rest))
          (when (string-match "\\`\\([A-Za-z][-A-Za-z0-9]*\\)[ \t]*\\(=\\)?" line)
            (push (cons (concat section "." (downcase (match-string 1 line)))
                        (if (match-beginning 2)
                            (ngit--config-value line (match-end 0))
                          t))
                  entries)))
         ((and section (string-match "\\`\\([A-Za-z][-A-Za-z0-9]*\\)[ \t]*\\(=\\)?" line))
          (push (cons (concat section "." (downcase (match-string 1 line)))
                      (if (match-beginning 2)
                          (ngit--config-value line (match-end 0))
                        t))
                entries)))))
    (nreverse entries)))

(defvar ngit--config-cache nil "(REPO . ENTRIES) of the last config read.")

(defun ngit-config-entries (repo)
  "Return all config entries of REPO, lowest precedence first."
  (if (and ngit--config-cache (eq (car ngit--config-cache) repo))
      (cdr ngit--config-cache)
    (let ((entries nil))
      (dolist (f (ngit--config-files repo))
        (when (file-readable-p f)
          (setq entries (append entries
                                (ngit--config-parse
                                 (decode-coding-string (ngit--read-file f) 'utf-8))))))
      (setq ngit--config-cache (cons repo entries))
      entries)))

(defun ngit--config-normalize (key)
  "Lowercase the section and variable parts of KEY, keep the subsection."
  (let ((first (string-search "." key))
        (last (let ((i (1- (length key)))) (while (and (>= i 0) (/= (aref key i) ?.)) (setq i (1- i))) i)))
    (if (or (null first) (= first last))
        (downcase key)
      (concat (downcase (substring key 0 first)) (substring key first last)
              (downcase (substring key last))))))

(defun ngit-config-get (repo key)
  "Return the last value of KEY in REPO's config: a string, t, or nil."
  (let ((key (ngit--config-normalize key)) (val nil))
    (dolist (e (ngit-config-entries repo))
      (when (string= (car e) key) (setq val (cdr e))))
    val))

(defun ngit-config-get-all (repo key)
  (let ((key (ngit--config-normalize key)) (vals nil))
    (dolist (e (ngit-config-entries repo))
      (when (string= (car e) key) (push (cdr e) vals)))
    (nreverse vals)))

(defun ngit-config-bool (repo key default)
  "Return KEY as a boolean, DEFAULT when unset."
  (let ((v (ngit-config-get repo key)))
    (cond ((null v) default)
          ((eq v t) t)
          ((member (downcase v) '("true" "yes" "on" "1")) t)
          ((member (downcase v) '("false" "no" "off" "0" "")) nil)
          (t default))))

;;;; Lazy hook gate

(defun ngit-hook-setenv (name value)
  "Export NAME as VALUE to both NeLisp and spawned hook environments.
NeLisp's setenv updates getenv without updating process-environment."
  (let ((prefix (concat name "=")) kept)
    (dolist (entry process-environment)
      (unless (or (equal entry name) (string-prefix-p prefix entry))
        (push entry kept)))
    (setq process-environment (nreverse kept))
    (when value (push (concat prefix value) process-environment))
    (setenv name value)))

(defvar ngit--hook-root-cache (make-hash-table :test 'eq)
  "Repository hook roots checked during this process.")

(defvar ngit--ignored-hook-advice nil
  "Alist of hook names already advised, keyed by repository identity.")

(defvar ngit-hook-working-directory nil
  "Working directory explicitly selected by the current hook caller.")

(defun ngit--hook-relative-path (path base)
  "Return absolute PATH relative to absolute directory BASE."
  (let ((path-parts (split-string (expand-file-name path) "/" t))
        (base-parts (split-string (expand-file-name base) "/" t)))
    (while (and path-parts base-parts
                (equal (car path-parts) (car base-parts)))
      (setq path-parts (cdr path-parts)
            base-parts (cdr base-parts)))
    (mapconcat #'identity
               (append (mapcar (lambda (_part) "..") base-parts) path-parts)
               "/")))

(defun ngit--hook-worktree (repo)
  "Return the work tree Git uses as the hook directory for REPO, or nil."
  (or (ngit-repo-toplevel repo)
      (let ((environment (getenv "GIT_WORK_TREE")))
        (and environment (not (string= environment ""))
             (expand-file-name environment default-directory)))
      (and (not (ngit-config-bool repo "core.bare" t))
           (let ((configured (ngit-config-get repo "core.worktree")))
             (cond
              ((and (stringp configured) (file-name-absolute-p configured))
               configured)
              ((stringp configured)
               (expand-file-name configured (ngit-repo-gitdir repo)))
              (t default-directory))))))

(defun ngit--hook-cwd (repo)
  "Return Git's hook working directory for REPO."
  (file-name-as-directory
   (expand-file-name
    (or ngit-hook-working-directory
        (ngit--hook-worktree repo)
        (ngit-repo-gitdir repo)))))

(defun ngit--hook-root-info (repo)
  "Return cached [ROOT CWD ABSOLUTE-CONFIG CONFIGURED] for REPO.
ROOT is nil when the configured hooks directory did not exist at the first
check.  This keeps hook-free commands to one directory test and avoids loading
the process runner merely because git-init installed *.sample files."
  (or (gethash repo ngit--hook-root-cache)
      (let* ((cwd (ngit--hook-cwd repo))
             (configured (ngit-config-get repo "core.hooksPath"))
             (configured (and (stringp configured) configured))
             (absolute (and configured
                            (or (file-name-absolute-p configured)
                                (string-prefix-p "~" configured))))
             (root (if configured
                       (expand-file-name configured cwd)
                     (expand-file-name "hooks" (ngit-repo-commondir repo))))
             (info (vector (and (file-directory-p root)
                                (file-name-as-directory root))
                           cwd absolute configured)))
        (puthash repo info ngit--hook-root-cache)
        info)))

(defun ngit--hook-advised-p (repo name)
  (member name (cdr (assq repo ngit--ignored-hook-advice))))

(defun ngit--hook-mark-advised (repo name)
  (let ((entry (assq repo ngit--ignored-hook-advice)))
    (if entry (setcdr entry (cons name (cdr entry)))
      (push (list repo name) ngit--ignored-hook-advice))))

(defun ngit--hook-path (repo name)
  "Return executable hook path for NAME, or nil, without loading hook code."
  (let* ((info (ngit--hook-root-info repo))
         (root (aref info 0))
         (path (and root (expand-file-name name root))))
    (cond
     ((and path (file-executable-p path)) path)
     ((and path (file-exists-p path)
           (not (ngit--hook-advised-p repo name)))
      (ngit--hook-mark-advised repo name)
      (when (ngit-config-bool repo "advice.ignoredHook" t)
        (let ((shown (ngit--hook-command-path repo name path)))
          (ngit-native-write-stderr
           (concat "hint: The '" shown "' hook was ignored because it's not set as executable.\n"
                   "hint: You can disable this warning with `git config advice.ignoredHook false`.\n"))))
      nil)
     (t nil))))

(defun ngit--hook-command-path (repo name &optional absolute-path absolute-gitdir)
  "Return the hook pathname Git passes as argv[0] for NAME in REPO."
  (let* ((info (ngit--hook-root-info repo))
         (path (or absolute-path
                   (and (aref info 0) (expand-file-name name (aref info 0)))))
         (top (ngit-repo-toplevel repo))
         (linked (and top
                      (not (equal (directory-file-name
                                   (expand-file-name (ngit-repo-gitdir repo)))
                                  (directory-file-name
                                   (expand-file-name ".git" top)))))))
    (and path
         (if (or (aref info 2)
                 (and (null (aref info 3))
                      (or absolute-gitdir
                          (let ((gitdir (getenv "GIT_DIR")))
                            (and gitdir (file-name-absolute-p gitdir)))
                          linked)))
             path
           (ngit--hook-relative-path path (aref info 1))))))

(defun ngit-hook-exists-p (repo name)
  "Return non-nil when REPO has an executable hook named NAME.
The implementation module is loaded only after the named executable exists."
  (when (ngit--hook-path repo name)
    (unless (featurep 'ngit-hook)
      (load (expand-file-name "ngit-hook.el" ngit-native--dir) nil t))
    t))

(defun ngit-hook-run (repo name &optional args input env)
  "Run REPO hook NAME and return its numeric exit status.
ARGS is a list of strings.  INPUT is a string, or nil for /dev/null.  ENV is
a list of `KEY=value' entries and bare `KEY' entries which unset variables."
  (if (ngit-hook-exists-p repo name)
      (ngit-hook--run repo name args input env)
    0))

(provide 'ngit-config)

;;; ngit-config.el ends here
