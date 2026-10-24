;;; ngit-config.el --- git config files -*- lexical-binding: t; -*-

;;; Commentary:

;; Reads the system, global and repository config files in git's order and
;; answers last-one-wins lookups.  Keys are normalized like git: section and
;; variable names lowercased, subsection kept verbatim. Includes expand at
;; their position in each file, before higher-precedence sources.

;;; Code:

(require 'ngit-store)

(defvar ngit--config-source nil)
(defvar ngit--config-line 0)
(defvar ngit--config-locations nil)

(defun ngit--config-bad-line ()
  (ngit--die "fatal: bad config line %d in file %s"
             ngit--config-line ngit--config-source))

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
          (when (and (< pos n) (= (aref line pos) ?\n))
            (setq ngit--config-line (1+ ngit--config-line)))
          (when (or (>= pos n) (not (memq (aref line pos) '(?n ?t ?b ?\\ ?\" ?\n))))
            (when ngit--config-source (ngit--config-bad-line)))
          (when (< pos n)
            (when pending-space (push pending-space out) (setq pending-space nil))
            (push (pcase (aref line pos) (?n "\n") (?t "\t") (?b "\b") (?\n "")
                         (c2 (string c2)))
                  out)))
         ((and (not quoted) (memq c '(?\s ?\t)))
          (when out (setq pending-space (concat pending-space (string c)))))
         (t
          (when pending-space (push pending-space out) (setq pending-space nil))
          (push (string c) out)))
        (setq pos (1+ pos))))
    (when (and quoted ngit--config-source) (ngit--config-bad-line))
    (apply #'concat (nreverse out))))

(defun ngit--config-logical-lines (text)
  "Return logical config lines paired with their first physical line number.
Retain escaped newlines so value parsing diagnoses the exact physical line."
  (let ((physical (split-string text "\n")) (number 0) (first 1) (pending "") (lines nil))
    (while physical
      (setq number (1+ number)
            pending (concat pending (string-remove-suffix "\r" (pop physical))))
      (let ((position 0) (quoted nil) (comment nil) (continued nil))
        (while (and (< position (length pending)) (not comment))
          (let ((char (aref pending position)))
            (cond ((= char ?\\)
                   (setq position (1+ position))
                   (when (= position (length pending)) (setq continued t)))
                  ((= char ?\") (setq quoted (not quoted)))
                  ((and (not quoted) (memq char '(?# ?\;))) (setq comment t))))
          (setq position (1+ position)))
        (if (and continued physical)
            (setq pending (concat pending "\n"))
          (push (cons first pending) lines)
          (setq pending "" first (1+ number)))))
    (nreverse lines)))

(defun ngit--config-parse (text &optional callback)
  "Parse config TEXT into a list of (KEY . VALUE), in file order."
  (let ((section nil) (entries nil)
        (lines (ngit--config-logical-lines text)))
    (dolist (raw lines)
      (setq ngit--config-line (car raw))
      (let ((line (string-trim-left (cdr raw))) (before entries))
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
                entries))
         (ngit--config-source (ngit--config-bad-line)))
        (unless (eq before entries)
          (push (cons (car entries) ngit--config-line) ngit--config-locations)
          (when callback (funcall callback (car entries))))))
    (nreverse entries)))

(defun ngit--config-condition (condition file repo)
  "Test an include CONDITION using FILE and the discovered REPO context."
  (unless (fboundp 'ngit-wildmatch) (require 'ngit-match))
  (cond
   ((and repo (string-prefix-p "onbranch:" condition))
    (let* ((path (expand-file-name "HEAD" (ngit-repo-gitdir repo)))
           (head (and (file-readable-p path)
                      (string-trim (decode-coding-string (ngit--read-file path) 'utf-8))))
           (pattern (substring condition 9)))
      (when (string-suffix-p "/" pattern) (setq pattern (concat pattern "**")))
      (and head (string-prefix-p "ref: refs/heads/" head)
           (ngit-wildmatch pattern (substring head 16) t))))
   ((and repo (or (string-prefix-p "gitdir:" condition)
                  (string-prefix-p "gitdir/i:" condition)))
    (let* ((fold (string-prefix-p "gitdir/i:" condition))
           (pattern (substring condition (if fold 9 7))) (prefix 0)
           (gitdir (directory-file-name (expand-file-name (ngit-repo-gitdir repo))))
           (physical (directory-file-name (file-truename gitdir))))
      (cond ((string-prefix-p "~/" pattern) (setq pattern (expand-file-name pattern)))
            ((string-prefix-p "./" pattern)
             (let ((directory (file-name-directory (file-truename file))))
               (setq prefix (length directory)
                     pattern (concat directory (substring pattern 2)))))
            ((not (file-name-absolute-p pattern)) (setq pattern (concat "**/" pattern))))
      (when (string-suffix-p "/" pattern) (setq pattern (concat pattern "**")))
      (when fold (setq pattern (downcase pattern) gitdir (downcase gitdir)
                       physical (downcase physical)))
      (let ((literal (substring pattern 0 prefix)) (glob (substring pattern prefix)))
        (or (and (string-prefix-p literal gitdir)
                 (ngit-wildmatch glob (substring gitdir prefix) t))
            (and (string-prefix-p literal physical)
                 (ngit-wildmatch glob (substring physical prefix) t))))))))

(defun ngit--config-display-file (file repo)
  "Use Git's discovered local config spelling for diagnostics."
  (if (and repo (ngit-repo-toplevel repo) (not (getenv "GIT_DIR"))
           (equal (expand-file-name file)
                  (expand-file-name ".git/config" (ngit-repo-toplevel repo))))
      ".git/config"
    file))

(defun ngit--config-access-error (file)
  "Return zero or the actual open errno for FILE, closing any owned descriptor."
  (let* ((bytes (encode-coding-string file 'utf-8))
         (address (nl-ffi-memory-address (ngit-native--buffer 3 (1+ (length bytes))))))
    (ngit-native--write-bytes address bytes)
    (ptr-write-u8 address (length bytes) 0)
    (let ((descriptor (syscall-direct ngit-native--sys-open address
                                      ngit-native--o-rdonly-cloexec 0 0 0 0)))
      (if (< descriptor 0) (- descriptor)
        (syscall-direct ngit-native--sys-close descriptor 0 0 0 0 0)
        0))))

(defun ngit--config-access-message (number)
  "Return the conventional Linux diagnostic for config access errno NUMBER."
  (or (cdr (assq number '((1 . "Operation not permitted") (2 . "No such file or directory")
                          (5 . "Input/output error") (13 . "Permission denied")
                          (20 . "Not a directory") (21 . "Is a directory")
                          (23 . "Too many open files in system") (24 . "Too many open files")
                          (36 . "File name too long") (40 . "Too many levels of symbolic links"))))
      (format "error %d" number)))

(defun ngit--config-readable (file display &optional required primary)
  "Check FILE, ignoring only optional ENOENT/ENOTDIR failures.
REQUIRED requests the diagnostic used for an explicitly selected list source."
  (let ((number (ngit--config-access-error file)))
    (when (and primary (/= number 0) (not (memq number '(2 20))))
      (ngit-native-write-stderr
       (format "warning: unable to access '%s': %s\n" display (ngit--config-access-message number))))
    (cond ((= number 0) t)
          (required (ngit--die "fatal: unable to read config file '%s': %s"
                               display (ngit--config-access-message number)))
          ((memq number '(2 20)) nil)
          (primary nil)
          (t (ngit--die "fatal: unable to access '%s': %s"
                       display (ngit--config-access-message number))))))

(defvar ngit--config-startup-repo nil)

(defun ngit--config-startup (repo)
  "Validate REPO's primary syntax before command-specific include expansion.
Git's repository setup reads the local primary config without following includes."
  (when (and repo (not (eq repo ngit--config-startup-repo)))
    (let ((file (expand-file-name "config" (ngit-repo-commondir repo))))
      (ngit--config-read file repo nil))
    (setq ngit--config-startup-repo repo)))

(defun ngit--config-read (file repo includes &optional depth display)
  "Read FILE, expanding INCLUDES in place, to Git's maximum DEPTH of ten."
  (when (ngit--config-readable file (or display (ngit--config-display-file file repo)) nil (null depth))
    (let* ((display (or display (ngit--config-display-file file repo)))
           (ngit--config-source display) (ngit--config-line 0)
           (ngit--config-locations nil)
           (depth (or depth 0)) (out nil))
      (ngit--config-parse
       (decode-coding-string (ngit--read-file file) 'utf-8)
       (lambda (entry)
        (push entry out)
        (let* ((key (car entry))
               (condition (and (string-prefix-p "includeif." key)
                               (string-suffix-p ".path" key)
                               (substring key 10 -5))))
          (when (and includes
                     (or (string= key "include.path")
                         (and condition (ngit--config-condition condition file repo))))
            (unless (stringp (cdr entry))
              (ngit-native-write-stderr (format "error: missing value for '%s'\n" key))
              (ngit--config-bad-line))
            (let* ((value (cdr entry))
                   (shown (cond ((string-prefix-p "~" value) (expand-file-name value))
                                ((file-name-absolute-p value) value)
                                (t (concat (file-name-directory display) value))))
                   (path (expand-file-name value (file-name-directory (expand-file-name file)))))
              (when (and (ngit--config-readable path shown) (>= depth 10))
                (ngit--die (concat "fatal: exceeded maximum include depth (10) while including\n\t%s\nfrom\n\t%s\n"
                                   "This might be due to circular includes.") shown display))
              (dolist (included (ngit--config-read path repo t (1+ depth) shown))
                (push included out)))))))
      (nreverse out))))

(defvar ngit--config-cache nil "(REPO . ENTRIES) of the last config read.")

(defun ngit-config-entries (repo)
  "Return all config entries of REPO, lowest precedence first."
  (ngit--config-startup repo)
  (if (and ngit--config-cache (eq (car ngit--config-cache) repo))
      (cdr ngit--config-cache)
    (let ((entries nil))
      (dolist (f (ngit--config-files repo))
        (setq entries (append entries (ngit--config-read f repo t))))
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
