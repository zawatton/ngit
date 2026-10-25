;;; ngit-clean.el --- remove untracked work-tree files -*- lexical-binding: t; -*-

;;; Commentary:

;; Implements the non-interactive clean modes.  The walker keeps ignored
;; entries visible while deciding whether a whole directory can be removed;
;; this is the distinction between "dir/" and individual paths in Git's
;; clean output.

;;; Code:

(require 'ngit-add)

(defun ngit-clean--path-item (repo arg)
  "Return ARG as a repository-relative unibyte path."
  (let* ((top (file-name-as-directory (ngit-repo-toplevel repo)))
         (abs (directory-file-name (expand-file-name arg default-directory)))
         (rel (cond ((string= (file-name-as-directory abs) top) "")
                    ((string-prefix-p top abs) (substring abs (length top)))
                    (t "../"))))
    (when (string-prefix-p "../" rel)
      (ngit--die "fatal: %s: '%s' is outside repository at '%s'"
                 arg arg (directory-file-name top)))
    (encode-coding-string rel 'utf-8)))

(defun ngit-clean--compile-pathspecs (repo args)
  (let ((items
         (mapcar (lambda (arg)
                   (when (string= arg "")
                     (ngit--die "fatal: empty string is not a valid pathspec. please use . instead if you meant to match all paths"))
                   (or (ngit-pathspec-compile repo arg)
                       (ngit-pathspec-compile repo (concat ":()" arg))))
                 (or args '(".")))))
    (if (seq-every-p (lambda (item) (aref item 4)) items)
        (cons (ngit-pathspec-compile repo ":().") items)
      items)))

(defun ngit-clean--literal-prefix (item)
  ;; The compiler's fixed prefix already treats the invocation directory
  ;; literally; rescanning from zero would interpret its metacharacters.
  (if (aref item 3) (substring (aref item 0) 0 (aref item 7))
    (aref item 9)))

(defvar ngit-clean--prefixes nil)
(defvar ngit-clean--display-prefix "")
(defvar ngit-clean--cwd nil)
(defvar ngit-clean--from-cwd nil)
(defvar ngit-clean--removal-prefix nil)

(defun ngit-clean--pathspec-class (items path dirp)
  "Return full, partial, or nil for PATH against compiled ITEMS."
  (if (ngit-pathspec-match-p items (if dirp (concat path "/") path))
      'full
    (and dirp
         (let ((directory (concat path "/")))
           (seq-some (lambda (prefix)
                       (or (string-prefix-p prefix directory)
                           (string-prefix-p directory prefix))) ngit-clean--prefixes))
         'partial)))

(defun ngit-clean--ignored-class (items path)
  "Admit ignored directory PATH by positive lexical ITEMS.
Git skips full pathspec matching for ignored directories.  The preliminary
nowildcard-prefix check still bounds traversal, with case folding limited
to the pattern after the invocation prefix.  Leading ignored ancestors
are admitted whole too (dir.c treat_leading_path)."
  (let ((directory (concat path "/")) (selected nil))
    (dolist (item items)
      (unless (aref item 4)
        (let* ((pattern (aref item 0)) (prefix (aref item 9))
               (name directory) (fixed (substring pattern 0 (aref item 7))))
          ;; Git's simple_length stops at metacharacters in the literal cwd,
          ;; then clamps nowildcard_len to the invocation prefix.  Preserve
          ;; that ignored-admission rule without wildcard-matching the cwd.
          (when (and (not (aref item 1)) (ngit--glob-special-p fixed 0))
            (setq prefix fixed))
          (when (or (not (aref item 3))
                    (string-prefix-p fixed name) (string-prefix-p name fixed))
            (when (aref item 3)
              (setq pattern (downcase pattern) prefix (downcase prefix)
                    name (downcase name)))
            (when (or (if (< (length prefix) (length pattern))
                          (string-prefix-p prefix name)
                        (ngit-paths-covers-p pattern (substring name 0 -1)))
                      (string-prefix-p name prefix))
              (setq selected 'full))))))
    selected))

(defun ngit-clean--display (path)
  (let ((display
         (if (and ngit-clean--removal-prefix
                  (string-prefix-p (car ngit-clean--removal-prefix) path))
             ;; remove_dirs keeps the selected ancestor's lexical ../ path
             ;; while recursing, including when it reaches the original cwd.
             (concat (cdr ngit-clean--removal-prefix)
                     (substring path (length (car ngit-clean--removal-prefix))))
           (ngit-paths-relative ngit-clean--display-prefix path))))
    (ngit-quote-path (if (and ngit-clean--from-cwd
                              (not (string-prefix-p "../" display))
                              (not (string-prefix-p "./" display)))
                         (concat "./" display) display))))

(defun ngit-clean--tracked-under-p (entries dir)
  (ngit-index-dir-p entries (concat dir "/")))

(defun ngit-clean--basename-start (path)
  (let ((i (1- (length path))))
    (while (and (>= i 0) (/= (aref path i) ?/)) (setq i (1- i)))
    (1+ i)))

(defun ngit-clean--scan (repo entries top dir stack cmd-stack pathspecs
                              remove-dirs ignored-all ignored-only force)
  "Return (CANDIDATES . PROTECTED) below DIR.
PROTECTED means the directory contains something this invocation must keep."
  (let* ((abs (concat top (decode-coding-string dir 'utf-8)))
         (own (ngit-read-ignore-file
               (concat (file-name-as-directory abs) ".gitignore") dir t))
         (stack (if own (cons (ngit-compile-ignore own) stack) stack))
         (candidates nil) (protected nil))
    (dolist (name (sort (ngit--dir-names abs) #'string<))
      (let* ((path (concat dir name))
             (st (ngit-wt-lstat path))
             (dirp (and st (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir)))
             (tracked (and st (ngit-index-lookup entries path)))
             (tracked-under (and dirp (ngit-clean--tracked-under-p entries path)))
             (class (and st
                         (if (and dirp (not tracked-under)
                                  (or ignored-only (ngit-pathspec-attr-p pathspecs)))
                             ;; DIR_SHOW_IGNORED_TOO admits an ignored directory
                             ;; before attribute matching; clean -X removes it
                             ;; whole if its literal prefix can be relevant.
                             (if (and ignored-only
                                      (ngit-excluded-p
                                       (if cmd-stack (cons cmd-stack stack) stack)
                                       path (ngit-clean--basename-start path) t))
                                 (ngit-clean--ignored-class pathspecs path)
                               (ngit-pathspec-untracked-class pathspecs path))
                           (ngit-clean--pathspec-class pathspecs path dirp)))))
        ;; Ordinary clean must retain ignored content even when pathspec
        ;; matching would skip it or descend through a partial directory.
        ;; Otherwise a selected ancestor could be collapsed around that data.
        ;; -x admits it, while -X has its own ignored-directory admission.
        (when (and st (not ignored-all) (not ignored-only)
                   (ngit-excluded-p (if cmd-stack (cons cmd-stack stack) stack)
                                    path (ngit-clean--basename-start path) dirp))
          (setq protected t class nil))
        (when (and st class)
          (cond
           (tracked)
           (tracked-under
            (let ((sub (ngit-clean--scan
                        repo entries top (concat path "/") stack cmd-stack
                        pathspecs remove-dirs ignored-all ignored-only force)))
              (setq candidates (append (car sub) candidates)
                    protected (or protected (cdr sub)))))
           (dirp
            (let* ((nested (file-exists-p
                            (concat (file-name-as-directory
                                     (expand-file-name
                                      (decode-coding-string path 'utf-8) top))
                                    ".git")))
                   (excluded (ngit-excluded-p
                              (if cmd-stack (cons cmd-stack stack) stack)
                              path (ngit-clean--basename-start path) t)))
              (cond
               ((and nested (< force 2)))
               ((eq class 'partial)
                (let ((sub (ngit-clean--scan
                            repo entries top (concat path "/") stack cmd-stack
                            pathspecs remove-dirs ignored-all ignored-only force)))
                  (setq candidates (append (car sub) candidates)
                        protected (or protected (cdr sub)))))
               ((not remove-dirs) (setq protected t))
               ((and ignored-only excluded)
                (push (concat path "/") candidates))
               ((and ignored-only (not excluded))
                (let ((sub (ngit-clean--scan
                            repo entries top (concat path "/") stack cmd-stack
                            pathspecs remove-dirs ignored-all ignored-only force)))
                  (if (and (car sub) (not (cdr sub)))
                      (push (concat path "/") candidates)
                    (setq candidates (append (car sub) candidates)
                          ;; Empty or non-ignored content keeps this directory.
                          protected t))))
               ((and (not ignored-all) excluded) (setq protected t))
               (t
                (let ((sub (ngit-clean--scan
                            repo entries top (concat path "/") stack cmd-stack
                            pathspecs remove-dirs ignored-all ignored-only force)))
                  (if (cdr sub)
                      (setq candidates (append (car sub) candidates)
                            protected t)
                    (push (concat path "/") candidates)))))))
           (t
            (let ((excluded (ngit-excluded-p
                             (if cmd-stack (cons cmd-stack stack) stack)
                             path (ngit-clean--basename-start path) nil)))
              (cond ((and ignored-only excluded) (push path candidates))
                    (ignored-only (setq protected t))
                    ((and (not ignored-all) excluded) (setq protected t))
                    (t (push path candidates)))))))))
    (cons candidates protected)))

(defun ngit-clean--delete-directory (directory rel force dry-run quiet)
  "Remove DIRECTORY recursively and return non-nil when it is gone.
REL is its repository-relative name without a trailing slash.  With fewer
than two FORCE flags, retain nested repositories.  If that leaves this
directory in place, report successfully removed direct children as Git's
remove_dirs does."
  (let* ((cwd-p (string= (directory-file-name (file-truename directory)) ngit-clean--cwd))
         (ngit-clean--from-cwd (or ngit-clean--from-cwd cwd-p))
         (gone t) (deleted nil))
    (dolist (path (directory-files directory t nil t))
      (let ((name (file-name-nondirectory path)))
        (unless (member name '("." ".."))
          (let* ((bytes (if (multibyte-string-p name)
                            (encode-coding-string name 'utf-8) name))
                 (child (concat rel "/" bytes)))
            (cond
             ((file-symlink-p path)
              (unless dry-run (delete-file path))
              (push child deleted))
             ((file-directory-p path)
              (if (and (< force 2)
                       (file-exists-p
                        (expand-file-name ".git" (file-name-as-directory path))))
                  (progn
                    (setq gone nil)
                    (unless quiet
                      (ngit--emit (if dry-run "Would skip repository "
                                    "Skipping repository ")
                                  (ngit-clean--display child) "\n")))
                (if (ngit-clean--delete-directory
                     path child force dry-run quiet)
                    (push child deleted)
                  (setq gone nil))))
             (t
              (unless dry-run (delete-file path))
              (push child deleted)))))))
    (when (and gone cwd-p)
      (ngit--emit (if dry-run "Would refuse to remove current working directory\n"
                    "Refusing to remove current working directory\n"))
      (setq gone nil))
    (if gone
        (progn (unless dry-run (delete-directory directory)) t)
      (unless quiet
        (dolist (path (nreverse deleted))
          (ngit--emit (if dry-run "Would remove " "Removing ")
                      (ngit-clean--display path) "\n")))
      nil)))

;;;###autoload
(defun ngit-cmd-clean (repo args)
  "Implement clean -n/-f/-d/-x/-X/-e with pathspecs."
  (unless (ngit-repo-toplevel repo)
    (ngit--die "fatal: this operation must be run in a work tree"))
  (let ((dry-run nil) (force 0) (remove-dirs nil) (ignored-all nil)
        (ignored-only nil) (quiet nil) (excludes nil) (paths nil)
        (ngit-clean--display-prefix (ngit-paths-prefix repo)))
    (while args
      (let ((arg (pop args)))
        (cond
         ((string= arg "--") (setq paths (append (reverse args) paths) args nil))
         ((member arg '("--dry-run")) (setq dry-run t))
         ((member arg '("--force")) (setq force (1+ force)))
         ((string= arg "--exclude")
          (unless args (ngit--die-with 129 "error: option `exclude' requires a value"))
          (push (pop args) excludes))
         ((string-prefix-p "--exclude=" arg) (push (substring arg 10) excludes))
         ((string-prefix-p "--" arg)
          (ngit--die-with 129 "error: unknown option `%s'" arg))
         ((and (> (length arg) 1) (= (aref arg 0) ?-))
          (let ((i 1))
            (while (< i (length arg))
              (pcase (aref arg i)
                (?n (setq dry-run t))
                (?f (setq force (1+ force)))
                (?d (setq remove-dirs t))
                (?x (setq ignored-all t))
                (?X (setq ignored-only t))
                (?q (setq quiet t))
                (?e
                 (let ((value (if (< (1+ i) (length arg))
                                  (substring arg (1+ i))
                                (unless args
                                  (ngit--die-with 129 "error: option `e' requires a value"))
                                (pop args))))
                   (push value excludes)
                   (setq i (length arg))))
                (_ (ngit--die-with 129 "error: unknown switch `%c'" (aref arg i))))
              (setq i (1+ i)))))
         (t (push arg paths)))))
    (setq paths (nreverse paths) excludes (nreverse excludes))
    (when (and ignored-all ignored-only)
      (ngit--die "fatal: options '-x' and '-X' cannot be used together"))
    (when (and (not dry-run) (= force 0)
               (ngit-config-bool repo "clean.requireforce" t))
      (ngit--die
       "fatal: clean.requireForce is true and -f not given: refusing to clean"))
    (when paths (setq remove-dirs t))
    (let* ((top (file-name-as-directory (ngit-repo-toplevel repo)))
           (entries (ngit-index-entries repo))
           (standard (unless ignored-all (ngit-ignore-global-patterns repo)))
           (cmd (and excludes
                     (ngit-compile-ignore
                      (let ((out nil))
                        (dolist (pattern excludes)
                          (push (ngit-parse-pattern pattern "") out))
                        out))))
           (specs (ngit-clean--compile-pathspecs repo paths))
           (ngit-clean--prefixes
            (mapcar #'ngit-clean--literal-prefix
                    (seq-filter (lambda (item) (not (aref item 4))) specs)))
           (ngit-clean--cwd (directory-file-name
                            (file-truename (or ngit--invocation-directory default-directory))))
           (scan nil) (candidates nil))
      (ngit-wt-setup top)
      (setq scan (ngit-clean--scan repo entries top "" standard cmd specs
                                   remove-dirs ignored-all ignored-only force)
            candidates (sort (delete-dups (car scan)) #'string<))
      (dolist (path candidates)
        (let* ((dirp (string-suffix-p "/" path))
               (rel (if dirp (substring path 0 -1) path))
               (abs (expand-file-name (decode-coding-string rel 'utf-8) top))
               (ngit-clean--removal-prefix
                (and dirp (cons path (ngit-paths-relative ngit-clean--display-prefix path)))))
          (if (and dirp (file-directory-p abs) (not (file-symlink-p abs)))
              (when (ngit-clean--delete-directory
                     abs rel force dry-run quiet)
                (unless quiet
                  (ngit--emit (if dry-run "Would remove " "Removing ")
                              (ngit-clean--display path) "\n")))
            (unless quiet
              (ngit--emit (if dry-run "Would remove " "Removing ")
                          (ngit-clean--display path) "\n"))
            (unless dry-run
              (when (or (file-exists-p abs) (file-symlink-p abs))
                (delete-file abs))))))
      0)))

(provide 'ngit-clean)

;;; ngit-clean.el ends here
