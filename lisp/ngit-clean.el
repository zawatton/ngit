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
  (mapcar
   (lambda (arg)
     (let* ((path (ngit-clean--path-item repo arg))
            (path (if (string-suffix-p "/" path) (substring path 0 -1) path)))
       (when (string= path "")
         (setq path ""))
       (cons path (and (ngit--glob-special-p path 0) t))))
   args))

(defun ngit-clean--literal-prefix (item)
  (let ((s (car item)) (i 0))
    (while (and (< i (length s))
                (not (memq (aref s i) '(?* ?? ?\[ ?\\))))
      (setq i (1+ i)))
    (substring s 0 i)))

(defun ngit-clean--pathspec-class (items path dirp)
  "Return full, partial, or nil for PATH against compiled ITEMS."
  (if (null items)
      'full
    (let ((result nil))
      (while (and items (not (eq result 'full)))
        (let* ((item (car items)) (spec (car item)))
          (if (not (cdr item))
              (cond ((or (string= spec "") (string= spec path)
                         (and (> (length path) (length spec))
                              (= (aref path (length spec)) ?/)
                              (string-prefix-p spec path)))
                     (setq result 'full))
                    ((and dirp (> (length spec) (length path))
                          (= (aref spec (length path)) ?/)
                          (string-prefix-p path spec))
                     (setq result 'partial)))
            (let ((prefix (ngit-clean--literal-prefix item)))
              (cond ((and (ngit-wildmatch spec path nil)) (setq result 'full))
                    ((and dirp
                          (or (string-prefix-p prefix (concat path "/"))
                              (string-prefix-p (concat path "/") prefix)))
                     (setq result 'partial))))))
        (setq items (cdr items)))
      result)))

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
               (concat (file-name-as-directory abs) ".gitignore") dir))
         (stack (if own (cons (ngit-compile-ignore own) stack) stack))
         (candidates nil) (protected nil))
    (dolist (name (sort (ngit--dir-names abs) #'string<))
      (let* ((path (concat dir name))
             (st (ngit-wt-lstat path))
             (dirp (and st (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir)))
             (class (and st (ngit-clean--pathspec-class pathspecs path dirp)))
             (tracked (and st (ngit-index-lookup entries path)))
             (tracked-under (and dirp (ngit-clean--tracked-under-p entries path))))
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
               ((and nested (< force 2)) (setq protected t))
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
                  (setq candidates (append (car sub) candidates)
                        ;; Non-ignored content prevents collapsing this directory.
                        protected t)))
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
  (let ((gone t) (deleted nil))
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
                                  (ngit-quote-path child) "\n")))
                (if (ngit-clean--delete-directory
                     path child force dry-run quiet)
                    (push child deleted)
                  (setq gone nil))))
             (t
              (unless dry-run (delete-file path))
              (push child deleted)))))))
    (if gone
        (progn (unless dry-run (delete-directory directory)) t)
      (unless quiet
        (dolist (path (nreverse deleted))
          (ngit--emit (if dry-run "Would remove " "Removing ")
                      (ngit-quote-path path) "\n")))
      nil)))

;;;###autoload
(defun ngit-cmd-clean (repo args)
  "Implement clean -n/-f/-d/-x/-X/-e with pathspecs."
  (unless (ngit-repo-toplevel repo)
    (ngit--die "fatal: this operation must be run in a work tree"))
  (let ((dry-run nil) (force 0) (remove-dirs nil) (ignored-all nil)
        (ignored-only nil) (quiet nil) (excludes nil) (paths nil))
    (while args
      (let ((arg (pop args)))
        (cond
         ((string= arg "--") (setq paths (append (nreverse paths) args) args nil))
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
           (scan nil) (candidates nil))
      (ngit-wt-setup top)
      (setq scan (ngit-clean--scan repo entries top "" standard cmd specs
                                   remove-dirs ignored-all ignored-only force)
            candidates (sort (delete-dups (car scan)) #'string<))
      (dolist (path candidates)
        (let* ((dirp (string-suffix-p "/" path))
               (rel (if dirp (substring path 0 -1) path))
               (abs (expand-file-name (decode-coding-string rel 'utf-8) top)))
          (if (and dirp (file-directory-p abs) (not (file-symlink-p abs)))
              (when (ngit-clean--delete-directory
                     abs rel force dry-run quiet)
                (unless quiet
                  (ngit--emit (if dry-run "Would remove " "Removing ")
                              (ngit-quote-path path) "\n")))
            (unless quiet
              (ngit--emit (if dry-run "Would remove " "Removing ")
                          (ngit-quote-path path) "\n"))
            (unless dry-run
              (when (or (file-exists-p abs) (file-symlink-p abs))
                (delete-file abs))))))
      0)))

(provide 'ngit-clean)

;;; ngit-clean.el ends here
