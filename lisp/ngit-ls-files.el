;;; ngit-ls-files.el --- index and work-tree file listings -*- lexical-binding: t; -*-

;;; Commentary:

;; Selection uses root paths; output follows the invocation directory.

;;; Code:

(require 'ngit-status)
(require 'ngit-add)

(defun ngit-ls-files--walk (repo top entries dir stack standard ignored inherited
                                     &optional items directory noempty matching)
  "List other paths, preserving inherited exclusions and directory boundaries."
  (let* ((abs (concat top (decode-coding-string dir 'utf-8)))
         (own (and standard (not inherited)
                   (ngit-read-ignore-file (concat abs ".gitignore") dir t)))
         (stack (if own (cons (ngit-compile-ignore own) stack) stack))
         (out nil))
    (dolist (name (ngit--dir-names abs))
      (let* ((path (concat dir name)) (st (ngit-wt-lstat path))
             (dirp (and st (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir)))
             (excluded (or inherited (and standard (ngit-excluded-p stack path (length dir) dirp)))))
        (when (and st (not (ngit-index-lookup entries path)))
          (if (not dirp)
              (when (eq (and excluded t) (and ignored t)) (push path out))
            (unless (or (and (not ignored) excluded)
                        (and items (not excluded) (not (ngit-index-dir-p entries (concat path "/")))
                             (not (ngit-pathspec-untracked-class items path))))
              (let* ((tracked (ngit-index-dir-p entries (concat path "/")))
                     (nested (ngit-wt-lstat (concat path "/.git")))
                     (children (unless nested
                                 (ngit-ls-files--walk repo top entries (concat path "/")
                                                      stack standard ignored excluded items
                                                      directory noempty matching))))
                (cond
                 ((and (not tracked) (eq (and excluded t) (and ignored t))
                       (or nested
                           (and directory (or (not items)
                                              (eq (ngit-pathspec-untracked-class items path) 'full)))
                           (and (eq matching t) excluded)))
                  (when (or nested children (not noempty)) (push (concat path "/") out)))
                 ((and directory ignored (eq matching 'traditional) (not tracked) children
                       (not (ngit-ls-files--walk repo top entries (concat path "/") stack
                                                standard nil excluded items nil t nil)))
                  (push (concat path "/") out))
                 ((and ignored directory noempty (not matching) (not tracked) (not excluded)) nil)
                 (t (setq out (append children out))))))))))
    out))

;;;###autoload
(defun ngit-cmd-list-files (repo args)
  "Implement ls-files selection and invocation-relative display."
  (let ((stage nil) (unmerged nil) (modified nil) (deleted nil) (others nil)
        (directory nil) (noempty nil) (cached nil) (standard nil) (ignored nil) (full nil) (nul nil) (paths nil))
    (while args
      (let ((arg (pop args)))
        (cond ((string= arg "--") (setq paths (append (nreverse paths) args) args nil))
              ((member arg '("-s" "--stage")) (setq stage t cached t))
              ((member arg '("-u" "--unmerged")) (setq unmerged t stage t cached t))
              ((member arg '("-m" "--modified")) (setq modified t))
              ((member arg '("-d" "--deleted")) (setq deleted t))
              ((member arg '("-o" "--others")) (setq others t))
              ((member arg '("-c" "--cached")) (setq cached t))
              ((member arg '("-i" "--ignored")) (setq ignored t))
              ((string= arg "--directory") (setq directory t))
              ((string= arg "--no-empty-directory") (setq noempty t))
              ((string= arg "--exclude-standard") (setq standard t))
              ((string= arg "--full-name") (setq full t))
              ((string= arg "-z") (setq nul t))
              ((string-prefix-p "-" arg) (ngit--die "unsupported ls-files option %s" arg))
              (t (push arg paths)))))
    (unless (or others modified deleted unmerged) (setq cached t))
    (when (and ignored (not standard))
      (ngit--die "fatal: ls-files --ignored needs some exclude pattern"))
    (let* ((prefix (if (ngit-repo-toplevel repo) (ngit-paths-prefix repo) ""))
           (items (ngit-pathspec-items repo
                    (mapcar (lambda (path)
                              (when (string= path "")
                                (ngit--die "fatal: empty string is not a valid pathspec. please use . instead if you meant to match all paths"))
                              (if (string-prefix-p ":" path) path (concat ":()" path))) paths)))
           (entries (ngit-index-entries repo))
           (changes (and modified (ngit-wt-changes repo t)))
           (changed (make-hash-table :test 'equal))
           (top (and (ngit-repo-toplevel repo) (file-name-as-directory (ngit-repo-toplevel repo))))
           (names nil))
      (when (> (length items) (length paths))
        (setq items (cons prefix (cdr items))))
      (dolist (c changes) (puthash (aref c 0) t changed))
      (when top (ngit-wt-setup top))
      (when others
        (let* ((dir (if (or paths (string= prefix "")) "" (concat prefix "/")))
               (ctx (and standard (not (string= dir "")) (ngit--ignore-context repo dir)))
               (stack (and standard (if ctx (car ctx) (ngit-ignore-global-patterns repo)))))
          (dolist (path (sort (ngit-ls-files--walk repo top entries dir stack standard ignored
                                                 (and ctx (cdr ctx))
                                                 items directory noempty) #'string<))
            (push (cons path nil) names))))
      (let ((i 0))
        (while (< i (length entries))
          (let* ((entry (aref entries i)) (path (aref entry 0))
                 (missing (and deleted (not (ngit-wt-lstat path))))
                 (excluded (and ignored
                                (let* ((dir (or (file-name-directory path) ""))
                                       (ctx (ngit--ignore-context repo dir)))
                                  (or (cdr ctx) (ngit-excluded-p (car ctx) path (length dir) nil))))))
            (when (and (or (not ignored) excluded)
                       (or (not unmerged) (/= (aref entry 3) 0)))
              (when (or cached unmerged) (push (cons path entry) names))
              (when missing (push (cons path entry) names))
              (when (and modified (gethash path changed)) (push (cons path entry) names))))
          (setq i (1+ i))))
      (let (selected)
        (dolist (pair (nreverse names))
          (let ((path (car pair)))
            (when (and (or items (string= prefix "") (ngit-paths-covers-p prefix path))
                       (ngit-paths-match-p items path))
              (push pair selected))))
        (setq selected (nreverse selected))
        (let* ((display (vconcat (mapcar (lambda (pair)
                                         (if full (car pair)
                                           (ngit-paths-relative prefix (car pair)))) selected)))
               (quoted (if nul display (ngit-quote-paths display)))
               (modes nil) (i 0))
          (dolist (pair selected)
            (let ((entry (cdr pair)))
              (when (and stage entry)
                (let* ((mode (aref entry 2))
                       (octal (or (cdr (assq mode modes))
                                  (let ((value (ngit--octal6 mode)))
                                    (push (cons mode value) modes) value))))
                  (ngit--emit octal " " (ngit-hex (aref entry 1))
                              (format " %d\t" (aref entry 3)))))
              (ngit--emit (aref quoted i) (if nul (string 0) "\n")))
            (setq i (1+ i)))))
      0)))

(provide 'ngit-ls-files)
;;; ngit-ls-files.el ends here
