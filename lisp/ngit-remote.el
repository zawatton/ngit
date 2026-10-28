;;; ngit-remote.el --- repository initialization and remotes -*- lexical-binding: t; -*-

;;; Commentary:

;; Repository initialization and the local configuration/ref
;; parts of `git remote'.  Transport operations live in the transport module.

;;; Code:

(require 'ngit-write)
(require 'ngit-commit)
(require 'ngit-branch)

(defun ngit-init--valid-branch-p (name)
  (and (stringp name) (ngit--valid-branch-name-p name)))

(defun ngit-init--copy-template (source target)
  "Copy missing entries from template directory SOURCE into TARGET."
  (dolist (name (directory-files source nil nil t))
    (unless (string-prefix-p "." name)
      (let ((from (expand-file-name name source))
            (to (expand-file-name name target)))
        (cond
         ((or (file-exists-p to) (file-symlink-p to))
          (when (and (file-directory-p from) (file-directory-p to)
                     (not (file-symlink-p from)))
            (ngit-init--copy-template from to)))
         ((file-symlink-p from) (make-symbolic-link (file-symlink-p from) to))
         ((file-directory-p from)
          (ngit-make-directory to t)
          (ngit-init--copy-template from to))
         ((file-regular-p from)
          (ngit-write-file to (ngit--read-file from) nil nil (file-modes from))))))))

(defun ngit-init-repository (path bare branch &optional template)
  "Create or reinitialize a repository at PATH using optional TEMPLATE.
BARE selects a bare layout and BRANCH selects the initial branch for a new
repository.  Return its repository vector and do not print anything."
  (let* ((root (file-name-as-directory (expand-file-name path)))
         (gitdir (if bare root (file-name-as-directory (expand-file-name ".git" root))))
         (reinit (file-exists-p (expand-file-name "HEAD" gitdir)))
         (branch (or branch (getenv "GIT_TEST_DEFAULT_INITIAL_BRANCH_NAME")
                     (ngit-config-get nil "init.defaultbranch") "master")))
    (when (and (not reinit) (not (ngit-init--valid-branch-p branch)))
      (ngit--die "fatal: invalid initial branch name: '%s'" branch))
    (ngit-make-directory gitdir t)
    (let ((template (or template (getenv "GIT_TEMPLATE_DIR")
                        (ngit-config-get nil "init.templatedir"))))
      (when (and (stringp template) (not (string= template "")))
        (if (file-directory-p template)
            (ngit-init--copy-template (expand-file-name template) gitdir)
          (ngit-native-write-stderr
           (concat "warning: templates not found in " template "\n")))))
    (dolist (dir '("objects" "objects/pack" "objects/info" "refs"
                   "refs/heads" "refs/tags"))
      (ngit-make-directory (expand-file-name dir gitdir) t))
    (let ((repo (ngit--make-repo gitdir (and (not bare) root))))
      (ngit-config-set repo "core.repositoryformatversion" "0")
      (ngit-config-set repo "core.filemode" (if (ngit-windows-p) "false" "true"))
      (when (ngit-windows-p) (ngit-config-set repo "core.symlinks" "false"))
      (ngit-config-set repo "core.bare" (if bare "true" "false"))
      (if bare
          (ngit-config-set repo "core.logallrefupdates" nil)
        (ngit-config-set repo "core.logallrefupdates" "true"))
      (unless reinit
        (ngit-ref-write repo "HEAD" (concat "ref: refs/heads/" branch)))
      repo)))

(defun ngit-cmd-init (repo args)
  "Implement `git init' for ARGS, including invocation outside a repository."
  (let ((bare nil) (quiet nil) (branch nil) (path nil) (template nil))
    (while args
      (let ((a (pop args)))
        (cond
         ((string= a "--bare") (setq bare t))
         ((member a '("-q" "--quiet")) (setq quiet t))
         ((member a '("-b" "--initial-branch"))
          (unless args (ngit--die "error: option `%s' requires a value" a))
          (setq branch (pop args)))
         ((string-prefix-p "--initial-branch=" a) (setq branch (substring a 17)))
         ((string= a "--template")
          (unless args (ngit--die "error: option `template' requires a value"))
          (setq template (pop args)))
         ((string-prefix-p "--template=" a) (setq template (substring a 11)))
         ((string-prefix-p "-" a) (ngit--die "error: unknown option `%s'" a))
         (path (ngit--die "fatal: cannot mkdir %s: File exists" a))
         (t (setq path a)))))
    (when (and (not path) repo (null (ngit-repo-toplevel repo))) (setq bare t))
    (let* ((target (expand-file-name (or path default-directory)))
           (gitdir (if bare (file-name-as-directory target)
                     (file-name-as-directory (expand-file-name ".git" target))))
           (reinit (file-exists-p (expand-file-name "HEAD" gitdir))))
      (ngit-init-repository target bare branch template)
      (when (and (not reinit) (not branch) (not quiet)
                 (not (getenv "GIT_TEST_DEFAULT_INITIAL_BRANCH_NAME"))
                 (not (ngit-config-get nil "init.defaultbranch"))
                 (ngit-config-bool nil "advice.defaultBranchName" t))
        (ngit-native-write-stderr
         "hint: Using 'master' as the name for the initial branch. This default branch name\nhint: is subject to change. To configure the initial branch name to use in all\nhint: of your new repositories, which will suppress this warning, call:\nhint:\nhint: \tgit config --global init.defaultBranch <name>\nhint:\nhint: Names commonly chosen instead of 'master' are 'main', 'trunk' and\nhint: 'development'. The just-created branch can be renamed via this command:\nhint:\nhint: \tgit branch -m <name>\n"))
      (when (and reinit branch)
        (ngit-native-write-stderr
         (format "warning: re-init: ignored --initial-branch=%s\n" branch)))
      (unless quiet
        (ngit--emit
         (format "%s %s\n"
                 (if reinit "Reinitialized existing Git repository in"
                   "Initialized empty Git repository in") gitdir))))
    0))

(defun ngit-remote--names (repo)
  (let (names)
    (dolist (e (ngit-config-entries repo))
      (when (string-match "\\`remote\\.\\(.+\\)\\.[^.]+\\'" (car e))
        (let ((name (match-string 1 (car e))))
          (unless (member name names) (push name names)))))
    (sort names #'string<)))

(defun ngit-remote--exists-p (repo name)
  (member name (ngit-remote--names repo)))

(defun ngit-remote--valid-name-p (name)
  (and (not (string-prefix-p "-" name))
       (ngit--valid-branch-name-p (concat "remotes/" name "/x"))))

(defun ngit-remote--set-all (repo key values)
  (ngit-config-set repo key nil)
  (dolist (value values) (ngit-config-set repo key value t)))

(defun ngit-remote--urls (repo name push)
  (let ((vals (ngit-config-get-all
               repo (concat "remote." name "." (if push "pushurl" "url")))))
    (if (and push (null vals))
        (ngit-config-get-all repo (concat "remote." name ".url")) vals)))

(defun ngit-remote--delete-tree (path)
  (when (file-directory-p path) (ngit-delete-directory path t)))

(defun ngit-remote--rewrite-symrefs (dir old new)
  "Rewrite symbolic refs below DIR after remote OLD was renamed to NEW."
  (when (file-directory-p dir)
    (dolist (path (directory-files dir t "\\`[^.]"))
      (if (file-directory-p path)
          (ngit-remote--rewrite-symrefs path old new)
        (let* ((text (ngit--read-file path))
               (prefix (concat "ref: refs/remotes/" old "/")))
          (when (string-prefix-p prefix text)
            (ngit-write-file path
                             (concat "ref: refs/remotes/" new "/"
                                     (substring text (length prefix))))))))))

(defun ngit-remote--packed-transform (repo old new)
  "Remove refs below OLD, or rename their OLD prefix to NEW."
  (let ((file (expand-file-name "packed-refs" (ngit-repo-commondir repo))))
    (when (file-exists-p file)
      (let (headers records current)
        (dolist (line (split-string (ngit--read-file file) "\n"))
          (cond
           ((string-prefix-p "#" line)
            (setq current nil)
            (push line headers))
           ((and current (string-prefix-p "^" line))
            (aset current 2 line))
           ((> (length line) 41)
            (let* ((ref (substring line 41))
                   (renamed (and (string-prefix-p old ref)
                                 new (concat new (substring ref (length old))))))
              (setq current nil)
              (unless (and (string-prefix-p old ref) (null new))
                (setq current
                      (vector (or renamed ref)
                              (concat (substring line 0 41) (or renamed ref)) nil))
                (push current records))))))
        ;; A file advertising `sorted' must remain sorted after a namespace
        ;; moves across the other packed refs.  Keep each peeled line attached.
        (setq records (sort records (lambda (a b) (string< (aref a 0) (aref b 0)))))
        (let (body)
          (dolist (record records)
            (push (aref record 1) body)
            (when (aref record 2) (push (aref record 2) body)))
          (let ((out (append (nreverse headers) (nreverse body)))
                (lock (ngit-lock file)))
            (ngit-commit-lock
             lock (if out (concat (mapconcat #'identity out "\n") "\n") ""))))
        (aset repo 4 nil)))))

(defun ngit-remote--delete-refs (repo refs)
  "Delete REFS in one files-backend transaction."
  (when refs
    (let* ((common (ngit-repo-commondir repo))
           (updates (mapcar (lambda (ref) (list nil nil ref)) refs))
           (packed-refs (ngit--packed-refs repo))
           (has-packed (seq-some (lambda (ref) (assoc ref packed-refs)) refs))
           (lock-pairs nil) (packed-lock nil) (packed-input nil) (input nil))
      (unwind-protect
          (progn
            (dolist (ref refs)
              (let ((file (ngit--ref-file repo ref)))
                (ngit-make-directory (file-name-directory file) t)
                (push (cons ref (ngit-lock file)) lock-pairs)))
            (setq lock-pairs (nreverse lock-pairs)
                  packed-lock (ngit-lock (expand-file-name "packed-refs" common)))
            (let* ((outer-locks (mapcar #'cdr lock-pairs))
                   (all-locks (append outer-locks (list packed-lock))))
              (when has-packed
                (setq packed-input (ngit-ref-prepare repo updates nil all-locks)))
              (setq input
                    (ngit-ref-prepare repo updates (not has-packed)
                                      (if has-packed outer-locks all-locks)
                                      (and packed-input (list packed-input))
                                      nil
                                      (and has-packed (list packed-lock)))))
            (ngit--rewrite-packed-refs repo refs packed-lock)
            (ngit-ref-finish repo packed-input)
            (dolist (pair lock-pairs)
              (let* ((ref (car pair))
                     (file (ngit--ref-file repo ref))
                     (log (expand-file-name (concat "logs/" ref) common)))
                (when (file-exists-p file) (ngit-delete-file file))
                (when (file-exists-p log)
                  (ngit-delete-file log)
                  (ngit--remove-empty-dirs (file-name-directory log)
                                           (expand-file-name "logs/refs" common)))
                (ngit-unlock (cdr pair))
                (ngit--remove-empty-dirs (file-name-directory file)
                                         (expand-file-name "refs" common))))
            (ngit-unlock packed-lock)
            (aset repo 4 nil)
            (ngit-ref-finish repo input))
        (dolist (pair lock-pairs) (ngit-unlock (cdr pair)))
        (when packed-lock (ngit-unlock packed-lock))))))

(defun ngit-remote--rename-direct-ref (repo source target hex)
  "Rename direct ref SOURCE to TARGET while preserving backend hook behavior."
  (let* ((common (ngit-repo-commondir repo))
         (oldlog (expand-file-name (concat "logs/" source) common))
         (newlog (expand-file-name (concat "logs/" target) common))
         (log-data (and (file-exists-p oldlog) (ngit--read-file oldlog)))
         (log-mode (and log-data (file-modes oldlog)))
         (msg (concat "remote: renamed " source " to " target)))
    (ngit--delete-ref repo source nil hex)
    (when log-data
      (ngit-make-directory (file-name-directory newlog) t)
      (ngit-write-file newlog log-data nil nil log-mode))
    ;; refs_rename_ref copies the new backend ref outside a transaction.
    (let ((ngit-ref-inhibit-hooks t))
      (ngit--write-ref repo target hex hex msg t))))

(defun ngit-remote--move-namespace (repo old new)
  (unless (and (ngit-remote--valid-name-p old)
               (or (null new) (ngit-remote--valid-name-p new)))
    (ngit--die "fatal: invalid remote name"))
  (let* ((oldref (concat "refs/remotes/" old "/"))
         (newref (and new (concat "refs/remotes/" new "/")))
         (refs (ngit-list-refs repo oldref)))
    (if (null new)
        (ngit-remote--delete-refs repo (mapcar #'car refs))
      ;; remote.c removes symrefs, renames direct refs, then recreates the
      ;; symrefs so referents never point into a half-moved namespace.
      (dolist (entry refs)
        (when (nth 2 entry) (ngit--delete-ref repo (car entry))))
      (dolist (entry refs)
        (unless (nth 2 entry)
          (ngit-remote--rename-direct-ref
           repo (car entry) (concat newref (substring (car entry) (length oldref)))
           (nth 1 entry))))
      (let ((ident (and refs (ngit--branch-ident repo))))
        (dolist (entry refs)
          (let ((symbolic (nth 2 entry)))
            (when symbolic
              (let* ((source (car entry))
                     (target (concat newref (substring source (length oldref))))
                     (referent (concat newref (substring symbolic (length oldref))))
                     (hex (nth 1 entry))
                     (msg (concat "remote: renamed " source " to " target)))
                (ngit-ref-write repo target (concat "ref: " referent) nil)
                (ngit--append-reflog repo target nil hex ident msg)))))))))

(defun ngit-remote--replace-branch-links (repo old new)
  (dolist (e (copy-sequence (ngit-config-entries repo)))
    (when (and (string-match "\\`branch\\..+\\.\\(remote\\|pushremote\\)\\'" (car e))
               (string= (cdr e) old))
      (let ((key (car e)))
        (ngit-config-set repo key new)
        (when (and (null new) (string-suffix-p ".remote" key))
          (ngit-config-set repo (concat (substring key 0 -7) ".merge") nil))))))

(defun ngit-remote--add (repo args)
  (when (or (/= (length args) 2) (string-prefix-p "-" (car args)))
    (ngit--die "usage: git remote add <name> <url>"))
  (let ((name (car args)) (url (cadr args)))
    (when (ngit-remote--exists-p repo name)
      (ngit--die-with 3 "error: remote %s already exists." name))
    (unless (ngit-remote--valid-name-p name)
      (ngit--die "fatal: '%s' is not a valid remote name" name))
    (ngit-config-set repo (concat "remote." name ".url") url)
    (ngit-config-set repo (concat "remote." name ".fetch")
                     (concat "+refs/heads/*:refs/remotes/" name "/*"))))

(defun ngit-remote--remove (repo args)
  (unless (= (length args) 1) (ngit--die "usage: git remote remove <name>"))
  (let ((name (car args)))
    (unless (ngit-remote--exists-p repo name)
      (ngit--die-with 2 "error: No such remote: '%s'" name))
    (ngit-remote--replace-branch-links repo name nil)
    (ngit-remote--move-namespace repo name nil)
    (ngit-config-rename-section repo (concat "remote." name) nil)))

(defun ngit-remote--rename (repo args)
  (unless (= (length args) 2) (ngit--die "usage: git remote rename <old> <new>"))
  (let ((old (car args)) (new (cadr args)))
    (unless (ngit-remote--exists-p repo old)
      (ngit--die-with 2 "error: No such remote: '%s'" old))
    (when (ngit-remote--exists-p repo new)
      (ngit--die-with 3 "error: remote %s already exists." new))
    (unless (ngit-remote--valid-name-p new)
      (ngit--die "fatal: '%s' is not a valid remote name" new))
    (ngit-config-rename-section repo (concat "remote." old) (concat "remote." new))
    (let* ((key (concat "remote." new ".fetch"))
           (from (concat ":refs/remotes/" old "/"))
           (to (concat ":refs/remotes/" new "/"))
           (vals (mapcar (lambda (v) (replace-regexp-in-string
                                      (regexp-quote from) to v t t))
                         (ngit-config-get-all repo key))))
      (ngit-remote--set-all repo key vals))
    (ngit-remote--replace-branch-links repo old new)
    (ngit-remote--move-namespace repo old new)))

(defun ngit-remote--get-url (repo args)
  (let ((push nil) (all nil) positional)
    (dolist (a args)
      (cond ((string= a "--push") (setq push t))
            ((string= a "--all") (setq all t))
            (t (push a positional))))
    (setq positional (nreverse positional))
    (unless (= (length positional) 1) (ngit--die "usage: git remote get-url [--push] [--all] <name>"))
    (let ((name (car positional)))
      (unless (ngit-remote--exists-p repo name)
        (ngit--die-with 2 "error: No such remote '%s'" name))
      (let ((urls (ngit-remote--urls repo name push)))
        (dolist (url (if all urls (and urls (list (car urls)))))
          (ngit--emit (concat url "\n")))))))

(defun ngit-remote--set-url (repo args)
  (let ((push nil) (add nil) (delete nil) positional)
    (dolist (a args)
      (cond ((string= a "--push") (setq push t))
            ((string= a "--add") (setq add t))
            ((string= a "--delete") (setq delete t))
            (t (push a positional))))
    (setq positional (nreverse positional))
    (when (and add delete) (ngit--die "fatal: --add --delete doesn't make sense"))
    (unless (or (and (= (length positional) 2) (or add delete))
                (and (>= (length positional) 2) (<= (length positional) 3)))
      (ngit--die "usage: git remote set-url [--push] <name> <newurl> [<oldurl>]"))
    (let* ((name (nth 0 positional)) (new (nth 1 positional))
           (old (if delete new (nth 2 positional)))
           (key (concat "remote." name "." (if push "pushurl" "url"))))
      (unless (ngit-remote--exists-p repo name)
        (ngit--die-with 2 "error: No such remote '%s'" name))
      (let ((vals (ngit-config-get-all repo key)))
        (cond
         (add (ngit-config-set repo key new t))
         ((null old) (ngit-config-set repo key new))
         (t
          (let ((matched nil) out)
            (dolist (v vals)
              (if (string-match-p old v)
                  (progn (setq matched t) (unless delete (push new out)))
                (push v out)))
            (unless matched (ngit--die "fatal: No such URL found: %s" old))
            (setq out (nreverse out))
            (when (and delete (null out) (not push))
              (ngit--die "fatal: Will not delete all non-push URLs"))
            (ngit-remote--set-all repo key out))))))))

(defun ngit-cmd-remote (repo args)
  "Implement local `git remote' operations for REPO and ARGS."
  (let ((verbose nil))
    (when (and args (member (car args) '("-v" "--verbose")))
      (setq verbose t args (cdr args)))
    (if (null args)
        (dolist (name (ngit-remote--names repo))
          (if verbose
              (let ((fetch (car (ngit-remote--urls repo name nil)))
                    (push (car (ngit-remote--urls repo name t))))
                (when fetch (ngit--emit (format "%s\t%s (fetch)\n" name fetch)))
                (when push (ngit--emit (format "%s\t%s (push)\n" name push))))
            (ngit--emit (concat name "\n"))))
      (let ((sub (pop args)))
        (cond ((string= sub "add") (ngit-remote--add repo args))
              ((member sub '("remove" "rm")) (ngit-remote--remove repo args))
              ((string= sub "rename") (ngit-remote--rename repo args))
              ((string= sub "get-url") (ngit-remote--get-url repo args))
              ((string= sub "set-url") (ngit-remote--set-url repo args))
              (t (ngit--die "error: unknown subcommand: `%s'" sub)))))
    0))

(provide 'ngit-remote)

;;; ngit-remote.el ends here
