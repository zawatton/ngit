;;; ngit-worktree-cmd.el --- linked worktree command -*- lexical-binding: t; -*-

;;; Commentary:

;; Implements the core linked-worktree operations.  Administrative files are
;; written in the same form as Git's files backend; checkout itself reuses the
;; existing hard-reset path so index encoding and work-tree filters stay
;; consistent with the rest of ngit.

;;; Code:

(require 'ngit-switch)

(defun ngit-worktree-cmd--main-path (repo)
  (directory-file-name
   (file-name-directory (directory-file-name (ngit-repo-commondir repo)))))

(defun ngit-worktree-cmd--admin-root (repo)
  (expand-file-name "worktrees" (ngit-repo-commondir repo)))

(defun ngit-worktree-cmd--linked-records (repo)
  "Return linked worktree records [PATH ADMIN ID RAW-HEAD HEX]."
  (let ((root (ngit-worktree-cmd--admin-root repo)) (records nil))
    (when (file-directory-p root)
      (dolist (id (directory-files root nil "\\`[^.]"))
        (let* ((admin (file-name-as-directory (expand-file-name id root)))
               (gitdir-file (expand-file-name "gitdir" admin))
               (gitfile (and (file-readable-p gitdir-file)
                             (ngit--trim-eol (ngit--read-file gitdir-file))))
               (path (and gitfile
                          (directory-file-name (file-name-directory gitfile))))
               (head-file (expand-file-name "HEAD" admin))
               (raw (and (file-readable-p head-file)
                         (ngit--trim-eol (ngit--read-file head-file))))
               (view (vector admin (ngit-repo-commondir repo) path nil nil nil nil nil))
               (hex (and raw (ngit-resolve-ref view "HEAD"))))
          (when path (push (vector path admin id raw hex) records)))))
    (sort records (lambda (a b) (string< (aref a 0) (aref b 0))))))

(defun ngit-worktree-cmd--list (repo args)
  (unless (or (null args) (equal args '("--porcelain")))
    (ngit--die-with 129 "error: unsupported worktree list option"))
  (let* ((main (ngit-worktree-cmd--main-path repo))
         (main-view (vector (ngit-repo-commondir repo)
                            (ngit-repo-commondir repo) main
                            nil nil nil nil nil))
         (head (ngit-resolve-ref main-view "HEAD"))
         (raw (ngit-read-ref main-view "HEAD"))
         (records (ngit-worktree-cmd--linked-records repo))
         (items (list (vector main raw head))))
    (dolist (record records)
      (setq items (append items
                          (list (vector (aref record 0) (aref record 3)
                                        (aref record 4))))))
    (if (member "--porcelain" args)
        (dolist (item items)
          (ngit--emit "worktree " (aref item 0) "\n")
          (when (aref item 2) (ngit--emit "HEAD " (aref item 2) "\n"))
          (if (and (aref item 1) (string-prefix-p "ref: " (aref item 1)))
              (ngit--emit "branch " (substring (aref item 1) 5) "\n\n")
            (ngit--emit "detached\n\n")))
      (let ((path-width 0) (abbrev-width 0) (shown nil))
        (dolist (item items)
          (let ((abbr (and (aref item 2) (ngit-abbrev repo (aref item 2)))))
            (push (cons item (or abbr (make-string 7 ?0))) shown)
            (setq path-width (max path-width (length (aref item 0)))
                  abbrev-width (max abbrev-width (length (or abbr ""))))))
        (dolist (pair (nreverse shown))
          (let* ((item (car pair)) (abbr (cdr pair)) (raw (aref item 1))
                 (label (if (and raw (string-prefix-p "ref: refs/heads/" raw))
                            (concat "[" (substring raw 16) "]")
                          "(detached HEAD)")))
            (ngit--emit (aref item 0)
                        (make-string (+ 2 (- path-width (length (aref item 0)))) ?\s)
                        abbr
                        (make-string (1+ (- abbrev-width (length abbr))) ?\s)
                        label "\n")))))
  0))

(defun ngit-worktree-cmd--sanitize-id (path)
  (let* ((base (file-name-nondirectory (directory-file-name path)))
         (out nil))
    (dotimes (i (length base))
      (let ((c (aref base i)))
        (push (if (or (and (>= c ?a) (<= c ?z))
                      (and (>= c ?A) (<= c ?Z))
                      (and (>= c ?0) (<= c ?9))
                      (memq c '(?- ?_ ?.)))
                  c ?-)
              out)))
    (let ((name (apply #'string (nreverse out))))
      (if (string= name "") "worktree" name))))

(defun ngit-worktree-cmd--unused-id (repo base)
  (let ((root (ngit-worktree-cmd--admin-root repo))
        (name base) (counter 0))
    (while (file-exists-p (expand-file-name name root))
      (setq counter (1+ counter) name (concat base (number-to-string counter))))
    name))

(defun ngit-worktree-cmd--directory-empty-p (path)
  (and (file-directory-p path)
       (null (delq nil
                   (mapcar (lambda (name)
                             (unless (member name '("." "..")) name))
                           (directory-files path nil nil t))))))

(defun ngit-worktree-cmd--initial-head-log (repo hex ident)
  (let ((file (expand-file-name "logs/HEAD" (ngit-repo-gitdir repo))))
    (ngit-make-directory (file-name-directory file) t)
    (ngit-write-file file (concat (make-string 40 ?0) " " hex " " ident "\n"))))

(defun ngit-worktree-cmd--write-initial-head (hook-repo view value)
  "Write VIEW's initial HEAD while hooks run in HOOK-REPO's context."
  (let* ((file (ngit--ref-file view "HEAD"))
         (new (if (string-prefix-p "ref: " value)
                  (concat "ref:" (substring value 5)) value))
         (lock (progn (ngit-make-directory (file-name-directory file) t)
                      (ngit-lock file)))
         input)
    (unwind-protect
        (progn
          (setq input (ngit-ref-prepare
                       hook-repo (list (list nil new "HEAD")) nil (list lock)))
          (ngit-commit-lock lock (concat value "\n"))
          (setq lock nil)
          (aset view 4 nil)
          (ngit-ref-finish hook-repo input))
      (when lock (ngit-unlock lock)))))

(defun ngit-worktree-cmd--create-branch (repo branch start head-name)
  "Create BRANCH as worktree-add's child branch command does."
  (ngit-native-write-stderr (format "Preparing worktree (new branch '%s')\n" branch))
  (condition-case err
      (ngit--branch-create repo branch start nil nil 'default head-name)
    (ngit-fatal
     (if (equal (cadr err) "fatal: ref updates aborted by hook")
         (ngit--die-with 255 "%s" (cadr err))
       (signal (car err) (cdr err))))))

(defun ngit-worktree-cmd--add (repo args)
  (let ((new-branch nil) (detach nil) (positionals nil) (created-branch nil))
    (while args
      (let ((arg (pop args)))
        (cond ((member arg '("-b" "--branch"))
               (unless args (ngit--die-with 129 "error: option `b' requires a value"))
               (setq new-branch (pop args)))
              ((and (string-prefix-p "-b" arg) (> (length arg) 2))
               (setq new-branch (substring arg 2)))
              ((member arg '("-d" "--detach")) (setq detach t))
              ((string= arg "--") (setq positionals (append (nreverse positionals) args)
                                          args nil))
              ((string-prefix-p "-" arg)
               (ngit--die-with 129 "error: unknown option `%s'" arg))
              (t (push arg positionals)))))
    (setq positionals (nreverse positionals))
    (unless (memq (length positionals) '(1 2))
      (ngit--die-with 129 "error: worktree add needs a path and optional commit-ish"))
    (when (and new-branch detach)
      (ngit--die "fatal: options '-b' and '--detach' cannot be used together"))
    (let* ((path-arg (car positionals))
           (start (or (cadr positionals) "HEAD"))
           (path (directory-file-name (expand-file-name path-arg default-directory)))
           (head-name (let ((raw (ngit-read-ref repo "HEAD")))
                        (and raw (string-prefix-p "ref: refs/heads/" raw)
                             (substring raw 16))))
           (hex (condition-case nil
                    (ngit-peel repo (ngit-rev-parse repo start) 'commit)
                  (error nil)))
           (branch nil) (raw-head nil))
      (unless hex (ngit--die "fatal: invalid reference: %s" start))
      (cond
       (new-branch
        (ngit-worktree-cmd--create-branch repo new-branch start head-name)
        (setq created-branch t)
        (setq branch new-branch raw-head (concat "ref: refs/heads/" branch)))
       (detach (setq raw-head hex))
       (t
        (let ((dwim (ngit-dwim-ref repo start)))
          (if (and dwim (string-prefix-p "refs/heads/" (car dwim)))
              (setq branch (substring (car dwim) 11)
                    raw-head (concat "ref: " (car dwim)))
            (when (string= start "HEAD")
              (setq branch (ngit-worktree-cmd--sanitize-id path))
              (ngit-worktree-cmd--create-branch repo branch start head-name)
              (setq created-branch t)
              (setq raw-head (concat "ref: refs/heads/" branch)))
            (unless raw-head (setq raw-head hex))))))
      (when branch
        (let ((used (ngit--branch-checked-out repo (concat "refs/heads/" branch))))
          (when used
            (ngit--die "fatal: '%s' is already used by worktree at '%s'" branch used))))
      (when (and (file-exists-p path) (not (ngit-worktree-cmd--directory-empty-p path)))
        (ngit--die "fatal: '%s' already exists" path-arg))
      (let* ((id (ngit-worktree-cmd--unused-id
                  repo (ngit-worktree-cmd--sanitize-id path)))
             (admin (file-name-as-directory
                     (expand-file-name id (ngit-worktree-cmd--admin-root repo))))
             (gitfile (expand-file-name ".git" path))
             (ident (ngit-ident repo "COMMITTER"))
             (keep-worktree nil))
        (unless created-branch
          (ngit-native-write-stderr
           (format (if branch "Preparing worktree (checking out '%s')\n"
                     "Preparing worktree (detached HEAD %s)\n")
                   (or branch (ngit-abbrev repo hex)))))
        (ngit-make-directory admin t)
        (ngit-write-file (expand-file-name "locked" admin) "initializing")
        (ngit-make-directory path t)
        (ngit-make-directory (expand-file-name "refs" admin) t)
        (ngit-write-file (expand-file-name "gitdir" admin) (concat gitfile "\n"))
        (ngit-write-file gitfile
                         (concat "gitdir: "
                                 (directory-file-name (ngit-repo-commondir repo))
                                 "/worktrees/" id "\n"))
        (ngit-write-file (expand-file-name "commondir" admin) "../..\n")
        (unwind-protect
            (let* ((view (ngit-discover path))
                   (tree (ngit--switch-tree repo hex)))
              (ngit-worktree-cmd--write-initial-head repo view raw-head)
              (ngit-worktree-cmd--initial-head-log view hex ident)
              (ngit--reset-hard view tree "HEAD")
              (let ((ngit-ref-hook-env '("GIT_WORK_TREE=.")))
                (ngit-ref-write view "ORIG_HEAD" hex)
                (ngit-update-head view hex hex ident "reset: moving to HEAD")
                ;; reset_head clears AUTO_MERGE in both sequencer and merge cleanup.
                (ngit-ref-delete-state view "AUTO_MERGE")
                (ngit-ref-delete-state view "AUTO_MERGE"))
              (let ((subject (ngit--subject-of repo hex)))
                (ngit--emit "HEAD is now at " (ngit-abbrev repo hex)
                            (if (string= subject "") ""
                              (concat " " (decode-coding-string subject 'utf-8)))
                            "\n"))
              (delete-file (expand-file-name "locked" admin))
              (setq keep-worktree t)
              (ngit-hook-run view "post-checkout"
                             (list (make-string 40 ?0) hex "1") nil
                             '("GIT_DIR" "GIT_WORK_TREE")))
          (unless keep-worktree
            (dolist (directory (list path admin))
              (when (file-directory-p directory)
                (condition-case nil
                    (ngit-worktree-cmd--delete-directory directory)
                  (error nil))))))))))

(defun ngit-worktree-cmd--delete-directory (directory)
  (dolist (path (directory-files directory t nil t))
    (unless (member (file-name-nondirectory path) '("." ".."))
      (cond ((file-symlink-p path) (delete-file path))
            ((file-directory-p path) (ngit-worktree-cmd--delete-directory path))
            (t (delete-file path)))))
  (delete-directory directory))

(defun ngit-worktree-cmd--find (repo arg)
  (let ((absolute (directory-file-name (expand-file-name arg default-directory)))
        (found nil))
    (dolist (record (ngit-worktree-cmd--linked-records repo))
      (when (and (not found)
                 (or (string= absolute (aref record 0))
                     (string= arg (aref record 2))))
        (setq found record)))
    found))

(defun ngit-worktree-cmd--validate-removal (repo record)
  "Check the gitfile backlink before deleting RECORD's worktree."
  (let* ((path (aref record 0)) (admin (aref record 1))
         (gitfile (expand-file-name ".git" path)))
    (when (file-exists-p path)
      (unless (file-exists-p gitfile)
        (ngit--die "fatal: validation failed, cannot remove working tree: '%s' does not exist" gitfile))
      (let* ((data (and (file-regular-p gitfile) (ngit--read-file gitfile)))
             (target (and data (string-prefix-p "gitdir: " data)
                          (string-trim (substring data 8))))
             (resolved (and target (not (string= target ""))
                            (expand-file-name target path)))
             (code (cond ((not data) 2)
                         ((not target) 5)
                         ((not resolved) 6)
                         ((not (file-exists-p (expand-file-name "HEAD" resolved))) 7))))
        (when code
          (ngit--die "fatal: validation failed, cannot remove working tree: '%s' is not a .git file, error code %d"
                     gitfile code))
        (unless (string= (directory-file-name (file-truename resolved))
                         (directory-file-name (file-truename admin)))
          (ngit--die "fatal: validation failed, cannot remove working tree: '%s' does not point back to '%s'"
                     path (if (string= (ngit-repo-gitdir repo) (ngit-repo-commondir repo))
                              (directory-file-name (file-relative-name admin default-directory))
                            (directory-file-name admin))))))))

(defun ngit-worktree-cmd--remove (repo args)
  (let ((force 0) (path nil))
    (dolist (arg args)
      (cond ((member arg '("-f" "--force")) (setq force (1+ force)))
            ((string= arg "-ff") (setq force (+ force 2)))
            ((string-prefix-p "-" arg)
             (ngit--die-with 129 "error: unknown option `%s'" arg))
            (path (ngit--die-with 129 "error: worktree remove needs one path"))
            (t (setq path arg))))
    (unless path (ngit--die-with 129 "error: worktree remove needs one path"))
    (let ((record (ngit-worktree-cmd--find repo path)))
      (unless record (ngit--die "fatal: '%s' is not a working tree" path))
      (let ((locked (expand-file-name "locked" (aref record 1))))
        (when (and (< force 2) (file-exists-p locked))
          (let ((reason (string-trim (ngit--read-file locked))))
            (ngit--die "fatal: cannot remove a locked working tree%s\nuse 'remove -f -f' to override or unlock first"
                       (if (string= reason "") ";" (concat ", lock reason: " reason))))))
      (ngit-worktree-cmd--validate-removal repo record)
      (when (and (file-exists-p (aref record 0)) (= force 0))
        (let ((view (ngit-discover (aref record 0))))
          (when (ngit-repo-dirtiness view)
            (ngit--die
             "fatal: '%s' contains modified or untracked files, use --force to delete it"
             path))))
      (when (file-exists-p (aref record 0))
        (ngit-worktree-cmd--delete-directory (aref record 0)))
      (ngit-worktree-cmd--delete-directory (aref record 1))
      (let ((root (ngit-worktree-cmd--admin-root repo)))
        (when (and (file-directory-p root)
                   (null (directory-files root nil "\\`[^.]")))
          (delete-directory root)))
      0)))

(defun ngit-worktree-cmd--prune (repo args)
  (when args (ngit--die-with 129 "error: worktree prune options are not supported"))
  (let ((root (ngit-worktree-cmd--admin-root repo)))
    (when (file-directory-p root)
      (dolist (id (directory-files root nil "\\`[^.]"))
        (let* ((admin (expand-file-name id root))
               (locked (expand-file-name "locked" admin))
               (gitfile (expand-file-name "gitdir" admin))
               (target-git (and (file-regular-p gitfile)
                                (ngit--trim-eol (ngit--read-file gitfile)))))
          (cond ((or (file-symlink-p admin) (not (file-directory-p admin)))
                 (delete-file admin))
                ((and (not (file-exists-p locked))
                      (or (not target-git) (string= target-git "")
                          (not (file-exists-p target-git))))
                 (ngit-worktree-cmd--delete-directory admin))))))
    (when (and (file-directory-p root)
               (null (directory-files root nil "\\`[^.]")))
      (delete-directory root)))
  0)

;;;###autoload
(defun ngit-cmd-worktree (repo args)
  "Implement worktree add/list/remove/prune."
  (let ((sub (pop args)))
    (cond ((string= sub "add") (ngit-worktree-cmd--add repo args))
          ((string= sub "list") (ngit-worktree-cmd--list repo args))
          ((string= sub "remove") (ngit-worktree-cmd--remove repo args))
          ((string= sub "prune") (ngit-worktree-cmd--prune repo args))
          (t (ngit--die-with 129 "error: unsupported worktree subcommand: %s" sub)))))

(provide 'ngit-worktree-cmd)

;;; ngit-worktree-cmd.el ends here
