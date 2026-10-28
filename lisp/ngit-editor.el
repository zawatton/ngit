;;; ngit-editor.el --- message editing and identity fallbacks -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded only by commands which need an editor or identity/date fallbacks.
;; The child process uses the same descriptor and wait machinery as hooks.

;;; Code:

(defun ngit-editor-command (repo)
  "Resolve Git's editor precedence, preserving explicitly empty values."
  (or (getenv "GIT_EDITOR") (ngit-config-get repo "core.editor")
      (and (getenv "TERM") (not (equal (getenv "TERM") "dumb")) (getenv "VISUAL"))
      (getenv "EDITOR")
      (if (or (not (getenv "TERM")) (equal (getenv "TERM") "dumb"))
          (progn (ngit-native-write-stderr "error: Terminal is dumb, but EDITOR unset\n") nil)
        "vi")))

(defun ngit-editor-process (repo argv)
  "Run editor ARGV with inherited terminal descriptors; return (STATUS . ERRNO)."
  (if (and (fboundp 'ngit-windows-p) (ngit-windows-p))
      (let ((conn (ngit-win-process-spawn argv process-environment
                                           (or (ngit-repo-toplevel repo) default-directory)
                                           'inherit 'inherit nil)))
        (cons (ngit-win-process-close conn) nil))
  (let* ((program (or (and (string-search "/" (car argv)) (car argv))
                      (executable-find (car argv)) (car argv)))
         (av (ngit-transport--cstring-vector argv))
         (ev (ngit-transport--cstring-vector process-environment))
         (pv (ngit-transport--cstring-vector (list program)))
         (sv (ngit-transport--cstring-vector (append (list "/bin/sh" program) (cdr argv))))
         (cv (ngit-transport--cstring-vector (list (or (ngit-repo-toplevel repo) default-directory))))
         (pipe (ngit-transport--pipe))
         (memory (nl-ffi-memory-allocate 4))
         (addr (nl-ffi-memory-address memory))
         (pid nil))
    (unwind-protect
        (progn
          (syscall-direct 72 (cdr pipe) 2 1 0 0 0)
          (setq pid (syscall-direct 57 0 0 0 0 0 0))
          (cond
           ((= pid 0)
            (syscall-direct 3 (car pipe) 0 0 0 0 0)
            (let ((rc (syscall-direct 80 (ptr-read-u64 (cdr cv) 0) 0 0 0 0 0)))
              (when (= rc 0)
                (setq rc (syscall-direct 59 (ptr-read-u64 (cdr pv) 0) (cdr av) (cdr ev) 0 0 0)))
              (when (= rc -8)
                (setq rc (syscall-direct 59 (ptr-read-u64 (cdr sv) 0) (cdr sv) (cdr ev) 0 0 0)))
              (ptr-write-u32 addr 0 (- rc))
              (syscall-direct 1 (cdr pipe) addr 4 0 0 0)
              (syscall-direct 60 127 0 0 0 0 0)))
           ((< pid 0)
            (syscall-direct 3 (car pipe) 0 0 0 0 0)
            (ngit--die "fatal: fork failed (errno %d)" (- pid)))
           (t
            (syscall-direct 3 (cdr pipe) 0 0 0 0 0)
            (setcdr pipe -1)
            (let* ((conn (vector ngit-transport--tag pid -1 -1 nil nil (car pipe) nil))
                   (err (ngit-transport--exec-error conn))
                   (status (ngit-transport-close conn)))
              (cons status err)))))
      (when (>= (cdr pipe) 0) (syscall-direct 3 (cdr pipe) 0 0 0 0 0))
      (dolist (owner (list (car av) (car ev) (car pv) (car sv) (car cv) memory))
        (nl-ffi-memory-release owner))))))

(defun ngit-editor-run (repo file)
  "Run the configured editor on FILE; return its exit code."
  (let ((editor (ngit-editor-command repo)))
    (cond ((not editor) 1)
          ((equal editor ":") 0)
          ((equal editor "")
           (ngit-native-write-stderr "error: cannot run : No such file or directory\nerror: unable to start editor ''\n")
           1)
          (t
      (unless (featurep 'ngit-hook)
        (load (expand-file-name "ngit-hook.el" ngit-native--dir) nil t))
      (let* ((shell (string-match-p "[|&;<>()$`\\\"' \t\n*?\\[~#=%%]" editor))
             (argv (if (and shell (fboundp 'ngit-windows-p) (ngit-windows-p))
                       (progn
                         (when (member (getenv "NGIT_WINDOWS_SHELL") '(nil ""))
                           (ngit--die "fatal: Windows shell editors require NGIT_WINDOWS_SHELL"))
                         (ngit-win-process-shell
                          (concat editor " " (ngit-transport--shell-quote file))))
                     (if shell
                       (list "/bin/sh" "-c" (concat editor " \"$@\"") editor file)
                     (list editor file))))
             (result (ngit-editor-process repo argv))
             (status (car result)))
        (unless (= status 0)
          (ngit-native-write-stderr
           (if (cdr result)
               (format "error: cannot run %s: %s\nerror: unable to start editor '%s'\n"
                       editor (ngit-transport--errno-string (cdr result)) editor)
             (format "error: there was a problem with the editor '%s'\n" editor))))
        status)))))

(defvar ngit-editor-comment-character nil)

(defun ngit-editor-comment-settings (repo)
  "Return (COMMENT . AUTO) following config.c's ordered assignments."
  (let ((comment "#") auto)
    (dolist (entry (ngit-config-entries repo))
      (when (member (car entry) '("core.commentchar" "core.commentstring"))
        (if (and (stringp (cdr entry)) (equal (downcase (cdr entry)) "auto"))
            (setq auto t)
          (setq comment (cdr entry) auto nil))))
    (cons comment auto)))

(defun ngit-editor-comment (repo)
  (or ngit-editor-comment-character
      (car (ngit-editor-comment-settings repo))))

(defun ngit-editor-auto-comment (repo message)
  "Choose a comment character unused at the start of MESSAGE's lines."
  (when (cdr (ngit-editor-comment-settings repo))
    (let ((candidates (string-to-list "#;@!$%^&|:")))
      (when (string-search "#" message)
        (dolist (line (split-string message "[\n\r]"))
          (when (> (length line) 0) (setq candidates (delq (aref line 0) candidates)))))
      (unless candidates
        (ngit--die "fatal: unable to select a comment character that is not used\nin the current commit message"))
      (string (car candidates)))))

(defun ngit-editor-environment (overrides)
  "Apply OVERRIDES without leaving duplicate names in the exec environment."
  (let ((environment process-environment))
    (dolist (entry overrides)
      (let* ((eq (string-search "=" entry))
             (prefix (concat (if eq (substring entry 0 eq) entry) "=")))
        (setq environment (seq-remove (lambda (old) (string-prefix-p prefix old)) environment))
        (when eq (push entry environment))))
    environment))

(defun ngit-editor-cleanup-mode (repo mode editing)
  (let ((value (or mode (ngit-config-get repo "commit.cleanup") "default")))
    (unless (member value '("default" "strip" "whitespace" "verbatim" "scissors"))
      (ngit--die "fatal: Invalid cleanup mode %s" value))
    (if (equal value "default") (if editing "strip" "whitespace") value)))

(defun ngit-editor-clean (repo message mode editing)
  "Clean MESSAGE according to MODE, including Git's scissors marker."
  (let ((comment (ngit-editor-comment repo)))
    (when (and editing (equal mode "scissors"))
      (let* ((marker (concat comment " ------------------------ >8 ------------------------"))
             (start (if (string-prefix-p marker message) 0
                      (let ((hit (string-search (concat "\n" marker) message)))
                        (and hit (1+ hit))))))
        (when start (setq message (substring message 0 start)))))
    (when (equal mode "strip")
      (setq message (mapconcat #'identity
                              (seq-remove (lambda (line) (string-prefix-p comment line))
                                          (split-string message "\n")) "\n")))
    (if (equal mode "verbatim") message (ngit-stripspace message))))

(defun ngit-editor-status (repo &optional amend)
  "Return the status section printed by commit with verbose off."
  (let ((ngit--out nil) (status (ngit-status-collect repo)) lines)
    (when amend
      (let* ((head (ngit-resolve-ref repo "HEAD"))
             (parsed (and head (ngit-parse-commit (cdr (ngit-read-object repo (ngit-unhex head))))))
             (parent (and parsed (ngit-header parsed "parent")))
             (tree (and parent (ngit--commit-tree-of repo parent)))
             (staged (ngit-diff-tree-index repo tree)))
        (when (ngit--status-renames-p repo)
          (setq staged (ngit-diffcore-rename repo staged (ngit-config-int repo "status.renameLimit" nil))))
        (setq status (plist-put status :head parent)
              status (plist-put status :staged staged))))
    (ngit--status-long repo status)
    (dolist (line (split-string (apply #'concat (nreverse ngit--out)) "\n"))
      (unless (or (string-prefix-p "  (" line)
                  (string-prefix-p "no changes added" line)
                  (string-prefix-p "nothing " line))
        (push (if (equal line "No commits yet") "Initial commit" line) lines)))
    (setq lines (nreverse lines))
    (when (and lines (equal (car (last lines)) "")) (setq lines (butlast lines)))
    (mapconcat (lambda (line)
                 (concat (ngit-editor-comment repo)
                         (if (or (equal line "") (string-prefix-p "\t" line)) "" " ")
                         line "\n")) lines "")))

(defun ngit-editor-commit-template (repo message cleanup author committer amend &optional allow-empty-message)
  "Append commit's editable instructions and status to MESSAGE."
  (let* ((comment (ngit-editor-comment repo))
         (a (ngit--split-ident author)) (c (ngit--split-ident committer))
         (ident (unless (equal (car a) (car c))
                  (concat comment " Author:    " (car a) "\n")))
         (date (when amend
                 (concat comment " Date:      "
                         (ngit-format-date (nth 1 a) (nth 2 a)) "\n")))
         (committer-line (when ngit-commit-implicit-ident
                           (concat comment " Committer: " (car c) "\n"))))
    (concat message "\n"
            (if (equal cleanup "scissors")
                (concat comment " ------------------------ >8 ------------------------\n"
                        comment " Do not modify or remove the line above.\n"
                        comment " Everything below it will be ignored.\n")
              (concat comment " Please enter the commit message for your changes. Lines starting\n"
                      comment " with '" comment "' "
                      (if (equal cleanup "strip")
                          (if allow-empty-message "will be ignored.\n"
                            "will be ignored, and an empty message aborts the commit.\n")
                        (concat "will be kept; you may remove them yourself if you want to.\n"
                                (unless allow-empty-message
                                  (concat comment " An empty message aborts the commit.\n"))))))
            comment "\n" ident date committer-line
            (when (or ident date committer-line) (concat comment "\n"))
            (ngit-editor-status repo amend))))

(defun ngit-editor-ident-hint (who)
  (ngit-native-write-stderr
   (concat (if (equal who "AUTHOR") "Author" "Committer") " identity unknown\n\n"
           "*** Please tell me who you are.\n\nRun\n\n"
           "  git config --global user.email \"you@example.com\"\n"
           "  git config --global user.name \"Your Name\"\n\n"
           "to set your account's default identity.\n"
           "Omit --global to set the identity only in this repository.\n\n")))

(defun ngit-editor-merge-autoedit (messages)
  "Resolve merge's default editor flag, including the terminal default."
  (unless messages
    (let ((value (getenv "GIT_MERGE_AUTOEDIT")))
      (if value
          (cond ((member (downcase value) '("yes" "true" "on" "1")) t)
                ((member (downcase value) '("no" "false" "off" "0" "")) nil)
                (t (ngit--die "fatal: Bad value '%s' in environment 'GIT_MERGE_AUTOEDIT'" value)))
        (ngit-native--with-memory (memory 512)
          (let ((addr (nl-ffi-memory-address memory)))
            (and (= 0 (syscall-direct 16 0 #x5401 addr 0 0 0))
                 (= 0 (syscall-direct 16 1 #x5401 addr 0 0 0))
                 (= 0 (syscall-direct 5 0 addr 0 0 0 0))
                 (= 0 (syscall-direct 5 1 (+ addr 256) 0 0 0 0))
                 (equal (ptr-read-bytes addr 16) (ptr-read-bytes (+ addr 256) 16))
                 (equal (ptr-read-bytes (+ addr 24) 4) (ptr-read-bytes (+ addr 280) 4)))))))))

(defun ngit-editor-merge-template (repo message &optional cleanup)
  (let ((comment (ngit-editor-comment repo)))
    (concat message "\n"
            (when (equal cleanup "scissors")
              (concat comment " ------------------------ >8 ------------------------\n"
                      comment " Do not modify or remove the line above.\n"
                      comment " Everything below it will be ignored.\n" comment "\n"))
            comment " Please enter a commit message to explain why this merge is necessary,\n"
            comment " especially if it merges an updated upstream into a topic branch.\n"
            comment "\n"
            (if (equal cleanup "scissors") (concat comment " An empty message aborts the commit.\n")
              (concat comment " Lines starting with '" comment "' will be ignored, and an empty message aborts\n"
                      comment " the commit.\n")))))

(defun ngit-editor-passwd ()
  "Return the current account's passwd fields, independent of USER/LOGNAME."
  (let ((uid (number-to-string (syscall-direct 102 0 0 0 0 0 0))) found)
    (dolist (line (split-string (if (file-exists-p "/etc/passwd") (ngit--read-file "/etc/passwd") "") "\n" t))
      (let ((fields (split-string line ":")))
        (when (equal (nth 2 fields) uid) (setq found fields))))
    (or found '("unknown" "" "" "" "Unknown"))))

(defun ngit-editor-guess-ident (repo who name email)
  "Fill missing NAME and EMAIL using config-only policy and OS account data."
  (let ((only (ngit-config-bool repo "user.useconfigonly" nil))
        (pw nil))
    ;; Git checks email before name, and EMAIL does not bypass useConfigOnly.
    (unless email
      (when only
        (ngit-editor-ident-hint who)
        (ngit--die "fatal: no email was given and auto-detection is disabled"))
      (setq email (getenv "EMAIL"))
      (when (or (null email) (equal email ""))
        (setq pw (ngit-editor-passwd))
        (let* ((mailname (and (file-exists-p "/etc/mailname") (ngit--read-file "/etc/mailname")))
               (host (string-trim (or mailname
                                      (and (file-exists-p "/proc/sys/kernel/hostname")
                                           (ngit--read-file "/proc/sys/kernel/hostname")) "(none)"))))
          (unless (or mailname (string-search "." host))
            (setq host (concat host ".(none)")))
          (setq email (concat (car pw) "@" host))
          (when (string-suffix-p ".(none)" host)
            (ngit-editor-ident-hint who)
            (ngit--die "fatal: unable to auto-detect email address (got '%s')" email)))))
    (unless name
      (when only
        (ngit-editor-ident-hint who)
        (ngit--die "fatal: no name was given and auto-detection is disabled"))
      (unless pw (setq pw (ngit-editor-passwd)))
      (setq name (car (split-string (nth 4 pw) ",")))
      (setq name (replace-regexp-in-string
                  "&" (concat (upcase (substring (car pw) 0 1)) (substring (car pw) 1)) name t t)))
    (cons name email)))

(defun ngit-editor-ident-clean (value)
  (setq value (replace-regexp-in-string "\\`[[:space:],:;<>\"\\\\']+\\|[[:space:],:;<>\"\\\\']+\\'" "" value))
  (replace-regexp-in-string "[\n<>]" "" value))

(defun ngit-editor-date (value)
  "Parse Git's non-raw calendar date forms; reject relative identity dates."
  (let* ((case-fold-search t) (s value) (zone nil) (hour nil) (minute 0) (second 0)
         (year nil) (month nil) (day nil)
         (months '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")))
    (when (string-match "\\`\\([0-9]\\{4\\}\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\(.*\\)\\'" s)
      (setq s (concat (match-string 1 s) "-" (match-string 2 s) "-" (match-string 3 s)
                      "T" (match-string 4 s) ":" (match-string 5 s) ":" (match-string 6 s) (match-string 7 s))))
    (setq s (replace-regexp-in-string "[ \t]*([^)]*)" "" s))
    (when (string-match "\\([0-9][0-9]?\\):\\([0-9][0-9]\\)\\(?::\\([0-9][0-9]\\)\\(?:\\.[0-9]+\\)?\\)?" s)
      (setq hour (string-to-number (match-string 1 s))
            minute (string-to-number (match-string 2 s))
            second (string-to-number (or (match-string 3 s) "0"))
            s (concat (substring s 0 (match-beginning 0)) " " (substring s (match-end 0)))))
    (cond ((string-match "\\([-+]\\)\\([0-9][0-9]\\):?\\([0-9][0-9]\\)\\(?:[ )]*\\)\\'" s)
           (setq zone (* (if (equal (match-string 1 s) "-") -1 1)
                         (+ (* 3600 (string-to-number (match-string 2 s)))
                            (* 60 (string-to-number (match-string 3 s)))))
                 s (substring s 0 (match-beginning 0))))
          ((string-match "\\([A-Za-z]+\\) *\\'" s)
           (let ((entry (assoc (upcase (match-string 1 s))
                               '(("Z" . 0) ("UTC" . 0) ("GMT" . 0) ("WET" . 0)
                                 ("IDLE" . 12) ("NZST" . 12) ("NZT" . 12) ("NZDT" . 13)
                                 ("GST" . 10) ("EAST" . 10) ("EADT" . 11) ("JST" . 9)
                                 ("CCT" . 8) ("WAST" . 7) ("WADT" . 8)
                                 ("EET" . 2) ("EEST" . 3) ("CET" . 1) ("CEST" . 2)
                                 ("MET" . 1) ("MEWT" . 1) ("MEST" . 2) ("MESZ" . 2)
                                 ("FWT" . 1) ("FST" . 2) ("BST" . 1) ("WAT" . -1)
                                 ("AST" . -3) ("ADT" . -2) ("EST" . -5) ("EDT" . -4)
                                 ("CST" . -6) ("CDT" . -5) ("MST" . -7) ("MDT" . -6)
                                 ("PST" . -8) ("PDT" . -7) ("YST" . -9) ("YDT" . -8)
                                 ("HST" . -10) ("HDT" . -9) ("CAT" . -10) ("AHST" . -10)
                                 ("NT" . -11) ("IDLW" . -12)))))
             (when entry (setq zone (* 3600 (cdr entry)) s (substring s 0 (match-beginning 0)))))))
    (cond
     ((string-match "\\([0-9]\\{4\\}\\)[-./]\\([0-9][0-9]?\\)[-./]\\([0-9][0-9]?\\)" s)
      (setq year (string-to-number (match-string 1 s)) month (string-to-number (match-string 2 s)) day (string-to-number (match-string 3 s))))
     ((string-match "\\([0-9][0-9]?\\)/\\([0-9][0-9]?\\)/\\([0-9]\\{4\\}\\)" s)
      (setq month (string-to-number (match-string 1 s)) day (string-to-number (match-string 2 s)) year (string-to-number (match-string 3 s))))
     (t
      (let ((tokens (split-string (downcase s) "[^a-z0-9]+" t)) (i 0) (numbers nil))
        (dolist (token tokens)
          (if (member (substring token 0 (min 3 (length token))) months)
              (setq month (1+ (- (length months) (length (member (substring token 0 3) months)))))
            (when (string-match-p "\\`[0-9]+\\'" token) (push (string-to-number token) numbers)))
          (setq i (1+ i)))
        (dolist (number (nreverse numbers))
          (if (> number 31) (setq year (if (< number 100) (+ number (if (< number 70) 2000 1900)) number))
            (unless day (setq day number)))))))
    (unless (and year month day hour (<= 1 month 12) (<= 1 day 31)
                 (<= 0 hour 23) (<= 0 minute 59) (<= 0 second 59))
      (ngit--die "fatal: invalid date format: %s" value))
    (let* ((local (+ (* 86400 (ngit--days-from-civil year month day))
                     (* 3600 hour) (* 60 minute) second))
           (off (or zone (ngit-local-offset (- local (ngit-local-offset local))))))
      (concat (number-to-string (- local off)) " " (ngit--tz-string off)))))

(defun ngit-editor-path-commit (repo paths include thunk)
  "Run THUNK with the commit index for PATHS, publishing the real index on success."
  (let* ((saved (getenv "GIT_INDEX_FILE"))
         (index-file (ngit-index-file repo))
         (index (ngit-read-index repo)) (old (aref index 0))
         (head (ngit-resolve-ref repo "HEAD"))
         (head-entries nil) (candidates nil)
         (items (ngit-pathspec-mixed-items repo paths (ngit-pathspec-items repo paths)))
         (filemode (ngit-config-bool repo "core.filemode" t))
         (selected nil) (updates nil) (real nil) (partial nil)
         (temp (expand-file-name (format "next-index-%s" (emacs-pid)) (ngit-repo-gitdir repo)))
         (lock nil) (status nil))
    (when (or (ngit-resolve-ref repo "MERGE_HEAD")
              (file-exists-p (expand-file-name "MERGE_HEAD" (ngit-repo-gitdir repo))))
      (unless include (ngit--die "fatal: cannot do a partial commit during a merge.")))
    (unless include
      (when (ngit-resolve-ref repo "CHERRY_PICK_HEAD")
        (ngit--die "fatal: cannot do a partial commit during a cherry-pick.")))
    (when head
      (ngit--flatten-tree
       repo (ngit--commit-tree-of repo head) ""
       (lambda (path mode bin)
         (push (ngit-make-entry path bin mode
                                (concat (make-string 24 0) (ngit--be32-bytes mode) (make-string 12 0)))
               head-entries)
         (unless include (push path candidates)))))
    (dotimes (i (length old)) (push (aref (aref old i) 0) candidates))
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (setq selected
          (condition-case err
              (ngit-pathspec-expand repo items (sort (delete-dups candidates) #'string<) t)
            (ngit-fatal
             (if include (ngit--die "%s" (cadr err)) (signal (car err) (cdr err))))))
    (dolist (path selected)
      (let* ((entry (or (ngit-index-lookup old path)
                        (seq-find (lambda (e) (equal (aref e 0) path)) head-entries)))
             (st (ngit-wt-lstat path)))
        (cond ((and entry (memq 'skip-worktree (aref entry 5))) (push entry updates))
              ((and (not include) st (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir))
               (let ((head (ngit--gitlink-head repo path)))
                 (cond (head (push (ngit-make-entry path head ngit-mode-gitlink
                                                   (ngit-wt-stat-block ngit-mode-gitlink)) updates))
                       ((and entry (= (aref entry 2) ngit-mode-gitlink)) (push entry updates))
                       (t (ngit--die "error: '%s' does not have a commit checked out\nfatal: updating files failed" path)))))
              ((and st (or (/= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir)
                           (and entry (= (aref entry 2) ngit-mode-gitlink))))
               (push (ngit--new-entry repo path st filemode (and entry (aref entry 2)) nil) updates)))))
    (setq real (vconcat (sort (append (seq-remove (lambda (e) (member (aref e 0) selected))
                                                (append old nil)) (copy-sequence updates)) #'ngit--entry-less))
          partial (if include real
                    (vconcat (sort (append (seq-remove (lambda (e) (member (aref e 0) selected)) head-entries)
                                          (copy-sequence updates)) #'ngit--entry-less))))
    (unwind-protect
        (progn
          (setq lock (ngit-lock index-file))
          (ngit-write-file lock (ngit-index-bytes repo index real nil))
          (ngit-write-file temp (ngit-index-bytes repo index partial nil))
          (setenv "GIT_INDEX_FILE" temp)
          (aset repo 6 nil)
          (let ((ngit-commit-index-publish
                 (lambda ()
                   (when include (ngit-write-file lock (ngit--read-file temp)))
                   (ngit-rename-file lock index-file t)
                   (setq lock nil)
                   (setenv "GIT_INDEX_FILE" saved)
                   (aset repo 6 nil)
                   index-file)))
            (setq status (funcall thunk)))
          status)
      (setenv "GIT_INDEX_FILE" saved)
      (aset repo 6 nil)
      (when lock (ngit-unlock lock))
      (when (file-exists-p temp) (ngit-delete-file temp))
      (when (file-exists-p (concat temp ".lock")) (ngit-delete-file (concat temp ".lock"))))))

(provide 'ngit-editor)
;;; ngit-editor.el ends here
