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
(defvar ngit--config-origins nil "Optional entry-identity to origin table.")
(defvar ngit--config-sources nil "Files selected for the current complete read.")
(defvar ngit--config-parameters nil "Command-scope entries for the current read.")
(defvar ngit--config-remote-urls 'unread)
(defvar ngit--config-collecting nil)
(defvar ngit--config-forbid-remote nil)
(defvar ngit--config-entry-callback nil)
(defvar ngit--config-cli-parameters nil)
(defvar ngit--config-parameters-quiet nil)
(defvar ngit--config-early nil "Non-nil for Git's pager config read without refs/worktree.")
(defvar ngit--config-input-lines nil)
(defvar ngit--config-source-kind 'file)
(defvar ngit--config-refs-repo nil)

(defun ngit--config-bad-line ()
  (if (eq ngit--config-source-kind 'stdin)
      (ngit--die "fatal: bad config line %d in standard input" ngit--config-line)
    (ngit--die "fatal: bad config line %d in file %s"
               ngit--config-line ngit--config-source)))

(defun ngit--config-parse-integer (value)
  "Parse Git's base-zero signed int VALUE with an optional K/M/G suffix.
Return (NUMBER . nil) or (nil . invalid/range).  Git uses symmetric bounds."
  (if (not (and (stringp value)
                (string-match "\\`[ \t\n\r\f\013]*\\([-+]?\\)\\(0[xX][0-9a-fA-F]+\\|0[0-7]*\\|[1-9][0-9]*\\)\\(\\(?:.\\|\n\\)*\\)\\'" value)))
      (cons nil 'invalid)
    (let* ((sign (if (string= (match-string 1 value) "-") -1 1))
           (literal (downcase (match-string 2 value)))
           (scale (cdr (assoc (downcase (match-string 3 value))
                              '(("" . 1) ("k" . 1024) ("m" . 1048576) ("g" . 1073741824)))))
           (hex (string-prefix-p "0x" literal))
           (base (cond (hex 16) ((string-prefix-p "0" literal) 8) (t 10)))
           (digits (string-trim-left (if hex (substring literal 2) literal) "0+"))
           (bound (cond ((= base 16) "7fffffff") ((= base 8) "17777777777") (t "2147483647")))
           (wide-bound (cond ((= base 16) (if (< sign 0) "8000000000000000" "7fffffffffffffff"))
                             ((= base 8) (if (< sign 0) "1000000000000000000000" "777777777777777777777"))
                             (t (if (< sign 0) "9223372036854775808" "9223372036854775807")))))
      ;; strtoimax reports overflow before checking an unrecognized suffix.
      ;; Bound the text before conversion, including arbitrarily long inputs.
      (cond ((or (> (length digits) (length wide-bound))
                 (and (= (length digits) (length wide-bound)) (string< wide-bound digits)))
             (cons nil 'range))
            ((null scale) (cons nil 'invalid))
            ((or (> (length digits) (length bound))
                 (and (= (length digits) (length bound)) (string< bound digits)))
             (cons nil 'range))
            (t (let ((number (if (string= digits "") 0 (string-to-number digits base))))
                 (if (> number (/ 2147483647 scale)) (cons nil 'range)
                   (cons (* sign number scale) nil))))))))

(defun ngit--config-boolean-value (value key &optional environment)
  "Parse boolean VALUE for KEY, diagnosing ENVIRONMENT values as Git does."
  (cond ((eq value t) t)
        ((null value) nil)
        ((member (downcase value) '("true" "yes" "on")) t)
        ((member (downcase value) '("false" "no" "off" "")) nil)
        (t (let ((parsed (ngit--config-parse-integer value)))
             (if (null (cdr parsed)) (/= (car parsed) 0)
               (ngit--die "fatal: bad boolean %s value '%s' for '%s'"
                          (if environment "environment" "config") value key))))))

(defun ngit--config-files (repo)
  "Return the config files of REPO, lowest precedence first."
  (let ((home (getenv "HOME"))
        (xdg (getenv "XDG_CONFIG_HOME"))
        (global (getenv "GIT_CONFIG_GLOBAL"))
        (files nil))
    (unless (ngit--config-boolean-value (getenv "GIT_CONFIG_NOSYSTEM") "GIT_CONFIG_NOSYSTEM" t)
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
      (let* ((local (expand-file-name "config" (ngit-repo-commondir repo)))
             (entries (progn (ngit--config-startup repo)
                             ngit--config-startup-entries))
             (enabled nil))
        (push local files)
        (dolist (entry entries)
          (when (string= (car entry) "extensions.worktreeconfig")
            (setq enabled (ngit--config-boolean-value (cdr entry) "extensions.worktreeconfig"))))
        (when (and enabled (not ngit--config-early))
          (push (expand-file-name "config.worktree" (ngit-repo-gitdir repo)) files))))
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
        (lines (if (stringp text) (ngit--config-logical-lines text) text)))
    (while lines
     (let ((raw (pop lines)))
      (when (eq ngit--config-source-kind 'stdin) (setq ngit--config-input-lines lines))
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
         ((string-match "\\`\\([A-Za-z][-A-Za-z0-9]*\\)[ \t]*\\(=\\)?" line)
          (push (cons (concat (if section (concat section ".") "") (downcase (match-string 1 line)))
                      (if (match-beginning 2)
                          (ngit--config-value line (match-end 0))
                        t))
                entries))
         (ngit--config-source (ngit--config-bad-line)))
        (unless (eq before entries)
          (push (cons (car entries) ngit--config-line) ngit--config-locations)
          (when callback (funcall callback (car entries)))))
      ;; A hasconfig scan of stdin consumes the remainder of the same
      ;; stream, just as config_with_options reading stdin a second time.
      (when (eq ngit--config-source-kind 'stdin) (setq lines ngit--config-input-lines))))
    (nreverse entries)))

(defun ngit--config-condition (condition file repo)
  "Test an include CONDITION using FILE and the discovered REPO context."
  (unless (fboundp 'ngit-wildmatch) (require 'ngit-match))
  (cond
   ((string-prefix-p "hasconfig:remote.*.url:" condition)
    (if ngit--config-collecting t
      (when (eq ngit--config-remote-urls 'unread)
        (let ((ngit--config-collecting t)
              (ngit--config-forbid-remote nil)
              (ngit--config-origins nil)
              (ngit--config-entry-callback nil)
              (urls nil))
          ;; The second pass follows every recognized hasconfig include,
          ;; even those whose URL pattern will not match in the real pass.
          (dolist (entry (ngit--config-sequence repo t))
            (when (ngit--config-remote-url-p (car entry))
              (push (if (eq (cdr entry) t) "" (cdr entry)) urls)))
          (setq ngit--config-remote-urls (nreverse urls))))
      (let ((pattern (substring condition 23)) (matched nil))
        (dolist (url ngit--config-remote-urls)
          (when (and (not matched) (ngit-wildmatch pattern url t))
            (setq matched t)))
        matched)))
   ((and repo (not ngit--config-early) (string-prefix-p "onbranch:" condition))
    (unless (eq repo ngit--config-refs-repo)
      ;; Git initializes the ref backend here, which reads the repository's
      ;; config before the branch include is entered or its values emitted.
      (setq ngit--config-refs-repo repo)
      (let ((ngit--config-entry-callback nil) (ngit--config-origins nil))
        (ngit-config-entries repo)))
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
            ((and (string-prefix-p "./" pattern) (not file))
             (ngit-native-write-stderr "error: relative config include conditionals must come from files\n")
             (setq prefix -1))
            ((string-prefix-p "./" pattern)
             (let ((directory (file-name-directory (file-truename file))))
               (setq prefix (length directory)
                     pattern (concat directory (substring pattern 2)))))
            ((not (file-name-absolute-p pattern)) (setq pattern (concat "**/" pattern))))
      (when (string-suffix-p "/" pattern) (setq pattern (concat pattern "**")))
      (when fold (setq pattern (downcase pattern) gitdir (downcase gitdir)
                       physical (downcase physical)))
      (when (>= prefix 0)
       (let ((literal (substring pattern 0 prefix)) (glob (substring pattern prefix)))
        (or (and (string-prefix-p literal gitdir)
                 (ngit-wildmatch glob (substring gitdir prefix) t))
            (and (string-prefix-p literal physical)
                 (ngit-wildmatch glob (substring physical prefix) t)))))))))

(defun ngit--config-display-file (file repo)
  "Use Git's discovered local config spelling for diagnostics."
  (let ((gitdir (getenv "GIT_DIR")))
    (cond
     ((and repo (ngit-repo-toplevel repo) (not gitdir)
           (member (expand-file-name file)
                   (list (expand-file-name ".git/config" (ngit-repo-toplevel repo))
                         (expand-file-name ".git/config.worktree" (ngit-repo-toplevel repo)))))
      (concat ".git/" (file-name-nondirectory file)))
     ((and repo (not (ngit-repo-toplevel repo)) (not gitdir)
           (equal (directory-file-name (ngit-repo-gitdir repo))
                  (directory-file-name default-directory))
           (member file (list (expand-file-name "config" (ngit-repo-commondir repo))
                              (expand-file-name "config.worktree" (ngit-repo-gitdir repo)))))
      (file-name-nondirectory file))
     ((and repo gitdir (not (file-name-absolute-p gitdir))
           (member file (list (expand-file-name "config" (ngit-repo-gitdir repo))
                              (expand-file-name "config.worktree" (ngit-repo-gitdir repo)))))
      (concat (file-name-as-directory
               (let ((relative (ngit--hook-relative-path (expand-file-name gitdir) default-directory)))
                 (if (string= relative "") "." relative)))
              (file-name-nondirectory file)))
     (t file))))

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
(defvar ngit--config-startup-entries nil
  "Primary entries already parsed for repository setup, without includes.")
(defvar ngit--config-text-cache nil
  "Raw config text cached for one command; parsing retains per-read context.")

(defun ngit--config-file-text (file)
  "Read FILE once per command, sharing startup and include-scan bytes."
  (let ((cached (and ngit--config-text-cache (gethash file ngit--config-text-cache))))
    (or cached
        (let ((text (decode-coding-string (ngit--read-file file) 'utf-8)))
          (when ngit--config-text-cache (puthash file text ngit--config-text-cache))
          text))))

(defun ngit--config-startup (repo)
  "Validate REPO's primary syntax before command-specific include expansion.
Git's repository setup reads the local primary config without following includes."
  (when (and repo (not (eq repo ngit--config-startup-repo)))
    (let ((file (expand-file-name "config" (ngit-repo-commondir repo)))
          (ngit--config-entry-callback nil)
          (ngit--config-origins nil)
          (ngit--config-forbid-remote nil))
      (setq ngit--config-startup-entries (ngit--config-read file repo nil)))
    (setq ngit--config-startup-repo repo)))

(defun ngit--config-read (file repo includes &optional depth display)
  "Read FILE, expanding INCLUDES in place, to Git's maximum DEPTH of ten."
  (unless (eq file 'stdin)
    (setq display (or display (ngit--config-display-file file repo))
          file (expand-file-name file)))
  (when (or (eq file 'stdin)
            (ngit--config-readable file (or display (ngit--config-display-file file repo)) nil (null depth)))
    (let* ((stdin (eq file 'stdin))
           (display (if stdin "standard input" (or display (ngit--config-display-file file repo))))
           (ngit--config-source-kind (if stdin 'stdin 'file))
           (ngit--config-source display) (ngit--config-line 0)
           (ngit--config-locations nil)
           (depth (or depth 0)) (out nil))
      (ngit--config-parse
       (if stdin (ngit-config-input--lines ngit--config-input-lines)
         (ngit--config-file-text file))
       (lambda (entry)
        (when (and ngit--config-forbid-remote
                   (ngit--config-remote-url-p (car entry)))
          (ngit--die (concat "fatal: remote URLs cannot be configured in file directly or indirectly "
                             "included by includeIf.hasconfig:remote.*.url")))
        (when ngit--config-origins
          (puthash entry (if stdin "standard input:" (concat "file:" display)) ngit--config-origins))
        (push entry out)
        (when ngit--config-entry-callback (funcall ngit--config-entry-callback entry))
        (let* ((key (car entry))
               (condition (and (string-prefix-p "includeif." key)
                               (string-match "\\.[^.]*\\'" key)
                               (substring key 10 (match-beginning 0))))
               (matches (and includes condition (ngit--config-condition condition (unless stdin file) repo))))
          (when (and includes
                     (or (string= key "include.path")
                         (and matches (string-suffix-p ".path" key))))
            (unless (stringp (cdr entry))
              (ngit-native-write-stderr "error: missing value for 'include.path'\n")
              (ngit--config-bad-line))
            (when (and stdin (not (or (file-name-absolute-p (cdr entry))
                                     (string-prefix-p "~" (cdr entry)))))
              (ngit-native-write-stderr "error: relative config includes must come from files\n")
              (ngit--config-bad-line))
            (let* ((value (cdr entry))
                   (shown (cond ((string-prefix-p "~" value) (expand-file-name value))
                                ((file-name-absolute-p value) value)
                                (t (concat (file-name-directory display) value))))
                   (path (expand-file-name value (unless stdin (file-name-directory (expand-file-name file))))))
              (when (and (ngit--config-readable path shown) (>= depth 10))
                (ngit--die (concat "fatal: exceeded maximum include depth (10) while including\n\t%s\nfrom\n\t%s\n"
                                   "This might be due to circular includes.") shown display))
              (let ((ngit--config-forbid-remote
                     (or ngit--config-forbid-remote
                         (and ngit--config-collecting condition))))
                (dolist (included (ngit--config-read path repo t (1+ depth) shown))
                  (push included out))))))))
      (nreverse out))))

(defun ngit--config-remote-url-p (key)
  "Whether KEY has section remote, a subsection, and variable url."
  (and (> (length key) 10) (string-prefix-p "remote." key)
       (string-suffix-p ".url" key)))

(defun ngit--config-sequence (repo includes)
  "Read the selected files and command parameters in precedence order."
  (let (entries)
    (dolist (file ngit--config-sources)
      (setq entries (append entries (ngit--config-read file repo includes))))
    (dolist (entry ngit--config-parameters)
      (when ngit--config-origins (puthash entry "command line:" ngit--config-origins))
      (setq entries (append entries (list entry)))
      (when ngit--config-entry-callback (funcall ngit--config-entry-callback entry))
      (when (and includes
                 (let* ((key (car entry))
                        (condition (and (string-prefix-p "includeif." key)
                                        (string-match "\\.[^.]*\\'" key)
                                        (substring key 10 (match-beginning 0))))
                        (matches (and condition (ngit--config-condition condition nil repo))))
                   (or (string= key "include.path")
                       (and matches (string-suffix-p ".path" key)))))
        (unless (stringp (cdr entry))
          (ngit-config-parameters--error "missing value for 'include.path'"))
        (unless (or (file-name-absolute-p (cdr entry)) (string-prefix-p "~" (cdr entry)))
          (ngit-native-write-stderr "error: relative config includes must come from files\n")
          (ngit--die "fatal: unable to parse command-line config"))
        (let ((ngit--config-forbid-remote
               (or ngit--config-forbid-remote
                   (and ngit--config-collecting (string-prefix-p "includeif." (car entry))))))
          (setq entries (append entries (ngit--config-read
                                         (expand-file-name (cdr entry)) repo t 1
                                         (if (string-prefix-p "~" (cdr entry))
                                             (expand-file-name (cdr entry)) (cdr entry))))))))
    entries))

(defun ngit--config-load (repo includes &optional file display required)
  "Read one selected FILE or the complete config, including a lazy URL scan."
  (when (equal file "-") (setq file 'stdin))
  (let ((top (and repo (ngit-repo-toplevel repo))))
    (when (and display (stringp file) (not (file-name-absolute-p file)))
      (when (and top (string-prefix-p top default-directory))
        (setq display (concat (substring default-directory (length top)) display)))
      (setq file (expand-file-name file)))
   (let* ((default-directory (or top default-directory))
        (ngit--config-remote-urls 'unread)
        (ngit--config-collecting nil)
        (ngit--config-forbid-remote nil)
        (ngit--config-sources (if file (list file) (ngit--config-files repo)))
        (ngit--config-parameters (unless file (ngit--config-env-parameters)))
        (ngit--config-input-lines
         (when (eq file 'stdin)
           (require 'ngit-config-input)
           (ngit--config-logical-lines (ngit-config-input--read)))))
    (when (and required file (not (eq file 'stdin)))
      (ngit--config-readable (expand-file-name file) (or display file) t t))
    (if file (ngit--config-read file repo includes nil display)
      (ngit--config-sequence repo includes)))))

(defun ngit--config-env-parameters ()
  "Return command-scope config, loading its parser only when needed."
  (if (or (getenv "GIT_CONFIG_COUNT") (getenv "GIT_CONFIG_PARAMETERS"))
      (progn
        (require 'ngit-config-parameters)
        (append (ngit-config-parameters--environment) ngit--config-cli-parameters))
    ngit--config-cli-parameters))

(defconst ngit--config-default-commands
  '("add" "am" "apply" "blame" "branch" "cat-file" "check-attr" "checkout" "cherry-pick"
    "clean" "clone" "commit" "count-objects" "describe" "diff" "fetch" "format-patch" "fsck"
    "gc" "grep" "hash-object" "index-pack" "init" "log" "ls-files" "ls-tree" "merge"
    "merge-file" "mv" "name-rev" "pack-objects" "pack-refs" "pull" "push" "rebase" "reflog" "repack"
    "reset" "restore" "rev-list" "rev-parse" "revert" "rm" "shortlog" "show" "stash" "status"
    "switch" "symbolic-ref" "tag" "update-index" "verify-pack" "worktree" "write-tree")
  "Commands proven to consume Git's default configuration callback.")

(defconst ngit--config-diff-commands
  '("cherry-pick" "commit" "diff" "format-patch" "log" "merge" "reflog" "revert" "show"
    "stash" "status")
  "Commands proven to consume diff.renameLimit through a config callback.")

(defvar ngit--config-validation-command nil)
(defvar ngit--config-validated-cache 'unvalidated)

(defun ngit--config-command-parameters (repo)
  "Read parameters quietly first; retry in source order on a parse error."
  (let (parameters failure)
    (let ((ngit--config-parameters-quiet t))
      (condition-case parameter-error
          (setq parameters (ngit--config-env-parameters))
        (ngit-fatal (setq failure parameter-error))))
    (when failure
      ;; Repository setup syntax precedes environment-parser diagnostics.
      ;; The retry emits the environment error once if no earlier source fails.
      (ngit-config-entries repo)
      (signal (car failure) (cdr failure)))
    parameters))

(defun ngit--config-main (original args)
  "Handle global -c options before ORIGINAL dispatches the command."
  (condition-case err
   (let ((ngit--config-cli-parameters nil) (prefix nil)
         (ngit--config-text-cache (make-hash-table :test 'equal)))
    (while (member (car args) '("-c" "-C"))
      (let ((option (pop args)) (value (pop args)))
        (if (string= option "-C")
            (setq prefix (append prefix (list option value)))
          (require 'ngit-config-parameters)
          (unless value (ngit-config-parameters--missing-argument))
          (ngit-config-parameters--push value))))
    (let* ((command (car args))
           (handler (cdr (assoc command ngit--commands)))
           (ngit--commands
            (if (and handler (member command ngit--config-default-commands))
                (cons (cons command
                            (lambda (repo arguments)
                              (let ((ngit--config-validation-command command)
                                    (ngit--config-validated-cache 'unvalidated))
                                ;; Command parameters are already in memory.  Validate
                                ;; them before revision/argument processing, without
                                ;; adding config reads to historical zero-read paths.
                                (let ((parameters (ngit--config-command-parameters repo)))
                                  (condition-case parameter-error
                                      (dolist (entry parameters)
                                        (ngit--config-validate-entry entry nil command))
                                    (ngit-fatal
                                     ;; Only an invalid parameter needs an ordered
                                     ;; file read to select Git's first diagnostic.
                                     (ngit-config-entries repo)
                                     (signal (car parameter-error) (cdr parameter-error)))))
                                (funcall handler repo arguments))))
                      ngit--commands)
              ngit--commands)))
      (funcall original (append prefix args))))
   (ngit-fatal
    (ngit-native-write-stderr (concat (cadr err) "\n"))
    (or (nth 2 err) 128))))

;; Config is loaded after the CLI; keep global config arguments at this
;; boundary so all command implementations see the same parameter scope.
(advice-add 'ngit-main :around #'ngit--config-main)

(defvar ngit--config-cache nil "(REPO . ENTRIES) of the last config read.")
(defvar ngit--config-entry-origins nil "Origins of entries in the config cache.")

(defun ngit-config-entries (repo)
  "Return all config entries of REPO, lowest precedence first."
  (ngit--config-startup repo)
  (let ((entries
         (if (and ngit--config-cache (eq (car ngit--config-cache) repo))
             (cdr ngit--config-cache)
           (let* ((ngit--config-origins (make-hash-table :test 'eq))
                  (loaded (ngit--config-load repo t)))
             (setq ngit--config-entry-origins ngit--config-origins
                   ngit--config-cache (cons repo loaded))
             loaded))))
    ;; Preserve the existing first-read point.  In particular, a command
    ;; which historically needed no config files keeps its zero-read path.
    (when (and ngit--config-validation-command
               (not (eq entries ngit--config-validated-cache)))
      (setq ngit--config-validated-cache entries)
      (dolist (entry entries)
        (ngit--config-validate-entry
         entry (and ngit--config-entry-origins (gethash entry ngit--config-entry-origins))
         ngit--config-validation-command)))
    entries))

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

(defun ngit--config-invalidate ()
  "Forget parsed entries and bytes after an in-process config write."
  (setq ngit--config-cache nil ngit--config-entry-origins nil
        ngit--config-startup-repo nil ngit--config-startup-entries nil
        ngit--config-validated-cache 'unvalidated)
  (when ngit--config-text-cache
    (setq ngit--config-text-cache (make-hash-table :test 'equal))))

(defun ngit--config-entry-int (entry origin)
  "Parse integer ENTRY, retaining its diagnostic ORIGIN without reading files."
  (let* ((key (car entry)) (value (cdr entry))
         (parsed (ngit--config-parse-integer value)))
    (if (null (cdr parsed)) (car parsed)
      (ngit--die "fatal: bad numeric config value '%s' for '%s'%s: %s"
                 (if (stringp value) value "") key
                 (cond ((and origin (string-prefix-p "file:" origin))
                        (concat " in file " (substring origin 5)))
                       ((equal origin "standard input:") " in standard input")
                       (t ""))
                 (if (eq (cdr parsed) 'range) "out of range" "invalid unit")))))

(defun ngit--config-compression-level (level key)
  "Validate compression LEVEL for KEY with Git's range diagnostic."
  (when (or (< level -1) (> level 9))
    (ngit--die "fatal: bad %s compression level %d"
               (if (string= key "pack.compression") "pack" "zlib") level))
  level)

(defun ngit--config-validate-entry (entry origin command)
  "Validate one callback ENTRY in source order, retaining ORIGIN."
  (let ((key (car entry)))
    (cond ((member key '("core.compression" "core.loosecompression" "pack.compression"))
           (ngit--config-compression-level (ngit--config-entry-int entry origin) key))
          ((and (member command ngit--config-diff-commands)
                (or (string= key "diff.renamelimit")
                    (and (string= command "status") (string= key "status.renamelimit"))))
           (ngit--config-entry-int entry origin)))))

(defun ngit-config-int (repo key default)
  "Return the final KEY parsed by Git's signed-int rules, or DEFAULT."
  (let ((normalized (ngit--config-normalize key)) found)
    (dolist (entry (ngit-config-entries repo))
      (when (string= (car entry) normalized) (setq found entry)))
    (if (null found) default
      (ngit--config-entry-int
       found (and ngit--config-entry-origins (gethash found ngit--config-entry-origins))))))

(defun ngit-config-compression-level (repo key default)
  "Read compression KEY with Git's integer and zlib range diagnostics."
  (ngit--config-compression-level (ngit-config-int repo key default) key))

(defun ngit-config-bool (repo key default)
  "Return KEY as a Git boolean, DEFAULT when unset."
  (let* ((key (ngit--config-normalize key))
         (value (ngit-config-get repo key)))
    (if (null value) default (ngit--config-boolean-value value key))))

(defun ngit-config-renames-p (repo key default)
  "Read rename KEY, accepting Git's case-insensitive copy synonyms."
  (let ((value (ngit-config-get repo key)))
    (or (and (stringp value) (member (downcase value) '("copy" "copies")))
        (if (null value) default (ngit--config-boolean-value value key)))))

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
