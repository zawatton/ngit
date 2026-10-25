;;; ngit-config-cmd.el --- writable git config command -*- lexical-binding: t; -*-

;;; Commentary:

;; The read-only config command lives in ngit-status.el.  This module adds
;; the writing forms while reusing ngit-branch.el's config.c-style event
;; parser, so untouched bytes of a config file stay untouched.

;;; Code:

(require 'ngit-branch)

(defun ngit-config-cmd--global-file ()
  "Return the file selected by `git config --global'."
  (let* ((explicit (getenv "GIT_CONFIG_GLOBAL"))
         (home (getenv "HOME"))
         (xdg (getenv "XDG_CONFIG_HOME"))
         (user-file (and home (expand-file-name ".gitconfig" home)))
         (xdg-file (expand-file-name "git/config"
                                    (if (and xdg (not (string= xdg ""))) xdg
                                      (expand-file-name ".config" home)))))
    (cond ((and explicit (not (string= explicit ""))) (expand-file-name explicit))
          ((and user-file (not (file-exists-p user-file)) (file-exists-p xdg-file)) xdg-file)
          ((and home (not (string= home ""))) user-file)
          (t (ngit--die "fatal: $HOME not set")))))

(defun ngit-config-cmd--local-file (repo)
  (unless repo (ngit--die "fatal: --local can only be used inside a git repository"))
  (expand-file-name "config" (ngit-repo-commondir repo)))

(defun ngit-config-cmd--worktree-file (repo)
  "Select the worktree source, requiring the extension for linked trees."
  (unless repo (ngit--die "fatal: --worktree can only be used inside a git repository"))
  (let ((file (expand-file-name "config.worktree" (ngit-repo-gitdir repo)))
        (admin (expand-file-name "worktrees" (ngit-repo-commondir repo)))
        (multiple nil))
    (if (member file (ngit--config-files repo)) file
      (when (file-directory-p admin)
        (dolist (entry (directory-files admin t "\\`[^.]"))
          (when (file-exists-p (expand-file-name "gitdir" entry))
            (setq multiple t))))
      (when multiple
        (ngit--die (concat "fatal: --worktree cannot be used with multiple working trees unless the config\n"
                           "extension worktreeConfig is enabled. Please read \"CONFIGURATION FILE\"\n"
                           "section in \"git help worktree\" for details")))
      (ngit-config-cmd--local-file repo))))

(defun ngit-config-cmd--lock (file)
  "Exclusively reserve FILE.lock, preserving errno and lock ownership."
  (let* ((lock (concat file ".lock"))
         (bytes (encode-coding-string lock 'utf-8))
         (address (nl-ffi-memory-address (ngit-native--buffer 3 (1+ (length bytes))))))
    (ngit-native--write-bytes address bytes)
    (ptr-write-u8 address (length bytes) 0)
    ;; O_WRONLY | O_CREAT | O_EXCL, retaining the native close-on-exec flag.
    (let ((descriptor (syscall-direct ngit-native--sys-open address
                                     (logior ngit-native--o-rdonly-cloexec #o301)
                                     #o666 0 0 0)))
      (when (< descriptor 0)
        (ngit--die-with 255 "error: could not lock config file %s: %s"
                        file (if (= descriptor -17) "File exists"
                               (ngit--config-access-message (- descriptor)))))
      (syscall-direct ngit-native--sys-close descriptor 0 0 0 0 0))
    lock))

(defun ngit-config-cmd--write-file (file text)
  "Atomically replace config FILE with TEXT, preserving its mode."
  (let ((directory (file-name-directory file)))
    (when directory (ngit-make-directory directory t)))
  (let* ((mode (or (and (file-exists-p file) (file-modes file))
                   (ngit-native-creation-mode #o666)))
         (lock (ngit-config-cmd--lock file)))
    (unwind-protect
        (progn
          (ngit-commit-lock lock (encode-coding-string text 'utf-8) mode)
          (ngit--config-invalidate))
      (ngit-unlock lock))))

(defun ngit-config-cmd--set-file (file key value &optional add)
  "Set KEY to VALUE in FILE, or unset it when VALUE is nil.
With ADD, append a value.  Return nil when Git would report that nothing
was set, including an unset miss and a single-value write over duplicates."
  (let* ((text (and (file-exists-p file)
                    (decode-coding-string (ngit--read-file file) 'utf-8)))
         (split (ngit--cfg-split-key key))
         (base (car split))
         (full (concat base "." (downcase (cdr split)))))
    (if (or (null text) (= (length text) 0))
        (when value
          (ngit-config-cmd--write-file
           file (concat (ngit--cfg-section-header base)
                        (ngit--cfg-pair (cdr split) value)))
          t)
      (let* ((events (ngit--cfg-events text))
             (nev (length events))
             (slot nil) (matches nil) (key-seen nil) (section-seen nil)
             (in-keys nil) (keys-flags (make-vector nev nil)))
        (dotimes (i nev)
          (let ((e (aref events i)))
            (pcase (aref e 0)
              ('section
               (setq in-keys (string= (aref e 3) base))
               (aset keys-flags i in-keys)
               (when in-keys
                 (setq section-seen t)
                 (unless key-seen (setq slot i))))
              ('entry
               (let ((match (and (not add) (string= (aref e 3) full))))
                 (cond (key-seen (when match (push i matches)))
                       (in-keys
                        (setq slot i section-seen t)
                        (when match (setq key-seen t) (push i matches)))))))))
        (setq matches (nreverse matches))
        (cond
         ((and (null matches) (null value)) nil)
         ((and (cdr matches) value) nil)
         (t
          (let ((seen (vconcat (or matches (list (or slot (max 0 (1- nev)))))))
                (out nil) (copy-begin 0) (n (length text)) (k 0))
            (while (< k (length seen))
              (let* ((j (aref seen k)) (e (aref events j))
                     (copy-end 0) (replace-end 0))
                (if (not key-seen)
                    (progn
                      (setq copy-end (if (> nev 0) (aref e 2) 0))
                      (when (and (> copy-end 0) (< copy-end n)
                                 (/= (aref text (1- copy-end)) ?\n)
                                 (= (aref text copy-end) ?\n))
                        (setq copy-end (1+ copy-end)))
                      (setq replace-end copy-end))
                  (setq replace-end (aref e 2) copy-end (aref e 1))
                  (unless value
                    (let ((r (ngit--cfg-maybe-remove-section
                              events keys-flags seen k)))
                      (when r
                        (setq k (nth 0 r) copy-end (nth 1 r)
                              replace-end (nth 2 r)))))
                  (while (and (> copy-end 0)
                              (let ((c (aref text (1- copy-end))))
                                (and (ngit--cfg-space-p c) (/= c ?\n))))
                    (setq copy-end (1- copy-end))))
                (when (> copy-end copy-begin)
                  (push (substring text copy-begin copy-end) out)
                  (when (/= (aref text (1- copy-end)) ?\n) (push "\n" out)))
                (setq copy-begin replace-end))
              (setq k (1+ k)))
            (when value
              (unless section-seen (push (ngit--cfg-section-header base) out))
              (push (ngit--cfg-pair (cdr split) value) out))
            (when (< copy-begin n) (push (substring text copy-begin) out))
            (ngit-config-cmd--write-file file (apply #'concat (nreverse out)))
            t)))))))

(defun ngit-config-cmd--entries (repo file includes &optional explicit-file required)
  "Read selected FILE or all sources, with the requested INCLUDES policy."
  (ngit--config-load repo includes file (and explicit-file file) required))

(defun ngit-config-cmd--origin (entry origins)
  "Emit the origin of ENTRY when ORIGINS was requested."
  (when origins
    (ngit--emit (ngit-quote-path (gethash entry origins)) "\t")))

(defun ngit-config-cmd--get (entries key)
  (let ((key (ngit--config-normalize key)) (value nil) (seen nil))
    (dolist (entry entries)
      (when (string= (car entry) key)
        (setq value (cdr entry) seen t)))
    (cons seen value)))

(defun ngit-config-cmd--file-match-count (file key)
  (let ((count 0) (key (ngit--config-normalize key)))
    (when (file-readable-p file)
      (dolist (entry (ngit--config-parse
                      (decode-coding-string (ngit--read-file file) 'utf-8)))
        (when (string= (car entry) key) (setq count (1+ count)))))
    count))

;;;###autoload
(defun ngit-cmd-config-write (repo args)
  "Implement config get/list/set/add/unset and --global."
  (ngit--config-startup repo)
  (let ((file (getenv "GIT_CONFIG")) (selection (and (getenv "GIT_CONFIG") t))
        (explicit-file (and (getenv "GIT_CONFIG") t)) (includes 'default) (all nil)
        (action nil) (rest nil) (show-origin nil))
    ;; Git 2.47 accepts these subcommands only in the first argument slot.
    ;; A later literal "list" or "add" is an ordinary value.
    (when (member (car args) '("get" "list" "set" "add" "unset"))
      (setq action (intern (pop args))))
    (while args
      (let ((arg (pop args)))
        (cond ((string= arg "--global")
               (setq selection t file (ngit-config-cmd--global-file)))
              ((string= arg "--system")
               (setq selection t file (or (getenv "GIT_CONFIG_SYSTEM") "/etc/gitconfig")))
              ((string= arg "--worktree")
               (setq selection t file (ngit-config-cmd--worktree-file repo)))
              ((string= arg "--local")
               (setq selection t file (ngit-config-cmd--local-file repo)))
              ((member arg '("--file" "-f"))
               (unless args (ngit--die-with 129 "error: option `%s' requires a value" arg))
               (setq selection t explicit-file t file (pop args)))
              ((string-prefix-p "--file=" arg)
               (setq selection t explicit-file t file (substring arg 7)))
              ((string= arg "--includes") (setq includes t))
              ((string= arg "--no-includes") (setq includes nil))
              ((string= arg "--show-origin") (setq show-origin t))
              ((string= arg "--get-all") (setq action 'get all t))
              ((string= arg "--all") (setq all t))
              ((string= arg "--get") (setq action 'get))
              ((member arg '("--list" "-l")) (setq action 'list))
              ((string= arg "--unset") (setq action 'unset))
              ((string= arg "--add") (setq action 'add))
              ((string= arg "--") (setq rest (append (nreverse rest) args) args nil))
              ((string-prefix-p "-" arg)
               (ngit--die-with 129 "error: unknown option `%s'" arg))
              (t (push arg rest)))))
    (setq rest (nreverse rest))
    (when (eq includes 'default) (setq includes (or (not selection) (equal file "-"))))
    (unless action
      (setq action (cond ((= (length rest) 1) 'get)
                         ((= (length rest) 2) 'set)
                         (t (ngit--die-with 129 "error: no action specified")))))
    (pcase action
      ('list
       (when rest (ngit--die-with 129 "error: wrong number of arguments"))
       ;; List consults the normal config for pager setup, even when the
       ;; eventual list selects one file or disables includes.
       (let ((ngit--config-early t)) (ngit--config-load repo t))
       (let* ((ngit--config-origins (and show-origin (make-hash-table :test 'eq)))
              (ngit--config-entry-callback
               (lambda (entry)
                 (ngit-config-cmd--origin entry ngit--config-origins)
                 (ngit--emit (car entry)
                             (if (eq (cdr entry) t) "\n" (concat "=" (cdr entry) "\n"))))))
         (ngit-config-cmd--entries repo file includes explicit-file t))
       0)
      ('get
       (unless (= (length rest) 1)
         (ngit--die-with 129 "error: wrong number of arguments"))
       (when all (let ((ngit--config-early t)) (ngit--config-load repo t)))
       (let* ((ngit--config-origins (and show-origin (make-hash-table :test 'eq)))
              (entries (ngit-config-cmd--entries repo file includes explicit-file))
              (found (ngit-config-cmd--get entries (car rest))))
         (if (not (car found))
             1
           (if all
               (let ((key (ngit--config-normalize (car rest))))
                 (dolist (entry entries)
                   (when (string= key (car entry))
                     (ngit-config-cmd--origin entry ngit--config-origins)
                     (ngit--emit (if (eq (cdr entry) t) "" (cdr entry)) "\n"))))
             (when show-origin
               (let ((key (ngit--config-normalize (car rest))) (last nil))
                 (dolist (entry entries) (when (string= key (car entry)) (setq last entry)))
                 (ngit-config-cmd--origin last ngit--config-origins)))
             (ngit--emit (if (eq (cdr found) t) "" (cdr found)) "\n"))
           0)))
      (_
       (let ((file (or file (ngit-config-cmd--local-file repo))))
         (pcase action
           ('set
            (unless (= (length rest) 2)
              (ngit--die-with 129 "error: wrong number of arguments"))
            (if (> (ngit-config-cmd--file-match-count file (car rest)) 1)
                (progn
                  (ngit-native-write-stderr
                   (format (concat "warning: %s has multiple values\n"
                                   "error: cannot overwrite multiple values with a single value\n"
                                   "       Use a regexp, --add or --replace-all to change %s.\n")
                           (ngit--config-normalize (car rest)) (car rest)))
                  5)
              (if (ngit-config-cmd--set-file file (car rest) (cadr rest)) 0 5)))
           ('add
            (unless (= (length rest) 2)
              (ngit--die-with 129 "error: wrong number of arguments"))
            (ngit-config-cmd--set-file file (car rest) (cadr rest) t)
            0)
           ('unset
            (unless (= (length rest) 1)
              (ngit--die-with 129 "error: wrong number of arguments"))
            (let ((count (ngit-config-cmd--file-match-count file (car rest))))
              (cond ((= count 0) 5)
                    ((> count 1)
                     (ngit-native-write-stderr
                      (format "warning: %s has multiple values\n" (ngit--config-normalize (car rest))))
                     5)
                    (t (ngit-config-cmd--set-file file (car rest) nil) 0))))))))))

(provide 'ngit-config-cmd)

;;; ngit-config-cmd.el ends here
