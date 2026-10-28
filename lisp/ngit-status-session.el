;;; ngit-status-session.el --- resident porcelain status -*- lexical-binding: t; -*-

;;; Commentary:

;; One serialized repository session per process.  Coverage is discovered,
;; watched, then rediscovered while computing the published result.  Only
;; ordinary tracked-file modifications have a selected-entry certificate.
;; Everything else rebuilds the repository and all session-owned caches.
;; inotify cannot certify mmap-only, remote or mount-over writes: callers
;; must explicitly refresh after those operations.

;;; Code:

(require 'ngit-status)
(require 'ngit-status-watch)

(defvar ngit-status-session--active nil)
(defvar ngit-ss--output-buffer nil "Private bounded output owner during one server invocation.")
(define-error 'ngit-status-session-write-error "ngit status-server transport failure")
(defconst ngit-ss--environment-names
  '("HOME" "XDG_CONFIG_HOME" "GIT_DIR" "GIT_WORK_TREE" "GIT_COMMON_DIR"
    "GIT_INDEX_FILE" "GIT_CONFIG" "GIT_CONFIG_SYSTEM" "GIT_CONFIG_GLOBAL"
    "GIT_CONFIG_NOSYSTEM" "GIT_CONFIG_COUNT" "GIT_CONFIG_PARAMETERS"
    "GIT_OBJECT_DIRECTORY" "GIT_ALTERNATE_OBJECT_DIRECTORIES"
    "GIT_ATTR_NOSYSTEM" "GIT_ATTR_SOURCE" "GIT_NAMESPACE"
    "GIT_CEILING_DIRECTORIES" "GIT_DISCOVERY_ACROSS_FILESYSTEM"))

;; Session slots: tag directory environment cli repo watcher status bytes
;; enabled reason entries coverage dirs policies indexes native-fds sizes
;; top-fds counters closed config-context scratch.  Slots 0..20 remain stable.
;; Scratch is [path-owner stat-owner top-bytes-and-length directory-prefix].
;; The handle identity is checked, not just its tag.

(defun ngit-ss--check (session)
  (unless (and (eq session ngit-status-session--active)
               (not (aref session 19)))
    (error "ngit: closed or foreign status session"))
  (dolist (pair (aref session 2))
    (unless (equal (getenv (car pair)) (cdr pair))
      (error "ngit: status session environment changed; close and reopen"))))

(defun ngit-ss--count (session slot &optional amount)
  (let ((counters (aref session 18)))
    (aset counters slot (+ (aref counters slot) (or amount 1)))))

(defun ngit-ss--run (session function)
  "Run FUNCTION with private caches and the fixed invocation context."
  (ngit-ss--check session)
  (let* ((config (and (aref session 8) (aref session 5) (aref session 20)))
         (scratch (aref session 21))
         (default-directory (aref session 1))
        (ngit--invocation-directory (aref session 1))
        (ngit--config-cli-parameters (aref session 3))
        (ngit--config-validation-command "status")
        (ngit--config-cache (and config (aref config 0)))
        (ngit--config-entry-origins (and config (aref config 1)))
        (ngit--config-startup-repo (and config (aref config 2)))
        (ngit--config-startup-entries (and config (aref config 3)))
        (ngit--config-validated-cache (if config (aref config 4) 'unvalidated))
        (ngit--config-text-cache (if config (aref config 5) (make-hash-table :test 'equal)))
        (ngit--config-refs-repo (and config (aref config 6)))
        (ngit--config-remote-urls (if config (aref config 7) 'unread))
        (ngit--config-early (and config (aref config 8)))
        (ngit--config-forbid-remote (and config (aref config 9)))
        (ngit--config-parameters (and config (aref config 10)))
        (ngit--config-sources (and config (aref config 11)))
        (ngit-native--fds (aref session 15))
        (ngit-native--sizes (aref session 16))
        (ngit-wt--top-fds (aref session 17))
        (ngit-wt--path (aref scratch 0)) (ngit-wt--stat (aref scratch 1))
        (ngit-wt--top (aref scratch 2)) (ngit-wt--dir (aref scratch 3))
        (ngit--loose-names (make-hash-table :test 'equal))
        (ngit--pack-order nil)
        (ngit--abbrev-cache (make-hash-table :test 'equal))
        (ngit--out nil) (ngit--status-prefix "")
        (ngit--status-rename-override 'default)
        (ngit--status-ignored-mode "no")
        (ngit--status-untracked-mode "normal")
        (ngit--no-excludes nil) (ngit--untracked-pathspecs nil)
        (ngit--directory-read-observer nil)
        (ngit-index-read-observer
         (lambda (index) (push index (aref session 14)))))
    (let ((succeeded nil))
      (unwind-protect
        (prog1
            (funcall function)
          ;; Publish only after the calculation succeeds.  The
          ;; repository identities and validation identity travel together.
          (aset session 20
                (and (aref session 8) (aref session 5)
                     (eq (car ngit--config-cache) (aref session 4))
                     (eq ngit--config-startup-repo (aref session 4))
                     (eq ngit--config-validated-cache (cdr ngit--config-cache))
                     (or (and config (eq ngit--config-cache (aref config 0)) config)
                         (vector ngit--config-cache ngit--config-entry-origins
                                 ngit--config-startup-repo ngit--config-startup-entries
                                 ngit--config-validated-cache ngit--config-text-cache
                                 ngit--config-refs-repo ngit--config-remote-urls
                                 ngit--config-early ngit--config-forbid-remote
                                 ngit--config-parameters ngit--config-sources
                                 ;; Resolve these policies once per certified
                                 ;; configuration, including explicit false.
                                 (vector (ngit-config-bool (aref session 4) "core.filemode" t)
                                         (ngit-config-bool (aref session 4) "core.quotepath" t))))))
          ;; The path bytes and their directory metadata are one workspace.
          ;; Publishing remains inside the abnormal-exit invalidation guard.
          (aset scratch 0 ngit-wt--path)
          (aset scratch 1 ngit-wt--stat)
          (aset scratch 2 ngit-wt--top)
          (aset scratch 3 ngit-wt--dir)
          (setq succeeded t))
        ;; A throw or quit can consume the only event just as an error can.
        ;; Preserve that exit while dropping every published cache identity.
        (unless succeeded (ngit-ss--reset session))))))

(defun ngit-ss--clear-scratch (session &optional path stat)
  "Release SESSION's workspace and any unpublished local PATH and STAT."
  (let* ((scratch (aref session 21))
         (owners (list path stat (aref scratch 0) (aref scratch 1))) released)
    ;; Clear ownership and matching metadata before releasing aliased owners.
    (dotimes (i 4) (aset scratch i nil))
    (dolist (owner owners)
      (when (and owner (not (memq owner released)))
        (push owner released)
        (nl-ffi-memory-release owner)))))

(defun ngit-ss--clear-config (session)
  "Discard SESSION's complete private configuration read context."
  (aset session 20 nil)
  (ngit--config-invalidate)
  (setq ngit--config-refs-repo nil ngit--config-remote-urls 'unread
        ngit--config-early nil ngit--config-forbid-remote nil
        ngit--config-parameters nil ngit--config-sources nil))

(defun ngit-ss--close-fds (table)
  (maphash (lambda (_ fd) (syscall-direct 3 fd 0 0 0 0 0)) table)
  (clrhash table))

(defun ngit-ss--reset (session)
  "Discard all published and repository state before rediscovery."
  (aset session 7 nil)
  (aset session 8 nil)
  (aset session 6 nil)
  (ngit-ss--clear-scratch session ngit-wt--path ngit-wt--stat)
  (setq ngit-wt--path nil ngit-wt--stat nil ngit-wt--top nil ngit-wt--dir nil)
  (when (aref session 5)
    (ngit-status-watch-close (aref session 5))
    (aset session 5 nil))
  (dolist (index (aref session 14)) (ngit-index-release-native index))
  (aset session 14 nil)
  (ngit-ss--close-fds (aref session 15))
  (ngit-ss--close-fds (aref session 17))
  (clrhash (aref session 16))
  (clrhash ngit--loose-names)
  (setq ngit--pack-order nil)
  (ngit-ss--clear-config session)
  (aset session 4 nil))

(defun ngit-ss--bytes (path)
  (if (multibyte-string-p path) (encode-coding-string path 'utf-8) path))

(defun ngit-ss--path (path)
  (ngit-ss--bytes (directory-file-name (expand-file-name path))))

(defun ngit-ss--parents (path function)
  "Call FUNCTION for PATH and each ancestor, without following links."
  (let ((path (ngit-ss--path path)) parent)
    (while path
      (funcall function path)
      (setq parent (file-name-directory path)
            parent (and parent (directory-file-name parent))
            path (and parent (not (equal path parent)) parent)))))

(defun ngit-ss--mounts ()
  "Return local mount points and types, or nil if they cannot be proven."
  (when (file-readable-p "/proc/self/mountinfo")
    (let (mounts unproven)
      (dolist (line (split-string (ngit--read-file "/proc/self/mountinfo") "\n" t))
        (let* ((halves (split-string line " - "))
               (fields (split-string (car halves) " " t))
               (type (car (split-string (cadr halves) " " t)))
               (point (nth 4 fields)))
          ;; An omitted escaped mount could otherwise inherit the local
          ;; filesystem certificate of a shallower mount.  Fail closed.
          (if (and point (string-search "\\" point)) (setq unproven t)
            (when (and point type) (push (cons point type) mounts)))))
      (unless unproven mounts))))

(defun ngit-ss--local-p (path mounts)
  (let ((best nil))
    (dolist (mount mounts)
      (when (and (or (equal path (car mount))
                     (string-prefix-p (file-name-as-directory (car mount)) path))
                 (or (not best) (> (length (car mount)) (length (car best)))))
        (setq best mount)))
    (and best (member (cdr best) '("ext2" "ext3" "ext4" "xfs" "btrfs"
                                               "tmpfs" "overlay" "ramfs")))))

(defun ngit-ss--coverage (repo)
  "Discover exact watched paths and conservative eligibility for REPO."
  (let* ((paths (make-hash-table :test 'equal))
         (interests (make-hash-table :test 'equal))
         (dirs (make-hash-table :test 'equal))
         (policies (make-hash-table :test 'equal))
         (entries (make-hash-table :test 'equal))
         (mounts (ngit-ss--mounts))
         (top (ngit-ss--path (ngit-repo-toplevel repo)))
         (gitdir (ngit-ss--path (ngit-repo-gitdir repo)))
         (reason nil) (result nil)
         (index (ngit-read-index repo)))
    (cl-labels
        ((fail (message) (unless reason (setq reason message)))
         (cover (path)
           (ngit-ss--parents
            path (lambda (p)
                   ;; Include missing ancestors too: creating one can expose
                   ;; a previously absent policy source.  Event classification
                   ;; then needs one lookup, not a scan of all watched paths.
                   (puthash p t interests)
                   (unless (gethash p paths)
                     (when (file-symlink-p p) (fail "symlink dependency"))
                     (when (file-exists-p p)
                       (unless (ngit-ss--local-p p mounts) (fail "unsupported mount"))
                       (when (or (file-directory-p p) (file-regular-p p))
                         (puthash p t paths)))))))
         (policy (path)
           (setq path (ngit-ss--path path))
           (puthash path t policies)
           (cover path))
         (directory (path)
           (setq path (ngit-ss--path path))
           (unless (gethash path dirs)
             (puthash path t dirs)
             (cover path)
             (policy (concat path "/.gitignore"))
             (policy (concat path "/.gitattributes"))))
         (metadata (path)
           (cover path)
           (if (file-symlink-p path) (fail "symlink metadata")
             (when (file-directory-p path)
               (dolist (child (directory-files path t nil t))
                 (unless (member (file-name-nondirectory child) '("." ".."))
                   (metadata child)))))))
      (unless (and (equal gitdir (concat top "/.git"))
                   (equal (ngit-ss--path (ngit-repo-commondir repo)) gitdir))
        (fail "linked worktree or external gitdir"))
      (dolist (name '("GIT_INDEX_FILE" "GIT_OBJECT_DIRECTORY"
                      "GIT_ALTERNATE_OBJECT_DIRECTORIES" "GIT_COMMON_DIR"
                      "GIT_ATTR_SOURCE"))
        (when (getenv name) (fail "external repository dependency")))
      (dolist (entry (ngit-config-entries repo))
        (when (or (string-prefix-p "include." (car entry))
                  (string-prefix-p "includeif." (car entry)))
          (fail "config includes"))
        (when (string-prefix-p "filter." (car entry)) (fail "clean filters")))
      (dolist (file (ngit--config-files repo)) (policy file))
      ;; The existing attribute reader has no system-file provider.  Cover
      ;; conventional missing sources so their creation forces refusal,
      ;; rather than silently certifying an incomplete attribute policy.
      (unless (ngit--config-boolean-value (getenv "GIT_ATTR_NOSYSTEM")
                                          "GIT_ATTR_NOSYSTEM" t)
        (dolist (file '("/etc/gitattributes" "/usr/etc/gitattributes"))
          (policy file)
          (when (or (file-exists-p file) (file-symlink-p file))
            (error "ngit: unsupported system attributes source"))))
      (let* ((xdg (getenv "XDG_CONFIG_HOME"))
             (base (if (and xdg (not (equal xdg ""))) xdg
                     (expand-file-name ".config" (getenv "HOME")))))
        (dolist (pair '(("core.excludesfile" . "git/ignore")
                        ("core.attributesfile" . "git/attributes")))
          (policy (expand-file-name (cdr pair) base))
          (let ((value (ngit-config-get repo (car pair))))
            (when (stringp value) (policy (expand-file-name value))))))
      (metadata gitdir)
      (directory top)
      (ngit-wt-setup (ngit-repo-toplevel repo))
      (dotimes (i (length (aref index 0)))
        (let* ((entry (aref (aref index 0) i))
               (path (concat top "/" (aref entry 0)))
               (raw (aref index 1)) (pos (aref entry 4)))
          (when (or (/= (aref entry 3) 0)
                    (/= (logand (aref entry 2) #o170000) #o100000)
                    (aref entry 5)
                    (/= 0 (logand (ngit--be16 raw (+ pos 60)) #xf000)))
            (fail "unsupported index entries or flags"))
          (puthash path entry entries)
          (cover path)
          (let ((parent (file-name-directory path)))
            (while (and parent (string-prefix-p (concat top "/") parent))
              (directory parent)
              (setq parent (file-name-directory (directory-file-name parent)))))))
      ;; EXPAND visits empty and nonempty untracked directories using the
      ;; real ignore stack; tracked ancestors above cover excluded parents.
      (let ((ngit--directory-read-observer #'directory))
        (ngit--walk-untracked
         repo (file-name-as-directory (ngit-repo-toplevel repo))
         (ngit--tracked-set (aref index 0)) "" (ngit-ignore-global-patterns repo)
         (lambda (path)
           (when (string-suffix-p "/" path)
             (directory (concat top "/" path)))) nil t))
      (when (> (hash-table-count paths) 65536) (fail "watch limit"))
      (maphash (lambda (p _) (push p result)) paths)
      (list :paths (sort result #'string<) :dirs dirs :policies policies
            :entries entries :interests interests :reason reason :top top :gitdir gitdir))))

(defun ngit-ss--render (repo status &optional policy)
  (let ((ngit--out nil) (ngit--status-prefix "")
        (ngit--quote-high-bytes (if policy (aref policy 1)
                                 (ngit-config-bool repo "core.quotepath" t))))
    (ngit--status-short status t)
    (ngit-ss--bytes (apply #'concat (nreverse ngit--out)))))

(defun ngit-ss--trim-data (status)
  (dolist (kind '(:staged :unstaged :unmerged))
    (dolist (change (plist-get status kind)) (aset change 7 nil))))

(defun ngit-ss--modifications-p (changes)
  (let ((ok t))
    (dolist (change changes)
      (unless (and (= (aref change 1) ?M)
                   (not (aref change 6)) (not (aref change 8)))
        (setq ok nil)))
    ok))

(defun ngit-ss--install (session coverage status output)
  (aset session 6 status)
  (aset session 7 output)
  (aset session 9 (plist-get coverage :reason))
  (aset session 10 (plist-get coverage :entries))
  (aset session 11 coverage)
  (aset session 12 (plist-get coverage :dirs))
  (aset session 13 (plist-get coverage :policies))
  (ngit-ss--trim-data status))

(defun ngit-ss--read (session)
  "Compute a fresh result, publishing only after bounded certification."
  (let ((attempt 0) (done nil) output)
    (while (not done)
      (setq attempt (1+ attempt))
      (ngit-ss--reset session)
      (let* ((repo (ngit-discover (aref session 1)))
             (_ (unless (ngit-repo-toplevel repo)
                  (error "fatal: this operation must be run in a work tree")))
             (before (ngit-ss--coverage repo))
             (watch nil) (failure (plist-get before :reason)))
        (unless failure
          (condition-case err
              (progn (setq watch (ngit-status-watch-open (plist-get before :paths)))
                     (aset session 5 watch)
                     (ngit-ss--count session 5))
            (error (setq failure (concat "watch setup: " (error-message-string err))))))
        ;; Forget even bootstrap reads.  An index, ref or config replacement
        ;; during registration must not survive in the certified computation.
        (ngit-ss--clear-scratch session ngit-wt--path ngit-wt--stat)
        (setq ngit-wt--path nil ngit-wt--stat nil ngit-wt--top nil ngit-wt--dir nil)
        (dolist (index (aref session 14)) (ngit-index-release-native index))
        (aset session 14 nil)
        (ngit-ss--close-fds (aref session 15))
        (ngit-ss--close-fds (aref session 17))
        (clrhash (aref session 16))
        (ngit-ss--clear-config session)
        (setq repo (ngit-discover (aref session 1)))
        (aset session 4 repo)
        (let* ((ngit--status-untracked-mode
                (or (ngit-config-get repo "status.showuntrackedfiles") "normal"))
               (status (ngit-status-collect repo))
               (after (ngit-ss--coverage repo))
               (events (and watch (ngit-status-watch-drain watch)))
               (unstable (or (not (equal (plist-get before :paths)
                                        (plist-get after :paths)))
                             (and events
                                  (or (seq-some (lambda (path)
                                                  (ngit-ss--relevant-p session path after))
                                                (plist-get events :paths))
                                      (plist-get events :lost)
                                      (plist-get events :overflow))))))
          (ngit-ss--count session 2)
          (ngit-ss--count session 4 (length (ngit-index-entries repo)))
          (when (and events (plist-get events :overflow)) (ngit-ss--count session 6))
          (setq output (ngit-ss--render repo status))
          (ngit-ss--install session after status output)
          (cond
           ((or failure (plist-get after :reason))
            (aset session 9 (or failure (plist-get after :reason)))
            (setq done t))
           ((not unstable) (aset session 8 t) (setq done t))
           ((>= attempt 3)
            (aset session 9 "concurrent changes; uncached result")
            (setq done t)))
          (when (and done (not (aref session 8)) watch)
            (ngit-status-watch-close watch)
            (aset session 5 nil))
          ;; Temporary sub-repository indexes are never retained.
          (let ((main (aref repo 6)) keep)
            (dolist (index (aref session 14))
              (if (eq index main) (push index keep) (ngit-index-release-native index)))
            (aset session 14 keep)))))
    output))

(defun ngit-ss--relevant-p (session path &optional coverage)
  "Whether PATH can affect a monitored repository or dependency."
  (let* ((coverage (or coverage (aref session 11)))
         (top (plist-get coverage :top))
         (gitdir (plist-get coverage :gitdir)))
    (or (equal path top) (equal path gitdir)
        (string-prefix-p (concat gitdir "/") path)
        (gethash path (plist-get coverage :policies))
        (gethash path (plist-get coverage :entries))
        (gethash path (plist-get coverage :dirs))
        (gethash (directory-file-name (or (file-name-directory path) "/"))
                 (plist-get coverage :dirs))
        ;; Ancestor replacement of a missing or existing dependency.
        (gethash path (plist-get coverage :interests)))))

(defun ngit-ss--selected (session paths)
  "Return selected entry vector, or nil when a full refresh is required."
  (when (ngit-ss--modifications-p (plist-get (aref session 6) :unstaged))
    (let ((ok t) (seen (make-hash-table :test 'equal)) selected)
      (dolist (path paths)
        (let ((entry (gethash path (aref session 10))))
          (if (or (not entry) (gethash path (aref session 13)))
              (setq ok nil)
            (unless (gethash path seen)
              (puthash path t seen)
              (push entry selected)))))
      (and ok selected
           (vconcat (sort selected (lambda (a b) (string< (aref a 0) (aref b 0)))))))))

(defun ngit-ss--increment (session selected)
  (let* ((repo (aref session 4))
         (config (aref session 20))
         (policy (and config (aref config 12)))
         (changes (ngit-wt-changes repo nil t selected policy))
         (events (ngit-status-watch-drain (aref session 5))))
    (ngit-ss--count session 4 (length selected))
    (when (plist-get events :overflow) (ngit-ss--count session 6))
    (if (or (seq-some (lambda (path) (ngit-ss--relevant-p session path))
                      (plist-get events :paths))
            (plist-get events :lost) (plist-get events :overflow)
            (not (ngit-ss--modifications-p changes)))
        (ngit-ss--read session)
      (let* ((status (aref session 6))
             (names (make-hash-table :test 'equal)) retained)
        (dotimes (i (length selected)) (puthash (aref (aref selected i) 0) t names))
        (dolist (change (plist-get status :unstaged))
          (unless (gethash (aref change 0) names) (push change retained)))
        (setq status (plist-put status :unstaged
                                (sort (append changes (nreverse retained))
                                      (lambda (a b) (string< (aref a 0) (aref b 0))))))
        (aset session 6 status)
        (aset session 7 (ngit-ss--render repo status policy))
        (ngit-ss--trim-data status)
        (ngit-ss--count session 3)
        (aref session 7)))))

;;;###autoload
(defun ngit-status-session-open (directory)
  "Open the process's sole status session, fixed to DIRECTORY and startup state."
  (when ngit-status-session--active (error "ngit: a status session is already open"))
  (let* ((names (copy-sequence ngit-ss--environment-names))
         (count (string-to-number (or (getenv "GIT_CONFIG_COUNT") "0")))
         environment session succeeded)
    (dotimes (i count)
      (push (format "GIT_CONFIG_KEY_%d" i) names)
      (push (format "GIT_CONFIG_VALUE_%d" i) names))
    (dolist (name names) (push (cons name (getenv name)) environment))
    (setq session
          (vector 'ngit-status-session (file-name-as-directory (expand-file-name directory))
                  environment (copy-tree ngit--config-cli-parameters)
                  nil nil nil nil nil nil nil nil nil nil nil
                  (make-hash-table :test 'equal) (make-hash-table :test 'equal)
                  (make-hash-table :test 'equal) (vector 0 0 0 0 0 0 0) nil nil
                  (vector nil nil nil nil))
          ngit-status-session--active session)
    (unwind-protect
        (prog1 (progn (ngit-ss--run session (lambda () (ngit-ss--read session))) session)
          (setq succeeded t))
      (unless succeeded (ngit-status-session-close session)))))

;;;###autoload
(defun ngit-status-session-status (session)
  "Return current porcelain-v1 bytes, with monitored reuse when certified."
  (ngit-ss--run
   session
   (lambda ()
     (ngit-ss--count session 0)
     (if (not (aref session 8)) (ngit-ss--read session)
       (let* ((events (ngit-status-watch-drain (aref session 5)))
              (paths (seq-filter (lambda (path) (ngit-ss--relevant-p session path))
                                 (plist-get events :paths))))
         (when (plist-get events :overflow) (ngit-ss--count session 6))
         (cond
          ((or (plist-get events :overflow) (plist-get events :lost))
           (ngit-ss--read session))
          ((not paths) (ngit-ss--count session 1) (aref session 7))
          ((if (memq :structural-paths events)
               (seq-some (lambda (path) (ngit-ss--relevant-p session path))
                         (plist-get events :structural-paths))
             (plist-get events :structural))
           (ngit-ss--read session))
          (t (let ((selected (ngit-ss--selected session paths)))
               (if selected (ngit-ss--increment session selected)
                 (ngit-ss--read session))))))))))

;;;###autoload
(defun ngit-status-session-refresh (session)
  "Discard SESSION's caches and coverage and return a fresh porcelain read."
  (ngit-ss--run session (lambda () (ngit-ss--count session 0) (ngit-ss--read session))))

;;;###autoload
(defun ngit-status-session-stats (session)
  "Return actual work counters and cache eligibility as a plist."
  (ngit-ss--check session)
  (let ((counters (aref session 18)))
    (list :requests (aref counters 0) :cache_hits (aref counters 1)
          :full_refreshes (aref counters 2) :incremental_updates (aref counters 3)
          :tracked_entries_checked (aref counters 4) :watch_rebuilds (aref counters 5)
          :overflows (aref counters 6) :cache_enabled (if (aref session 8) 1 0)
          :cache_reason (or (aref session 9) "covered local paths"))))

;;;###autoload
(defun ngit-status-session-close (session)
  "Idempotently close SESSION's owned watches, indexes and descriptors."
  (when (and (vectorp session) (= (length session) 22)
             (eq (aref session 0) 'ngit-status-session) (not (aref session 19)))
    (unless (eq session ngit-status-session--active) (error "ngit: foreign status session"))
    ;; Closing must remain possible even when the caller changed its environment.
    (ngit-ss--clear-scratch session)
    (when (aref session 5) (ngit-status-watch-close (aref session 5)))
    (dolist (index (aref session 14)) (ngit-index-release-native index))
    (ngit-ss--close-fds (aref session 15))
    (ngit-ss--close-fds (aref session 17))
    (dotimes (i 18) (unless (memq i '(0 18)) (aset session i nil)))
    (aset session 20 nil)
    (aset session 21 nil)
    (aset session 19 t)
    (setq ngit-status-session--active nil))
  nil)

(defun ngit-ss--write-buffer (bytes buffer)
  "Write all BYTES through owned BUFFER, including partial writes."
  (let ((address (nl-ffi-memory-address buffer)) (position 0) (interrupts 0))
    (ptr-write-bytes address bytes)
    (while (< position (length bytes))
      (let ((written (syscall-direct 1 1 (+ address position)
                                     (- (length bytes) position) 0 0 0)))
        (cond ((= written -4)
               (setq interrupts (1+ interrupts))
               (when (>= interrupts 8) (error "ngit: status-server write interrupted")))
              ((<= written 0) (error "ngit: status-server write failed"))
              (t (setq position (+ position written))))))))

(defun ngit-ss--write (bytes)
  "Write all BYTES, reusing only the current server's bounded private owner."
  (condition-case err
      (if (and ngit-ss--output-buffer (<= (length bytes) 65536))
          (ngit-ss--write-buffer bytes ngit-ss--output-buffer)
        (ngit-native--with-memory (buffer (length bytes))
          (ngit-ss--write-buffer bytes buffer)))
    ;; Once a frame has started, no second frame can repair its boundary.
    (error (signal 'ngit-status-session-write-error (list (error-message-string err))))))

(defun ngit-ss--frame (payload &optional code)
  (setq payload (ngit-ss--bytes payload))
  (ngit-ss--write (concat (if code (format "ERR %d %d\n" code (length payload))
                           (format "OK %d\n" (length payload))) payload)))

(defun ngit-cmd-status-server (_repo args)
  "Serve bounded ASCII verbs on stdin with byte-counted stdout frames."
  (when args (ngit--die-with 64 "ngit: status-server takes no arguments"))
  (let ((session nil) (running t) (input "") (position 0) (interrupts 0)
        (ngit-ss--output-buffer nil))
    (unwind-protect
        (condition-case err
            (progn
              (setq ngit-ss--output-buffer (nl-ffi-memory-allocate 65536))
              (setq session (ngit-status-session-open default-directory))
              (ngit-native--with-memory (buffer 4097)
                (let ((address (nl-ffi-memory-address buffer)))
                  (while running
                    (let ((newline (string-search "\n" input position)))
                      (cond
                       ((and newline (<= (- newline position) 4096))
                        (let ((line (substring input position newline)))
                          (setq position (1+ newline))
                            (condition-case request-error
                                (cond
                                 ((equal line "status")
                                  (ngit-ss--frame (ngit-status-session-status session)))
                                 ((equal line "refresh")
                                  (ngit-ss--frame (ngit-status-session-refresh session)))
                                 ((equal line "stats")
                                  (let ((stats (ngit-status-session-stats session)) fields)
                                    (while stats
                                      (push (format "%s=%s\n" (substring (symbol-name (pop stats)) 1)
                                                    (pop stats)) fields))
                                    (ngit-ss--frame (apply #'concat (nreverse fields)))))
                                 ((equal line "quit") (ngit-ss--frame "") (setq running nil))
                                 (t (ngit-ss--frame "unknown request\n" 64)))
                              (ngit-status-session-write-error
                               (signal (car request-error) (cdr request-error)))
                              (error (ngit-ss--frame (concat (error-message-string request-error) "\n")
                                                     (or (and (eq (car request-error) 'ngit-fatal)
                                                              (nth 2 request-error)) 128))))))
                       ((or newline (> (- (length input) position) 4096))
                        (ngit-ss--frame "request exceeds 4096 bytes\n" 64)
                        (setq running nil))
                       (t
                        ;; Keep at most one bounded incomplete request.  A
                        ;; chunk may contain many complete pipelined verbs.
                        (when (> position 0)
                          (setq input (substring input position) position 0))
                        (let ((read (syscall-direct 0 0 address (- 4097 (length input)) 0 0 0)))
                          (cond
                           ((= read -4)
                            (setq interrupts (1+ interrupts))
                            (when (>= interrupts 8) (error "ngit: status-server read interrupted")))
                           ((< read 0) (error "ngit: status-server read failed"))
                           ((= read 0) (setq running nil))
                           (t (setq interrupts 0
                                    input (concat input (ptr-read-bytes address read))))))))))))
              0)
          (ngit-status-session-write-error 74)
          (error
           (ngit-ss--frame (concat (error-message-string err) "\n")
                          (or (and (eq (car err) 'ngit-fatal) (nth 2 err)) 128))
           128))
      (when ngit-ss--output-buffer (nl-ffi-memory-release ngit-ss--output-buffer))
      (when session (ngit-status-session-close session)))))

(provide 'ngit-status-session)

;;; ngit-status-session.el ends here
