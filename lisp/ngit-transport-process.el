;;; ngit-transport-process.el --- binary-safe Git transport subprocesses -*- lexical-binding: t; -*-

;;; Commentary:

;; NeLisp's Emacs-compatible process bridge currently recodes bytes above
;; ASCII even with binary process coding.  Git protocol and pack data cannot
;; tolerate that, so this module uses the smallest working substrate instead:
;; Linux x86_64 pipe/fork/dup2/execve/read/write/wait4 syscalls.  The child
;; performs no Lisp allocation after fork; argv and envp are prepared first.

;;; Code:

(require 'ngit-native)
(require 'ngit-config)

(defconst ngit-transport--sys-read 0)
(defconst ngit-transport--sys-write 1)
(defconst ngit-transport--sys-open 2)
(defconst ngit-transport--sys-close 3)
(defconst ngit-transport--sys-pipe 22)
(defconst ngit-transport--sys-dup2 33)
(defconst ngit-transport--sys-fork 57)
(defconst ngit-transport--sys-execve 59)
(defconst ngit-transport--sys-exit 60)
(defconst ngit-transport--sys-wait4 61)
(defconst ngit-transport--sys-kill 62)
(defconst ngit-transport--sys-fcntl 72)
(defconst ngit-transport--sys-chdir 80)
(defconst ngit-transport--sigterm 15)
(defconst ngit-transport--wnohang 1)
(defconst ngit-transport--f-setfd 2)
(defconst ngit-transport--fd-cloexec 1)

;; A connection is [ngit-transport PID WRITE-FD READ-FD STATUS EOF].
(defconst ngit-transport--tag 'ngit-transport)

(defun ngit-transport--conn-p (conn)
  (and (vectorp conn) (>= (length conn) 6)
       (eq (aref conn 0) ngit-transport--tag)))

(defun ngit-transport--fail (format &rest args)
  (let ((message (apply #'format format args)))
    (if (fboundp 'ngit--die) (ngit--die "%s" message) (error "%s" message))))

(defun ngit-transport--hex (c)
  (cond ((and (>= c ?0) (<= c ?9)) (- c ?0))
        ((and (>= c ?a) (<= c ?f)) (+ 10 (- c ?a)))
        ((and (>= c ?A) (<= c ?F)) (+ 10 (- c ?A)))
        (t nil)))

(defun ngit-transport--percent-decode (string)
  "Decode percent escapes in STRING as path bytes."
  (let ((i 0) (n (length string)) (parts nil))
    (while (< i n)
      (if (and (= (aref string i) ?%) (< (+ i 2) n))
          (let ((a (ngit-transport--hex (aref string (1+ i))))
                (b (ngit-transport--hex (aref string (+ i 2)))))
            (unless (and a b)
              (ngit-transport--fail "fatal: invalid URL escape in '%s'" string))
            (push (string (+ (* a 16) b)) parts)
            (setq i (+ i 3)))
        (push (string (aref string i)) parts)
        (setq i (1+ i))))
    (decode-coding-string (apply #'concat (nreverse parts)) 'utf-8)))

(defun ngit-transport--parse-url (url)
  "Return (KIND . FIELDS) for supported URL."
  (cond
   ((string-prefix-p "file://" url)
    (let ((rest (substring url 7)))
      ;; Git's file transport ignores the authority and uses the local path.
      (unless (string-prefix-p "/" rest)
        (let ((slash (string-search "/" rest)))
          (unless slash (ngit-transport--fail "fatal: no path specified; see 'git help pull' for valid url syntax"))
          (setq rest (substring rest slash))))
      (cons 'local (list (ngit-transport--percent-decode rest)))))
   ((string-prefix-p "ssh://" url)
    (unless (string-match
             "\\`ssh://\\(?:\\([^/@]+\\)@\\)?\\(\\[[^]]+\\]\\|[^/:]+\\)\\(?::\\([0-9]+\\)\\)?\\(/.*\\)\\'"
             url)
      (ngit-transport--fail "fatal: invalid ssh URL: %s" url))
    (cons 'ssh (list (match-string 1 url) (match-string 2 url)
                     (match-string 3 url)
                     (ngit-transport--percent-decode (match-string 4 url)))))
   ((string-match "\\`[A-Za-z][A-Za-z0-9+.-]*://" url)
    (ngit-transport--fail
     "fatal: unable to find remote helper for '%s'"
     (substring url 0 (string-search ":" url))))
   ((string-match "\\`\\(?:\\([^/@:]+\\)@\\)?\\(\\[[^]]+\\]\\|[^/:]+\\):\\(.+\\)\\'" url)
    (cons 'ssh (list (match-string 1 url) (match-string 2 url) nil
                     (match-string 3 url))))
   (t (cons 'local (list url)))))

(defun ngit-transport--shell-quote (string)
  "Quote STRING as one POSIX shell word."
  (let ((i 0) (start 0) (n (length string)) (parts (list "'")))
    (while (< i n)
      (when (= (aref string i) ?')
        (push (substring string start i) parts)
        (push "'\"'\"'" parts)
        (setq start (1+ i)))
      (setq i (1+ i)))
    (push (substring string start) parts)
    (push "'" parts)
    (apply #'concat (nreverse parts))))

(defun ngit-transport--executable (name)
  (or (and (file-name-absolute-p name) (file-executable-p name) name)
      (executable-find name)
      (ngit-transport--fail "fatal: cannot run %s: No such file or directory" name)))

(defun ngit-transport--ssh-command (repo fields service)
  (let* ((user (nth 0 fields)) (host (nth 1 fields))
         (host (if (string-prefix-p "[" host) (substring host 1 -1) host))
         (port (nth 2 fields)) (path (nth 3 fields))
         (path (if (string-prefix-p "/~" path) (substring path 1) path))
         (configured (and (fboundp 'ngit-config-get)
                          (ngit-config-get repo "core.sshCommand")))
         (shell-command (or (getenv "GIT_SSH_COMMAND") configured))
         (git-ssh (getenv "GIT_SSH"))
         (dest (if user (concat user "@" host) host))
         (remote (concat service " " (ngit-transport--shell-quote path)))
         (args (append (and port (list "-p" port))
                       (and (string= service "git-upload-pack")
                            (list "-o" "SendEnv=GIT_PROTOCOL"))
                       (list dest remote))))
    (when (string-prefix-p "-" dest)
      (ngit-transport--fail "fatal: strange hostname '%s' blocked" dest))
    (if shell-command
        (list "/bin/sh" "-c"
              (concat "exec " shell-command " "
                      (mapconcat #'ngit-transport--shell-quote args " ")))
      (cons (ngit-transport--executable (or git-ssh "ssh")) args))))

(defun ngit-transport--env (protocol-v2)
  ;; Match connect.c: preserve identities, dates and SSH configuration while
  ;; removing variables that would make the server use the client's repository.
  (let ((local '("GIT_ALTERNATE_OBJECT_DIRECTORIES" "GIT_CONFIG" "GIT_CONFIG_PARAMETERS"
                 "GIT_CONFIG_COUNT" "GIT_OBJECT_DIRECTORY" "GIT_DIR" "GIT_WORK_TREE"
                 "GIT_IMPLICIT_WORK_TREE" "GIT_GRAFT_FILE" "GIT_INDEX_FILE"
                 "GIT_NO_REPLACE_OBJECTS" "GIT_REPLACE_REF_BASE" "GIT_PREFIX"
                 "GIT_INTERNAL_SUPER_PREFIX" "GIT_SHALLOW_FILE" "GIT_COMMON_DIR"
                 "GIT_PROTOCOL")) env)
    (dolist (entry process-environment)
      (let ((equal (string-search "=" entry)))
        (when (and equal (not (member (substring entry 0 equal) local)))
          (push entry env))))
    (when protocol-v2 (push "GIT_PROTOCOL=version=2" env))
    (nreverse env)))

(defun ngit-transport--cstring-vector (strings)
  "Return (OWNER . ADDRESS) holding a C string pointer vector."
  (let ((bytes 0))
    (dolist (s strings)
      (setq bytes (+ bytes 1 (length (encode-coding-string s 'utf-8)))))
    (let* ((slots (1+ (length strings)))
           (owner (nl-ffi-memory-allocate (+ (* 8 slots) bytes)))
           (base (nl-ffi-memory-address owner))
           (pos (* 8 slots)) (i 0))
      (dolist (s strings)
        (let ((encoded (encode-coding-string s 'utf-8)))
          (ptr-write-u64 base (* 8 i) (+ base pos))
          (ptr-write-bytes (+ base pos) encoded)
          (ptr-write-u8 base (+ pos (length encoded)) 0)
          (setq pos (+ pos (length encoded) 1) i (1+ i))))
      (ptr-write-u64 base (* 8 i) 0)
      (cons owner base))))

(defun ngit-transport--pipe ()
  (let* ((owner (nl-ffi-memory-allocate 8))
         (addr (nl-ffi-memory-address owner))
         (rc (syscall-direct ngit-transport--sys-pipe addr 0 0 0 0 0)))
    (if (< rc 0)
        (progn (nl-ffi-memory-release owner)
               (ngit-transport--fail "fatal: pipe failed (errno %d)" (- rc)))
      (prog1 (cons (ptr-read-u32 addr 0) (ptr-read-u32 addr 4))
        (nl-ffi-memory-release owner)))))

(defun ngit-transport--spawn (argv env &optional cwd stdout-mode no-stdin fallback report-exec)
  "Spawn ARGV with ENV and return a transport connection.
When CWD is non-nil, change to it before exec.  STDOUT-MODE t makes file
descriptors 1 and 2 identical; `inherit' leaves fd 1 inherited.
NO-STDIN connects standard input to /dev/null.  FALLBACK, when non-nil,
is exec'd after an ENOEXEC from ARGV (the hook shell-script fallback).
REPORT-EXEC adds a close-on-exec pipe used for exact hook diagnostics."
  (let* ((input (and (not no-stdin) (ngit-transport--pipe)))
         (output (and (not stdout-mode) (ngit-transport--pipe)))
         (exec-error (and report-exec (ngit-transport--pipe)))
         (error-owner (and report-exec (nl-ffi-memory-allocate 4)))
         (error-address (and error-owner (nl-ffi-memory-address error-owner)))
         (av (ngit-transport--cstring-vector argv))
         (ev (ngit-transport--cstring-vector env))
         (cv (and cwd (ngit-transport--cstring-vector (list cwd))))
         (dv (and no-stdin (ngit-transport--cstring-vector '("/dev/null"))))
         (fv (and fallback (ngit-transport--cstring-vector fallback)))
         (pid (syscall-direct ngit-transport--sys-fork 0 0 0 0 0 0)))
    (when exec-error
      (syscall-direct ngit-transport--sys-fcntl (cdr exec-error)
                      ngit-transport--f-setfd ngit-transport--fd-cloexec 0 0 0))
    (cond
     ((= pid 0)
      (when exec-error
        (syscall-direct ngit-transport--sys-close (car exec-error) 0 0 0 0 0))
      (when cv
        (let ((rc (syscall-direct ngit-transport--sys-chdir
                                  (ptr-read-u64 (cdr cv) 0) 0 0 0 0 0)))
          (when (< rc 0)
            (when exec-error
              (ptr-write-u32 error-address 0 (+ #x10000 (- rc)))
              (syscall-direct ngit-transport--sys-write (cdr exec-error)
                              error-address 4 0 0 0))
            (syscall-direct ngit-transport--sys-exit 127 0 0 0 0 0))))
      (if no-stdin
          (let ((fd (syscall-direct ngit-transport--sys-open
                                    (ptr-read-u64 (cdr dv) 0) 0 0 0 0 0)))
            (when (< fd 0)
              (syscall-direct ngit-transport--sys-exit 127 0 0 0 0 0))
            (syscall-direct ngit-transport--sys-dup2 fd 0 0 0 0 0)
            (syscall-direct ngit-transport--sys-close fd 0 0 0 0 0))
        (syscall-direct ngit-transport--sys-dup2 (car input) 0 0 0 0 0))
      (cond
       ((eq stdout-mode t)
        (syscall-direct ngit-transport--sys-dup2 2 1 0 0 0 0))
       ((null stdout-mode)
        (syscall-direct ngit-transport--sys-dup2 (cdr output) 1 0 0 0 0)))
      (when input
        (syscall-direct ngit-transport--sys-close (cdr input) 0 0 0 0 0)
        (syscall-direct ngit-transport--sys-close (car input) 0 0 0 0 0))
      (when output
        (syscall-direct ngit-transport--sys-close (car output) 0 0 0 0 0)
        (syscall-direct ngit-transport--sys-close (cdr output) 0 0 0 0 0))
      (let ((rc (syscall-direct ngit-transport--sys-execve
                                (ptr-read-u64 (cdr av) 0) (cdr av) (cdr ev) 0 0 0)))
        (when (and (= rc -8) fv)
          (setq rc (syscall-direct ngit-transport--sys-execve
                                   (ptr-read-u64 (cdr fv) 0) (cdr fv) (cdr ev) 0 0 0)))
        (when exec-error
          (ptr-write-u32 error-address 0 (- rc))
          (syscall-direct ngit-transport--sys-write (cdr exec-error)
                          error-address 4 0 0 0)))
      (syscall-direct ngit-transport--sys-exit 127 0 0 0 0 0))
     ((< pid 0)
      (dolist (fd (append (and input (list (car input) (cdr input)))
                          (and output (list (car output) (cdr output)))
                          (and exec-error (list (car exec-error) (cdr exec-error)))))
        (syscall-direct ngit-transport--sys-close fd 0 0 0 0 0))
      (nl-ffi-memory-release (car av))
      (nl-ffi-memory-release (car ev))
      (when cv (nl-ffi-memory-release (car cv)))
      (when dv (nl-ffi-memory-release (car dv)))
      (when fv (nl-ffi-memory-release (car fv)))
      (when error-owner (nl-ffi-memory-release error-owner))
      (ngit-transport--fail "fatal: fork failed (errno %d)" (- pid)))
     (t
      (when input
        (syscall-direct ngit-transport--sys-close (car input) 0 0 0 0 0))
      (when output
        (syscall-direct ngit-transport--sys-close (cdr output) 0 0 0 0 0))
      (when exec-error
        (syscall-direct ngit-transport--sys-close (cdr exec-error) 0 0 0 0 0))
      (nl-ffi-memory-release (car av))
      (nl-ffi-memory-release (car ev))
      (when cv (nl-ffi-memory-release (car cv)))
      (when dv (nl-ffi-memory-release (car dv)))
      (when fv (nl-ffi-memory-release (car fv)))
      (when error-owner (nl-ffi-memory-release error-owner))
      (if exec-error
          (vector ngit-transport--tag pid (if input (cdr input) -1)
                  (if output (car output) -1) nil nil (car exec-error) nil)
        (vector ngit-transport--tag pid (if input (cdr input) -1)
                (if output (car output) -1) nil nil))))))

(defun ngit-transport--exec-error (conn)
  "Read and close CONN's close-on-exec error pipe."
  (when (> (length conn) 6)
    (let* ((fd (aref conn 6))
           (owner (nl-ffi-memory-allocate 4))
           (address (nl-ffi-memory-address owner))
           (got -4))
      (while (= got -4)
        (setq got (syscall-direct ngit-transport--sys-read fd address 4 0 0 0)))
      (syscall-direct ngit-transport--sys-close fd 0 0 0 0 0)
      (aset conn 6 -1)
      (prog1 (and (= got 4) (ptr-read-u32 address 0))
        (nl-ffi-memory-release owner)))))

(defun ngit-transport--errno-string (number)
  (or (cdr (assq number '((2 . "No such file or directory")
                           (8 . "Exec format error")
                           (13 . "Permission denied")
                           (20 . "Not a directory"))))
      (format "errno %d" number)))

(defun ngit-transport-run-command (argv env cwd input &optional fallback inherit-stdout)
  "Run ARGV synchronously for a hook and return its numeric exit status.
ENV is a complete exec environment, CWD is the hook working directory,
and INPUT is a string or nil.  Nil uses /dev/null; a string, including an
empty one, is written to a pipe.  Hook stdout normally uses fd 2; when
INHERIT-STDOUT is non-nil it retains fd 1, as Git's pre-push runner does."
  (let* ((conn (ngit-transport--spawn argv env cwd
                                      (if inherit-stdout 'inherit t)
                                      (null input) fallback t))
         (exec-error (ngit-transport--exec-error conn)))
    (unwind-protect
        (if exec-error
            (progn
              (ngit-native-write-stderr
               (if (>= exec-error #x10000)
                   (format "fatal: cannot chdir to '%s': %s\n" cwd
                           (ngit-transport--errno-string (- exec-error #x10000)))
                 (format "fatal: cannot exec '%s': %s\n" (car argv)
                         (ngit-transport--errno-string exec-error))))
              (ngit-transport-finish-write conn)
              (ngit-transport-close conn)
              1)
          (progn
          (when input
            (condition-case err
                (ngit-transport-write conn input)
              ;; A hook is allowed to exit without consuming all pre-push
              ;; input.  Linux reports that as EPIPE after SIGPIPE is ignored
              ;; by the host runtime.
              (error
               (unless (string-search "errno 32" (error-message-string err))
                 (signal (car err) (cdr err))))))
          (ngit-transport-finish-write conn)
          (let ((status (ngit-transport-close conn)))
            (when (and (> (length conn) 7) (aref conn 7)
                       (not (memq (aref conn 7) '(2 3 13))))
              (ngit-native-write-stderr
               (format "error: %s died of signal %d\n" (car argv) (aref conn 7))))
            status)))
      (ngit-transport-finish-write conn)
      (when (>= (aref conn 3) 0)
        (syscall-direct ngit-transport--sys-close (aref conn 3) 0 0 0 0 0)
        (aset conn 3 -1)))))

(defun ngit-transport-open (repo url service)
  "Open URL for SERVICE and return a binary transport connection."
  (unless (member service '("git-upload-pack" "git-receive-pack"))
    (ngit-transport--fail "fatal: unsupported Git service: %s" service))
  (if (string-match-p "\\`https?://" url)
      (progn (require 'ngit-transport-http) (ngit-http-open repo url service))
    (let* ((parsed (ngit-transport--parse-url url))
         (ssh (eq (car parsed) 'ssh))
         (v2 (string= service "git-upload-pack"))
         (argv (if ssh
                   (ngit-transport--ssh-command repo (cdr parsed) service)
                 (list (ngit-transport--executable service)
                       (expand-file-name (car (cdr parsed)))))))
    (setcar argv (ngit-transport--executable (car argv)))
    (ngit-transport--spawn argv (ngit-transport--env v2)))))

(defun ngit-transport-write (conn bytes)
  "Write all of unibyte string BYTES to CONN."
  (unless (and (ngit-transport--conn-p conn) (stringp bytes))
    (signal 'wrong-type-argument (list 'ngit-transport-write conn bytes)))
  (when (and (fboundp 'ngit-http-p) (ngit-http-p conn))
    (when (aref (aref conn 6) 5) (ngit-transport--fail "fatal: HTTP RPC already sent"))
    (aset (aref conn 6) 8 (+ (aref (aref conn 6) 8) (string-bytes bytes))))
  (let* ((n (string-bytes bytes))
         (owner (nl-ffi-memory-allocate (max 1 n)))
         (addr (nl-ffi-memory-address owner)) (done 0))
    (unwind-protect
        (progn
          (ptr-write-bytes addr bytes)
          (while (< done n)
            (let ((rc (syscall-direct ngit-transport--sys-write (aref conn 2)
                                      (+ addr done) (- n done) 0 0 0)))
              (cond ((= rc -4))
                    ((< rc 0)
                (ngit-transport--fail "fatal: transport write failed (errno %d)" (- rc)))
                    (t (setq done (+ done rc))))))
          done)
      (nl-ffi-memory-release owner))))

(defun ngit-transport-read (conn count)
  "Read exactly COUNT bytes from CONN, or nil at clean EOF.
An EOF after a partial result is a truncated Git stream and signals an error."
  (unless (and (ngit-transport--conn-p conn) (integerp count) (>= count 0))
    (signal 'wrong-type-argument (list 'ngit-transport-read conn count)))
  (when (and (fboundp 'ngit-http-p) (ngit-http-p conn)) (ngit-http-ready conn))
  (if (= count 0) ""
    (if (aref conn 5) nil
      (let* ((owner (nl-ffi-memory-allocate count))
             (addr (nl-ffi-memory-address owner)) (done 0) (eof nil))
        (unwind-protect
            (progn
              (while (and (< done count) (not eof))
                (let ((rc (syscall-direct ngit-transport--sys-read (aref conn 3)
                                          (+ addr done) (- count done) 0 0 0)))
                  (cond ((< rc 0)
                         (unless (= rc -4)
                           (ngit-transport--fail "fatal: transport read failed (errno %d)" (- rc))))
                        ((= rc 0) (setq eof t) (aset conn 5 t))
                        (t (setq done (+ done rc))))))
              (cond ((= done count) (ptr-read-bytes addr count))
                    ((= done 0) nil)
                    (t (ngit-transport--fail
                        "fatal: unexpected EOF in Git transport (%d of %d bytes)"
                        done count))))
          (nl-ffi-memory-release owner))))))

(defun ngit-transport-send-file (conn path)
  "Stream PATH to CONN without constructing one whole-file Lisp string."
  (let ((size (ngit-native-file-size path)) (offset 0) (chunk (* 64 1024)))
    (while (< offset size)
      (let ((bytes (ngit-native-read-bytes path offset (min chunk (- size offset)))))
        (ngit-transport-write conn bytes)
        (setq offset (+ offset (length bytes)))))
    offset))

(defun ngit-transport-copy (conn fd count)
  "Copy exactly COUNT bytes from CONN to FD without Lisp payload strings."
  (when (and (fboundp 'ngit-http-p) (ngit-http-p conn)) (ngit-http-ready conn))
  (let* ((owner (ngit-native--buffer 0 (max 1 (min count 65536))))
         (address (nl-ffi-memory-address owner)) (left count))
    (while (> left 0)
      (let ((got -4))
        (while (= got -4)
          (setq got (syscall-direct ngit-transport--sys-read (aref conn 3)
                                    address (min left 65536) 0 0 0)))
        (when (<= got 0) (ngit-transport--fail "fatal: early EOF in pack stream"))
        (let ((done 0))
          (while (< done got)
            (let ((written (syscall-direct ngit-transport--sys-write fd
                                           (+ address done) (- got done) 0 0 0)))
              (cond ((= written -4))
                    ((<= written 0) (ngit-transport--fail "fatal: cannot write received pack"))
                    (t (setq done (+ done written)))))))
        (setq left (- left got))))
    count))

(defun ngit-transport-finish-write (conn)
  "Close CONN's input side."
  (unless (ngit-transport--conn-p conn)
    (signal 'wrong-type-argument (list 'ngit-transport-finish-write conn)))
  (when (and (not (and (fboundp 'ngit-http-p) (ngit-http-p conn)))
             (>= (aref conn 2) 0))
    (syscall-direct ngit-transport--sys-close (aref conn 2) 0 0 0 0 0)
    (aset conn 2 -1))
  nil)

(defun ngit-transport--wait (conn options)
  (let* ((owner (nl-ffi-memory-allocate 4))
         (addr (nl-ffi-memory-address owner))
         (rc -4))
    (while (= rc -4)
      (setq rc (syscall-direct ngit-transport--sys-wait4
                               (aref conn 1) addr options 0 0 0)))
    (prog1 (and (> rc 0) (ptr-read-u32 addr 0))
      (nl-ffi-memory-release owner))))

(defun ngit-transport--status (raw)
  (if (= (logand raw #x7f) 0)
      (logand (ash raw -8) #xff)
    (+ 128 (logand raw #x7f))))

(defun ngit-transport-close (conn)
  "Close CONN, let the server finish its work, reap it and return status."
  (unless (ngit-transport--conn-p conn)
    (signal 'wrong-type-argument (list 'ngit-transport-close conn)))
  (if (and (fboundp 'ngit-http-p) (ngit-http-p conn)) (ngit-http-close conn)
    (or (aref conn 4)
      (progn
        (ngit-transport-finish-write conn)
        (when (>= (aref conn 3) 0)
          (syscall-direct ngit-transport--sys-close (aref conn 3) 0 0 0 0 0)
          (aset conn 3 -1))
        (let ((raw (ngit-transport--wait conn 0)))
          (when (and (> (length conn) 7) (/= (logand raw #x7f) 0))
            (aset conn 7 (logand raw #x7f)))
          (aset conn 4 (ngit-transport--status raw)))))))

(defun ngit-transport-tty-p ()
  "Return non-nil when stderr is attached to a terminal."
  (and (fboundp 'file-tty-p) (file-tty-p "/dev/stderr")))

(provide 'ngit-transport-process)

;;; ngit-transport-process.el ends here
