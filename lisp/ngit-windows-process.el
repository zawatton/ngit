;;; ngit-windows-process.el --- Native Win64 subprocess I/O -*- lexical-binding: t; -*-

;;; Commentary:
;; Windows children run with an explicit UTF-16 command line, environment and
;; directory.  Anonymous pipes carry bytes without the runtime process bridge.
;; A raw OS-call worker drains duplex writes without executing Lisp in a thread.

;;; Code:
(require 'ngit-windows)

(defvar ngit-win-process--thunk nil)
(defvar ngit-win-process--last-error 0)
(defvar ngit-win-process--pending-writes nil
  "Owned (THREAD CALL-BLOCK DATA PIPE) still pending after bounded cancellation.")
(defconst ngit-win-process--queue-limit (* 16 1024 1024)
  "Maximum unread duplex stdout bytes; overflow aborts rather than truncates.")

(defun ngit-win-process--alloc (size)
  (ngit-map-anon (max 1 size)))

(defun ngit-win-process--thunk ()
  "Return the Win64 integer-call thunk, made RX before its first invocation."
  (or ngit-win-process--thunk
      (let* ((code (unibyte-string
                    #x53                         ; push rbx
                    #x48 #x83 #xec #x50           ; sub rsp,80
                    #x48 #x89 #xcb                ; mov rbx,rcx
                    #x48 #x8b #x03                ; mov rax,[rbx]
                    #x4c #x8b #x53 #x28 #x4c #x89 #x54 #x24 #x20
                    #x4c #x8b #x53 #x30 #x4c #x89 #x54 #x24 #x28
                    #x4c #x8b #x53 #x38 #x4c #x89 #x54 #x24 #x30
                    #x4c #x8b #x53 #x40 #x4c #x89 #x54 #x24 #x38
                    #x4c #x8b #x53 #x48 #x4c #x89 #x54 #x24 #x40
                    #x4c #x8b #x53 #x50 #x4c #x89 #x54 #x24 #x48
                    #x48 #x8b #x4b #x08           ; rcx = arg1
                    #x48 #x8b #x53 #x10           ; rdx = arg2
                    #x4c #x8b #x43 #x18           ; r8  = arg3
                    #x4c #x8b #x4b #x20           ; r9  = arg4
                    #xff #xd0                    ; call rax
                    #x48 #x89 #x43 #x58           ; result = rax
                    #x48 #x8b #x43 #x60 #xff #xd0 ; call saved GetLastError
                    #x48 #x89 #x43 #x68           ; save thread-local error
                    #x48 #x8b #x43 #x58           ; restore result
                    #x48 #x83 #xc4 #x50 #x5b #xc3))
             (address (ngit-win-process--alloc (length code)))
             (old (ngit-win-process--alloc 4))
             (ok nil))
        ;; Entry RSP is 8 modulo 16.  Saving nonvolatile RBX and subtracting
        ;; 80 aligns the call site, reserves 32 bytes of shadow space and
        ;; places arguments 5..10 at RSP+32..72.  The block is 12 uint64s:
        ;; function, ten integer/pointer arguments, full-width result.
        ;; Two additional uint64s hold GetLastError and its thread-local value.
        ;; The same one-pointer ABI is valid as a CreateThread callback.
        (unwind-protect
            (progn
              (ptr-write-bytes address code)
              (unless (and (/= 0 (ngit-win-call "kernel32.dll" "VirtualProtect"
                                              address (length code) #x20 old))
                           (/= 0 (ngit-win-call "kernel32.dll" "FlushInstructionCache"
                                              -1 address (length code))))
                (error "Cannot make Windows OS-call thunk executable"))
              (setq ngit-win-process--thunk address ok t)
              address)
          (ngit-win-free old)
          (unless ok (ngit-win-free address))))))

(defun ngit-win-process--call-block (dll name args)
  (when (> (length args) 10) (error "Windows call exceeds ten arguments"))
  (let* ((function (ngit-win-symbol dll name))
         (last-error (ngit-win-symbol "kernel32.dll" "GetLastError"))
         (block (ngit-win-process--alloc 112)) (offset 8))
    (ptr-write-u64 block 0 function)
    (ptr-write-u64 block 96 last-error)
    (dolist (arg args)
      (ptr-write-u64 block offset arg)
      (setq offset (+ offset 8)))
    block))

(defun ngit-win-call-n (dll name args)
  "Call Win64 DLL NAME with up to ten integer/pointer ARGS."
  (let ((block (ngit-win-process--call-block dll name args)))
    (unwind-protect
        (progn
          (ptr-call (ngit-win-process--thunk) block 0 0 0 0 0)
          (setq ngit-win-process--last-error (ptr-read-u64 block 104))
          (ptr-read-u64 block 88))
      (ngit-win-free block))))

(defun ngit-win-process--close-handle (handle)
  (when (and handle (> handle 0) (/= handle #xffffffffffffffff))
    (ngit-win-call "kernel32.dll" "CloseHandle" handle)))

(defun ngit-win-process--quote (arg)
  "Quote one argument according to the Windows CRT command-line grammar."
  (when (string-search (string 0) arg) (error "NUL in process argument"))
  (if (and (> (length arg) 0) (not (string-match-p "[ \t\n\v\"]" arg))) arg
  (let ((out (list "\"")) (slashes 0))
    (dolist (ch (string-to-list arg))
      (cond ((= ch ?\\) (setq slashes (1+ slashes)))
            ((= ch ?\")
             (push (make-string (1+ (* 2 slashes)) ?\\) out)
             (push "\"" out) (setq slashes 0))
            (t (push (make-string slashes ?\\) out)
               (push (char-to-string ch) out) (setq slashes 0))))
    (push (make-string (* 2 slashes) ?\\) out)
    (push "\"" out)
    (apply #'concat (nreverse out)))))

(defun ngit-win-process-shell (command)
  "Return an explicitly selected POSIX shell or the native command processor."
  (let ((shell (getenv "NGIT_WINDOWS_SHELL")))
    (if (and shell (not (equal shell ""))) (list shell "-c" command)
      (list (or (getenv "COMSPEC") (getenv "ComSpec")
                (let ((root (or (getenv "SystemRoot") (getenv "SYSTEMROOT")
                                (getenv "WINDIR") (getenv "windir"))))
                  (and root (expand-file-name "System32/cmd.exe" root)))
                (executable-find "cmd.exe")
                (error "Cannot find the native Windows command processor"))
            "/d" "/s" "/c" command))))

(defun ngit-win-process--command-line (argv)
  ;; cmd.exe parses /c's tail itself; CRT backslash escaping changes commands.
  (if (and (= (length argv) 5)
           (equal (downcase (file-name-nondirectory (car argv))) "cmd.exe")
           (equal (mapcar #'downcase (list (nth 1 argv) (nth 2 argv) (nth 3 argv)))
                  '("/d" "/s" "/c")))
      (progn
        (when (string-search (string 0) (nth 4 argv)) (error "NUL in shell command"))
        (concat (ngit-win-process--quote (car argv)) " /d /s /c \"" (nth 4 argv) "\""))
    (mapconcat #'ngit-win-process--quote argv " ")))

(defun ngit-win-process--pipe ()
  "Return an inheritable (READ . WRITE) anonymous pipe."
  (let ((memory (ngit-win-process--alloc 40)))
    (unwind-protect
        (progn
          (ptr-write-u32 memory 16 24)
          (ptr-write-u32 memory 32 1)
          (unless (/= 0 (ngit-win-call "kernel32.dll" "CreatePipe"
                                      memory (+ memory 8) (+ memory 16) 65536))
            (error "CreatePipe failed (%d)" (ngit-win-call "kernel32.dll" "GetLastError")))
          (cons (ptr-read-u64 memory 0) (ptr-read-u64 memory 8)))
      (ngit-win-free memory))))

(defun ngit-win-process--environment (environment)
  "Allocate a sorted, double-NUL terminated UTF-16 environment block."
  (dolist (entry environment)
    (when (string-search (string 0) entry) (error "NUL in process environment")))
  (let* ((text (concat (mapconcat #'identity
                                 (sort (copy-sequence environment)
                                       (lambda (a b) (string-lessp (downcase a) (downcase b))))
                                 (string 0)) (string 0 0)))
         (bytes (encode-coding-string text 'utf-8))
         (source (ngit-win-process--alloc (length bytes)))
         (target (ngit-win-process--alloc (* 2 (length bytes)))) (ok nil))
    (unwind-protect
        (progn
          (ptr-write-bytes source bytes)
          (when (= 0 (ngit-win-call "kernel32.dll" "MultiByteToWideChar"
                                    65001 8 source (length bytes) target (length bytes)))
            (error "Cannot encode Windows process environment"))
          (setq ok t)
          target)
      (ngit-win-free source)
      (unless ok (ngit-win-free target)))))

(defun ngit-win-process-p (conn)
  (and (vectorp conn) (> (length conn) 10) (eq (aref conn 10) 'windows)))

(defun ngit-win-process-executable (name)
  "Resolve NAME using native semicolon PATH and Windows .exe suffix rules."
  (let ((wide (ngit-win-wide name)) (extension (ngit-win-wide ".exe"))
        (path (let* ((base (or (getenv "PATH") (getenv "Path")))
                     (exec (and (member name '("git-upload-pack" "git-receive-pack"
                                               "git-upload-pack.exe" "git-receive-pack.exe"))
                                (getenv "GIT_EXEC_PATH")))
                     (value (if (and exec (not (equal exec "")))
                                (concat exec (and base ";") base) base)))
                (and value (ngit-win-wide value))))
        (buffer (ngit-win-process--alloc 65536)))
    (unwind-protect
        (let ((size (ngit-win-call "kernel32.dll" "SearchPathW"
                                  (or path 0) wide extension 32768 buffer 0)))
          (and (> size 0) (< size 32768) (ngit-win-read-wide buffer size)))
      (ngit-win-free wide) (ngit-win-free extension)
      (when path (ngit-win-free path)) (ngit-win-free buffer))))

(defun ngit-win-process-spawn (argv env cwd stdout-mode no-stdin fallback
                                  &optional discard-stderr isolated-job)
  "Spawn native ARGV, preserving the transport connection's public slots."
  (let ((input nil) (output nil) (owners nil) (handles nil) (success nil)
        (process nil) (thread nil) (conn nil) (job nil) (null-handle nil))
    (unwind-protect
        (progn
          (unless (eq no-stdin 'inherit)
            (setq input (ngit-win-process--pipe)
                  handles (list (car input) (cdr input))))
          (unless stdout-mode
            (setq output (ngit-win-process--pipe)
                  handles (append (list (car output) (cdr output)) handles)))
          (dolist (handle (append (and input (list (cdr input))) (and output (list (car output)))))
            (unless (/= 0 (ngit-win-call "kernel32.dll" "SetHandleInformation" handle 1 0))
              (error "Cannot protect parent pipe handle")))
          (let* ((application (car (push (ngit-win-wide (car argv)) owners)))
                 (command (car (push (ngit-win-wide (ngit-win-process--command-line argv)) owners)))
                 ;; CreateProcess requires a sorted, double-NUL UTF-16 block.
                 (environment (car (push (ngit-win-process--environment env) owners)))
                 (directory (and cwd (car (push (ngit-win-wide cwd) owners))))
                 (startup (car (push (ngit-win-process--alloc 104) owners)))
                 (information (car (push (ngit-win-process--alloc 24) owners))))
            (when discard-stderr
              (let ((name (car (push (ngit-win-wide "NUL") owners))))
                (setq null-handle (ngit-win-call-n
                                   "kernel32.dll" "CreateFileW"
                                   (list name #x40000000 3 0 3 #x80 0)))
                (when (or (= null-handle #xffffffffffffffff) (= null-handle -1))
                  (error "Cannot open the Windows null device"))
                (unless (/= 0 (ngit-win-call "kernel32.dll" "SetHandleInformation" null-handle 1 1))
                  (error "Cannot inherit the Windows null device"))))
            (when isolated-job
              (setq job (ngit-win-call "kernel32.dll" "CreateJobObjectW" 0 0))
              (when (= job 0) (error "Cannot create credential process job"))
              (let ((limits (car (push (ngit-win-process--alloc 144) owners))))
                (ptr-write-u32 limits 16 #x2000) ; JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
                (unless (/= 0 (ngit-win-call "kernel32.dll" "SetInformationJobObject" job 9 limits 144))
                  (error "Cannot constrain credential process job"))))
            (ptr-write-u32 startup 0 104)
            (ptr-write-u32 startup 60 #x100) ; STARTF_USESTDHANDLES
            (ptr-write-u64 startup 80 (if input (car input)
                                       (ngit-win-call "kernel32.dll" "GetStdHandle" -10)))
            (ptr-write-u64 startup 88
                           (cond ((eq stdout-mode t) (ngit-win-call "kernel32.dll" "GetStdHandle" -12))
                                 (stdout-mode (ngit-win-call "kernel32.dll" "GetStdHandle" -11))
                                 (t (cdr output))))
            (ptr-write-u64 startup 96 (or null-handle (ngit-win-call "kernel32.dll" "GetStdHandle" -12)))
            (let ((ok (ngit-win-call-n
                       "kernel32.dll" "CreateProcessW"
                       (list application command 0 0 1 (if job #x404 #x400) environment
                             (or directory 0) startup information))))
              (when (= ok 0)
                (let ((code ngit-win-process--last-error))
                  (if (and fallback (memq code '(193 216)))
                      (setq conn (ngit-win-process-spawn fallback env cwd stdout-mode no-stdin nil
                                                         discard-stderr isolated-job)
                            success t)
                    (error "Cannot start %s (Windows error %d)" (car argv) code)))))
            (unless conn
              (setq process (ptr-read-u64 information 0)
                    thread (ptr-read-u64 information 8))
              (when job
                (unless (/= 0 (ngit-win-call "kernel32.dll" "AssignProcessToJobObject" job process))
                  (error "Cannot assign credential child to its process job"))
                (when (= #xffffffff (ngit-win-call "kernel32.dll" "ResumeThread" thread))
                  (error "Cannot resume credential child")))
              (ngit-win-process--close-handle thread)
              (setq thread nil)
              (when input
                (ngit-win-process--close-handle (car input))
                (setq handles (delq (car input) handles)))
              (when output
                (ngit-win-process--close-handle (cdr output))
                (setq handles (delq (cdr output) handles)))
              (setq conn (vector 'ngit-transport process (if input (cdr input) -1)
                                 (if output (car output) -1) nil nil nil nil nil nil 'windows nil job))
              (when no-stdin (ngit-win-process-finish-write conn))
              (setq success t)))
          conn)
      (dolist (owner owners) (when owner (ngit-win-free owner)))
      (ngit-win-process--close-handle thread)
      (ngit-win-process--close-handle null-handle)
      (unless (and success (> (length conn) 12) (eq job (aref conn 12)))
        (ngit-win-process--close-handle job))
      (unless success
        (when process
          (ngit-win-call "kernel32.dll" "TerminateProcess" process 127)
          (ngit-win-call "kernel32.dll" "WaitForSingleObject" process #xffffffff)
          (ngit-win-process--close-handle process)))
      ;; On success only the connection's parent ends stay open.
      (dolist (handle handles)
        (unless (and success (or (= handle (aref conn 2)) (= handle (aref conn 3))))
          (ngit-win-process--close-handle handle))))))

(defun ngit-win-process--drain (conn)
  "Queue at most four 64KiB stdout chunks, then return to the writer poll.
Bound unread output by `ngit-win-process--queue-limit' and any tighter
connection limit.  Signal before exceeding the bound; never discard bytes."
  (when (>= (aref conn 3) 0)
    (let ((memory (ngit-win-process--alloc 65544)) (chunks 0)
          (limit (min ngit-win-process--queue-limit
                      (or (and (> (length conn) 13) (aref conn 13))
                          ngit-win-process--queue-limit))))
      (unwind-protect
          (while (and (< chunks 4)
                      (/= 0 (ngit-win-call "kernel32.dll" "PeekNamedPipe"
                                          (aref conn 3) 0 0 0 memory 0))
                      (> (ptr-read-u32 memory 0) 0))
            (let ((size (min 65536 (ptr-read-u32 memory 0))))
              (when (> (+ (length (or (aref conn 11) "")) size) limit)
                (error "Windows child output limit exceeded (%d bytes)" limit))
              (unless (/= 0 (ngit-win-call "kernel32.dll" "ReadFile"
                                          (aref conn 3) (+ memory 8) size (+ memory 4) 0))
                (error "Cannot drain Windows child output"))
              (aset conn 11 (concat (aref conn 11)
                                    (ptr-read-bytes (+ memory 8) (ptr-read-u32 memory 4))))
              (setq chunks (1+ chunks))))
        (ngit-win-free memory)))))

(defun ngit-win-process-write (conn bytes &optional deadline)
  "Write binary BYTES while draining the child's stdout to avoid deadlocks."
  (let (pending)
    (dolist (entry ngit-win-process--pending-writes)
      (if (= 0 (ngit-win-call "kernel32.dll" "WaitForSingleObject" (nth 0 entry) 0))
          (progn (ngit-win-process--close-handle (nth 0 entry))
                 (ngit-win-free (nth 1 entry)) (ngit-win-free (nth 2 entry))
                 (ngit-win-process--close-handle (nth 3 entry)))
        (push entry pending)))
    (setq ngit-win-process--pending-writes (nreverse pending)))
  (let* ((size (string-bytes bytes)) (memory (ngit-win-process--alloc (+ 4 size)))
         (block nil) (thread nil) (done 0))
    (unwind-protect
        (progn
          (ptr-write-bytes (+ memory 4) bytes)
          (while (< done size)
            (setq block (ngit-win-process--call-block
                         "kernel32.dll" "WriteFile"
                         (list (aref conn 2) (+ memory 4 done) (- size done) memory 0))
                  thread (ngit-win-call "kernel32.dll" "CreateThread"
                                       0 0 (ngit-win-process--thunk) block 0 0))
            (when (= thread 0) (error "Cannot create Windows pipe writer"))
            (while (= 258 (ngit-win-call "kernel32.dll" "WaitForSingleObject" thread 0))
              (when (and deadline (>= (ngit-win-call "kernel32.dll" "GetTickCount64") deadline))
                (error "Windows child input deadline expired"))
              (ngit-win-process--drain conn)
              (ngit-win-call "kernel32.dll" "Sleep" 1))
            (when (or (= (ptr-read-u64 block 88) 0) (= (ptr-read-u32 memory 0) 0))
              (error "fatal: transport write failed (Windows error %d, wrote %d)"
                     (ptr-read-u64 block 104) (ptr-read-u32 memory 0)))
            (setq done (+ done (ptr-read-u32 memory 0)))
            (ngit-win-process--close-handle thread)
            (setq thread nil)
            (ngit-win-free block) (setq block nil))
          done)
      (when (and thread (/= thread 0))
        ;; Cancellation can race the worker's first WriteFile.  Tear down the
        ;; owned peer first, then retry cancellation with bounded joins.  A
        ;; still-running worker retains its call block and data, never dangling
        ;; pointers; a later write reaps that explicitly owned retired record.
        (when (and (> (length conn) 12) (aref conn 12))
          (ngit-win-call "kernel32.dll" "TerminateJobObject" (aref conn 12) 143))
        (when (> (aref conn 1) 0)
          (ngit-win-call "kernel32.dll" "TerminateProcess" (aref conn 1) 143))
        (let ((attempt 0) (complete nil))
          (while (and (< attempt 20) (not complete))
            (ngit-win-call "kernel32.dll" "CancelSynchronousIo" thread)
            (setq complete (= 0 (ngit-win-call "kernel32.dll" "WaitForSingleObject" thread 50))
                  attempt (1+ attempt)))
          (if complete (ngit-win-process--close-handle thread)
            ;; Retain the pipe too: its numeric HANDLE must not be closed and
            ;; reused before a delayed callback issues its first WriteFile.
            (push (list thread block memory (aref conn 2)) ngit-win-process--pending-writes)
            (aset conn 2 -1)
            (setq block nil memory nil))))
      (when block (ngit-win-free block))
      (when memory (ngit-win-free memory)))))

(defun ngit-win-process-read (conn count)
  "Read exactly COUNT binary bytes, returning nil at clean EOF."
  (if (= count 0) ""
    (let* ((queued (or (aref conn 11) ""))
           (prefix (substring queued 0 (min count (length queued))))
           (done (length prefix)) (memory (ngit-win-process--alloc (+ 4 count)))
           (eof (aref conn 5)))
      (aset conn 11 (substring queued done))
      (unwind-protect
          (progn
            (ptr-write-bytes (+ memory 4) prefix)
            (while (and (< done count) (not eof))
              (let ((ok (ngit-win-call "kernel32.dll" "ReadFile"
                                      (aref conn 3) (+ memory 4 done) (- count done) memory 0)))
                (cond ((/= ok 0)
                       (if (= 0 (ptr-read-u32 memory 0)) (setq eof t)
                         (setq done (+ done (ptr-read-u32 memory 0)))))
                      ((memq (ngit-win-call "kernel32.dll" "GetLastError") '(109 232))
                       (setq eof t))
                      (t (error "fatal: transport read failed (Windows)")))))
            (when eof (aset conn 5 t))
            (cond ((= done count) (ptr-read-bytes (+ memory 4) count))
                  ((= done 0) nil)
                  (t (error "fatal: unexpected EOF in Git transport (%d of %d bytes)" done count))))
        (ngit-win-free memory)))))

(defun ngit-win-process-finish-write (conn)
  (when (>= (aref conn 2) 0)
    (ngit-win-process--close-handle (aref conn 2))
    (aset conn 2 -1)))

(defun ngit-win-process-close (conn &optional abort)
  (or (aref conn 4)
      (progn
        (ngit-win-process-finish-write conn)
        (when abort (ngit-win-call "kernel32.dll" "TerminateProcess" (aref conn 1) 143))
        (when (>= (aref conn 3) 0)
          (ngit-win-process--close-handle (aref conn 3)) (aset conn 3 -1))
        (unless (= 0 (ngit-win-call "kernel32.dll" "WaitForSingleObject" (aref conn 1) #xffffffff))
          (error "Cannot wait for Windows child"))
        (let ((memory (ngit-win-process--alloc 4)))
          (unwind-protect
              (progn
                (unless (/= 0 (ngit-win-call "kernel32.dll" "GetExitCodeProcess" (aref conn 1) memory))
                  (error "Cannot obtain Windows child exit status"))
                (aset conn 4 (ptr-read-u32 memory 0))
                (ngit-win-process--close-handle (aref conn 1))
                (when (and (> (length conn) 12) (aref conn 12))
                  (ngit-win-process--close-handle (aref conn 12))
                  (aset conn 12 nil))
                (aset conn 1 -1)
                (aref conn 4))
            (ngit-win-free memory))))))

(provide 'ngit-windows-process)
;;; ngit-windows-process.el ends here
