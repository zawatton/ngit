;;; ngit-status-watch.el --- owned raw Linux inotify watches -*- lexical-binding: t; -*-

;;; Commentary:

;; Linux x86_64 byte-path provider only.  Coverage and Git policy belong to
;; the caller.  A lost watcher must be closed and replaced, never repaired
;; in place: queued watch descriptors can otherwise acquire new identities.
;; No ACCESS, OPEN or CLOSE_NOWRITE subscriptions: reads must remain idle.

;;; Code:

(require 'ngit-native)

(defconst ngit-status-watch--mask #x00002fce)
(defconst ngit-status-watch--structural-mask #x0000afc0)
(defconst ngit-status-watch--loss-mask #x0000ac00)
(defconst ngit-status-watch--known-mask #x4000efce)
(defconst ngit-status-watch--buffer-size 65536)
(defconst ngit-status-watch--watch-limit 65536)
(defconst ngit-status-watch--event-limit 4096)
(defvar ngit-status-watch--generation 0)

(defun ngit-status-watch--check (watcher)
  "Reject values that are not watcher handles."
  (unless (and (vectorp watcher) (= (length watcher) 9)
               (eq (aref watcher 0) 'ngit-status-watch))
    (error "ngit: invalid status watcher")))

;;;###autoload
(defun ngit-status-watch-close (watcher)
  "Release WATCHER's descriptor and memory; repeated calls are harmless.
Linux close is never retried, including after EINTR, to avoid closing a
descriptor that another operation may have reused."
  (ngit-status-watch--check watcher)
  (let ((fd (aref watcher 1)) (owner (aref watcher 2)))
    (aset watcher 1 nil)
    (aset watcher 2 nil)
    (aset watcher 3 nil)
    (aset watcher 5 t)
    (unwind-protect
        (when fd (syscall-direct 3 fd 0 0 0 0 0))
      (when owner (nl-ffi-memory-release owner))))
  nil)

(defun ngit-status-watch--identity (path-address stat-address)
  "Return device, inode and type for a non-symlink PATH-ADDRESS."
  (let ((rc (syscall-direct 6 path-address stat-address 0 0 0 0)))
    (when (< rc 0) (error "ngit: cannot stat watch path (errno %d)" (- rc)))
    (let ((kind (logand (ptr-read-u32 stat-address 24) #o170000)))
      (unless (or (= kind #o040000) (= kind #o100000))
        (error "ngit: watch path must be a regular file or directory"))
      (list (ptr-read-u64 stat-address 0) (ptr-read-u64 stat-address 8) kind))))

;;;###autoload
(defun ngit-status-watch-open (paths)
  "Watch explicit existing absolute byte PATHS and return an owned handle.
PATHS are ngit's UTF-8 byte strings, not decoded text.  Missing paths,
symlinks, registration races and a reached watch budget fail the entire
open.  Every pathname alias of a shared kernel descriptor is retained."
  ;; Slots: marker, fd, buffer owner, wd -> ((path . directoryp) ...),
  ;; generation, sticky lost, sticky kernel overflow, reason, budget flag.
  (let ((watcher (vector 'ngit-status-watch nil nil
                         (make-hash-table :test 'eql) 0 nil nil nil nil))
        (complete nil) (count 0))
    (unwind-protect
        (progn
          (aset watcher 2 (nl-ffi-memory-allocate ngit-status-watch--buffer-size))
          (let ((fd (syscall-direct 294 526336 0 0 0 0 0)))
            (when (< fd 0) (error "ngit: inotify_init1 failed (errno %d)" (- fd)))
            (aset watcher 1 fd))
          (dolist (path paths)
            (setq count (1+ count))
            (when (>= count ngit-status-watch--watch-limit)
              (error "ngit: status watch coverage budget reached"))
            (unless (and (stringp path) (> (length path) 0)
                         (= (aref path 0) ?/) (< (length path) 4096))
              (error "ngit: invalid absolute watch byte path"))
            (let ((i 0))
              (while (< i (length path))
                (let ((byte (aref path i)))
                  (unless (and (> byte 0) (<= byte 255))
                    (error "ngit: watch path contains NUL or non-byte character")))
                (setq i (1+ i))))
            (let* ((address (nl-ffi-memory-address (aref watcher 2)))
                   (stat-address (+ address 4096)))
              (ptr-write-bytes address path)
              (ptr-write-u8 address (length path) 0)
              (let* ((before (ngit-status-watch--identity address stat-address))
                     (directoryp (= (nth 2 before) #o040000))
                     ;; DONT_FOLLOW plus ONLYDIR closes the type race; the
                     ;; second lstat rejects replacement during registration.
                     (mask (logior ngit-status-watch--mask #x02000000
                                   (if directoryp #x01000000 0)))
                     (wd (syscall-direct 254 (aref watcher 1) address mask 0 0 0)))
                (when (< wd 0)
                  (error "ngit: inotify_add_watch failed (errno %d)" (- wd)))
                (unless (equal before (ngit-status-watch--identity address stat-address))
                  (error "ngit: watch path changed during registration"))
                (let* ((table (aref watcher 3))
                       (bindings (gethash wd table))
                       (binding (cons (copy-sequence path) directoryp)))
                  (unless (member binding bindings)
                    (puthash wd (cons binding bindings) table))))))
          (setq ngit-status-watch--generation (1+ ngit-status-watch--generation))
          (aset watcher 4 ngit-status-watch--generation)
          (setq complete t)
          watcher)
      (unless complete (ngit-status-watch-close watcher)))))

;;;###autoload
(defun ngit-status-watch-drain (watcher)
  "Drain WATCHER without blocking and return a change/health plist.
:paths contains absolute byte paths; :events counts consumed kernel
records.  :generation changes on non-idle drains.  :structural includes
create/delete/move and identity loss; :structural-paths is the subset of
aliased paths associated with those events.  Unlocatable events and watch
loss still set :lost, regardless of that subset, and cannot be filtered
away by callers.  :overflow means actual IN_Q_OVERFLOW;
the distinct :budget-exhausted flag means the local processing bound was
reached.  Both make :lost sticky until the caller opens a new watcher.
Errors and unknown descriptors also produce sticky :lost, never reuse."
  (ngit-status-watch--check watcher)
  (let ((paths (make-hash-table :test 'equal)) (changed nil)
        (structural-paths (make-hash-table :test 'equal)) (structural-changed nil)
        (structural (aref watcher 5)) (events 0) (done nil) (interrupts 0))
    (unless (aref watcher 1)
      (error "ngit: status watcher is closed"))
    (condition-case nil
        (while (and (not done) (not (aref watcher 5)))
          (let* ((address (nl-ffi-memory-address (aref watcher 2)))
                 (n (syscall-direct 0 (aref watcher 1) address
                                    ngit-status-watch--buffer-size 0 0 0)))
            (cond
             ((= n -11) (setq done t)) ; EAGAIN is a completed drain.
             ((= n -4)
              (setq interrupts (1+ interrupts))
              (when (>= interrupts 8)
                (aset watcher 5 t) (aset watcher 7 'interrupted)))
             ((<= n 0) (aset watcher 5 t) (aset watcher 7 'read-error))
             (t
              (let ((offset 0))
                (while (and (< offset n) (not (aref watcher 5)))
                  (when (< (- n offset) 16) (error "ngit: truncated inotify header"))
                  (let* ((record (+ address offset))
                         (wd (ptr-read-u32 record 0))
                         (mask (ptr-read-u32 record 4))
                         (size (ptr-read-u32 record 12))
                         (end (+ offset 16 size))
                         (structural-event
                          (/= 0 (logand mask ngit-status-watch--structural-mask)))
                         (bindings (gethash wd (aref watcher 3))))
                    (when (> end n) (error "ngit: truncated inotify name"))
                    (setq events (1+ events))
                    (when structural-event (setq structural t))
                    (cond
                     ((/= 0 (logand mask #x4000))
                      (aset watcher 6 t) (aset watcher 5 t)
                      (aset watcher 7 'kernel-overflow))
                     ((or (not bindings)
                          (/= 0 (logand mask (lognot ngit-status-watch--known-mask))))
                      (aset watcher 5 t) (aset watcher 7 'unknown-event))
                     (t
                      (let ((length 0) (name ""))
                        (while (and (< length size)
                                    (/= 0 (ptr-read-u8 record (+ 16 length))))
                          (setq length (1+ length)))
                        (when (> size 0)
                          (when (= length size) (error "ngit: unterminated inotify name"))
                          ;; ptr-read-bytes preserves raw UTF-8 filename bytes.
                          (setq name (ptr-read-bytes (+ record 16) length)))
                        (dolist (binding bindings)
                          (when (and (cdr binding) (= length 0)
                                     (/= 0 (logand mask #x3c0)))
                            (error "ngit: unlocatable inotify child event"))
                          (let* ((base (car binding))
                                 (path (if (and (cdr binding) (> length 0))
                                           (concat base
                                                   (if (= (aref base (1- (length base))) ?/)
                                                       "" "/") name)
                                         base)))
                            (unless (gethash path paths)
                              (puthash path t paths)
                              (setq changed (cons path changed)))
                            (when (and structural-event
                                       (not (gethash path structural-paths)))
                              (puthash path t structural-paths)
                              (setq structural-changed (cons path structural-changed))))))
                      (when (/= 0 (logand mask ngit-status-watch--loss-mask))
                        (aset watcher 5 t) (aset watcher 7 'watch-loss))))
                    (setq offset end)
                    ;; This is a userspace budget, not kernel queue overflow.
                    ;; Even an exactly exhausted batch is uncertified: do not
                    ;; discard remaining bytes and later claim an idle queue.
                    (when (>= events ngit-status-watch--event-limit)
                      (aset watcher 5 t) (aset watcher 8 t)
                      (unless (aref watcher 7) (aset watcher 7 'event-budget))))))))))
      (error (aset watcher 5 t) (aset watcher 7 'provider-error)))
    (when (or (> events 0) (and (aref watcher 5) (not structural)))
      (setq ngit-status-watch--generation (1+ ngit-status-watch--generation))
      (aset watcher 4 ngit-status-watch--generation))
    (list :paths (nreverse changed) :structural (or structural (aref watcher 5))
          :structural-paths (nreverse structural-changed)
          :overflow (aref watcher 6) :lost (aref watcher 5)
          :events events :generation (aref watcher 4)
          :reason (aref watcher 7) :budget-exhausted (aref watcher 8))))

(provide 'ngit-status-watch)
;;; ngit-status-watch.el ends here
