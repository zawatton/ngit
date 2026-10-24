;;; ngit-transport-delta.el --- gather Git deltas in one kernel copy -*- lexical-binding: t; -*-

;;; Commentary:

;; Decode delta instructions in Lisp, then gather every base and literal span
;; into one contiguous external buffer with a single process_vm_readv syscall.
;; This avoids one expensive FFI transition per copy instruction.  The caller
;; owns the returned memory owner and must release it.

;;; Code:

(require 'ngit-native)

(defconst ngit-transport-delta--sys-getpid 39)
(defconst ngit-transport-delta--sys-close 3)
(defconst ngit-transport-delta--sys-pread64 17)
(defconst ngit-transport-delta--sys-writev 20)
(defconst ngit-transport-delta--sys-process-vm-readv 310)
(defconst ngit-transport-delta--sys-memfd-create 319)
(defconst ngit-transport-delta--iov-max 1024)
(defvar ngit-transport-delta--io-byte-limit #x7ffff000)
(defvar ngit-transport-delta--process-vm-readv-p t)
(defvar ngit-transport-delta--use-kernel t)

(defun ngit-transport-delta--gather-fallback
    (iov-address piece-count output-address output-length)
  "Gather PIECE-COUNT spans with bounded writev and pread calls."
  (let* ((name-owner (nl-ffi-memory-allocate 16))
         (name (nl-ffi-memory-address name-owner)) fd)
    (unwind-protect
        (progn
          (ptr-write-bytes name "ngit-delta")
          (ptr-write-u8 name 10 0)
          (setq fd (syscall-direct ngit-transport-delta--sys-memfd-create
                                   name 1 0 0 0 0))
          (when (< fd 0) (error "ngit: memfd_create failed (errno %d)" (- fd)))
          (let ((first 0) (total 0))
            (while (< first piece-count)
              (let* ((address (+ iov-address 16 (* first 16)))
                     (count 0) (expected 0) clipped-length stop)
                (while (and (< (+ first count) piece-count)
                            (< count ngit-transport-delta--iov-max)
                            (not stop))
                  (let ((length (ptr-read-u64 address (+ 8 (* count 16)))))
                    (cond
                     ((and (= count 0)
                           (> length ngit-transport-delta--io-byte-limit))
                      (setq clipped-length length
                            expected ngit-transport-delta--io-byte-limit
                            count 1 stop t)
                      (ptr-write-u64 address 8 expected))
                     ((> (+ expected length)
                         ngit-transport-delta--io-byte-limit)
                      (setq stop t))
                     (t (setq expected (+ expected length)
                              count (1+ count))))))
                (let ((written -4))
                  (while (= written -4)
                    (setq written
                          (syscall-direct ngit-transport-delta--sys-writev
                                          fd address count 0 0 0)))
                  (when clipped-length (ptr-write-u64 address 8 clipped-length))
                  (when (<= written 0)
                    (error "ngit: delta writev failed (result %d)" written))
                  (setq total (+ total written))
                  (let ((left written))
                    (while (> left 0)
                      (let* ((entry (+ iov-address 16 (* first 16)))
                             (base (ptr-read-u64 entry 0))
                             (length (ptr-read-u64 entry 8)))
                        (if (>= left length)
                            (setq left (- left length) first (1+ first))
                          (ptr-write-u64 entry 0 (+ base left))
                          (ptr-write-u64 entry 8 (- length left))
                          (setq left 0))))))))
            (unless (= total output-length)
              (error "ngit: delta gather size mismatch")))
          (let ((done 0))
            (while (< done output-length)
              (let ((read -4) (want (min (- output-length done)
                                         ngit-transport-delta--io-byte-limit)))
                (while (= read -4)
                  (setq read (syscall-direct ngit-transport-delta--sys-pread64
                                             fd (+ output-address done) want done 0 0)))
                (when (<= read 0)
                  (error "ngit: delta pread failed at %d (result %d)" done read))
                (setq done (+ done read)))))
          output-length)
      (when (and fd (>= fd 0))
        (syscall-direct ngit-transport-delta--sys-close fd 0 0 0 0 0))
      (nl-ffi-memory-release name-owner))))

(defun ngit-transport-delta--varint (delta pos)
  (let ((value 0) (shift 0) (byte 128) (n (length delta)))
    (while (/= 0 (logand byte 128))
      (when (>= pos n) (error "ngit: truncated delta varint"))
      (setq byte (aref delta pos)
            value (logior value (ash (logand byte 127) shift))
            shift (+ shift 7) pos (1+ pos))
      (when (> shift 63) (error "ngit: oversized delta varint")))
    (cons value pos)))

(defun ngit-transport-delta--pieces (base-address base-length delta delta-address)
  "Return (OUTPUT-LENGTH . PIECES) after validating DELTA.
Each piece is (ADDRESS . LENGTH)."
  (let* ((r1 (ngit-transport-delta--varint delta 0))
         (r2 (ngit-transport-delta--varint delta (cdr r1)))
         (output-length (car r2)) (pos (cdr r2))
         (n (length delta)) (written 0) pieces)
    (unless (= (car r1) base-length) (error "ngit: delta base size mismatch"))
    (while (< pos n)
      (let ((command (aref delta pos)))
        (setq pos (1+ pos))
        (if (/= 0 (logand command 128))
            (let ((offset 0) (size 0))
              (when (/= 0 (logand command 1))
                (when (>= pos n) (error "ngit: truncated delta copy offset"))
                (setq offset (aref delta pos) pos (1+ pos)))
              (when (/= 0 (logand command 2))
                (when (>= pos n) (error "ngit: truncated delta copy offset"))
                (setq offset (logior offset (ash (aref delta pos) 8)) pos (1+ pos)))
              (when (/= 0 (logand command 4))
                (when (>= pos n) (error "ngit: truncated delta copy offset"))
                (setq offset (logior offset (ash (aref delta pos) 16)) pos (1+ pos)))
              (when (/= 0 (logand command 8))
                (when (>= pos n) (error "ngit: truncated delta copy offset"))
                (setq offset (logior offset (ash (aref delta pos) 24)) pos (1+ pos)))
              (when (/= 0 (logand command 16))
                (when (>= pos n) (error "ngit: truncated delta copy size"))
                (setq size (aref delta pos) pos (1+ pos)))
              (when (/= 0 (logand command 32))
                (when (>= pos n) (error "ngit: truncated delta copy size"))
                (setq size (logior size (ash (aref delta pos) 8)) pos (1+ pos)))
              (when (/= 0 (logand command 64))
                (when (>= pos n) (error "ngit: truncated delta copy size"))
                (setq size (logior size (ash (aref delta pos) 16)) pos (1+ pos)))
              (when (= size 0) (setq size #x10000))
              (when (> (+ offset size) base-length)
                (error "ngit: delta copy outside base"))
              (push (cons (+ base-address offset) size) pieces)
              (setq written (+ written size)))
          (when (= command 0) (error "ngit: invalid delta opcode 0"))
          (when (> (+ pos command) n) (error "ngit: truncated delta insert"))
          (push (cons (+ delta-address pos) command) pieces)
          (setq pos (+ pos command) written (+ written command)))
        (when (> written output-length) (error "ngit: delta result exceeds size"))))
    (unless (= written output-length) (error "ngit: delta result size mismatch"))
    (cons output-length (nreverse pieces))))

(defun ngit-transport-delta--gather-apply (base-address base-length delta)
  "Apply DELTA to native BASE-ADDRESS of BASE-LENGTH bytes.
Return [OWNER ADDRESS LENGTH].  The caller must release OWNER."
  (let* ((delta-length (length delta))
         (delta-owner (nl-ffi-memory-allocate (max 1 delta-length)))
         (delta-address (nl-ffi-memory-address delta-owner))
         output-owner iov-owner)
    (unwind-protect
        (progn
          (ptr-write-bytes delta-address delta)
          (let* ((parsed (ngit-transport-delta--pieces
                          base-address base-length delta delta-address))
                 (output-length (car parsed)) (pieces (cdr parsed))
                 (piece-count (length pieces))
                 (output (setq output-owner
                               (nl-ffi-memory-allocate (max 1 output-length))))
                 (iov (setq iov-owner
                            (nl-ffi-memory-allocate (* 16 (1+ piece-count)))))
                 (output-address (nl-ffi-memory-address output))
                 (iov-address (nl-ffi-memory-address iov)) (slot 1))
            (ptr-write-u64 iov-address 0 output-address)
            (ptr-write-u64 iov-address 8 output-length)
            (dolist (piece pieces)
              (ptr-write-u64 iov-address (* slot 16) (car piece))
              (ptr-write-u64 iov-address (+ (* slot 16) 8) (cdr piece))
              (setq slot (1+ slot)))
            (let* ((pid (syscall-direct ngit-transport-delta--sys-getpid 0 0 0 0 0 0))
                   (copied (if (and ngit-transport-delta--process-vm-readv-p
                                    (<= piece-count ngit-transport-delta--iov-max)
                                    (<= output-length ngit-transport-delta--io-byte-limit))
                               (syscall-direct
                                ngit-transport-delta--sys-process-vm-readv pid
                                iov-address 1 (+ iov-address 16) piece-count 0)
                             -1)))
              (unless (= copied output-length)
                (if (< copied 0)
                    (progn
                      (setq ngit-transport-delta--process-vm-readv-p nil)
                      (ngit-transport-delta--gather-fallback
                       iov-address piece-count output-address output-length))
                  (error "ngit: delta gather failed (%d of %d)"
                         copied output-length))))
            (let ((result (vector output output-address output-length)))
              (setq output-owner nil)
              result)))
      (when iov-owner (nl-ffi-memory-release iov-owner))
      (when output-owner (nl-ffi-memory-release output-owner))
      (nl-ffi-memory-release delta-owner))))

(defun ngit-transport-delta-apply (base-address base-length delta)
  "Apply DELTA to native BASE-ADDRESS; return owned [OWNER ADDRESS LENGTH]."
  (or (and ngit-transport-delta--use-kernel
           (fboundp 'ngit-transport-kernel-apply)
           (let* ((base-size (ngit-transport-delta--varint delta 0))
                  (size (car (ngit-transport-delta--varint delta (cdr base-size)))))
             (ngit-transport-kernel-apply base-address base-length delta size)))
      (ngit-transport-delta--gather-apply base-address base-length delta)))

(defun ngit-transport-delta-apply-string (base delta)
  "Apply DELTA to Lisp BASE using the already prepared native kernel."
  (let* ((owner (nl-ffi-memory-allocate (max 1 (length base))))
         (address (nl-ffi-memory-address owner)) result)
    (unwind-protect
        (progn
          (ptr-write-bytes address base)
          (setq result (ngit-transport-delta-apply address (length base) delta))
          (ptr-read-bytes (aref result 1) (aref result 2)))
      (when result (nl-ffi-memory-release (aref result 0)))
      (nl-ffi-memory-release owner))))

(provide 'ngit-transport-delta)

;;; ngit-transport-delta.el ends here
