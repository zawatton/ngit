;;; transport-delta.el --- native delta application unit probe -*- lexical-binding: t; -*-

(let ((directory (expand-file-name "../lisp/" (file-name-directory load-file-name))))
  (load (expand-file-name "ngit-native.el" directory) nil t)
  (load (expand-file-name "ngit-transport-kernel.el" directory) nil t)
  (load (expand-file-name "ngit-transport-delta.el" directory) nil t))

(defvar ngit-delta-test--passed 0)
(defvar ngit-delta-test--failed 0)

(unless (ngit-transport-kernel-prepare)
  (error "Native delta kernel unavailable: %s" ngit-transport-kernel--error))

(defun ngit-delta-test--varint (value)
  (let (out)
    (while (>= value 128)
      (push (logior 128 (logand value 127)) out)
      (setq value (ash value -7)))
    (push value out)
    (apply #'unibyte-string (nreverse out))))

(defun ngit-delta-test--stream (base-size result-size instructions)
  (concat (ngit-delta-test--varint base-size)
          (ngit-delta-test--varint result-size)
          instructions))

(defun ngit-delta-test--run (name base delta expected &optional io-limit force-fallback)
  (let* ((base-owner (nl-ffi-memory-allocate (+ 3 (length base))))
         ;; Deliberately keep the source unaligned.
         (base-address (1+ (nl-ffi-memory-address base-owner)))
         result actual)
    (unwind-protect
        (condition-case err
            (let ((ngit-transport-delta--io-byte-limit
                   (or io-limit ngit-transport-delta--io-byte-limit))
                  (ngit-transport-delta--use-kernel (not force-fallback))
                  (ngit-transport-delta--process-vm-readv-p
                   (if force-fallback nil ngit-transport-delta--process-vm-readv-p)))
              (ptr-write-bytes base-address base)
              (setq result (ngit-transport-delta-apply
                            base-address (length base) delta))
              (setq actual (ptr-read-bytes (aref result 1) (aref result 2)))
              (if (and (= (aref result 2) (length expected))
                       (string= actual expected))
                  (setq ngit-delta-test--passed (1+ ngit-delta-test--passed))
                (setq ngit-delta-test--failed (1+ ngit-delta-test--failed))
                (ngit-native-write-stderr
                 (format "FAIL %s: expected %d bytes, got %d\n"
                         name (length expected) (length actual)))))
          (error
           (setq ngit-delta-test--failed (1+ ngit-delta-test--failed))
           (ngit-native-write-stderr
            (format "FAIL %s: %s\n" name (error-message-string err)))))
      (when result (nl-ffi-memory-release (aref result 0)))
      (nl-ffi-memory-release base-owner))))

(defun ngit-delta-test--reject (name base delta)
  (let* ((owner (nl-ffi-memory-allocate (max 1 (length base))))
         (address (nl-ffi-memory-address owner)) result rejected)
    (unwind-protect
        (progn
          (ptr-write-bytes address base)
          (condition-case nil
              (setq result (ngit-transport-delta-apply address (length base) delta))
            (error (setq rejected t)))
          (if rejected
              (setq ngit-delta-test--passed (1+ ngit-delta-test--passed))
            (setq ngit-delta-test--failed (1+ ngit-delta-test--failed))
            (ngit-native-write-stderr (format "FAIL %s: malformed delta accepted\n" name))))
      (when result (nl-ffi-memory-release (aref result 0)))
      (nl-ffi-memory-release owner))))

(let* ((base "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
       (instructions (unibyte-string 1 ?! #x91 1 7 #x91 8 8 #x91 16 9))
       (expected (concat "!" (substring base 1 8) (substring base 8 16)
                         (substring base 16 25))))
  (ngit-delta-test--run
   "unaligned source and destination" base
   (ngit-delta-test--stream (length base) (length expected) instructions) expected))

(let* ((literal (apply #'unibyte-string
                       (let (out) (dotimes (i 256) (push i out)) (nreverse out))))
       (instructions (concat (unibyte-string 127) (substring literal 0 127)
                             (unibyte-string 127) (substring literal 127 254)
                             (unibyte-string 2) (substring literal 254))))
  (ngit-delta-test--run
   "mixed literal bytes" "" (ngit-delta-test--stream 0 256 instructions) literal))

(let ((base (make-string #x10000 ?d)))
  (ngit-delta-test--run
   "default 65536-byte copy" base
   (ngit-delta-test--stream #x10000 #x10000 (unibyte-string #x80)) base))

(let* ((offset #x01030201) (size #x010203) (base-length (+ offset size))
       (base (make-string base-length ?x)))
  (aset base (+ offset 7) ?Y)
  (aset base (+ offset 513) ?Z)
  (ngit-delta-test--run
   "all copy offset and size fields" base
   (ngit-delta-test--stream
    base-length size (unibyte-string #xff 1 2 3 1 3 2 1))
   (substring base offset (+ offset size))))

(let ((instructions nil) (expected nil))
  (dotimes (i 1025)
    (push (unibyte-string 1 (logand i 255)) instructions)
    (push (unibyte-string (logand i 255)) expected))
  (setq instructions (apply #'concat (nreverse instructions))
        expected (apply #'concat (nreverse expected)))
  (dolist (limit '(17 3))
    (ngit-delta-test--run
     (format "fallback 1025 literals io-limit %d" limit) ""
     (ngit-delta-test--stream 0 1025 instructions) expected limit t)))

(ngit-delta-test--run "zero-length result" "abc"
                      (ngit-delta-test--stream 3 0 "") "")

(let ((base "abcd"))
  (ngit-delta-test--reject "bad base size" base
                           (ngit-delta-test--stream 3 0 ""))
  (ngit-delta-test--reject "unterminated varint" base (unibyte-string #x80))
  (ngit-delta-test--reject "oversized varint" base
                           (make-string 11 #x80))
  (ngit-delta-test--reject "terminated oversized varint" base
                           (concat (make-string 10 #x80) (unibyte-string 0 0)))
  (ngit-delta-test--reject "opcode zero" base
                           (ngit-delta-test--stream 4 0 (unibyte-string 0)))
  (ngit-delta-test--reject "truncated copy offset" base
                           (ngit-delta-test--stream 4 1 (unibyte-string #x81)))
  (ngit-delta-test--reject "truncated copy size" base
                           (ngit-delta-test--stream 4 1 (unibyte-string #x90)))
  (ngit-delta-test--reject "copy outside base" base
                           (ngit-delta-test--stream 4 2 (unibyte-string #x91 3 2)))
  (ngit-delta-test--reject "truncated insert" base
                           (ngit-delta-test--stream 4 2 (unibyte-string 2 ?x)))
  (ngit-delta-test--reject "result overrun" base
                           (ngit-delta-test--stream 4 1 (unibyte-string 2 ?x ?y)))
  (ngit-delta-test--reject "result underrun" base
                           (ngit-delta-test--stream 4 2 (unibyte-string 1 ?x))))

;; Bypass the Lisp header parser so native varint overflow cannot hide behind
;; an earlier rejection in the wrapper.
(let* ((delta (apply #'unibyte-string (append (make-list 10 #x80) '(0 0))))
       (input (nl-ffi-memory-allocate (length delta)))
       (base (nl-ffi-memory-allocate 1)) (output (nl-ffi-memory-allocate 1)))
  (unwind-protect
      (progn
        (ptr-write-bytes (nl-ffi-memory-address input) delta)
        (if (/= 0 (ptr-call ngit-transport-kernel--address
                            (nl-ffi-memory-address base) 0
                            (nl-ffi-memory-address input) (length delta)
                            (nl-ffi-memory-address output) 0))
            (setq ngit-delta-test--passed (1+ ngit-delta-test--passed))
          (setq ngit-delta-test--failed (1+ ngit-delta-test--failed))))
    (nl-ffi-memory-release input)
    (nl-ffi-memory-release base)
    (nl-ffi-memory-release output)))

(if (= ngit-delta-test--failed 0)
    (progn
      (princ (format "transport-delta: PASS (%d cases)\n" ngit-delta-test--passed))
      (kill-emacs 0))
  (ngit-native-write-stderr
   (format "transport-delta: FAIL (%d passed, %d failed)\n"
           ngit-delta-test--passed ngit-delta-test--failed))
  (kill-emacs 1))

;;; transport-delta.el ends here
