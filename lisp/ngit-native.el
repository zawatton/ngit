;;; ngit-native.el --- native I/O and zlib for ngit -*- lexical-binding: t; -*-

;;; Commentary:

;; NeLisp standalone has no zlib primitive and its interpreter is too slow
;; for a pure Lisp inflate, so ngit reads file ranges with raw syscalls into
;; mmap-backed external memory and inflates them with libz's `uncompress2'
;; through the nl-ffi dlsym path.  Lisp only ever sees the final unibyte
;; result via `ptr-read-bytes'.
;;
;; Requires Linux x86_64 (the syscall numbers and nl-ffi-memory profile).

;;; Code:

(defvar ngit-native--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(defvar ngit-nelisp-root
  (let ((env (getenv "NGIT_NELISP_ROOT")))
    (if (and env (not (string= env "")))
        (file-name-as-directory env)
      (expand-file-name "../../nelisp/" ngit-native--dir)))
  "Root of the NeLisp checkout that provides packages/nl-ffi.")

(let ((src (expand-file-name "packages/nl-ffi/src/" ngit-nelisp-root)))
  (load (expand-file-name "nl-ffi.el" src) nil t)
  (load (expand-file-name "nl-ffi-memory.el" src) nil t))

(ffi:library "libz.so.1")
(ffi:defun ngit-native--uncompress2 "uncompress2"
  [:sint32 :pointer :pointer :pointer :pointer]
  "uncompress2(dest, &destLen, source, &sourceLen).")

(ffi:defun ngit-native--compress2 "compress2"
  [:sint32 :pointer :pointer :pointer :uint64 :sint32]
  "compress2(dest, &destLen, source, sourceLen, level).")

(defvar ngit-native--direct (make-hash-table :test 'equal)
  "Resolved addresses of the zlib functions, for direct `ptr-call's.")

(defun ngit-native--zlib-call (symbol fallback &rest args)
  "Call zlib SYMBOL with ARGS (at most six integers) through `ptr-call'.
`ffi:defun' wrappers convert arguments, miss the build-time table and
re-resolve the symbol on every call, which costs most of a millisecond;
the address resolved once by nl-ffi's own lookup is equivalent.  Falls
back to FALLBACK, the `ffi:defun' wrapper, when that lookup is missing."
  (let ((addr (or (gethash symbol ngit-native--direct)
                  (puthash symbol
                           (if (and (fboundp 'nl-ffi--resolve-via-dlsym) (fboundp 'ptr-call))
                               (funcall 'nl-ffi--resolve-via-dlsym symbol)
                             0)
                           ngit-native--direct))))
    (if (zerop addr)
        (apply fallback args)
      (let ((padded (append args (make-list (- 6 (length args)) 0))))
        (apply #'ptr-call addr padded)))))

(defconst ngit-native--sys-close 3)
(defconst ngit-native--sys-open 2)
(defconst ngit-native--sys-pread64 17)
(defconst ngit-native--sys-umask 95)
(defconst ngit-native--o-rdonly-cloexec #o2000000)

(defvar ngit-native--umask nil
  "Process umask, read and restored once on first use.")

(defun ngit-native-creation-mode (mode)
  "Return creation MODE masked by the cached process umask."
  (unless ngit-native--umask
    (let ((mask (syscall-direct ngit-native--sys-umask 0 0 0 0 0 0)))
      (syscall-direct ngit-native--sys-umask mask 0 0 0 0 0)
      (setq ngit-native--umask mask)))
  (logand mode (lognot ngit-native--umask)))

(defconst ngit-native--z-ok 0)
(defconst ngit-native--z-data-error -3)
(defconst ngit-native--z-buf-error -5)

(defmacro ngit-native--with-memory (spec &rest body)
  "Bind (VAR SIZE) to an external memory owner around BODY, then release it."
  (declare (indent 1))
  (let ((var (car spec)))
    `(let ((,var (nl-ffi-memory-allocate (max 1 ,(cadr spec)))))
       (unwind-protect (progn ,@body)
         (nl-ffi-memory-release ,var)))))

(defvar ngit-native--sizes (make-hash-table :test 'equal))

(defun ngit-native-file-size (path)
  "Return the size in bytes of PATH (cached; git object files are immutable)."
  (or (gethash path ngit-native--sizes)
      (puthash path (file-attribute-size (file-attributes path)) ngit-native--sizes)))

(defvar ngit-native--fds (make-hash-table :test 'equal)
  "Open read-only descriptors keyed by path; kept for the process lifetime.")

(defvar ngit-native--buffers (make-vector 4 nil)
  "Reusable external buffers: 0 read/source, 1 inflate destination,
2 length words, 3 C strings.  Allocation costs milliseconds here.")

(defun ngit-native--buffer (slot size)
  "Return the owner in SLOT, regrown to hold at least SIZE bytes."
  (let ((owner (aref ngit-native--buffers slot)))
    (if (and owner (>= (aref owner 3) size))
        owner
      (when owner (nl-ffi-memory-release owner))
      (aset ngit-native--buffers slot
            (nl-ffi-memory-allocate (max size 4096 (if owner (* 2 (aref owner 3)) 0)))))))

(defun ngit-native--write-bytes (addr bytes)
  "Copy unibyte BYTES to ADDR."
  (ptr-write-bytes addr bytes))

(defun ngit-native-try-open (path)
  "Open PATH read-only and cache the descriptor; return nil when that fails."
  (or (gethash path ngit-native--fds)
      (let* ((bytes (encode-coding-string path 'utf-8))
             (addr (nl-ffi-memory-address
                    (ngit-native--buffer 3 (1+ (length bytes))))))
        (ngit-native--write-bytes addr bytes)
        (ptr-write-u8 addr (length bytes) 0)
        (let ((fd (syscall-direct ngit-native--sys-open addr
                                  ngit-native--o-rdonly-cloexec 0 0 0 0)))
          (and (>= fd 0) (puthash path fd ngit-native--fds))))))

(defun ngit-native--open (path)
  (or (ngit-native-try-open path)
      (error "ngit: cannot open %s" path)))

(defun ngit-native--pread (path owner offset len)
  "Read up to LEN bytes of PATH at OFFSET into OWNER; return the count."
  (let ((fd (ngit-native--open path))
        (addr (nl-ffi-memory-address owner))
        (done 0)
        (eof nil))
    (while (and (< done len) (not eof))
      (let ((n (syscall-direct ngit-native--sys-pread64 fd (+ addr done)
                               (- len done) (+ offset done) 0 0)))
        (cond ((< n 0) (error "ngit: read %s failed (errno %d)" path (- n)))
              ((= n 0) (setq eof t))
              (t (setq done (+ done n))))))
    done))

(defun ngit-native-close (path)
  "Close the cached descriptor of PATH, if any (for files read once)."
  (let ((fd (gethash path ngit-native--fds)))
    (when fd
      (syscall-direct ngit-native--sys-close fd 0 0 0 0 0)
      (remhash path ngit-native--fds))))

(defun ngit-native-read-bytes (path offset len)
  "Return up to LEN bytes of PATH starting at OFFSET as a unibyte string."
  (if (<= len 0)
      ""
    (let* ((buf (ngit-native--buffer 0 len))
           (n (ngit-native--pread path buf offset len)))
      (ptr-read-bytes (nl-ffi-memory-address buf) n))))

(defun ngit-native-write-stderr (string)
  "Write STRING to file descriptor 2 (`message' is silent in standalone)."
  (let ((bytes (encode-coding-string string 'utf-8)))
    (ngit-native--with-memory (buf (length bytes))
      (let ((addr (nl-ffi-memory-address buf)) (i 0))
        (while (< i (length bytes))
          (ptr-write-u8 addr i (aref bytes i))
          (setq i (1+ i)))
        (syscall-direct 1 2 addr (length bytes) 0 0 0)))))

(defun ngit-native--inflate-once (src src-len dst-cap &optional skip)
  "Inflate SRC-LEN bytes of owner SRC (from byte SKIP) into a DST-CAP buffer.
Return (RC . (DATA . CONSUMED)); DATA is nil unless RC is Z_OK."
  (let ((da (nl-ffi-memory-address (ngit-native--buffer 1 dst-cap)))
        (la (nl-ffi-memory-address (ngit-native--buffer 2 16))))
    (ptr-write-u64 la 0 dst-cap)
    (ptr-write-u64 la 8 src-len)
    (let ((rc (ngit-native--zlib-call "uncompress2" #'ngit-native--uncompress2
                                      da la (+ (nl-ffi-memory-address src) (or skip 0))
                                      (+ la 8))))
      ;; The dlsym path returns :sint32 zero-extended; sign-extend it.
      (setq rc (logand rc #xffffffff))
      (when (>= rc #x80000000) (setq rc (- rc #x100000000)))
      (cons rc
            (and (= rc ngit-native--z-ok)
                 (cons (ptr-read-bytes da (ptr-read-u64 la 0))
                       (ptr-read-u64 la 8)))))))

(defun ngit-native-inflate-file (path offset &optional size)
  "Inflate the zlib stream of PATH that starts at OFFSET.
SIZE is the known inflated size (pack entries) or nil (loose objects).
Return (DATA . CONSUMED-COMPRESSED-BYTES)."
  (let* ((avail (- (ngit-native-file-size path) offset))
         (src-len (if size (min avail (+ size (/ size 8) 64)) avail))
         (dst-cap (or size (+ (* 4 avail) 1024)))
         (result nil))
    (while (not result)
      (let ((src (ngit-native--buffer 0 src-len)))
        (let* ((n (ngit-native--pread path src offset src-len))
               (r (ngit-native--inflate-once src n dst-cap))
               (rc (car r)))
          (cond
           ((= rc ngit-native--z-ok) (setq result (cdr r)))
           ((and (= rc ngit-native--z-buf-error) (null size))
            (setq dst-cap (* 2 dst-cap)))
           ;; A truncated input reports Z_DATA_ERROR, or Z_BUF_ERROR when
           ;; the output is already full but the trailer is still missing.
           ((and (or (= rc ngit-native--z-data-error)
                     (and size (= rc ngit-native--z-buf-error)))
                 (< src-len avail))
            (setq src-len (min avail (* 2 src-len))))
           (t (error "ngit: inflate of %s at %d failed (zlib %d)"
                     path offset rc))))))
    (when (and size (/= (length (car result)) size))
      (error "ngit: inflated %d bytes, expected %d (%s at %d)"
             (length (car result)) size path offset))
    result))

(defun ngit-native-read-head (path offset len)
  "Read up to LEN bytes of PATH at OFFSET into the read buffer.
Return the number of bytes read; see `ngit-native-head-bytes' and
`ngit-native-inflate-head'."
  (ngit-native--pread path (ngit-native--buffer 0 len) offset len))

(defun ngit-native-head-bytes (n)
  "Return the first N bytes of the read buffer as a unibyte string."
  (ptr-read-bytes (nl-ffi-memory-address (aref ngit-native--buffers 0)) n))

(defun ngit-native-inflate-head (skip avail size)
  "Inflate the SIZE-byte stream at byte SKIP of the read buffer.
AVAIL bytes are available from SKIP.  Return the data, or nil when the
stream does not inflate from those bytes."
  (let ((r (ngit-native--inflate-once (aref ngit-native--buffers 0) avail size skip)))
    (and (= (car r) ngit-native--z-ok)
         (= (length (cadr r)) size)
         (cadr r))))

(defun ngit-native-inflate-whole-head (n)
  "Inflate the N bytes in the read buffer, a whole zlib stream of unknown
inflated size.  Return the data, or nil when it does not inflate."
  (let ((cap (+ (* 4 n) 1024)) (result nil) (done nil))
    (while (not done)
      (let* ((r (ngit-native--inflate-once (aref ngit-native--buffers 0) n cap))
             (rc (car r)))
        (cond ((= rc ngit-native--z-ok) (setq result (cadr r) done t))
              ((and (= rc ngit-native--z-buf-error) (< cap (* 64 1024 1024)))
               (setq cap (* 2 cap)))
              (t (setq done t)))))
    result))

(defun ngit-native-deflate-file (path len level)
  "Return the first LEN bytes of PATH compressed as one zlib stream.
The data goes through a file because copying a Lisp string into
native memory costs a primitive call per four bytes."
  (let* ((src (ngit-native--buffer 0 len))
         (n (prog1 (ngit-native--pread path src 0 len)
              (ngit-native-close path)))
         (cap (+ n (ash n -12) (ash n -14) 64))
         (da (nl-ffi-memory-address (ngit-native--buffer 1 cap)))
         (la (nl-ffi-memory-address (ngit-native--buffer 2 16))))
    (ptr-write-u64 la 0 cap)
    (let ((rc (logand (ngit-native--compress2 da la (nl-ffi-memory-address src) n level)
                      #xffffffff)))
      (unless (= rc ngit-native--z-ok)
        (error "ngit: deflate of %s failed (zlib %d)" path rc))
      (ptr-read-bytes da (ptr-read-u64 la 0)))))

(provide 'ngit-native)

;;; ngit-native.el ends here
