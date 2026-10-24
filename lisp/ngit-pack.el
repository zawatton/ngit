;;; ngit-pack.el --- pack writing and inspection -*- lexical-binding: t; -*-

;;; Commentary:

;; Pack v2 writer, index v2 writer/reader commands, and verify-pack output.
;; Existing packs are the fast path: a wholly selected pack is copied as one
;; byte range, preserving its compressed entries, OFS deltas, and CRCs.

;;; Code:

(require 'ngit-objects)
(require 'ngit-write)

(ffi:defun ngit-pack--crc32-ffi "crc32"
  [:uint64 :uint64 :pointer :uint64]
  "zlib crc32(crc, bytes, length).")

(defconst ngit-pack--sys-read 0)
(defconst ngit-pack--sys-write 1)
(defconst ngit-pack--sys-close 3)
(defconst ngit-pack--sys-open 2)
(defconst ngit-pack--sys-socket 41)
(defconst ngit-pack--sys-accept 43)
(defconst ngit-pack--sys-sendto 44)
(defconst ngit-pack--sys-bind 49)
(defconst ngit-pack--af-alg 38)
(defconst ngit-pack--sock-seqpacket 5)
(defconst ngit-pack--msg-more #x8000)
(defconst ngit-pack--o-wronly #o1)
(defconst ngit-pack--o-creat #o100)
(defconst ngit-pack--o-trunc #o1000)
(defconst ngit-pack--o-cloexec #o2000000)
(defconst ngit-pack--copy-chunk (* 4 1024 1024))

(defvar ngit-pack--hash-stream nil
  "Dynamically bound [SOCKET OP VALID] for pack body SHA-1 streaming.")

(defvar ngit-pack--payload-temp nil
  "Dynamically bound reusable temp path for large pack payloads.")

(defconst ngit-pack--direct-compress-limit (* 64 1024 1024)
  "Largest payload copied directly to zlib's reusable native buffer.")

(defconst ngit-pack--native-loose-threshold (* 64 1024)
  "Minimum compressed loose-object size for native pack emission.")

(defun ngit-pack--u32 (v)
  (unibyte-string (logand (ash v -24) 255) (logand (ash v -16) 255)
                  (logand (ash v -8) 255) (logand v 255)))

(defun ngit-pack--u64 (v)
  (concat (ngit-pack--u32 (ash v -32)) (ngit-pack--u32 v)))

(defun ngit-pack--type-id (type)
  (cond ((eq type 'commit) 1) ((eq type 'tree) 2) ((eq type 'blob) 3)
        ((eq type 'tag) 4) ((eq type 'ofs-delta) 6) ((eq type 'ref-delta) 7)
        (t (error "ngit: cannot pack object type %s" type))))

(defun ngit-pack--entry-header (type size)
  (let ((id (if (numberp type) type (ngit-pack--type-id type)))
        (first t) out)
    (while (or first (> size 0))
      (let ((byte (if first
                      (logior (ash id 4) (logand size 15))
                    (logand size 127))))
        (setq size (ash size (if first -4 -7)) first nil)
        (when (> size 0) (setq byte (logior byte 128)))
        (push byte out)))
    (apply #'unibyte-string (nreverse out))))

(defun ngit-pack--ofs-bytes (distance)
  (let ((bytes (list (logand distance 127))))
    (setq distance (ash distance -7))
    (while (> distance 0)
      (setq distance (1- distance))
      (push (logior 128 (logand distance 127)) bytes)
      (setq distance (ash distance -7)))
    (apply #'unibyte-string bytes)))

(defun ngit-pack--open-output (path)
  (let* ((bytes (encode-coding-string path 'utf-8))
         (owner (ngit-native--buffer 3 (1+ (length bytes))))
         (addr (nl-ffi-memory-address owner)))
    (ptr-write-bytes addr bytes)
    (ptr-write-u8 addr (length bytes) 0)
    (let ((fd (syscall-direct ngit-pack--sys-open addr
                              (logior ngit-pack--o-wronly ngit-pack--o-creat
                                      ngit-pack--o-trunc ngit-pack--o-cloexec)
                              (ngit-native-creation-mode #o666) 0 0 0)))
      (if (< fd 0) (error "ngit: cannot create %s (errno %d)" path (- fd)) fd))))

(defun ngit-pack--write-pointer (fd addr len)
  (let ((done 0))
    (while (< done len)
      (let ((n (syscall-direct ngit-pack--sys-write fd (+ addr done) (- len done) 0 0 0)))
        (when (<= n 0) (error "ngit: pack write failed (errno %d)" (- n)))
        (when (and ngit-pack--hash-stream (aref ngit-pack--hash-stream 2))
          (let ((sent (syscall-direct ngit-pack--sys-sendto
                                      (aref ngit-pack--hash-stream 1)
                                      (+ addr done) n ngit-pack--msg-more 0 0)))
            ;; The pack write already succeeded.  A hashing error only loses
            ;; the optimization; the caller validates by rereading the file.
            (unless (= sent n) (aset ngit-pack--hash-stream 2 nil))))
        (setq done (+ done n))))))

(defun ngit-pack--write-bytes (fd bytes)
  (when (> (length bytes) 0)
    (let* ((owner (ngit-native--buffer 0 (length bytes)))
           (addr (nl-ffi-memory-address owner)))
      (ptr-write-bytes addr bytes)
      (ngit-pack--write-pointer fd addr (length bytes)))))

(defun ngit-pack--crc-pointer (crc addr len)
  (logand (ngit-native--zlib-call "crc32" #'ngit-pack--crc32-ffi crc addr len)
          #xffffffff))

(defun ngit-pack--crc-bytes (bytes &optional crc)
  (if (= (length bytes) 0) (or crc 0)
    (let* ((owner (ngit-native--buffer 0 (length bytes)))
           (addr (nl-ffi-memory-address owner)))
      (ptr-write-bytes addr bytes)
      (ngit-pack--crc-pointer (or crc 0) addr (length bytes)))))

(defun ngit-pack--copy-range (src dst offset len &optional crc)
  "Copy LEN bytes of SRC at OFFSET to DST.  Return updated CRC when non-nil."
  (let ((left len) (at offset) (sum crc))
    (while (> left 0)
      (let* ((want (min left ngit-pack--copy-chunk))
             (owner (ngit-native--buffer 0 want))
             (got (ngit-native--pread src owner at want))
             (addr (nl-ffi-memory-address owner)))
        (unless (= got want) (error "ngit: short read from %s" src))
        (when sum (setq sum (ngit-pack--crc-pointer sum addr got)))
        (ngit-pack--write-pointer dst addr got)
        (setq at (+ at got) left (- left got))))
    sum))

(defun ngit-pack--crc-range (path offset len)
  (let ((left len) (at offset) (crc 0))
    (while (> left 0)
      (let* ((want (min left ngit-pack--copy-chunk))
             (owner (ngit-native--buffer 0 want))
             (got (ngit-native--pread path owner at want)))
        (unless (= got want) (error "ngit: short read from %s" path))
        (setq crc (ngit-pack--crc-pointer crc (nl-ffi-memory-address owner) got)
              at (+ at got) left (- left got))))
    crc))

(defun ngit-pack--sha1-stream-open ()
  "Return [SOCKET OP VALID] for a streaming Linux AF_ALG SHA-1, or nil."
  (let ((sock (syscall-direct ngit-pack--sys-socket ngit-pack--af-alg
                              ngit-pack--sock-seqpacket 0 0 0 0)))
    (when (>= sock 0)
      (let* ((sa (ngit-native--buffer 2 88))
             (addr (nl-ffi-memory-address sa)))
        (dotimes (i 88) (ptr-write-u8 addr i 0))
        (ptr-write-u8 addr 0 ngit-pack--af-alg)
        (ptr-write-u8 addr 1 0)
        (ptr-write-bytes (+ addr 2) "hash")
        (ptr-write-bytes (+ addr 24) "sha1")
        (let ((rc (syscall-direct ngit-pack--sys-bind sock addr 88 0 0 0)))
          (if (< rc 0)
              (progn (syscall-direct ngit-pack--sys-close sock 0 0 0 0 0) nil)
            (let ((op (syscall-direct ngit-pack--sys-accept sock 0 0 0 0 0)))
              (if (< op 0)
                  (progn (syscall-direct ngit-pack--sys-close sock 0 0 0 0 0) nil)
                (vector sock op t)))))))))

(defun ngit-pack--sha1-stream-close (stream)
  (when stream
    (when (>= (aref stream 1) 0)
      (syscall-direct ngit-pack--sys-close (aref stream 1) 0 0 0 0 0)
      (aset stream 1 -1))
    (when (>= (aref stream 0) 0)
      (syscall-direct ngit-pack--sys-close (aref stream 0) 0 0 0 0 0)
      (aset stream 0 -1))))

(defun ngit-pack--sha1-stream-final (stream)
  "Finish STREAM and return its binary SHA-1, or nil to request rereading."
  (when stream
    (unwind-protect
        (when (aref stream 2)
          ;; A zero-length send without MSG_MORE terminates the hash request.
          (when (= (syscall-direct ngit-pack--sys-sendto
                                   (aref stream 1) 0 0 0 0 0) 0)
            (let* ((owner (ngit-native--buffer 0 20))
                   (addr (nl-ffi-memory-address owner))
                   (got (syscall-direct ngit-pack--sys-read
                                        (aref stream 1) addr 20 0 0 0)))
              (and (= got 20) (ptr-read-bytes addr 20)))))
      (ngit-pack--sha1-stream-close stream))))

(defun ngit-pack--sha1-file (path len)
  "Return the binary SHA-1 of the first LEN bytes of PATH using Linux AF_ALG."
  (let ((sock (syscall-direct ngit-pack--sys-socket ngit-pack--af-alg
                              ngit-pack--sock-seqpacket 0 0 0 0)))
    (if (< sock 0)
        ;; Seccomp profiles sometimes deny AF_ALG; libcrypto is also native.
        (ngit-native-sha1 (ngit-native-read-bytes path 0 len))
      (unwind-protect
        (let* ((sa (ngit-native--buffer 2 88))
               (addr (nl-ffi-memory-address sa)))
          (dotimes (i 88) (ptr-write-u8 addr i 0))
          (ptr-write-u8 addr 0 ngit-pack--af-alg)
          (ptr-write-u8 addr 1 0)
          (ptr-write-bytes (+ addr 2) "hash")
          (ptr-write-bytes (+ addr 24) "sha1")
          (let ((rc (syscall-direct ngit-pack--sys-bind sock addr 88 0 0 0)))
            (when (< rc 0) (error "ngit: AF_ALG SHA-1 bind failed (errno %d)" (- rc))))
          (let ((op (syscall-direct ngit-pack--sys-accept sock 0 0 0 0 0)))
            (when (< op 0) (error "ngit: AF_ALG SHA-1 accept failed (errno %d)" (- op)))
            (unwind-protect
                (let ((fd (ngit-native--open path)) (at 0) (left len))
                  (while (> left 0)
                    (let* ((want (min left ngit-pack--copy-chunk))
                           (owner (ngit-native--buffer 0 want))
                           (got (ngit-native--pread path owner at want))
                           (flags (if (> left got) ngit-pack--msg-more 0))
                           (sent (syscall-direct ngit-pack--sys-sendto op
                                                 (nl-ffi-memory-address owner) got flags 0 0)))
                      (unless (= sent got) (error "ngit: AF_ALG SHA-1 send failed"))
                      (setq at (+ at got) left (- left got))))
                  (let* ((owner (ngit-native--buffer 0 20))
                         (got (syscall-direct ngit-pack--sys-read op
                                              (nl-ffi-memory-address owner) 20 0 0 0)))
                    (unless (= got 20) (error "ngit: AF_ALG SHA-1 read failed"))
                    (ptr-read-bytes (nl-ffi-memory-address owner) 20)))
              (syscall-direct ngit-pack--sys-close op 0 0 0 0 0))))
        (syscall-direct ngit-pack--sys-close sock 0 0 0 0 0)))))

(defun ngit-pack--parse-header-bytes (h offset)
  "Parse entry bytes H at absolute OFFSET; return [TYPE SIZE DATA-OFF BASE]."
  (let* ((c (aref h 0)) (type (aref ngit--type-names (logand (ash c -4) 7)))
         (size (logand c 15)) (shift 4) (p 1) base)
    (while (/= 0 (logand c 128))
      (setq c (aref h p) size (+ size (ash (logand c 127) shift))
            shift (+ shift 7) p (1+ p)))
    (cond
     ((eq type 'ofs-delta)
      (setq c (aref h p) p (1+ p) base (logand c 127))
      (while (/= 0 (logand c 128))
        (setq c (aref h p) p (1+ p)
              base (+ (ash (1+ base) 7) (logand c 127))))
      (setq base (- offset base)))
     ((eq type 'ref-delta)
      (setq base (substring h p (+ p 20)) p (+ p 20))))
    (vector type size (+ offset p) base)))

(defun ngit-pack--parse-header (path offset)
  "Return [TYPE SIZE DATA-OFF BASE] for the entry at OFFSET in PATH."
  (ngit-pack--parse-header-bytes (ngit-native-read-bytes path offset 64) offset))

(defconst ngit-pack--header-window (* 4 1024 1024))

(defun ngit-pack--parse-header-windowed (path offset cache)
  "Parse PATH entry at OFFSET using the native read-ahead CACHE.
CACHE is [OWNER ADDR START LENGTH FILE-SIZE]."
  (let ((start (aref cache 2)) (got (aref cache 3)))
    (when (or (< offset start) (> (+ offset 64) (+ start got)))
      (let* ((left (- (aref cache 4) offset))
             (want (min ngit-pack--header-window left))
             (n (ngit-native--pread path (aref cache 0) offset want)))
        (unless (= n want) (error "ngit: short read from %s" path))
        (aset cache 2 offset) (aset cache 3 n)
        (setq start offset got n)))
    (ngit-pack--parse-header-bytes
     (ptr-read-bytes (+ (aref cache 1) (- offset start))
                     (min 64 (- got (- offset start))))
     offset)))

(defun ngit-pack--ptr-be32 (addr offset)
  "Read a big-endian unsigned 32-bit integer at ADDR+OFFSET."
  (let ((v (ptr-read-u32 addr offset)))
    (logior (ash (logand v #x000000ff) 24)
            (ash (logand v #x0000ff00) 8)
            (ash (logand v #x00ff0000) -8)
            (logand (ash v -24) 255))))

(defun ngit-pack--idx-info (pack)
  "Return entries [BIN CRC OFFSET HEX] from PACK's v2 index."
  (let* ((n (aref pack 2))
         (bytes (* n 28))
         (owner (nl-ffi-memory-allocate (max 1 bytes)))
         (addr (nl-ffi-memory-address owner))
         (crc-at (* n 20)) (off-at (* n 24)) out)
    (unwind-protect
        (progn
          (unless (= (ngit-native--pread (aref pack 1) owner 1032 bytes) bytes)
            (error "ngit: short pack index %s" (aref pack 1)))
          (dotimes (i n)
            (let* ((off (ngit-pack--ptr-be32 addr (+ off-at (* i 4))))
                   (off (if (zerop (logand off #x80000000)) off
                          (let* ((j (logand off #x7fffffff))
                                 (b (ngit-native-read-bytes
                                     (aref pack 1) (+ 1032 (* n 28) (* j 8)) 8)))
                            (logior (ash (ngit--be32 b 0) 32) (ngit--be32 b 4))))))
              (let ((bin (ptr-read-bytes (+ addr (* i 20)) 20)))
                (push (vector bin (ngit-pack--ptr-be32 addr (+ crc-at (* i 4)))
                              off (ngit-object--hex bin))
                      out))))
          (nreverse out))
      (nl-ffi-memory-release owner))))

(defvar ngit-pack--validated-sources (make-hash-table :test 'equal))

(defun ngit-pack--validate-source (pack)
  "Validate PACK's pack and index checksums before copying trusted bytes."
  (let ((pack-path (aref pack 0)) (idx-path (aref pack 1)))
    (unless (gethash pack-path ngit-pack--validated-sources)
      (let* ((pack-size (ngit-native-file-size pack-path))
             (pack-sum (ngit-native-read-bytes pack-path (- pack-size 20) 20)))
        (unless (string= pack-sum (ngit-pack--sha1-file pack-path (- pack-size 20)))
          (error "fatal: pack checksum mismatch in %s" pack-path))
        (let* ((idx (ngit--read-file idx-path))
               (n (length idx)))
          (unless (and (>= n 40)
                       (string= (substring idx (- n 40) (- n 20)) pack-sum)
                       (string= (substring idx (- n 20))
                                (ngit-native-sha1 (substring idx 0 (- n 20)))))
            (error "fatal: pack index checksum mismatch in %s" idx-path)))
        (puthash pack-path t ngit-pack--validated-sources)))))

(defun ngit-pack--idx-bytes (records pack-sum)
  "Return an exact v2 index for RECORDS [BIN CRC OFFSET] and PACK-SUM."
  (let* ((radix (if (>= (length records) 16384) 65536 256))
         (buckets (make-vector radix nil)) (fan (make-vector 256 0))
         names crcs offs large (large-i 0))
    (dolist (r records)
      (let* ((oid (aref r 0)) (first (aref oid 0))
             (bucket (if (= radix 256) first
                       (+ (ash first 8) (aref oid 1)))))
        (aset buckets bucket (cons r (aref buckets bucket)))
        (aset fan first (1+ (aref fan first)))))
    ;; Two OID bytes give small radix buckets even for large repositories.
    ;; Numeric bucket order is the first two bytes of Git's lexical order.
    (dotimes (i radix)
      (let ((bucket (aref buckets i)))
        (when (cdr bucket)
          (aset buckets i
                (sort bucket
                      (lambda (a b)
                        (let ((aoid (aref a 0)) (boid (aref b 0)))
                          (or (string< aoid boid)
                              (and (string= aoid boid)
                                   (< (aref a 2) (aref b 2)))))))))))
    (let ((sum 0)) (dotimes (i 256) (setq sum (+ sum (aref fan i))) (aset fan i sum)))
    (dotimes (i radix)
      (dolist (r (aref buckets i))
        (push (aref r 0) names)
        (push (ngit-pack--u32 (aref r 1)) crcs)
        (if (< (aref r 2) #x80000000)
            (push (ngit-pack--u32 (aref r 2)) offs)
          (push (ngit-pack--u32 (logior #x80000000 large-i)) offs)
          (push (ngit-pack--u64 (aref r 2)) large)
          (setq large-i (1+ large-i)))))
    (let ((body (concat (unibyte-string 255 116 79 99 0 0 0 2)
                        (apply #'concat (mapcar #'ngit-pack--u32 (append fan nil)))
                        (apply #'concat (nreverse names))
                        (apply #'concat (nreverse crcs))
                        (apply #'concat (nreverse offs))
                        (apply #'concat (nreverse large)) pack-sum)))
      (concat body (ngit-native-sha1 body)))))

(defun ngit-pack--deflate (data level)
  (let ((tmp (make-temp-file "ngit-pack-zlib-")))
    (unwind-protect
        (progn (ngit-write-file tmp data)
               (ngit-native-deflate-file tmp (length data) level))
      (when (file-exists-p tmp) (delete-file tmp)))))

(defun ngit-pack--compress-entry (fd payload level header)
  "Write HEADER and compressed PAYLOAD to FD; return (CRC . BYTE-COUNT).
The compressed result stays in zlib's reusable destination buffer, avoiding
a large Lisp-string-to-native copy for blobs."
  (let* ((n (length payload)) (tmp nil) (own-temp nil) src got)
    (unwind-protect
        (progn
          (setq src (ngit-native--buffer 0 n))
          (if (<= n ngit-pack--direct-compress-limit)
              (progn
                ;; Small commits and trees are dominated by temp-file setup.
                ;; One native copy is much cheaper than create/write/open/read.
                (ptr-write-bytes (nl-ffi-memory-address src) payload)
                (setq got n))
            (cond ((eq ngit-pack--payload-temp 'deferred)
                   (setq ngit-pack--payload-temp (make-temp-file "ngit-pack-zlib-")
                         tmp ngit-pack--payload-temp))
                  ((stringp ngit-pack--payload-temp)
                   (setq tmp ngit-pack--payload-temp))
                  (t
                   (setq tmp (make-temp-file "ngit-pack-zlib-") own-temp t)))
            (ngit-write-file tmp payload)
            (setq got (prog1 (ngit-native--pread tmp src 0 n)
                        (ngit-native-close tmp))))
          (let* ((cap (+ got (ash got -12) (ash got -14) 64))
                 (dst (ngit-native--buffer 1 cap))
                 (da (nl-ffi-memory-address dst))
                 (la (nl-ffi-memory-address (ngit-native--buffer 2 16))))
            (ptr-write-u64 la 0 cap)
            (let ((rc (logand (ngit-native--zlib-call
                               "compress2" #'ngit-native--compress2 da la
                               (nl-ffi-memory-address src) got level) #xffffffff)))
              (unless (= rc 0) (error "ngit: pack deflate failed (zlib %d)" rc))
              (let* ((zlen (ptr-read-u64 la 0))
                     (crc (ngit-pack--crc-bytes header)))
                (ngit-pack--write-bytes fd header)
                (setq crc (ngit-pack--crc-pointer crc da zlen))
                (ngit-pack--write-pointer fd da zlen)
                (cons crc (+ (length header) zlen))))))
      (when (and own-temp tmp (file-exists-p tmp)) (delete-file tmp)))))

(defun ngit-pack--compression-level (repo)
  (let ((v (or (ngit-config-get repo "pack.compression")
               (ngit-config-get repo "core.compression"))))
    (if (stringp v) (string-to-number v) 1)))

;;;; Delta creation (diff-delta.c)

(defconst ngit-pack--delta-search-limit (* 4 1024)
  "Largest object searched byte-for-byte by the interpreted delta port.")

(defconst ngit-pack--delta-work-budget (* 32 1024)
  "Maximum aggregate base+target bytes examined for new deltas per pack.")

(defvar ngit-pack--delta-work-left 0)

(defconst ngit-pack--rabin-basis
  [#xab59b4d1 #x56b369a2 #x063f6795 #x0c7ecf2a
   #x18fd9e54 #x31fb3ca8 #x63f67950 #x6cb54671])

(defconst ngit-pack--rabin-t
  (let ((v (make-vector 256 0)))
    (dotimes (i 256)
      (let ((n i) (bit 0) (x 0))
        (while (> n 0)
          (when (= (logand n 1) 1) (setq x (logxor x (aref ngit-pack--rabin-basis bit))))
          (setq n (ash n -1) bit (1+ bit)))
        (aset v i x)))
    v))

(defconst ngit-pack--rabin-u
  (let ((v (make-vector 256 0)))
    (dotimes (i 256)
      (let ((x i) (j 0))
        (while (< j 15)
          (setq x (logand #xffffffff
                          (logxor (ash x 8) (aref ngit-pack--rabin-t (ash x -23))))
                j (1+ j)))
        (aset v i x)))
    v))

(defun ngit-pack--rabin-step (value byte)
  (logand #xffffffff
          (logxor (logior (ash value 8) byte)
                  (aref ngit-pack--rabin-t (ash value -23)))))

(defun ngit-pack--delta-size-header (n)
  (let (out)
    (while (>= n 128) (push (logior 128 (logand n 127)) out) (setq n (ash n -7)))
    (push n out)
    (nreverse out)))

(defun ngit-pack--delta-index (base)
  "Build diff-delta.c's 16-byte Rabin index for BASE."
  (let* ((entries (/ (1- (length base)) 16))
         (hsize 16) (goal (/ entries 4)))
    (while (< hsize goal) (setq hsize (* hsize 2)))
    (let ((buckets (make-vector hsize nil)) (prev -1) last
          (pos (- (* entries 16) 16)))
      (while (>= pos 0)
        (let ((val 0) (i 1))
          (while (<= i 16)
            (setq val (ngit-pack--rabin-step val (aref base (+ pos i))) i (1+ i)))
          (if (= val prev)
              (aset last 0 (+ pos 16))
            (let ((entry (vector (+ pos 16) val)))
              (aset buckets (logand val (1- hsize))
                    (cons entry (aref buckets (logand val (1- hsize)))))
              (setq last entry prev val))))
        (setq pos (- pos 16)))
      ;; Iteration above prepends while walking backwards, hence each bucket
      ;; is already in increasing source-offset order.  Git caps a bucket at
      ;; 64 probes; uniform thinning only affects adversarial repeated data.
      (dotimes (i hsize)
        (let ((bucket (aref buckets i)))
          (when (> (length bucket) 64)
            (let ((n (length bucket)) keep)
              (dotimes (k 64)
                (push (nth (/ (* k n) 64) bucket) keep))
              (aset buckets i (nreverse keep))))))
      (vector (1- hsize) buckets))))

(defun ngit-pack--delta-copy-op (offset size)
  (let ((op 128) bytes)
    (dotimes (i 4)
      (let ((b (logand (ash offset (- (* i 8))) 255)))
        (when (/= b 0) (setq op (logior op (ash 1 i))) (push b bytes))))
    (dotimes (i 2)
      (let ((b (logand (ash size (- (* i 8))) 255)))
        (when (/= b 0) (setq op (logior op (ash 1 (+ 4 i)))) (push b bytes))))
    (cons op (nreverse bytes))))

(defun ngit-pack--create-delta (base target &optional max-size)
  "Return Git-format delta from BASE to TARGET, or nil above MAX-SIZE."
  (when (> (length target) 0)
    (let* ((index (ngit-pack--delta-index base))
           (mask (aref index 0)) (buckets (aref index 1))
           (out (reverse (append (ngit-pack--delta-size-header (length base))
                                 (ngit-pack--delta-size-header (length target)))))
           (out-len (length out))
           (data 0) (top (length target)) (val 0) (i 0)
           (ins 0) placeholder (moff 0) (msize 0))
      ;; The first window starts as an insert, leaving its count byte open.
      (setq placeholder (cons 0 out) out placeholder out-len (1+ out-len))
      (while (and (< i 16) (< data top))
        (push (aref target data) out)
        (setq val (ngit-pack--rabin-step val (aref target data))
              data (1+ data) i (1+ i) ins (1+ ins)
              out-len (1+ out-len)))
      (while (< data top)
        (when (< msize 4096)
          (setq val (logxor val (aref ngit-pack--rabin-u (aref target (- data 16))))
                val (ngit-pack--rabin-step val (aref target data)))
          (let ((entries (aref buckets (logand val mask))))
            (while entries
              (let* ((entry (car entries)) (ref (aref entry 0))
                     (src data) (limit (min (- (length base) ref) (- top data)))
                     (n 0))
                (when (and (= (aref entry 1) val) (> limit msize))
                  (while (and (< n limit) (= (aref target (+ src n)) (aref base (+ ref n))))
                    (setq n (1+ n)))
                  (when (> n msize) (setq msize n moff ref))))
              (setq entries (cdr entries)))))
        (if (< msize 4)
            (progn
              (when (= ins 0)
                (setq placeholder (cons 0 out) out placeholder out-len (1+ out-len)))
              (push (aref target data) out)
              (setq data (1+ data) ins (1+ ins) msize 0 out-len (1+ out-len))
              (when (= ins 127) (setcar placeholder ins) (setq ins 0 placeholder nil)))
          (when (> ins 0)
            (while (and (> moff 0) (> ins 0)
                        (= (aref base (1- moff)) (aref target (1- data))))
              (setq msize (1+ msize) moff (1- moff) data (1- data)
                    out (cdr out) ins (1- ins) out-len (1- out-len)))
            (if (> ins 0) (setcar placeholder ins)
              (setq out (cdr out) out-len (1- out-len)))
            (setq ins 0 placeholder nil))
          (let* ((left (if (< msize #x10000) 0 (- msize #x10000)))
                 (size (- msize left)) (op (ngit-pack--delta-copy-op moff size)))
            (push (car op) out)
            (setq out-len (1+ out-len))
            (dolist (b (cdr op)) (push b out) (setq out-len (1+ out-len)))
            (setq data (+ data size) moff (+ moff size) msize left)
            (when (and (< msize 4096) (< data top))
              (setq val 0 i (max 0 (- data 16)))
              (while (< i data)
                (setq val (ngit-pack--rabin-step val (aref target i)) i (1+ i))))))
        (when (and max-size (> out-len (+ max-size 64)))
          (setq data top out nil out-len 0)))
      (when out
        (when (> ins 0) (setcar placeholder ins))
        (let ((delta (apply #'unibyte-string (nreverse out))))
          (and (or (null max-size) (<= (length delta) max-size)) delta))))))

;;;; Pack writer

(defun ngit-pack--emit-large-loose (fd repo hex offset level)
  "Emit a large loose HEX without copying its payload into a Lisp string.
Return (RECORD CANDIDATE SIZE), or nil when the ordinary path is cheaper."
  (let* ((path (ngit--loose-path repo hex))
         (compressed-size (and (file-exists-p path)
                               (ngit-native-file-size path))))
    (when (and compressed-size
               (>= compressed-size ngit-pack--native-loose-threshold))
      (unwind-protect
          (catch 'fallback
            (let* ((compressed (ngit-native--buffer 0 compressed-size))
                   (got (ngit-native--pread path compressed 0 compressed-size))
                   (head-owner (ngit-native--buffer 1 128))
                   (head-address (nl-ffi-memory-address head-owner))
                   (lengths (ngit-native--buffer 2 16))
                   (length-address (nl-ffi-memory-address lengths)))
              (unless (= got compressed-size)
                (error "ngit: short read of loose object %s" hex))
              ;; A bounded first inflate reveals the textual loose-object
              ;; header without constructing the full payload in Lisp.
              (ptr-write-u64 length-address 0 128)
              (ptr-write-u64 length-address 8 compressed-size)
              (let ((rc (logand
                         (ngit-native--zlib-call
                          "uncompress2" #'ngit-native--uncompress2
                          head-address length-address
                          (nl-ffi-memory-address compressed) (+ length-address 8))
                         #xffffffff)))
                (when (>= rc #x80000000) (setq rc (- rc #x100000000)))
                (unless (or (= rc ngit-native--z-ok)
                            (= rc ngit-native--z-buf-error))
                  (throw 'fallback nil)))
              (let* ((head-length (ptr-read-u64 length-address 0))
                     (head (ptr-read-bytes head-address head-length))
                     (space (ngit--byte-pos head ?\s 0))
                     (nul (and space (ngit--byte-pos head 0 (1+ space)))))
                (unless (and space (> space 0) nul (> nul (1+ space)))
                  (throw 'fallback nil))
                (let* ((type-name (substring head 0 space))
                       (size-text (substring head (1+ space) nul))
                       (valid-type (member type-name '("commit" "tree" "blob" "tag")))
                       (digits t) (i 0))
                  (while (and digits (< i (length size-text)))
                    (let ((c (aref size-text i)))
                      (unless (and (>= c ?0) (<= c ?9)) (setq digits nil)))
                    (setq i (1+ i)))
                  (unless (and valid-type digits)
                    (throw 'fallback nil))
                  (let* ((payload-size (string-to-number size-text))
                         (header-size (1+ nul)))
                    (when (<= payload-size ngit-pack--delta-search-limit)
                      (throw 'fallback nil))
                    (let* ((raw-size (+ header-size payload-size))
                           (raw (ngit-native--buffer 1 raw-size))
                           (raw-address (nl-ffi-memory-address raw)))
                      (ptr-write-u64 length-address 0 raw-size)
                      (ptr-write-u64 length-address 8 compressed-size)
                      (let ((rc (logand
                                 (ngit-native--zlib-call
                                  "uncompress2" #'ngit-native--uncompress2
                                  raw-address length-address
                                  (nl-ffi-memory-address compressed)
                                  (+ length-address 8))
                                 #xffffffff)))
                        (unless (and (= rc 0)
                                     (= (ptr-read-u64 length-address 0) raw-size))
                          (throw 'fallback nil)))
                      (unless (string= (ptr-read-bytes raw-address header-size)
                                       (substring head 0 header-size))
                        (throw 'fallback nil))
                      (let* ((type (intern type-name))
                             (pack-head (ngit-pack--entry-header type payload-size))
                             ;; Both helpers use native buffer 0, so consume
                             ;; the loose compressed input before writing the
                             ;; pack header and calculating its CRC.
                             (crc (ngit-pack--crc-bytes pack-head)))
                        (ngit-pack--write-bytes fd pack-head)
                        (let* ((cap (+ payload-size (ash payload-size -12)
                                       (ash payload-size -14) 64))
                               (packed (ngit-native--buffer 0 cap))
                               (packed-address (nl-ffi-memory-address packed)))
                          (ptr-write-u64 length-address 0 cap)
                          (let ((rc (logand
                                     (ngit-native--zlib-call
                                      "compress2" #'ngit-native--compress2
                                      packed-address length-address
                                      (+ raw-address header-size) payload-size level)
                                     #xffffffff)))
                            (unless (= rc 0)
                              (error "ngit: pack deflate failed (zlib %d)" rc)))
                          (let ((packed-size (ptr-read-u64 length-address 0)))
                            (setq crc (ngit-pack--crc-pointer
                                       crc packed-address packed-size))
                            (ngit-pack--write-pointer fd packed-address packed-size)
                            (list (vector (ngit-unhex hex) crc offset)
                                  (vector type nil offset 0 hex payload-size)
                                  (+ (length pack-head) packed-size)))))))))))
        (ngit-native-close path)))))

(defun ngit-pack--emit-materialized (fd repo hex offset recent window depth level
                                        &optional object)
  "Write HEX as a full object or an OFS delta; return (RECORD CANDIDATE SIZE)."
  (let* ((obj (or object (ngit-read-object repo (ngit-unhex hex))))
         (type (car obj)) (data (cdr obj)) (data-size (length data))
         (best nil) (best-base nil)
         (candidates recent) (left window))
    (while (and candidates (> left 0) (> depth 0)
                (<= data-size ngit-pack--delta-search-limit)
                (<= data-size ngit-pack--delta-work-left))
      (let* ((c (car candidates)) (candidate-size (aref c 5)))
        (when (and (eq type (aref c 0)) (< (aref c 3) depth)
                   (<= candidate-size ngit-pack--delta-search-limit))
          (let ((cost (+ data-size candidate-size)))
            (when (and (aref c 1)
                     (<= cost ngit-pack--delta-work-left))
              (setq ngit-pack--delta-work-left (- ngit-pack--delta-work-left cost))
              (let ((d (ngit-pack--create-delta
                        (aref c 1) data
                        (if best (1- (length best)) (1- data-size)))))
                (when (and d (or (null best) (< (length d) (length best))))
                  (setq best d best-base c))))))
        (setq candidates (cdr candidates) left (1- left))))
    (let* ((payload (or best data))
           (kind (if best 'ofs-delta type))
           (header (concat (ngit-pack--entry-header kind (length payload))
                           (if best (ngit-pack--ofs-bytes (- offset (aref best-base 2))) "")))
           (d (if best (1+ (aref best-base 3)) 0)))
      (let ((packed (ngit-pack--compress-entry fd payload level header)))
        (list (vector (ngit-unhex hex) (car packed) offset)
              (vector type (and (<= data-size ngit-pack--delta-search-limit) data)
                      offset d hex data-size)
              (cdr packed))))))

(defun ngit-pack--normalize-oid (hex)
  "Validate HEX as a 40-digit object name and return lowercase HEX."
  (unless (and (stringp hex) (= (length hex) 40))
    (error "ngit: invalid object name %s" hex))
  (let ((i 0) (upper nil) (valid t))
    (while (and valid (< i 40))
      (let ((c (aref hex i)))
        (cond ((or (and (>= c ?0) (<= c ?9))
                   (and (>= c ?a) (<= c ?f))))
              ((and (>= c ?A) (<= c ?F)) (setq upper t))
              (t (setq valid nil))))
      (setq i (1+ i)))
    (unless valid (error "ngit: invalid object name %s" hex))
    (if upper (downcase hex) hex)))

(defun ngit-pack--wanted (hex-list &optional trusted)
  (if (and trusted (boundp 'ngit-object--closure) (consp ngit-object--closure)
           (eq hex-list (car ngit-object--closure)))
      ;; A reachability walk already built this exact validated, normalized
      ;; list and membership table.  Reuse both on the maintenance hot path.
      (cons (cdr ngit-object--closure) hex-list)
    (let ((seen (make-hash-table :test 'equal)) out)
      (dolist (h hex-list)
        (if (not trusted)
            (setq h (ngit-pack--normalize-oid h))
          ;; Graph callers have already validated the names.  Bulk case
          ;; conversion is still required for upper-case reflog text.
          (setq h (downcase h)))
        (unless (gethash h seen) (puthash h t seen) (push h out)))
      (cons seen (nreverse out)))))

(defvar ngit-pack--loose-only nil
  "Non-nil when the caller has verified every selected object is loose.")

(defun ngit-pack-write (repo hex-list base-name &optional window depth trusted)
  "Pack HEX-LIST and write BASE-NAME-SHA.pack/.idx; return SHA.
WINDOW defaults to 10 and DEPTH to 50.  The exact unique input set is
written.  Existing compressed entries and deltas are retained when safe.
TRUSTED skips textual OID validation for a caller-provided graph closure."
  (let* ((wanted-pair (ngit-pack--wanted hex-list trusted))
         (wanted (car wanted-pair)) (ordered (cdr wanted-pair))
         (count (length ordered)) (window (if (null window) 10 window))
         (depth (if (null depth) 50 depth))
         (level (ngit-pack--compression-level repo))
         (tmp (concat base-name "-tmp-" (number-to-string (emacs-pid))))
         (fd nil) (position 12) records recent
         (ngit-pack--hash-stream nil)
         (ngit-pack--payload-temp 'deferred)
         (ngit-pack--delta-work-left ngit-pack--delta-work-budget)
         (out-offset (make-hash-table :test 'equal)))
    (make-directory (file-name-directory base-name) t)
    (setq fd (ngit-pack--open-output tmp))
    (unwind-protect
        (progn
          (setq ngit-pack--hash-stream (ngit-pack--sha1-stream-open))
          (ngit-pack--write-bytes fd (concat "PACK" (ngit-pack--u32 2) (ngit-pack--u32 count)))
          (dolist (pack (unless ngit-pack--loose-only (ngit-repo-packs repo)))
            (ngit-pack--validate-source pack)
            (let* ((infos (ngit-pack--idx-info pack))
                   (chosen-count 0) duplicate previous)
              (dolist (info infos)
                (let* ((hex (aref info 3))
                       (same (and previous (string= previous hex))))
                  ;; Duplicate records are adjacent in an OID-sorted index.
                  ;; Detect them even when the duplicated object is not
                  ;; selected: another selected delta may depend on one.
                  (when same (setq duplicate t))
                  (when (and (gethash hex wanted) (not (gethash hex out-offset)))
                    ;; Count selected OIDs rather than physical records.
                    (unless same (setq chosen-count (1+ chosen-count))))
                  (setq previous hex)))
              (cond
               ;; Duplicate entries can participate in otherwise legal
               ;; forward REF/OFS chains.  Dropping one physical occurrence
               ;; while retaining those deltas can create a cycle, so resolve
               ;; this rare source pack completely and emit each selected OID
               ;; once from its materialized value.
               ((and duplicate (> chosen-count 0))
                (dolist (entry (ngit-pack--scan (aref pack 0)))
                  (let ((hex (ngit-object--hex (aref entry 8))))
                    (when (and (gethash hex wanted)
                               (not (gethash hex out-offset)))
                      (let ((r (ngit-pack--emit-materialized
                                fd repo hex position recent window depth level
                                (cons (aref entry 6) (aref entry 7)))))
                        (push (car r) records) (push (cadr r) recent)
                        (when (> (length recent) window)
                          (setq recent (butlast recent)))
                        (puthash hex position out-offset)
                        (setq position (+ position (nth 2 r))))))))
               ;; Whole-pack bulk path.  All raw entry bytes and OFS distances
               ;; remain identical after a constant offset translation.
               ((and (> chosen-count 0) (= chosen-count (length infos)))
                (let* ((path (aref pack 0))
                       (body (- (ngit-native-file-size path) 32))
                       (shift (- position 12)))
                  (ngit-pack--copy-range path fd 12 body)
                  (dolist (info infos)
                    (let ((hex (aref info 3)))
                      (push (vector (aref info 0) (aref info 1) (+ shift (aref info 2))) records)
                      (puthash hex (+ shift (aref info 2)) out-offset)))
                  (setq position (+ position body))))
               ((> chosen-count 0)
                (let* ((by-offset (sort (copy-sequence infos)
                                        (lambda (a b) (< (aref a 2) (aref b 2)))))
                       (end-by (make-hash-table :test 'eql))
                       (oid-by (make-hash-table :test 'eql))
                       chosen
                       (path (aref pack 0))
                       (pack-size (ngit-native-file-size path))
                       (pack-end (- pack-size 20))
                       (header-owner (nl-ffi-memory-allocate ngit-pack--header-window))
                       (header-cache (vector header-owner
                                             (nl-ffi-memory-address header-owner)
                                             0 0 pack-size))
                       raw-start raw-end)
                  (unwind-protect
                      (progn
                        (let ((xs by-offset))
                          (while xs
                            (let* ((cur (car xs))
                                   (hex (aref cur 3)))
                              (puthash (aref cur 2)
                                       (if (cdr xs) (aref (cadr xs) 2) pack-end) end-by)
                              (puthash (aref cur 2) hex oid-by)
                              ;; Reuse this physical-order pass to select the
                              ;; entries without sorting a second list.
                              (when (and (gethash hex wanted)
                                         (not (gethash hex out-offset)))
                                (push cur chosen)))
                            (setq xs (cdr xs))))
                        (setq chosen (nreverse chosen))
                        (dolist (info chosen)
                          (let* ((hex (aref info 3))
                                 (old (aref info 2)) (end (gethash old end-by))
                                 (head (ngit-pack--parse-header-windowed path old header-cache))
                                 (type (aref head 0)) (base (aref head 3))
                                 (base-hex
                                  (cond ((eq type 'ofs-delta)
                                         (gethash base oid-by))
                                        ((eq type 'ref-delta) (ngit-object--hex base))))
                                 (base-offset (and base-hex
                                                   (gethash base-hex out-offset)))
                                 (safe (or (not (memq type '(ofs-delta ref-delta)))
                                           (and base-hex (gethash base-hex wanted)
                                                (or (eq type 'ref-delta)
                                                    base-offset))))
                                 ;; An OFS entry can retain its complete raw bytes
                                 ;; when base and child moved by the same amount.
                                 (raw (and safe
                                           (or (not (eq type 'ofs-delta))
                                               (= (- position old)
                                                  (- base-offset base))))))
                            (cond
                             (raw
                              (unless (and raw-start (= old raw-end))
                                (when raw-start
                                  (ngit-pack--copy-range path fd raw-start (- raw-end raw-start)))
                                (setq raw-start old))
                              (setq raw-end end)
                              (push (vector (aref info 0) (aref info 1) position) records)
                              (puthash hex position out-offset)
                              (setq position (+ position (- end old))))
                             ((not safe)
                              (when raw-start
                                (ngit-pack--copy-range path fd raw-start (- raw-end raw-start))
                                (setq raw-start nil raw-end nil))
                              (let ((r (ngit-pack--emit-materialized fd repo hex position recent
                                                                     window depth level)))
                                (push (car r) records) (push (cadr r) recent)
                                (when (> (length recent) window)
                                  (setq recent (butlast recent)))
                                (puthash hex position out-offset)
                                (setq position (+ position (nth 2 r)))))
                             (t
                              (when raw-start
                                (ngit-pack--copy-range path fd raw-start (- raw-end raw-start))
                                (setq raw-start nil raw-end nil))
                              (let* ((new-head
                                      (concat (ngit-pack--entry-header
                                               'ofs-delta (aref head 1))
                                              (ngit-pack--ofs-bytes
                                               (- position base-offset))))
                                     (payload-at (aref head 2))
                                     (payload-len (- end payload-at))
                                     (crc (ngit-pack--crc-bytes new-head)))
                                (ngit-pack--write-bytes fd new-head)
                                (setq crc (ngit-pack--copy-range
                                           path fd payload-at payload-len crc))
                                (push (vector (aref info 0) crc position) records)
                                (puthash hex position out-offset)
                                (setq position (+ position (length new-head) payload-len)))))))
                        (when raw-start
                          (ngit-pack--copy-range path fd raw-start (- raw-end raw-start))))
                    (nl-ffi-memory-release header-owner)))))))
          ;; Objects not owned by a source pack (normally loose objects).
          (dolist (hex ordered)
            (unless (gethash hex out-offset)
              ;; Every pack index has already been visited.  An unwritten
              ;; object must therefore be loose; avoid searching all packs
              ;; twice before reading it.
              (let* ((r (or (ngit-pack--emit-large-loose
                             fd repo hex position level)
                            (let ((obj (ngit--read-loose repo hex)))
                              (unless obj (error "fatal: bad object %s" hex))
                              (ngit-pack--emit-materialized
                               fd repo hex position recent window depth level obj)))))
                (push (car r) records) (push (cadr r) recent)
                (when (> (length recent) window) (setq recent (butlast recent)))
                (puthash hex position out-offset)
                (setq position (+ position (nth 2 r))))))
          (unless (= (length records) count)
            (error "ngit: packed %d objects, expected %d" (length records) count))
          (let ((stream-sum (ngit-pack--sha1-stream-final ngit-pack--hash-stream)))
            ;; Disable streaming before writing the checksum trailer itself.
            (setq ngit-pack--hash-stream nil)
            (let* ((sum (or stream-sum (ngit-pack--sha1-file tmp position)))
                   (hex (ngit-hex sum))
                   (pack-path (concat base-name "-" hex ".pack"))
                   (idx-path (concat base-name "-" hex ".idx")))
              (ngit-pack--write-bytes fd sum)
              (syscall-direct ngit-pack--sys-close fd 0 0 0 0 0) (setq fd nil)
              (set-file-modes tmp #o444)
              (unless (file-exists-p pack-path) (rename-file tmp pack-path))
              (when (file-exists-p tmp) (delete-file tmp))
              (let ((idx-tmp (concat idx-path ".tmp-" (number-to-string (emacs-pid)))))
                (ngit-write-file idx-tmp (ngit-pack--idx-bytes records sum))
                (rename-file idx-tmp idx-path t))
              hex)))
      (ngit-pack--sha1-stream-close ngit-pack--hash-stream)
      (when (stringp ngit-pack--payload-temp)
        (ngit-native-close ngit-pack--payload-temp)
        (when (file-exists-p ngit-pack--payload-temp)
          (delete-file ngit-pack--payload-temp)))
      (when fd (syscall-direct ngit-pack--sys-close fd 0 0 0 0 0))
      (when (file-exists-p tmp) (delete-file tmp)))))

;;;; Pack scanning and commands

(defun ngit-pack--object-id (type data)
  (ngit-native-object-id type data))

(defun ngit-pack--scan (path)
  "Validate PATH and return resolved entry records in pack order.
Records are [OFF END STORED-TYPE STORED-SIZE PAYLOAD BASE TYPE DATA OID
DEPTH BASE-OID CRC]."
  (let* ((size (ngit-native-file-size path))
         (head (ngit-native-read-bytes path 0 12)))
    (unless (and (>= size 32) (string= (substring head 0 4) "PACK")
                 (= (ngit--be32 head 4) 2))
      (error "fatal: pack is not a version 2 pack file"))
    (let* ((count (ngit--be32 head 8)) (position 12) records
           (by-offset (make-hash-table :test 'eql))
           (by-oid (make-hash-table :test 'equal)))
      (dotimes (_ count)
        (let* ((off position) (h (ngit-pack--parse-header path off))
               (type (aref h 0)) (stored-size (aref h 1)) (data-at (aref h 2))
               (inflated (ngit-native-inflate-file path data-at stored-size))
               (payload (car inflated)) (end (+ data-at (cdr inflated)))
               (r (vector off end type stored-size payload (aref h 3)
                          nil nil nil nil nil (ngit-pack--crc-range path off (- end off)))))
          (push r records) (puthash off r by-offset) (setq position end)))
      (setq records (nreverse records))
      (unless (= position (- size 20))
        (error "fatal: pack has junk at the end"))
      (let ((expected (ngit-native-read-bytes path position 20))
            (actual (ngit-pack--sha1-file path position)))
        (unless (string= expected actual) (error "fatal: pack checksum mismatch")))
      ;; Resolve direct entries first.  Delta resolution below is a fixed
      ;; point: a legal OFS delta may depend on an earlier REF delta whose
      ;; named base appears later in the pack.
      (dolist (r records)
        (let ((stored (aref r 2)))
          (when (memq stored '(commit tree blob tag))
            (aset r 6 stored) (aset r 7 (aref r 4)) (aset r 9 0)
            (aset r 8 (ngit-pack--object-id stored (aref r 7)))
            (puthash (aref r 8) r by-oid))))
      (let ((progress t))
        (while progress
          (setq progress nil)
          (dolist (r records)
            (when (and (memq (aref r 2) '(ofs-delta ref-delta)) (null (aref r 8)))
              (let ((base (if (eq (aref r 2) 'ofs-delta)
                              (gethash (aref r 5) by-offset)
                            (gethash (aref r 5) by-oid))))
                (when base
                  (when (aref base 8)
                    (aset r 6 (aref base 6))
                    (aset r 7 (ngit--apply-delta (aref base 7) (aref r 4)))
                    (aset r 8 (ngit-pack--object-id (aref r 6) (aref r 7)))
                    (aset r 9 (1+ (aref base 9))) (aset r 10 (aref base 8))
                    (puthash (aref r 8) r by-oid)
                    (setq progress t)))))))
        (dolist (r records)
          (unless (aref r 8)
            (error "fatal: unresolved %s base %s" (aref r 2)
                   (if (stringp (aref r 5)) (ngit-hex (aref r 5)) (aref r 5))))))
      records)))

(defun ngit-pack--index-pack (path idx-path)
  "Index PATH at IDX-PATH; return its hexadecimal checksum and records."
  (let* ((records (ngit-pack--scan path))
         (size (ngit-native-file-size path))
         (sum (ngit-native-read-bytes path (- size 20) 20)) index)
    (dolist (r records)
      (push (vector (aref r 8) (aref r 11) (aref r 0)) index))
    (let ((tmp (concat idx-path ".tmp-" (number-to-string (emacs-pid))))
          (bytes (ngit-pack--idx-bytes index sum)))
      (ngit-write-file tmp bytes)
      (rename-file tmp idx-path t))
    (cons (ngit-hex sum) records)))

(defun ngit-pack--slurp-stdin ()
  (let* ((owner (ngit-native--buffer 0 65536))
         (addr (nl-ffi-memory-address owner)) parts done)
    (while (not done)
      (let ((n (syscall-direct ngit-pack--sys-read 0 addr 65536 0 0 0)))
        (cond ((< n 0) (error "ngit: stdin read failed (errno %d)" (- n)))
              ((= n 0) (setq done t))
              (t (push (ptr-read-bytes addr n) parts)))))
    (apply #'concat (nreverse parts))))

(defun ngit-pack--copy-stdin-raw (fd)
  "Copy fd 0 byte-for-byte to FD using an external buffer."
  (let* ((owner (ngit-native--buffer 0 65536))
         (addr (nl-ffi-memory-address owner))
         (done nil))
    (while (not done)
      (let ((n (syscall-direct ngit-pack--sys-read 0 addr 65536 0 0 0)))
        (cond ((< n 0) (error "ngit: stdin read failed (errno %d)" (- n)))
              ((= n 0) (setq done t))
              (t (ngit-pack--write-pointer fd addr n)))))))

(defun ngit-pack--stdin-lines ()
  (split-string (decode-coding-string (ngit-pack--slurp-stdin) 'utf-8) "[\r\n]+" t))

(defun ngit-pack--revision-set (repo lines all)
  (let (positive negative)
    (when all (setq positive (ngit-pack-roots repo)))
    (dolist (line lines)
      (if (string-prefix-p "^" line)
          (push (ngit-rev-parse repo (substring line 1)) negative)
        (push (ngit-rev-parse repo line) positive)))
    (let ((drop (make-hash-table :test 'equal)) out)
      (dolist (hex (ngit-pack-reachable repo negative)) (puthash hex t drop))
      (dolist (hex (ngit-pack-reachable repo positive))
        (unless (gethash hex drop) (push hex out)))
      (nreverse out))))

(defun ngit-cmd-pack-objects (repo args)
  (let ((stdout nil) (revs nil) (all nil) (window 10) (depth 50) base)
    (while args
      (let ((a (pop args)))
        (cond ((string= a "--stdout") (setq stdout t))
              ((string= a "--revs") (setq revs t))
              ((string= a "--all") (setq all t))
              ((string-prefix-p "--window=" a) (setq window (string-to-number (substring a 9))))
              ((string= a "--window") (setq window (string-to-number (pop args))))
              ((string-prefix-p "--depth=" a) (setq depth (string-to-number (substring a 8))))
              ((string= a "--depth") (setq depth (string-to-number (pop args))))
              ((string-prefix-p "-" a) (ngit--die "fatal: unsupported pack-objects option %s" a))
              (base (ngit--die "fatal: too many pack base names"))
              (t (setq base a)))))
    (unless (or stdout base) (ngit--die "fatal: pack-objects requires a base name or --stdout"))
    (let* ((lines (ngit-pack--stdin-lines))
           ;; --all names all refs for the revision walker even when the
           ;; redundant --revs spelling is omitted; unreachable objects are
           ;; not selected.
           (oids (if (or revs all) (ngit-pack--revision-set repo lines all) lines))
           (prefix (or base (make-temp-file "ngit-pack-stdout-")))
           (_ (when (and stdout (file-exists-p prefix)) (delete-file prefix)))
           (hex (ngit-pack-write repo oids prefix window depth))
           (path (concat prefix "-" hex ".pack")))
      (if stdout
          (unwind-protect
              (ngit-pack--copy-range path 1 0 (ngit-native-file-size path))
            (delete-file path)
            (let ((idx (concat prefix "-" hex ".idx")))
              (when (file-exists-p idx) (delete-file idx))))
        (ngit--emit hex "\n"))
      0)))

(defun ngit-cmd-index-pack (repo args)
  (let ((stdin nil) path)
    (dolist (a args)
      (cond ((string= a "--stdin") (setq stdin t))
            ((member a '("--strict")) nil)
            ((string-prefix-p "-" a) (ngit--die "fatal: unsupported index-pack option %s" a))
            (path (ngit--die "fatal: too many pack files"))
            (t (setq path (expand-file-name a)))))
    (if stdin
        (let* ((dir (expand-file-name "objects/pack/" (ngit-repo-commondir repo)))
               (_ (make-directory dir t))
               (tmp (expand-file-name (concat "pack-tmp-" (number-to-string (emacs-pid))) dir))
               (tmp-idx (concat tmp ".idx"))
               (fd (ngit-pack--open-output tmp)))
          (unwind-protect
              (progn
                (ngit-pack--copy-stdin-raw fd)
                (syscall-direct ngit-pack--sys-close fd 0 0 0 0 0) (setq fd nil)
                ;; Validate and build the index entirely under temporary
                ;; names before publishing either file.
                (let* ((result (ngit-pack--index-pack tmp tmp-idx))
                       (hex (car result))
                       (pack (expand-file-name (concat "pack-" hex ".pack") dir))
                       (idx (expand-file-name (concat "pack-" hex ".idx") dir)))
                  (if (file-exists-p pack) (delete-file tmp) (rename-file tmp pack))
                  (rename-file tmp-idx idx t)
                  (ngit--emit "pack\t" hex "\n")))
            (when fd (syscall-direct ngit-pack--sys-close fd 0 0 0 0 0))
            (when (file-exists-p tmp) (delete-file tmp))
            (when (file-exists-p tmp-idx) (delete-file tmp-idx))))
      (unless path (ngit--die "fatal: index-pack requires a pack file"))
      (let* ((idx (concat (substring path 0 -5) ".idx"))
             (result (ngit-pack--index-pack path idx)))
        (ngit--emit (car result) "\n")))
    0))

(defun ngit-pack--plural (n)
  (if (= n 1) "object" "objects"))

(defun ngit-cmd-verify-pack (_repo args)
  (let ((verbose nil) files)
    (dolist (a args)
      (cond ((or (string= a "-v") (string= a "--verbose")) (setq verbose t))
            ((string= a "--") nil)
            ((string-prefix-p "-" a) (ngit--die "fatal: unsupported verify-pack option %s" a))
            (t (push a files))))
    (dolist (arg (nreverse files))
      (let* ((display-pack (if (string-suffix-p ".idx" arg)
                               (concat (substring arg 0 -4) ".pack") arg))
             (idx (expand-file-name arg))
             (pack (if (string-suffix-p ".idx" idx) (concat (substring idx 0 -4) ".pack") idx))
             (records (ngit-pack--scan pack))
             (non 0) (chains (make-hash-table :test 'eql)) (max-depth 0))
        (when (string-suffix-p ".idx" idx)
          (let* ((sum (ngit-native-read-bytes pack (- (ngit-native-file-size pack) 20) 20))
                 index)
            (dolist (r records)
              (push (vector (aref r 8) (aref r 11) (aref r 0)) index))
            (unless (string= (ngit--read-file idx) (ngit-pack--idx-bytes index sum))
              (error "fatal: pack index %s is corrupt" idx))))
        (when verbose
          (dolist (r records)
            (let ((depth (aref r 9)))
              (ngit--emit (ngit-hex (aref r 8)) " "
                          (format "%-6s %d %d %d" (symbol-name (aref r 6))
                                  (if (> depth 0) (aref r 3) (length (aref r 7)))
                                  (- (aref r 1) (aref r 0)) (aref r 0)))
              (if (> depth 0)
                  (progn (ngit--emit " " (number-to-string depth) " "
                                     (ngit-hex (aref r 10)) "\n")
                         (puthash depth (1+ (or (gethash depth chains) 0)) chains)
                         (setq max-depth (max max-depth depth)))
                (ngit--emit "\n") (setq non (1+ non)))))
          (ngit--emit "non delta: " (number-to-string non) " " (ngit-pack--plural non) "\n")
          (let ((d 1))
            (while (<= d max-depth)
              (let ((n (gethash d chains)))
                (when n (ngit--emit "chain length = " (number-to-string d) ": "
                                    (number-to-string n) " " (ngit-pack--plural n) "\n")))
              (setq d (1+ d)))))
        (ngit--emit display-pack ": ok\n")))
    0))

(provide 'ngit-pack)

;;; ngit-pack.el ends here
