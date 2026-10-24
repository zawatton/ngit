;;; ngit-transport-index.el --- bounded-memory received-pack indexing -*- lexical-binding: t; -*-

;;; Commentary:

;; The inspection scanner intentionally retains every inflated object.  A
;; received clone pack can be much larger than the Lisp heap, so this indexer
;; retains only compact metadata and object names.  Delta bases are inflated
;; on demand and held in a cache bounded by both entries and bytes.  OFS
;; workers walk descendants while their parent is live, avoiding repeated
;; reconstruction when a delta chain exceeds the cache.

;;; Code:

(require 'ngit-pack)
(require 'ngit-transport-delta)
(require 'ngit-transport-memory
         (expand-file-name "ngit-transport-memory.el"
                           (file-name-directory
                            (or (and (boundp 'byte-compile-current-file)
                                     byte-compile-current-file)
                                load-file-name buffer-file-name))))

(defconst ngit-transport-index--cache-count 512)
(defconst ngit-transport-index--cache-bytes (* 1024 1024 1024))
(defconst ngit-transport-index--cache-object-limit (* 128 1024 1024))
(defconst ngit-transport-index--native-threshold (* 1024 1024))
(defconst ngit-transport-index--sys-mmap 9)
(defconst ngit-transport-index--sys-munmap 11)
(defconst ngit-transport-index--prot-read 1)
(defconst ngit-transport-index--map-private 2)

;; Records are [OFF END STORED SIZE DATA-AT BASE TYPE OID CRC DEPTH CHILDREN
;; ROOT/WORKER FOREST-WEIGHT].
(defvar ngit-transport-index--path nil)
(defvar ngit-transport-index--by-offset nil)
(defvar ngit-transport-index--by-oid nil)
(defvar ngit-transport-index--cache nil)
(defvar ngit-transport-index--cache-order nil)
(defvar ngit-transport-index--cache-tail nil)
(defvar ngit-transport-index--cached-count 0)
(defvar ngit-transport-index--cache-size 0)
(defvar ngit-transport-index--map-address nil)
(defvar ngit-transport-index--map-size nil)
(defvar ngit-transport-index--inflated-id nil)
(defvar ngit-transport-index--workers 8)
(defvar ngit-transport-index--parallel-threshold 16384)

(defun ngit-transport-index--single-thread-p ()
  (= (length (directory-files
              (concat "/proc/" (number-to-string (emacs-pid)) "/task")
              nil "\\`[0-9]")) 1))

(ffi:defun ngit-transport-index--memcpy "memcpy"
           [:pointer :pointer :pointer :uint64]
           "memcpy(destination, source, count).")

(defun ngit-transport-index--data-size (data)
  (if (stringp data) (length data) (aref data 2)))

(defun ngit-transport-index--release (data)
  (when (and (vectorp data) (> (aref (aref data 0) 1) 0))
    (nl-ffi-memory-release (aref data 0))))

(defun ngit-transport-index--native-copy (address size)
  (let* ((owner (nl-ffi-memory-allocate size))
         (destination (nl-ffi-memory-address owner)) success)
    (unwind-protect
        (progn
          (ngit-native--zlib-call "memcpy" #'ngit-transport-index--memcpy destination address size)
          (setq success t)
          (vector owner destination size))
      (unless success (nl-ffi-memory-release owner)))))

(defun ngit-transport-index--apply (base delta)
  (let* ((header (ngit--delta-varint delta 0))
         (size (car (ngit--delta-varint delta (cdr header)))))
    (if (and (not (and (boundp 'ngit-transport-kernel--ready)
                       ngit-transport-kernel--ready (> (length delta) 1024)))
             (stringp base)
             (< (length base) ngit-transport-index--native-threshold)
             (< size ngit-transport-index--native-threshold))
        (ngit--apply-delta base delta)
      (let ((temporary nil) result)
        (unwind-protect
            (progn
              (when (stringp base)
                (let* ((owner (nl-ffi-memory-allocate (length base)))
                       (address (nl-ffi-memory-address owner)))
                  (setq temporary (vector owner address (length base)))
                  (ptr-write-bytes address base)
                  (setq base temporary)))
              (setq result (ngit-transport-delta-apply (aref base 1) (aref base 2) delta))
              (if (< (aref result 2) ngit-transport-index--native-threshold)
                  (unwind-protect (ptr-read-bytes (aref result 1) (aref result 2))
                    (ngit-transport-index--release result))
                result))
          (when temporary (ngit-transport-index--release temporary)))))))

(defun ngit-transport-index--cache-get (off)
  (gethash off ngit-transport-index--cache))

(defun ngit-transport-index--cache-put (off data)
  (when (and (> ngit-transport-index--cache-count 0)
             (<= (ngit-transport-index--data-size data)
                 ngit-transport-index--cache-object-limit)
             (<= (ngit-transport-index--data-size data)
                 ngit-transport-index--cache-bytes))
    (unless (gethash off ngit-transport-index--cache)
      (puthash off data ngit-transport-index--cache)
      (let ((cell (list off)))
        (if ngit-transport-index--cache-tail
            (setcdr ngit-transport-index--cache-tail cell)
          (setq ngit-transport-index--cache-order cell))
        (setq ngit-transport-index--cache-tail cell))
      (setq ngit-transport-index--cached-count
            (1+ ngit-transport-index--cached-count))
      (setq ngit-transport-index--cache-size
            (+ ngit-transport-index--cache-size (ngit-transport-index--data-size data))))
    (while (or (> ngit-transport-index--cached-count
                  ngit-transport-index--cache-count)
               (> ngit-transport-index--cache-size
                  ngit-transport-index--cache-bytes))
      (let* ((old (car ngit-transport-index--cache-order))
             (old-data (gethash old ngit-transport-index--cache)))
        (setq ngit-transport-index--cache-size
              (- ngit-transport-index--cache-size (ngit-transport-index--data-size old-data)))
        (ngit-transport-index--release old-data)
        (remhash old ngit-transport-index--cache)
        (setq ngit-transport-index--cached-count
              (1- ngit-transport-index--cached-count))
        (setq ngit-transport-index--cache-order
              (cdr ngit-transport-index--cache-order))
        (unless ngit-transport-index--cache-order
          (setq ngit-transport-index--cache-tail nil)))))
  data)

(defun ngit-transport-index--base (record)
  (if (eq (aref record 2) 'ofs-delta)
      (gethash (aref record 5) ngit-transport-index--by-offset)
    (gethash (aref record 5) ngit-transport-index--by-oid)))

(defun ngit-transport-index--object-id (type data)
  (if (vectorp data)
      (ngit-native-object-id-pointer type (aref data 1) (aref data 2))
    (ngit-native-object-id type data)))

(defun ngit-transport-index--inflate (data-at size &optional direct-type force-data omit-data)
  "Inflate mapped entry at DATA-AT to exactly SIZE bytes.
Return (DATA . CONSUMED-COMPRESSED-BYTES).  When DIRECT-TYPE names a
non-delta object, hash the native output in place and set
`ngit-transport-index--inflated-id'.  Large direct objects return nil DATA."
  (when (or (< data-at 12) (> data-at (- ngit-transport-index--map-size 20))
            (< size 0) (> size #x7fffffff))
    (error "fatal: malformed pack entry bounds"))
  (let* ((available (- ngit-transport-index--map-size 20 data-at))
         (dst-owner (ngit-native--buffer 1 (max 1 size)))
         (len-owner (ngit-native--buffer 2 16))
         (dst (nl-ffi-memory-address dst-owner))
         (lens (nl-ffi-memory-address len-owner)))
    (ptr-write-u64 lens 0 size)
    (ptr-write-u64 lens 8 available)
    (let ((rc (ngit-native--zlib-call
               "uncompress2" #'ngit-native--uncompress2 dst lens
               (+ ngit-transport-index--map-address data-at) (+ lens 8))))
      (setq rc (logand rc #xffffffff))
      (when (>= rc #x80000000) (setq rc (- rc #x100000000)))
      (unless (= rc ngit-native--z-ok)
        (error "fatal: pack inflate failed at %d (zlib %d)" data-at rc))
      (unless (= (ptr-read-u64 lens 0) size)
        (error "fatal: pack entry inflated size mismatch at %d" data-at))
      (setq ngit-transport-index--inflated-id
            (and direct-type
                 (ngit-native-object-id-pointer direct-type dst size)))
      (cons (and (not omit-data) (or force-data (null direct-type)
                                     (<= size ngit-transport-index--cache-object-limit))
                 (if (and direct-type (>= size ngit-transport-index--native-threshold))
                     (ngit-transport-index--native-copy dst size)
                   (ptr-read-bytes dst size)))
            (ptr-read-u64 lens 8)))))

(defun ngit-transport-index--header (offset)
  (when (or (< offset 12) (>= offset (- ngit-transport-index--map-size 20)))
    (error "fatal: truncated pack entry"))
  (ngit-pack--parse-header-bytes
   (ptr-read-bytes (+ ngit-transport-index--map-address offset)
                   (min 64 (- ngit-transport-index--map-size 20 offset)))
   offset))

(defun ngit-transport-index--delta-varint-pointer (address offset size)
  "Read one bounded Git delta varint at ADDRESS+OFFSET within SIZE."
  (let ((value 0) (shift 0) (count 0) byte)
    (while (progn
             (when (or (>= offset size) (>= count 9))
               (error "fatal: malformed pack delta varint"))
             (setq byte (ptr-read-u8 address offset)
                   offset (1+ offset)
                   value (+ value (* (ash 1 shift) (logand byte 127)))
                   shift (+ shift 7)
                   count (1+ count))
             (/= 0 (logand byte 128))))
    (cons value offset)))

(defun ngit-transport-index--delta-output-size (size)
  "Return result size from the delta currently in native inflate buffer 1."
  (let* ((address (nl-ffi-memory-address (aref ngit-native--buffers 1)))
         (base (ngit-transport-index--delta-varint-pointer address 0 size)))
    (car (ngit-transport-index--delta-varint-pointer
          address (cdr base) size))))

(defun ngit-transport-index--materialize (record)
  "Return fully materialized data for resolved RECORD."
  (or (ngit-transport-index--cache-get (aref record 0))
      (let* ((stored (aref record 2))
             (direct (memq stored '(commit tree blob tag)))
             (payload (car (ngit-transport-index--inflate
                            (aref record 4) (aref record 3) (and direct stored) t)))
             (data (if (memq stored '(commit tree blob tag)) payload
                     (let ((base (ngit-transport-index--base record)))
                       (unless (and base (aref base 7))
                         (error "fatal: unresolved %s base" stored))
                       (ngit-transport-index--apply-base base payload)))))
        (ngit-transport-index--cache-put (aref record 0) data))))

(defun ngit-transport-index--apply-base (record delta)
  (let ((data (ngit-transport-index--materialize record)))
    (unwind-protect (ngit-transport-index--apply data delta)
      (unless (eq data (ngit-transport-index--cache-get (aref record 0)))
        (ngit-transport-index--release data)))))

(defun ngit-transport-index--resolve (record &optional payload)
  "Resolve RECORD when its base is known; return non-nil on success."
  (unless (aref record 7)
    (let ((stored (aref record 2)))
      (if (memq stored '(commit tree blob tag))
          (aset record 6 stored)
        (let ((base (ngit-transport-index--base record)))
          ;; OFS bases always precede their delta, so recursive resolution is
          ;; acyclic even when a worker starts in the middle of a pack.
          (when (and base (eq stored 'ofs-delta) (not (aref base 7)))
            (ngit-transport-index--resolve base))
          (when (and base (aref base 7))
            (aset record 6 (aref base 6))
            (aset record 9 (1+ (aref base 9))))))
      (when (aref record 6)
        (let ((data (if payload
                        (if (memq stored '(commit tree blob tag)) payload
                          (ngit-transport-index--apply-base
                           (ngit-transport-index--base record) payload))
                      (ngit-transport-index--materialize record))))
          (unwind-protect
              (progn
                (aset record 7 (ngit-transport-index--object-id (aref record 6) data))
                (puthash (aref record 7) record ngit-transport-index--by-oid)
                (ngit-transport-index--cache-put (aref record 0) data))
            (unless (eq data (ngit-transport-index--cache-get (aref record 0)))
              (ngit-transport-index--release data)))
          t)))))

(defun ngit-transport-index--walk (record base-data type worker)
  "Resolve RECORD and its children assigned to WORKER while DATA stays live."
  ;; Children bypass the eviction cache, so recursive siblings cannot free
  ;; a pinned parent.  Iterate through the last child to bound linear chains.
  (let* ((stored (aref record 2))
         (direct (memq stored '(commit tree blob tag)))
         (payload (and (not direct)
                       (car (ngit-transport-index--inflate
                             (aref record 4) (aref record 3)))))
         (data (if direct (ngit-transport-index--materialize record)
                 (ngit-transport-index--apply base-data payload)))
         (current record) (current-type type) children next parent)
    (unwind-protect
        (while current
          (unless (memq (aref current 2) '(commit tree blob tag))
            (aset current 6 current-type)
            (aset current 9 (1+ (aref (ngit-transport-index--base current) 9))))
          (unless (aref current 7)
            (aset current 7 (ngit-transport-index--object-id (aref current 6) data))
            (puthash (aref current 7) current ngit-transport-index--by-oid))
          (setq children nil)
          (dolist (child (aref current 10))
            (when (eq (aref child 11) worker) (push child children)))
          (while (cdr children)
            (ngit-transport-index--walk
             (car children) data (aref current 6) worker)
            (setq children (cdr children)))
          (if children
              (progn
                (setq next
                      (ngit-transport-index--apply
                       data (car (ngit-transport-index--inflate
                                  (aref (car children) 4)
                                  (aref (car children) 3)))))
                (setq parent data)
                (setq current-type (aref current 6))
                (setq current (car children))
                (setq data next)
                (setq next nil)
                (unless (eq parent
                            (ngit-transport-index--cache-get
                             (aref (ngit-transport-index--base current) 0)))
                  (ngit-transport-index--release parent))
                (setq parent nil))
            (unless (eq data
                        (ngit-transport-index--cache-get (aref current 0)))
              (ngit-transport-index--release data))
            (setq data nil)
            (setq current nil)))
      (when data
        (unless (eq data (ngit-transport-index--cache-get (aref current 0)))
          (ngit-transport-index--release data))))))

(defun ngit-transport-index--walk-worker (records worker)
  "Resolve complete OFS forests assigned to WORKER."
  (dolist (record records)
    (when (and (eq (aref record 11) worker)
               (memq (aref record 2) '(commit tree blob tag))
               (aref record 10))
      (ngit-transport-index--walk record nil (aref record 6) worker))))

(defun ngit-transport-index--parallel (records path)
  "Resolve complete OFS forests in bounded fork workers."
  (let* ((loads (make-vector ngit-transport-index--workers 0))
         roots children (status (nl-ffi-memory-allocate 8))
         (profile (equal (getenv "NGIT_PROFILE") "1"))
         (status-address (nl-ffi-memory-address status)) success)
    (unwind-protect
        (progn
          (dolist (record records)
            (when (eq (aref record 11) record) (push record roots)))
          (setq roots
                (sort roots (lambda (a b) (> (aref a 12) (aref b 12)))))
          ;; Largest-processing-time-first scheduling keeps every ancestry
          ;; forest intact while balancing expanded bytes and delta work.
          (dolist (root roots)
            (let ((worker 0) (candidate 1))
              (while (< candidate ngit-transport-index--workers)
                (when (< (aref loads candidate) (aref loads worker))
                  (setq worker candidate))
                (setq candidate (1+ candidate)))
              (aset root 11 worker)
              (aset loads worker (+ (aref loads worker) (aref root 12)))))
          (dolist (record records)
            (unless (integerp (aref record 11))
              (aset record 11 (aref (aref record 11) 11))))
          (when profile
            (ngit-native-write-stderr
             (format "profile/transport-forests: %d loads=%S\n"
                     (length roots) loads)))
          (unless (ngit-transport-index--single-thread-p)
            (error "fatal: threaded runtime cannot fork pack index workers"))
          (let ((worker 0))
            (while (< worker ngit-transport-index--workers)
              (let* ((file (make-temp-file (concat path ".worker-")))
                     (pid (syscall-direct 57 0 0 0 0 0 0)))
                (when (< pid 0)
                  (delete-file file)
                  (error "fatal: cannot fork pack indexer"))
                (when (= pid 0)
                  (let ((exit-code 1))
                    (unwind-protect
                        (condition-case err
                            (let ((done 0) ids)
                              (ngit-transport-index--walk-worker records worker)
                              (dolist (record records)
                                (when (eq (aref record 11) worker)
                                  (unless (aref record 7)
                                    (error "fatal: unresolved pack delta"))
                                  (push (aref record 7) ids)
                                  (setq done (1+ done))
                                  (when (and profile (= (% done 2000) 0))
                                    (ngit-native-write-stderr
                                     (format "profile/transport-worker %d: %d\n"
                                             worker done)))))
                              (ngit-write-file file (apply #'concat (nreverse ids)))
                              (setq exit-code 0))
                          (error
                           (ngit-native-write-stderr
                            (concat (error-message-string err) "\n"))))
                      (syscall-direct 60 exit-code 0 0 0 0 0))))
                (push (vector pid file worker nil) children)
                (setq worker (1+ worker)))))
          (dolist (child children)
            (let ((waited -4))
              (while (= waited -4)
                (setq waited (syscall-direct 61 (aref child 0) status-address 0 0 0 0)))
              (when (= waited (aref child 0)) (aset child 3 t))
              (unless (and (= waited (aref child 0))
                           (= (ptr-read-u32 status-address 0) 0))
                (error "fatal: pack index worker failed")))
            (let ((bytes (ngit--read-file (aref child 1)))
                  (worker (aref child 2)) (position 0))
              (dolist (record records)
                (when (eq (aref record 11) worker)
                  (when (> (+ position 20) (length bytes))
                    (error "fatal: truncated pack index worker result"))
                  (aset record 7 (substring bytes position (+ position 20)))
                  (setq position (+ position 20))))
              (unless (= position (length bytes))
                (error "fatal: invalid pack index worker result"))))
          (setq success t))
      (dolist (child children)
        (unless (aref child 3)
          (unless success (syscall-direct 62 (aref child 0) 15 0 0 0 0))
          (let ((waited -4))
            (while (= waited -4)
              (setq waited (syscall-direct 61 (aref child 0) status-address 0 0 0 0)))))
        (when (file-exists-p (aref child 1)) (delete-file (aref child 1))))
      (nl-ffi-memory-release status))))

(defun ngit-transport-index--scan (path)
  "Validate PATH and return compact resolved metadata records."
  (let* ((file-size (ngit-native-file-size path))
         (ngit-transport-index--workers
          (ngit-transport-memory-workers ngit-transport-index--workers))
         (ngit-transport-index--cache-bytes
          (ngit-transport-memory-cache-bytes
           ngit-transport-index--workers ngit-transport-index--cache-bytes))
         (ngit-transport-index--cache-object-limit
          (min ngit-transport-index--cache-object-limit
               ngit-transport-index--cache-bytes))
         (head (ngit-native-read-bytes path 0 12)))
    (unless (and (>= file-size 32) (= (length head) 12)
                 (string= (substring head 0 4) "PACK")
                 (= (ngit--be32 head 4) 2))
      (error "fatal: pack is not a version 2 pack file"))
    (let* ((count (ngit--be32 head 8)) (position 12) records
           (prepared (when (and (>= count ngit-transport-index--parallel-threshold)
                                (fboundp 'ngit-transport-kernel-prepare))
                       (unless (ngit-transport-kernel-prepare)
                         (error "fatal: cannot prepare delta kernel: %s"
                                ngit-transport-kernel--error))))
           (parallel (and (>= count ngit-transport-index--parallel-threshold)
                          (> ngit-transport-index--workers 1)
                          (ngit-transport-index--single-thread-p)))
           (ref-deltas nil)
           (profile (equal (getenv "NGIT_PROFILE") "1"))
           (ngit-transport-index--path path)
           (ngit-transport-index--by-offset (make-hash-table :test 'eql))
           (ngit-transport-index--by-oid (make-hash-table :test 'equal))
           (ngit-transport-index--cache (make-hash-table :test 'eql))
           (ngit-transport-index--cache-order nil)
           (ngit-transport-index--cache-tail nil)
           (ngit-transport-index--cached-count 0)
           (ngit-transport-index--cache-size 0)
           (fd (ngit-native--open path))
           (ngit-transport-index--map-size file-size)
           (ngit-transport-index--map-address
            (syscall-direct ngit-transport-index--sys-mmap 0 file-size
                            ngit-transport-index--prot-read
                            ngit-transport-index--map-private fd 0)))
      (when (< ngit-transport-index--map-address 0)
        (error "fatal: cannot map pack (errno %d)"
               (- ngit-transport-index--map-address)))
      (unwind-protect
          (progn
            (dotimes (_ count)
              (when (and profile (= (% _ 10000) 0))
                (ngit-native-write-stderr
                 (format "profile/transport-index: %d/%d\n" _ count)))
              (let* ((off position) (h (ngit-transport-index--header off))
                     (stored (aref h 0)) (size (aref h 1)) (data-at (aref h 2))
                     (direct (memq stored '(commit tree blob tag)))
                     (inflated (ngit-transport-index--inflate
                                data-at size (and direct stored) nil parallel))
                     (delta-output (and (not direct)
                                        (ngit-transport-index--delta-output-size size)))
                     (direct-id ngit-transport-index--inflated-id)
                     (end (+ data-at (cdr inflated))))
                (when (> end (- file-size 20)) (error "fatal: truncated pack entry"))
                (let ((record
                       (vector off end stored size data-at (aref h 3) nil nil
                               (ngit-pack--crc-pointer
                                0 (+ ngit-transport-index--map-address off) (- end off))
                               0 nil nil 0)))
                  (push record records)
                  (cond
                   (direct
                    (aset record 11 record)
                    (aset record 12 (+ size 64)))
                   ((eq stored 'ref-delta)
                    ;; REF deltas disable parallel resolution, but remain a
                    ;; valid root for a following OFS-delta chain.
                    (aset record 11 record)
                    (aset record 12 (+ delta-output (* 8 size))))
                   ((eq stored 'ofs-delta)
                    (unless (and (< (aref h 3) off)
                                 (gethash (aref h 3) ngit-transport-index--by-offset))
                      (error "fatal: invalid OFS delta base"))
                    (let ((base (gethash (aref h 3)
                                         ngit-transport-index--by-offset)))
                      (aset base 10 (cons record (aref base 10)))
                      (aset record 11 (aref base 11))
                      (aset (aref record 11) 12
                            (+ (aref (aref record 11) 12)
                               delta-output (* 8 size))))))
                  (puthash off record ngit-transport-index--by-offset)
                  (when (eq stored 'ref-delta) (setq ref-deltas t))
                  (if direct
                      (progn
                        (aset record 6 stored)
                        (aset record 7 direct-id)
                        (puthash direct-id record ngit-transport-index--by-oid)
                        (when (car inflated)
                          (ngit-transport-index--cache-put off (car inflated))))
                    (unless parallel
                      (ngit-transport-index--resolve record (car inflated)))))
                (setq position end)))
            (setq records (nreverse records))
            (unless (= position (- file-size 20))
              (error "fatal: pack has junk at the end"))
            (let ((expected (ptr-read-bytes
                             (+ ngit-transport-index--map-address position) 20))
                  (actual (ngit-pack--sha1-file path position)))
              (unless (string= expected actual)
                (error "fatal: pack checksum mismatch")))
            ;; Ordered deltas were resolved during scanning.  Repeated passes
            ;; handle the uncommon forward REF_DELTA chains.
            (when (and parallel (not ref-deltas))
              (ngit-transport-index--parallel records path))
            (let ((left count) (progress t))
              (while (and (> left 0) progress)
                (setq progress nil left 0)
                (dolist (record records)
                  (unless (aref record 7)
                    (setq left (1+ left))
                    (when (ngit-transport-index--resolve record)
                      (setq progress t left (1- left))))))
              (when (> left 0)
                (let ((record nil) (tail records))
                  (while (and tail (not record))
                    (unless (aref (car tail) 7) (setq record (car tail)))
                    (setq tail (cdr tail)))
                  (error "fatal: unresolved %s base %s" (aref record 2)
                         (if (stringp (aref record 5))
                             (ngit-hex (aref record 5)) (aref record 5))))))
            records)
        (dolist (offset ngit-transport-index--cache-order)
          (ngit-transport-index--release (gethash offset ngit-transport-index--cache)))
        (syscall-direct ngit-transport-index--sys-munmap
                        ngit-transport-index--map-address file-size 0 0 0 0)))))

(defun ngit-transport-index-pack (path idx-path)
  "Index received pack PATH at IDX-PATH and return (HEX-SHA . nil)."
  (let* ((records (ngit-transport-index--scan path))
         (file-size (ngit-native-file-size path))
         (sum (ngit-native-read-bytes path (- file-size 20) 20))
         index)
    (dolist (record records)
      (push (vector (aref record 7) (aref record 8) (aref record 0)) index))
    (let ((tmp (concat idx-path ".tmp-" (number-to-string (emacs-pid)))))
      (unwind-protect
          (progn
            (ngit-write-file tmp (ngit-pack--idx-bytes index sum))
            (rename-file tmp idx-path t))
        (when (file-exists-p tmp) (delete-file tmp))))
    (cons (ngit-hex sum) nil)))

(provide 'ngit-transport-index)

;;; ngit-transport-index.el ends here
