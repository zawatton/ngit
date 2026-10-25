;;; ngit-binary-patch.el --- Git binary patch bodies -*- lexical-binding: t; -*-

;;; Commentary:
;; format-patch needs binary bodies when an attribute selector includes -diff.
;; Literal bodies are exact for additions/deletions.  Delta selection for
;; binary modifications is not implemented.
;; Input accepts literal and delta bodies and verifies both object names.

;;; Code:

(defconst ngit-binary-patch--alphabet
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&()*+-;<=>?@^_`{|}~")

(defconst ngit-binary-patch--decode
  (let ((table (make-vector 256 -1)) (i 0))
    (while (< i 85)
      (aset table (aref ngit-binary-patch--alphabet i) i)
      (setq i (1+ i)))
    table))

(defun ngit-binary-patch--corrupt (lines position)
  (ngit-native-write-stderr
   (format "error: corrupt binary patch at line %d: %s\n" (1+ position)
           (if (< position (length lines))
               (ngit-patch--strip-line (aref lines position)) "")))
  (ngit--die-with 128 "error: No valid patches in input (allow with \"--allow-empty\")"))

(defun ngit-binary-patch--decode-line (lines position)
  "Decode one length-prefixed base85 line in four-byte words."
  (let* ((line (aref lines position)) (size (length line))
         (first (aref line 0))
         (count (cond ((and (<= ?A first) (<= first ?Z)) (1+ (- first ?A)))
                      ((and (<= ?a first) (<= first ?z)) (+ 27 (- first ?a)))))
         (maximum (* 4 (/ (- size 2) 5)))
         (offset 1) (parts nil) (table ngit-binary-patch--decode))
    (unless (and (>= size 7) (= (aref line (1- size)) ?\n)
                 (= (% (- size 2) 5) 0) count
                 (<= count maximum) (> count (- maximum 4)))
      (ngit-binary-patch--corrupt lines position))
    (while (< offset (- size 1))
      (let* ((a (aref table (aref line offset)))
             (b (aref table (aref line (+ offset 1))))
             (c (aref table (aref line (+ offset 2))))
             (d (aref table (aref line (+ offset 3))))
             (e (aref table (aref line (+ offset 4))))
             (word (+ (* (+ (* (+ (* (+ (* a 85) b) 85) c) 85) d) 85) e)))
        (when (< (min a b c d e) 0)
          (ngit-native-write-stderr
           (format "error: invalid base85 alphabet %c\n"
                   (aref line (+ offset (cond ((< a 0) 0) ((< b 0) 1)
                                             ((< c 0) 2) ((< d 0) 3) (t 4))))))
          (ngit-binary-patch--corrupt lines position))
        (when (> word #xffffffff)
          (ngit-native-write-stderr
           (format "error: invalid base85 sequence %s\n" (substring line offset (+ offset 5))))
          (ngit-binary-patch--corrupt lines position))
        (push (ngit--be32-bytes word) parts))
      (setq offset (+ offset 5)))
    (substring (apply #'concat (nreverse parts)) 0 count)))

(defun ngit-binary-patch--hunk (lines position)
  "Parse a binary hunk as (METHOD DATA NEXT-LINE), or nil if absent."
  (when (< position (length lines))
    (let* ((header (aref lines position))
           (method (cond ((string-prefix-p "literal " header) 'literal)
                         ((string-prefix-p "delta " header) 'delta))))
      (when method
        (let ((size (string-to-number (substring header (if (eq method 'literal) 8 6))))
              (parts nil) inflated)
          (setq position (1+ position))
          (while (and (< position (length lines)) (not (equal (aref lines position) "\n")))
            (push (ngit-binary-patch--decode-line lines position) parts)
            (setq position (1+ position)))
          (unless (< position (length lines))
            (ngit-binary-patch--corrupt lines position))
          (let* ((data (apply #'concat (nreverse parts)))
                 (buffer (ngit-native--buffer 0 (length data))))
            (ptr-write-bytes (nl-ffi-memory-address buffer) data)
            (setq inflated (ngit-native--inflate-once buffer (length data) (max 1 size))))
          (unless (and (= (car inflated) 0) (= (length (cadr inflated)) size))
            (ngit-binary-patch--corrupt lines position))
          (list method (cadr inflated) (1+ position)))))))

(defun ngit-binary-patch-parse (lines position)
  "Return ((FORWARD REVERSE) . NEXT-LINE), validating both binary bodies."
  (let ((forward (ngit-binary-patch--hunk lines position)) reverse)
    (unless forward
      (ngit-native-write-stderr (format "error: unrecognized binary patch at line %d\n" position))
      (ngit--die-with 128 "error: No valid patches in input (allow with \"--allow-empty\")"))
    (setq reverse (ngit-binary-patch--hunk lines (nth 2 forward)))
    (cons (list forward reverse) (nth 2 (or reverse forward)))))

(defun ngit-binary-patch--fail (message &rest args)
  (ngit-native-write-stderr (concat "error: " (apply #'format message args) "\n"))
  (throw 'ngit-binary-patch-invalid nil))

(defun ngit-binary-patch--delta-size (delta position)
  "Read a delta size, stopping at the input end like get_delta_hdr_size."
  (let ((value 0) (shift 0) (more t))
    (while (and more (< position (length delta)))
      (let* ((byte (aref delta position)) (digit (logand byte 127))
             (effective (% shift 64)))
        (when (and (= shift 63) (> digit 1))
          (ngit--die "fatal: size_t overflow: %d << %d" digit shift))
        ;; Keep the native 64-bit shift/result semantics for extended headers
        ;; without relying on arbitrary-precision bitwise operations.
        (when (> effective 57)
          (setq digit (logand digit (1- (ash 1 (- 64 effective))))))
        (setq value (logior value (ash digit effective))
              more (/= (logand byte 128) 0)
              shift (+ shift 7) position (1+ position))))
    (cons value position)))

(defun ngit-binary-patch--delta-bad (name &optional opcode)
  (ngit-native-write-stderr
   (if opcode "error: unexpected delta opcode 0\n" "error: delta replay has gone wild\n"))
  (ngit-binary-patch--fail "binary patch does not apply to '%s'" name))

(defun ngit-binary-patch--delta (source delta name)
  "Apply DELTA with patch-delta.c's bounds checks and diagnostic order."
  (when (< (length delta) 4)
    (ngit-binary-patch--fail "binary patch does not apply to '%s'" name))
  (let* ((base (ngit-binary-patch--delta-size delta 0))
         (source-size (length source)))
    ;; Check the base before reading the result size; a mismatched base is
    ;; a silent rejection, even if the remaining header is truncated.
    (unless (= (car base) source-size)
      (ngit-binary-patch--fail "binary patch does not apply to '%s'" name))
    (let* ((result (ngit-binary-patch--delta-size delta (cdr base)))
           (remaining (car result)) (position (cdr result))
           (size (length delta)) (parts nil))
      (while (< position size)
        (let ((command (aref delta position)))
          (setq position (1+ position))
          (cond
           ((/= (logand command 128) 0)
            (let ((offset 0) (count 0) (shift 0))
              (dolist (bit '(1 2 4 8))
                (when (/= (logand command bit) 0)
                  (when (>= position size) (ngit-binary-patch--delta-bad name))
                  (setq offset (logior offset (ash (aref delta position) shift))
                        position (1+ position)))
                (setq shift (+ shift 8)))
              (setq shift 0)
              (dolist (bit '(16 32 64))
                (when (/= (logand command bit) 0)
                  (when (>= position size) (ngit-binary-patch--delta-bad name))
                  (setq count (logior count (ash (aref delta position) shift))
                        position (1+ position)))
                (setq shift (+ shift 8)))
              (when (= count 0) (setq count #x10000))
              (when (or (> (+ offset count) source-size) (> count remaining))
                (ngit-binary-patch--delta-bad name))
              (push (substring source offset (+ offset count)) parts)
              (setq remaining (- remaining count))))
           ((> command 0)
            (when (or (> command remaining) (> (+ position command) size))
              (ngit-binary-patch--delta-bad name))
            (push (substring delta position (+ position command)) parts)
            (setq position (+ position command) remaining (- remaining command)))
           (t (ngit-binary-patch--delta-bad name t)))))
      (unless (= remaining 0) (ngit-binary-patch--delta-bad name))
      (apply #'concat (nreverse parts)))))

(defun ngit-binary-patch-apply (repo source patch reverse)
  "Verify PATCH's preimage and return its binary result, or nil on failure."
  (catch 'ngit-binary-patch-invalid
    (let* ((name (or (plist-get patch :old) (plist-get patch :new)))
           (old (plist-get patch (if reverse :new-oid :old-oid)))
           (new (plist-get patch (if reverse :old-oid :new-oid)))
           (old-path (plist-get patch (if reverse :new :old)))
           result)
      (unless (and old new (string-match-p "\\`[0-9a-fA-F]\\{40\\}\\'" old)
                   (string-match-p "\\`[0-9a-fA-F]\\{40\\}\\'" new))
        (ngit-binary-patch--fail "cannot apply binary patch to '%s' without full index line" name))
      (when (and old-path (not (equal old (ngit-hex (ngit-hash-blob source)))))
        (ngit-binary-patch--fail
         "the patch applies to '%s' (%s), which does not match the current contents."
         name (ngit-hex (ngit-hash-blob source))))
      (if (equal new (make-string 40 ?0)) (setq result "")
        (if (ngit-object-exists-p repo (ngit-unhex new))
            ;; Git prefers an available postimage, after validating the body
            ;; framing and old OID, without executing its binary delta.
            (setq result (cdr (ngit-read-object repo (ngit-unhex new))))
          (let ((fragment (nth (if reverse 1 0) (plist-get patch :binary))))
            (unless fragment
              (ngit-native-write-stderr
               (format (if (and reverse (car (plist-get patch :binary)))
                           "error: cannot reverse-apply a binary patch without the reverse hunk to '%s'\n"
                         "error: missing binary patch data for '%s'\n") name))
              (ngit-binary-patch--fail "binary patch does not apply to '%s'" name))
            (setq result
                  (if (eq (car fragment) 'literal) (nth 1 fragment)
                    (ngit-binary-patch--delta source (nth 1 fragment) name)))
            (unless (equal new (ngit-hex (ngit-hash-blob result)))
              (ngit-binary-patch--fail
               "binary patch to '%s' creates incorrect result (expecting %s, got %s)"
               name new (ngit-hex (ngit-hash-blob result)))))))
      (cons result nil))))

(defun ngit-binary-patch--deflate (data level)
  "Compress DATA at Git's configured zlib LEVEL (default one)."
  (let* ((size (length data)) (capacity (+ size (/ size 8) 64))
         (source (nl-ffi-memory-address (ngit-native--buffer 0 size)))
         (destination (nl-ffi-memory-address (ngit-native--buffer 1 capacity)))
         (length-address (nl-ffi-memory-address (ngit-native--buffer 2 8))))
    (ptr-write-bytes source data)
    (ptr-write-u64 length-address 0 capacity)
    (unless (= 0 (ngit-native--zlib-call "compress2" #'ngit-native--compress2
                                        destination length-address source size level))
      (error "ngit: binary patch compression failed"))
    (ptr-read-bytes destination (ptr-read-u64 length-address 0))))

(defun ngit-binary-patch--literal (data level)
  "Emit the literal half of a Git binary patch for DATA."
  (ngit--emit "literal " (number-to-string (length data)) "\n")
  (let* ((compressed (ngit-binary-patch--deflate data level))
         (size (length compressed)) (start 0))
    (while (< start size)
      (let* ((count (min 52 (- size start)))
             (end (+ start count))
             (padded (concat (substring compressed start end) (make-string 3 0)))
             (line (make-string (1+ (* 5 (/ (+ count 3) 4))) 0))
             (position 0) (out 1))
        (aset line 0 (if (<= count 26) (+ ?A count -1) (+ ?a count -27)))
        ;; Each iteration encodes a four-byte word, without per-byte scans.
        (while (< position count)
          (let* ((word (ngit--be32 padded position))
                 (a (/ word 85)) (b (/ a 85)) (c (/ b 85)) (d (/ c 85)))
            (aset line out (aref ngit-binary-patch--alphabet d))
            (aset line (+ out 1) (aref ngit-binary-patch--alphabet (% c 85)))
            (aset line (+ out 2) (aref ngit-binary-patch--alphabet (% b 85)))
            (aset line (+ out 3) (aref ngit-binary-patch--alphabet (% a 85)))
            (aset line (+ out 4) (aref ngit-binary-patch--alphabet (% word 85))))
          (setq position (+ position 4) out (+ out 5)))
        (ngit--emit line "\n")
        (setq start end))))
  (ngit--emit "\n"))

(defun ngit-binary-patch-emit (old new level)
  "Emit forward and reverse literal bodies for OLD and NEW."
  (ngit--emit "GIT binary patch\n")
  (ngit-binary-patch--literal new level)
  (ngit-binary-patch--literal old level))

(provide 'ngit-binary-patch)
;;; ngit-binary-patch.el ends here
