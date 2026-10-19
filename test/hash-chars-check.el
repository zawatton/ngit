;;; hash-chars-check.el --- span hash equivalence and timing -*- lexical-binding: t; -*-

;; Run: nelisp --load test/hash-chars-check.el -- x
;; NGIT_HASH_CHARS_BENCH=reference or new runs one isolated 1 MiB timing.

(add-to-list 'load-path
             (expand-file-name "../lisp" (file-name-directory load-file-name)))
(require 'ngit-rename)

;; Keep the original implementation unchanged except for its name.
(defun ngit-test--hash-chars-reference (data is-text)
  "diffcore-delta's hash_chars: [HASHES COUNTS] sorted by hash value."
  ;; One flat `setq' per byte and no per-byte `let': the interpreter's
  ;; cost is per form, and this loop runs over every byte of the blob.
  (let ((tbl (make-hash-table :test 'eql))
        (keys nil)
        (i 0) (n (length data))
        (a1 0) (a2 0) (cnt 0) (c 0) (o1 0) (h 0) (old nil)
        (cr (and is-text (string-search "\r" data) t)))
    (while (< i n)
      (setq c (aref data i) i (1+ i))
      (unless (and cr (= c 13) (< i n) (= (aref data i) 10))
        (setq o1 a1
              a1 (logand (+ (logxor (logand (ash a1 7) 4294967295) (ash a2 -25)) c) 4294967295)
              a2 (logand (logxor (ash a2 7) (ash o1 -25)) 4294967295)
              cnt (1+ cnt))
        (when (or (= cnt 64) (= c 10))
          (setq h (% (logand (+ a1 (* a2 97)) 4294967295) ngit--hashbase)
                old (gethash h tbl))
          (if old
              (puthash h (+ old cnt) tbl)
            (puthash h cnt tbl)
            (push h keys))
          (setq cnt 0 a1 0 a2 0))))
    (when (> cnt 0)
      (let* ((h (% (logand (+ a1 (* a2 97)) #xffffffff) ngit--hashbase))
             (old (gethash h tbl)))
        (if old (puthash h (+ old cnt) tbl) (puthash h cnt tbl) (push h keys))))
    (let* ((sorted (vconcat (sort keys #'<)))
           (counts (make-vector (length sorted) 0))
           (k 0))
      (while (< k (length sorted))
        (aset counts k (gethash (aref sorted k) tbl))
        (setq k (1+ k)))
      (vector sorted counts))))

(defun ngit-test--random-bytes (size)
  "Return SIZE deterministic pseudorandom bytes."
  (let ((chunks nil) (seed "ngit hash_chars check") (n 0))
    (while (< n size)
      (setq seed (secure-hash 'sha256 seed nil nil t)
            chunks (cons seed chunks)
            n (+ n 32)))
    (substring (apply #'concat chunks) 0 size)))

(defun ngit-test--text (crlf)
  "Return varied text lines, optionally using CRLF."
  (let ((chunks nil) (i 0) (seed "ngit text") (eol (if crlf "\r\n" "\n")))
    (while (< i 2048)
      (setq seed (secure-hash 'sha256 seed)
            chunks (cons (concat (substring seed 0 (% i 65)) eol) chunks)
            i (1+ i)))
    (apply #'concat chunks)))

(defun ngit-test--assert-equal (expected actual label)
  "Fail unless EXPECTED and ACTUAL match."
  (unless (equal expected actual)
    (error "hash_chars mismatch: %s" label)))

(defvar ngit-test--checked 0)

(defun ngit-test--check (data is-text label)
  "Compare DATA against the saved implementation."
  (ngit-test--assert-equal (ngit-test--hash-chars-reference data is-text)
                          (ngit--hash-chars data is-text) label)
  (setq ngit-test--checked (1+ ngit-test--checked)))

(defun ngit-test--bench (mode)
  "Time MODE on one MiB of pseudorandom binary data."
  (let* ((data (ngit-test--random-bytes 1048576))
         (fn (if (equal mode "reference")
                 #'ngit-test--hash-chars-reference #'ngit--hash-chars))
         (start 0) (elapsed 0))
    ;; Warm shared tables separately and report the 64 KiB warmup cost.
    (setq start (float-time))
    (funcall fn (substring data 0 65536) nil)
    (princ (format "%s setup: %.3f s\n" mode (- (float-time) start)))
    (garbage-collect)
    (setq start (float-time))
    (funcall fn data nil)
    (setq elapsed (- (float-time) start))
    (princ (format "%s: %.3f us/byte (1048576 bytes, %.3f s)\n"
                   mode (/ (* elapsed 1000000) 1048576) elapsed))))

(defun ngit-test--run ()
  "Check boundaries, random inputs and the checker's failure signal."
  (let ((caught nil) (data (ngit-test--random-bytes 307200)) (i 0))
    ;; A broken result must fail the same assertion used by every case.
    (condition-case nil
        (ngit-test--assert-equal (vector [] []) (vector [0] [1]) "negative control")
      (error (setq caught t)))
    (unless caught (error "hash_chars negative control did not fail"))
    (while (<= i 512)
      (ngit-test--check (substring data 0 i) nil (format "binary length %d" i))
      (setq i (1+ i)))
    (dolist (n '(1023 4096 16384 65536 153600 307200))
      (ngit-test--check (substring data 0 n) nil (format "binary length %d" n))
      (ngit-test--check (substring data 0 n) t (format "binary as text %d" n)))
    (dolist (n '(0 1 2 3 4 63 64 65 127 128 129 4096))
      (ngit-test--check (make-string n 10) nil (format "all LF %d" n))
      (ngit-test--check (make-string n ?a) nil (format "no LF %d" n))
      (ngit-test--check (make-string n 255) nil (format "all FF %d" n)))
    (setq i 0)
    (while (< i 129)
      (let ((s (concat (make-string i ?x) "\n" (make-string (- 128 i) ?y)))
            (crlf (concat (make-string i ?x) "\r\n" (make-string (- 128 i) ?y))))
        (ngit-test--check s nil (format "LF at %d" i))
        (ngit-test--check crlf t (format "CRLF at %d" i))
        (ngit-test--check crlf nil (format "binary CRLF at %d" i)))
      (setq i (1+ i)))
    (dolist (s '("" "\r" "\r\r\n" "\r\n\r\n" "a\rb\r" "\n\r"))
      (ngit-test--check s t "CR boundary"))
    ;; Reachable carries, including (a1 & 2047) = 2045 at a four-byte
    ;; boundary, where discarding an intermediate overflow matters.
    (dolist (bytes '((0 0 1 126 1 126 0 0 0 0 15 125 255 255 1 0)
                     (0 0 0 15 255 255 255 255 0 0 0 0)
                     (15 127 127 127 128)))
      (ngit-test--check (apply #'unibyte-string bytes) nil "discarded carry"))
    (ngit-test--check (ngit-test--text nil) t "random LF text")
    (ngit-test--check (ngit-test--text t) t "random CRLF text")
    ;; High bytes exercise carry propagation without an intervening LF.
    (let ((s (unibyte-string 255 254 253 252 251 250 249 248 247)))
      (setq i 0)
      (while (< i 10) (setq s (concat s s) i (1+ i)))
      (ngit-test--check s nil "high-byte carries"))
    (princ (format "PASS: %d cases; negative control PASS\n" ngit-test--checked))))

(let ((mode (or (and (member "hash-chars-bench-reference" command-line-args) "reference")
                (and (member "hash-chars-bench-new" command-line-args) "new")
                (getenv "NGIT_HASH_CHARS_BENCH"))))
  (if mode
      (ngit-test--bench mode)
    (ngit-test--run)
    ;; Fresh processes avoid arena growth bias between the two timings.
    (unless (getenv "NGIT_HASH_CHARS_CHECK_ONLY")
      (dolist (variant '("reference" "new"))
        (with-temp-buffer
          (unless (= (call-process "nelisp" nil t nil "--load" load-file-name "--"
                                   (concat "hash-chars-bench-" variant)) 0)
            (error "%s benchmark failed: %s" variant (buffer-string)))
          (princ (buffer-string)))))))

(kill-emacs 0)

;;; hash-chars-check.el ends here
