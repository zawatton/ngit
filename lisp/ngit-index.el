;;; ngit-index.el --- the git index (dircache) -*- lexical-binding: t; -*-

;;; Commentary:

;; Reads index versions 2, 3 and 4 and the TREE (cache-tree) extension.
;; Entries are vectors [NAME BIN MODE STAGE POS FLAGS] where NAME is the
;; unibyte path, BIN the 20-byte object name, MODE an integer such as
;; #o100644, POS the entry offset in the raw index (stat data is decoded
;; lazily from there) and FLAGS a list that may contain `ita' (intent to
;; add) and `skip-worktree'.  Decoding costs ~10 µs per primitive in
;; NeLisp, so only the fields every caller needs are decoded eagerly.

;;; Code:

(require 'ngit-store)

(defun ngit-index-file (repo)
  (let ((env (getenv "GIT_INDEX_FILE")))
    (if (and env (not (string= env "")))
        (expand-file-name env)
      (expand-file-name "index" (ngit-repo-gitdir repo)))))

(defsubst ngit--be16 (s pos)
  (logior (ash (aref s pos) 8) (aref s (1+ pos))))

(defun ngit--index-varint (s pos)
  "Decode git's offset varint in S at POS; return (VALUE . NEXT-POS)."
  (let* ((c (aref s pos)) (val (logand c 127)))
    (while (>= c 128)
      (setq pos (1+ pos) c (aref s pos)
            val (+ (ash (1+ val) 7) (logand c 127))))
    (cons val (1+ pos))))

(defun ngit--index-name (raw pos hdr nlen version prev)
  "Decode the name of the entry at POS with header size HDR.
Return (NAME . NEXT-ENTRY-POS)."
  (if (= version 4)
      (let* ((v (ngit--index-varint raw (+ pos hdr)))
             (nul (ngit--byte-pos raw 0 (cdr v))))
        (cons (concat (substring prev 0 (- (length prev) (car v)))
                      (substring raw (cdr v) nul))
              (1+ nul)))
    (when (= nlen #xfff)
      (setq nlen (- (ngit--byte-pos raw 0 (+ pos hdr)) pos hdr)))
    (cons (substring raw (+ pos hdr) (+ pos hdr nlen))
          (+ pos (logand (+ hdr nlen 8) (lognot 7))))))

(defun ngit--parse-ascii-int (s pos)
  "Parse an optionally negative decimal at POS; return (VALUE . NEXT-POS)."
  (let ((neg (= (aref s pos) ?-)) (val 0))
    (when neg (setq pos (1+ pos)))
    (while (and (>= (aref s pos) ?0) (<= (aref s pos) ?9))
      (setq val (+ (* val 10) (- (aref s pos) ?0)) pos (1+ pos)))
    (cons (if neg (- val) val) pos)))

(defun ngit--parse-cache-tree (s pos end table prefix)
  "Parse one cache-tree node at POS under PREFIX into TABLE; return the
position after it and its subtrees.  TABLE maps \"dir/\" (\"\" for the
root) to (ENTRY-COUNT . BIN-OR-NIL)."
  (let* ((nul (ngit--byte-pos s 0 pos))
         (name (substring s pos nul))
         (path (if (string= prefix "") (if (string= name "") "" (concat name "/"))
                 (concat prefix name "/")))
         (cnt (ngit--parse-ascii-int s (1+ nul)))
         (subs (ngit--parse-ascii-int s (1+ (cdr cnt))))
         (p (1+ (cdr subs))))
    (if (>= (car cnt) 0)
        (progn (puthash path (cons (car cnt) (substring s p (+ p 20))) table)
               (setq p (+ p 20)))
      (puthash path (cons (car cnt) nil) table))
    (dotimes (_ (car subs))
      (when (< p end)
        (setq p (ngit--parse-cache-tree s p end table path))))
    p))

(defun ngit-read-index (repo)
  "Return REPO's index as [ENTRIES RAW VERSION CACHE-TREE MTIME-S MTIME-NS
NATIVE-ADDRESS],
or an empty index when the file does not exist.  Cached in the repo."
  (or (aref repo 6)
      (aset repo 6
            (let ((file (ngit-index-file repo)))
              (if (not (file-exists-p file))
                  (vector [] "" 2 nil 0 0 nil)
                (let* ((size (file-attribute-size (file-attributes file)))
                       ;; The index stays in native memory too: its NUL-padded
                       ;; names serve directly as C paths for fstatat(2).
                       (native (nl-ffi-memory-allocate (max size 1)))
                       (raw (prog1 (progn (ngit-native--pread file native 0 size)
                                          (ptr-read-bytes (nl-ffi-memory-address native) size))
                              (ngit-native-close file)))
                       (mtime (ngit--file-mtime file)))
                  (unless (string= (substring raw 0 4) "DIRC")
                    (error "fatal: index file corrupt"))
                  (let* ((version (ngit--be32 raw 4))
                         (count (ngit--be32 raw 8))
                         (entries (make-vector count nil))
                         (pos 12) (k 0) (prev "") (tree nil)
                         (end (- (length raw) 20)))
                    (unless (memq version '(2 3 4))
                      (error "fatal: index file has unsupported version %d" version))
                    (while (< k count)
                      ;; Inlined decoding: each function call costs
                      ;; microseconds in the NeLisp interpreter.
                      (let* ((flags (logior (ash (aref raw (+ pos 60)) 8) (aref raw (+ pos 61))))
                             (nlen (logand flags #xfff))
                             (name nil) (next nil) (eflags nil))
                        (if (= 0 (logand flags #x4000))
                            (if (and (< nlen #xfff) (/= version 4))
                                (setq name (substring raw (+ pos 62) (+ pos 62 nlen))
                                      next (+ pos (logand (+ 70 nlen) -8)))
                              (let ((r (ngit--index-name raw pos 62 nlen version prev)))
                                (setq name (car r) next (cdr r))))
                          (let ((ef (ngit--be16 raw (+ pos 62))))
                            (setq eflags (delq nil (list (and (/= 0 (logand ef #x2000)) 'ita)
                                                         (and (/= 0 (logand ef #x4000))
                                                              'skip-worktree)))))
                          (let ((r (ngit--index-name raw pos 64 nlen version prev)))
                            (setq name (car r) next (cdr r))))
                        (aset entries k
                              (vector name (substring raw (+ pos 40) (+ pos 60))
                                      (logior (ash (aref raw (+ pos 26)) 8) (aref raw (+ pos 27)))
                                      (logand (ash flags -12) 3)
                                      pos eflags))
                        (setq prev name pos next k (1+ k))))
                    (while (< (+ pos 8) end)
                      (let ((sig (substring raw pos (+ pos 4)))
                            (len (ngit--be32 raw (+ pos 4))))
                        ;; Parsed on demand by `ngit-index-cache-tree'.
                        (when (string= sig "TREE")
                          (setq tree (cons (+ pos 8) len)))
                        (setq pos (+ pos 8 len))))
                    (vector entries raw version tree (car mtime) (cdr mtime)
                            (nl-ffi-memory-address native)))))))))

(defun ngit-index-entries (repo) (aref (ngit-read-index repo) 0))

(defun ngit-index-cache-tree (index)
  "Return INDEX's cache-tree table (see `ngit--parse-cache-tree'), or nil."
  (let ((tree (aref index 3)))
    (if (consp tree)
        (aset index 3
              (let ((table (make-hash-table :test 'equal)))
                (when (> (cdr tree) 0)
                  (ngit--parse-cache-tree (aref index 1) (car tree)
                                          (+ (car tree) (cdr tree)) table ""))
                table))
      tree)))

(defun ngit-index-stat (index entry)
  "Return the cached stat data of ENTRY as [CTIME MTIME MTIME-NS INO SIZE]."
  (let ((raw (aref index 1)) (pos (aref entry 4)))
    (vector (ngit--be32 raw pos) (ngit--be32 raw (+ pos 8)) (ngit--be32 raw (+ pos 12))
            (ngit--be32 raw (+ pos 20)) (ngit--be32 raw (+ pos 36)))))

(defun ngit-index-find (entries name)
  "Return the position of the first entry >= NAME in sorted ENTRIES."
  (let ((lo 0) (hi (length entries)))
    (while (< lo hi)
      (let ((mid (ash (+ lo hi) -1)))
        (if (string< (aref (aref entries mid) 0) name)
            (setq lo (1+ mid))
          (setq hi mid))))
    lo))

(defun ngit-index-lookup (entries name)
  "Return the stage-0 (or first) entry named NAME, or nil."
  (let ((i (ngit-index-find entries name)))
    (and (< i (length entries))
         (string= (aref (aref entries i) 0) name)
         (aref entries i))))

(defun ngit-index-dir-p (entries dir)
  "Non-nil when some entry lies under DIR (which ends in \"/\")."
  (let ((i (ngit-index-find entries dir)))
    (and (< i (length entries))
         (string-prefix-p dir (aref (aref entries i) 0)))))

(defun ngit--file-mtime (file)
  (let ((m (file-attribute-modification-time (file-attributes file))))
    ;; (HIGH LOW USEC PSEC) or (TICKS . HZ); reduce to seconds and nanoseconds.
    (cond ((and (consp m) (integerp (cdr m)) (not (listp (cdr m))))
           (let ((ticks (car m)) (hz (cdr m)))
             (cons (/ ticks hz) (/ (* (mod ticks hz) 1000000000) hz))))
          ((consp m)
           (cons (+ (ash (nth 0 m) 16) (nth 1 m))
                 (+ (* 1000 (or (nth 2 m) 0)) (/ (or (nth 3 m) 0) 1000))))
          (t (cons (truncate (float-time m)) 0)))))

(provide 'ngit-index)

;;; ngit-index.el ends here
