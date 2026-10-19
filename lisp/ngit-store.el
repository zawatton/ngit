;;; ngit-store.el --- repository discovery and object store -*- lexical-binding: t; -*-

;;; Commentary:

;; Read-only access to a git repository: locating the git directory
;; (including linked worktrees), loose objects, pack index v2 lookup,
;; pack entries with OFS_DELTA/REF_DELTA resolution, and parsing of
;; commit/tree/tag objects.  Object names are passed around as 20-byte
;; unibyte strings ("bin") internally and 40-char hex at the edges.

;;; Code:

(require 'ngit-native)

;;;; Byte helpers

(defconst ngit--hex-table
  (let ((v (make-vector 256 nil)) (i 0))
    (while (< i 256)
      (aset v i (string (aref "0123456789abcdef" (ash i -4))
                        (aref "0123456789abcdef" (logand i 15))))
      (setq i (1+ i)))
    v))

(defun ngit-hex (bin)
  "Return the lowercase hex form of unibyte string BIN.
Per-call overhead dominates in NeLisp, so 20-byte names are unrolled."
  (let ((tb ngit--hex-table))
    (if (= (length bin) 20)
        (concat (aref tb (aref bin 0)) (aref tb (aref bin 1)) (aref tb (aref bin 2))
                (aref tb (aref bin 3)) (aref tb (aref bin 4)) (aref tb (aref bin 5))
                (aref tb (aref bin 6)) (aref tb (aref bin 7)) (aref tb (aref bin 8))
                (aref tb (aref bin 9)) (aref tb (aref bin 10)) (aref tb (aref bin 11))
                (aref tb (aref bin 12)) (aref tb (aref bin 13)) (aref tb (aref bin 14))
                (aref tb (aref bin 15)) (aref tb (aref bin 16)) (aref tb (aref bin 17))
                (aref tb (aref bin 18)) (aref tb (aref bin 19)))
      (mapconcat (lambda (b) (aref tb b)) bin ""))))

(defun ngit--byte-pos (s byte start)
  "Return the first index >= START of BYTE in S, or nil.
An `aref' loop: `string-search' with START costs time proportional to
the whole string in NeLisp."
  (let ((n (length s)) (i start))
    (while (and (< i n) (/= (aref s i) byte))
      (setq i (1+ i)))
    (and (< i n) i)))

(defconst ngit--nibbles
  (let ((v (make-vector 128 0)) (i 0))
    (while (< i 10) (aset v (+ ?0 i) i) (setq i (1+ i)))
    (setq i 0)
    (while (< i 6) (aset v (+ ?a i) (+ 10 i)) (aset v (+ ?A i) (+ 10 i)) (setq i (1+ i)))
    v)
  "Value of each hex digit character.")

(defmacro ngit--unhex-40 (hex tb)
  "Unrolled conversion of the 40-character HEX using nibble table TB."
  `(unibyte-string
    ,@(let (forms (i 0))
        (while (< i 40)
          (push `(+ (ash (aref ,tb (aref ,hex ,i)) 4) (aref ,tb (aref ,hex ,(1+ i)))) forms)
          (setq i (+ i 2)))
        (nreverse forms))))

(defun ngit-unhex (hex)
  "Return the unibyte string for the even-length HEX string.
Per-call overhead dominates in NeLisp, so 40-digit names are unrolled."
  (let ((tb ngit--nibbles))
    (if (= (length hex) 40)
        (ngit--unhex-40 hex tb)
      (let ((bytes nil) (i 0) (n (length hex)))
        (while (< i n)
          (push (+ (ash (aref tb (aref hex i)) 4) (aref tb (aref hex (1+ i)))) bytes)
          (setq i (+ i 2)))
        (apply #'unibyte-string (nreverse bytes))))))

(defun ngit-hex-p (s &optional len)
  "Non-nil when S is a lowercase/uppercase hex string (of LEN chars)."
  (and (stringp s)
       (or (null len) (= (length s) len))
       (> (length s) 0)
       (let ((case-fold-search nil))
         (string-match-p "\\`[0-9a-fA-F]+\\'" s))))

(defun ngit--be32 (s pos)
  (logior (ash (aref s pos) 24) (ash (aref s (+ pos 1)) 16)
          (ash (aref s (+ pos 2)) 8) (aref s (+ pos 3))))

(defun ngit--read-file (path)
  "Return the contents of PATH as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun ngit--trim-eol (s)
  (if (string-match "[\r\n]+\\'" s) (substring s 0 (match-beginning 0)) s))

;;;; Repository

;; A repo is a vector [GITDIR COMMONDIR TOPLEVEL PACKS PACKED-REFS - INDEX
;; ATTR-CACHE].

(defun ngit-repo-gitdir (repo) (aref repo 0))
(defun ngit-repo-commondir (repo) (aref repo 1))
(defun ngit-repo-toplevel (repo) (aref repo 2))

(defun ngit--gitdir-from-file (file dir)
  (let ((line (ngit--trim-eol (ngit--read-file file))))
    (unless (string-prefix-p "gitdir: " line)
      (error "ngit: invalid gitfile format: %s" file))
    (file-name-as-directory (expand-file-name (substring line 8) dir))))

(defun ngit--make-repo (gitdir toplevel)
  (let* ((cfile (expand-file-name "commondir" gitdir))
         (common (if (file-exists-p cfile)
                     (file-name-as-directory
                      (expand-file-name (ngit--trim-eol (ngit--read-file cfile))
                                        gitdir))
                   gitdir)))
    (vector gitdir common toplevel nil nil nil nil nil)))

(defun ngit-discover (&optional dir)
  "Return the repository containing DIR (default `default-directory').
Honors GIT_DIR.  Signals an error outside a repository."
  (let ((env (getenv "GIT_DIR")))
    (if (and env (not (string= env "")))
        (ngit--make-repo (file-name-as-directory (expand-file-name env)) nil)
      (let ((d (file-name-as-directory (expand-file-name (or dir default-directory))))
            (found nil))
        (while (and d (not found))
          (let ((dotgit (expand-file-name ".git" d)))
            (cond
             ((file-directory-p dotgit)
              (setq found (ngit--make-repo (file-name-as-directory dotgit) d)))
             ((file-regular-p dotgit)
              (setq found (ngit--make-repo (ngit--gitdir-from-file dotgit d) d)))
             (t
              (let ((parent (file-name-directory (directory-file-name d))))
                (setq d (and parent (not (string= parent d)) parent)))))))
        (or found
            (error "fatal: not a git repository (or any of the parent directories): .git"))))))

;;;; Pack index

;; A pack is a vector [PACK-PATH IDX-PATH COUNT FANOUT-BYTES SLICE-CACHE
;; ENTRY-CACHE]; FANOUT-BYTES is the raw 1032-byte index header.

(defun ngit--open-pack (idx-path)
  (let ((head (ngit-native-read-bytes idx-path 0 1032)))
    (unless (and (>= (length head) 1032)
                 (equal (substring head 0 8) (unibyte-string 255 116 79 99 0 0 0 2)))
      (error "ngit: unsupported pack index (only v2): %s" idx-path))
    (vector (concat (substring idx-path 0 -4) ".pack") idx-path
            (ngit--be32 head (+ 8 (* 4 255))) head
            (make-hash-table :test 'eql) (make-hash-table :test 'eql) nil)))

(defun ngit-repo-packs (repo)
  "Return the list of packs of REPO, loading the indexes on first use."
  (or (aref repo 3)
      (aset repo 3
            (let ((dir (expand-file-name "objects/pack" (ngit-repo-commondir repo))))
              (and (file-directory-p dir)
                   (mapcar #'ngit--open-pack
                           (sort (directory-files dir t "\\.idx\\'") #'string<)))))))

(defun ngit--pack-range (pack byte)
  "Return (LO . HI) of index positions whose name starts with BYTE."
  (let ((head (aref pack 3)))
    (cons (if (= byte 0) 0 (ngit--be32 head (+ 8 (* 4 (1- byte)))))
          (ngit--be32 head (+ 8 (* 4 byte))))))

(defun ngit--pack-slice (pack byte)
  "Return the name table bytes for first byte BYTE of PACK (cached)."
  (let ((cache (aref pack 4)))
    (or (gethash byte cache)
        (puthash byte
                 (let ((r (ngit--pack-range pack byte)))
                   (ngit-native-read-bytes (aref pack 1) (+ 1032 (* 20 (car r)))
                                           (* 20 (- (cdr r) (car r)))))
                 cache))))

(defun ngit--slice-lower-bound (slice key)
  "First position in SLICE whose name prefix is not less than KEY.
Names in a slice share their first byte, so the second byte usually
decides a probe without building a substring."
  (let ((lo 0) (hi (/ (length slice) 20)) (k (length key)))
    (if (< k 2)
        (while (< lo hi)
          (let ((mid (/ (+ lo hi) 2)))
            (if (string< (substring slice (* 20 mid) (+ (* 20 mid) k)) key)
                (setq lo (1+ mid))
              (setq hi mid))))
      (let ((k1 (aref key 1)))
        (while (< lo hi)
          (let* ((mid (ash (+ lo hi) -1))
                 (at (* 20 mid))
                 (b (aref slice (1+ at))))
            (if (or (< b k1)
                    (and (= b k1) (string< (substring slice at (+ at k)) key)))
                (setq lo (1+ mid))
              (setq hi mid))))))
    lo))

(defun ngit--pack-find (pack bin)
  "Return the index position of BIN in PACK, or nil."
  (let* ((slice (ngit--pack-slice pack (aref bin 0)))
         (pos (ngit--slice-lower-bound slice bin))
         (at (* 20 pos)))
    (and (< at (length slice))
         (string= (substring slice at (+ 20 at)) bin)
         (+ pos (car (ngit--pack-range pack (aref bin 0)))))))

(defun ngit--pack-offset (pack index)
  "Offset of the INDEXth entry of PACK; the 4-byte table is read once."
  (let* ((n (aref pack 2))
         (table (or (aref pack 6)
                    (aset pack 6 (ngit-native-read-bytes (aref pack 1) (+ 1032 (* 24 n))
                                                         (* 4 n)))))
         (off (ngit--be32 table (* 4 index))))
    (if (zerop (logand off #x80000000))
        off
      (let ((s (ngit-native-read-bytes
                (aref pack 1) (+ 1032 (* 28 n) (* 8 (logand off #x7fffffff))) 8)))
        (logior (ash (ngit--be32 s 0) 32) (ngit--be32 s 4))))))

;;;; Pack entries

(defconst ngit--type-names [nil commit tree blob tag nil ofs-delta ref-delta])

(defun ngit--delta-varint (d pos)
  (let ((val 0) (shift 0) (c 128))
    (while (/= 0 (logand c 128))
      (setq c (aref d pos)
            val (logior val (ash (logand c 127) shift))
            shift (+ shift 7)
            pos (1+ pos)))
    (cons val pos)))

(defun ngit--apply-delta (base delta)
  "Return BASE with git binary DELTA applied."
  (let* ((r1 (ngit--delta-varint delta 0))
         (r2 (ngit--delta-varint delta (cdr r1)))
         (dst-size (car r2))
         (p (cdr r2))
         (n (length delta))
         (pieces nil))
    (unless (= (car r1) (length base))
      (error "ngit: delta base size mismatch"))
    (while (< p n)
      (let ((cmd (aref delta p)))
        (setq p (1+ p))
        (cond
         ((/= 0 (logand cmd 128))
          (let ((off 0) (size 0) (bit 0))
            (while (< bit 4)
              (when (/= 0 (logand cmd (ash 1 bit)))
                (setq off (logior off (ash (aref delta p) (* 8 bit))) p (1+ p)))
              (setq bit (1+ bit)))
            (while (< bit 7)
              (when (/= 0 (logand cmd (ash 1 bit)))
                (setq size (logior size (ash (aref delta p) (* 8 (- bit 4)))) p (1+ p)))
              (setq bit (1+ bit)))
            (when (= size 0) (setq size #x10000))
            (push (substring base off (+ off size)) pieces)))
         ((> cmd 0)
          (push (substring delta p (+ p cmd)) pieces)
          (setq p (+ p cmd)))
         (t (error "ngit: invalid delta opcode 0")))))
    (let ((out (apply #'concat (nreverse pieces))))
      (unless (= (length out) dst-size)
        (error "ngit: delta result size mismatch"))
      out)))

(declare-function ngit-read-object "ngit-store")

(defconst ngit--pack-head-size 4096
  "Bytes read at an entry's offset; most commits and trees fit.")

(defun ngit--pack-inflate (path offset skip size head-len)
  "Inflate the SIZE-byte zlib stream at OFFSET+SKIP of PATH.
HEAD-LEN bytes from OFFSET are already in the read buffer; use them when
they hold the whole stream, otherwise read the file again."
  (or (and (or (< head-len ngit--pack-head-size)
               (<= (+ skip size (/ size 8) 64) head-len))
           (ngit-native-inflate-head skip (- head-len skip) size))
      (car (ngit-native-inflate-file path (+ offset skip) size))))

(defun ngit--pack-entry (repo pack offset)
  "Return (TYPE . DATA) for the entry at OFFSET in PACK."
  (let ((cache (aref pack 5)))
    (or (gethash offset cache)
        (let* ((path (aref pack 0))
               (got (ngit-native-read-head path offset ngit--pack-head-size))
               (h (ngit-native-head-bytes (min got 32)))
               (c (aref h 0))
               (type (aref ngit--type-names (logand (ash c -4) 7)))
               (size (logand c 15))
               (shift 4)
               (p 1)
               (result nil))
          (while (/= 0 (logand c 128))
            (setq c (aref h p)
                  size (+ size (ash (logand c 127) shift))
                  shift (+ shift 7)
                  p (1+ p)))
          (setq result
                (pcase type
                  ('ofs-delta
                   (setq c (aref h p) p (1+ p))
                   (let ((rel (logand c 127)))
                     (while (/= 0 (logand c 128))
                       (setq c (aref h p) p (1+ p)
                             rel (+ (ash (1+ rel) 7) (logand c 127))))
                     ;; Inflate before resolving the base: that reuses the
                     ;; read buffer.
                     (let* ((delta (ngit--pack-inflate path offset p size got))
                            (base (ngit--pack-entry repo pack (- offset rel))))
                       (cons (car base) (ngit--apply-delta (cdr base) delta)))))
                  ('ref-delta
                   (let* ((delta (ngit--pack-inflate path offset (+ p 20) size got))
                          (base (ngit-read-object repo (substring h p (+ p 20)))))
                     (cons (car base) (ngit--apply-delta (cdr base) delta))))
                  ((or 'commit 'tree 'blob 'tag)
                   (cons type (ngit--pack-inflate path offset p size got)))
                  (_ (error "ngit: bad pack entry type at %s:%d" path offset))))
          (when (> (hash-table-count cache) 1024)
            (clrhash cache))
          (puthash offset result cache)))))

;;;; Objects

(defun ngit--objects-dir (repo)
  "REPO's objects directory with a trailing slash (cached)."
  (or (aref repo 5)
      (aset repo 5 (expand-file-name "objects/" (ngit-repo-commondir repo)))))

(defun ngit--loose-path (repo hex)
  (concat (ngit--objects-dir repo) (substring hex 0 2) "/" (substring hex 2)))

(defconst ngit--loose-head-size 65536
  "Loose objects up to this compressed size are read with one pread.")

(defun ngit--read-loose (repo hex)
  (let ((path (ngit--loose-path repo hex)))
    (when (ngit-native-try-open path)
      (let* ((got (ngit-native-read-head path 0 ngit--loose-head-size))
             (raw (prog1 (or (and (< got ngit--loose-head-size)
                                  (ngit-native-inflate-whole-head got))
                             (car (ngit-native-inflate-file path 0)))
                    (ngit-native-close path)))
             (nul (ngit--byte-pos raw 0 0))
             (sp (ngit--byte-pos raw ?\s 0))
             (data (substring raw (1+ nul))))
        (unless (= (string-to-number (substring raw (1+ sp) nul)) (length data))
          (error "ngit: loose object %s has a bad length" hex))
        (cons (intern (substring raw 0 sp)) data)))))

(defvar ngit--loose-names (make-hash-table :test 'equal)
  "Per objects/XX directory: its loose object names, keyed by name.")

(defun ngit-loose-names (repo sub)
  "Loose object names (38 digits) in objects/SUB: a hash table of the
names, plus under each two-digit prefix string the list of names that
start with it (`maphash' costs tens of milliseconds even on a small
table in NeLisp, so callers look lists up instead of iterating)."
  (or (gethash sub ngit--loose-names)
      (puthash sub
               (let ((dir (concat (ngit--objects-dir repo) sub))
                     (table (make-hash-table :test 'equal)))
                 (when (file-directory-p dir)
                   (dolist (f (directory-files dir nil nil t))
                     (when (= (length f) 38)
                       (puthash f t table)
                       (let ((k (substring f 0 2)))
                         (puthash k (cons f (gethash k table)) table)))))
                 table)
               ngit--loose-names)))

(defun ngit-forget-loose-names ()
  "Drop the cached loose object listings (after writing objects)."
  (clrhash ngit--loose-names))

(defvar ngit--pack-order nil
  "REPO's packs, most recently hit first (git's MRU pack list).")

(defun ngit--find-packed (repo bin)
  "Return (PACK . POSITION) of BIN in REPO's packs, or nil."
  (unless (and ngit--pack-order
               (= (length ngit--pack-order) (length (ngit-repo-packs repo)))
               (memq (car ngit--pack-order) (ngit-repo-packs repo)))
    (setq ngit--pack-order (copy-sequence (ngit-repo-packs repo))))
  (let ((packs ngit--pack-order) (found nil) (first t))
    (while (and packs (not found))
      (let* ((pack (car packs)) (pos (ngit--pack-find pack bin)))
        (when pos
          (setq found (cons pack pos))
          (unless first
            (setq ngit--pack-order (cons pack (delq pack ngit--pack-order))))))
      (setq packs (cdr packs) first nil))
    found))

(defun ngit-read-object (repo bin)
  "Return (TYPE . DATA) for object BIN in REPO, or signal an error.
Packs are searched before loose objects, as in git; the order only
affects speed, since an object's content is fixed."
  (let* ((hex (ngit-hex bin))
         ;; A listed loose object skips probing every pack.
         (loose (and (gethash (substring hex 2) (ngit-loose-names repo (substring hex 0 2)))
                     (ngit--read-loose repo hex))))
    (or loose
        (let ((hit (ngit--find-packed repo bin)))
          (and hit (ngit--pack-entry repo (car hit) (ngit--pack-offset (car hit) (cdr hit)))))
        (ngit--read-loose repo hex)
        (error "fatal: Not a valid object name %s" hex))))

(defun ngit-object-exists-p (repo bin)
  (or (ngit--find-packed repo bin)
      (file-exists-p (ngit--loose-path repo (ngit-hex bin)))))

(defun ngit-find-prefix (repo hex)
  "Return the list of distinct hex names in REPO starting with HEX."
  (let* ((hex (downcase hex))
         (nbytes (/ (length hex) 2))
         (key (ngit-unhex (substring hex 0 (* 2 nbytes))))
         ;; The odd trailing nibble, compared against the next byte.
         (nibble (and (= (% (length hex) 2) 1)
                      (string-to-number (substring hex -1) 16)))
         (found nil))
    (dolist (pack (ngit-repo-packs repo))
      (let* ((slice (ngit--pack-slice pack (aref key 0)))
             (pos (ngit--slice-lower-bound slice key))
             (n (/ (length slice) 20))
             (more t))
        (while (and more (< pos n))
          (let ((at (* 20 pos)))
            (if (not (string= (substring slice at (+ at nbytes)) key))
                (setq more nil)
              (when (or (null nibble) (= (ash (aref slice (+ at nbytes)) -4) nibble))
                (let ((h (ngit-hex (substring slice at (+ at 20)))))
                  (unless (member h found) (push h found))))))
          (setq pos (1+ pos)))))
    (let ((dir (expand-file-name (concat "objects/" (substring hex 0 2))
                                 (ngit-repo-commondir repo)))
          (rest (substring hex 2)))
      (when (file-directory-p dir)
        (dolist (f (directory-files dir nil nil t))
          (when (and (= (length f) 38) (string-prefix-p rest f))
            (let ((h (concat (substring hex 0 2) f)))
              (unless (member h found) (push h found)))))))
    found))

;;;; Object parsing

(defun ngit--header-end (data)
  "Return the index of the blank line ending the headers of DATA."
  (let ((pos 0) (end nil))
    (while (not end)
      (let ((eol (ngit--byte-pos data ?\n pos)))
        (cond ((null eol) (setq end (length data)))
              ((= eol pos) (setq end (max 0 (1- pos))))
              (t (setq pos (1+ eol))))))
    end))

(defun ngit--parse-headers (data pos end headers)
  "Parse the header lines of DATA from POS to END onto HEADERS (reversed)."
  (while (< pos end)
    (let* ((eol (or (ngit--byte-pos data ?\n pos) (length data)))
           (line (substring data pos eol)))
      (if (and headers (string-prefix-p " " line))
          ;; Continuation line (e.g. gpgsig).
          (setcdr (car headers) (concat (cdar headers) "\n" (substring line 1)))
        (let ((sp (or (ngit--byte-pos line ?\s 0) (length line))))
          (push (cons (substring line 0 sp)
                      (if (< sp (length line)) (substring line (1+ sp)) ""))
                headers)))
      (setq pos (1+ eol))))
  headers)

(defun ngit--scan-eol (data pos)
  "Index of the first newline at or after POS in DATA (which has one)."
  (while (/= (aref data pos) ?\n)
    (setq pos (1+ pos)))
  pos)

(defun ngit--fast-commit-headers (data)
  "Return (HEADERS-REVERSED . POS) for the usual commit header layout.
Tree and parent lines have fixed lengths and the committer ident often
repeats the author's, so only the ident lines are scanned.  POS is where
any further headers start.  Return nil for other layouts."
  (let ((n (length data)))
    (when (and (> n 52) (eq (aref data 0) ?t) (eq (aref data 45) ?\n)
               (eq (aref data 4) ?\s))
      (let ((headers (list (cons "tree" (substring data 5 45))))
            (pos 46))
        (while (and (< (+ pos 48) n) (eq (aref data pos) ?p) (eq (aref data (+ pos 47)) ?\n)
                    (eq (aref data (+ pos 6)) ?\s))
          (push (cons "parent" (substring data (+ pos 7) (+ pos 47))) headers)
          (setq pos (+ pos 48)))
        (when (and (< (+ pos 7) n) (eq (aref data pos) ?a) (eq (aref data (+ pos 6)) ?\s)
                   (string= (substring data pos (+ pos 7)) "author "))
          (let* ((aeol (ngit--scan-eol data (+ pos 7)))
                 (author (substring data (+ pos 7) aeol))
                 (cpos (1+ aeol)))
            (push (cons "author" author) headers)
            (when (and (< (+ cpos 10) n) (eq (aref data cpos) ?c)
                       (string= (substring data cpos (+ cpos 10)) "committer "))
              (let* ((start (+ cpos 10))
                     (guess (+ start (length author)))
                     (ceol (if (and (< guess n) (eq (aref data guess) ?\n)
                                    (string= (substring data start guess) author))
                               guess
                             (ngit--scan-eol data start))))
                (push (cons "committer" (substring data start ceol)) headers)
                (cons headers (1+ ceol))))))))))

(defun ngit-parse-commit (data)
  "Parse commit or tag DATA into a plist.
Keys: :headers (list of (NAME . VALUE) in order) and :message."
  (let* ((fast (ngit--fast-commit-headers data))
         (n (length data)))
    (if (and fast (< (cdr fast) n) (eq (aref data (cdr fast)) ?\n))
        (list :headers (nreverse (car fast))
              :message (substring data (1+ (cdr fast))))
      (let* ((end (ngit--header-end data))
             (headers (if fast
                          (ngit--parse-headers data (cdr fast) end (car fast))
                        (ngit--parse-headers data 0 end nil))))
        (list :headers (nreverse headers)
              :message (if (< (+ end 2) n) (substring data (+ end 2)) ""))))))

(defun ngit-header (parsed name)
  (cdr (assoc name (plist-get parsed :headers))))

(defun ngit-header-all (parsed name)
  (let (vals)
    (dolist (h (plist-get parsed :headers))
      (when (string= (car h) name) (push (cdr h) vals)))
    (nreverse vals)))

(defun ngit-parse-tree (data)
  "Parse tree DATA into a list of (MODE NAME BIN)."
  (let ((pos 0) (n (length data)) (entries nil))
    (while (< pos n)
      (let* ((sp (ngit--byte-pos data ?\s pos))
             (nul (ngit--byte-pos data 0 sp)))
        (push (list (substring data pos sp)
                    (substring data (1+ sp) nul)
                    (substring data (1+ nul) (+ nul 21)))
              entries)
        (setq pos (+ nul 21))))
    (nreverse entries)))

(defun ngit-mode-type (mode)
  (cond ((string= mode "40000") "tree")
        ((string= mode "160000") "commit")
        (t "blob")))

(provide 'ngit-store)

;;; ngit-store.el ends here
