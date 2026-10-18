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

(defun ngit-unhex (hex)
  "Return the unibyte string for the even-length HEX string."
  (let ((bytes nil) (i 0) (n (length hex)))
    (while (< i n)
      (push (string-to-number (substring hex i (+ i 2)) 16) bytes)
      (setq i (+ i 2)))
    (apply #'unibyte-string (nreverse bytes))))

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
            (make-hash-table :test 'eql) (make-hash-table :test 'eql))))

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
  "First position in SLICE whose name prefix is not less than KEY."
  (let ((lo 0) (hi (/ (length slice) 20)) (k (length key)))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (string< (substring slice (* 20 mid) (+ (* 20 mid) k)) key)
            (setq lo (1+ mid))
          (setq hi mid))))
    lo))

(defun ngit--pack-find (pack bin)
  "Return the index position of BIN in PACK, or nil."
  (let* ((slice (ngit--pack-slice pack (aref bin 0)))
         (pos (ngit--slice-lower-bound slice bin)))
    (and (< (* 20 pos) (length slice))
         (string= (substring slice (* 20 pos) (+ 20 (* 20 pos))) bin)
         (+ pos (car (ngit--pack-range pack (aref bin 0)))))))

(defun ngit--pack-offset (pack index)
  (let* ((n (aref pack 2))
         (idx (aref pack 1))
         (off (ngit--be32 (ngit-native-read-bytes idx (+ 1032 (* 24 n) (* 4 index)) 4) 0)))
    (if (zerop (logand off #x80000000))
        off
      (let ((s (ngit-native-read-bytes
                idx (+ 1032 (* 28 n) (* 8 (logand off #x7fffffff))) 8)))
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

(defun ngit--pack-entry (repo pack offset)
  "Return (TYPE . DATA) for the entry at OFFSET in PACK."
  (let ((cache (aref pack 5)))
    (or (gethash offset cache)
        (let* ((path (aref pack 0))
               (h (ngit-native-read-bytes path offset 32))
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
                     (let ((base (ngit--pack-entry repo pack (- offset rel))))
                       (cons (car base)
                             (ngit--apply-delta
                              (cdr base)
                              (car (ngit-native-inflate-file path (+ offset p) size)))))))
                  ('ref-delta
                   (let ((base (ngit-read-object repo (substring h p (+ p 20)))))
                     (cons (car base)
                           (ngit--apply-delta
                            (cdr base)
                            (car (ngit-native-inflate-file path (+ offset p 20) size))))))
                  ((or 'commit 'tree 'blob 'tag)
                   (cons type (car (ngit-native-inflate-file path (+ offset p) size))))
                  (_ (error "ngit: bad pack entry type at %s:%d" path offset))))
          (when (> (hash-table-count cache) 1024)
            (clrhash cache))
          (puthash offset result cache)))))

;;;; Objects

(defun ngit--loose-path (repo hex)
  (expand-file-name (concat "objects/" (substring hex 0 2) "/" (substring hex 2))
                    (ngit-repo-commondir repo)))

(defun ngit--read-loose (repo hex)
  (let ((path (ngit--loose-path repo hex)))
    (when (file-exists-p path)
      (let* ((raw (prog1 (car (ngit-native-inflate-file path 0))
                    (ngit-native-close path)))
             (nul (ngit--byte-pos raw 0 0))
             (header (substring raw 0 nul))
             (sp (ngit--byte-pos header ?\s 0))
             (data (substring raw (1+ nul))))
        (unless (= (string-to-number (substring header (1+ sp))) (length data))
          (error "ngit: loose object %s has a bad length" hex))
        (cons (intern (substring header 0 sp)) data)))))

(defun ngit-read-object (repo bin)
  "Return (TYPE . DATA) for object BIN in REPO, or signal an error."
  (or (let ((packs (ngit-repo-packs repo)) (found nil))
        (while (and packs (not found))
          (let ((pos (ngit--pack-find (car packs) bin)))
            (when pos
              (setq found (ngit--pack-entry repo (car packs)
                                            (ngit--pack-offset (car packs) pos)))))
          (setq packs (cdr packs)))
        found)
      (ngit--read-loose repo (ngit-hex bin))
      (error "fatal: Not a valid object name %s" (ngit-hex bin))))

(defun ngit-object-exists-p (repo bin)
  (or (let ((packs (ngit-repo-packs repo)) (found nil))
        (while (and packs (not found))
          (setq found (ngit--pack-find (car packs) bin) packs (cdr packs)))
        found)
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

(defun ngit-parse-commit (data)
  "Parse commit or tag DATA into a plist.
Keys: :headers (list of (NAME . VALUE) in order) and :message."
  (let* ((end (ngit--header-end data))
         (headers nil)
         (pos 0))
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
    (list :headers (nreverse headers)
          :message (if (< (+ end 2) (length data)) (substring data (+ end 2)) ""))))

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
