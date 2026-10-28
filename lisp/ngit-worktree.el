;;; ngit-worktree.el --- work tree files: lstat, contents, hashing -*- lexical-binding: t; -*-

;;; Commentary:

;; Work tree access by raw syscalls.  `file-attributes' costs ~3 ms per
;; call in NeLisp, so lstat(2) writes the path into a native buffer and
;; reads the needed fields of struct stat (x86_64 layout).  Paths are
;; unibyte strings relative to the top of the work tree; consecutive
;; calls in sorted order share the directory prefix already written.
;;
;; Hashing applies git's convert_to_git end-of-line rules (core.autocrlf,
;; the text/eol/crlf attributes and the "safer autocrlf" index check).

;;; Code:

(require 'ngit-index)
(require 'ngit-match)

(defconst ngit-wt--sys-lstat 6)
(defconst ngit-wt--sys-readlink 89)

(defconst ngit-s-ifmt #o170000)
(defconst ngit-s-ifreg #o100000)
(defconst ngit-s-ifdir #o040000)
(defconst ngit-s-iflnk #o120000)

(defvar ngit-wt--path nil "Native buffer holding the current C path.")
(defvar ngit-wt--stat nil "Native buffer for struct stat.")
(defvar ngit-wt--top nil "(TOP-BYTES . LEN) written at the start of the path buffer.")
(defvar ngit-wt--dir nil "Relative directory prefix currently written after the top.")

(defun ngit-wt--write (addr off bytes from)
  "Write BYTES starting at index FROM to ADDR+OFF; return the ending offset."
  (let ((n (length bytes)))
    (if (and (not (multibyte-string-p bytes)) (>= from 0) (<= from n))
        (progn
          (when (< from n)
            (ptr-write-bytes (+ addr off) (substring bytes from)))
          (+ off (- n from)))
      (let ((i from) (o off))
        (while (<= (+ i 4) n)
          (ptr-write-u32 addr o (logior (aref bytes i) (ash (aref bytes (+ i 1)) 8)
                                        (ash (aref bytes (+ i 2)) 16)
                                        (ash (aref bytes (+ i 3)) 24)))
          (setq i (+ i 4) o (+ o 4)))
        (while (< i n)
          (ptr-write-u8 addr o (aref bytes i))
          (setq i (1+ i) o (1+ o)))
        o))))

(defun ngit-wt-setup (top)
  "Prepare the path buffer for work tree TOP (a directory name)."
  (unless ngit-wt--path
    (setq ngit-wt--path (nl-ffi-memory-allocate 8192)
          ngit-wt--stat (nl-ffi-memory-allocate 256)))
  (let ((bytes (encode-coding-string (file-name-as-directory top) 'utf-8)))
    (unless (equal (car ngit-wt--top) bytes)
      (ngit-wt--write (nl-ffi-memory-address ngit-wt--path) 0 bytes 0)
      (setq ngit-wt--top (cons bytes (length bytes)) ngit-wt--dir nil))))

(defun ngit-wt--set-path (rel)
  "Make the path buffer hold TOP/REL; return its address."
  (when (ngit-windows-p) (ngit-win-validate-worktree-path rel))
  (let* ((addr (nl-ffi-memory-address ngit-wt--path))
         (base (cdr ngit-wt--top))
         (end nil))
    (if (and ngit-wt--dir (> (length rel) (length ngit-wt--dir))
             (string= ngit-wt--dir (substring rel 0 (length ngit-wt--dir))))
        (setq end (ngit-wt--write addr (+ base (length ngit-wt--dir)) rel
                                  (length ngit-wt--dir)))
      (let ((i (1- (length rel))))
        (while (and (>= i 0) (/= (aref rel i) ?/)) (setq i (1- i)))
        (setq ngit-wt--dir (and (>= i 0) (substring rel 0 (1+ i)))
              end (ngit-wt--write addr base rel 0))))
    (ptr-write-u8 addr end 0)
    addr))

(defun ngit-wt-lstat (rel)
  "Return [MODE INO SIZE MTIME MTIME-NS CTIME] for REL, or nil if absent."
  (if (ngit-windows-p)
      (ngit-win-stat (ngit-wt-absolute rel) (nl-ffi-memory-address ngit-wt--stat))
   (let* ((addr (ngit-wt--set-path rel))
         (sa (nl-ffi-memory-address ngit-wt--stat)))
    (and (>= (syscall-direct ngit-wt--sys-lstat addr sa 0 0 0 0) 0)
         (vector (ptr-read-u32 sa 24) (ptr-read-u64 sa 8) (ptr-read-u64 sa 48)
                 (ptr-read-u64 sa 88) (ptr-read-u64 sa 96) (ptr-read-u64 sa 104))))))

(defun ngit-wt-absolute (rel)
  "Expand UTF-8 relative worktree REL into a native Lisp path."
  (when (ngit-windows-p) (ngit-win-validate-worktree-path rel))
  (expand-file-name (decode-coding-string rel 'utf-8)
                    (decode-coding-string (car ngit-wt--top) 'utf-8)))

(defun ngit-wt-gitdir-lstat (parent)
  "Read-only nested-repository probe below validated worktree PARENT.
Only this literal internal metadata suffix bypasses the user-path guard."
  (if (ngit-windows-p)
      (ngit-win-stat (expand-file-name ".git" (file-name-as-directory (ngit-wt-absolute parent)))
                     (nl-ffi-memory-address ngit-wt--stat))
    (ngit-wt-lstat (concat parent "/.git"))))

(defun ngit-wt-lstat-result (rel)
  "Return zero for present REL, -2 when absent; other failures signal."
  (if (ngit-windows-p) (if (ngit-wt-lstat rel) 0 -2)
    (syscall-direct ngit-wt--sys-lstat (ngit-wt--set-path rel)
                    (nl-ffi-memory-address ngit-wt--stat) 0 0 0 0)))

(defconst ngit-wt--sys-newfstatat 262)
(defconst ngit-wt--at-symlink-nofollow #x100)
(defvar ngit-wt--top-fds (make-hash-table :test 'equal))

(defun ngit-wt--top-fd ()
  "Return a directory descriptor for the current work tree top."
  (let ((top (car ngit-wt--top)))
    (or (gethash top ngit-wt--top-fds)
        (let ((fd (syscall-direct ngit-native--sys-open (ngit-wt--set-path "")
                                  (logior #o200000 ngit-native--o-rdonly-cloexec) 0 0 0 0)))
          (when (< fd 0) (error "ngit: cannot open work tree (errno %d)" (- fd)))
          (puthash top fd ngit-wt--top-fds)))))

(defun ngit-wt-lstat-at (cpath)
  "Like `ngit-wt-lstat' for a NUL-terminated relative path at address CPATH."
  (if (ngit-windows-p)
      (let ((size 0))
        (while (/= 0 (ptr-read-u8 cpath size)) (setq size (1+ size)))
        (ngit-wt-lstat (ptr-read-bytes cpath size)))
   (let ((sa (nl-ffi-memory-address ngit-wt--stat)))
    (and (>= (syscall-direct ngit-wt--sys-newfstatat (ngit-wt--top-fd) cpath sa
                             ngit-wt--at-symlink-nofollow 0 0)
             0)
         (vector (ptr-read-u32 sa 24) (ptr-read-u64 sa 8) (ptr-read-u64 sa 48)
                 (ptr-read-u64 sa 88) nil (ptr-read-u64 sa 104))))))

(defun ngit-wt-exists-p (rel)
  (ngit-wt-lstat rel))

(defun ngit-wt-read (rel size)
  "Return the SIZE-byte contents of regular file REL (path buffer reused)."
  (if (ngit-windows-p)
      (let ((path (ngit-wt-absolute rel)))
        (unwind-protect (ngit-native-read-bytes path 0 size)
          (ngit-native-close path)))
   (if (<= size 0)
      ""
    (let* ((addr (ngit-wt--set-path rel))
           (fd (syscall-direct ngit-native--sys-open addr ngit-native--o-rdonly-cloexec 0 0 0 0)))
      (when (< fd 0) (error "ngit: cannot open %s (errno %d)" rel (- fd)))
      (unwind-protect
          (let* ((buf (ngit-native--buffer 0 size))
                 (ba (nl-ffi-memory-address buf))
                 (done 0) (eof nil))
            (while (and (< done size) (not eof))
              (let ((n (syscall-direct ngit-native--sys-pread64 fd (+ ba done)
                                       (- size done) done 0 0)))
                (if (<= n 0) (setq eof t) (setq done (+ done n)))))
            (ptr-read-bytes ba done))
        (syscall-direct ngit-native--sys-close fd 0 0 0 0 0))))))

(defun ngit-wt-readlink (rel)
  (when (ngit-windows-p) (error "ngit: Windows symbolic links are unsupported"))
  (let* ((addr (ngit-wt--set-path rel))
         (buf (ngit-native--buffer 0 4096))
         (ba (nl-ffi-memory-address buf))
         (n (syscall-direct ngit-wt--sys-readlink addr ba 4096 0 0 0)))
    (if (< n 0) "" (ptr-read-bytes ba n))))

;;;; Content as git sees it

(defun ngit-hash-blob (data)
  "Return the 20-byte blob name of unibyte DATA."
  (ngit-native-object-id 'blob data))

(defun ngit--count-literal (lit s)
  "Count occurrences of literal string LIT in S (~1 ms per occurrence)."
  (let ((n 0) (pos 0) (re (regexp-quote lit)))
    (while (string-match re s pos)
      (setq n (1+ n) pos (match-end 0)))
    n))

(defconst ngit--nonprintables
  (let ((l (list (string 127))) (c 0))
    (while (< c 32)
      (unless (memq c '(8 9 10 13 27 12)) (push (string c) l))
      (setq c (1+ c)))
    l)
  "Bytes convert.c gather_stats counts as non-printable.")

(defun ngit--text-stats (s)
  "Return (CRLF-P LONECR-P NUL-P BINARYP) for S, following convert.c
gather_stats.  Character-class regexps scan at ~0.5 ms per byte in
NeLisp, so only literal searches are used, and counts are taken only
when the binary test needs them."
  (let* ((crlf (and (string-match-p "\r\n" s) t))
         (lonecr (and (string-match-p "\r" (if crlf (ngit--strip-crlf s) s)) t))
         (nul (and (string-match-p "\0" s) t))
         (nonprint 0))
    (dolist (c ngit--nonprintables)
      (when (string-match-p (regexp-quote c) s)
        (setq nonprint (+ nonprint (ngit--count-literal c s)))))
    (when (and (> (length s) 0) (= (aref s (1- (length s))) 26))
      (setq nonprint (1- nonprint)))
    (list crlf lonecr nul
          (or lonecr nul
              (and (> nonprint 0)
                   (let ((printable (- (length s) (ngit--count-literal "\r" s)
                                       (ngit--count-literal "\n" s) nonprint)))
                     (< (ash printable -7) nonprint)))))))

(defun ngit--strip-crlf (data)
  "Return DATA with every CR LF replaced by LF."
  (let ((parts nil) (pos 0) (hit nil))
    (while (setq hit (string-match "\r\n" data pos))
      (push (substring data pos hit) parts)
      (setq pos (1+ hit)))
    (push (substring data pos) parts)
    (apply #'concat (nreverse parts))))

(defun ngit--crlf-action (repo path)
  "Return the convert.c crlf_action symbol for PATH."
  (let* ((attrs (ngit-attrs repo path '("text" "crlf" "eol")))
         (text (cdr (assoc "text" attrs)))
         (text (if (eq text 'unspecified) (cdr (assoc "crlf" attrs)) text))
         (eol (cdr (assoc "eol" attrs)))
         (action (cond ((eq text 'set) 'text)
                       ((eq text 'unset) 'binary)
                       ((equal text "auto") 'auto)
                       ((equal text "input") 'text-input)
                       (t nil))))
    (unless (eq action 'binary)
      (cond ((and (eq action 'auto) (equal eol "lf")) (setq action 'auto-input))
            ((and (eq action 'auto) (equal eol "crlf")) (setq action 'auto-crlf))
            ((equal eol "lf") (setq action 'text-input))
            ((equal eol "crlf") (setq action 'text-crlf))))
    (let ((autocrlf (ngit-config-get repo "core.autocrlf")))
      (cond (action (if (eq action 'text) 'text-input action))
            ((equal autocrlf "input") 'auto-input)
            ((and autocrlf (ngit-config-bool repo "core.autocrlf" nil)) 'auto-crlf)
            (t 'binary)))))

(defun ngit--index-has-crlf-p (repo path)
  (let* ((entries (ngit-index-entries repo))
         (e (ngit-index-lookup entries path)))
    ;; read_blob_data_from_index uses our side during a conflict.
    (when (and e (> (aref e 3) 0))
      (setq e (aref (cdr (ngit-index-group entries (ngit-index-find entries path))) 2)))
    (and e
         (let ((data (cdr (ngit-read-object repo (aref e 1)))))
           (and (string-match-p "\r" data)
                (let ((st (ngit--text-stats data)))
                  (and (not (nth 3 st)) (nth 0 st))))))))

(defun ngit-convert-to-git (repo path data)
  "Apply git's clean-side end-of-line conversion for PATH to DATA."
  ;; Regexps, not `string-search': the latter costs ~0.2 ms per byte in
  ;; NeLisp, while a literal regexp scans file-sized data in milliseconds.
  (if (or (string= data "") (not (string-match-p "\r\n" data)))
      data
    (let ((action (ngit--crlf-action repo path)))
      (if (or (eq action 'binary)
              (and (memq action '(auto auto-input auto-crlf))
                   (or (nth 3 (ngit--text-stats data))
                       (ngit--index-has-crlf-p repo path))))
          data
        (ngit--strip-crlf data)))))

(defun ngit-wt-content (repo rel st)
  "Return the git-side content of work tree path REL with lstat ST."
  (if (= (logand (aref st 0) ngit-s-ifmt) ngit-s-iflnk)
      (ngit-wt-readlink rel)
    (ngit-convert-to-git repo rel (ngit-wt-read rel (aref st 2)))))

(defun ngit-wt-mode (st)
  "Return the git mode for lstat ST (regular file, executable, symlink)."
  (let ((fmt (logand (aref st 0) ngit-s-ifmt)))
    (cond ((= fmt ngit-s-iflnk) #o120000)
          ((= fmt ngit-s-ifdir) #o040000)
          ((/= 0 (logand (aref st 0) #o100)) #o100755)
          (t #o100644))))

(provide 'ngit-worktree)

;;; ngit-worktree.el ends here
