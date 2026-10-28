;;; ngit-merge-file.el --- Byte-exact three-way content merge  -*- lexical-binding: t; -*-

;;; Commentary:

;; A plain-Elisp port of Git 2.47's xdiff/xmerge.c.  It shares the Myers
;; implementation in ngit-xdiff.el and implements the merge refinement and
;; marker emission used by `git merge-file'.

;;; Code:

(require 'ngit-xdiff)

(defconst ngit-merge-file--default-marker-size 7)

;; Merge records are [MODE I0 CHG0 I1 CHG1 I2 CHG2].

(defun ngit-merge-file--append (state mode i0 chg0 i1 chg1 i2 chg2)
  "Append one xmerge record to mutable STATE, coalescing overlap."
  (let ((tail (aref state 1)) m cell)
    (if (and tail
             (let ((last (car tail)))
               (or (<= i1 (+ (aref last 3) (aref last 4)))
                   (<= i2 (+ (aref last 5) (aref last 6))))))
        (progn
          (setq m (car tail))
          (unless (= mode (aref m 0)) (aset m 0 0))
          (aset m 2 (- (+ i0 chg0) (aref m 1)))
          (aset m 4 (- (+ i1 chg1) (aref m 3)))
          (aset m 6 (- (+ i2 chg2) (aref m 5))))
      (setq m (vector mode i0 chg0 i1 chg1 i2 chg2))
      (setq cell (list m))
      (if tail (setcdr tail cell) (aset state 0 cell))
      (aset state 1 cell))))

(defun ngit-merge-file--same-lines-p (r1 i1 r2 i2 count)
  (let ((i 0) (same t))
    (while (and same (< i count))
      (setq same (string= (aref r1 (+ i1 i)) (aref r2 (+ i2 i))))
      (setq i (1+ i)))
    same))

(defun ngit-merge-file--slice-string (recs start count)
  (let ((i start) (end (+ start count)) (out nil))
    (while (< i end)
      (setq out (cons (aref recs i) out))
      (setq i (1+ i)))
    (apply #'concat (nreverse out))))

(defun ngit-merge-file--slice-vector (recs start count)
  (let ((out (make-vector count nil)) (i 0))
    (while (< i count)
      (aset out i (aref recs (+ start i)))
      (setq i (1+ i)))
    out))

(defun ngit-merge-file--refine-zdiff3 (ours theirs changes)
  (let ((p changes) m)
    (while p
      (setq m (car p))
      (when (= (aref m 0) 0)
        (while (and (> (aref m 4) 0) (> (aref m 6) 0)
                    (string= (aref ours (aref m 3))
                             (aref theirs (aref m 5))))
          (aset m 4 (1- (aref m 4)))
          (aset m 6 (1- (aref m 6)))
          (aset m 3 (1+ (aref m 3)))
          (aset m 5 (1+ (aref m 5))))
        (while (and (> (aref m 4) 0) (> (aref m 6) 0)
                    (string= (aref ours (+ (aref m 3) (aref m 4) -1))
                             (aref theirs (+ (aref m 5) (aref m 6) -1))))
          (aset m 4 (1- (aref m 4)))
          (aset m 6 (1- (aref m 6)))))
      (setq p (cdr p)))))

(defun ngit-merge-file--refine (ours theirs changes)
  "Split nonempty conflicts in CHANGES at common OURS/THEIRS lines."
  (let ((cell changes) m i1 i2 sub1 sub2 script first rest new tail)
    (while cell
      (setq m (car cell))
      (when (and (= (aref m 0) 0) (> (aref m 4) 0) (> (aref m 6) 0))
        (setq i1 (aref m 3) i2 (aref m 5))
        (setq sub1 (ngit-merge-file--slice-vector ours i1 (aref m 4)))
        (setq sub2 (ngit-merge-file--slice-vector theirs i2 (aref m 6)))
        (setq script (ngit-xdiff--diff-script sub1 sub2 nil))
        (if (not script)
            (aset m 0 4)
          (setq first (car script))
          (aset m 3 (+ i1 (aref first 0)))
          (aset m 4 (aref first 2))
          (aset m 5 (+ i2 (aref first 1)))
          (aset m 6 (aref first 3))
          (setq rest (cdr script) tail cell)
          (while rest
            (setq first (car rest))
            (setq new (list (vector 0 0 0
                                    (+ i1 (aref first 0)) (aref first 2)
                                    (+ i2 (aref first 1)) (aref first 3))))
            (setcdr new (cdr tail))
            (setcdr tail new)
            (setq tail new rest (cdr rest)))
          (setq cell tail)))
      (setq cell (cdr cell)))))

(defun ngit-merge-file--alnum-prefix (recs)
  "Return prefix counts of records in RECS containing an ASCII alnum.
The regular expression is applied once to the whole file, outside the
conflict traversal."
  (let* ((n (length recs))
         (text (ngit-merge-file--slice-string recs 0 n))
         (kept (replace-regexp-in-string "[^A-Za-z0-9\n]" "" text))
         (lines (split-string kept "\n" nil))
         (prefix (make-vector (1+ n) 0))
         (i 0))
    (while (< i n)
      (aset prefix (1+ i)
            (+ (aref prefix i) (if (> (length (car lines)) 0) 1 0)))
      (setq lines (cdr lines) i (1+ i)))
    prefix))

(defun ngit-merge-file--simplify (changes alnum-prefix)
  "Fold short non-conflicting gaps between conflict records."
  (let ((cell changes) m next begin end)
    (while (and cell (cdr cell))
      (setq m (car cell) next (car (cdr cell)))
      (setq begin (+ (aref m 3) (aref m 4)) end (aref next 3))
      (if (or (/= (aref m 0) 0) (/= (aref next 0) 0)
              (and (> (- end begin) 3)
                   (or (not alnum-prefix)
                       (> (- (aref alnum-prefix end)
                             (aref alnum-prefix begin))
                          0))))
          (setq cell (cdr cell))
        (aset m 4 (- (+ (aref next 3) (aref next 4)) (aref m 3)))
        (aset m 6 (- (+ (aref next 5) (aref next 6)) (aref m 5)))
        (setcdr cell (cdr (cdr cell)))))))

(defun ngit-merge-file--make-changes (base ours theirs script1 script2 level)
  "Translate two base-to-side scripts to the xmerge change list."
  (let ((state (vector nil nil)) (s1 script1) (s2 script2)
        x1 x2 i0 i1 i2 chg0 chg1 chg2 off ffo e1 e2)
    (while (and s1 s2)
      (setq x1 (car s1) x2 (car s2))
      (cond
       ((< (+ (aref x1 0) (aref x1 2)) (aref x2 0))
        (setq i0 (aref x1 0) i1 (aref x1 1)
              i2 (+ (- (aref x2 1) (aref x2 0)) (aref x1 0))
              chg0 (aref x1 2) chg1 (aref x1 3) chg2 (aref x1 2))
        (ngit-merge-file--append state 1 i0 chg0 i1 chg1 i2 chg2)
        (setq s1 (cdr s1)))
       ((< (+ (aref x2 0) (aref x2 2)) (aref x1 0))
        (setq i0 (aref x2 0)
              i1 (+ (- (aref x1 1) (aref x1 0)) (aref x2 0))
              i2 (aref x2 1)
              chg0 (aref x2 2) chg1 (aref x2 2) chg2 (aref x2 3))
        (ngit-merge-file--append state 2 i0 chg0 i1 chg1 i2 chg2)
        (setq s2 (cdr s2)))
       (t
        (when (or (= level 0)
                  (/= (aref x1 0) (aref x2 0))
                  (/= (aref x1 2) (aref x2 2))
                  (/= (aref x1 3) (aref x2 3))
                  (not (ngit-merge-file--same-lines-p
                        ours (aref x1 1) theirs (aref x2 1) (aref x1 3))))
          (setq off (- (aref x1 0) (aref x2 0)))
          (setq ffo (- (+ off (aref x1 2)) (aref x2 2)))
          (setq i0 (aref x1 0) i1 (aref x1 1) i2 (aref x2 1))
          (if (> off 0)
              (setq i0 (- i0 off) i1 (- i1 off))
            (setq i2 (+ i2 off)))
          (setq chg0 (- (+ (aref x1 0) (aref x1 2)) i0))
          (setq chg1 (- (+ (aref x1 1) (aref x1 3)) i1))
          (setq chg2 (- (+ (aref x2 1) (aref x2 3)) i2))
          (if (< ffo 0)
              (setq chg0 (- chg0 ffo) chg1 (- chg1 ffo))
            (setq chg2 (+ chg2 ffo)))
          (ngit-merge-file--append state 0 i0 chg0 i1 chg1 i2 chg2))
        (setq e1 (+ (aref x1 0) (aref x1 2)))
        (setq e2 (+ (aref x2 0) (aref x2 2)))
        (when (>= e1 e2) (setq s2 (cdr s2)))
        (when (>= e2 e1) (setq s1 (cdr s1))))))
    (while s1
      (setq x1 (car s1))
      (ngit-merge-file--append
       state 1 (aref x1 0) (aref x1 2) (aref x1 1) (aref x1 3)
       (+ (aref x1 0) (- (length theirs) (length base))) (aref x1 2))
      (setq s1 (cdr s1)))
    (while s2
      (setq x2 (car s2))
      (ngit-merge-file--append
       state 2 (aref x2 0) (aref x2 2)
       (+ (aref x2 0) (- (length ours) (length base))) (aref x2 2)
       (aref x2 1) (aref x2 3))
      (setq s2 (cdr s2)))
    (aref state 0)))

(defun ngit-merge-file--eol-crlf (recs i)
  (let ((n (length recs)) rec size)
    (cond
     ((= n 0) -1)
     ((< i (1- n))
      (setq rec (aref recs i) size (length rec))
      (if (and (> size 1) (= (aref rec (- size 2)) 13)) 1 0))
     (t
      (setq rec (aref recs i) size (length rec))
      (cond
       ((and (> size 0) (= (aref rec (1- size)) 10))
        (if (and (> size 1) (= (aref rec (- size 2)) 13)) 1 0))
       ((= i 0) -1)
       (t
        (setq rec (aref recs (1- i)) size (length rec))
        (if (and (> size 1) (= (aref rec (- size 2)) 13)) 1 0)))))))

(defun ngit-merge-file--needs-cr (base ours theirs m)
  (let ((v (ngit-merge-file--eol-crlf ours
                                      (if (> (aref m 3) 0) (1- (aref m 3)) 0))))
    (when (/= v 0)
      (setq v (ngit-merge-file--eol-crlf
               theirs (if (> (aref m 5) 0) (1- (aref m 5)) 0))))
    (when (/= v 0) (setq v (ngit-merge-file--eol-crlf base 0)))
    (= v 1)))

(defun ngit-merge-file--copy (recs start count needs-cr add-nl)
  (let ((s (ngit-merge-file--slice-string recs start count)))
    (if (and add-nl (> count 0)
             (or (= (length s) 0) (/= (aref s (1- (length s))) 10)))
        (concat s (if needs-cr "\r\n" "\n"))
      s)))

(defun ngit-merge-file--marker (char size label needs-cr)
  (concat (make-string size char)
          (if label (concat " " label) "")
          (if needs-cr "\r\n" "\n")))

(defun ngit-merge-file--emit (base ours theirs changes labels style favor marker-size)
  (let ((out nil) (i 0) (p changes) m mode needs-cr
        (base-label (car labels)) (ours-label (cadr labels))
        (theirs-label (car (cdr (cdr labels)))))
    (while p
      (setq m (car p) mode (aref m 0))
      (when (and (/= favor 0) (= mode 0))
        (setq mode favor)
        (aset m 0 mode))
      (cond
       ((= mode 0)
        (setq needs-cr (ngit-merge-file--needs-cr base ours theirs m))
        (setq out (cons (ngit-merge-file--copy ours i (- (aref m 3) i) nil nil) out))
        (setq out (cons (ngit-merge-file--marker ?< marker-size ours-label needs-cr) out))
        (setq out (cons (ngit-merge-file--copy ours (aref m 3) (aref m 4) needs-cr t) out))
        (when (or (= style 1) (= style 2))
          (setq out (cons (ngit-merge-file--marker ?| marker-size base-label needs-cr) out))
          (setq out (cons (ngit-merge-file--copy base (aref m 1) (aref m 2) needs-cr t) out)))
        (setq out (cons (ngit-merge-file--marker ?= marker-size nil needs-cr) out))
        (setq out (cons (ngit-merge-file--copy theirs (aref m 5) (aref m 6) needs-cr t) out))
        (setq out (cons (ngit-merge-file--marker ?> marker-size theirs-label needs-cr) out)))
       ((/= 0 (logand mode 3))
        (setq out (cons (ngit-merge-file--copy ours i (- (aref m 3) i) nil nil) out))
        (when (/= 0 (logand mode 1))
          (setq needs-cr (ngit-merge-file--needs-cr base ours theirs m))
          (setq out (cons (ngit-merge-file--copy ours (aref m 3) (aref m 4)
                                                  needs-cr (/= 0 (logand mode 2))) out)))
        (when (/= 0 (logand mode 2))
          (setq out (cons (ngit-merge-file--copy theirs (aref m 5) (aref m 6) nil nil) out)))))
      ;; mode 4 is a refinement artifact meaning the two sides were equal.
      ;; xmerge's `continue' leaves I unchanged so normal copying emits it.
      (when (or (= mode 0) (/= 0 (logand mode 3)))
        (setq i (+ (aref m 3) (aref m 4))))
      (setq p (cdr p)))
    (setq out (cons (ngit-merge-file--copy ours i (- (length ours) i) nil nil) out))
    (apply #'concat (nreverse out))))

(defun ngit-merge-file--option-number (value names what min max default)
  (if (null value)
      default
    (let ((n (if (numberp value) value (cdr (assq value names)))))
      (unless (and (numberp n) (<= min n) (<= n max))
        (error "Invalid %s: %S" what value))
      n)))

;;;###autoload
(defun ngit-merge-file (base ours theirs &rest options)
  "Merge unibyte strings BASE, OURS and THEIRS using Git xdiff semantics.
OPTIONS accepts :labels (BASE OURS THEIRS), :style merge/diff3/zdiff3 (or
0/1/2), :favor none/ours/theirs/union, :marker-size, and :level 0..3.
Return (RESULT . CONFLICTS), where RESULT is a unibyte string."
  (let* ((labels (or (plist-get options :labels) '(nil nil nil)))
         (style (ngit-merge-file--option-number
                 (plist-get options :style) '((merge . 0) (diff3 . 1) (zdiff3 . 2))
                 "merge style" 0 2 0))
         (favor (ngit-merge-file--option-number
                 (plist-get options :favor)
                 '((none . 0) (ours . 1) (theirs . 2) (union . 3))
                 "merge favor" 0 3 0))
         (marker-size (or (plist-get options :marker-size)
                          ngit-merge-file--default-marker-size))
         (level (or (plist-get options :level) 3))
         (base-recs (ngit-xdiff--split base))
         (ours-recs (ngit-xdiff--split ours))
         (theirs-recs (ngit-xdiff--split theirs))
         (script1 (ngit-xdiff--diff-script base-recs ours-recs nil))
         (script2 (ngit-xdiff--diff-script base-recs theirs-recs nil))
         changes result conflicts p)
    (unless (and (listp labels) (= (length labels) 3))
      (error ":labels must be a three-element list"))
    (unless (numberp marker-size) (error ":marker-size must be numeric"))
    (when (<= marker-size 0)
      (setq marker-size ngit-merge-file--default-marker-size))
    (unless (and (numberp level) (<= 0 level) (<= level 3))
      (error ":level must be from 0 through 3"))
    (cond
     ((not script1) (setq result theirs conflicts 0))
     ((not script2) (setq result ours conflicts 0))
     (t
      (when (or (= style 1) (= style 2)) (setq level (min level 1)))
      (setq changes (ngit-merge-file--make-changes
                     base-recs ours-recs theirs-recs script1 script2 level))
      (cond
       ((= style 2) (ngit-merge-file--refine-zdiff3 ours-recs theirs-recs changes))
       ((>= level 2)
        (ngit-merge-file--refine ours-recs theirs-recs changes)
        (ngit-merge-file--simplify
         changes (and (> level 2)
                      (ngit-merge-file--alnum-prefix ours-recs)))))
      (setq result (ngit-merge-file--emit base-recs ours-recs theirs-recs
                                          changes labels style favor marker-size))
      (setq conflicts 0 p changes)
      (while p
        (when (= (aref (car p) 0) 0) (setq conflicts (1+ conflicts)))
        (setq p (cdr p)))))
    (cons (if (multibyte-string-p result)
              (encode-coding-string result 'utf-8)
            result)
          conflicts)))

;;;; merge-file command (builtin/merge-file.c)

(defconst ngit-merge-file--usage
  (concat
   "usage: git merge-file [<options>] [-L <name1> [-L <orig> [-L <name2>]]] <file1> <orig-file> <file2>\n\n"
   "    -p, --[no-]stdout     send results to standard output\n"
   "    --[no-]object-id      use object IDs instead of filenames\n"
   "    --[no-]diff3          use a diff3 based merge\n"
   "    --[no-]zdiff3         use a zealous diff3 based merge\n"
   "    --[no-]ours           for conflicts, use our version\n"
   "    --[no-]theirs         for conflicts, use their version\n"
   "    --[no-]union          for conflicts, use a union version\n"
   "    --diff-algorithm <algorithm>\n"
   "                          choose a diff algorithm\n"
   "    --[no-]marker-size <n>\n"
   "                          for conflicts, use this marker size\n"
   "    -q, --[no-]quiet      do not warn about conflicts\n"
   "    -L <name>             set labels for file1/orig-file/file2\n\n"))

(defun ngit-merge-file--usage-error (&optional message)
  "Raise a parse-options usage error, optionally preceded by MESSAGE."
  (ngit--die-with 129 "%s%s" (if message (concat "error: " message "\n") "")
                  (substring ngit-merge-file--usage 0 -1)))

(defun ngit-merge-file--errno (errno)
  "Return the C locale message for a file operation's ERRNO."
  (or (cdr (assq errno '((1 . "Operation not permitted")
                         (2 . "No such file or directory")
                         (5 . "Input/output error") (9 . "Bad file descriptor")
                         (13 . "Permission denied") (20 . "Not a directory")
                         (21 . "Is a directory") (22 . "Invalid argument")
                         (24 . "Too many open files") (27 . "File too large")
                         (28 . "No space left on device")
                         (30 . "Read-only file system")
                         (36 . "File name too long")
                         (40 . "Too many levels of symbolic links"))))
      (format "Unknown error %d" errno)))

(defun ngit-merge-file--filename (path)
  "Resolve PATH against the Lisp cwd without simplifying symlink/.. paths."
  (if (or (string= path "") (= (aref path 0) ?/)) path
    (concat default-directory path)))

(defun ngit-merge-file--path (path)
  "Return a native NUL-terminated PATH, using the shared path buffer."
  (let* ((bytes (encode-coding-string (ngit-merge-file--filename path) 'utf-8))
         (addr (nl-ffi-memory-address
                (ngit-native--buffer 3 (1+ (length bytes))))))
    (ptr-write-bytes addr bytes)
    (ptr-write-u8 addr (length bytes) 0)
    addr))

(defun ngit-merge-file--read (path)
  "Read PATH as read_mmfile does, with its Git error messages."
  (let ((addr (ngit-merge-file--path path)) size mode)
    (ngit-native--with-memory (statbuf 144)
      (let* ((stat (nl-ffi-memory-address statbuf))
             (rc (if (ngit-windows-p)
                     (if (ngit-win-stat (ngit-merge-file--filename path) stat) 0 -2)
                   (syscall-direct 4 addr stat 0 0 0 0))))
        (when (< rc 0)
          (ngit--die-with 255 "error: Could not stat %s: %s"
                          path (ngit-merge-file--errno (- rc))))
        (setq size (ptr-read-u64 stat 48) mode (ptr-read-u32 stat 24))))
    (when (= (logand mode #o170000) #o040000)
      (ngit--die-with 255 "error: Could not open %s: Is a directory" path))
    (unless (ngit-native-try-open (ngit-merge-file--filename path))
      (ngit--die-with 255 "error: Could not open %s: Permission denied" path))
    (when (> size (* 1024 1024 1023))
      (ngit--die-with 255 "error: Cannot merge binary files: %s" path))
    (let ((text (condition-case nil
                    (ngit-native-read-bytes (ngit-merge-file--filename path) 0 size)
                  (error (ngit--die-with 255 "error: Could not read %s" path)))))
      (unless (= (length text) size)
        (ngit--die-with 255 "error: Could not read %s" path))
      (when (string-search "\0" (if (> size 8000) (substring text 0 8000) text))
        (ngit--die-with 255 "error: Cannot merge binary files: %s" path))
      text)))

(defun ngit-merge-file--write (path text &optional stdout)
  "Write TEXT to PATH or STDOUT, checking write and close errors."
  (let* ((fd (if stdout 1
               (ngit-native-fd-open (ngit-merge-file--filename path) #o1101 #o666)))
         (size (length text)))
    (when (< fd 0)
      (ngit--die-with 255 "error: Could not open %s for writing: %s"
                      path (ngit-merge-file--errno (- fd))))
    (unwind-protect
        (when (> (length text) 0)
          (let ((buf (nl-ffi-memory-address (ngit-native--buffer 0 (length text))))
                (offset 0) rc)
            (ptr-write-bytes buf text)
            (while (< offset (length text))
              (setq rc (ngit-native-fd-write fd (+ buf offset)
                                            (- (length text) offset)))
              (when (<= rc 0)
                ;; Git's stdio buffers short writes until fclose.
                (if (< size 4096)
                    (ngit--die-with 255 "error: Could not close %s: %s"
                                    path (ngit-merge-file--errno (if (= rc 0) 5 (- rc))))
                  (ngit--die-with 255 "error: Could not write to %s: %s"
                                  path (ngit-merge-file--errno (if (= rc 0) 5 (- rc))))))
              (setq offset (+ offset rc)))))
      (let ((rc (ngit-native-fd-close fd)))
        (when (< rc 0)
          (ngit--die-with 255 "error: Could not close %s: %s"
                          path (ngit-merge-file--errno (- rc))))))))

;;;###autoload
(defun ngit-cmd-merge-file (repo args)
  "Run merge-file with ARGS; REPO is optional for this standalone command."
  (let ((style (and repo (ngit-config-get repo "merge.conflictstyle")))
        (favor 'none) (marker 7) (labels nil) (paths nil)
        (stdout nil) (quiet nil) (end-options nil))
    (when (and style (not (member style '("merge" "diff3" "zdiff3"))))
      (ngit--die "error: unknown style '%s' given for 'merge.conflictstyle'" style))
    (setq style (if style (intern style) 'merge))
    (while args
      (let ((arg (pop args)))
        (cond
         (end-options (push arg paths))
         ((string= arg "--") (setq end-options t))
         ((string= arg "-h")
          (ngit--emit ngit-merge-file--usage) (ngit--flush)
          (ngit--die-with 129 "%s" ""))
         ((string-prefix-p "--" arg)
          (let* ((equal-pos (string-search "=" arg))
                 (name (substring arg 2 equal-pos))
                 (value (and equal-pos (substring arg (1+ equal-pos))))
                 (neg (string-prefix-p "no-" name))
                 (key (if neg (substring name 3) name))
                 (options '("stdout" "diff3" "zdiff3" "ours" "theirs"
                            "union" "marker-size" "quiet"))
                 (matches nil))
            (if (member key options)
                (setq matches (list key))
              (dolist (option (append options (if neg '("object-id")
                                               '("object-id" "diff-algorithm"))))
                (when (string-prefix-p key option) (push option matches))))
            (when (> (length matches) 1)
              ;; parse-options sends the usage for ambiguity to stdout.
              (setq matches (nreverse matches))
              (when (member "object-id" matches)
                (setq matches '("object-id" "ours")))
              (ngit-native-write-stderr
               (format "error: ambiguous option: %s (could be --%s or --%s)\n"
                       name (concat (if neg "no-" "") (nth 0 matches))
                       (concat (if neg "no-" "") (nth 1 matches))))
              (ngit--emit ngit-merge-file--usage)
              (ngit--flush)
              (ngit--die-with 129 "%s" ""))
            (unless (= (length matches) 1)
              (ngit-merge-file--usage-error (format "unknown option `%s'" (substring arg 2))))
            (setq key (car matches))
            (when (member key '("object-id" "diff-algorithm"))
              (ngit-merge-file--usage-error (format "unknown option `%s'" (substring arg 2))))
            (setq name (concat (if neg "no-" "") key))
            (when (and value (or neg (not (string= key "marker-size"))))
              (ngit--die-with 129 "error: option `%s' takes no value" name))
            (cond
             ((string= key "marker-size")
              (if neg
                  (setq marker 0)
                (unless value
                  (unless args
                    (ngit--die-with 129 "error: option `%s' requires a value" name))
                  (setq value (pop args)))
                (unless (string-match "\\`[ \t\n\r\v\f]*[+-]?[0-9]+\\'" value)
                  (ngit--die-with 129 "error: option `%s' expects a numerical value" name))
                ;; parse-options assigns a C int, including wraparound.
                (setq marker (logand (string-to-number value) #xffffffff))
                (when (>= marker #x80000000) (setq marker (- marker #x100000000)))))
             ((string= key "stdout") (setq stdout (not neg)))
             ((string= key "quiet") (setq quiet (not neg)))
             ((member key '("diff3" "zdiff3"))
              (setq style (if neg 'merge (intern key))))
             (t (setq favor (if neg 'none (intern key)))))))
         ((and (> (length arg) 1) (= (aref arg 0) ?-))
          (let ((i 1) (n (length arg)))
            (while (< i n)
              (let ((ch (aref arg i)))
                (cond
                 ((= ch ?p) (setq stdout t))
                 ((= ch ?q) (setq quiet t))
                 ((= ch ?L)
                  (let ((label (if (< (1+ i) n)
                                   (substring arg (1+ i))
                                 (or (pop args)
                                     (ngit--die-with 129 "error: switch `L' requires a value")))))
                    (when (= (length labels) 3)
                      (ngit--die-with 129 "error: too many labels on the command line"))
                    (push label labels)
                    (setq i n)))
                 (t (ngit-merge-file--usage-error (format "unknown switch `%c'" ch)))))
              (setq i (1+ i)))))
         (t (push arg paths)))))
    (setq paths (nreverse paths) labels (nreverse labels))
    (unless (= (length paths) 3) (ngit-merge-file--usage-error))
    (condition-case err
        (let* ((ours (ngit-merge-file--read (nth 0 paths)))
               (base (ngit-merge-file--read (nth 1 paths)))
               (theirs (ngit-merge-file--read (nth 2 paths)))
               (result (ngit-merge-file
                        base ours theirs
                        :labels (list (or (nth 1 labels) (nth 1 paths))
                                      (or (nth 0 labels) (nth 0 paths))
                                      (or (nth 2 labels) (nth 2 paths)))
                        :style style :favor favor :marker-size marker)))
          (ngit-merge-file--write (car paths) (car result) stdout)
          (min 127 (cdr result)))
      (ngit-fatal
       (if quiet (or (nth 2 err) 255)
         (signal (car err) (cdr err)))))))

(provide 'ngit-merge-file)

;;; ngit-merge-file.el ends here
