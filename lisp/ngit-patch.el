;;; ngit-patch.el --- Mail patches, apply and am -*- lexical-binding: t; -*-

;;; Commentary:

;; Git-compatible format-patch, a deliberately strict unified patch
;; applicator, and the am mailbox driver.  Applying accepts displaced hunks,
;; but never drops context lines (Git's fuzz); that is the useful boundary for
;; generated mail patches and keeps a failed check free of partial writes.

;;; Code:

(require 'ngit-sequencer)

(defconst ngit-patch--version "2.47.3")

(defun ngit-patch--subject (message)
  (let ((eol (or (ngit--byte-pos message ?\n 0) (length message))))
    (substring message 0 eol)))

(defun ngit-patch--body (message)
  (let ((eol (ngit--byte-pos message ?\n 0)))
    (if (not eol) ""
      (let ((body (substring message (1+ eol))))
        (while (string-prefix-p "\n" body) (setq body (substring body 1)))
        body))))

(defun ngit-patch--subject-prefix (subject number total numbered)
  (concat "Subject: [PATCH"
          (if numbered
              (format (concat " %0" (number-to-string
                                      (length (number-to-string total)))
                              "d/%d")
                      number total)
            "")
          "] " subject "\n"))

(defun ngit-patch--mail (repo hex number total numbered)
  (let* ((c (ngit-seq-commit-object repo hex))
         (parents (ngit--c-parents c))
         (old (and parents (ngit-seq-tree repo (ngit--c-hex (car parents)))))
         (new (ngit-unhex (ngit--c-tree c)))
         (changes (ngit-diffcore-rename repo (ngit-diff-trees repo old new)))
         (author (ngit--split-ident (ngit--c-author c)))
         (message (ngit--c-message c))
         (subject (ngit-patch--subject message))
         (body (ngit-patch--body message))
         (ngit--out nil))
    (ngit--emit "From " hex " Mon Sep 17 00:00:00 2001\n"
                "From: " (car author) "\n"
                "Date: " (ngit-show-date (nth 1 author) (nth 2 author) '(rfc)) "\n"
                (ngit-patch--subject-prefix subject number total numbered) "\n")
    (unless (string= body "")
      (ngit--emit (string-trim-right body) "\n\n"))
    (ngit--emit "---\n")
    (let ((entries (ngit--stat-entries repo changes)))
      (ngit--emit-stat entries)
      (ngit--emit-summary changes))
    (ngit--emit "\n")
    (dolist (change changes) (ngit--emit-patch repo change 3))
    (ngit--emit "-- \n" ngit-patch--version "\n\n")
    (apply #'concat (nreverse ngit--out))))

(defun ngit-patch--filename-subject (subject)
  (ngit--sanitized-subject subject))

(defun ngit-patch--last-commits (repo revision count)
  "Return at most COUNT first-parent commits ending at REVISION, oldest first."
  (let ((hex (ngit-seq-resolve repo revision)) (out nil))
    (while (and hex (> count 0))
      (push hex out)
      (setq count (1- count))
      (let ((parents (ngit--c-parents (ngit-seq-commit-object repo hex))))
        (setq hex (and parents (ngit--c-hex (car parents))))))
    out))

(defun ngit-patch--commits (repo revisions &optional max-count)
  (let* ((simple (and (= (length revisions) 1)
                      (not (string-search ".." (car revisions)))
                      (not (string-prefix-p "^" (car revisions)))))
         (direct (and max-count (or (null revisions) simple))))
    (if direct
        (ngit-patch--last-commits repo (or (car revisions) "HEAD") max-count)
      (let ((revs revisions))
        (cond ((null revs) (setq revs '("HEAD")))
              (simple (setq revs (list (concat (car revs) "..HEAD")))))
        (let ((commits (ngit-seq-commits repo revs nil)))
          (if (and max-count (> (length commits) max-count))
              (last commits max-count) commits))))))

;;;###autoload
(defun ngit-cmd-format-patch (repo args)
  (let ((stdout nil) (numbered nil) (outdir nil) (revisions nil) (max-count nil))
    (while args
      (let ((arg (pop args)))
        (cond ((string= arg "--stdout") (setq stdout t))
              ((member arg '("-n" "--numbered")) (setq numbered t))
              ((member arg '("-N" "--no-numbered")) (setq numbered 'never))
              ((string-match "\\`-\\([0-9]+\\)\\'" arg)
               (setq max-count (string-to-number (match-string 1 arg))))
              ((member arg '("-o" "--output-directory")) (setq outdir (pop args)))
              ((string-prefix-p "--output-directory=" arg) (setq outdir (substring arg 19)))
              ((string-prefix-p "-" arg) (ngit--die "ngit: format-patch: unsupported argument %s" arg))
              (t (push arg revisions)))))
    (when (and stdout outdir)
      (ngit--die "fatal: --stdout and --output-directory are mutually exclusive"))
    (let* ((commits (ngit-patch--commits repo (nreverse revisions) max-count))
           (total (length commits)) (num 0)
           (numbered (and (not (eq numbered 'never)) (or numbered (> total 1))))
           (width (max 4 (length (number-to-string total))))
           (directory (file-name-as-directory (expand-file-name (or outdir default-directory)))))
      (unless stdout (ngit-make-directory directory t))
      (dolist (hex commits)
        (setq num (1+ num))
        (let* ((c (ngit-seq-commit-object repo hex))
               (mail (ngit-patch--mail repo hex num total numbered))
               (name (concat (format (concat "%0" (number-to-string width) "d-") num)
                             (ngit-patch--filename-subject (ngit-patch--subject (ngit--c-message c)))
                             ".patch")))
          (if stdout (ngit--emit mail (if (< num total) "\n" ""))
            (let ((path (expand-file-name (decode-coding-string name 'utf-8) directory)))
              (ngit-write-file path mail)
              (ngit--emit (if outdir
                              (if (file-name-absolute-p outdir) path
                                (concat (file-name-as-directory outdir) name))
                            name)
                          "\n")))))
      0)))

;;;; Unified diff parser

(defun ngit-patch--strip-line (line)
  (if (and (> (length line) 0) (= (aref line (1- (length line))) ?\n))
      (substring line 0 -1) line))

(defun ngit-patch--path (line prefix)
  (let* ((text (substring (ngit-patch--strip-line line) (length prefix)))
         (tab (ngit--byte-pos text ?\t 0)))
    (setq text (if tab (substring text 0 tab) text))
    (cond ((string= text "/dev/null") nil)
          ((or (string-prefix-p "a/" text) (string-prefix-p "b/" text)) (substring text 2))
          (t text))))

(defun ngit-patch--hunk-header (line)
  (when (string-match "\\`@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@" line)
    (vector (string-to-number (match-string 1 line))
            (string-to-number (or (match-string 2 line) "1"))
            (string-to-number (match-string 3 line))
            (string-to-number (or (match-string 4 line) "1")) nil 0 0)))

(defun ngit-patch--parse (data)
  "Parse DATA into patch plists, ignoring mail and diffstat headers."
  (let* ((lines (ngit-xdiff--split data)) (n (length lines)) (i 0)
         (patches nil) current hunk)
    (while (< i n)
      (let ((line (aref lines i)))
        (cond
         ((string-prefix-p "diff --git " line)
          (when current
            (when hunk (plist-put current :hunks (append (plist-get current :hunks) (list hunk))))
            (push current patches))
          (let* ((words (split-string (ngit-patch--strip-line line) " " t))
                 (a (nth 2 words)) (b (nth 3 words)))
            (setq current (list :hunks nil :old-mode #o100644 :new-mode #o100644
                                :mode-known nil
                                :old (and a (if (string-prefix-p "a/" a) (substring a 2) a))
                                :new (and b (if (string-prefix-p "b/" b) (substring b 2) b)))
                  hunk nil)))
         ((and current (string-prefix-p "index " line))
          (let* ((text (ngit-patch--strip-line line))
                 (space (string-search " " text 6)))
            (when space
              (let ((mode (substring text (1+ space))))
                (when (string-match-p "\\`[0-7]+\\'" mode)
                  (setq current (plist-put current :old-mode (string-to-number mode 8)))
                  (setq current (plist-put current :new-mode (string-to-number mode 8)))
                  (setq current (plist-put current :mode-known t)))))))
         ((and current (string-prefix-p "new file mode " line))
          (setq current (plist-put current :mode-known t))
          (setq current (plist-put current :old-mode 0))
          (setq current (plist-put current :new-mode (string-to-number (substring line 14) 8))))
         ((and current (string-prefix-p "deleted file mode " line))
          (setq current (plist-put current :mode-known t))
          (setq current (plist-put current :old-mode (string-to-number (substring line 18) 8)))
          (setq current (plist-put current :new-mode 0)))
         ((and current (string-prefix-p "old mode " line))
          (setq current (plist-put current :mode-known t))
          (setq current (plist-put current :old-mode (string-to-number (substring line 9) 8))))
         ((and current (string-prefix-p "new mode " line))
          (setq current (plist-put current :mode-known t))
          (setq current (plist-put current :new-mode (string-to-number (substring line 9) 8))))
         ((and current (string-prefix-p "rename from " line))
          (setq current (plist-put current :old
                                   (substring (ngit-patch--strip-line line) 12))))
         ((and current (string-prefix-p "rename to " line))
          (setq current (plist-put current :new
                                   (substring (ngit-patch--strip-line line) 10))))
         ((string-prefix-p "--- " line)
          (unless current
            (setq current (list :hunks nil :old-mode #o100644 :new-mode #o100644
                                :mode-known nil :old nil :new nil)))
          (setq current (plist-put current :old (ngit-patch--path line "--- "))))
         ((and current (string-prefix-p "+++ " line))
          (setq current (plist-put current :new (ngit-patch--path line "+++ "))))
         ((and current (string-prefix-p "@@ " line))
          (when hunk (plist-put current :hunks (append (plist-get current :hunks) (list hunk))))
          (setq hunk (ngit-patch--hunk-header line)))
         ((and hunk (string-prefix-p "\\ No newline at end of file" line))
          (let* ((body (aref hunk 4)) (last (car (last body))))
            (when (and last (string-suffix-p "\n" last))
              (setcar (last body) (substring last 0 -1)))))
         ((and hunk (> (length line) 0) (memq (aref line 0) '(?\s ?+ ?-)))
          (if (and (= (aref hunk 5) (aref hunk 1))
                   (= (aref hunk 6) (aref hunk 3)))
              (progn
                (plist-put current :hunks (append (plist-get current :hunks) (list hunk)))
                (setq hunk nil))
            (aset hunk 4 (append (aref hunk 4) (list line)))
            (unless (= (aref line 0) ?+) (aset hunk 5 (1+ (aref hunk 5))))
            (unless (= (aref line 0) ?-) (aset hunk 6 (1+ (aref hunk 6))))))))
      (setq i (1+ i)))
    (when current
      (when hunk (plist-put current :hunks (append (plist-get current :hunks) (list hunk))))
      (push current patches))
    (nreverse patches)))

(defun ngit-patch--source (repo patch cached reverse)
  (let* ((path (if reverse (plist-get patch :new) (plist-get patch :old)))
         (entry (and path (ngit-index-lookup (ngit-index-entries repo) path))))
    (cond
     ((null path) "")
     (cached
      (unless entry (ngit--die-with 1 "error: %s: does not exist in index" path))
      (cdr (ngit-read-object repo (aref entry 1))))
     (t
      (ngit-wt-setup (ngit-repo-toplevel repo))
      (let* ((file (expand-file-name (decode-coding-string path 'utf-8)
                                     (ngit-repo-toplevel repo)))
             (st (ngit-wt-lstat path)))
        (unless st (ngit--die-with 1 "error: %s: No such file or directory" path))
        (let* ((actual (logand (aref st 0) ngit-s-ifmt))
               (expected (if reverse (plist-get patch :new-mode)
                           (plist-get patch :old-mode))))
          (when (and (plist-get patch :mode-known)
                     (= actual ngit-s-iflnk)
                     (/= (logand expected #o170000) #o120000))
            (ngit--die-with 1 "error: %s: wrong type" path))
          (if (= actual ngit-s-iflnk)
              (encode-coding-string (file-symlink-p file) 'utf-8)
            (ngit--read-file file))))))))

(defun ngit-patch--line-data (line) (substring line 1))

(defun ngit-patch--match-at (source pos body reverse)
  (let ((at pos) (ok t))
    (dolist (line body)
      (let ((kind (aref line 0)))
        (when reverse (setq kind (cond ((= kind ?+) ?-) ((= kind ?-) ?+) (t kind))))
        (when (memq kind '(?\s ?-))
          (if (or (>= at (length source))
                  (not (string= (aref source at) (ngit-patch--line-data line))))
              (setq ok nil)
            (setq at (1+ at))))))
    ok))

(defun ngit-patch--find-hunk (source expected body reverse minimum)
  (let ((distance 0) found (n (length source)))
    (while (and (not found) (<= distance n))
      (dolist (pos (if (= distance 0) (list expected)
                     (list (- expected distance) (+ expected distance))))
        (when (and (not found) (>= pos minimum) (<= pos n)
                   (ngit-patch--match-at source pos body reverse))
          (setq found (cons pos (- pos expected)))))
      (setq distance (1+ distance)))
    found))

(defun ngit-patch--apply-one (source patch reverse)
  (let ((lines (ngit-xdiff--split source)) (out nil) (cursor 0)
        (hunks (plist-get patch :hunks)) (failed nil) (offsets nil))
    (dolist (h hunks)
      (unless failed
        (let* ((start (if reverse (aref h 2) (aref h 0)))
               (expected (if (= start 0) 0 (1- start)))
               (found (ngit-patch--find-hunk lines expected (aref h 4) reverse cursor)))
          (if (not found) (setq failed t)
            (let ((pos (car found)))
              (when (/= (cdr found) 0) (push (cons start (cdr found)) offsets))
              (while (< cursor pos) (push (aref lines cursor) out) (setq cursor (1+ cursor)))
              (dolist (line (aref h 4))
                (let ((kind (aref line 0)))
                  (when reverse (setq kind (cond ((= kind ?+) ?-) ((= kind ?-) ?+) (t kind))))
                  (cond ((= kind ?\s) (push (aref lines cursor) out) (setq cursor (1+ cursor)))
                        ((= kind ?-) (setq cursor (1+ cursor)))
                        ((= kind ?+) (push (ngit-patch--line-data line) out))))))))))
    (if failed nil
      (while (< cursor (length lines)) (push (aref lines cursor) out) (setq cursor (1+ cursor)))
      (cons (apply #'concat (nreverse out)) (nreverse offsets)))))

(defun ngit-patch--target-path (patch reverse)
  (if reverse (plist-get patch :old) (plist-get patch :new)))

(defun ngit-patch--target-mode (patch reverse)
  (if reverse (plist-get patch :old-mode) (plist-get patch :new-mode)))

(defun ngit-patch--safe-path (repo path)
  "Reject paths that escape the work tree or traverse a symlink."
  (when path
    (let ((parts (split-string path "/")))
      (when (or (string= path "") (= (aref path 0) ?/)
                (member "" parts) (member ".." parts)
                (seq-some
                 (lambda (part)
                   (string= (downcase (replace-regexp-in-string "[ .]+\\'" "" part)) ".git"))
                 parts))
        (ngit--die-with 128 "error: invalid path '%s'" path)))
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (let ((start 0) slash)
      (while (setq slash (ngit--byte-pos path ?/ start))
        (let ((st (ngit-wt-lstat (substring path 0 slash))))
          (when (and st (/= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir))
            (ngit--die-with 128 "error: affected file '%s' is beyond a symbolic link" path)))
        (setq start (1+ slash))))))

(defun ngit-patch--prepare (repo patches cached reverse index-check)
  (let ((prepared nil))
    (dolist (patch patches)
      (let* ((old-path (if reverse (plist-get patch :new) (plist-get patch :old)))
             (path (ngit-patch--target-path patch reverse))
             source result)
        (ngit-patch--safe-path repo old-path)
        (ngit-patch--safe-path repo path)
        (setq source (ngit-patch--source repo patch cached reverse)
              result (ngit-patch--apply-one source patch reverse))
        (when (and (null old-path) path
                   (or (ngit-index-lookup (ngit-index-entries repo) path)
                       (and (not cached)
                            (file-exists-p (expand-file-name (decode-coding-string path 'utf-8)
                                                            (ngit-repo-toplevel repo))))))
          (ngit--die-with 1 "error: %s: already exists in working directory" path))
        (when (and path old-path (not (string= path old-path)) (not cached)
                   (file-exists-p (expand-file-name (decode-coding-string path 'utf-8)
                                                   (ngit-repo-toplevel repo))))
          (ngit--die-with 1 "error: %s: already exists in working directory" path))
        (when index-check
          (let ((entry (and old-path (ngit-index-lookup (ngit-index-entries repo) old-path))))
            (unless (or (and (null old-path) (null entry))
                        (and entry (string= source (cdr (ngit-read-object repo (aref entry 1))))))
              (ngit--die-with 1 "error: %s: does not match index" old-path))))
        (unless result
          (ngit-native-write-stderr (format "error: patch failed: %s:%d\n"
                                            (or old-path path) (aref (car (plist-get patch :hunks))
                                                                     (if reverse 2 0))))
          (ngit--die-with 1 "error: %s: patch does not apply" (or old-path path)))
        (push (list :old old-path :path path :mode (ngit-patch--target-mode patch reverse)
                    :data (car result) :offsets (cdr result) :patch patch)
              prepared)))
    (nreverse prepared)))

(defun ngit-patch--write-worktree (repo prepared)
  (dolist (one prepared)
    (let* ((path (plist-get one :path)) (old (plist-get one :old))
           (file (and path (expand-file-name (decode-coding-string path 'utf-8)
                                             (ngit-repo-toplevel repo)))))
      (when (and old (not (equal old path)))
        (let ((old-file (expand-file-name (decode-coding-string old 'utf-8)
                                          (ngit-repo-toplevel repo))))
          (when (file-exists-p old-file) (delete-file old-file))))
      (if (or (null path) (= (plist-get one :mode) 0))
          (when file (delete-file file))
        (ngit-make-directory (file-name-directory file) t)
        (ngit-wt-setup (ngit-repo-toplevel repo))
        (let* ((st (ngit-wt-lstat path))
               (symlink (and st (= (logand (aref st 0) ngit-s-ifmt) ngit-s-iflnk)))
               (target-symlink (= (logand (plist-get one :mode) #o170000) #o120000))
               (existed (and st (not symlink)))
               (perm (ngit-native-creation-mode
                      (if (= (plist-get one :mode) #o100755) #o777 #o666))))
          (when st (when (or symlink target-symlink) (delete-file file)))
          (if target-symlink
              (make-symbolic-link (decode-coding-string (plist-get one :data) 'utf-8) file)
            (ngit-write-file file (plist-get one :data) nil nil (and (not existed) perm))
            (when (and existed (/= (logand (file-modes file) #o111)
                                    (logand perm #o111)))
              (set-file-modes file perm))))))))

(defun ngit-patch--write-index (repo prepared)
  (let* ((index (ngit-read-index repo)) (entries (aref index 0))
         (targets nil) (dropped (make-hash-table :test 'equal))
         (root (or (ngit-ct-root index) (ngit-ct-new))))
    (dolist (one prepared)
      (let ((old (plist-get one :old)) (path (plist-get one :path))
            (mode (plist-get one :mode)))
        (when old (puthash old t dropped) (ngit-ct-invalidate root old))
        (when path (ngit-ct-invalidate root path))
        (when (and path (/= mode 0))
          (push (ngit-make-entry path (ngit-write-object repo "blob" (plist-get one :data)) mode
                                 (concat (make-string 24 0) (ngit--be32-bytes mode)
                                         (make-string 12 0)))
                targets))))
    (setq entries (ngit-index-overlay entries (sort targets #'ngit--entry-less) dropped))
    (ngit-write-index repo index entries root)))

(defun ngit-patch--emit-offsets (prepared)
  (dolist (one prepared)
    (dolist (off (plist-get one :offsets))
      (ngit-native-write-stderr
       (format "Hunk #1 succeeded at %d (offset %d line%s).\n"
               (+ (car off) (cdr off)) (abs (cdr off)) (if (= (abs (cdr off)) 1) "" "s"))))))

(defun ngit-patch--stat (patches reverse)
  (let ((files 0) (adds 0) (dels 0) (entries nil)
        (max-len 0) (max-change 0))
    (dolist (patch patches)
      (let ((a 0) (d 0))
        (dolist (h (plist-get patch :hunks))
          (dolist (line (aref h 4))
            (cond ((= (aref line 0) ?+) (setq a (1+ a)))
                  ((= (aref line 0) ?-) (setq d (1+ d))))))
        (when reverse (let ((x a)) (setq a d d x)))
        (setq files (1+ files) adds (+ adds a) dels (+ dels d))
        (let* ((old (plist-get patch (if reverse :new :old)))
               (new (plist-get patch (if reverse :old :new)))
               (name (or new old))
               (quoted (ngit-quote-path name))
               (total (+ a d)))
          (dolist (candidate (delq nil (list old new)))
            (setq max-len (max max-len (length (ngit-quote-path candidate)))))
          (setq max-change (max max-change total))
          (push (list quoted a d total) entries))))
    (setq max-len (min max-len 50))
    (let ((max-bar (min max-change (- 70 max-len))))
      (dolist (entry (nreverse entries))
        (let* ((name (car entry))
               (a (nth 1 entry)) (d (nth 2 entry)) (total (nth 3 entry)))
          (when (> (length name) max-len)
            (let* ((start (+ (length name) 3 (- max-len)))
                   (slash (string-search "/" name start)))
              (setq name (concat "..." (substring name (or slash start))))))
          (let* ((scaled-total (if (> max-change 0)
                                   (/ (+ (* total max-bar) (/ max-change 2)) max-change)
                                 total))
                 (scaled-add (if (> max-change 0)
                                 (/ (+ (* a max-bar) (/ max-change 2)) max-change)
                               a))
                 (scaled-del (- scaled-total scaled-add)))
            (ngit--emit " " name (make-string (- max-len (length name)) ?\s) " |"
                        (format "%5d " total)
                        (make-string scaled-add ?+) (make-string scaled-del ?-) "\n")))))
    (ngit--emit (ngit--stat-summary-line files adds dels))))

(defun ngit-patch--read-inputs (args)
  (if args (mapconcat (lambda (f) (ngit--read-file (expand-file-name f))) args "\n")
    (let* ((owner (ngit-native--buffer 0 65536))
           (addr (nl-ffi-memory-address owner)) parts done)
      (while (not done)
        (let ((n (syscall-direct 0 0 addr 65536 0 0 0)))
          (cond ((< n 0) (error "ngit: stdin read failed (errno %d)" (- n)))
                ((= n 0) (setq done t))
                (t (push (ptr-read-bytes addr n) parts)))))
      (apply #'concat (nreverse parts)))))

(defun ngit-patch--warn-whitespace (data label reverse)
  (let ((line-no 0) (count 0) (details 0) (pending-blank nil)
        (marker (if reverse ?- ?+)) (header (if reverse "--- " "+++ ")))
    (dolist (line (append (ngit-xdiff--split data) (list "")))
      (setq line-no (1+ line-no))
      (when (and pending-blank
                 (not (and (> (length line) 0) (= (aref line 0) marker)
                           (string= (ngit-patch--strip-line (substring line 1)) ""))))
        (when (or (string= line "") (string-prefix-p "-- " line)
                  (string-prefix-p "diff --git " line))
          (setq count (1+ count))
          (when (< details 5)
            (setq details (1+ details))
            (ngit-native-write-stderr
             (concat label ":" (number-to-string pending-blank)
                     ": new blank line at EOF.\n+\n"))))
        (setq pending-blank nil))
      (when (and (> (length line) 0) (= (aref line 0) marker)
                 (not (string-prefix-p header line))
                 (not (string-prefix-p "-- " line)))
        (let* ((text (substring line 1))
               (text (if (string-suffix-p "\n" text) (substring text 0 -1) text)))
          (cond
           ((string= text "") (unless pending-blank (setq pending-blank line-no)))
           ((or (and (> (length text) 0)
                     (memq (aref text (1- (length text))) '(?\s ?\t)))
                (string-match-p "\\` +\t" text))
            (setq count (1+ count))
            (when (< details 5)
              (setq details (1+ details))
              (ngit-native-write-stderr
               (concat label ":" (number-to-string line-no) ": "
                       (if (string-match-p "\\` +\t" text)
                           "space before tab in indent." "trailing whitespace.")
                       "\n" text "\n"))))))))
    (when (> count 0)
      (when (> count details)
        (ngit-native-write-stderr
         (format "warning: squelched %d whitespace error%s\n"
                 (- count details) (if (= (- count details) 1) "" "s"))))
      (ngit-native-write-stderr
       (format "warning: %d line%s add%s whitespace errors.\n"
               count (if (= count 1) "" "s") (if (= count 1) "s" ""))))))

;;;###autoload
(defun ngit-cmd-apply (repo args)
  (let ((check nil) (cached nil) (index nil) (reverse nil) (stat nil) (files nil))
    (while args
      (let ((arg (pop args)))
        (cond ((string= arg "--check") (setq check t))
              ((string= arg "--cached") (setq cached t))
              ((string= arg "--index") (setq index t))
              ((member arg '("-R" "--reverse")) (setq reverse t))
              ((string= arg "--stat") (setq stat t))
              ((string= arg "--") (setq files (append files args) args nil))
              ((string-prefix-p "-" arg) (ngit--die "ngit: apply: unsupported argument %s" arg))
              (t (setq files (append files (list arg)))))))
    (let* ((data (ngit-patch--read-inputs files))
           (patches (ngit-patch--parse data)))
      (unless patches (ngit--die-with 128 "error: No valid patches in input"))
      (if stat (ngit-patch--stat patches reverse)
        (let ((prepared (ngit-patch--prepare repo patches cached reverse index)))
          (unless check
            (ngit-patch--warn-whitespace data
                                         (if (= (length files) 1) (car files) "<stdin>")
                                         reverse))
          (unless check
            (unless cached (ngit-patch--write-worktree repo prepared))
            (when (or cached index)
              (if index
                  (ngit-cmd-add repo (delete-dups
                                      (delq nil (apply #'append
                                                       (mapcar (lambda (one)
                                                                 (list (plist-get one :path)
                                                                       (plist-get one :old)))
                                                               prepared)))))
                (ngit-patch--write-index repo prepared))))))
      0)))

;;;; am

(defun ngit-patch--mail-header (mail name)
  (let ((pos (string-match (concat "\n" (regexp-quote name) ": ") (concat "\n" mail))))
    (when pos
      (let* ((start (+ pos (length name) 3))
             (end (or (ngit--byte-pos (concat "\n" mail) ?\n start) (length mail))))
        (substring (concat "\n" mail) start end)))))

(defun ngit-patch--mail-message (mail)
  (let* ((subject (or (ngit-patch--mail-header mail "Subject") ""))
         (subject (replace-regexp-in-string "\\`\\[PATCH[^]]*\\] *" "" subject))
         (sep (string-search "\n---\n" mail))
         (headers-end (string-search "\n\n" mail))
         (body (if (and sep headers-end (< (1+ headers-end) sep))
                   (substring mail (+ headers-end 2) sep) "")))
    (concat subject "\n" (if (string= body "") "" (concat "\n" (string-trim-right body) "\n")))))

(defun ngit-patch--mails (data)
  (let ((starts nil) (pos 0) (n (length data)) out)
    (when (string-prefix-p "From " data) (push 0 starts))
    (while (and (< pos n) (setq pos (string-search "\nFrom " data pos)))
      (let* ((start (1+ pos)) (eol (ngit--byte-pos data ?\n start))
             (line (and eol (substring data start eol))))
        (when (and line (= (length line) 70)
                   (string-suffix-p " Mon Sep 17 00:00:00 2001" line))
          (push start starts)))
      (setq pos (+ pos 6)))
    (setq starts (nreverse starts))
    (while starts
      (let ((start (pop starts)))
        (push (substring data start (or (car starts) n)) out)))
    (nreverse out)))

(defun ngit-patch--am-dir (repo) (expand-file-name "rebase-apply" (ngit-repo-gitdir repo)))

(defun ngit-patch--am-write (repo name data)
  (ngit-write-file (expand-file-name name (ngit-patch--am-dir repo)) data))

(defun ngit-patch--am-state (repo mails)
  (let ((dir (ngit-patch--am-dir repo)) (n 0))
    (ngit-make-directory dir t)
    (dolist (mail mails)
      (setq n (1+ n))
      (ngit-patch--am-write repo (format "%04d" n) mail))
    (ngit-patch--am-write repo "next" "1\n")
    (ngit-patch--am-write repo "last" (format "%d\n" n))
    (ngit-patch--am-write repo "abort-safety" (concat (ngit-resolve-ref repo "HEAD") "\n"))
    (ngit--merge-write repo "ORIG_HEAD" (concat (ngit-resolve-ref repo "HEAD") "\n"))
    (dolist (pair '(("apply-opt" . "") ("applying" . "") ("keep" . "f\n")
                    ("messageid" . "f\n") ("quiet" . "f\n")
                    ("quoted-cr" . "") ("scissors" . "") ("sign" . "f\n")
                    ("threeway" . "f\n") ("utf8" . "t\n")))
      (ngit-patch--am-write repo (car pair) (cdr pair)))))

(defvar ngit-patch--am-no-verify nil)

(defun ngit-patch--am-no-verify-p (_repo)
  ngit-patch--am-no-verify)

(defun ngit-patch--am-date-ident (repo mail)
  (let* ((from (or (ngit-patch--mail-header mail "From") ""))
         (date (or (ngit-patch--mail-header mail "Date") ""))
         (words (split-string date "[ ,:]+" t))
         (day (string-to-number (or (nth 1 words) "1")))
         (month (1+ (or (seq-position (append ngit--months nil) (nth 2 words)
                                      #'string=) 0)))
         (year (string-to-number (or (nth 3 words) "1970")))
         (hour (string-to-number (or (nth 4 words) "0")))
         (minute (string-to-number (or (nth 5 words) "0")))
         (second (string-to-number (or (nth 6 words) "0")))
         (tz (or (nth 7 words) "+0000"))
         (sign (if (string-prefix-p "-" tz) -1 1))
         (off (* sign (+ (* 3600 (string-to-number (substring tz 1 3)))
                         (* 60 (string-to-number (substring tz 3 5))))))
         (stamp (- (+ (* 86400 (ngit--days-from-civil year month day))
                      (* hour 3600) (* minute 60) second) off)))
    (ignore repo)
    (concat from " " (number-to-string stamp) " " tz)))

(defun ngit-patch--am-current (repo)
  (let* ((next (string-to-number (ngit--read-file (expand-file-name "next" (ngit-patch--am-dir repo)))))
         (last (string-to-number (ngit--read-file (expand-file-name "last" (ngit-patch--am-dir repo))))))
    (and (<= next last) (cons next (ngit--read-file (expand-file-name (format "%04d" next)
                                                                      (ngit-patch--am-dir repo)))))))

(defun ngit-patch--am-write-author-state (repo mail subject)
  "Write the current mail's author metadata before invoking hooks."
  (let* ((from (or (ngit-patch--mail-header mail "From") ""))
         (date (or (ngit-patch--mail-header mail "Date") ""))
         (open (string-search " <" from))
         (close (string-search ">" from))
         (name (if open (substring from 0 open) from))
         (email (if (and open close) (substring from (+ open 2) close) "")))
    (ngit-patch--am-write
     repo "author-script"
     (concat "GIT_AUTHOR_NAME=" (ngit-seq-sq-quote name)
             "\nGIT_AUTHOR_EMAIL=" (ngit-seq-sq-quote email)
             "\nGIT_AUTHOR_DATE=" (ngit-seq-sq-quote date) "\n"))
    (ngit-patch--am-write
     repo "info" (concat "Author: " name "\nEmail: " email "\nSubject: " subject
                         "\nDate: " date "\n\n"))))

(defun ngit-patch--am-run (repo)
  (let ((status 0) current)
    (ngit-patch--am-write repo "msg" "")
    (while (and (= status 0) (setq current (ngit-patch--am-current repo)))
      (let* ((number (car current)) (mail (cdr current))
             (message (ngit-patch--mail-message mail))
             (message-file (expand-file-name "final-commit" (ngit-patch--am-dir repo)))
             (subject (ngit-patch--subject message))
             (patches (ngit-patch--parse mail)))
        (ngit-patch--am-write repo "final-commit" message)
        (ngit-patch--am-write-author-state repo mail subject)
        (ngit-patch--am-write repo "patch"
                              (substring mail (or (string-search "---\n" mail) 0)))
        (unless (ngit-patch--am-no-verify-p repo)
          (when (/= (ngit-hook-run repo "applypatch-msg"
                                   (list (ngit-hook-path-arg repo message-file))) 0)
            (ngit--die-with 1 ""))
          (setq message (ngit--read-file message-file)
                subject (ngit-patch--subject message)))
        (ngit--emit "Applying: " subject "\n")
        (condition-case err
            (let ((prepared (ngit-patch--prepare repo patches nil nil t)))
              (ngit-patch--write-worktree repo prepared)
              (ngit-cmd-add repo
                            (delete-dups
                             (delq nil (apply #'append
                                              (mapcar (lambda (one)
                                                        (list (plist-get one :path)
                                                              (plist-get one :old)))
                                                      prepared)))))
              ;; When pre-applypatch runs, git-am keeps its in-memory index
              ;; across the hook.  A hook may change the index file, but
              ;; write_index_as_tree writes the already loaded index back.
              (let* ((index (ngit-read-index repo))
                     (root (or (ngit-ct-root index) (ngit-ct-new)))
                     (tree (ngit-ct-update repo root (aref index 0)))
                     (hook-ran (and (not (ngit-patch--am-no-verify-p repo))
                                    (ngit-hook-exists-p repo "pre-applypatch")))
                     (hook-status (if hook-ran
                                      (ngit-hook-run repo "pre-applypatch")
                                    0)))
                (if (/= hook-status 0)
                    (setq status 1)
                  (when hook-ran
                    (ngit-write-index repo index (aref index 0) root))
                  (let* ((head (ngit-resolve-ref repo "HEAD"))
                         (author (ngit-patch--am-date-ident repo mail)))
                    (let ((ngit-commit-replay-action "am"))
                      (ngit--finish-commit repo head (list head) tree message nil t nil
                                           (ngit-index-file repo) (ngit-ident repo "COMMITTER") author
                                           nil t nil t)))
                  (ngit-hook-run repo "post-applypatch")
                  (ngit-ref-delete-state repo "REBASE_HEAD")
                  (ngit-patch--am-write repo "abort-safety"
                                        (concat (ngit-resolve-ref repo "HEAD") "\n"))
                  (ngit-patch--am-write repo "next" (format "%d\n" (1+ number))))))
          (error
           (when (and (eq (car err) 'ngit-fatal)
                      (equal (cadr err) "fatal: ref updates aborted by hook"))
             (signal (car err) (cdr err)))
           (setq status 128)
           (when (and (cdr err) (stringp (cadr err)))
             (ngit-native-write-stderr (concat (cadr err) "\n")))
           (ngit-patch--am-write repo "final-commit" message)
           (ngit--emit (format "Patch failed at %04d %s\n" number subject))
           (ngit-native-write-stderr
            (concat "hint: Use 'git am --show-current-patch=diff' to see the failed patch\n"
                    "hint: When you have resolved this problem, run \"git am --continue\".\n"
                    "hint: If you prefer to skip this patch, run \"git am --skip\" instead.\n"
                    "hint: To restore the original branch and stop patching, run \"git am --abort\".\n"
                    "hint: Disable this message with \"git config advice.mergeConflict false\"\n"))))))
    (when (and (= status 0) (not (ngit-patch--am-current repo)))
      (ngit--switch-delete-directory-tree (ngit-patch--am-dir repo)))
    status))

(defun ngit-patch--am-clear-branch-state (repo)
  "Clear branch state in the order used by git-am's clean_index."
  (ngit-commit-clear-merge-state repo)
  ;; remove_branch_state deletes AUTO_MERGE in both sequencer cleanup and
  ;; merge-state cleanup, even when the reference is already absent.
  (ngit-ref-delete-state repo "AUTO_MERGE"))

(defun ngit-patch--am-control (repo control)
  (unless (file-directory-p (ngit-patch--am-dir repo))
    (ngit--die "fatal: Resolve operation not in progress, we are not resuming."))
  (cond
   ((string= control "--abort")
    (let* ((dir (ngit-patch--am-dir repo))
           (dirty (file-exists-p (expand-file-name "dirtyindex" dir)))
           (current (ngit-resolve-ref repo "HEAD"))
           (safety (string-trim-right
                    (ngit--read-file (expand-file-name "abort-safety" dir)))))
      (cond
       (dirty nil)
       ((not (equal current safety))
        (ngit-native-write-stderr
         (concat "warning: You seem to have moved HEAD since the last 'am' failure.\n"
                 "Not rewinding to ORIG_HEAD\n")))
       (t
        (let ((target (string-trim-right
                       (ngit--read-file (expand-file-name "ORIG_HEAD"
                                                         (ngit-repo-gitdir repo))))))
          (ngit--reset-hard repo (ngit-seq-tree repo target) target)
          (ngit-patch--am-clear-branch-state repo)
          (ngit-update-head repo target current (ngit-ident repo "COMMITTER")
                            "am --abort")))))
    (ngit--switch-delete-directory-tree (ngit-patch--am-dir repo)) 0)
   ((string= control "--skip")
    (let* ((cur (ngit-patch--am-current repo)) (head (ngit-resolve-ref repo "HEAD")))
      (ngit--reset-hard repo (ngit-seq-tree repo head) head)
      (ngit-patch--am-clear-branch-state repo)
      (ngit-ref-delete-state repo "REBASE_HEAD")
      (ngit-patch--am-write repo "next" (format "%d\n" (1+ (car cur))))
      (ngit-patch--am-run repo)))
   (t
    (ngit--commit-check-unmerged repo (ngit-index-entries repo))
    (let* ((cur (ngit-patch--am-current repo)) (mail (cdr cur))
           (message (or (ngit--read-file
                         (expand-file-name "final-commit" (ngit-patch--am-dir repo)))
                        (ngit-patch--mail-message mail)))
           (index (ngit-read-index repo))
           (root (or (ngit-ct-root index) (ngit-ct-new)))
           (tree (ngit-ct-update repo root (aref index 0)))
           (head (ngit-resolve-ref repo "HEAD")))
      (ngit--emit "Applying: " (ngit-patch--subject message) "\n")
      (if (equal tree (ngit-seq-tree repo head))
          (progn
            (ngit--emit
             (concat "No changes - did you forget to use 'git add'?\n"
                     "If there is nothing left to stage, chances are that something else\n"
                     "already introduced the same changes; you might want to skip this patch.\n"))
            (ngit-native-write-stderr
             (concat "hint: When you have resolved this problem, run \"git am --continue\".\n"
                     "hint: If you prefer to skip this patch, run \"git am --skip\" instead.\n"
                     "hint: To restore the original branch and stop patching, run \"git am --abort\".\n"
                     "hint: Disable this message with \"git config advice.mergeConflict false\"\n"))
            128)
        (let ((ngit-commit-replay-action "am"))
          (unless (ngit-patch--am-no-verify-p repo)
            (when (/= (ngit-hook-run repo "pre-applypatch") 0)
              (ngit--die-with 1 "")))
          (ngit-write-index repo index (aref index 0) root)
          (ngit--finish-commit repo head (list head) tree message nil t nil (ngit-index-file repo)
                               (ngit-ident repo "COMMITTER") (ngit-patch--am-date-ident repo mail)
                               nil t nil t))
        (ngit-hook-run repo "post-applypatch")
        (ngit-ref-delete-state repo "REBASE_HEAD")
        (ngit-patch--am-write repo "next" (format "%d\n" (1+ (car cur))))
        (ngit-patch--am-run repo))))))

;;;###autoload
(defun ngit-cmd-am (repo args)
  (let ((ngit-patch--am-no-verify
         (and (seq-some (lambda (arg) (member arg '("-n" "--no-verify"))) args) t)))
    (setq args (seq-remove (lambda (arg) (member arg '("-n" "--no-verify"))) args))
  (if (and (= (length args) 1) (member (car args) '("--abort" "--continue" "--skip")))
      (ngit-patch--am-control repo (car args))
    (let* ((data (ngit-patch--read-inputs args)) (mails (ngit-patch--mails data)))
      (unless mails (ngit--die-with 128 "Patch format detection failed."))
      (when (file-directory-p (ngit-patch--am-dir repo))
        (ngit--die "fatal: previous rebase directory .git/rebase-apply still exists but mbox given."))
      (let* ((head (ngit-resolve-ref repo "HEAD"))
             (dirty (ngit-diff-tree-index repo (ngit-seq-tree repo head))))
        (ngit-ref-delete-state repo "REBASE_HEAD")
        (ngit-patch--am-state repo mails)
        (when dirty
          (ngit-patch--am-write repo "dirtyindex" "t\n")
          (ngit--die-with 128 "fatal: Dirty index: cannot apply patches (dirty: %s)"
                          (aref (car dirty) 0))))
      (ngit-patch--am-run repo)))))

(provide 'ngit-patch)

;;; ngit-patch.el ends here
