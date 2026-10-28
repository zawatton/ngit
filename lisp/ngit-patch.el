;;; ngit-patch.el --- Mail patches, apply and am -*- lexical-binding: t; -*-

;;; Commentary:

;; Git-compatible format-patch, a deliberately strict unified patch
;; applicator, and the am mailbox driver.  Applying accepts displaced hunks,
;; but never drops context lines (Git's fuzz); that is the useful boundary for
;; generated mail patches and keeps a failed check free of partial writes.

;;; Code:

(require 'ngit-sequencer)

(defconst ngit-patch--version "2.47.3")

(defun ngit-patch--binary (repo old new)
  "Load the literal binary patch encoder only for a selected binary pair."
  (require 'ngit-binary-patch)
  (ngit-binary-patch-emit old new (ngit--loose-level repo)))

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

(defun ngit-patch--mail (repo hex number total numbered &optional items)
  (let* ((c (ngit-seq-commit-object repo hex))
         (parents (ngit--c-parents c))
         (old (and parents (ngit-seq-tree repo (ngit--c-hex (car parents)))))
         (new (ngit-unhex (ngit--c-tree c)))
         (changes (ngit-diffcore-rename
                   repo (ngit-paths-filter-changes items (ngit-diff-trees repo old new))))
         (author (ngit--split-ident (ngit--c-author c)))
         (message (ngit--c-message c))
         (subject (ngit-patch--subject message))
         (body (ngit-patch--body message))
         (ngit--out nil)
         (ngit-diff--binary-patch-function
          (lambda (old new) (ngit-patch--binary repo old new))))
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
  (let ((stdout nil) (numbered nil) (outdir nil) (revisions nil) (max-count nil)
        (paths nil))
    (while args
      (let ((arg (pop args)))
        (cond ((string= arg "--stdout") (setq stdout t))
              ((string= arg "--") (setq paths args args nil))
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
    (setq revisions (nreverse revisions))
    (let* ((items (and paths (ngit-pathspec-items
                             repo (mapcar (lambda (path)
                                            (if (string-prefix-p ":" path) path
                                              (concat ":()" path))) paths))))
           (commits
            (if (not paths) (ngit-patch--commits repo revisions max-count)
              ;; Share revision.c's path pruning with log/rev-list, including
              ;; worktree attribute lookup when walking historical trees.
              (require 'ngit-describe)
              (let ((revs (or revisions '("HEAD"))))
                (when (and (not max-count) (= (length revs) 1)
                           (not (string-search ".." (car revs)))
                           (not (string-prefix-p "^" (car revs))))
                  (setq revs (list (concat (car revs) "..HEAD"))))
                (mapcar (lambda (commit) (ngit--c-hex commit))
                        (ngit-inspect--walk
                         repo (append '("--reverse" "--no-merges")
                                      (and max-count (list (format "--max-count=%d" max-count)))
                                      revs (list "--") paths))))))
           (total (length commits)) (num 0)
           (numbered (and (not (eq numbered 'never)) (or numbered (> total 1))))
           (width (max 4 (length (number-to-string total))))
           (directory (file-name-as-directory (expand-file-name (or outdir default-directory)))))
      (unless stdout (ngit-make-directory directory t))
      (dolist (hex commits)
        (setq num (1+ num))
        (let* ((c (ngit-seq-commit-object repo hex))
               (mail (ngit-patch--mail repo hex num total numbered items))
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

(defun ngit-patch--unquote (text)
  "Decode a C-quoted header name, retaining unquoted spaces."
  (if (not (string-prefix-p "\"" text)) text
    (require 'ngit-match)
    (let ((decoded (ngit--attr-unquote text)))
      (if decoded (car decoded) text))))

(defvar ngit-patch--p-value 1)

(defun ngit-patch--dev-null-p (line prefix)
  "Test is_dev_null on the literal header, before C unquoting."
  (string-match-p (concat "\\`" (regexp-quote prefix) "/dev/null[ \t\r\n]") line))

(defun ngit-patch--tree-name (name)
  "Strip the selected number of tree components from NAME."
  (let ((start 0) (left ngit-patch--p-value) slash)
    (while (and (> left 0) (setq slash (string-search "/" name start)))
      (setq start (1+ slash) left (1- left)))
    (and (= left 0) (not (string-prefix-p "/" name)) (substring name start))))

(defun ngit-patch--header-name (line)
  "Find apply.c's default name only when both diff header names agree."
  (let ((text (substring (ngit-patch--strip-line line) 11)))
    (if (string-prefix-p "\"" text)
        (progn
          (require 'ngit-match)
          (let* ((first (ngit--attr-unquote text))
                 (a (and first (ngit-patch--tree-name (car first))))
                 ;; git_header_name compares an unquoted second name with
                 ;; the original line length, including its newline.
                 (rest (and first (string-trim-left (substring line (+ 11 (cdr first))))))
                 (b (and rest (ngit-patch--tree-name (ngit-patch--unquote rest)))))
            (and a (equal a b) a)))
      (let ((quote (string-search "\"" text)) found)
        (if quote
            (let* ((a (ngit-patch--tree-name (substring text 0 quote)))
                   (b (ngit-patch--tree-name (ngit-patch--unquote (substring text quote)))))
              (setq found (and a b (string-prefix-p (concat b " ") a) b)))
          (let ((start 0) space tab at)
            (while (and (not found)
                        (progn
                          (setq space (string-search " " text start)
                                tab (string-search "\t" text start)
                                at (if (and tab (or (not space) (< tab space))) tab space))))
              (let ((a (ngit-patch--tree-name (substring text 0 at)))
                    (b (ngit-patch--tree-name (substring text (1+ at)))))
                (when (and a (equal a b)) (setq found a)))
              (setq start (1+ at)))))
        found))))

(defun ngit-patch--path (line prefix &optional count fallback keep-tabs)
  "Read find_name's filename, component stripping and unquoted fallback."
  (let* ((text (substring (ngit-patch--strip-line line) (length prefix)))
         (left (or count ngit-patch--p-value)) (start 0) slash name decoded gnu)
    ;; find_name_gnu returns a successful empty string at -p0. If unquoting
    ;; or component stripping fails, find_name_common reads the ORIGINAL
    ;; header, including its quotes, and may use the default name.
    (when (string-prefix-p "\"" text)
      (require 'ngit-match)
      (setq decoded (ngit--attr-unquote text))
      (when decoded
        (let* ((quoted (car decoded)) (nul (string-search (unibyte-string 0) quoted))
               (strip left) (at 0))
          ;; strchr and the consumers of the detached C buffer stop at NUL.
          (when nul (setq quoted (substring quoted 0 nul)))
          (while (and (> strip 0) (setq slash (string-search "/" quoted at)))
            (setq at (1+ slash) strip (1- strip)))
          (when (= strip 0)
            (setq gnu t name (substring quoted at))))))
    (unless gnu
      (let ((tab (and (not keep-tabs) (string-search "\t" text))))
        (when tab (setq text (substring text 0 tab))))
      (while (and (> left 0) (setq slash (string-search "/" text start)))
        (setq start (1+ slash) left (1- left)))
      (when (and (= left 0) (< start (length text)))
        (setq name (substring text start)))
      (when (and fallback
                 (or (not name) (and (< (length fallback) (length name))
                                    (string-prefix-p fallback name))))
        (setq name fallback)))
    ;; /dev/null is recognized only by is_dev_null on the literal header.
    ;; find_name squashes slashes; git_header_name deliberately does not.
    (and name (replace-regexp-in-string "/+" "/" name))))

(defun ngit-patch--traditional-path (line prefix &optional count fallback)
  "Read a traditional filename, excluding apply.c's diff_timestamp_len suffix."
  (let* ((text (ngit-patch--strip-line line))
         (stamp (string-match
                      (concat "\\(?:\t\\| +\\)\\(?:[0-9][0-9]\\)?[0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
                              "\\(?: [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\(?:\\.[0-9]+\\)?\\)?"
                              "\\(?: [+-][0-9][0-9]:?[0-9][0-9]\\)?\\'")
                      text (length prefix))))
    ;; A recognized timestamp supplies an explicit end, so earlier tabs
    ;; belong to the filename. Without one, find_name terminates at a tab.
    (ngit-patch--path (if stamp (substring text 0 stamp) line)
                      prefix count fallback (and stamp t))))

(defun ngit-patch--check-header-name (patch line)
  "Reject a Git header missing a required name after component stripping."
  (when (plist-get patch :git)
    (cond
     ((and (not (plist-get patch :old)) (not (plist-get patch :new)))
      (ngit--die-with 128
                      "error: git diff header lacks filename information when removing %d leading pathname component%s (line %d)"
                      ngit-patch--p-value (if (= ngit-patch--p-value 1) "" "s") line))
     ((or (and (not (plist-get patch :old)) (not (plist-get patch :is-new)))
          (and (not (plist-get patch :new)) (not (plist-get patch :is-delete)))
          (and (or (plist-get patch :old-header) (plist-get patch :new-header))
               (not (and (not (plist-get patch :rename)) (not (plist-get patch :copy))
                         (or (and (plist-get patch :is-new) (not (plist-get patch :old)))
                             (and (plist-get patch :is-delete) (not (plist-get patch :new))))))
               (or (and (not (plist-get patch :old-header)) (not (plist-get patch :is-new)))
                   (and (not (plist-get patch :new-header)) (not (plist-get patch :is-delete))))))
      (ngit--die-with 128 "error: git diff header lacks filename information (line %d)" line)))))

(defun ngit-patch--epoch-header-p (line)
  "Recognize GNU diff's epoch timestamp, including its numeric timezone."
  (when (string-match
         "\t\\(1969-12-31\\|1970-01-01\\) \\([0-2][0-9]\\):\\([0-5][0-9]\\):00\\(?:\\.0+\\)? \\([-+]\\)\\([0-2][0-9]\\):?\\([0-5][0-9]\\)\n\\'"
         line)
    (let ((day (match-string 1 line))
          (minutes (+ (* 60 (string-to-number (match-string 2 line)))
                      (string-to-number (match-string 3 line))))
          (zone (* (if (equal (match-string 4 line) "-") -1 1)
                   (+ (* 60 (string-to-number (match-string 5 line)))
                      (string-to-number (match-string 6 line))))))
      (= (- minutes zone) (if (equal day "1969-12-31") 1440 0)))))

(defun ngit-patch--hunk-header (line)
  (when (and (string-suffix-p "\n" line)
             (string-match "\\`@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@" line))
    (vector (string-to-number (match-string 1 line))
            (string-to-number (or (match-string 2 line) "1"))
            (string-to-number (match-string 3 line))
            (string-to-number (or (match-string 4 line) "1")) nil 0 0)))

(defun ngit-patch--parse (data &optional p-value)
  "Parse DATA into patch plists, ignoring mail and diffstat headers."
  (let* ((lines (ngit-xdiff--split data)) (n (length lines)) (i 0)
         (ngit-patch--p-value (or p-value 1)) (p-known (not (null p-value)))
         (patches nil) (no-newlines 0) current hunk malformed-traditional)
    (while (< i n)
      (let* ((line (aref lines i))
             (header-line (seq-some
                           (lambda (prefix) (string-prefix-p prefix line))
                           '("index " "old mode " "new mode " "new file mode "
                             "deleted file mode " "similarity index " "dissimilarity index "
                             "rename old " "rename new " "rename from " "rename to "
                             "copy from " "copy to " "--- " "+++ "))))
        ;; parse_single_patch consumes only consecutive fragments. A gap
        ;; returns to find_header, which rejects any later orphan fragment.
        (when (and current hunk (not (string-prefix-p "@@ -" line)))
          (plist-put current :hunks (append (plist-get current :hunks) (list hunk)))
          (push current patches)
          (setq current nil hunk nil))
        ;; parse_git_diff_header stops at the first non-header line. Once
        ;; hunks or binary data begin, trailing mail prose is never metadata.
        (when (and (plist-get current :header-open)
                   (or (not header-line) (not (string-suffix-p "\n" line))))
          (ngit-patch--check-header-name current (- (1+ i) no-newlines))
          (setq current (plist-put current :header-open nil)))
        (cond
         ((and (not current) (string-prefix-p "@@ -" line)
               (string-suffix-p "\n" line)
               (ngit-patch--hunk-header line))
          (ngit--die-with 128 "error: patch fragment without header at line %d: %s"
                          (- (1+ i) no-newlines) (ngit-patch--strip-line line)))
         ((string-prefix-p "diff --git " line)
          (when current
            (ngit-patch--check-header-name current (- (1+ i) no-newlines))
            (when hunk (plist-put current :hunks (append (plist-get current :hunks) (list hunk))))
            (push current patches))
          (let ((name (ngit-patch--header-name line)))
            (setq current (list :hunks nil :old-mode #o100644 :new-mode #o100644
                                :mode-known nil :git t :header-open t
                                :header-line (- (+ i 2) no-newlines)
                                :old name :new name)
                  hunk nil)))
         ((and (plist-get current :header-open) (string-prefix-p "index " line))
          (let* ((text (ngit-patch--strip-line line))
                 (space (string-search " " text 6))
                 (dots (string-search ".." text 6)))
            (when dots
              (setq current (plist-put current :old-oid (substring text 6 dots)))
              (setq current (plist-put current :new-oid (substring text (+ dots 2) space))))
            (when space
              (let ((mode (substring text (1+ space))))
                (when (string-match-p "\\`[0-7]+\\'" mode)
                  (setq current (plist-put current :old-mode (string-to-number mode 8)))
                  (setq current (plist-put current :new-mode (string-to-number mode 8)))
                  (setq current (plist-put current :mode-known t)))))))
         ((and current (not hunk) (not (plist-get current :hunks))
               (or (equal line "GIT binary patch\n")
                   (and (or (string-prefix-p "Binary files " line)
                            (string-prefix-p "Files " line))
                        (string-suffix-p " differ\n" line))))
          (ngit-patch--check-header-name current (- (1+ i) no-newlines))
          (require 'ngit-binary-patch)
          (if (equal line "GIT binary patch\n")
              (let ((binary (ngit-binary-patch-parse lines (1+ i))))
                (setq current (plist-put current :binary (car binary))
                      i (1- (cdr binary))))
            (setq current (plist-put current :binary (list nil nil))))
          (when (= (plist-get current :old-mode) 0)
            (setq current (plist-put current :old nil)))
          (when (= (plist-get current :new-mode) 0)
            (setq current (plist-put current :new nil))))
         ((and (plist-get current :header-open) (string-prefix-p "new file mode " line))
          (setq current (plist-put current :is-new t))
          (setq current (plist-put current :old nil))
          (setq current (plist-put current :mode-known t))
          (setq current (plist-put current :old-mode 0))
          (setq current (plist-put current :new-mode (string-to-number (substring line 14) 8))))
         ((and (plist-get current :header-open) (string-prefix-p "deleted file mode " line))
          (setq current (plist-put current :is-delete t))
          (setq current (plist-put current :new nil))
          (setq current (plist-put current :mode-known t))
          (setq current (plist-put current :old-mode (string-to-number (substring line 18) 8)))
          (setq current (plist-put current :new-mode 0)))
         ((and (plist-get current :header-open) (string-prefix-p "old mode " line))
          (setq current (plist-put current :mode-change t))
          (setq current (plist-put current :mode-known t))
          (setq current (plist-put current :old-mode (string-to-number (substring line 9) 8))))
         ((and (plist-get current :header-open) (string-prefix-p "new mode " line))
          (setq current (plist-put current :mode-change t))
          (setq current (plist-put current :mode-known t))
          (setq current (plist-put current :new-mode (string-to-number (substring line 9) 8))))
         ((and (plist-get current :header-open)
               (or (string-prefix-p "rename from " line) (string-prefix-p "rename old " line)
                   (string-prefix-p "copy from " line)))
          (setq current (plist-put current :old-header t))
          (when (string-prefix-p "copy " line) (setq current (plist-put current :copy t)))
          (when (string-prefix-p "rename " line) (setq current (plist-put current :rename t)))
          (setq current (plist-put current :old
                                   (ngit-patch--path line
                                                     (cond ((string-prefix-p "copy from " line) "copy from ")
                                                           ((string-prefix-p "rename old " line) "rename old ")
                                                           (t "rename from "))
                                                     (max 0 (1- ngit-patch--p-value)) nil t))))
         ((and (plist-get current :header-open)
               (or (string-prefix-p "rename to " line) (string-prefix-p "rename new " line)
                   (string-prefix-p "copy to " line)))
          (setq current (plist-put current :new-header t))
          (when (string-prefix-p "copy " line) (setq current (plist-put current :copy t)))
          (when (string-prefix-p "rename " line) (setq current (plist-put current :rename t)))
          (setq current (plist-put current :new
                                   (ngit-patch--path line
                                                     (cond ((string-prefix-p "copy to " line) "copy to ")
                                                           ((string-prefix-p "rename new " line) "rename new ")
                                                           (t "rename to "))
                                                     (max 0 (1- ngit-patch--p-value)) nil t))))
         ((and (string-prefix-p "--- " line)
               (or (and (plist-get current :header-open) (not hunk)
                        (not (plist-get current :hunks)))
                   (and (>= (length line) 6) (< (+ i 2) n)
                        (string-prefix-p "+++ " (aref lines (1+ i)))
                        (string-prefix-p "@@ -" (aref lines (+ i 2))))))
          (if (and (plist-get current :header-open) (not hunk)
                   (not (plist-get current :hunks)))
              (let ((name (and (not (ngit-patch--dev-null-p line "--- "))
                               (ngit-patch--path line "--- "))))
                (when (and name (plist-get current :old)
                           (not (equal name (plist-get current :old))))
                  (ngit--die-with 128 "error: git apply: bad git-diff - inconsistent old filename on line %d"
                                  (plist-get current :header-line)))
                (setq current (plist-put current :old-header t))
                (setq current (plist-put current :old name)))
            (when current
              (ngit-patch--check-header-name current (- (1+ i) no-newlines))
              (when hunk (plist-put current :hunks (append (plist-get current :hunks) (list hunk))))
              (push current patches))
            (let* ((second (and (< (1+ i) n) (aref lines (1+ i))))
                   ;; is_dev_null tests the literal header, not find_name's
                   ;; result: an empty name may still fall back to the old one.
                   (old-null (ngit-patch--dev-null-p line "--- "))
                   (new-null (and second (ngit-patch--dev-null-p second "+++ ")))
                   (first-name (ngit-patch--traditional-path line "--- " 0))
                   (second-name (and second (ngit-patch--traditional-path second "+++ " 0)))
                   (p (if (and (not old-null) first-name (not (string-search "/" first-name))) 0 -1))
                   (q (if (and (not new-null) second-name (not (string-search "/" second-name))) 0 -1)))
              (unless (and second (string-prefix-p "+++ " second))
                (ngit--die-with 128 "error: No valid patches in input"))
              (when (< p 0) (setq p q))
              (when (and (not p-known) (>= p 0) (= p q))
                (setq ngit-patch--p-value p p-known t))
              (let* ((old (ngit-patch--traditional-path line "--- "))
                     (name (if (and new-null (not old-null)) old
                             (ngit-patch--traditional-path second "+++ " nil
                                                           (and (not old-null) old))))
                     ;; parse_traditional_patch checks both /dev/null names
                     ;; before guessing creation/deletion from timestamps.
                     (creation (or old-null
                                   (and (not new-null) (ngit-patch--epoch-header-p line))))
                     (deletion (and (not creation)
                                    (or new-null (ngit-patch--epoch-header-p second)))))
                (unless name
                  (ngit--die-with 128 "error: unable to find filename in patch at line %d"
                                  (- (1+ i) no-newlines)))
                (setq current (list :hunks nil :old-mode #o100644 :new-mode #o100644
                                    :mode-known nil :old (and (not creation) name)
                                    :new (and (not deletion) name))
                      hunk nil i (1+ i))))))
         ((and (not current) (string-prefix-p "--- " line))
          ;; Skip prose before a later patch, but reject it as the only candidate.
          (setq malformed-traditional t))
         ((and (plist-get current :header-open) (string-prefix-p "+++ " line))
          (let ((name (and (not (ngit-patch--dev-null-p line "+++ "))
                           (ngit-patch--path line "+++ "))))
            (when (and name (plist-get current :new)
                       (not (equal name (plist-get current :new))))
              (ngit--die-with 128 "error: git apply: bad git-diff - inconsistent new filename on line %d"
                              (plist-get current :header-line)))
            (setq current (plist-put current :new-header t))
            (setq current (plist-put current :new name))))
         ((and current (string-prefix-p "@@ " line))
          (ngit-patch--check-header-name current (- (1+ i) no-newlines))
          (when hunk (plist-put current :hunks (append (plist-get current :hunks) (list hunk))))
          (setq hunk (ngit-patch--hunk-header line))
          ;; parse_fragment consumes exactly the two declared line counts.
          ;; A header, EOF, or an unterminated body before then is corruption,
          ;; never permission to replace bytes that were not in the preimage.
          (unless (and hunk (string-suffix-p "\n" line))
            (ngit--die-with 128 "error: corrupt patch at line %d" (- (1+ i) no-newlines)))
          (let ((old (aref hunk 1)) (new (aref hunk 3))
                (j (1+ i)) (changes 0) body)
            (while (and (< j n) (or (/= old 0) (/= new 0)))
              (let* ((text (aref lines j))
                     (kind (and (> (length text) 0) (aref text 0))))
                (unless (and (string-suffix-p "\n" text)
                             (memq kind '(?\n ?\s ?- ?+ ?\\)))
                  (ngit--die-with 128 "error: corrupt patch at line %d" (- (1+ j) no-newlines)))
                (cond
                 ((= kind ?\\)
                  (unless (and (>= (length text) 12) (string-prefix-p "\\ " text))
                    (ngit--die-with 128 "error: corrupt patch at line %d" (- (1+ j) no-newlines)))
                  (when (and body (string-suffix-p "\n" (car body)))
                    (setcar body (substring (car body) 0 -1))))
                 (t
                  ;; GNU diff omits the space on empty context lines.
                  (when (= kind ?\n) (setq kind ?\s text " \n"))
                  (unless (= kind ?+) (setq old (1- old)))
                  (unless (= kind ?-) (setq new (1- new)))
                  (unless (= kind ?\s) (setq changes (1+ changes)))
                  (push text body))))
              (setq j (1+ j)))
            (unless (and (= old 0) (= new 0) (> changes 0))
              (ngit--die-with 128 "error: corrupt patch at line %d" (- (1+ j) no-newlines)))
            ;; The last no-newline marker is outside parse_fragment's loop,
            ;; and unlike interior markers does not advance state->linenr.
            (when (and (< j n) (> (length (aref lines j)) 12)
                       (string-prefix-p "\\ " (aref lines j)))
              (when (and body (string-suffix-p "\n" (car body)))
                (setcar body (substring (car body) 0 -1)))
              (setq j (1+ j) no-newlines (1+ no-newlines)))
            (aset hunk 4 (nreverse body))
            (aset hunk 5 (aref hunk 1))
            (aset hunk 6 (aref hunk 3))
            (setq i (1- j)))))
        ;; check_header_line rejects conflicting extensions immediately,
        ;; remembering the first extension's physical line for its diagnostic.
        (when (plist-get current :header-open)
          (let ((extensions 0) (lineno (- (1+ i) no-newlines)))
            (dolist (flag '(:is-new :is-delete :rename :copy))
              (when (plist-get current flag) (setq extensions (1+ extensions))))
            (when (> extensions 1)
              (ngit--die-with 128 "error: inconsistent header lines %d and %d"
                              (plist-get current :extension-line) lineno))
            (when (and (> extensions 0) (not (plist-get current :extension-line)))
              (setq current (plist-put current :extension-line lineno)))))
        (when (and (plist-get current :git) (not hunk)
                   (seq-some (lambda (prefix) (string-prefix-p prefix line))
                             '("diff --git " "index " "old mode " "new mode "
                               "new file mode " "deleted file mode "
                               "similarity index " "dissimilarity index "
                               "rename old " "rename new " "rename from " "rename to "
                               "copy from " "copy to "
                               "--- " "+++ ")))
          (setq current (plist-put current :header-end (- (+ i 2) no-newlines)))))
      (setq i (1+ i)))
    (when current
      (ngit-patch--check-header-name current (- (1+ i) no-newlines))
      (when hunk (plist-put current :hunks (append (plist-get current :hunks) (list hunk))))
      (push current patches))
    (when (and malformed-traditional (not patches))
      (ngit--die-with 128 "error: No valid patches in input (allow with \"--allow-empty\")"))
    (dolist (patch (reverse patches))
      (let ((old 0) (new 0))
        (dolist (fragment (plist-get patch :hunks))
          (setq old (+ old (aref fragment 1)) new (+ new (aref fragment 3))))
        ;; With no explicit creation marker, a single insertion-only hunk
        ;; may create a missing preimage. Existing files remain modifications.
        (when (and (not (plist-get patch :git))
                   (plist-get patch :old) (plist-get patch :new)
                   (= (length (plist-get patch :hunks)) 1))
          (when (= old 0) (plist-put patch :maybe-new t))
          (when (= new 0) (plist-put patch :maybe-delete t)))
        (when (and (not (plist-get patch :old)) (> old 0))
          (ngit--die-with 128 "error: new file %s depends on old contents" (plist-get patch :new)))
        (when (and (not (plist-get patch :new)) (> new 0))
          (ngit--die-with 128 "error: deleted file %s still has contents" (plist-get patch :old))))
      (when (and (plist-get patch :git) (not (plist-get patch :hunks))
                 (not (plist-get patch :binary))
                 (not (plist-get patch :is-new)) (not (plist-get patch :is-delete))
                 (not (plist-get patch :copy))
                 (not (plist-get patch :rename))
                 (equal (plist-get patch :old) (plist-get patch :new))
                 (= (plist-get patch :old-mode) (plist-get patch :new-mode)))
        (ngit--die-with 128 "error: patch with only garbage at line %d"
                        (or (plist-get patch :header-end) (plist-get patch :header-line)))))
    (nreverse patches)))

(defun ngit-patch--index-entry (repo path)
  "Return only a stage-zero entry, as index_name_pos does in apply.c."
  (let ((entry (and path (ngit-index-lookup (ngit-index-entries repo) path))))
    (and entry (= (aref entry 3) 0) entry)))

(defun ngit-patch--source (repo patch cached reverse)
  (let* ((path (if reverse (plist-get patch :new) (plist-get patch :old)))
         (entry (ngit-patch--index-entry repo path)))
    (cond
     ((null path) "")
     (cached
      (unless entry (ngit--die-with 1 "error: %s: does not exist in index" path))
      (if (= (aref entry 2) #o160000)
          (concat "Subproject commit " (ngit-hex (aref entry 1)) "\n")
        (cdr (ngit-read-object repo (aref entry 1)))))
     (t
      (ngit-wt-setup (ngit-repo-toplevel repo))
      (let* ((file (expand-file-name (decode-coding-string path 'utf-8)
                                     (ngit-repo-toplevel repo)))
             (st (ngit-wt-lstat path)))
        (unless st
          (ngit--die-with 1 "error: %s: %s" path
                          (if (ngit-patch--blocked-parent path) "Not a directory"
                            "No such file or directory")))
        (let* ((actual (logand (aref st 0) ngit-s-ifmt))
               (expected (if reverse (plist-get patch :new-mode)
                           (plist-get patch :old-mode))))
          (when (and (plist-get patch :mode-known)
                     (/= expected #o160000)
                     (/= actual (logand expected #o170000)))
            (ngit--die-with 1 "error: %s: wrong type" path))
          (cond
           ((= expected #o160000) "")
           ((= actual ngit-s-iflnk)
            (encode-coding-string (file-symlink-p file) 'utf-8))
           (t (let ((data (ngit--read-file file))
                    (keep-crlf nil))
		;; read_old_data uses KEEP_CRLF when the patch's preimage
		;; contains CRLF; otherwise it renormalizes without an index.
		(dolist (h (plist-get patch :hunks))
                  (dolist (line (aref h 4))
                    (when (and (memq (aref line 0) (if reverse '(?\s ?+) '(?\s ?-)))
                               (string-suffix-p "\r\n" line))
                      (setq keep-crlf t))))
		(let ((action (ngit--crlf-action repo path)))
                  (if (or keep-crlf (eq action 'binary)
                          (and (memq action '(auto auto-input auto-crlf))
                               (nth 3 (ngit--text-stats data))))
                      data
                    (ngit--strip-crlf data))))))))))))

(defun ngit-patch--line-data (line) (substring line 1))

(defun ngit-patch--match-at (source pos body reverse &optional patched ending)
  (let ((at pos) (ok t) (different nil) preimage target)
    (dolist (line body)
      (let ((kind (aref line 0)))
        (when reverse (setq kind (cond ((= kind ?+) ?-) ((= kind ?-) ?+) (t kind))))
        (when (memq kind '(?\s ?-))
          (if (or (>= at (length source)) (and patched (aref patched at)))
              (setq ok nil)
            (let ((old (ngit-patch--line-data line)) (actual (aref source at)))
              (push old preimage)
              (push actual target)
              (unless (string= old actual)
                (setq different t)
                ;; match_fragment first checks whitespace-insensitive line
                ;; hashes, then memcmp over the preimage's byte length.
                (let (hashes)
                  (dolist (text (list old actual))
                    (let ((hash 0) (i 0))
                      (while (< i (length text))
                        (let ((byte (aref text i)))
                          (unless (memq byte '(9 10 11 12 13 32))
                            (setq hash (logand #xffffffff (+ (* hash 3) byte)))))
                        (setq i (1+ i)))
                      (push hash hashes)))
                  (unless (= (car hashes) (cadr hashes)) (setq ok nil)))))
            (setq at (1+ at))))))
    (and ok
         (or (not different)
             (let ((old (apply #'concat (nreverse preimage)))
                   (actual (apply #'concat (nreverse target))))
               ;; An incomplete final preimage can match a prefix of its
               ;; last line. update_image still removes that whole line.
               (if ending (string= old actual) (string-prefix-p old actual)))))))

(defun ngit-patch--find-hunk (source expected body reverse beginning ending count patched)
  "Find a fragment using apply.c's find_pos and match_fragment order."
  (let* ((distance 0) found (n (length source))
         (start (cond (beginning 0) (ending (- n count)) (t expected))))
    (when (or (< start 0) (> start n)) (setq start n))
    (while (and (not found) (<= distance n))
      ;; find_pos's even iteration advances forwards, then the odd one
      ;; advances backwards.  The larger line number wins equal distances.
      (dolist (pos (if (= distance 0) (list start)
                     (list (+ start distance) (- start distance))))
        (when (and (not found) (>= pos 0) (<= (+ pos count) n)
                   (or (not beginning) (= pos 0))
                   (or (not ending) (= (+ pos count) n))
                   (ngit-patch--match-at source pos body reverse patched ending))
          (setq found (cons pos (- pos expected)))))
      (setq distance (1+ distance)))
    found))

(defun ngit-patch--apply-one (source patch reverse)
  (let* ((lines (ngit-xdiff--split source))
         (patched (make-vector (length lines) nil))
         (hunks (plist-get patch :hunks)) (failed nil) (offsets nil))
    (dolist (h hunks)
      (unless failed
        (let* ((start (if reverse (aref h 2) (aref h 0)))
               (new-start (if reverse (aref h 0) (aref h 2)))
               (expected (if (= new-start 0) 0 (1- new-start)))
               (count (if reverse (aref h 3) (aref h 1)))
               (trailing (let ((n 0))
                           (dolist (line (aref h 4))
                             (setq n (if (= (aref line 0) ?\s) (1+ n) 0))) n))
               (found (ngit-patch--find-hunk lines expected (aref h 4) reverse
                                             (<= start 1) (= trailing 0) count patched)))
          (if (not found)
              (progn (setq failed t) (plist-put patch :failed-start start))
            (let ((pos (car found)) (replacement nil))
              (when (/= (cdr found) 0) (push (cons start (cdr found)) offsets))
              (dolist (line (aref h 4))
                (let ((kind (aref line 0)))
                  (when reverse (setq kind (cond ((= kind ?+) ?-) ((= kind ?-) ?+) (t kind))))
                  (unless (= kind ?-)
                    (push (ngit-patch--line-data line) replacement))))
              (setq replacement (vconcat (nreverse replacement))
                    lines (vconcat (seq-subseq lines 0 pos) replacement
                                   (seq-subseq lines (+ pos count)))
                    patched (vconcat (seq-subseq patched 0 pos)
                                     (make-vector (length replacement) t)
                                     (seq-subseq patched (+ pos count)))))))))
    (if failed nil
      (cons (mapconcat #'identity lines "") (nreverse offsets)))))

(defun ngit-patch--target-path (patch reverse)
  (if reverse (plist-get patch :old) (plist-get patch :new)))

(defun ngit-patch--target-mode (patch reverse)
  ;; reverse_patches swaps modes only when the original new_mode is set.
  (if reverse (if (= (plist-get patch :new-mode) 0) #o100644
                (plist-get patch :old-mode))
    (plist-get patch :new-mode)))

(defun ngit-patch--blocked-parent (path)
  "Return a non-directory leading component of PATH, if any."
  (let ((start 0) slash blocked)
    (while (setq slash (ngit--byte-pos path ?/ start))
      (let ((st (ngit-wt-lstat (substring path 0 slash))))
        (when (and st (/= (ngit-wt-mode st) ngit-mode-tree))
          (setq blocked (substring path 0 slash))))
      (setq start (1+ slash)))
    blocked))

(defun ngit-patch--validate-path (path)
  "Require a single canonical spelling before path or symlink bookkeeping."
  (when path
    (when (ngit-windows-p) (ngit-win-validate-worktree-path path))
    (let ((parts (split-string path "/")))
      (when (or (string= path "") (= (aref path 0) ?/)
                (member "" parts) (member "." parts) (member ".." parts)
                (seq-some
                 (lambda (part)
                   (string= (downcase (replace-regexp-in-string "[ .]+\\'" "" part)) ".git"))
                 parts))
        (ngit--die-with 128 "error: invalid path '%s'" path)))))

(defun ngit-patch--safe-path (repo path &optional modes cached)
  "Reject paths that escape the work tree or traverse a symlink."
  (when path
    (ngit-patch--validate-path path)
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (let ((start 0) slash)
      (while (setq slash (ngit--byte-pos path ?/ start))
        (let* ((parent (substring path 0 slash))
               (mode (and modes (gethash parent modes)))
               (entry (and cached (ngit-index-lookup (ngit-index-entries repo) parent)))
               (st (and (not mode) (not cached) (ngit-wt-lstat parent))))
          (when (= (logand (or mode (and entry (aref entry 2)) (and st (aref st 0)) 0)
                           #o170000) #o120000)
            (ngit--die-with 1 "error: affected file '%s' is beyond a symbolic link" path)))
        (setq start (1+ slash))))))

(defun ngit-patch--prepare (repo patches cached reverse index-check)
  (let ((prepared nil) (modes (make-hash-table :test 'equal))
        (results (make-hash-table :test 'equal))
        (removed (make-hash-table :test 'equal)) errors)
    ;; Inspect the whole post-patch tree before checking or writing any path.
    ;; A symlink introduced later in the input still protects its descendants.
    (dolist (patch patches)
      (let ((old (if reverse (plist-get patch :new) (plist-get patch :old))))
        (when (and old (plist-get patch (if reverse :maybe-delete :maybe-new))
                   (not (if (or cached index-check)
                            (ngit-patch--index-entry repo old)
                          (progn (ngit-wt-setup (ngit-repo-toplevel repo))
                                 (ngit-wt-lstat old)))))
          (plist-put patch (if reverse :new :old) nil)))
      (let ((old (if reverse (plist-get patch :new) (plist-get patch :old)))
            (path (ngit-patch--target-path patch reverse)))
        ;; Accepted names already have their find_name normalization and
        ;; contain no empty/dot components. All consumers use these names.
        ;; check_preimage diagnoses a quoted empty preimage before checking
        ;; path safety. Never turn it into the traditional default name.
        (when (equal old "")
          (ngit--die-with 1 "error: : %s"
                          (if (or cached index-check) "does not exist in index"
                            "No such file or directory")))
        (ngit-patch--validate-path old)
        (ngit-patch--validate-path path)
        (when (and old (not (equal old path)) (not (plist-get patch :copy)))
          (puthash old t removed)
          ;; A destination may already occupy this name in the postimage.
          ;; It must not hide the independent removal of this preimage.
          (unless (gethash old modes) (puthash old 0 modes)))
        (when (and path (= (logand (ngit-patch--target-mode patch reverse) #o170000) #o120000))
          (puthash path #o120000 modes))))
    (dolist (patch patches)
      (condition-case err
      (let* ((old-path (if reverse (plist-get patch :new) (plist-get patch :old)))
             (path (ngit-patch--target-path patch reverse))
             (mode (ngit-patch--target-mode patch reverse))
             ;; previous_patch ignores copies and renames, whose sources
             ;; describe the original tree rather than an incremental diff.
             (previous (and old-path (not (plist-get patch :copy))
                            (not (plist-get patch :rename))
                            (or (not path) (equal old-path path))
                            (gethash old-path results)))
             source result)
        (ngit-patch--safe-path repo old-path nil cached)
        (ngit-patch--safe-path repo path modes cached)
        (when (and previous (not (plist-get previous :path)))
          (ngit--die-with 1 "error: path %s has been renamed/deleted" old-path))
        ;; verify_index_match runs before mode warnings and load_preimage.
        ;; Once verified, --index always reads the index, retaining CRLF
        ;; blobs stored before autocrlf was enabled.
        (when (and index-check old-path (not previous))
          (let* ((entry (ngit-patch--index-entry repo old-path))
                 (st (and entry (not cached) (ngit-wt-lstat old-path))))
            (unless entry
              (ngit--die-with 1 "error: %s: does not exist in index" old-path))
            ;; verify_index_match accepts gitlinks first. ie_match_stat tests
            ;; CE_FSMONITOR_VALID before intent-to-add; FSMN stores its inverse.
            (when (and (not cached) (/= (logand (aref entry 2) #o170000) #o160000)
                       (memq 'ita (aref entry 5)))
              (let* ((index (ngit-read-index repo)) (raw (aref index 1))
                     (entries (aref index 0)) (last (aref entries (1- (length entries))))
                     (pos (aref last 4)) (flags (ngit--be16 raw (+ pos 60)))
                     (hdr (if (= 0 (logand flags #x4000)) 62 64))
                     (ext (if (= (aref index 2) 4)
                              (1+ (ngit--byte-pos raw 0
                                                (cdr (ngit--index-varint raw (+ pos hdr)))))
                            (+ pos (logand (+ hdr (length (aref last 0)) 8) -8))))
                     (bit (seq-position entries entry #'eq))
                     (monitor (ngit-config-get repo "core.fsmonitor")) valid)
                (while (< (+ ext 8) (- (length raw) 20))
                  (let ((len (ngit--be32 raw (+ ext 4))))
                    (when (and (equal (substring raw ext (+ ext 4)) "FSMN")
                               (and monitor (not (member (downcase monitor)
                                                         '("false" "no" "off" "0" "")))))
                      (let* ((version (ngit--be32 raw (+ ext 8)))
                             (map (if (= version 1) (+ ext 20)
                                    (1+ (ngit--byte-pos raw 0 (+ ext 12)))))
                             (bits (ngit--be32 raw (+ map 4)))
                             (words (ngit--be32 raw (+ map 8)))
                             (at (+ map 12)) (offset 0) dirty)
                        (while (and (< at (+ map 12 (* words 8))) (<= offset bit)
                                    (< bit bits))
                          (let* ((word (ngit--be64s raw at))
                                 (run (* 64 (logand (ash word -1) #xffffffff)))
                                 (literal (logand (ash word -33) #x7fffffff)))
                            (setq at (+ at 8))
                            (if (< bit (+ offset run))
                                (setq dirty (/= 0 (logand word 1)))
                              (when (< bit (+ offset run (* literal 64)))
                                (let* ((slot (- bit offset run))
                                       (byte (+ at (* (/ slot 64) 8) (- 7 (/ (% slot 64) 8)))))
                                  (setq dirty (/= 0 (logand (aref raw byte)
                                                           (ash 1 (% slot 8))))))))
                            (setq offset (+ offset run (* literal 64))
                                  at (+ at (* literal 8)))))
                        (setq valid (not dirty))))
                    (setq ext (+ ext 8 len))))
                (unless valid
                  (ngit--die-with 1 "error: %s: does not match index" old-path))))
            (when st
              (unless
                  (if (= (logand (aref entry 2) #o170000) #o160000)
                      (= (ngit-wt-mode st) ngit-mode-tree)
                    (and (or (= (ngit-wt-mode st) (aref entry 2))
                             (and (= (aref entry 2) ngit-mode-symlink)
                                  (= (logand (ngit-wt-mode st) ngit-s-ifmt) #o100000)
                                  (not (ngit-config-bool repo "core.symlinks" t)))
                             (and (not (ngit-config-bool repo "core.filemode" t))
                                  (= (logand (ngit-wt-mode st) ngit-s-ifmt) #o100000)
                                  (= (logand (aref entry 2) ngit-s-ifmt) #o100000)))
                         (string= (ngit-wt-content repo old-path st)
                                  (cdr (ngit-read-object repo (aref entry 1))))))
                (ngit--die-with 1 "error: %s: does not match index" old-path)))))
        (setq source (if previous (plist-get previous :data)
                       (ngit-patch--source repo patch (or cached index-check) reverse)))
        (when (and path (not (equal path old-path)) (not (gethash path removed)))
          (let ((entry (and (or cached index-check) (ngit-patch--index-entry repo path))))
            (when entry
              (if (memq 'ita (aref entry 5))
                  (unless cached (ngit--die-with 1 "error: %s: does not match index" path))
                (ngit--die-with 1 "error: %s: already exists in index" path))))
          (let ((st (and (not cached) (ngit-wt-lstat path))))
            (when (and st (/= (ngit-wt-mode st) ngit-mode-tree))
              (ngit--die-with 1 "error: %s: already exists in working directory" path))))
        (when old-path
          (let* ((entry (and (or cached index-check)
                             (ngit-patch--index-entry repo old-path)))
                 (st (and (not cached) (not previous) (ngit-wt-lstat old-path)))
                 (expected (if reverse (plist-get patch :new-mode) (plist-get patch :old-mode)))
                 (actual (or (and previous (plist-get previous :mode))
                             (and st (cond
                                      ((and (= expected #o160000)
                                            (= (ngit-wt-mode st) ngit-mode-tree)) #o160000)
                                      ;; ce_mode_from_stat preserves an indexed
                                      ;; symlink represented by a regular file.
                                      ((and entry (= (aref entry 2) ngit-mode-symlink)
                                            (= (logand (ngit-wt-mode st) ngit-s-ifmt) #o100000)
                                            (not (ngit-config-bool repo "core.symlinks" t)))
                                       ngit-mode-symlink)
                                      (t (ngit-wt-mode st))))
                             (and entry (aref entry 2)) expected)))
            (when (and (= (logand actual ngit-s-ifmt) #o100000)
                       (not (ngit-config-bool repo "core.filemode" t)))
              (setq actual (if entry (aref entry 2) expected)))
            (unless (plist-get patch :mode-known) (setq expected actual))
            (when (and (plist-get patch :mode-known)
                       (/= (logand actual ngit-s-ifmt) (logand expected ngit-s-ifmt)))
              (ngit--die-with 1 "error: %s: wrong type" old-path))
            (when (and (plist-get patch :mode-known) (/= actual expected))
              (ngit-native-write-stderr
               (format "warning: %s has type %s, expected %s\n"
                       old-path (ngit--octal6 actual) (ngit--octal6 expected))))
            (when (and path (not (plist-get patch :mode-change))) (setq mode actual))
            (when (and path (/= (logand expected ngit-s-ifmt) (logand mode ngit-s-ifmt)))
              (ngit--die-with 1
                              (if (equal old-path path)
                                  "error: new mode (%o) of %s does not match old mode (%o)"
                                "error: new mode (%o) of %s does not match old mode (%o) of %s")
                              mode path expected old-path))))
        ;; Without an index Git deliberately ignores gitlink hunks.
        (when (and old-path (not cached) (not index-check)
                   (= (if reverse (plist-get patch :new-mode) (plist-get patch :old-mode)) #o160000))
          (setq patch (plist-put patch :hunks nil)))
        (setq result (if (plist-get patch :binary)
                         (ngit-binary-patch-apply repo source patch reverse)
                       (ngit-patch--apply-one source patch reverse)))
        (unless result
          (unless (plist-get patch :binary)
            (ngit-native-write-stderr (format "error: patch failed: %s:%d\n"
                                            (or old-path path) (or (plist-get patch :failed-start)
                                                                  (aref (car (plist-get patch :hunks))
                                                                        (if reverse 2 0))))))
          (ngit--die-with 1 "error: %s: patch does not apply" (or old-path path)))
        (when (and (null path) (not (equal (car result) "")))
          (ngit-native-write-stderr "error: removal patch leaves file contents\n")
          (ngit--die-with 1 "error: %s: patch does not apply" old-path))
        (let ((one (list :old old-path :path path :mode mode
                         :data (car result) :offsets (cdr result) :patch patch)))
          (when (and old-path (not (equal old-path path)) (not (plist-get patch :copy)))
            (puthash old-path (list :path nil) results))
          (when path (puthash path one results))
          (push one prepared)))
        (ngit-fatal
         (ngit-native-write-stderr (concat (cadr err) "\n"))
         (setq errors t))))
    (when errors (ngit--die-with 1 ""))
    (nreverse prepared)))

(defun ngit-patch--remove-or-warn (file path directory)
  "Remove FILE, warning on failure as remove_or_warn does; return success."
  ;; Use the native result: delete-file on the standalone runtime may return
  ;; nil on failure, whereas host Emacs signals a file-error.
  (let* ((bytes (encode-coding-string file 'utf-8))
         (address (nl-ffi-memory-address (ngit-native--buffer 3 (1+ (length bytes))))))
    (ngit-native--write-bytes address bytes)
    (ptr-write-u8 address (length bytes) 0)
    ;; Linux x86-64 rmdir/unlink, matching the native I/O substrate.
    (let ((rc (if (ngit-windows-p)
                  (condition-case nil
                      (progn (if directory (ngit-delete-directory file) (ngit-delete-file file)) 0)
                    (file-error -13))
                (syscall-direct (if directory 84 87) address 0 0 0 0 0))))
      (if (or (= rc 0) (= rc -2)) t
        (ngit-native-write-stderr
         (format "warning: unable to %s '%s': %s\n" (if directory "rmdir" "unlink") path
                 (or (cdr (assq (- rc) '((16 . "Device or resource busy")
                                         (30 . "Read-only file system")
                                         (39 . "Directory not empty"))))
                     (ngit--config-access-message (- rc)))))
        nil))))

(defun ngit-patch--create-file (repo path file mode data)
  "Create FILE exclusively; replace a surviving preimage via a sibling."
  (let* ((symlink (and (= mode #o120000) (ngit-config-bool repo "core.symlinks" t)))
         (perm (ngit-native-creation-mode (if (= mode #o100755) #o777 #o666)))
         (contents (if symlink (decode-coding-string data 'utf-8)
                     (ngit--checkout-smudge repo path data)))
         (create (lambda (name)
                   (if symlink (make-symbolic-link contents name)
                     (ngit-write-file name contents t nil perm)))))
    (condition-case nil
        (funcall create file)
      (file-already-exists
       ;; create_one_file never truncates a file that unlink_or_warn left
       ;; behind. The parent must permit a new file and an atomic rename.
       (let ((number (emacs-pid)) temporary done)
         (while (not done)
           (setq temporary (concat file "~" (number-to-string number)))
           (condition-case nil
               (progn (funcall create temporary) (setq done t))
             (file-already-exists (setq number (1+ number)))))
         (unwind-protect (ngit-rename-file temporary file t)
           (when (or (file-exists-p temporary) (file-symlink-p temporary))
             (ngit-delete-file temporary))))))))

(defun ngit-patch--write-worktree (repo prepared)
  ;; Remove preimages before creating any result, including symlinks that
  ;; the post-patch path checks treated as removed ancestors.
  (dolist (one prepared)
    (let ((old (plist-get one :old)))
      (when (and old (not (plist-get (plist-get one :patch) :copy)))
        (let ((file (expand-file-name (decode-coding-string old 'utf-8)
                                      (ngit-repo-toplevel repo))))
          ;; remove_file only prunes empty parents for deletions/renames.
          ;; Modifications must leave their existing directory modes intact.
          (when (and (ngit-patch--remove-or-warn
                      file old (and (file-directory-p file) (not (file-symlink-p file))))
                     (not (equal old (plist-get one :path))))
            (let ((dir (directory-file-name (file-name-directory file)))
                  (top (directory-file-name (ngit-repo-toplevel repo)))
                  (cwd (ngit--reset-original-cwd)))
              (while (and (not (equal dir top)) (file-directory-p dir)
                          (not (equal dir cwd))
                          (null (ngit--dir-names (file-name-as-directory dir))))
                ;; dir.c:remove_path stops on rmdir failure, ignoring it.
                (condition-case nil
                    (progn
                      (ngit-delete-directory dir)
                      (setq dir (if (file-directory-p dir) top
                                  (directory-file-name (file-name-directory dir)))))
                  (file-error (setq dir top))))))))))
  (dolist (one prepared)
    (let* ((path (plist-get one :path))
           (file (and path (expand-file-name (decode-coding-string path 'utf-8)
                                             (ngit-repo-toplevel repo)))))
      (when (and path (/= (plist-get one :mode) 0))
	(if (= (plist-get one :mode) #o160000)
            (ngit-make-directory file t)
          (condition-case err
              (progn
		(let ((reason (and (ngit-patch--blocked-parent path) "Not a directory")))
		  (when (file-directory-p file)
		    (if (ngit--dir-names (file-name-as-directory file))
			(setq reason "Directory not empty")
		      (ngit-delete-directory file)))
		  (when reason
		    (ngit--die-with 1 "error: unable to write file '%s' mode %s: %s"
				    path (ngit--octal6 (plist-get one :mode)) reason)))
		(ngit-make-directory (file-name-directory file) t)
		(ngit-wt-setup (ngit-repo-toplevel repo))
		(ngit-patch--create-file repo path file (plist-get one :mode) (plist-get one :data)))
            (error
             (if (eq (car err) 'ngit-fatal) (signal (car err) (cdr err))
               ;; create_one_file probes lstat after EACCES, so a missing
               ;; target reports ENOENT, even when its parent is unwritable.
               (ngit--die-with 128 "error: unable to write file '%s' mode %o: %s"
                               path (plist-get one :mode)
                               (cond ((not (file-exists-p file)) "No such file or directory")
                                     ((not (file-writable-p (file-name-directory file))) "Permission denied")
                                     (t (error-message-string err))))))))))))

(defun ngit-patch--write-index (repo prepared &optional worktree)
  (let* ((index (ngit-read-index repo)) (entries (aref index 0))
         (targets nil) (latest (make-hash-table :test 'equal))
         (directories (make-hash-table :test 'equal))
         (dropped (make-hash-table :test 'equal))
         (root (or (ngit-ct-root index) (ngit-ct-new))))
    (dolist (one prepared)
      (let ((old (plist-get one :old)) (path (plist-get one :path)))
        (when (and old (not (plist-get (plist-get one :patch) :copy)))
          (puthash old t dropped) (ngit-ct-invalidate root old))
        (when path (ngit-ct-invalidate root path))))
    ;; add_index_entry refuses D/F conflicts with surviving stage-zero
    ;; entries. All removals precede additions; never let overlay silently
    ;; discard an unrelated ancestor or descendant of a result.
    (dolist (one prepared)
      (let ((path (plist-get one :path)) (mode (plist-get one :mode)))
        (when (and path (/= mode 0))
          (let* ((prefix (concat path "/")) (pos (ngit-index-find entries prefix))
                 (start 0) slash (conflict (gethash path directories)))
            (while (and (not conflict) (< pos (length entries))
                        (string-prefix-p prefix (aref (aref entries pos) 0)))
              (let ((entry (aref entries pos)))
                (when (and (= (aref entry 3) 0) (not (gethash (aref entry 0) dropped)))
                  (setq conflict t)))
              (setq pos (1+ pos)))
            (while (and (not conflict) (setq slash (ngit--byte-pos path ?/ start)))
              (let* ((ancestor (substring path 0 slash))
                     (entry (ngit-index-lookup entries ancestor)))
                (when (or (gethash ancestor latest)
                          (and entry (= (aref entry 3) 0) (not (gethash ancestor dropped))))
                  (setq conflict t)))
              (setq start (1+ slash)))
            (when conflict
              (ngit--die-with 128
                              "error: '%s' appears as both a file and as a directory\nerror: unable to add cache entry for %s"
                              path path))
            (setq start 0)
            (while (setq slash (ngit--byte-pos path ?/ start))
              (puthash (substring path 0 slash) t directories)
              (setq start (1+ slash))))
          (let* ((data (plist-get one :data))
                 (gitlink (= mode #o160000))
                 (oid (if gitlink
                          (progn
                            (unless (string-match "\\`Subproject commit \\([0-9a-fA-F]\\{40\\}\\)" data)
                              (ngit--die-with 1 "error: corrupt patch for submodule %s" path))
                            (ngit-unhex (match-string 1 data)))
                        (ngit-write-object repo "blob" data)))
                 (stat (if (and worktree (not gitlink)
                                (progn (ngit-wt-setup (ngit-repo-toplevel repo))
                                       (ngit-wt-lstat path)))
                           (ngit-wt-stat-block mode)
                         (concat (make-string 24 0) (ngit--be32-bytes mode)
                                 (make-string 12 0)))))
            (puthash path (ngit-make-entry path oid mode stat) latest)))))
    (maphash (lambda (_path entry) (push entry targets)) latest)
    (let ((conflicts (make-hash-table :test 'equal)) (old entries))
      (setq entries
            (ngit-index-overlay
             entries (sort targets #'ngit--entry-less) dropped
             (lambda (entry)
               (when (> (aref entry 3) 0) (puthash (aref entry 0) t conflicts)))))
      ;; D/F coexistence is legal across stages. Overlay's general-purpose
      ;; topology cleanup must not erase those unrelated conflict records.
      (when (> (hash-table-count conflicts) 0)
        (let (retained)
          (dotimes (i (length old))
            (let* ((entry (aref old i)) (name (aref entry 0)))
              (when (and (> (aref entry 3) 0) (gethash name conflicts)
                         (not (gethash name latest)) (not (gethash name dropped)))
                (push entry retained))))
          (setq entries (vconcat (sort (append entries retained) #'ngit--entry-less))))))
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
        (let ((n (ngit-native-fd-read 0 addr 65536)))
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

(defun ngit-patch--preflight-windows (repo prepared index-write)
  "Validate the complete Windows patch result before any mutation."
  (when (ngit-windows-p)
    (let ((removed (make-hash-table :test 'equal))
          (replaced (make-hash-table :test 'equal)) targets)
      (dolist (one prepared)
        (let ((old (plist-get one :old)) (path (plist-get one :path)))
          (when (and old (not (plist-get (plist-get one :patch) :copy)))
            (puthash old t removed))
          (when (and path (/= (plist-get one :mode) 0))
            (puthash path t replaced)
            (push (vector path) targets))))
      (ngit-win-validate-worktree-records targets)
      (when index-write
        (let ((old (ngit-index-entries repo)) surviving)
          (ngit-win-validate-worktree-records old)
          (mapc (lambda (entry)
                  (unless (or (gethash (aref entry 0) removed)
                              (gethash (aref entry 0) replaced))
                    (push entry surviving))) old)
          (ngit-win-validate-worktree-records (append surviving targets)))))))

;;;###autoload
(defun ngit-cmd-apply (repo args &optional input)
  (let ((check nil) (cached nil) (index nil) (reverse nil) (stat nil) (files nil) p-value
        (case-fold-search nil))
    (while args
      (let ((arg (pop args)))
        (cond ((string= arg "--check") (setq check t))
              ((string= arg "--cached") (setq cached t))
              ((string= arg "--index") (setq index t))
              ((member arg '("-R" "--reverse")) (setq reverse t))
              ((string= arg "--stat") (setq stat t))
              ((string-match "\\`-p\\([0-9]+\\)\\'" arg)
               (setq p-value (string-to-number (match-string 1 arg))))
              ((and (string= arg "-p") args)
               (setq p-value (string-to-number (pop args))))
              ((string= arg "--") (setq files (append files args) args nil))
              ((string-prefix-p "-" arg) (ngit--die "ngit: apply: unsupported argument %s" arg))
              (t (setq files (append files (list arg)))))))
    (let* ((data (or input (ngit-patch--read-inputs files)))
           (patches (ngit-patch--parse data p-value)))
      (unless patches (ngit--die-with 128 "error: No valid patches in input"))
      (when reverse (setq patches (reverse patches)))
      (if stat (ngit-patch--stat patches reverse)
        (let ((prepared (ngit-patch--prepare repo patches cached reverse index)))
          (ngit-patch--preflight-windows repo prepared (or cached index))
          (unless check
            (ngit-patch--warn-whitespace data
                                         (if (= (length files) 1) (car files) "<stdin>")
                                         reverse))
          (unless check
            (unless cached (ngit-patch--write-worktree repo prepared))
            (when (or cached index)
              (ngit-patch--write-index repo prepared (not cached))))))
      0)))

;;;; am

(defun ngit-patch--mail-header (mail name)
  (let ((pos (string-match (concat "\n" (regexp-quote name) ": ") (concat "\n" mail))))
    (when pos
      (let* ((start (+ pos (length name) 3))
             (end (or (ngit--byte-pos (concat "\n" mail) ?\n start) (length mail))))
        (substring (concat "\n" mail) start end)))))

(defun ngit-patch--mail-patch-start (mail)
  "Find mailinfo's first patch boundary after the mail headers."
  (let ((body (or (string-search "\n\n" mail) 0))
        (case-fold-search nil))
    ;; mailinfo keeps this boundary in the patch; apply counts even lines
    ;; it skips before a valid traditional or Git diff header.
    (or (string-match
         "^\\(?:diff -\\|Index: \\|---\\(?: [^ \t\r\n\v\f]\\|[ \t\r\v\f]*\n\\)\\)"
         mail body)
        (length mail))))

(defun ngit-patch--mail-message (mail)
  (let* ((subject (or (ngit-patch--mail-header mail "Subject") ""))
         (subject (replace-regexp-in-string "\\`\\[PATCH[^]]*\\] *" "" subject))
         (sep (ngit-patch--mail-patch-start mail))
         (headers-end (string-search "\n\n" mail))
         (body (if (and sep headers-end (< (1+ headers-end) sep))
                   (substring mail (+ headers-end 2) sep) "")))
    (concat subject "\n" (if (string= body "") "" (concat "\n" (string-trim-right body) "\n")))))

(defun ngit-patch--mails (data)
  (let ((starts nil) (pos 0) (n (length data)) out)
    (when (string-prefix-p "From " data) (push 0 starts))
    ;; mailsplit also accepts a bare message beginning with From:.
    (when (string-prefix-p "From: " data) (push 0 starts))
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

(defun ngit-patch--am-parent (repo)
  "Resolve HEAD freshly after pre-applypatch, as do_commit does."
  (aset repo 4 nil)
  (let* ((name "HEAD") (value (ngit-read-ref repo name)) (depth 0))
    (while (and value (string-prefix-p "ref: " value) (< depth 6))
      (setq name (substring value 5) value (ngit-read-ref repo name)
            depth (1+ depth)))
    (if (and value (ngit-hex-p value 40)) (downcase value)
      (ngit-native-write-stderr "applying to an empty history\n")
      ;; An absent symref target is unborn. A present, invalid ref cannot
      ;; be updated: retain am state and fail before writing any commit.
      (when value
        (ngit--die-with 128
                        "fatal: update_ref failed for ref 'HEAD': cannot lock ref 'HEAD': unable to resolve reference '%s': reference broken"
                        name))
      nil)))

(defun ngit-patch--am-run (repo)
  (let ((status 0) current)
    (ngit-patch--am-write repo "msg" "")
    (while (and (= status 0) (setq current (ngit-patch--am-current repo)))
      (let* ((number (car current)) (mail (cdr current))
             (message (ngit-patch--mail-message mail))
             (message-file (expand-file-name "final-commit" (ngit-patch--am-dir repo)))
             (subject (ngit-patch--subject message))
             (patch-data (substring mail (ngit-patch--mail-patch-start mail)))
             committing)
        (ngit-patch--am-write repo "final-commit" message)
        (ngit-patch--am-write-author-state repo mail subject)
        (ngit-patch--am-write repo "patch" patch-data)
        (unless (ngit-patch--am-no-verify-p repo)
          (when (/= (ngit-hook-run repo "applypatch-msg"
                                   (list (ngit-hook-path-arg repo message-file))) 0)
            (ngit--die-with 1 ""))
          (setq message (ngit--read-file message-file)
                subject (ngit-patch--subject message)))
        (ngit--emit "Applying: " subject "\n")
        (condition-case err
            (let* ((patches (ngit-patch--parse patch-data))
                   (prepared (progn
                               (unless patches
                                 (ngit--die-with 128 "error: No valid patches in input (allow with \"--allow-empty\")"))
                               (ngit-patch--prepare repo patches nil nil t))))
                (ngit-patch--preflight-windows repo prepared t)
                (ngit-patch--write-worktree repo prepared)
              (ngit-patch--write-index repo prepared t)
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
                  (setq committing t)
                  (when hook-ran
                    (ngit-write-index repo index (aref index 0) root))
                  (let* ((head (ngit-patch--am-parent repo))
                         (author (ngit-patch--am-date-ident repo mail)))
                    (let ((ngit-commit-replay-action "am"))
                      (ngit--finish-commit repo head (and head (list head)) tree message nil t nil
                                           (ngit-index-file repo) (ngit-ident repo "COMMITTER") author
                                           nil t nil t)))
                  (ngit-hook-run repo "post-applypatch")
                  (ngit-ref-delete-state repo "REBASE_HEAD")
                  (ngit-patch--am-write repo "abort-safety"
                                        (concat (ngit-resolve-ref repo "HEAD") "\n"))
                  (ngit-patch--am-write repo "next" (format "%d\n" (1+ number))))))
          (error
           (when (or committing (and (eq (car err) 'ngit-fatal)
                      (or (equal (cadr err) "fatal: ref updates aborted by hook")
                          (string-prefix-p "fatal: size_t overflow: " (cadr err)))))
             (signal (car err) (cdr err)))
           (setq status 128)
           (when (and (cdr err) (stringp (cadr err)) (not (equal (cadr err) "")))
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

(defun ngit-patch--am-refresh-index (repo)
  "Refresh stat data before am's clean_index two-way reset."
  (let* ((index (ngit-read-index repo)) (entries (aref index 0))
         (checked (make-hash-table :test 'eql)))
    (ngit--reset-refresh repo index entries checked t)
    (ngit-write-index repo index entries (ngit-ct-root index) checked)))

(defun ngit-patch--am-reset-index (repo tree head-tree rev)
  "Refresh and reset for am, retaining clean_index's failure diagnostic."
  (ngit-patch--am-refresh-index repo)
  (condition-case err
      (ngit--reset-preserving repo tree head-tree rev nil)
    (ngit-fatal
     (let* ((message (cadr err))
            (tail (string-match
                   "\nfatal: Could not reset index file to revision '.*'\\.\\'" message)))
       (signal (car err)
               (cons (if tail (concat (substring message 0 tail)
                                     "\nfatal: failed to clean index") message)
                     (cddr err)))))))

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
          ;; clean_index uses a two-way merge, preserving unrelated dirty
          ;; worktree paths whose index entries already match the target.
          (ngit-patch--am-reset-index repo (ngit-seq-tree repo target)
                                     (ngit-seq-tree repo current) target)
          (ngit-patch--am-clear-branch-state repo)
          (ngit-update-head repo target current (ngit-ident repo "COMMITTER")
                            "am --abort")))))
    (ngit--switch-delete-directory-tree (ngit-patch--am-dir repo)) 0)
   ((string= control "--skip")
    (let* ((cur (ngit-patch--am-current repo)) (head (ngit-resolve-ref repo "HEAD")))
      (ngit-patch--am-reset-index repo (ngit-seq-tree repo head)
                                 (ngit-seq-tree repo head) head)
      (ngit-patch--am-clear-branch-state repo)
      (ngit-ref-delete-state repo "REBASE_HEAD")
      ;; am_next refreshes abort-safety on skip as well as after a commit.
      (aset repo 4 nil)
      (ngit-patch--am-write repo "abort-safety" (concat (ngit-resolve-ref repo "HEAD") "\n"))
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
          ;; do_commit resolves the parent after pre-applypatch, which may
          ;; move HEAD. Retain the index loaded before the hook, as Git does.
          (ngit-write-index repo index (aref index 0) root)
          (setq head (ngit-patch--am-parent repo))
          (ngit--finish-commit repo head (and head (list head)) tree message nil t nil (ngit-index-file repo)
                               (ngit-ident repo "COMMITTER") (ngit-patch--am-date-ident repo mail)
                               nil t nil t))
        (ngit-hook-run repo "post-applypatch")
        (ngit-ref-delete-state repo "REBASE_HEAD")
        (ngit-patch--am-write repo "abort-safety" (concat (ngit-resolve-ref repo "HEAD") "\n"))
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
