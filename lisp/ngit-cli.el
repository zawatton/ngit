;;; ngit-cli.el --- git-compatible read-only commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Command implementations whose stdout is meant to be byte-identical to
;; git's for the supported options: rev-parse, symbolic-ref, cat-file,
;; ls-tree and log (medium and raw formats).  `ngit-main' dispatches a
;; git-style argument list and returns the process exit status.

;;; Code:

(require 'ngit-rev)

(defvar ngit--out nil "Reversed list of pending output strings.")

(defun ngit--emit (&rest strings)
  (dolist (s strings) (push s ngit--out)))

(defun ngit--flush ()
  (when ngit--out
    (princ (apply #'concat (nreverse ngit--out)))
    (setq ngit--out nil)))

(defun ngit--die (fmt &rest args)
  (signal 'ngit-fatal (list (apply #'format fmt args))))

(define-error 'ngit-fatal "ngit fatal error")

;;;; Abbreviation

(defvar ngit--default-abbrev nil)

(defun ngit-default-abbrev (repo)
  "Git's automatic abbreviation length for REPO's approximate object count."
  (or ngit--default-abbrev
      (setq ngit--default-abbrev
            (let ((count 0) (msb -1))
              (dolist (p (ngit-repo-packs repo)) (setq count (+ count (aref p 2))))
              (while (> count 0) (setq count (ash count -1) msb (1+ msb)))
              (max 7 (/ (1+ (+ msb 2)) 2))))))

(defun ngit-abbrev (repo hex)
  "Return the shortest unique abbreviation of HEX, at least the default."
  (let ((len (ngit-default-abbrev repo)))
    (while (and (< len 40) (cdr (ngit-find-prefix repo (substring hex 0 len))))
      (setq len (1+ len)))
    (substring hex 0 len)))

;;;; Path quoting (core.quotePath=true)

(defconst ngit--octal-escapes
  (let ((v (make-vector 256 nil)) (i 0))
    (while (< i 256)
      (aset v i (string ?\\ (+ ?0 (ash i -6)) (+ ?0 (logand (ash i -3) 7))
                        (+ ?0 (logand i 7))))
      (setq i (1+ i)))
    v)
  "Precomputed \\NNN escapes; `format' is slow in NeLisp.")

(defun ngit--needs-quote-p (path)
  ;; An aref scan; a regexp match costs milliseconds per call in NeLisp.
  (let ((n (length path)) (i 0) (found nil))
    (while (and (< i n) (not found))
      (let ((c (aref path i)))
        (setq found (or (< c 32) (>= c 127) (= c ?\") (= c ?\\))
              i (1+ i))))
    found))

(defun ngit-quote-path (path)
  "Quote PATH the way git's quote_c_style does, when needed."
  (if (not (ngit--needs-quote-p path))
      path
    (let ((parts (list "\"")))
      (dotimes (i (length path))
        (let ((c (aref path i)))
          (push (cond ((= c ?\") "\\\"")
                      ((= c ?\\) "\\\\")
                      ((= c 7) "\\a") ((= c 8) "\\b") ((= c 9) "\\t")
                      ((= c 10) "\\n") ((= c 11) "\\v") ((= c 12) "\\f")
                      ((= c 13) "\\r")
                      ((or (< c 32) (>= c 127)) (aref ngit--octal-escapes c))
                      (t (string c)))
                parts)))
      (push "\"" parts)
      (apply #'concat (nreverse parts)))))

;;;; Dates

(defconst ngit--days ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"])
(defconst ngit--months ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep"
                        "Oct" "Nov" "Dec"])

(defun ngit--civil (days)
  "Return (YEAR MONTH DAY) for DAYS since 1970-01-01 (proleptic Gregorian)."
  (let* ((z (+ days 719468))
         (era (/ (if (>= z 0) z (- z 146096)) 146097))
         (doe (- z (* era 146097)))
         (yoe (/ (- doe (/ doe 1460) (- (/ doe 36524)) (/ doe 146096)) 365))
         (y (+ yoe (* era 400)))
         (doy (- doe (- (+ (* 365 yoe) (/ yoe 4)) (/ yoe 100))))
         (mp (/ (+ (* 5 doy) 2) 153))
         (d (1+ (- doy (/ (+ (* 153 mp) 2) 5))))
         (m (if (< mp 10) (+ mp 3) (- mp 9))))
    (list (if (<= m 2) (1+ y) y) m d)))

(defun ngit-format-date (stamp tz)
  "Format epoch STAMP in zone TZ (\"+0900\") like git's default date."
  (let* ((sign (if (string-prefix-p "-" tz) -1 1))
         (hh (string-to-number (substring tz 1 3)))
         (mm (string-to-number (substring tz 3 5)))
         (local (+ stamp (* sign (+ (* hh 3600) (* mm 60)))))
         (days (floor local 86400))
         (secs (- local (* days 86400)))
         (ymd (ngit--civil days)))
    (format "%s %s %d %02d:%02d:%02d %d %s"
            (aref ngit--days (mod (+ days 4) 7))
            (aref ngit--months (1- (nth 1 ymd)))
            (nth 2 ymd)
            (/ secs 3600) (/ (% secs 3600) 60) (% secs 60)
            (nth 0 ymd) tz)))

(defun ngit--split-ident (ident)
  "Split \"Name <email> STAMP TZ\" into (NAME-EMAIL STAMP TZ).
Parsed from the end with `aref'; a regexp costs milliseconds here."
  (let* ((n (length ident))
         (sp2 (let ((i (1- n))) (while (and (>= i 0) (/= (aref ident i) ?\s)) (setq i (1- i))) i))
         (sp1 (let ((i (1- sp2))) (while (and (>= i 0) (/= (aref ident i) ?\s)) (setq i (1- i))) i)))
    (if (and (> sp1 0) (= (aref ident (1- sp1)) ?>))
        (list (substring ident 0 sp1)
              (string-to-number (substring ident (1+ sp1) sp2))
              (substring ident (1+ sp2)))
      (list ident 0 "+0000"))))

;;;; rev-parse / symbolic-ref

(defun ngit-cmd-rev-parse (repo args)
  (let ((short nil) (abbrev-ref nil) (verify nil) (revs nil))
    (dolist (a args)
      (cond ((string= a "--verify") (setq verify t))
            ((string= a "--quiet") nil)
            ((string= a "-q") nil)
            ((string= a "--abbrev-ref") (setq abbrev-ref t))
            ((string= a "--short") (setq short t))
            ((string-prefix-p "--short=" a) (setq short (string-to-number (substring a 8))))
            ((string= a "--show-toplevel")
             (push (cons 'toplevel nil) revs))
            ((string= a "--is-inside-work-tree")
             (push (cons 'inside nil) revs))
            ((string-prefix-p "-" a) (ngit--die "unsupported rev-parse option %s" a))
            (t (push (cons 'rev a) revs))))
    (when (and verify (/= (length revs) 1))
      (ngit--die "fatal: Needed a single revision"))
    (dolist (r (nreverse revs))
      (pcase (car r)
        ('toplevel
         (ngit--emit (directory-file-name (file-truename (ngit-repo-toplevel repo))) "\n"))
        ('inside (ngit--emit (if (ngit-repo-toplevel repo) "true" "false") "\n"))
        ('rev
         (let ((rev (cdr r)))
           (if abbrev-ref
               (let ((val (ngit-read-ref repo rev)))
                 (ngit--emit
                  (cond ((and val (string-prefix-p "ref: refs/heads/" val)) (substring val 16))
                        ((and val (string-prefix-p "ref: " val)) (substring val 5))
                        (t (let ((d (ngit-dwim-ref repo rev)))
                             (cond ((null d) (ngit-rev-parse repo rev) rev)
                                   ((string-prefix-p "refs/heads/" (car d)) (substring (car d) 11))
                                   ((string-prefix-p "refs/" (car d)) (substring (car d) 5))
                                   (t (car d))))))
                  "\n"))
             (let ((hex (condition-case err (ngit-rev-parse repo rev)
                          (error
                           ;; git echoes an unresolvable argument before failing.
                           (unless verify (ngit--emit rev "\n"))
                           (signal (car err) (cdr err))))))
               (when verify
                 (unless (ngit-object-exists-p repo (ngit-unhex hex))
                   (ngit--die "fatal: Needed a single revision")))
               (ngit--emit (cond ((integerp short)
                                  (let ((ngit--default-abbrev (max 4 short)))
                                    (ngit-abbrev repo hex)))
                                 (short (ngit-abbrev repo hex))
                                 (t hex))
                           "\n")))))))
    0))

(defun ngit-cmd-symbolic-ref (repo args)
  (let* ((short (member "--short" args))
         (name (or (let ((r nil)) (dolist (a args) (unless (or r (string-prefix-p "-" a)) (setq r a))) r) "HEAD"))
         (val (ngit-read-ref repo name)))
    (unless (and val (string-prefix-p "ref: " val))
      (ngit--die "fatal: ref %s is not a symbolic ref" name))
    (let ((target (substring val 5)))
      (ngit--emit (if (and short (string-prefix-p "refs/heads/" target))
                      (substring target 11)
                    target)
                  "\n"))
    0))

;;;; cat-file / ls-tree

(defun ngit--tree-line (mode type hex path)
  (concat (make-string (max 0 (- 6 (length mode))) ?0) mode " " type " " hex
          "\t" (ngit-quote-path path) "\n"))

(defun ngit--emit-tree (repo hex prefix recursive show-trees name-only)
  (dolist (e (ngit-parse-tree (cdr (ngit-read-object repo (ngit-unhex hex)))))
    (let* ((mode (nth 0 e))
           (type (ngit-mode-type mode))
           (path (concat prefix (nth 1 e)))
           (ehex (ngit-hex (nth 2 e)))
           (descend (and recursive (string= type "tree"))))
      (when (or (not descend) show-trees)
        (ngit--emit (if name-only
                        (concat (ngit-quote-path path) "\n")
                      (ngit--tree-line mode type ehex path))))
      (when descend
        (ngit--emit-tree repo ehex (concat path "/") t show-trees name-only)))))

(defun ngit-cmd-cat-file (repo args)
  (let ((mode (car args)) (name (cadr args)))
    (unless (and mode name)
      (ngit--die "usage: git cat-file (-t | -s | -e | -p | <type>) <object>"))
    (let ((hex (condition-case nil (ngit-rev-parse repo name) (error nil))))
      (if (string= mode "-e")
          (if (and hex (ngit-object-exists-p repo (ngit-unhex hex))) 0 1)
        (unless hex (ngit--die "fatal: Not a valid object name %s" name))
        (let ((obj (ngit-read-object repo (ngit-unhex hex))))
          (cond
           ((string= mode "-t") (ngit--emit (symbol-name (car obj)) "\n"))
           ((string= mode "-s") (ngit--emit (number-to-string (length (cdr obj))) "\n"))
           ((string= mode "-p")
            (if (eq (car obj) 'tree)
                (ngit--emit-tree repo hex "" nil nil nil)
              (ngit--emit (cdr obj))))
           ((member mode '("blob" "tree" "commit" "tag"))
            (let ((want (intern mode)))
              (ngit--emit (cdr (if (eq (car obj) want)
                                   obj
                                 (ngit-read-object repo (ngit-unhex (ngit-peel repo hex want))))))))
           (t (ngit--die "fatal: unsupported cat-file mode %s" mode))))
        0))))

(defun ngit-cmd-ls-tree (repo args)
  (let ((recursive nil) (show-trees nil) (name-only nil) (rev nil))
    (dolist (a args)
      (cond ((string= a "-r") (setq recursive t))
            ((string= a "-t") (setq show-trees t))
            ((member a '("--name-only" "--name-status")) (setq name-only t))
            ((string-prefix-p "-" a) (ngit--die "unsupported ls-tree option %s" a))
            ((null rev) (setq rev a))
            (t (ngit--die "ngit: ls-tree path filters are not supported"))))
    (unless rev (ngit--die "usage: git ls-tree [<options>] <tree-ish>"))
    (ngit--emit-tree repo (ngit-peel repo (ngit-rev-parse repo rev) 'tree) "" recursive
                     (or show-trees (not recursive)) name-only)
    0))

;;;; log

(defun ngit--indent-message (msg)
  "Indent each line of MSG by four spaces the way git's pretty printer does."
  (let ((lines (split-string msg "\n")))
    ;; Drop the empty string after the final newline.
    (when (and lines (string= (car (last lines)) ""))
      (setq lines (butlast lines)))
    (mapconcat (lambda (l) (concat "    " l "\n")) lines "")))

(defun ngit--commit-date (parsed)
  (nth 1 (ngit--split-ident (or (ngit-header parsed "committer") ""))))

(defun ngit--log-entry (repo hex data parsed format)
  (if (eq format 'raw)
      (let ((end (ngit--header-end data)))
        (concat "commit " hex "\n" (substring data 0 end) "\n\n"
                (ngit--indent-message (plist-get parsed :message))))
    (let* ((parents (ngit-header-all parsed "parent"))
           (author (ngit--split-ident (or (ngit-header parsed "author") ""))))
      (concat "commit " hex "\n"
              (if (cdr parents)
                  (concat "Merge: "
                          (mapconcat (lambda (p) (ngit-abbrev repo p)) parents " ")
                          "\n")
                "")
              "Author: " (nth 0 author) "\n"
              "Date:   " (ngit-format-date (nth 1 author) (nth 2 author)) "\n\n"
              (ngit--indent-message (plist-get parsed :message))))))

(defun ngit-cmd-log (repo args)
  (let ((limit -1) (format 'medium) (revs nil))
    (while args
      (let ((a (pop args)))
        (cond ((string= a "-n") (setq limit (string-to-number (pop args))))
              ((string-match "\\`-n?\\([0-9]+\\)\\'" a)
               (setq limit (string-to-number (match-string 1 a))))
              ((string-prefix-p "--max-count=" a)
               (setq limit (string-to-number (substring a 12))))
              ((member a '("--format=raw" "--pretty=raw")) (setq format 'raw))
              ((member a '("--format=medium" "--pretty=medium" "--pretty" "--no-decorate"))
               (setq format 'medium))
              ((string-prefix-p "-" a) (ngit--die "unsupported log option %s" a))
              (t (push a revs)))))
    (let ((seen (make-hash-table :test 'equal))
          (queue nil)
          (counter 0)
          (first t))
      ;; QUEUE holds (DATE COUNTER HEX DATA PARSED), newest date first,
      ;; insertion order among equal dates, like git's prio_queue.
      (let ((enqueue
             (lambda (hex)
                  (unless (gethash hex seen)
                    (puthash hex t seen)
                    (let* ((data (cdr (ngit-read-object repo (ngit-unhex hex))))
                           (parsed (ngit-parse-commit data))
                           (item (list (ngit--commit-date parsed) (setq counter (1+ counter))
                                       hex data parsed)))
                      (if (or (null queue) (> (car item) (caar queue)))
                          (push item queue)
                        (let ((cell queue))
                          (while (and (cdr cell) (<= (car item) (car (cadr cell))))
                            (setq cell (cdr cell)))
                          (setcdr cell (cons item (cdr cell))))))))))
        (dolist (r (or (nreverse revs) '("HEAD")))
          (funcall enqueue (ngit-peel repo (ngit-rev-parse repo r) 'commit)))
        (while (and queue (/= limit 0))
          (let ((item (pop queue)))
            (unless first (ngit--emit "\n"))
            (setq first nil)
            (ngit--emit (ngit--log-entry repo (nth 2 item) (nth 3 item) (nth 4 item) format))
            (setq limit (1- limit))
            (when (/= limit 0)
              (dolist (p (ngit-header-all (nth 4 item) "parent"))
                (funcall enqueue p)))))))
    0))

(defun ngit-cmd-test-cat-all (repo _args)
  "Test hook: `cat-file -t' and `-p' for every name in $NGIT_BATCH_LIST.
Each object is also verified against its SHA-1 before it is printed."
  (dolist (hex (split-string (ngit--read-file (getenv "NGIT_BATCH_LIST")) "\n" t))
    (let* ((obj (ngit-read-object repo (ngit-unhex hex)))
           (type (symbol-name (car obj))))
      (unless (string= hex (secure-hash 'sha1 (concat type " "
                                                      (number-to-string (length (cdr obj)))
                                                      "\0" (cdr obj))))
        (ngit--die "SHA-1 mismatch for %s" hex))
      (ngit--emit type "\n")
      (if (eq (car obj) 'tree)
          (ngit--emit-tree repo hex "" nil nil nil)
        (ngit--emit (cdr obj)))))
  0)

;;;; Dispatch

(defconst ngit--commands
  '(("test-cat-all" . ngit-cmd-test-cat-all)
    ("rev-parse" . ngit-cmd-rev-parse)
    ("symbolic-ref" . ngit-cmd-symbolic-ref)
    ("cat-file" . ngit-cmd-cat-file)
    ("ls-tree" . ngit-cmd-ls-tree)
    ("log" . ngit-cmd-log)
    ("ls-files" . ngit-cmd-ls-files)
    ("config" . ngit-cmd-config)
    ("check-attr" . ngit-cmd-check-attr)
    ("status" . ngit-cmd-status)
    ("diff" . ngit-cmd-diff)))

(defun ngit-main (args)
  "Run the git-style command line ARGS; return the exit status."
  (let ((default-directory default-directory)
        (ngit--out nil))
    (while (and args (string= (car args) "-C"))
      (setq default-directory (file-name-as-directory
                               (expand-file-name (cadr args)))
            args (cddr args)))
    (let ((cmd (assoc (car args) ngit--commands)))
      (if (null cmd)
          (progn (ngit-native-write-stderr (format "ngit: unsupported command: %s\n" (car args))) 1)
        (condition-case err
            (prog1 (funcall (cdr cmd) (ngit-discover) (cdr args))
              (ngit--flush))
          (error
           (ngit--flush)
           (ngit-native-write-stderr
            (concat (if (eq (car err) 'ngit-fatal)
                        (cadr err)
                      (error-message-string err))
                    "\n"))
           128))))))

(provide 'ngit-cli)

;;; ngit-cli.el ends here
