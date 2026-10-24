;;; ngit-branch.el --- the branch command -*- lexical-binding: t; -*-

;;; Commentary:

;; `git branch': listing (with -v/-vv tracking, worktrees, detached HEAD,
;; --list patterns, --contains/--merged filters and --sort), creation with
;; upstream tracking, deletion, renaming and copying, and upstream changes.
;;
;; Config changes follow config.c: keys are set by the same parse-event
;; offsets (git_config_set_multivar_in_file) and sections are renamed,
;; copied or removed line by line (git_config_copy_or_rename_section), so
;; the rest of the file is left byte for byte as git leaves it.

;;; Code:

(require 'ngit-log)

;;;; Config file events (config.c git_parse_source with event_fn)

(defun ngit--cfg-space-p (c) (memq c '(?\s ?\t ?\n ?\r ?\f ?\v)))

(defun ngit--cfg-alpha-p (c) (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z))))

(defun ngit--cfg-keychar-p (c)
  (or (ngit--cfg-alpha-p c) (and (>= c ?0) (<= c ?9)) (= c ?-)))

(defun ngit--cfg-parse-section (text pos)
  "Parse a section header after `[' at POS; return (VAR . END) where VAR
is \"section.subsection\" (section lowercased) and END follows `]'."
  (let ((n (length text)) (name nil) (sub nil) (i pos))
    (while (and (< i n) (or (ngit--cfg-keychar-p (aref text i)) (= (aref text i) ?.)))
      (push (downcase (aref text i)) name)
      (setq i (1+ i)))
    (when (and (< i n) (ngit--cfg-space-p (aref text i)))
      (while (and (< i n) (ngit--cfg-space-p (aref text i))) (setq i (1+ i)))
      (when (and (< i n) (= (aref text i) ?\"))
        (setq i (1+ i))
        (while (and (< i n) (/= (aref text i) ?\"))
          (when (= (aref text i) ?\\) (setq i (1+ i)))
          (when (< i n) (push (aref text i) sub))
          (setq i (1+ i)))
        (setq i (1+ i))))
    (while (and (< i n) (/= (aref text i) ?\])) (setq i (1+ i)))
    (cons (concat (apply #'string (nreverse name))
                  (if sub (concat "." (apply #'string (nreverse sub))) ""))
          (min n (1+ i)))))

(defun ngit--cfg-parse-entry (text pos)
  "Parse an entry starting at POS; return (NAME VALUE END), END after the
line's newline.  VALUE is t for a key without `='."
  (let ((n (length text)) (i pos) (name nil) (value nil))
    (while (and (< i n) (ngit--cfg-keychar-p (aref text i)))
      (push (downcase (aref text i)) name)
      (setq i (1+ i)))
    (while (and (< i n) (memq (aref text i) '(?\s ?\t))) (setq i (1+ i)))
    (if (or (>= i n) (/= (aref text i) ?=))
        ;; No value: the rest of the line (a comment at most).
        (progn
          (while (and (< i n) (/= (aref text i) ?\n)) (setq i (1+ i)))
          (setq value t))
      (setq i (1+ i))
      (let ((quote nil) (comment nil) (acc nil) (done nil))
        (while (and (< i n) (not done))
          (let ((c (aref text i)))
            (cond
             ((= c ?\n) (if quote (setq i (1+ i)) (setq done t)))
             (comment (setq i (1+ i)))
             ((and (not quote) (memq c '(?\; ?#))) (setq comment t i (1+ i)))
             ((= c ?\\)
              (let ((d (and (< (1+ i) n) (aref text (1+ i)))))
                (cond ((eq d ?\n) nil)
                      ((eq d ?n) (push ?\n acc))
                      ((eq d ?t) (push ?\t acc))
                      ((eq d ?b) (when acc (pop acc)))
                      (d (push d acc)))
                (setq i (+ i 2))))
             ((= c ?\") (setq quote (not quote) i (1+ i)))
             (t (push c acc) (setq i (1+ i))))))
        (setq value (string-trim-right (apply #'string (nreverse acc))))))
    (when (< i n) (setq i (1+ i)))
    (list (apply #'string (nreverse name)) value i)))

(defun ngit--cfg-events (text)
  "Parse events of config TEXT: a vector of [TYPE BEGIN END VAR VALUE],
TYPE one of whitespace, comment, section, entry; VAR the section for a
section event and the full key for an entry."
  (let ((n (length text)) (pos 0) (events nil) (base "") (comment nil) (prev nil))
    (while (< pos n)
      (let ((c (aref text pos)))
        (cond
         ((= c ?\n)
          (unless (eq (car prev) 'whitespace)
            (setq prev (list 'whitespace pos nil nil))
            (push prev events))
          (setq comment nil pos (1+ pos)))
         (comment (setq pos (1+ pos)))
         ((ngit--cfg-space-p c)
          (unless (eq (car prev) 'whitespace)
            (setq prev (list 'whitespace pos nil nil))
            (push prev events))
          (setq pos (1+ pos)))
         ((memq c '(?# ?\;))
          (setq prev (list 'comment pos nil nil) comment t)
          (push prev events)
          (setq pos (1+ pos)))
         ((= c ?\[)
          (let ((r (ngit--cfg-parse-section text (1+ pos))))
            (setq base (car r))
            (setq prev (list 'section pos (car r) nil))
            (push prev events)
            (setq pos (cdr r))))
         ((ngit--cfg-alpha-p c)
          (let ((r (ngit--cfg-parse-entry text pos)))
            (setq prev (list 'entry pos (concat base "." (nth 0 r)) (nth 1 r)))
            (push prev events)
            (setq pos (nth 2 r))))
         (t (ngit--die "fatal: bad config line in %s" "config")))))
    (setq events (nreverse events))
    ;; Each event ends where the next one begins.
    (let ((v (make-vector (length events) nil)) (i 0) (rest events))
      (while rest
        (let ((e (car rest)) (next (cadr rest)))
          (aset v i (vector (nth 0 e) (nth 1 e) (if next (nth 1 next) n) (nth 2 e) (nth 3 e))))
        (setq rest (cdr rest) i (1+ i)))
      v)))

(defun ngit--cfg-split-key (key)
  "Return (BASE . NAME) of KEY: BASE \"section.sub\" normalized, NAME lowercased."
  (let ((last (let ((i (1- (length key)))) (while (and (>= i 0) (/= (aref key i) ?.)) (setq i (1- i))) i))
        (first (string-search "." key)))
    (cons (concat (downcase (substring key 0 first)) (substring key first last))
          (downcase (substring key (1+ last))))))

(defun ngit--cfg-section-header (base)
  "store_create_section for BASE (\"section.sub\" or \"section\")."
  (let ((dot (string-search "." base)))
    (if (null dot)
        (concat "[" base "]\n")
      (concat "[" (substring base 0 dot) " \""
              (mapconcat (lambda (ch) (if (memq ch '(?\" ?\\)) (string ?\\ ch) (string ch)))
                         (substring base (1+ dot)) "")
              "\"]\n"))))

(defun ngit--cfg-pair (name value)
  "write_pair: \"\\tNAME = VALUE\\n\" with git's quoting."
  (let ((quote (if (or (and (> (length value) 0) (= (aref value 0) ?\s))
                       (seq-find (lambda (ch) (memq ch '(?\; ?# ?\r))) value)
                       (and (> (length value) 0) (= (aref value (1- (length value))) ?\s)))
                   "\"" "")))
    (concat "\t" name " = " quote
            (mapconcat (lambda (ch)
                         (pcase ch
                           (?\n "\\n") (?\t "\\t") (?\" "\\\"") (?\\ "\\\\")
                           (_ (string ch))))
                       value "")
            quote "\n")))

(defun ngit--config-file (repo) (expand-file-name "config" (ngit-repo-commondir repo)))

(defun ngit--config-write (repo text)
  (let* ((file (ngit--config-file repo))
         (mode (or (and (file-exists-p file) (file-modes file))
                   (ngit-native-creation-mode #o666)))
         (lock (ngit-lock file 'defer)))
    (ngit-commit-lock lock (encode-coding-string text 'utf-8) mode)
    (setq ngit--config-cache nil)))

(defun ngit-config-set (repo key value &optional add)
  "Set KEY to VALUE in REPO's config, or unset it when VALUE is nil; with
ADD, append another value (git's CONFIG_REGEX_NONE).  Return nil when
nothing was set (git's CONFIG_NOTHING_SET)."
  (let* ((file (ngit--config-file repo))
         (text (and (file-exists-p file) (decode-coding-string (ngit--read-file file) 'utf-8)))
         (split (ngit--cfg-split-key key))
         (base (car split))
         (full (concat base "." (cdr split))))
    (if (null text)
        (when value
          (ngit--config-write repo (concat (ngit--cfg-section-header base)
                                           (ngit--cfg-pair (cdr split) value)))
          t)
      (let* ((events (ngit--cfg-events text))
             (nev (length events))
             (slot nil) (matches nil) (key-seen nil) (section-seen nil) (in-keys nil)
             (keys-flags (make-vector nev nil)))
        ;; store_aux_event and store_aux.
        (dotimes (i nev)
          (let ((e (aref events i)))
            (pcase (aref e 0)
              ('section
               (setq in-keys (string= (aref e 3) base))
               (aset keys-flags i in-keys)
               (when in-keys
                 (setq section-seen t)
                 (unless key-seen (setq slot i))))
              ('entry
               (let ((match (and (not add) (string= (aref e 3) full))))
                 (cond (key-seen (when match (push i matches)))
                       (in-keys
                        (setq slot i section-seen t)
                        (when match (setq key-seen t) (push i matches)))))))))
        (setq matches (nreverse matches))
        (cond
         ((and (null matches) (null value)) nil)
         ((and (cdr matches) value) nil)
         (t
          (let ((seen (vconcat (or matches (list (or slot (max 0 (1- nev)))))))
                (out nil) (copy-begin 0) (n (length text)) (k 0))
            (while (< k (length seen))
              (let* ((j (aref seen k)) (e (aref events j)) (copy-end 0) (replace-end 0))
                (if (not key-seen)
                    (progn
                      (setq copy-end (if (> nev 0) (aref e 2) 0))
                      (when (and (> copy-end 0) (< copy-end n)
                                 (/= (aref text (1- copy-end)) ?\n)
                                 (= (aref text copy-end) ?\n))
                        (setq copy-end (1+ copy-end)))
                      (setq replace-end copy-end))
                  (setq replace-end (aref e 2) copy-end (aref e 1))
                  (unless value
                    (let ((r (ngit--cfg-maybe-remove-section events keys-flags seen k)))
                      (when r
                        (setq k (nth 0 r) copy-end (nth 1 r) replace-end (nth 2 r)))))
                  (while (and (> copy-end 0)
                              (let ((c (aref text (1- copy-end))))
                                (and (ngit--cfg-space-p c) (/= c ?\n))))
                    (setq copy-end (1- copy-end))))
                (when (> copy-end copy-begin)
                  (push (substring text copy-begin copy-end) out)
                  (when (/= (aref text (1- copy-end)) ?\n) (push "\n" out)))
                (setq copy-begin replace-end))
              (setq k (1+ k)))
            (when value
              (unless section-seen (push (ngit--cfg-section-header base) out))
              (push (ngit--cfg-pair (cdr split) value) out))
            (when (< copy-begin n) (push (substring text copy-begin) out))
            (ngit--config-write repo (apply #'concat (nreverse out)))
            t)))))))

(defun ngit--cfg-maybe-remove-section (events keys-flags seen k)
  "maybe_remove_section for the entry SEEN[K]: return (K BEGIN END) to
remove the whole section, or nil to remove the entry only."
  (catch 'keep
    (let ((i (aref seen k)) (section-seen nil) (stop nil))
      (while (and (not stop) (> i 0))
        (let ((type (aref (aref events (1- i)) 0)))
          (cond ((eq type 'comment) (throw 'keep nil))
                ((eq type 'entry)
                 (if section-seen (setq stop t) (throw 'keep nil)))
                ((eq type 'section)
                 (if (aref keys-flags (1- i))
                     (setq section-seen t)
                   (setq stop t)))))
        (unless stop (setq i (1- i))))
      (let ((begin (aref (aref events i) 1))
            (i (1+ (aref seen k)))
            (nev (length events))
            (stop nil))
        (while (and (not stop) (< i nev))
          (let ((type (aref (aref events i) 0)))
            (cond ((eq type 'comment) (throw 'keep nil))
                  ((eq type 'section)
                   (if (aref keys-flags i) (setq i (1+ i)) (setq stop t)))
                  ((eq type 'entry)
                   (setq k (1+ k))
                   (if (and (< k (length seen)) (= i (aref seen k)))
                       (setq i (1+ i))
                     (throw 'keep nil)))
                  (t (setq i (1+ i))))))
        (list k begin (if (< i nev) (aref (aref events i) 1) (aref (aref events (1- nev)) 2)))))))

(defun ngit--cfg-section-name-match (buf name)
  "section_name_match: offset after the header in BUF when it is section
NAME (\"branch.x\"), else 0."
  (let ((n (length buf)) (nlen (length name)) (i 1) (j 0) (dot nil) (brk nil)
        (at (lambda (s idx len) (if (< idx len) (aref s idx) 0))))
    (if (or (= n 0) (/= (aref buf 0) ?\[))
        0
      (while (and (not brk) (< i n) (/= (aref buf i) ?\]))
        (let ((c (aref buf i)))
          (if (and (not dot) (ngit--cfg-space-p c))
              (progn
                (setq dot t)
                (if (/= (funcall at name j nlen) ?.)
                    (setq brk t j (1+ j))
                  (setq j (1+ j) i (1+ i))
                  (while (and (< i n) (ngit--cfg-space-p (aref buf i))) (setq i (1+ i)))
                  (if (/= (funcall at buf i n) ?\")
                      (setq brk t)
                    (setq i (1+ i)))))
            (cond ((and (= c ?\\) dot) (setq i (1+ i)))
                  ((and (= c ?\") dot)
                   (setq i (1+ i))
                   (while (and (< i n) (ngit--cfg-space-p (aref buf i))) (setq i (1+ i)))
                   (setq brk 'quote)))
            (unless brk
              (if (/= (funcall at buf i n) (funcall at name j nlen))
                  (setq brk t j (1+ j))
                (setq j (1+ j) i (1+ i)))))))
      (if (and (= (funcall at buf i n) ?\]) (= j nlen))
          (progn
            (setq i (1+ i))
            (while (and (< i n) (ngit--cfg-space-p (aref buf i))) (setq i (1+ i)))
            i)
        0))))

(defun ngit-config-rename-section (repo old new &optional copy)
  "Rename section OLD (\"branch.x\") to NEW, remove it when NEW is nil, or
copy it with COPY; return the number of sections matched."
  (let* ((file (ngit--config-file repo))
         (count 0))
    (when (file-exists-p file)
      (let* ((text (decode-coding-string (ngit--read-file file) 'utf-8))
             (lines (let ((acc nil) (pos 0) (n (length text)))
                      (while (< pos n)
                        (let* ((nl (string-search "\n" text pos)) (end (if nl (1+ nl) n)))
                          (push (substring text pos end) acc)
                          (setq pos end)))
                      (nreverse acc)))
             (out nil) (remove nil) (copystr nil))
        (dolist (line lines)
          (let ((i 0) (output line) (is-section nil) (skip nil))
            (while (and (< i (length line)) (ngit--cfg-space-p (aref line i))) (setq i (1+ i)))
            (when (and (< i (length line)) (= (aref line i) ?\[))
              (setq is-section t)
              (when copystr (push copystr out) (setq copystr nil))
              (let ((offset (ngit--cfg-section-name-match (substring line i) old)))
                (when (> offset 0)
                  (setq count (1+ count))
                  (if (null new)
                      (setq remove t skip t)
                    (if (not copy)
                        (let ((rest (substring line (+ i offset))))
                          (push (ngit--cfg-section-header new) out)
                          (setq output (if (> (length rest) 0) (concat "\t" rest) "")))
                      (setq copystr (ngit--cfg-section-header new))))))
              (unless skip (setq remove nil)))
            (unless (or skip remove)
              (when (and (not is-section) copystr)
                (setq copystr (concat copystr output)))
              (push output out))))
        (when copystr (push copystr out))
        (ngit--config-write repo (apply #'concat (nreverse out)))))
    count))

;;;; Errors and messages

(defun ngit--branch-error (fmt &rest args)
  "Print git's \"error: ...\" line to stderr."
  (ngit-native-write-stderr (concat "error: " (apply #'format fmt args) "\n")))

(defun ngit--branch-warning (fmt &rest args)
  (ngit-native-write-stderr (concat "warning: " (apply #'format fmt args) "\n")))

;;;; Branch names (check_refname_format for refs/heads/NAME)

(defun ngit--valid-branch-name-p (name)
  "Non-nil when refs/heads/NAME passes git's check_refname_format."
  (and (> (length name) 0)
       (not (string= name "HEAD"))
       (not (string-prefix-p "-" name))
       (let ((ok t))
         (dolist (comp (split-string name "/"))
           (when (or (string= comp "")
                     (string-prefix-p "." comp)
                     (string-suffix-p ".lock" comp))
             (setq ok nil)))
         (dotimes (i (length name))
           (let ((c (aref name i)))
             (when (or (< c 32) (= c 127) (memq c '(?\s ?~ ?^ ?: ?? ?* ?\[ ?\\)))
               (setq ok nil))
             (when (and (= c ?.) (< (1+ i) (length name)) (= (aref name (1+ i)) ?.))
               (setq ok nil))
             (when (and (= c ?@) (< (1+ i) (length name)) (= (aref name (1+ i)) ?{))
               (setq ok nil))))
         (and ok (not (string-suffix-p "." name)) (not (string= name "@"))))))

(defun ngit--check-branch-name (name)
  (unless (ngit--valid-branch-name-p name)
    (ngit--die "fatal: '%s' is not a valid branch name\nhint: See `man git check-ref-format`\nhint: Disable this message with \"git config advice.refSyntax false\"" name)))

;;;; Worktrees

(defun ngit--worktrees (repo)
  "List of (PATH HEAD-REF HEAD-OID) for the main and linked worktrees."
  (let* ((common (ngit-repo-commondir repo))
         (main-path (directory-file-name (file-name-directory (directory-file-name common))))
         (read-head (lambda (dir)
                      (let ((f (expand-file-name "HEAD" dir)))
                        (and (file-exists-p f) (ngit--trim-eol (ngit--read-file f))))))
         (out nil))
    (let ((h (funcall read-head common)))
      (push (list main-path (and h (string-prefix-p "ref: " h) (substring h 5))) out))
    (let ((dir (expand-file-name "worktrees" common)))
      (when (file-directory-p dir)
        (dolist (name (sort (directory-files dir nil "\\`[^.]") #'string<))
          (let* ((wd (expand-file-name name dir))
                 (gitdir (expand-file-name "gitdir" wd)))
            (when (file-exists-p gitdir)
              (let ((h (funcall read-head wd))
                    (path (directory-file-name
                           (file-name-directory (ngit--trim-eol (ngit--read-file gitdir))))))
                (push (list path (and h (string-prefix-p "ref: " h) (substring h 5))) out)))))))
    (nreverse out)))

(defun ngit--branch-checked-out (repo ref)
  "Path of a worktree that has REF checked out, or nil."
  (let ((found nil))
    (dolist (wt (ngit--worktrees repo))
      (when (and (not found) (equal (nth 1 wt) ref)) (setq found (nth 0 wt))))
    found))

;;;; Refs

(defun ngit--branch-ident (repo)
  (let ((ngit-native-write-stderr-orig nil))
    (ignore ngit-native-write-stderr-orig)
    (ngit-ident repo "COMMITTER")))

(defun ngit--write-worktree-head-symref (repo gitdir newref)
  "Point the HEAD in GITDIR at NEWREF through a reference transaction."
  (let ((view (copy-sequence repo)) lock input)
    (aset view 0 (file-name-as-directory gitdir))
    (setq lock (ngit-lock (ngit--ref-file view "HEAD")))
    (unwind-protect
        (progn
          ;; Use the invoking repository for hook cwd/environment while the
          ;; cloned view selects the linked worktree's private HEAD file.
          (setq input
                (ngit-ref-prepare repo
                                  (list (list nil (concat "ref:" newref) "HEAD"))
                                  nil (list lock)))
          (ngit-commit-lock lock (concat "ref: " newref "\n"))
          (setq lock nil)
          (ngit-ref-finish repo input))
      (when lock (ngit-unlock lock)))))

(defun ngit--write-ref (repo ref new old msg &optional unchecked)
  "Point REF at NEW (hex), logging MSG with OLD as the previous value.
UNCHECKED corresponds to a NULL old OID in Git's reference transaction."
  (let* ((file (ngit--ref-file repo ref)))
    (ngit-make-directory (file-name-directory file) t)
    (let ((lock (ngit-lock file)) input)
      (unwind-protect
          (progn
            (unless unchecked
              (aset repo 4 nil)
              (let ((current (ngit-resolve-ref repo ref)))
                (unless (equal current old)
                  (ngit--die "fatal: cannot lock ref '%s': is at %s but expected %s"
                             ref (or current (make-string 40 ?0))
                             (or old (make-string 40 ?0))))))
            (setq input (ngit-ref-prepare repo (list (list (unless unchecked old) new ref)) nil (list lock)))
            ;; A ref that already has the value is not rewritten, so a
            ;; packed ref stays packed.
            (if (equal (ngit-read-ref repo ref) new)
                (ngit-unlock lock)
              (ngit-commit-lock lock (concat new "\n")))
            (setq lock nil)
            (when msg
              (ngit--append-reflog repo ref old new (ngit--branch-ident repo) msg))
            (aset repo 4 nil)
            (ngit-ref-finish repo input))
        (when lock (ngit-unlock lock))))
    nil))

(defun ngit--rewrite-packed-refs (repo drop &optional held-lock)
  "Remove the refs in DROP from packed-refs (with their peeled lines)."
  (let ((file (expand-file-name "packed-refs" (ngit-repo-commondir repo))))
    (when (file-exists-p file)
      (let* ((text (ngit--read-file file))
             (lines (split-string text "\n"))
             (out nil) (skip-peel nil) (changed nil))
        (when (string= (car (last lines)) "") (setq lines (butlast lines)))
        (dolist (line lines)
          (cond
           ((and skip-peel (string-prefix-p "^" line)) (setq changed t))
           ((and (> (length line) 41) (member (substring line 41) drop))
            (setq skip-peel t changed t))
           (t (setq skip-peel nil) (push line out))))
        (when changed
          (let ((lock (or held-lock (ngit-lock file))))
            (ngit-commit-lock lock (mapconcat (lambda (l) (concat l "\n")) (nreverse out) ""))))
        (aset repo 4 nil)))))

(defun ngit--remove-empty-dirs (dir stop)
  "Remove DIR and its empty parents up to (not including) STOP."
  (let ((d (directory-file-name dir)) (stop (directory-file-name stop)))
    (while (and (string-prefix-p (concat stop "/") d)
                (file-directory-p d)
                (null (directory-files d nil "\\`[^.]\\|\\`\\.[^.]")))
      (delete-directory d)
      (setq d (directory-file-name (file-name-directory d))))))

(defun ngit--delete-ref (repo ref &optional held-lock old-value head-old-value)
  "Delete REF: its loose file, packed entry and reflog.
HELD-LOCK, when non-nil, is an already-acquired lock for REF and is consumed.
OLD-VALUE is reported to reference-transaction for rename-style deletion.
HEAD-OLD-VALUE adds the dereferenced current HEAD deletion to that transaction."
  (let* ((common (ngit-repo-commondir repo))
         (file (ngit--ref-file repo ref))
         (log (expand-file-name (concat "logs/" ref) common))
         (updates (append (list (list old-value nil ref))
                          (and head-old-value
                               (list (list head-old-value nil "HEAD")))))
         (packed-updates (list (list nil nil ref)))
         (packed (cdr (assoc ref (ngit--packed-refs repo))))
         lock head-lock packed-lock packed-input input)
    (ngit-make-directory (file-name-directory file) t)
    (setq lock (or held-lock (ngit-lock file)))
    (unwind-protect
        (progn
          (when head-old-value
            (setq head-lock (ngit-lock (ngit--ref-file repo "HEAD"))))
          (setq packed-lock (ngit-lock (expand-file-name "packed-refs" common)))
          (let* ((outer-locks (append (list lock)
                                      (and head-lock (list head-lock))))
                 (all-locks (append outer-locks (list packed-lock))))
            (when packed
              (setq packed-input
                    (ngit-ref-prepare repo packed-updates nil all-locks)))
            (setq input (ngit-ref-prepare repo updates (not packed)
                                          (if packed outer-locks all-locks)
                                          (and packed-input (list packed-input))
                                          packed-updates
                                          (and packed (list packed-lock)))))
          (ngit--rewrite-packed-refs repo (list ref) packed-lock)
          (ngit-ref-finish repo packed-input)
          (when (file-exists-p file) (delete-file file))
          (when (file-exists-p log)
            (delete-file log)
            (ngit--remove-empty-dirs (file-name-directory log) (expand-file-name "logs/refs" common)))
          (ngit-unlock lock)
          (when head-lock (ngit-unlock head-lock))
          (ngit-unlock packed-lock)
          (ngit--remove-empty-dirs (file-name-directory file) (expand-file-name "refs" common))
          (aset repo 4 nil)
          (ngit-ref-finish repo input))
      (ngit-unlock lock)
      (when head-lock (ngit-unlock head-lock))
      (when packed-lock (ngit-unlock packed-lock)))))

;;;; Reachability helpers

(defun ngit--in-merge-bases (repo commit reference)
  "Non-nil when COMMIT is an ancestor of (or equal to) REFERENCE."
  (setq ngit--rv-repo repo)
  (if (eq commit reference)
      t
    (ngit--parse commit) (ngit--parse reference)
    (ngit--paint-down-to-common commit (list reference))
    (let ((r (ngit--has commit ngit-parent2)))
      (ngit--clear-marks commit ngit--all-paint-flags)
      (ngit--clear-marks reference ngit--all-paint-flags)
      r)))

(defun ngit--commit-of (repo hex)
  "The commit vector for object HEX (peeling tags), or nil."
  (let ((peeled (condition-case nil (ngit-peel repo hex 'commit) (error nil))))
    (and peeled (ngit-commit peeled))))

;;;; Listing

(defun ngit--head-description (repo head-hex)
  (let ((d (and head-hex (ngit-detached-from repo head-hex))))
    (cond ((null d) "(no branch)")
          ((cdr d) (concat "(HEAD detached at " (car d) ")"))
          (t (concat "(HEAD detached from " (car d) ")")))))

(defun ngit--branch-items (repo kinds detached)
  "Items [KIND REFNAME HEX SYMREF] to list; KIND local, remote or detached."
  (let ((items nil))
    (when (and (memq 'local kinds) detached)
      (let ((h (ngit-resolve-ref repo "HEAD")))
        (when h (push (vector 'detached "HEAD" h nil) items))))
    (when (memq 'local kinds)
      (dolist (r (ngit-list-refs repo "refs/heads/"))
        (push (vector 'local (nth 0 r) (nth 1 r) (nth 2 r)) items)))
    (when (memq 'remote kinds)
      (dolist (r (ngit-list-refs repo "refs/remotes/"))
        (push (vector 'remote (nth 0 r) (nth 1 r) (nth 2 r)) items)))
    (nreverse items)))

(defun ngit--branch-short (item)
  (let ((name (aref item 1)))
    (cond ((string-prefix-p "refs/heads/" name) (substring name 11))
          ((string-prefix-p "refs/remotes/" name) (substring name 13))
          (t name))))

(defun ngit--track-string (repo branch brackets)
  "%(upstream:track): \"[ahead N, behind M]\", \"[gone]\" or \"\"."
  (let ((up (ngit--upstream repo branch)))
    (if (null up)
        ""
      (let ((theirs (ngit-resolve-ref repo up))
            (ours (ngit-resolve-ref repo (concat "refs/heads/" branch))))
        (let ((s (if (null theirs)
                     "gone"
                   (let* ((ab (ngit-ahead-behind repo (ngit-unhex ours) (ngit-unhex theirs)))
                          (a (car ab)) (b (cdr ab)))
                     (cond ((and (= a 0) (= b 0)) nil)
                           ((= b 0) (format "ahead %d" a))
                           ((= a 0) (format "behind %d" b))
                           (t (format "ahead %d, behind %d" a b)))))))
          (cond ((null s) "")
                (brackets (concat "[" s "]"))
                (t s)))))))

(defun ngit--pad-to (s width)
  (let ((w (ngit--display-width s)))
    (if (< w width) (concat s (make-string (- width w) ?\s)) s)))

(defun ngit--subject-of (repo hex)
  (let ((c (ngit--commit-of repo hex)))
    (if c (progn (ngit-parse-commit-object repo c) (ngit--subject (ngit--c-message c))) "")))

(defun ngit--branch-sort (repo items keys)
  "Sort ITEMS by the --sort KEYS (last key primary), detached HEAD first."
  (let ((key-value
         (lambda (item key)
           (pcase key
             ("refname" (aref item 1))
             ("objectname" (aref item 2))
             ((or "committerdate" "creatordate")
              (let ((c (ngit--commit-of repo (aref item 2))))
                (if c (ngit--c-date (ngit-parse-commit-object repo c)) 0)))
             ("authordate"
              (let ((c (ngit--commit-of repo (aref item 2))))
                (if c (ngit--ident-date (ngit--c-author (ngit-parse-commit-object repo c))) 0)))
             (_ (ngit--die "fatal: unsupported sort key: %s" key))))))
    (let ((cmp
           (lambda (a b)
             (cond
              ((and (eq (aref a 0) 'detached) (not (eq (aref b 0) 'detached))) t)
              ((eq (aref b 0) 'detached) nil)
              (t
               (let ((result nil) (decided nil))
                 (dolist (k (reverse keys))
                   (unless decided
                     (let* ((rev (string-prefix-p "-" k))
                            (name (if rev (substring k 1) k))
                            (va (funcall key-value a name))
                            (vb (funcall key-value b name))
                            (c (cond ((equal va vb) 0)
                                     ((if (stringp va) (string< va vb) (< va vb)) -1)
                                     (t 1))))
                       (unless (= c 0)
                         (setq decided t result (if rev (> c 0) (< c 0)))))))
                 (if decided
                     result
                   (string< (aref a 1) (aref b 1)))))))))
      (sort items cmp))))

(defun ngit--branch-list (repo opts head-ref)
  (let* ((kinds (plist-get opts :kinds))
         (verbose (plist-get opts :verbose))
         (patterns (plist-get opts :patterns))
         (icase (plist-get opts :icase))
         (detached (not head-ref))
         (items (ngit--branch-items repo kinds detached))
         (remote-prefix (if (equal kinds '(remote)) "" "remotes/"))
         (abbrev (plist-get opts :abbrev)))
    ;; Name patterns (match on the name without refs/heads/ or refs/remotes/).
    (when patterns
      (setq items (seq-filter
                   (lambda (it)
                     (or (eq (aref it 0) 'detached)
                         (seq-find (lambda (p)
                                     (let ((case-fold-search icase))
                                       (if icase
                                           (ngit-wildmatch (downcase p) (downcase (ngit--branch-short it)))
                                         (ngit-wildmatch p (ngit--branch-short it)))))
                                   patterns)))
                   items))
      ;; A detached HEAD is only listed when it matches too.
      (setq items (seq-remove (lambda (it) (eq (aref it 0) 'detached)) items)))
    ;; Reachability filters.
    (let ((with (plist-get opts :contains)) (without (plist-get opts :no-contains))
          (merged (plist-get opts :merged)) (no-merged (plist-get opts :no-merged))
          (points (plist-get opts :points-at)))
      (when (or with without merged no-merged points)
        (setq items
              (seq-filter
               (lambda (it)
                 (let ((c (ngit--commit-of repo (aref it 2))))
                   (and c
                        (or (null points) (member (aref it 2) points)
                            (member (ngit-peel repo (aref it 2) nil) points))
                        (or (null with) (seq-find (lambda (w) (ngit--in-merge-bases repo w c)) with))
                        (or (null without) (not (seq-find (lambda (w) (ngit--in-merge-bases repo w c)) without)))
                        (or (null merged) (seq-find (lambda (m) (ngit--in-merge-bases repo c m)) merged))
                        (or (null no-merged) (not (seq-find (lambda (m) (ngit--in-merge-bases repo c m)) no-merged))))))
               items))))
    (setq items (ngit--branch-sort repo items (or (plist-get opts :sort)
                                                  (let ((v (ngit-config-get repo "branch.sort")))
                                                    (and (stringp v) (list v)))
                                                  '("refname"))))
    (let* ((head-hex (ngit-resolve-ref repo "HEAD"))
           (worktrees (ngit--worktrees repo))
           (wt-path (lambda (ref) (let ((f nil)) (dolist (w worktrees) (when (and (not f) (equal (nth 1 w) ref)) (setq f (nth 0 w)))) f)))
           (maxwidth 0))
      (when verbose
        (dolist (it items)
          (let ((w (+ (ngit--display-width (if (eq (aref it 0) 'detached)
                                               (ngit--head-description repo head-hex)
                                             (ngit--branch-short it)))
                      (if (eq (aref it 0) 'remote) (length remote-prefix) 0))))
            (setq maxwidth (max maxwidth w)))))
      (dolist (it items)
        (let* ((kind (aref it 0))
               (refname (aref it 1))
               (hex (aref it 2))
               (symref (aref it 3))
               (is-head (or (eq kind 'detached) (and head-ref (string= refname head-ref))))
               (short (if (eq kind 'detached) (ngit--head-description repo head-hex)
                        (ngit--branch-short it)))
               (obj (if (eq abbrev 'full) hex (ngit-abbrev repo hex abbrev))))
          (if (eq kind 'remote)
              (ngit--emit "  "
                          (if verbose
                              (concat (ngit--pad-to (concat remote-prefix short) maxwidth)
                                      (if symref
                                          (concat " -> " (ngit-shorten-ref repo symref))
                                        (concat " " obj " " (ngit--subject-of repo hex))))
                            (concat remote-prefix short
                                    (if symref (concat " -> " (ngit-shorten-ref repo symref)) "")))
                          "\n")
            (let ((path (and (eq kind 'local) (funcall wt-path refname))))
              (ngit--emit
               (cond (is-head "* ") (path "+ ") (t "  "))
               (if verbose
                   (concat
                    (ngit--pad-to short maxwidth) " " obj " "
                    (if (>= verbose 2)
                        (concat
                         (if (and (not is-head) path) (concat "(" path ") ") "")
                         (let ((up (and (eq kind 'local) (ngit--upstream repo short))))
                           (if up
                               (let ((tr (ngit--track-string repo short nil)))
                                 (concat "[" (ngit-shorten-ref repo up)
                                         (if (string= tr "") "" (concat ": " tr)) "] "))
                             "")))
                      (let ((tr (if (eq kind 'local) (ngit--track-string repo short t) "")))
                        (if (string= tr "") "" (concat tr " "))))
                    (ngit--subject-of repo hex))
                 (concat short (if symref (concat " -> " (ngit-shorten-ref repo symref)) "")))
               "\n"))))))
    0))

;;;; Tracking

(defun ngit--remote-for-tracking-ref (repo ref)
  "(REMOTE . SRC) when REF is a remote-tracking ref of exactly one remote."
  (let ((found nil) (count 0))
    (dolist (e (ngit-config-entries repo))
      (when (and (string-prefix-p "remote." (car e)) (string-suffix-p ".fetch" (car e))
                 (stringp (cdr e)))
        (let* ((remote (substring (car e) 7 -6))
               (spec (string-remove-prefix "+" (cdr e)))
               (colon (string-search ":" spec))
               (src (and colon (substring spec 0 colon)))
               (dst (and colon (substring spec (1+ colon))))
               (mapped
                (and dst
                     (let ((star (string-search "*" dst)))
                       (if star
                           (let ((pre (substring dst 0 star)) (suf (substring dst (1+ star))))
                             (and (string-prefix-p pre ref) (string-suffix-p suf ref)
                                  (> (length ref) (+ (length pre) (length suf)))
                                  (let ((mid (substring ref (length pre) (- (length ref) (length suf))))
                                        (sstar (string-search "*" src)))
                                    (if sstar
                                        (concat (substring src 0 sstar) mid (substring src (1+ sstar)))
                                      src))))
                         (and (string= dst ref) src))))))
          (when mapped
            (unless (and found (string= (car found) remote))
              (setq count (1+ count)))
            (setq found (cons remote mapped))))))
    (and (= count 1) found)))

(defun ngit--install-tracking (repo local remote merge quiet)
  "install_branch_config: branch.LOCAL.remote/merge, and the message."
  (ngit-config-set repo (concat "branch." local ".remote") (or remote "."))
  (ngit-config-set repo (concat "branch." local ".merge") nil)
  (ngit-config-set repo (concat "branch." local ".merge") merge t)
  (unless quiet
    (let ((short (if (string-prefix-p "refs/heads/" merge) (substring merge 11) merge)))
      (ngit--emit "branch '" local "' set up to track '"
                  (if remote (concat remote "/" short) short) "'.\n"))))

(defun ngit--setup-tracking (repo new-branch orig-ref track quiet)
  "setup_tracking for NEW-BRANCH started at ORIG-REF (a full ref)."
  (let ((remote-src (ngit--remote-for-tracking-ref repo orig-ref)))
    (cond
     (remote-src
      (ngit--install-tracking repo new-branch (car remote-src) (cdr remote-src) quiet))
     ((memq track '(explicit override always))
      (if (and (string-prefix-p "refs/heads/" orig-ref)
               (string= (substring orig-ref 11) new-branch))
          (ngit--branch-warning "not setting branch '%s' as its own upstream" new-branch)
        (ngit--install-tracking repo new-branch nil orig-ref quiet))))))

(defun ngit--dwim-start (repo start track)
  "dwim_branch_start: return (HEX . REAL-REF-OR-NIL) for START."
  (let* ((explicit (memq track '(explicit override)))
         (hex (condition-case nil (ngit-rev-parse repo start) (error nil))))
    (unless hex
      (if explicit
          (ngit--die "fatal: the requested upstream branch '%s' does not exist\nhint:\nhint: If you are planning on basing your work on an upstream\nhint: branch that already exists at the remote, you may need to\nhint: run \"git fetch\" to retrieve it.\nhint:\nhint: If you are planning to push out a new local branch that\nhint: will track its remote counterpart, you may want to use\nhint: \"git push -u\" to set the upstream config as you push.\nhint: Disable this message with \"git config advice.setUpstreamFailure false\"" start)
        (ngit--die "fatal: not a valid object name: '%s'" start)))
    (let* ((dwim (and (not (string-search "~" start)) (not (string-search "^" start))
                      (not (string-search ":" start))
                      (ngit-dwim-ref repo (if (string= start "@") "HEAD" start))))
           (real (and dwim (car dwim)))
           ;; dwim_ref reports the ref a symbolic ref finally names.
           (real (let ((r real) (depth 0))
                   (while (and r (< depth 5)
                               (let ((raw (ngit-read-ref repo r)))
                                 (and raw (string-prefix-p "ref: " raw)
                                      (setq r (substring raw 5)))))
                     (setq depth (1+ depth)))
                   (if (and r (string= r "HEAD")) nil r))))
      (cond
       ((null real) (when explicit (ngit--die "fatal: cannot set up tracking information; starting point '%s' is not a branch" start)))
       ((and (not (string-prefix-p "refs/heads/" real))
             (not (ngit--remote-for-tracking-ref repo real)))
        (when explicit (ngit--die "fatal: cannot set up tracking information; starting point '%s' is not a branch" start))
        (setq real nil)))
      (let ((c (ngit--commit-of repo hex)))
        (unless c (ngit--die "fatal: not a valid branch point: '%s'" start))
        (cons (ngit--c-hex c) real)))))

;;;; Actions

(defun ngit--branch-create (repo name start force quiet track head-name)
  (ngit--check-branch-name name)
  (let* ((ref (concat "refs/heads/" name))
         (exists (ngit-resolve-ref repo ref))
         (forcing nil))
    (when exists
      (unless force (ngit--die "fatal: a branch named '%s' already exists" name))
      (let ((path (ngit--branch-checked-out repo ref)))
        (when path
          (ngit--die "fatal: cannot force update the branch '%s' used by worktree at '%s'" name path)))
      (setq forcing t))
    (let* ((start (or start head-name))
           (r (ngit--dwim-start repo start track)))
      (ngit--write-ref repo ref (car r) (and forcing exists)
                       (if forcing (concat "branch: Reset to " start)
                         (concat "branch: Created from " start)) t)
      (when (and (cdr r) track (not (eq track 'none)))
        (ngit--setup-tracking repo name (cdr r) track quiet)))))

(defun ngit--branch-delete (repo names force remote quiet head-hex)
  (let ((ret 0) (to-delete nil)
        (head-commit (and (not force) head-hex (ngit--commit-of repo head-hex))))
    (dolist (arg names)
      (let* ((name (if (string-prefix-p "refs/heads/" arg) arg (concat (if remote "refs/remotes/" "refs/heads/") arg)))
             (short arg)
             (path (and (not remote) (ngit--branch-checked-out repo name)))
             (raw (ngit-read-ref repo name)))
        (cond
         (path (ngit--branch-error "cannot delete branch '%s' used by worktree at '%s'" short path)
               (setq ret 1))
         ((null raw)
          (if remote
              (ngit--branch-error "remote-tracking branch '%s' not found" short)
            (if (ngit-read-ref repo (concat "refs/remotes/" short))
                (ngit--branch-error "branch '%s' not found.\nDid you forget --remote?" short)
              (ngit--branch-error "branch '%s' not found" short)))
          (setq ret 1))
         (t
          (let* ((symref (and (string-prefix-p "ref: " raw) (substring raw 5)))
                 (hex (and (not symref) raw))
                 (ok t))
            (when (and hex (not force) (not remote))
              (let* ((c (ngit--commit-of repo hex))
                     (up (ngit--upstream repo short))
                     (up-hex (and up (ngit-resolve-ref repo up)))
                     (ref-commit (or (and up-hex (ngit--commit-of repo up-hex)) head-commit))
                     (merged (and ref-commit (ngit--in-merge-bases repo c ref-commit))))
                (unless (eq ref-commit head-commit)
                  (let ((expect (and head-commit (ngit--in-merge-bases repo c head-commit))))
                    (cond ((eq (and expect t) (and merged t)) nil)
                          (merged (ngit--branch-warning "deleting branch '%s' that has been merged to\n         '%s', but not yet merged to HEAD" short up))
                          (t (ngit--branch-warning "not deleting branch '%s' that is not yet merged to\n         '%s', even though it is merged to HEAD" short up)))))
                (unless merged
                  (ngit--branch-error "the branch '%s' is not fully merged" short)
                  (ngit-native-write-stderr
                   (format "hint: If you are sure you want to delete it, run 'git branch -D %s'\nhint: Disable this message with \"git config advice.forceDeleteBranch false\"\n" short))
                  (setq ok nil ret 1))))
            (when ok
              (push (list name short (or symref (ngit-abbrev repo hex))) to-delete)))))))
    (dolist (d (nreverse to-delete))
      (ngit--delete-ref repo (nth 0 d))
      (unless quiet
        (ngit--emit (if remote "Deleted remote-tracking branch " "Deleted branch ")
                    (nth 1 d) " (was " (nth 2 d) ").\n"))
      (ngit-config-rename-section repo (concat "branch." (nth 1 d)) nil))
    ret))

(defun ngit--branch-copy-or-rename (repo old new copy force head-ref)
  (let* ((oldref (concat "refs/heads/" old))
         (is-head nil) (orphan nil))
    (unless (ngit--valid-branch-name-p old)
      (unless (ngit-read-ref repo oldref)
        (ngit--die "fatal: invalid branch name: '%s'" old)))
    (dolist (wt (ngit--worktrees repo))
      (when (equal (nth 1 wt) oldref)
        (setq is-head t)
        (unless (ngit-resolve-ref repo oldref) (setq orphan t))))
    (when (and (or copy (not is-head)) (not (ngit-resolve-ref repo oldref)))
      (if is-head
          (ngit--die "fatal: no commit on branch '%s' yet" old)
        (ngit--die "fatal: no branch named '%s'" old)))
    (ngit--check-branch-name new)
    (let ((newref (concat "refs/heads/" new)))
      (unless (string= old new)
        (when (ngit-resolve-ref repo newref)
          (unless force (ngit--die "fatal: a branch named '%s' already exists" new))
          (let ((path (ngit--branch-checked-out repo newref)))
            (when path
              (ngit--die "fatal: cannot force update the branch '%s' used by worktree at '%s'" new path)))))
      (let ((msg (concat (if copy "Branch: copied " "Branch: renamed ") oldref " to " newref))
            (common (ngit-repo-commondir repo)))
        (unless (and (not copy) orphan)
          (let* ((hex (ngit-resolve-ref repo oldref))
                 (oldlog (expand-file-name (concat "logs/" oldref) common))
                 (newlog (expand-file-name (concat "logs/" newref) common))
                 (log-data (and (file-exists-p oldlog) (ngit--read-file oldlog)))
                 (log-mode (and log-data (not copy) (file-modes oldlog))))
            (unless copy
              (ngit--delete-ref repo oldref nil hex
                                (and (equal head-ref oldref) hex)))
            ;; A rename deletes an existing target; a copy writes over it
            ;; (leaving any packed entry, as git does).
            (when (and (not copy) (not (string= old new)) (ngit-read-ref repo newref))
              (ngit--delete-ref repo newref))
            (when log-data
              (ngit-make-directory (file-name-directory newlog) t)
              (ngit-write-file newlog log-data nil nil log-mode))
            ;; The files backend copies the new ref without a transaction.
            (let ((ngit-ref-inhibit-hooks t))
              (ngit--write-ref repo newref hex hex msg t))))
        (when (and (not copy) is-head)
          ;; replace_each_worktree_head_symref
          (dolist (wt (ngit--worktrees repo))
            (when (equal (nth 1 wt) oldref)
              (let* ((gitdir (if (string= (file-name-as-directory (nth 0 wt))
                                          (file-name-as-directory (directory-file-name (file-name-directory (directory-file-name common)))))
                                 common
                               (ngit--worktree-gitdir repo (nth 0 wt))))
                     (hex (ngit-resolve-ref repo newref)))
                (ngit--write-worktree-head-symref repo gitdir newref)
                ;; Deleting the old ref logs the current worktree's HEAD
                ;; going to zero; pointing HEAD at a different ref logs it
                ;; coming back in every worktree.
                (when hex
                  (let ((log (expand-file-name "logs/HEAD" gitdir))
                        (ident (ngit--branch-ident repo))
                        (zero (make-string 40 ?0)))
                    (ngit-make-directory (file-name-directory log) t)
                    (ngit-write-file
                     log (concat (if (string= (file-name-as-directory gitdir)
                                              (file-name-as-directory (ngit-repo-gitdir repo)))
                                     (concat hex " " zero " " ident "\t" msg "\n")
                                   "")
                                 ;; Rewriting the ref HEAD names logs
                                 ;; HEAD as well; a new target logs
                                 ;; HEAD's switch from nothing.
                                 (concat (if (string= oldref newref) hex zero)
                                         " " hex " " ident "\t" msg "\n"))
                     nil t)))))))
        (if copy
            (unless (string= old new)
              (ngit-config-rename-section repo (concat "branch." old) (concat "branch." new) t))
          (ngit-config-rename-section repo (concat "branch." old) (concat "branch." new)))))
    (ignore head-ref)
    0))

(defun ngit--worktree-gitdir (repo path)
  "The administrative directory of the linked worktree at PATH."
  (let ((dir (expand-file-name "worktrees" (ngit-repo-commondir repo))) (found nil))
    (dolist (name (directory-files dir nil "\\`[^.]"))
      (let ((g (expand-file-name "gitdir" (expand-file-name name dir))))
        (when (and (not found) (file-exists-p g)
                   (string= (directory-file-name (file-name-directory (ngit--trim-eol (ngit--read-file g))))
                            (directory-file-name path)))
          (setq found (expand-file-name name dir)))))
    found))

;;;; The command

(defun ngit-cmd-branch (repo args)
  (let ((delete 0) (rename 0) (copy 0) (list nil) (show-current nil) (force nil)
        (quiet nil) (verbose 0) (kinds '(local)) (track 'default) (new-upstream nil)
        (unset-upstream nil) (patterns nil) (positional nil) (opts nil) (icase nil)
        (abbrev nil) (sort-keys nil))
    (while args
      (let ((a (pop args)))
        (cond
         ((string= a "--") (setq positional (append positional args) args nil))
         ((member a '("-d" "--delete")) (setq delete (max delete 1)))
         ((string= a "-D") (setq delete 2))
         ((member a '("-m" "--move")) (setq rename (max rename 1)))
         ((string= a "-M") (setq rename 2))
         ((member a '("-c" "--copy")) (setq copy (max copy 1)))
         ((string= a "-C") (setq copy 2))
         ((member a '("-f" "--force")) (setq force t))
         ((member a '("-q" "--quiet")) (setq quiet t))
         ((member a '("-v" "--verbose")) (setq verbose (1+ verbose)))
         ((string= a "-vv") (setq verbose (+ verbose 2)))
         ((member a '("-a" "--all")) (setq kinds '(local remote)))
         ((member a '("-r" "--remotes")) (setq kinds '(remote)))
         ((member a '("-l" "--list")) (setq list t))
         ((string= a "--show-current") (setq show-current t))
         ((member a '("-t" "--track" "--track=direct")) (setq track 'explicit))
         ((string= a "--no-track") (setq track 'none))
         ((member a '("-i" "--ignore-case")) (setq icase t))
         ((member a '("-u" "--set-upstream-to")) (setq new-upstream (pop args)))
         ((string-prefix-p "--set-upstream-to=" a) (setq new-upstream (substring a 18)))
         ((string-prefix-p "-u" a) (setq new-upstream (substring a 2)))
         ((string= a "--unset-upstream") (setq unset-upstream t))
         ((string= a "--no-abbrev") (setq abbrev 'full))
         ((string-prefix-p "--abbrev=" a) (setq abbrev (max 4 (string-to-number (substring a 9)))))
         ((string-prefix-p "--sort=" a) (setq sort-keys (append sort-keys (list (substring a 7)))))
         ((member a '("--contains" "--no-contains" "--merged" "--no-merged" "--points-at"))
          (let ((v (if (and args (not (string-prefix-p "-" (car args)))) (pop args) "HEAD")))
            (setq opts (plist-put opts (intern (concat ":" (substring a 2)))
                                  (append (plist-get opts (intern (concat ":" (substring a 2)))) (list v))))))
         ((string-match "\\`--\\(contains\\|no-contains\\|merged\\|no-merged\\|points-at\\)=\\(.*\\)\\'" a)
          (let ((k (intern (concat ":" (match-string 1 a)))) (v (match-string 2 a)))
            (setq opts (plist-put opts k (append (plist-get opts k) (list v))))))
         ((string= a "--no-column") nil)
         ((string-prefix-p "-" a) (ngit--die "error: unknown option `%s'" (string-remove-prefix "--" (string-remove-prefix "-" a))))
         (t (setq positional (append positional (list a)))))))
    (let* ((raw-head (ngit-read-ref repo "HEAD"))
           (head-ref (and raw-head (string-prefix-p "ref: refs/heads/" raw-head) (substring raw-head 5)))
           (head-name (if head-ref (substring head-ref 11) "HEAD"))
           (head-hex (ngit-resolve-ref repo "HEAD")))
      (setq ngit--rv-repo repo)
      (when (and (not (or (> delete 0) (> rename 0) (> copy 0) new-upstream show-current unset-upstream))
                 (null positional))
        (setq list t))
      (when (or (plist-get opts :contains) (plist-get opts :no-contains) (plist-get opts :merged)
                (plist-get opts :no-merged) (plist-get opts :points-at))
        (setq list t))
      (cond
       ((> delete 0)
        (unless positional (ngit--die "fatal: branch name required"))
        (when (equal kinds '(local remote)) (ngit--die "fatal: cannot use -a with -d"))
        (ngit--branch-delete repo positional (or force (= delete 2) (equal kinds '(remote)))
                             (equal kinds '(remote)) quiet head-hex))
       (show-current
        (when head-ref (ngit--emit head-name "\n"))
        0)
       (list
        (let ((resolve (lambda (revs)
                         (mapcar (lambda (r)
                                   (let ((hex (condition-case nil (ngit-rev-parse repo r) (error nil))))
                                     (unless hex (ngit--die-with 129 "error: malformed object name %s" r))
                                     (or (ngit--commit-of repo hex)
                                         (ngit--die "error: object %s is a %s, not a commit" hex
                                                    (car (ngit-read-object repo (ngit-unhex hex)))))))
                                 revs))))
          (ngit--branch-list
           repo
           (list :kinds kinds :verbose (and (> verbose 0) verbose) :patterns positional
                 :icase icase :sort sort-keys
                 :abbrev (or abbrev (ngit-default-abbrev repo))
                 :contains (funcall resolve (plist-get opts :contains))
                 :no-contains (funcall resolve (plist-get opts :no-contains))
                 :merged (funcall resolve (plist-get opts :merged))
                 :no-merged (funcall resolve (plist-get opts :no-merged))
                 :points-at (mapcar (lambda (r) (ngit-rev-parse repo r)) (plist-get opts :points-at)))
           head-ref)))
       ((or (> copy 0) (> rename 0))
        (let ((is-copy (> copy 0)) (force (or force (= copy 2) (= rename 2))))
          (cond ((null positional) (ngit--die "fatal: branch name required"))
                ((and (null (cdr positional)) (not head-ref))
                 (ngit--die (if is-copy "fatal: cannot copy the current branch while not on any"
                              "fatal: cannot rename the current branch while not on any")))
                ((null (cdr positional))
                 (ngit--branch-copy-or-rename repo head-name (car positional) is-copy force head-ref))
                ((null (cddr positional))
                 (ngit--branch-copy-or-rename repo (car positional) (cadr positional) is-copy force head-ref))
                (t (ngit--die (if is-copy "fatal: too many branches for a copy operation"
                                "fatal: too many arguments for a rename operation"))))))
       (new-upstream
        (let* ((branch (cond ((null positional) (and head-ref head-name))
                             ((cdr positional) (ngit--die "fatal: too many arguments to set new upstream"))
                             ((string= (car positional) "HEAD") (and head-ref head-name))
                             (t (car positional)))))
          (unless branch
            (ngit--die "fatal: could not set upstream of HEAD to %s when it does not point to any branch" new-upstream))
          (unless (ngit-resolve-ref repo (concat "refs/heads/" branch))
            (if (or (null positional) (ngit--branch-checked-out repo (concat "refs/heads/" branch)))
                (ngit--die "fatal: no commit on branch '%s' yet" branch)
              (ngit--die "fatal: branch '%s' does not exist" branch)))
          (let ((r (ngit--dwim-start repo new-upstream 'override)))
            (ngit--setup-tracking repo branch (cdr r) 'override quiet))
          0))
       (unset-upstream
        (let* ((branch (cond ((null positional) (and head-ref head-name))
                             ((cdr positional) (ngit--die "fatal: too many arguments to unset upstream"))
                             ((string= (car positional) "HEAD") (and head-ref head-name))
                             (t (car positional)))))
          (unless branch
            (ngit--die "fatal: could not unset upstream of HEAD when it does not point to any branch"))
          (unless (ngit-config-get repo (concat "branch." branch ".merge"))
            (ngit--die "fatal: branch '%s' has no upstream information" branch))
          (ngit-config-set repo (concat "branch." branch ".remote") nil)
          (ngit-config-set repo (concat "branch." branch ".merge") nil)
          0))
       ((<= (length positional) 2)
        (unless (equal kinds '(local))
          (ngit--die "fatal: the -a, and -r, options to 'git branch' do not take a branch name.\nDid you mean to use: -a|-r --list <pattern>?"))
        (let ((track (if (eq track 'default)
                         (let ((v (ngit-config-get repo "branch.autoSetupMerge")))
                           (cond ((null v) 'remote)
                                 ((equal v "always") 'always)
                                 ((equal v "simple") 'simple)
                                 ((equal v "inherit") 'inherit)
                                 ((ngit-config-bool repo "branch.autoSetupMerge" t) 'remote)
                                 (t 'none)))
                       track)))
          (ngit--branch-create repo (car positional) (cadr positional) force quiet
                               (if (eq track 'none) nil track) head-name))
        0)
       (t (ngit--die "fatal: too many arguments"))))))

(provide 'ngit-branch)

;;; ngit-branch.el ends here
