;;; ngit-match.el --- wildmatch, gitignore and gitattributes -*- lexical-binding: t; -*-

;;; Commentary:

;; A port of git's wildmatch.c (`dowild') and the pattern rules shared by
;; .gitignore and .gitattributes (dir.c `parse_path_pattern',
;; `match_basename', `match_pathname').  Paths and patterns are unibyte
;; strings relative to the work tree.
;;
;; A pattern is a vector [TEXT FLAGS BASE ATTRS]: TEXT without the leading
;; "!" and trailing "/", FLAGS a list of symbols (negative mustbedir nodir
;; literal endswith), BASE the directory of the defining file ("" or
;; "dir/"), ATTRS the parsed attribute assignments (gitattributes only).

;;; Code:

(require 'ngit-config)

;;;; wildmatch

(defsubst ngit--wm-at (s i)
  (if (< i (length s)) (aref s i) 0))

(defun ngit--wm-class-p (name c)
  (pcase name
    ("alnum" (or (and (>= c ?0) (<= c ?9)) (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z))))
    ("alpha" (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z))))
    ("blank" (memq c '(?\s ?\t)))
    ("cntrl" (or (< c 32) (= c 127)))
    ("digit" (and (>= c ?0) (<= c ?9)))
    ("graph" (and (> c 32) (< c 127)))
    ("lower" (and (>= c ?a) (<= c ?z)))
    ("print" (and (>= c 32) (< c 127)))
    ("punct" (and (> c 32) (< c 127)
                  (not (or (and (>= c ?0) (<= c ?9)) (and (>= c ?a) (<= c ?z))
                           (and (>= c ?A) (<= c ?Z))))))
    ("space" (memq c '(?\s ?\t ?\n ?\r ?\f ?\v)))
    ("upper" (and (>= c ?A) (<= c ?Z)))
    ("xdigit" (or (and (>= c ?0) (<= c ?9)) (and (>= c ?a) (<= c ?f)) (and (>= c ?A) (<= c ?F))))
    (_ 'malformed)))

(defun ngit--dowild (pat p text ti pathname)
  "Match PAT from P against TEXT from TI.
Return one of `match', `nomatch', `abort-all', `abort-to-starstar'."
  (let ((result nil) (plen (length pat)))
    (while (and (null result) (< p plen))
      (let ((p-ch (aref pat p))
            (t-ch (ngit--wm-at text ti)))
        (if (and (= t-ch 0) (/= p-ch ?*))
            (setq result 'abort-all)
          (cond
           ((= p-ch ??)
            (when (and pathname (= t-ch ?/)) (setq result 'nomatch)))
           ((= p-ch ?*)
            (let ((match-slash nil))
              (setq p (1+ p))
              (if (= (ngit--wm-at pat p) ?*)
                  (let ((prev-p (- p 2)))
                    (while (= (ngit--wm-at pat (1+ p)) ?*) (setq p (1+ p)))
                    (setq p (1+ p))
                    (if (and (or (< prev-p 0) (= (aref pat prev-p) ?/))
                             (let ((c (ngit--wm-at pat p)))
                               (or (= c 0) (= c ?/)
                                   (and (= c ?\\) (= (ngit--wm-at pat (1+ p)) ?/)))))
                        (progn
                          (when (and (= (ngit--wm-at pat p) ?/)
                                     (eq (ngit--dowild pat (1+ p) text ti pathname) 'match))
                            (setq result 'match))
                          (setq match-slash t))
                      (setq match-slash (not pathname))))
                (setq match-slash (not pathname)))
              (unless result
                (cond
                 ((= (ngit--wm-at pat p) 0)
                  (setq result (if (and (not match-slash)
                                        (let ((i ti) (found nil))
                                          (while (and (not found) (< i (length text)))
                                            (setq found (= (aref text i) ?/) i (1+ i)))
                                          found))
                                   'nomatch 'match)))
                 ((and (not match-slash) (= (ngit--wm-at pat p) ?/))
                  (let ((i ti))
                    (while (and (< i (length text)) (/= (aref text i) ?/)) (setq i (1+ i)))
                    (if (>= i (length text))
                        (setq result 'nomatch)
                      ;; The slash is consumed by the outer loop.
                      (setq ti i))))
                 (t
                  (let ((done nil))
                    (while (not done)
                      (setq t-ch (ngit--wm-at text ti))
                      (if (= t-ch 0)
                          (setq done t result 'abort-all)
                        (let ((pc (ngit--wm-at pat p)) (fail nil))
                          (unless (memq pc '(?* ?? ?\[ ?\\))
                            (while (and (/= (setq t-ch (ngit--wm-at text ti)) 0)
                                        (or match-slash (/= t-ch ?/))
                                        (/= t-ch pc))
                              (setq ti (1+ ti)))
                            (when (/= t-ch pc)
                              (setq fail t done t
                                    result (if (or match-slash (= t-ch 0))
                                               'abort-all 'abort-to-starstar))))
                          (unless fail
                            (let ((m (ngit--dowild pat p text ti pathname)))
                              (cond
                               ((and (not (eq m 'nomatch))
                                     (or (not match-slash) (not (eq m 'abort-to-starstar))))
                                (setq done t result m))
                               ((and (eq m 'nomatch) (not match-slash) (= t-ch ?/))
                                (setq done t result 'abort-to-starstar))
                               (t (setq ti (1+ ti)))))))))))))))
           ((= p-ch ?\[)
            (setq p (1+ p))
            (let* ((pc (ngit--wm-at pat p))
                   (negated nil) (matched nil) (prev 0) (stop nil))
              (when (= pc ?^) (setq pc ?!))
              (when (= pc ?!) (setq negated t p (1+ p) pc (ngit--wm-at pat p)))
              (while (not stop)
                (cond
                 ((= pc 0) (setq result 'abort-all stop t))
                 ((= pc ?\\)
                  (setq p (1+ p) pc (ngit--wm-at pat p))
                  (if (= pc 0) (setq result 'abort-all stop t)
                    (when (= t-ch pc) (setq matched t))))
                 ((and (= pc ?-) (/= prev 0) (/= (ngit--wm-at pat (1+ p)) 0)
                       (/= (ngit--wm-at pat (1+ p)) ?\]))
                  (setq p (1+ p) pc (ngit--wm-at pat p))
                  (when (= pc ?\\) (setq p (1+ p) pc (ngit--wm-at pat p)))
                  (if (= pc 0) (setq result 'abort-all stop t)
                    (when (and (<= t-ch pc) (>= t-ch prev)) (setq matched t))
                    (setq pc 0)))
                 ((and (= pc ?\[) (= (ngit--wm-at pat (1+ p)) ?:))
                  (let* ((s (+ p 2)) (q s))
                    (while (and (/= (ngit--wm-at pat q) 0) (/= (aref pat q) ?\]))
                      (setq q (1+ q)))
                    (cond
                     ((= (ngit--wm-at pat q) 0) (setq result 'abort-all stop t))
                     ((or (< (- q s 1) 0) (/= (aref pat (1- q)) ?:))
                      (when (= t-ch ?\[) (setq matched t))
                      (setq pc ?\[))
                     (t
                      (let ((r (ngit--wm-class-p (substring pat s (1- q)) t-ch)))
                        (if (eq r 'malformed)
                            (setq result 'abort-all stop t)
                          (when r (setq matched t))
                          (setq p q pc 0)))))))
                 ((= t-ch pc) (setq matched t)))
                (unless stop
                  (setq prev pc p (1+ p) pc (ngit--wm-at pat p))
                  (when (= pc ?\]) (setq stop t))))
              (unless result
                (when (or (eq matched negated) (and pathname (= t-ch ?/)))
                  (setq result 'nomatch)))))
           (t
            (when (= p-ch ?\\)
              (setq p (1+ p) p-ch (ngit--wm-at pat p)))
            (unless (= t-ch p-ch) (setq result 'nomatch))))
          (setq p (1+ p) ti (1+ ti)))))
    (or result (if (< ti (length text)) 'nomatch 'match))))

(defun ngit-wildmatch (pattern text &optional pathname)
  "Non-nil when PATTERN matches all of TEXT (WM_PATHNAME when PATHNAME)."
  (eq (ngit--dowild pattern 0 text 0 pathname) 'match))

;;;; Patterns

(defun ngit--glob-special-p (s from)
  (let ((n (length s)) (i from) (found nil))
    (while (and (< i n) (not found))
      (setq found (memq (aref s i) '(?* ?? ?\[ ?\\)) i (1+ i)))
    found))

(defun ngit--trim-trailing-spaces (s)
  (let ((end (length s)) (i 0) (last-keep 0))
    (while (< i end)
      (let ((c (aref s i)))
        (cond ((= c ?\\) (setq i (1+ i)) (setq last-keep (min end (1+ i))))
              ((/= c ?\s) (setq last-keep (1+ i)))))
      (setq i (1+ i)))
    (substring s 0 last-keep)))

(defun ngit-parse-pattern (text base)
  "Parse pattern TEXT defined in directory BASE into a pattern vector."
  (let ((flags nil))
    (when (and (> (length text) 0) (= (aref text 0) ?!))
      (push 'negative flags) (setq text (substring text 1)))
    (when (and (> (length text) 0) (= (aref text (1- (length text))) ?/))
      (push 'mustbedir flags) (setq text (substring text 0 -1)))
    (unless (string-search "/" text) (push 'nodir flags))
    (cond ((not (ngit--glob-special-p text 0)) (push 'literal flags))
          ((and (> (length text) 0) (= (aref text 0) ?*)
                (not (ngit--glob-special-p text 1)))
           (push 'endswith flags)))
    (vector text flags base nil)))

(defun ngit-pattern-match-p (pat path basename-start isdir)
  "Non-nil when pattern PAT matches PATH (BASENAME-START is its offset)."
  (let ((text (aref pat 0)) (flags (aref pat 1)))
    (and (or isdir (not (memq 'mustbedir flags)))
         (if (memq 'nodir flags)
             (let ((base (if (= basename-start 0) path (substring path basename-start))))
               (cond ((memq 'literal flags) (string= text base))
                     ((memq 'endswith flags) (string-suffix-p (substring text 1) base))
                     (t (ngit-wildmatch text base nil))))
           (let* ((pbase (aref pat 2))
                  (text (if (and (> (length text) 0) (= (aref text 0) ?/))
                            (substring text 1)
                          text)))
             (and (> (length path) (length pbase))
                  (string= pbase (substring path 0 (length pbase)))
                  (let ((name (substring path (length pbase))))
                    (if (memq 'literal flags)
                        (string= text name)
                      (ngit-wildmatch text name t)))))))))

(defun ngit--pattern-lines (data)
  "Split file DATA into lines with CR stripped, skipping blanks/comments."
  (let ((lines nil))
    (dolist (l (split-string data "\n"))
      (when (string-suffix-p "\r" l) (setq l (substring l 0 -1)))
      (unless (or (string= l "") (= (aref l 0) ?#))
        (push l lines)))
    (nreverse lines)))

;;;; gitignore

(defun ngit--worktree-nofollow-p (path name)
  "Reject a worktree symlink PATH with Git's nofollow diagnostic for NAME."
  (if (file-symlink-p path)
      (progn
        (ngit-native-write-stderr
         (concat "warning: unable to access '" (decode-coding-string name 'utf-8)
                 "': Too many levels of symbolic links\n"))
        nil)
    t))

(defun ngit--read-worktree-file (path name)
  "Read PATH atomically without following a symlink, diagnosing NAME.
The descriptor is private; a rename between lstat and open cannot redirect
attribute or ignore reads to the target of a symlink."
  (let* ((bytes (encode-coding-string path 'utf-8))
         (address (nl-ffi-memory-address (ngit-native--buffer 3 (1+ (length bytes))))))
    (ptr-write-bytes address bytes)
    (ptr-write-u8 address (length bytes) 0)
    (let ((descriptor (syscall-direct ngit-native--sys-open address
                                      (logior ngit-native--o-rdonly-cloexec #o400000)
                                      0 0 0 0)))
      (if (< descriptor 0)
          (progn
            (when (= descriptor -40)
              (ngit-native-write-stderr
               (concat "warning: unable to access '" (decode-coding-string name 'utf-8)
                       "': Too many levels of symbolic links\n")))
            nil)
        (unwind-protect
            (let ((buffer (nl-ffi-memory-address (ngit-native--buffer 0 65536)))
                  parts done)
              (while (not done)
                (let ((size (syscall-direct 0 descriptor buffer 65536 0 0 0)))
                  (if (<= size 0) (setq done t)
                    (push (ptr-read-bytes buffer size) parts))))
              (apply #'concat (nreverse parts)))
          (syscall-direct ngit-native--sys-close descriptor 0 0 0 0 0))))))

(defun ngit-read-ignore-file (path base &optional nofollow)
  "Return the patterns of ignore file PATH, last line first."
  (when (and (or (not nofollow)
                 (ngit--worktree-nofollow-p path (concat base ".gitignore")))
             (file-regular-p path))
    (let ((pats nil))
      (dolist (l (ngit--pattern-lines
                 (if nofollow (or (ngit--read-worktree-file path (concat base ".gitignore")) "")
                   (ngit--read-file path))))
        (let ((l (ngit--trim-trailing-spaces l)))
          (unless (string= l "")
            (push (ngit-parse-pattern l base) pats))))
      pats)))

(defun ngit-ignore-global-patterns (repo)
  "Return the info/exclude and core.excludesFile pattern lists, in check order."
  (let* ((xf (ngit-config-get repo "core.excludesfile"))
         (xdg (getenv "XDG_CONFIG_HOME"))
         (xf (cond ((stringp xf) (expand-file-name xf))
                   (t (expand-file-name "git/ignore"
                                        (if (and xdg (not (string= xdg ""))) xdg
                                          (expand-file-name ".config" (getenv "HOME"))))))))
    (delq nil
          (mapcar (lambda (pats) (and pats (ngit-compile-ignore pats)))
                  (list (ngit-read-ignore-file
                         (expand-file-name "info/exclude" (ngit-repo-commondir repo)) "")
                        (ngit-read-ignore-file xf ""))))))

(defun ngit-compile-ignore (pats)
  "Index pattern list PATS (last line first) for fast matching.
Matching every pattern costs ~1 ms each in NeLisp, so patterns are
bucketed: literal basenames and \"*SUFFIX\" basenames by hash, slash
patterns by their literal first component; the rest are tried in turn.
Slash patterns whose first component is a glob (\"**/x/y\") are
bucketed by a literal last component instead.
Return [LITERALS SUFFIXES FIRST-COMPONENT OTHERS HAS-SLASH BASE
LAST-COMPONENT], each bucket holding (INDEX . PATTERN) with higher INDEX
for later lines."
  (let ((lit (make-hash-table :test 'equal))
        (lastc (make-hash-table :test 'equal))
        (suffixes nil)
        (comp (make-hash-table :test 'equal))
        (others nil) (has-slash nil) (base "")
        (idx (length pats)))
    (dolist (p pats)
      (let* ((text (aref p 0)) (flags (aref p 1)) (item (cons idx p)))
        (setq base (aref p 2))
        (cond
         ((and (memq 'nodir flags) (memq 'literal flags))
          (push item (gethash text lit)))
         ((and (memq 'nodir flags) (memq 'endswith flags))
          (let* ((suf (substring text 1))
                 (slot (or (assq (length suf) suffixes)
                           (car (push (cons (length suf) (make-hash-table :test 'equal))
                                      suffixes)))))
            (push item (gethash suf (cdr slot)))))
         ((memq 'nodir flags) (push item others))
         (t
          (setq has-slash t)
          (let* ((tx (if (and (> (length text) 0) (= (aref text 0) ?/)) (substring text 1) text))
                 (slash (string-search "/" tx))
                 (first (and slash (substring tx 0 slash))))
            (if (and first (not (ngit--glob-special-p first 0)))
                (push item (gethash first comp))
              (let* ((ls (let ((i (1- (length tx)))) (while (and (>= i 0) (/= (aref tx i) ?/)) (setq i (1- i))) i))
                     (last (substring tx (1+ ls))))
                (if (and (> (length last) 0) (not (ngit--glob-special-p last 0)))
                    (push item (gethash last lastc))
                  (push item others))))))))
      (setq idx (1- idx)))
    (vector lit suffixes comp others has-slash base lastc)))

(defun ngit--compiled-match (m path bstart isdir)
  "Return the matching pattern of highest index in compiled list M, or nil."
  (let* ((basename (if (= bstart 0) path (substring path bstart)))
         (blen (length basename))
         (cands (append (gethash basename (aref m 0)) (gethash basename (aref m 6)))))
    (dolist (s (aref m 1))
      (when (>= blen (car s))
        (setq cands (append (gethash (substring basename (- blen (car s))) (cdr s)) cands))))
    (when (aref m 4)
      (let* ((base (aref m 5))
             (rel (if (string= base "") path
                    (and (> (length path) (length base))
                         (string= base (substring path 0 (length base)))
                         (substring path (length base)))))
             (slash (and rel (let ((i 0) (n (length rel)))
                               (while (and (< i n) (/= (aref rel i) ?/)) (setq i (1+ i)))
                               (and (< i n) i)))))
        (when slash
          (setq cands (append (gethash (substring rel 0 slash) (aref m 2)) cands)))))
    (let ((best nil))
      (dolist (c cands)
        (when (and (or (null best) (> (car c) (car best)))
                   (ngit-pattern-match-p (cdr c) path bstart isdir))
          (setq best c)))
      (dolist (c (aref m 3))
        (when (and (or (null best) (> (car c) (car best)))
                   (ngit-pattern-match-p (cdr c) path bstart isdir))
          (setq best c)))
      (cdr best))))

(defun ngit-excluded-p (stack path basename-start isdir)
  "Non-nil when PATH is ignored.  STACK is a list of compiled pattern
lists (see `ngit-compile-ignore') in check order: deepest .gitignore
first, then info/exclude and core.excludesFile."
  (let ((found nil))
    (while (and stack (not found))
      (setq found (ngit--compiled-match (car stack) path basename-start isdir)
            stack (cdr stack)))
    (and found (not (memq 'negative (aref found 1))))))

;;;; gitattributes

(defconst ngit--attr-macros '(("binary" . (("diff" . unset) ("merge" . unset) ("text" . unset))))
  "Built-in attribute macros.")

(defun ngit--parse-attr-states (fields)
  (let ((states nil))
    (dolist (f fields)
      (push (cond ((string-prefix-p "-" f) (cons (substring f 1) 'unset))
                  ((string-prefix-p "!" f) (cons (substring f 1) 'unspecified))
                  ((string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" f)
                   (cons (match-string 1 f) (match-string 2 f)))
                  (t (cons f 'set)))
            states))
    (nreverse states)))

(defun ngit--attr-unquote (line)
  "C-unquote LINE's initial pattern, returning (PATTERN . END) or nil."
  (let ((position 1) (parts nil) (done nil) (valid t) quote slash end)
    (while (and valid (not done))
      (setq quote (string-search "\"" line position)
            slash (string-search "\\" line position)
            end (if (and slash (or (not quote) (< slash quote))) slash quote))
      (if (not end) (setq valid nil)
        (push (substring line position end) parts)
        (setq position (1+ end))
        (if (equal end quote) (setq done t)
          (if (>= position (length line)) (setq valid nil)
            (let* ((c (aref line position))
                   (escape (assq c '((?a . 7) (?b . 8) (?f . 12) (?n . 10)
                                     (?r . 13) (?t . 9) (?v . 11) (?\\ . 92) (?\" . 34)))))
              (cond (escape (push (unibyte-string (cdr escape)) parts)
                            (setq position (1+ position)))
                    ((and (<= ?0 c) (<= c ?3) (< (+ position 2) (length line))
                          (<= ?0 (aref line (1+ position))) (<= (aref line (1+ position)) ?7)
                          (<= ?0 (aref line (+ position 2))) (<= (aref line (+ position 2)) ?7))
                     (push (unibyte-string (string-to-number (substring line position (+ position 3)) 8)) parts)
                     (setq position (+ position 3)))
                    (t (setq valid nil))))))))
    (and valid done (cons (apply #'concat (nreverse parts)) position))))

(defun ngit--parse-attr-data (data base)
  "Return attribute patterns from DATA in directory BASE, last line first."
  (let ((pats nil))
    (dolist (l (ngit--pattern-lines data))
      (let* ((line (string-trim-left l "[ \t]+"))
             (quoted (and (string-prefix-p "\"" line) (ngit--attr-unquote line)))
             (fields (split-string (if quoted (substring line (cdr quoted)) line) "[ \t]+" t))
             ;; attr.c falls back to the raw token when C-unquoting fails.
             (pat (if quoted (car quoted) (pop fields))))
        ;; attr.c consumes the decoded buffer as a NUL-terminated string.
        (when quoted
          (let ((nul (string-search (unibyte-string 0) pat)))
            (when nul (setq pat (substring pat 0 nul)))))
        (when (and pat (not (string-prefix-p "#" line)) (not (string-prefix-p "[attr]" pat))
                   (not (string-prefix-p "!" pat)))
          (let ((p (ngit-parse-pattern pat base)))
            (aset p 3 (ngit--parse-attr-states fields))
            (push p pats)))))
    pats))

(defun ngit-read-attr-file (path base)
  "Return the attribute lines of PATH as patterns, last line first."
  (when (file-regular-p path)
    (ngit--parse-attr-data (ngit--read-file path) base)))

(defun ngit--read-worktree-attr-file (repo base)
  "Read BASE's .gitattributes, falling back to the index when absent.
An existing empty file must override the index just like any other file."
  (let* ((name (concat base ".gitattributes"))
         (top (ngit-repo-toplevel repo))
         (path (and top (expand-file-name (decode-coding-string name 'utf-8) top))))
    (if (and path (ngit--worktree-nofollow-p path name)
             (or (file-regular-p path)
                 ;; Git opens a directory as an empty attribute source.
                 (file-directory-p path)))
        (when (file-regular-p path)
          (let ((data (ngit--read-worktree-file path name)))
            (if data (ngit--parse-attr-data data base)
              ;; An open-time symlink race uses the same index fallback.
              (ngit--read-index-attr-file repo name base))))
      (when top
        (ngit--read-index-attr-file repo name base)))))

(defun ngit--read-index-attr-file (repo name base)
  "Read NAME's attribute blob from the index, with directory BASE."
  (require 'ngit-index)
  (let* ((entries (ngit-index-entries repo))
         (pos (ngit-index-find entries name)) (entry nil))
    ;; Git reads ours while the attribute file is unmerged.  Never
    ;; use the base or theirs when stage 0 and stage 2 are absent.
    (while (and (not entry) (< pos (length entries))
                (string= name (aref (aref entries pos) 0)))
      (when (memq (aref (aref entries pos) 3) '(0 2))
        (setq entry (aref entries pos)))
      (setq pos (1+ pos)))
    (when entry
      (let ((object (ngit-read-object repo (aref entry 1))))
        ;; read_blob_data_from_index checks object type, not entry mode.
        (when (eq (car object) 'blob)
          (ngit--parse-attr-data (cdr object) base))))))

(defun ngit-attr-stack (repo dir)
  "Return the attribute pattern lists that apply inside DIR (\"\" or
\"a/b/\"), highest precedence first.  Cached per directory."
  (let ((cache (or (aref repo 7) (aset repo 7 (make-hash-table :test 'equal)))))
    (or (gethash dir cache)
        (puthash
         dir
         (let ((top (ngit-repo-toplevel repo)))
           (if (string= dir "")
               (let* ((af (ngit-config-get repo "core.attributesfile"))
                      (xdg (getenv "XDG_CONFIG_HOME"))
                      (global (if (stringp af) (expand-file-name af)
                                (expand-file-name "git/attributes"
                                                  (if (and xdg (not (string= xdg ""))) xdg
                                                    (expand-file-name ".config" (getenv "HOME")))))))
                 (cons (ngit-read-attr-file
                        (expand-file-name "info/attributes" (ngit-repo-commondir repo)) "")
                       (delq nil (list (ngit--read-worktree-attr-file repo "")
                                      (ngit-read-attr-file global "")))))
             (let* ((parent-dir (let ((d (file-name-directory (substring dir 0 -1))))
                                  (or d "")))
                    (parent (ngit-attr-stack repo parent-dir))
                    (own (ngit--read-worktree-attr-file repo dir)))
               ;; info/attributes stays first; the deeper file comes next.
               (if own
                   (cons (car parent) (cons own (cdr parent)))
                 parent))))
         cache))))

(defun ngit-attrs (repo path names)
  "Return an alist (NAME . STATE) for attribute NAMES of PATH.
STATE is `set', `unset', a string value, or `unspecified'."
  (let* ((slash (let ((i (1- (length path)))) (while (and (>= i 0) (/= (aref path i) ?/)) (setq i (1- i))) i))
         (dir (if (< slash 0) "" (substring path 0 (1+ slash))))
         (bstart (1+ slash))
         ;; Resolve macro names even when only their expanded states were
         ;; requested.  A higher-priority -binary or !binary blocks expansion.
         (result (mapcar (lambda (n) (cons n nil))
                         (delete-dups (append names (mapcar #'car ngit--attr-macros)))))
         (requested (mapcar (lambda (name) (assoc name result)) names)))
    (catch 'done
      (dolist (pats (ngit-attr-stack repo dir))
        (dolist (pat pats)
          (when (ngit-pattern-match-p pat path bstart nil)
            ;; Later assignments on one line win, so walk it backwards.
            (dolist (st (reverse (aref pat 3)))
              (let* ((own (assoc (car st) result))
                     (expanded (cons st (and own (null (cdr own)) (eq (cdr st) 'set)
                                             (cdr (assoc (car st) ngit--attr-macros))))))
                (dolist (e expanded)
                  (let ((slot (assoc (car e) result)))
                    (when (and slot (null (cdr slot)))
                      (setcdr slot (cdr e)))))))
            ;; Macro slots track blocking assignments, but only requested
            ;; attributes determine whether the query is complete.
            (unless (rassq nil requested) (throw 'done nil))))))
    (mapcar (lambda (name) (cons name (or (cdr (assoc name result)) 'unspecified))) names)))

(provide 'ngit-match)

;;; ngit-match.el ends here
