;;; ngit-tag.el --- the tag command -*- lexical-binding: t; -*-

;;; Commentary:

;; Tag listing follows ref-filter.c: reachability peels to commits,
;; points-at checks every object in a tag chain, and dates belong to the
;; referenced object itself.  Writes reuse the branch ref transactions
;; without creating reflogs under refs/tags.

;;; Code:

(require 'ngit-branch)
(require 'ngit-commit)

;;;; Listing

(defun ngit--tag-object (repo item)
  "Read and cache ITEM's object and parsed headers."
  (unless (aref item 2)
    (let ((obj (ngit-read-object repo (ngit-unhex (aref item 1)))))
      (aset item 2 (car obj))
      (when (memq (car obj) '(tag commit))
        (aset item 3 (ngit-parse-commit (cdr obj))))))
  (aref item 3))

(defun ngit--tag-points-at (repo hex points)
  "Whether HEX or any object in its tag chain occurs in POINTS."
  (catch 'found
    (while hex
      (when (member hex points) (throw 'found t))
      (let ((obj (ngit-read-object repo (ngit-unhex hex))))
        (setq hex (and (eq (car obj) 'tag)
                       (ngit-header (ngit-parse-commit (cdr obj)) "object")))))
    nil))

(defconst ngit--tag-version-results
  [2 2 2 2 3 2 2 2 2
   2 -1 -1 1 3 3 1 3 3
   2 2 2 2 2 2 2 2 2
   2 1 1 -1 2 2 -1 2 2]
  "Git versioncmp result table: 2 compares bytes, 3 compares digit lengths.")

(defun ngit--tag-version-key (name)
  "Cache numeric runs as [START END LEADING-ZERO-END] for NAME."
  (let ((prefixes (split-string name "[0-9]+")) (pos 0) (runs nil))
    (dolist (number (split-string name "[^0-9]+" t))
      (setq pos (+ pos (length (pop prefixes))))
      (let* ((len (length number))
             (zeros (and (= (aref number 0) ?0)
                         (compare-strings number nil nil (make-string len ?0) nil nil))))
        (push (vector pos (+ pos len)
                      (and zeros (+ pos (if (eq zeros t) len (1- (abs zeros)))))) runs)
        (setq pos (+ pos len))))
    (vector name (nreverse runs))))

(defun ngit--tag-version-cmp (a b)
  "Git's versioncmp state machine, skipping the common prefix natively."
  (let* ((x (aref a 0)) (y (aref b 0))
         (common (compare-strings x nil nil y nil nil)))
    (if (eq common t) 0
      (let* ((pos (1- (abs common)))
             (cx (if (< pos (length x)) (aref x pos) 0))
             (cy (if (< pos (length y)) (aref y pos) 0))
             (dx (if (= cx ?0) 2 (if (and (> cx ?0) (<= cx ?9)) 1 0)))
             (dy (if (= cy ?0) 2 (if (and (> cy ?0) (<= cy ?9)) 1 0)))
             (run (seq-find (lambda (r) (and (< (aref r 0) pos) (<= pos (aref r 1))))
                            (aref a 1)))
             (state (if (null run) 0
                      (if (null (aref run 2)) 3
                        (if (<= pos (aref run 2)) 9 6))))
             (result (aref ngit--tag-version-results (+ (* (+ state dx) 3) dy))))
        (cond
         ((= result 2) (- cx cy))
         ((= result 3)
          (let ((rx (seq-find (lambda (r) (and (<= (aref r 0) pos) (< pos (aref r 1)))) (aref a 1)))
                (ry (seq-find (lambda (r) (and (<= (aref r 0) pos) (< pos (aref r 1)))) (aref b 1))))
            (if (= (aref rx 1) (aref ry 1)) (- cx cy)
              (- (aref rx 1) (aref ry 1)))))
         (t result))))))

(defun ngit--tag-sort (repo items keys icase)
  "Sort ITEMS by KEYS, last key primary; cache values before sorting."
  (let ((specs nil))
    (dolist (key keys)
      (let* ((desc (string-prefix-p "-" key))
             (name (if desc (substring key 1) key)))
        (unless (member name '("refname" "version:refname" "v:refname"
                               "creatordate" "taggerdate"))
          (ngit--die "fatal: unknown field name: %s" name))
        (push (cons name desc) specs)))
    (dolist (it items)
      (let ((values nil))
        (dolist (spec specs)
          (let ((key (car spec)))
            (push
             (cond
              ((member key '("version:refname" "v:refname"))
               (ngit--tag-version-key (aref it 0)))
              ((member key '("creatordate" "taggerdate"))
                 (let* ((parsed (ngit--tag-object repo it))
                        (header (cond ((eq (aref it 2) 'tag) "tagger")
                                      ((and (eq (aref it 2) 'commit)
                                            (string= key "creatordate")) "committer")))
                        (ident (and header (ngit-header parsed header))))
                   (if ident (ngit--ident-date ident) 0)))
              (t (aref it 4)))
             values)))
        (aset it 5 (nreverse values))))
    (sort items
          (lambda (a b)
            (let ((va (aref a 5)) (vb (aref b 5)) (ks specs)
                  (decided nil) (result nil))
              (while (and ks (not decided))
                (let ((x (pop va)) (y (pop vb)) (spec (pop ks)))
                  (unless (equal x y)
                    (setq decided t
                          result (if (member (car spec) '("version:refname" "v:refname"))
                                     (if (cdr spec) (> (ngit--tag-version-cmp x y) 0)
                                       (< (ngit--tag-version-cmp x y) 0))
                                   (if (cdr spec)
                                       (if (stringp x) (string< y x) (< y x))
                                     (if (stringp x) (string< x y) (< x y))))))))
              (if decided result
                (string< (if icase (aref a 4) (aref a 0))
                         (if icase (aref b 4) (aref b 0)))))))))

(defun ngit--tag-list (repo patterns lines keys icase filters &optional format column color)
  (let ((items nil) (resolved nil)
        (ngit--ref-color (if (eq color 'default) (equal (ngit-config-get repo "color.ui") "always") color)))
    (setq ngit--rv-repo repo)
    (dolist (key '(:contains :no-contains :merged :no-merged :points-at))
      (let ((values nil))
        (dolist (rev (plist-get filters key))
          (let ((hex (condition-case nil (ngit-rev-parse repo (encode-coding-string rev 'utf-8)) (error nil))))
            (unless hex (ngit--die-with 129 "error: malformed object name %s" rev))
            (push (if (eq key :points-at) hex
                    (or (ngit--commit-of repo hex)
                        (progn
                          (ngit--branch-error "object %s is a %s, not a commit" hex
                                              (ngit--object-type repo hex))
                          (if (memq key '(:merged :no-merged))
                              (ngit--die-with 129 "error: option `%s' must point to a commit"
                                              (substring (symbol-name key) 1))
                            (ngit--die-with 129 "error: no such commit %s" rev)))))
                  values)))
        (setq resolved (plist-put resolved key values))))
    (setq patterns (mapcar (lambda (p) (encode-coding-string p 'utf-8)) patterns))
    (when icase (setq patterns (mapcar #'downcase patterns)))
    (let ((with (plist-get resolved :contains)) (without (plist-get resolved :no-contains))
          (merged (plist-get resolved :merged)) (unmerged (plist-get resolved :no-merged))
          (points (plist-get resolved :points-at)))
      (dolist (ref (ngit-list-refs repo "refs/tags/"))
        (let* ((name (substring (car ref) 10)) (hex (nth 1 ref))
               (folded (if icase (downcase name) name)))
          (when (and (or (null patterns)
                         (seq-find (lambda (p) (ngit-wildmatch p folded)) patterns))
                     (or (null points) (ngit--tag-points-at repo hex points))
                     (or (not (or with without merged unmerged))
                         (let ((c (ngit--commit-of repo hex)))
                           (and c
                                (or (null with) (seq-find (lambda (w) (ngit--in-merge-bases repo w c)) with))
                                (or (null without) (not (seq-find (lambda (w) (ngit--in-merge-bases repo w c)) without)))
                                (or (null merged) (seq-find (lambda (m) (ngit--in-merge-bases repo c m)) merged))
                                (or (null unmerged) (not (seq-find (lambda (m) (ngit--in-merge-bases repo c m)) unmerged)))))))
            ;; NAME HEX TYPE PARSED SORT-NAME SORT-VALUES.
            (push (vector name hex nil nil folded nil) items)))))
    (setq items (ngit--tag-sort repo items
                                (or keys (let ((v (ngit-config-get repo "tag.sort")))
                                           (and (stringp v) (list v)))
                                    '("refname")) icase))
    (if (or format (and (null lines) (ngit--column-options repo "tag" column)))
        (let ((output (mapcar (lambda (it) (if format (ngit--ref-format repo (concat "refs/tags/" (aref it 0)) (aref it 1) format) (aref it 0))) items))
              (options (and (null lines) (ngit--column-options repo "tag" column))))
          (if options (ngit--print-columns output options 2)
            (dolist (line output) (ngit--emit line "\n"))))
    (dolist (it items)
      (if (or (null lines) (= lines 0))
          (ngit--emit (aref it 0) "\n")
        (let* ((parsed (ngit--tag-object repo it))
               (msg (or (plist-get parsed :message) ""))
               (start 0) (limit (length msg)) (parts nil) (count 0))
          ;; contents:lines excludes signatures, including when listing
          ;; tags created by another implementation with signing enabled.
          (dolist (marker '("-----BEGIN PGP SIGNATURE-----"
                             "-----BEGIN SIGNED MESSAGE-----"
                             "-----BEGIN SSH SIGNATURE-----"))
            (let ((pos (if (string-prefix-p marker msg) 0
                         (let ((hit (string-search (concat "\n" marker) msg)))
                           (and hit (1+ hit))))))
              (when pos (setq limit (min limit pos)))))
          ;; find_subpos skips leading empty lines, but keeps other spaces.
          (while (and (< start limit) (= (aref msg start) ?\n))
            (setq start (1+ start)))
          (while (and (< start limit) (< count lines))
            (let ((end (min limit (or (string-search "\n" msg start) limit))))
              (push (substring msg start end) parts)
              (setq start (1+ end) count (1+ count))))
          (ngit--emit (ngit--pad-to (aref it 0) 15) " "
                      (mapconcat #'identity (nreverse parts) "\n    ") "\n")))))
    0))

;;;; Writes

(defun ngit--tag-delete (repo names)
  (let ((found nil) (status 0) (seen nil) (duplicate nil))
    ;; Report missing tags before printing any successful deletions.
    (dolist (name names)
      (let* ((ref (concat "refs/tags/" (encode-coding-string name 'utf-8)))
             (hex (and (ngit--valid-branch-name-p (concat "tag/" name))
                       (ngit-resolve-ref repo ref))))
        (if hex (progn
                  (when (member ref seen) (setq duplicate ref))
                  (push ref seen)
                  (push (list name ref hex) found))
          (ngit--branch-error "tag '%s' not found." name)
          (setq status 1))))
    (if duplicate
        (progn
          (ngit--branch-error "could not delete references: multiple updates for ref '%s' not allowed" duplicate)
          (setq status 1))
      (dolist (it (nreverse found))
        (ngit--delete-ref repo (nth 1 it))
        (ngit--emit (format "Deleted tag '%s' (was %s)\n" (car it)
                            (ngit-abbrev repo (nth 2 it) (ngit-default-abbrev repo))))))
    status))

(defun ngit--tag-create (repo names annotate given message file force cleanup &optional edit)
  (when (cddr names) (ngit--die "fatal: too many arguments"))
  (when (and given file)
    (ngit--die "fatal: options '-F' and '-m' cannot be used together"))
  (when file
    (unless (file-exists-p file)
      (ngit--die "fatal: could not open or read '%s': No such file or directory" file))
    (setq message (ngit--read-file (expand-file-name file))))
  (let* ((name (car names)) (rev (or (cadr names) "HEAD"))
         (hex (condition-case nil (ngit-rev-parse repo (encode-coding-string rev 'utf-8)) (error nil)))
         (ref (concat "refs/tags/" (encode-coding-string name 'utf-8))) (old nil))
    (unless hex (ngit--die "fatal: Failed to resolve '%s' as a valid ref." rev))
    ;; Prefixing permits HEAD and @, which are valid tag names.
    (unless (and name (not (string-prefix-p "-" name))
                 (ngit--valid-branch-name-p (concat "tag/" name)))
      (ngit--die "fatal: '%s' is not a valid tag name." name))
    (setq old (ngit-resolve-ref repo ref))
    (when (and old (not force)) (ngit--die "fatal: tag '%s' already exists" name))
    (unless (ngit-object-exists-p repo (ngit-unhex hex))
      (if (or annotate given file) (ngit--die "fatal: bad object type.")
        (ngit--die "fatal: cannot update ref '%s': trying to write ref '%s' with nonexistent object %s"
                   ref ref hex)))
    (when (or annotate given file edit)
      (unless (member cleanup '("strip" "whitespace" "verbatim"))
        (ngit--die "fatal: Invalid cleanup mode %s" cleanup))
      (let ((tagger (ngit-ident repo "COMMITTER"))
            (message-path (expand-file-name "TAG_EDITMSG" (ngit-repo-gitdir repo)))
            (editing (or edit (not (or given file)))))
      (when editing
        (require 'ngit-editor)
        (let ((comment (ngit-editor-comment repo)))
          (ngit-write-file
           message-path
           (cond ((or given file) (if (string-suffix-p "\n" message) message (concat message "\n")))
                 ((and old (eq (ngit--object-type repo old) 'tag))
                  (ngit--message-body (cdr (ngit-read-object repo (ngit-unhex old)))))
                 (old "")
                 (t (concat "\n" comment "\n" comment " Write a message for tag:\n"
                            comment "   " name "\n" comment " Lines starting with '" comment "' "
                            (if (equal cleanup "strip") "will be ignored.\n"
                              "will be kept; you may remove them yourself if you want to.\n"))))))
        (set-file-modes message-path #o600)
        (when (/= (ngit-editor-run repo message-path) 0)
          (ngit--die-with 1 "Please supply the message using either -m or -F option."))
        (setq message (ngit--read-file message-path)))
      (setq message (if (multibyte-string-p message) (encode-coding-string message 'utf-8) message))
      (when (string= cleanup "strip")
        (let ((comment "#"))
          ;; config.c's auto assignment sets a flag without replacing the
          ;; previous comment string.  Tags never auto-select a new string.
          ;; Keep non-editing tags on their existing lazy-load fast path.
          (dolist (entry (ngit-config-entries repo))
            (when (and (member (car entry) '("core.commentchar" "core.commentstring"))
                       (not (and (stringp (cdr entry))
                                 (equal (downcase (cdr entry)) "auto"))))
              (setq comment (cdr entry))))
          (setq message (mapconcat #'identity
                                   (seq-remove (lambda (line) (string-prefix-p comment line))
                                               (split-string message "\n")) "\n"))))
      (unless (string= cleanup "verbatim") (setq message (ngit-stripspace message)))
      (when (and (not (or given file)) (equal message ""))
        (ngit--die "fatal: no tag message?"))
      (let* ((type (ngit--object-type repo hex))
             (data (concat "object " hex "\ntype " (symbol-name type)
                           "\ntag " (encode-coding-string name 'utf-8)
                           "\ntagger " tagger "\n\n" message)))
        (when (and (eq type 'tag) (not (equal (ngit-config-get repo "advice.nestedtag") "false")))
          (ngit-native-write-stderr
           (concat "hint: You have created a nested tag. The object referred to by your new tag is\n"
                   "hint: already a tag. If you meant to tag the object that it points to, use:\n"
                   "hint:\n"
                   "hint: \tgit tag -f " name " " rev "^{}\n"
                   "hint: Disable this message with \"git config advice.nestedTag false\"\n")))
        (setq hex (ngit-hex (ngit-write-object repo "tag" data))))))
    (dolist (existing (ngit-list-refs repo "refs/tags/"))
      (let ((other (car existing)))
        (when (or (string-prefix-p (concat other "/") ref)
                  (string-prefix-p (concat ref "/") other))
          (when (or annotate given file)
            (ngit-native-write-stderr "The tag message has been left in .git/TAG_EDITMSG\n"))
          (ngit--die "fatal: cannot lock ref '%s': '%s' exists; cannot create '%s'" ref other ref))))
    (ngit--write-ref repo ref hex old nil)
    (when (and (or annotate given file edit)
               (file-exists-p (expand-file-name "TAG_EDITMSG" (ngit-repo-gitdir repo))))
      (delete-file (expand-file-name "TAG_EDITMSG" (ngit-repo-gitdir repo))))
    (when (and old force (not (string= old hex)))
      (ngit--emit (format "Updated tag '%s' (was %s)\n" name
                          (ngit-abbrev repo old (ngit-default-abbrev repo)))))
    0))

;;;; Command line

(defun ngit-cmd-tag (repo args)
  (let ((list nil) (delete nil) (annotate nil) (force nil) (icase nil)
        (given nil) (message "") (file nil) (lines nil) (keys nil)
        (filters nil) (names nil) (cleanup "strip") (edit nil) (format nil) (column nil) (color 'default))
    (while args
      (let ((a (pop args)))
        ;; Boolean short options may precede another option in one word.
        (when (and (> (length a) 2) (= (aref a 0) ?-)
                   (memq (aref a 1) '(?a ?f ?d ?l ?i)))
          (push (concat "-" (substring a 2)) args)
          (setq a (substring a 0 2)))
        (cond
         ((string= a "--") (setq names (append names args) args nil))
         ((member a '("-l" "--list")) (setq list t))
         ((member a '("-d" "--delete")) (setq delete t))
         ((member a '("-a" "--annotate")) (setq annotate t))
         ((member a '("-e" "--edit")) (setq edit t annotate t))
         ((member a '("-f" "--force")) (setq force t))
         ((member a '("-i" "--ignore-case")) (setq icase t))
         ((or (member a '("-m" "--message")) (string-prefix-p "--message=" a)
              (and (string-prefix-p "-m" a) (> (length a) 2)))
          (let ((v (cond ((member a '("-m" "--message")) (pop args))
                         ((string-prefix-p "--message=" a) (substring a 10))
                         (t (substring a 2)))))
            (unless v (ngit--die-with 129 "error: option `%s' requires a value" a))
            (setq given t message (concat message (if (string= message "") "" "\n\n") v))))
         ((or (member a '("-F" "--file")) (string-prefix-p "--file=" a)
              (and (string-prefix-p "-F" a) (> (length a) 2)))
          (setq file (cond ((member a '("-F" "--file")) (pop args))
                           ((string-prefix-p "--file=" a) (substring a 7))
                           (t (substring a 2))))
          (unless file (ngit--die-with 129 "error: option `%s' requires a value" a)))
         ((string-prefix-p "-n" a)
          (setq lines (if (string= a "-n") 1
                        (if (string-match-p "\\`[0-9]+\\'" (substring a 2))
                            (string-to-number (substring a 2))
                          (ngit--die-with 129 "error: option `n' expects a numerical value")))))
         ((or (string= a "--sort") (string-prefix-p "--sort=" a))
          (let ((v (if (string= a "--sort") (pop args) (substring a 7))))
            (unless v (ngit--die-with 129 "error: option `sort' requires a value"))
            (setq keys (append keys (list v)))))
         ((or (string= a "--format") (string-prefix-p "--format=" a))
          (setq format (if (string= a "--format") (pop args) (substring a 9)) list t)
          (unless format (ngit--die-with 129 "error: option `format' requires a value")))
         ((or (string= a "--column") (string-prefix-p "--column=" a))
          (setq column (if (string= a "--column") "always" (concat "always," (substring a 9)))))
         ((string= a "--no-column") (setq column "never"))
         ((or (string= a "--color") (string-prefix-p "--color=" a))
          (let ((v (if (string= a "--color") "always" (substring a 8))))
            (unless (member v '("always" "never" "auto")) (ngit--die "fatal: bad color value '%s' for variable 'command line option'" v))
            (setq color (equal v "always"))))
         ((string= a "--no-color") (setq color nil))
         ((string-prefix-p "--cleanup=" a) (setq cleanup (substring a 10)))
         ((string-match "\\`--\\(contains\\|no-contains\\|merged\\|no-merged\\|points-at\\)\\(?:=\\(.*\\)\\)?\\'" a)
          (let* ((key (intern (concat ":" (match-string 1 a))))
                 (value (or (match-string 2 a)
                            (and args (not (string-prefix-p "-" (car args))) (pop args)) "HEAD")))
            (setq filters (plist-put filters key (append (plist-get filters key) (list value))))))
         ((string-prefix-p "-" a) (ngit--die "ngit: tag: unsupported argument %s" a))
         (t (setq names (append names (list a)))))))
    (when (and lines column (ngit--column-options repo "tag" column))
      (ngit--die "fatal: options '--column' and '-n' cannot be used together"))
    (when (and (not delete) (or (null names) lines filters)) (setq list t))
    (cond
     (delete (when (or list annotate given file force lines filters)
               (ngit--die "ngit: tag: incompatible options with --delete"))
             (ngit--tag-delete repo names))
     (list (when (or annotate given file force)
             (ngit--die "ngit: tag: incompatible options with --list"))
           (ngit--tag-list repo names lines keys icase filters format column color))
     (t (ngit--tag-create repo names annotate given message file force cleanup edit)))))

(provide 'ngit-tag)

;;; ngit-tag.el ends here
