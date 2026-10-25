;;; ngit-maint.el --- packs, pruning, refs and reflogs -*- lexical-binding: t; -*-

;;; Commentary:

;; Repository maintenance shares the object enumerator and pack writer.  A
;; `gc' run keeps its reachability closure alive across repack and prune; the
;; real-repository corpus is large enough that walking it twice is material.

;;; Code:

(require 'ngit-objects)
(require 'ngit-write)
(require 'ngit-commit)
(require 'ngit-pack nil t)

(declare-function ngit-pack-write "ngit-pack")

(defconst ngit-maint--zero (make-string 40 ?0))
(defconst ngit-maint--max-time #x7fffffffffffffff)
(defconst ngit-maint--sys-unlink 87)

(defvar ngit-maint--path-memory nil)
(defvar ngit-maint--stat-memory nil)
(defvar ngit-maint--gc-reachable nil)
(defvar ngit-maint--record-mtimes nil)

;;;; Files and object listings

(defun ngit-maint--write-c-string (text)
  "Write TEXT as a NUL-terminated native string and return its address."
  (let ((bytes (encode-coding-string text 'utf-8)))
    (unless (and ngit-maint--path-memory
                 (> (aref ngit-maint--path-memory 3) (length bytes)))
      (when ngit-maint--path-memory
        (nl-ffi-memory-release ngit-maint--path-memory))
      (setq ngit-maint--path-memory
            (nl-ffi-memory-allocate (max 4096 (1+ (length bytes))))))
    (let ((addr (nl-ffi-memory-address ngit-maint--path-memory)))
      (ptr-write-bytes addr bytes)
      (ptr-write-u8 addr (length bytes) 0)
      addr)))

(defun ngit-maint--stat (path)
  "Return [MODE SIZE BLOCKS MTIME] from lstat(2), or nil."
  (unless ngit-maint--stat-memory
    (setq ngit-maint--stat-memory (nl-ffi-memory-allocate 256)))
  (let ((sa (nl-ffi-memory-address ngit-maint--stat-memory)))
    (and (>= (syscall-direct 6 (ngit-maint--write-c-string path) sa 0 0 0 0) 0)
         (vector (ptr-read-u32 sa 24) (ptr-read-u64 sa 48)
                 (ptr-read-u64 sa 64) (ptr-read-u64 sa 88)))))

(defun ngit-maint--regular-p (stat)
  (= (logand (aref stat 0) #o170000) #o100000))

(defun ngit-maint--loose-path (repo hex)
  (ngit--loose-path repo hex))

(defun ngit-maint--delete-empty-parents (dir stop)
  "Delete empty directories from DIR up to, but excluding, STOP."
  (let ((dir (directory-file-name dir)) (stop (directory-file-name stop)))
    (while (and (string-prefix-p (concat stop "/") dir)
                (file-directory-p dir)
                (null (directory-files dir nil "\\`[^.]\\|\\`\\.[^.]")))
      (delete-directory dir)
      (setq dir (directory-file-name (file-name-directory dir))))))

(defun ngit-maint--delete-loose (repo hex &optional keep-directory)
  (let ((path (ngit-maint--loose-path repo hex)))
    (when (file-exists-p path)
      (delete-file path)
      (unless keep-directory
        (ngit-maint--delete-empty-parents
         (file-name-directory path) (ngit--objects-dir repo))))))

(defun ngit-maint--invalidate-packs (repo)
  (aset repo 3 nil)
  (setq ngit--pack-order nil)
  (clrhash ngit-native--sizes)
  (ngit-abbrev-forget))

(defun ngit-maint--invalidate-commit-graphs (repo)
  "Remove optional commit-graph caches before objects can disappear."
  ;; ngit does not rewrite graphs.  Leaving an expired commit in either graph
  ;; format makes native Git's fsck fail after its last object copy is removed.
  (let* ((info (expand-file-name "objects/info" (ngit-repo-commondir repo)))
         (split (expand-file-name "commit-graphs" info)))
    ;; Unpublish the split chain before removing any of its graph files.
    (dolist (path (list (expand-file-name "commit-graph" info)
                        (expand-file-name "commit-graph-chain" split)))
      (when (file-exists-p path) (delete-file path)))
    (when (file-directory-p split)
      (dolist (path (directory-files split t "\\`graph-[0-9a-f]+\\.graph\\'"))
        (delete-file path)))))

(defun ngit-maint--pack-files (repo)
  "Return existing pack basenames, without extensions."
  (let ((dir (expand-file-name "objects/pack" (ngit-repo-commondir repo))) out)
    (when (file-directory-p dir)
      (dolist (path (directory-files dir t "\\.pack\\'"))
        (push (substring path 0 -5) out)))
    out))

(defun ngit-maint--remove-pack (base)
  ;; A .keep file protects its pack from both repacking and deletion.  The
  ;; writer may still duplicate its reachable objects, which is harmless.
  (unless (file-exists-p (concat base ".keep"))
    (dolist (ext '(".pack" ".idx" ".rev" ".bitmap" ".mtimes"))
      (let ((path (concat base ext)))
        (when (file-exists-p path) (delete-file path))))))

(defun ngit-maint--prune-packed (repo dry-run &optional known)
  "Remove loose objects already present in a pack.
KNOWN is a membership table for a pack successfully written by this command."
  (let (dirs)
    (dolist (hex (ngit-pack-loose-oids repo))
      (when (or (and known (gethash hex known))
                (ngit--find-packed repo (ngit-unhex hex)))
        (let ((path (ngit-maint--loose-path repo hex)))
          (if dry-run
              (ngit--emit "rm -f " (file-relative-name path default-directory) "\n")
            ;; The loose listing already supplied PATH.  Unlink directly;
            ;; another process removing it first is harmless (ENOENT).
            (let ((rc (syscall-direct ngit-maint--sys-unlink
                                      (ngit-maint--write-c-string path) 0 0 0 0 0)))
              (cond
               ((= rc -38)
                (when (file-exists-p path) (delete-file path)))
               ((not (or (= rc 0) (= rc -2)))
                (error "ngit: cannot remove %s (errno %d)" path (- rc)))))
            (let ((prefix (substring hex 0 2)))
              (unless (equal prefix (car dirs)) (push prefix dirs)))))))
    (unless dry-run
      ;; Names are sorted, so each touched fanout appears once.  Avoid
      ;; rescanning its shrinking directory after every object deletion.
      (dolist (prefix dirs)
        (ngit-maint--delete-empty-parents
         (concat (ngit--objects-dir repo) prefix) (ngit--objects-dir repo)))
      (ngit-forget-loose-names))))

;;;; Repack

(defun ngit-maint--reachable (repo reflogs)
  (or ngit-maint--gc-reachable
      (setq ngit-maint--gc-reachable
            (ngit-pack-reachable repo (ngit-pack-roots repo reflogs t)))))

(defun ngit-maint--protected-reachable (repo cutoff)
  "Return normal reachability plus recent loose objects and their closure."
  (let ((roots (ngit-pack-roots repo t t)) recent)
    (dolist (hex (ngit-pack-loose-oids repo))
      (let ((stat (ngit-maint--stat (ngit-maint--loose-path repo hex))))
        (when (and stat (> (aref stat 3) cutoff)) (push hex recent))))
    ;; Visit typed ref edges first, so blobs reached there are never inflated
    ;; when the same names also occur among the untyped recent roots.
    (ngit-pack-reachable repo (append roots (nreverse recent)))))

(defun ngit-maint--pack-mtimes (pack)
  "Return PACK's validated mtimes bytes, or nil when it has none."
  (let* ((n (aref pack 2)) (path (aref pack 0))
         (mtimes-path (concat (substring path 0 -5) ".mtimes"))
         (mtimes (and (file-exists-p mtimes-path) (ngit--read-file mtimes-path))))
    (when mtimes
      (let* ((trailer (+ 12 (* 4 n)))
             (pack-stat (ngit-maint--stat path))
             (pack-hash (and pack-stat
                             (ngit-native-read-bytes path (- (aref pack-stat 1) 20) 20))))
        (unless (and (= (length mtimes) (+ trailer 40))
                   (string= (substring mtimes 0 4) "MTME")
                   (= (ngit--be32 mtimes 4) 1) (= (ngit--be32 mtimes 8) 1))
          (error "fatal: invalid pack mtimes file"))
        (unless (and (string= (substring mtimes trailer (+ trailer 20)) pack-hash)
                     (string= (substring mtimes (+ trailer 20))
                              (secure-hash 'sha1 (substring mtimes 0 (+ trailer 20))
                                           nil nil t)))
          (error "fatal: pack mtimes checksum mismatch"))))
    mtimes))

(defun ngit-maint--include-packed (repo oids cutoff)
  "Protect packed objects newer than CUTOFF and all their dependencies."
  (let ((seen (make-hash-table :test 'equal)) out)
    (dolist (hex oids) (puthash hex t seen) (push hex out))
    (dolist (pack (ngit-repo-packs repo))
      (let* ((n (aref pack 2))
             (path (aref pack 0))
             (mtimes (ngit-maint--pack-mtimes pack))
             (mtime (aref (ngit-maint--stat path) 3))
             (names (ngit-native-read-bytes (aref pack 1) 1032 (* 20 n)))
             (i 0))
        (while (< i n)
          (when (> (if mtimes (ngit--be32 mtimes (+ 12 (* 4 i))) mtime) cutoff)
            (let ((hex (ngit-hex (substring names (* i 20) (* (1+ i) 20)))))
              (unless (gethash hex seen)
                (puthash hex t seen)
                (push hex out))))
          (setq i (1+ i)))))
    ;; A retained packed tree or commit can refer to an old loose object;
    ;; protecting only the packed names would lose that dependency.
    (ngit-pack-reachable repo (nreverse out))))

(defun ngit-maint--object-mtimes (repo oids fallback)
  "Return an OID-to-mtime table for OIDS before their source packs change."
  (let ((wanted (make-hash-table :test 'equal))
        (mtimes (make-hash-table :test 'equal)))
    (dolist (hex oids) (puthash hex t wanted))
    (dolist (hex (ngit-pack-loose-oids repo))
      (when (gethash hex wanted)
        (let ((stat (ngit-maint--stat (ngit-maint--loose-path repo hex))))
          (when stat (puthash hex (aref stat 3) mtimes)))))
    (dolist (pack (ngit-repo-packs repo))
      (let* ((n (aref pack 2)) (pack-mtimes (ngit-maint--pack-mtimes pack))
             (pack-mtime (aref (ngit-maint--stat (aref pack 0)) 3))
             (names (ngit-native-read-bytes (aref pack 1) 1032 (* 20 n)))
             (i 0))
        (while (< i n)
          (let* ((hex (ngit-hex (substring names (* i 20) (* (1+ i) 20))))
                 (mtime (if pack-mtimes
                            (ngit--be32 pack-mtimes (+ 12 (* 4 i)))
                          pack-mtime)))
            ;; Git also keeps the newest observation when an object occurs in
            ;; more than one input source.
            (when (and (gethash hex wanted)
                       (> mtime (or (gethash hex mtimes) 0)))
              (puthash hex mtime mtimes)))
          (setq i (1+ i)))))
    (dolist (hex oids)
      (unless (gethash hex mtimes) (puthash hex fallback mtimes)))
    mtimes))

(defun ngit-maint--write-pack-mtimes (base sha mtimes)
  "Write the MTME sidecar for BASE-SHA using object times from MTIMES."
  (let* ((idx (concat base "-" sha ".idx"))
         (n (ngit--be32 (ngit-native-read-bytes idx 1028 4) 0))
         (names (ngit-native-read-bytes idx 1032 (* 20 n)))
         (parts nil)
         (i 0))
    (while (< i n)
      (let ((hex (ngit-hex (substring names (* i 20) (* (1+ i) 20)))))
        (push (ngit--be32-bytes (or (gethash hex mtimes) 0)) parts))
      (setq i (1+ i)))
    (setq parts (nreverse parts))
    (let* ((body (concat "MTME" (ngit--be32-bytes 1) (ngit--be32-bytes 1)
                         (mapconcat #'identity parts "") (ngit-unhex sha)))
           (data (concat body (secure-hash 'sha1 body nil nil t)))
           (path (concat base "-" sha ".mtimes"))
           (lock (ngit-lock path)))
      (unwind-protect
          (ngit-commit-lock lock data)
        (ngit-unlock lock)))))

(defun ngit-maint--repack (repo all delete quiet &optional objects window depth)
  "Perform a supported repack and return its new pack checksum, or nil."
  (let* ((reachable (or objects (ngit-maint--reachable repo t)))
         (oids (if all
                   reachable
                 (let ((loose (make-hash-table :test 'equal)) out)
                   (dolist (hex (ngit-pack-loose-oids repo)) (puthash hex t loose))
                   (dolist (hex reachable)
                     (when (gethash hex loose) (push hex out)))
                   (nreverse out))))
         (old (ngit-maint--pack-files repo))
         (base (expand-file-name "objects/pack/pack" (ngit-repo-commondir repo)))
         (mtimes (and ngit-maint--record-mtimes
                      (ngit-maint--object-mtimes
                       repo oids ngit-maint--record-mtimes)))
         (sha nil))
    ;; A full deleting repack can discard unreachable packed objects even
    ;; when there are no loose objects for the later prune step to remove.
    (when (and delete all) (ngit-maint--invalidate-commit-graphs repo))
    (if (null oids)
        (progn
          (unless quiet (ngit--emit "Nothing new to pack.\n"))
          ;; A full deleting repack must also remove an input pack when every
          ;; object in it expired.  There is no replacement pack in this case.
          (when (and delete all)
            (dolist (pack old) (ngit-maint--remove-pack pack))
            (ngit-maint--invalidate-packs repo))
          (when delete (ngit-maint--prune-packed repo nil)))
      (ngit-make-directory (file-name-directory base) t)
      (setq sha (ngit-pack-write repo oids base window depth t))
      (when mtimes (ngit-maint--write-pack-mtimes base sha mtimes))
      (ngit-maint--invalidate-packs repo)
      (when (and delete all)
        (let ((new (concat base "-" sha)))
          (dolist (pack old)
            (unless (string= pack new) (ngit-maint--remove-pack pack))))
        (ngit-maint--invalidate-packs repo))
      (when delete
        (ngit-maint--prune-packed
         repo nil (and (consp ngit-object--closure)
                       (eq oids (car ngit-object--closure))
                       (cdr ngit-object--closure)))))
    sha))

(defun ngit-cmd-repack (repo args)
  (let ((all nil) (delete nil) (quiet nil) (window nil) (depth nil))
    (while args
      (let ((a (pop args)))
        (cond
         ((or (string= a "-a") (string= a "--all")) (setq all t))
         ((string= a "-d") (setq delete t))
         ((or (string= a "-q") (string= a "--quiet")) (setq quiet t))
         ((or (string= a "-l") (string= a "--local")) nil)
         ((string-prefix-p "--window=" a) (setq window (string-to-number (substring a 9))))
         ((string-prefix-p "--depth=" a) (setq depth (string-to-number (substring a 8))))
         ((member a '("--window" "--depth"))
          (let ((value (or (pop args)
                           (ngit--die-with 129 "error: option '%s' requires a value" a))))
            (if (string= a "--window") (setq window (string-to-number value))
              (setq depth (string-to-number value)))))
         ((string-prefix-p "-" a)
          (ngit--die-with 129 "error: unsupported repack option %s" a))
         (t (ngit--die-with 129 "usage: git repack [<options>]")))))
    (let ((ngit-maint--gc-reachable nil))
      (ngit-maint--repack repo all delete quiet nil window depth))
    0))

;;;; pack-refs

(defun ngit-maint--packable-ref-p (name)
  (and (string-prefix-p "refs/" name)
       (not (string-prefix-p "refs/bisect/" name))
       (not (string-prefix-p "refs/worktree/" name))))

(defun ngit-maint--packed-ref-line (repo ref)
  (let* ((name (nth 0 ref)) (hex (nth 1 ref))
         (obj (ngit-read-object repo (ngit-unhex hex))))
    (concat hex " " name "\n"
            (if (eq (car obj) 'tag)
                (concat "^" (ngit-peel repo hex nil) "\n")
              ""))))

(defun ngit-maint--pack-refs (repo prune)
  (let* ((common (ngit-repo-commondir repo))
         (path (expand-file-name "packed-refs" common))
         (refs nil) (loose nil))
    (dolist (ref (ngit-list-refs repo))
      (when (and (ngit-maint--packable-ref-p (car ref))
                 (null (nth 2 ref)))
        (push ref refs)
        (let ((file (ngit--ref-file repo (car ref))))
          (when (file-regular-p file) (push (cons (car ref) file) loose)))))
    (setq refs (sort refs (lambda (a b) (string< (car a) (car b)))))
    (let ((lock (ngit-lock path)) input)
      (unwind-protect
          (progn
            (when (ngit-hook-exists-p repo "reference-transaction")
              (setq input (ngit-ref-prepare
                           repo (delq nil
                                      (mapcar (lambda (ref)
                                                (when (assoc (car ref) loose)
                                                  (list nil (cadr ref) (car ref))))
                                              refs)) nil (list lock))))
            (ngit-commit-lock
             lock
             (concat "# pack-refs with: peeled fully-peeled sorted \n"
                     (mapconcat (lambda (ref) (ngit-maint--packed-ref-line repo ref))
                                refs "")))
            (setq lock nil)
            (ngit-ref-finish repo input))
        (when lock (ngit-unlock lock))))
    (when prune
      (dolist (item loose)
        (let ((value (ngit--trim-eol (ngit--read-file (cdr item))))
              (packed (cdr (assoc (car item)
                                  (mapcar (lambda (r) (cons (car r) (cadr r))) refs)))))
          (when (and packed (string= value packed))
            (let ((ref-lock (ngit-lock (cdr item))) input)
              (unwind-protect
                  (progn
                    (setq input (ngit-ref-prepare repo (list (list value nil (car item)))
                                                  nil (list ref-lock)))
                    (delete-file (cdr item))
                    (ngit-unlock ref-lock)
                    (ngit-maint--delete-empty-parents
                     (file-name-directory (cdr item)) (expand-file-name "refs" common))
                    (ngit-ref-finish repo input))
                (ngit-unlock ref-lock)))))))
    (aset repo 4 nil)))

(defun ngit-cmd-pack-refs (repo args)
  (let ((all nil) (prune t))
    (dolist (a args)
      (cond ((string= a "--all") (setq all t))
            ((string= a "--prune") (setq prune t))
            ((string= a "--no-prune") (setq prune nil))
            (t (ngit--die-with 129 "error: unsupported pack-refs option %s" a))))
    (unless all
      (ngit--die-with 129 "error: ngit pack-refs currently requires --all"))
    (ngit-maint--pack-refs repo prune)
    0))

;;;; Expiry dates

(defun ngit-maint--expiry (value now)
  "Parse the expiry date VALUE, using NOW for relative forms."
  (cond
   ((member value '("never" "false")) 0)
   ((member value '("all" "now")) (if (string= value "all") ngit-maint--max-time now))
   ((string-match "\\`@?\\([0-9]+\\)\\'" value)
    (string-to-number (match-string 1 value)))
   ((string-match
     "\\`\\([0-9]+\\)\\.\\(seconds?\\|minutes?\\|hours?\\|days?\\|weeks?\\|months?\\|years?\\)\\.ago\\'"
     value)
    (let* ((n (string-to-number (match-string 1 value)))
           (unit (match-string 2 value))
           (seconds (cond ((string-prefix-p "second" unit) 1)
                          ((string-prefix-p "minute" unit) 60)
                          ((string-prefix-p "hour" unit) 3600)
                          ((string-prefix-p "day" unit) 86400)
                          ((string-prefix-p "week" unit) 604800)
                          ((string-prefix-p "month" unit) 2592000)
                          (t 31536000))))
      (- now (* n seconds))))
   (t
    (condition-case nil
        (string-to-number (car (split-string (ngit--parse-date value) " ")))
      (error (ngit--die "fatal: invalid expiry date '%s'" value))))))

;;;; Reflogs

(defun ngit-maint--log-time (line)
  "Return LINE's reflog timestamp, or zero for a malformed line."
  (let ((tab (string-search "\t" line)) (right nil) (left nil))
    (if (not tab)
        0
      (setq right (1- tab))
      (while (and (>= right 0) (/= (aref line right) ?\s))
        (setq right (1- right)))
      (setq left (1- right))
      (while (and (>= left 0) (/= (aref line left) ?\s))
        (setq left (1- left)))
      (if (< left 0) 0 (string-to-number (substring line (1+ left) right))))))

(defun ngit-maint--log-message (line)
  (let ((tab (string-search "\t" line)))
    (if tab (substring line (1+ tab)) "")))

(defun ngit-maint--commit-name (repo hex)
  (and (ngit-hex-p hex 40)
       (not (string= hex ngit-maint--zero))
       (condition-case nil (ngit-peel repo hex 'commit) (error nil))))

(defun ngit-maint--commit-reachable (repo roots)
  "Return a set of commits reachable from ROOTS."
  (let ((pending nil) (seen (make-hash-table :test 'equal)))
    (dolist (hex roots)
      (let ((commit (ngit-maint--commit-name repo hex)))
        (when commit (push commit pending))))
    (while pending
      (let ((hex (pop pending)))
        (unless (gethash hex seen)
          (puthash hex t seen)
          (dolist (parent (ngit-commit-parents repo hex)) (push parent pending)))))
    seen))

(defun ngit-maint--reflog-reachable (repo name)
  (if (string= name "HEAD")
      (let (roots)
        (dolist (ref (ngit-list-refs repo)) (push (nth 1 ref) roots))
        (let ((head (ngit-resolve-ref repo "HEAD"))) (when head (push head roots)))
        (ngit-maint--commit-reachable repo roots))
    (let ((tip (ngit-resolve-ref repo name)))
      (and tip (ngit-maint--commit-reachable repo (list tip))))))

(defun ngit-maint--line-unreachable-p (repo line reachable always)
  (if always
      t
    (let* ((old (and (>= (length line) 40) (substring line 0 40)))
           (new (and (>= (length line) 81) (substring line 41 81)))
           (oc (and old (ngit-maint--commit-name repo old)))
           (nc (and new (ngit-maint--commit-name repo new))))
      (or (and oc (not (gethash oc reachable)))
          (and nc (not (gethash nc reachable)))))))

(defun ngit-maint--rewrite-old (line old)
  (if (>= (length line) 40) (concat old (substring line 40)) line))

(defun ngit-maint--write-log (path lines)
  (let ((lock (ngit-lock path)))
    (unwind-protect
        (progn
          (ngit-commit-lock lock (mapconcat (lambda (line) (concat line "\n")) lines ""))
          (setq lock nil))
      (when lock (ngit-unlock lock)))))

(defun ngit-maint--expire-log (repo name path total unreachable dry verbose rewrite)
  (let* ((lines (split-string (ngit--read-file path) "\n" t))
         (reachable nil) (reachable-ready nil)
         (previous ngit-maint--zero) out changed)
    (dolist (line lines)
      (let* ((stamp (ngit-maint--log-time line))
             (prune (or (< stamp total)
                        (and (< stamp unreachable)
                             (progn
                               (unless reachable-ready
                                 (setq reachable
                                       (ngit-maint--reflog-reachable repo name)
                                       reachable-ready t))
                               (ngit-maint--line-unreachable-p
                                repo line reachable (null reachable)))))))
        (when verbose
          (ngit--emit (cond ((not prune) "keep ") (dry "would prune ") (t "prune "))
                      (ngit-maint--log-message line) "\n"))
        (if prune
            (setq changed t)
          (let ((kept (if rewrite (ngit-maint--rewrite-old line previous) line)))
            (push kept out)
            (when (>= (length kept) 81) (setq previous (substring kept 41 81)))))))
    (setq out (nreverse out))
    (when (and changed (not dry)) (ngit-maint--write-log path out))
    out))

(defun ngit-maint--reflog-defaults (repo name now explicit-total explicit-unreachable
                                          total unreachable)
  (let ((total (or total
                   (and (not (string= name "refs/stash"))
                        (ngit-maint--expiry
                         (or (ngit-config-get repo "gc.reflogexpire") "90.days.ago") now))
                   0))
        (unreachable (or unreachable
                         (and (not (string= name "refs/stash"))
                              (ngit-maint--expiry
                               (or (ngit-config-get repo "gc.reflogexpireunreachable")
                                   "30.days.ago") now))
                         0)))
    (ignore explicit-total explicit-unreachable)
    (cons total unreachable)))

(defun ngit-maint--select-log (repo spec logs)
  (let* ((name (if (member spec '("@" "HEAD")) "HEAD"
                 (let ((ref (ngit-dwim-ref repo spec))) (and ref (car ref)))))
         (name (or name spec)))
    (assoc name logs)))

(defun ngit-maint--reflog-expire (repo args)
  (let ((all nil) (dry nil) (verbose nil) (rewrite nil)
        (total nil) (unreachable nil) (refs nil) (now (floor (float-time))))
    (while args
      (let ((a (pop args)))
        (cond
         ((string= a "--all") (setq all t))
         ((member a '("-n" "--dry-run")) (setq dry t))
         ((string= a "--verbose") (setq verbose t))
         ((string= a "--rewrite") (setq rewrite t))
         ((string-prefix-p "--expire=" a)
          (setq total (ngit-maint--expiry (substring a 9) now)))
         ((string-prefix-p "--expire-unreachable=" a)
          (setq unreachable (ngit-maint--expiry (substring a 21) now)))
         ((member a '("--expire" "--expire-unreachable"))
          (let ((value (or (pop args)
                           (ngit--die-with 129 "error: option '%s' requires a value" a))))
            (if (string= a "--expire")
                (setq total (ngit-maint--expiry value now))
              (setq unreachable (ngit-maint--expiry value now)))))
         ((string-prefix-p "-" a)
          (ngit--die-with 129 "error: unsupported reflog expire option %s" a))
         (t (push a refs)))))
    (let ((logs (ngit-object-log-files repo)) selected)
      (if all
          (setq selected logs)
        (dolist (spec (nreverse refs))
          (let ((log (ngit-maint--select-log repo spec logs)))
            (if log (push log selected)
              (ngit-native-write-stderr (format "error: %s points nowhere!\n" spec)))))
        (setq selected (nreverse selected)))
      (dolist (log selected)
        (let ((limits (ngit-maint--reflog-defaults
                       repo (car log) now (not (null total)) (not (null unreachable))
                       total unreachable)))
          (ngit-maint--expire-log repo (car log) (cdr log) (car limits) (cdr limits)
                                  dry verbose rewrite))))
    0))

(defun ngit-maint--set-ref-tip (repo name hex)
  (let* ((raw (ngit-read-ref repo name))
         (name (if (and raw (string-prefix-p "ref: " raw)) (substring raw 5) name))
         (path (ngit--ref-file repo name)))
    (ngit-make-directory (file-name-directory path) t)
    ;; Reflog expiry updates its ref directly in the files backend.
    (let ((lock (ngit-lock path)))
      (unwind-protect
          (ngit-commit-lock lock (concat hex "\n"))
        (ngit-unlock lock)))
    (aset repo 4 nil)))

(defun ngit-maint--delete-log-spec (repo logs spec dry verbose rewrite update)
  "Delete one reflog SPEC and return zero, or one after a diagnostic."
  (if (not (string-match "\\`\\(.+\\)@{\\([0-9]+\\)}\\'" spec))
      (progn
        (ngit-native-write-stderr (format "error: not a reflog: %s\n" spec))
        1)
    (let* ((base (match-string 1 spec))
           (number (string-to-number (match-string 2 spec)))
           (log (ngit-maint--select-log repo base logs)))
      (if (not log)
          (progn
            (ngit-native-write-stderr (format "error: no reflog for '%s'\n" spec))
            1)
        (let* ((lines (split-string (ngit--read-file (cdr log)) "\n" t))
               (index (- (length lines) 1 number)))
          (if (or (< index 0) (>= index (length lines)))
              (progn
                (ngit-native-write-stderr
                 (format "error: log for '%s' only has %d entries\n"
                         base (length lines)))
                1)
            (let ((i 0) (previous ngit-maint--zero) out)
              (dolist (line lines)
                (if (= i index)
                    (when verbose
                      (ngit--emit (if dry "would prune " "prune ")
                                  (ngit-maint--log-message line) "\n"))
                  (let ((kept (if rewrite (ngit-maint--rewrite-old line previous) line)))
                    (push kept out)
                    (when (>= (length kept) 81)
                      (setq previous (substring kept 41 81)))))
                (setq i (1+ i)))
              (setq out (nreverse out))
              (unless dry
                (ngit-maint--write-log (cdr log) out)
                (when (and update (= index (1- (length lines))) out)
                  (ngit-maint--set-ref-tip
                   repo (car log) (substring (car (last out)) 41 81))))
              0)))))))

(defun ngit-maint--reflog-delete (repo args)
  (let ((dry nil) (verbose nil) (rewrite nil) (update nil) specs)
    (dolist (a args)
      (cond ((member a '("-n" "--dry-run")) (setq dry t))
            ((string= a "--verbose") (setq verbose t))
            ((string= a "--rewrite") (setq rewrite t))
            ((string= a "--updateref") (setq update t))
            ((string-prefix-p "-" a)
             (ngit--die-with 129 "error: unsupported reflog delete option %s" a))
            (t (push a specs))))
    (if (null specs)
        (progn
          (ngit-native-write-stderr "error: no reflog specified to delete\n")
          1)
      (let ((logs (ngit-object-log-files repo)) (status 0))
        (dolist (spec (nreverse specs))
          (setq status
                (logior status (ngit-maint--delete-log-spec
                                repo logs spec dry verbose rewrite update))))
        status))))

(defun ngit-cmd-reflog (repo args)
  (let ((sub (pop args)))
    (cond ((string= sub "expire") (ngit-maint--reflog-expire repo args))
          ((string= sub "delete") (ngit-maint--reflog-delete repo args))
          (t (ngit--die-with 129 "error: unsupported reflog subcommand %s" (or sub ""))))))

;;;; Prune

(defun ngit-maint--prune (repo cutoff dry verbose &optional extra-roots reachable)
  (unless dry (ngit-maint--invalidate-commit-graphs repo))
  (let ((loose (ngit-pack-loose-oids repo)))
    ;; A recent unreachable object protects the objects reachable from it.
    (unless reachable
      (let ((roots (append extra-roots (ngit-pack-roots repo t t))) recent)
        (dolist (hex loose)
          (let ((stat (ngit-maint--stat (ngit-maint--loose-path repo hex))))
            (when (and stat (> (aref stat 3) cutoff)) (push hex recent))))
        (setq reachable (ngit-pack-reachable repo (append roots (nreverse recent))))))
    (let* ((cached (and (consp ngit-object--closure)
                        (eq reachable (car ngit-object--closure))
                        (cdr ngit-object--closure)))
           (seen (or cached (make-hash-table :test 'equal))))
      ;; The closure is immutable and belongs to this exact list.  gc has
      ;; already walked it; copying every name again can dominate pruning.
      (unless cached (dolist (hex reachable) (puthash hex t seen)))
      (dolist (hex loose)
        (let* ((path (ngit-maint--loose-path repo hex))
               (stat (ngit-maint--stat path)))
          (when (and stat (<= (aref stat 3) cutoff) (not (gethash hex seen))
                     (not (ngit--find-packed repo (ngit-unhex hex))))
            (when (or dry verbose)
              (let ((type (condition-case nil
                              (symbol-name (car (ngit-read-object repo (ngit-unhex hex))))
                            (error "unknown"))))
                (ngit--emit hex " " type "\n")))
            (unless dry (ngit-maint--delete-loose repo hex))))))
    (ngit-maint--prune-packed repo dry)
    (unless dry (ngit-forget-loose-names))))

(defun ngit-cmd-prune (repo args)
  (let ((dry nil) (verbose nil) (cutoff ngit-maint--max-time)
        (now (floor (float-time))) roots)
    (while args
      (let ((a (pop args)))
        (cond ((member a '("-n" "--dry-run")) (setq dry t))
              ((member a '("-v" "--verbose")) (setq verbose t))
              ((string-prefix-p "--expire=" a)
               (setq cutoff (ngit-maint--expiry (substring a 9) now)))
              ((string= a "--expire")
               (setq cutoff (ngit-maint--expiry
                             (or (pop args)
                                 (ngit--die-with 129 "error: option 'expire' requires a value"))
                             now)))
              ((member a '("--progress" "--no-progress")) nil)
              ((string-prefix-p "-" a)
               (ngit--die-with 129 "error: unsupported prune option %s" a))
              (t (push (ngit-rev-parse repo a) roots)))))
    (let ((ngit-maint--gc-reachable nil))
      (ngit-maint--prune repo cutoff dry verbose (nreverse roots)))
    0))

;;;; count-objects

(defun ngit-maint--pack-garbage (repo)
  "Return (COUNT . SIZE) of loose and pack-directory garbage."
  (let* ((objects (ngit--objects-dir repo))
         (dir (expand-file-name "pack" objects))
         (bases (make-hash-table :test 'equal)) files count size)
    (dolist (sub (directory-files objects nil nil t))
      (let ((subdir (concat objects sub)))
        (when (and (ngit-hex-p sub 2) (file-directory-p subdir))
          (dolist (name (directory-files subdir nil nil t))
            (unless (member name '("." ".."))
              (let* ((path (expand-file-name name subdir))
                     (stat (ngit-maint--stat path)))
                (unless (and (ngit-hex-p name 38) stat
                             (ngit-maint--regular-p stat))
                  (setq count (1+ (or count 0))
                        size (+ (or size 0) (if stat (aref stat 1) 0))))))))))
    (when (file-directory-p dir)
      (dolist (path (directory-files dir t nil t))
        (unless (member (file-name-nondirectory path) '("." ".."))
          (push path files))
        (let ((ext (file-name-extension path t)))
          (when (member ext '(".pack" ".idx"))
            (let ((base (substring path 0 (- (length ext)))))
              (puthash base (cons ext (gethash base bases)) bases)))))
      (dolist (path files)
        (let* ((ext (file-name-extension path t))
               (base (if ext (substring path 0 (- (length ext))) path))
               (both (gethash base bases))
               (name (file-name-nondirectory path))
               (paired (and (member ".pack" both) (member ".idx" both))))
          (unless (or (and paired (member ext '(".pack" ".idx" ".rev" ".bitmap"
                                                ".mtimes" ".keep" ".promisor")))
                      (string= name "multi-pack-index")
                      (string= name "packs"))
            (let ((stat (ngit-maint--stat path)))
              (setq count (1+ (or count 0))
                    size (+ (or size 0) (if stat (aref stat 1) 0))))))))
    (cons (or count 0) (or size 0))))

(defun ngit-cmd-count-objects (repo args)
  (let ((verbose nil))
    (dolist (a args)
      (cond ((or (string= a "-v") (string= a "--verbose")) (setq verbose t))
            (t (ngit--die-with 129 "error: unsupported count-objects option %s" a))))
    (let ((loose 0) (loose-size 0) (packable 0) (packed 0) (pack-size 0)
          (packs (ngit-repo-packs repo)))
      (dolist (hex (ngit-pack-loose-oids repo))
        (let ((stat (ngit-maint--stat (ngit-maint--loose-path repo hex))))
          (when (and stat (ngit-maint--regular-p stat))
            (setq loose (1+ loose)
                  loose-size (+ loose-size (* 512 (aref stat 2))))
            (when (ngit--find-packed repo (ngit-unhex hex))
              (setq packable (1+ packable))))))
      (dolist (pack packs)
        (let ((pack-stat (ngit-maint--stat (aref pack 0)))
              (idx-stat (ngit-maint--stat (aref pack 1))))
          (setq packed (+ packed (aref pack 2))
                pack-size (+ pack-size (if pack-stat (aref pack-stat 1) 0)
                             (if idx-stat (aref idx-stat 1) 0)))))
      (if verbose
          (let ((garbage (ngit-maint--pack-garbage repo)))
            (ngit--emit "count: " (number-to-string loose) "\n"
                        "size: " (number-to-string (/ loose-size 1024)) "\n"
                        "in-pack: " (number-to-string packed) "\n"
                        "packs: " (number-to-string (length packs)) "\n"
                        "size-pack: " (number-to-string (/ pack-size 1024)) "\n"
                        "prune-packable: " (number-to-string packable) "\n"
                        "garbage: " (number-to-string (car garbage)) "\n"
                        "size-garbage: " (number-to-string (/ (cdr garbage) 1024)) "\n"))
        (ngit--emit (number-to-string loose) " objects, "
                    (number-to-string (/ loose-size 1024)) " kilobytes\n")))
    0))

;;;; gc

(defun ngit-maint--auto-pack-refs-p (repo)
  "Apply the files backend's packed-size versus loose-ref threshold."
  (let* ((path (expand-file-name "packed-refs" (ngit-repo-commondir repo)))
         (size (if (file-exists-p path) (ngit-native-file-size path) 0))
         (number (/ size 100)) (power 0) (count 0))
    (while (> number 1) (setq number (ash number -1) power (1+ power)))
    (dolist (ref (ngit-list-refs repo))
      (when (and (null (nth 2 ref)) (ngit-maint--packable-ref-p (car ref))
                 (file-regular-p (ngit--ref-file repo (car ref))))
        (setq count (1+ count))))
    (>= count (max 16 (* 5 power)))))

(defun ngit-maint--auto-needed (repo)
  "Return `packs', `loose' or nil using git gc's automatic thresholds."
  (let* ((auto (ngit-config-int repo "gc.auto" 6700))
         (pack-limit (ngit-config-int repo "gc.autopacklimit" 50)))
    (when (> auto 0)
      (cond
       ((and (> pack-limit 0) (> (length (ngit-repo-packs repo)) pack-limit)) 'packs)
       (t
        (let* ((dir (expand-file-name "objects/17" (ngit-repo-commondir repo)))
               (limit (/ (+ auto 255) 256)) (count 0))
          (when (file-directory-p dir)
            (dolist (name (directory-files dir nil nil t))
              (when (ngit-hex-p name 38) (setq count (1+ count)))))
          (and (> count limit) 'loose)))))))

(defun ngit-cmd-gc (repo args)
  (let ((auto nil) (quiet nil) (prune nil) (now (floor (float-time))))
    (dolist (a args)
      (cond ((string= a "--auto") (setq auto t))
            ((member a '("-q" "--quiet")) (setq quiet t))
            ((string= a "--prune") (setq prune "2.weeks.ago"))
            ((string-prefix-p "--prune=" a) (setq prune (substring a 8)))
            (t (ngit--die-with 129 "error: unsupported gc option %s" a))))
    (let ((need (and auto (ngit-maint--auto-needed repo))))
      (unless (and auto (or (null need)
                            (/= 0 (ngit-hook-run repo "pre-auto-gc"))))
        (when (and auto (not quiet))
          (ngit-native-write-stderr
           (concat "Auto packing the repository "
                   (if (ngit-config-bool repo "gc.autodetach" t) "in background " "")
                   "for optimum performance.\nSee \"git help gc\" for manual housekeeping.\n")))
        (setq prune (or prune (ngit-config-get repo "gc.pruneexpire") "2.weeks.ago"))
        (let ((pack-refs (ngit-config-get repo "gc.packrefs")))
          (when (and (or (not auto) (ngit-maint--auto-pack-refs-p repo))
                     (cond ((null pack-refs) t)
                           ((equal pack-refs "notbare") (ngit-repo-toplevel repo))
                           (t (ngit-config-bool repo "gc.packrefs" t))))
            (ngit-maint--pack-refs repo t))
          (ngit-maint--reflog-expire repo '("--all"))
          (let* ((cutoff (ngit-maint--expiry prune now))
                 (incremental (and auto (eq need 'loose)))
                 (protected (if incremental (ngit-maint--reachable repo t)
                              (ngit-maint--protected-reachable repo cutoff)))
                 (ngit-maint--gc-reachable
                  (if (and (not incremental) (< cutoff now))
                      (ngit-maint--include-packed repo protected cutoff)
                    protected))
                 (ngit-maint--record-mtimes (and (not incremental) (< cutoff now) now))
                 (all (or (not auto) (eq need 'packs))))
            (ngit-maint--repack repo all t quiet ngit-maint--gc-reachable)
            (ngit-maint--prune repo cutoff nil nil nil ngit-maint--gc-reachable))
          (when (and auto (eq (ngit-maint--auto-needed repo) 'loose))
            (ngit-native-write-stderr
             "warning: There are too many unreachable loose objects; run 'git prune' to remove them.\n")))))
    0))

(provide 'ngit-maint)

;;; ngit-maint.el ends here
