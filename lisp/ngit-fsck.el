;;; ngit-fsck.el --- object database consistency checks -*- lexical-binding: t; -*-

;;; Commentary:

;; Verify object identity, graph connectivity, and reachability without
;; invoking git.  The report order follows git's parsed-object hash table,
;; whose bucket is the little-endian first word of the object name.

;;; Code:

(require 'ngit-objects)

(defconst ngit-fsck--error-object 1)
(defconst ngit-fsck--error-reachable 2)
(defconst ngit-fsck--error-pack 4)
(defconst ngit-fsck--error-refs 8)
(defvar ngit-fsck--bad-commits nil)
(defvar ngit-fsck--unreadable nil)

(defun ngit-fsck--bad-commit (hex)
  "Repeat the commit parser's typed-pointer errors for HEX."
  (dolist (bad (and ngit-fsck--bad-commits (gethash hex ngit-fsck--bad-commits)))
    (ngit-fsck--stderr "error: object %s is a %s, not a %s"
                       (nth 1 bad) (nth 2 bad) (nth 0 bad))
    (ngit-fsck--stderr "error: bad %s pointer %s in commit %s"
                       (if (eq (nth 0 bad) 'tree) "tree" "parent") (nth 1 bad) hex)))

(defun ngit-fsck--stderr (fmt &rest args)
  (ngit-native-write-stderr (concat (apply #'format fmt args) "\n")))

(defun ngit-fsck--loose-path (repo hex)
  (concat (ngit--objects-dir repo) (substring hex 0 2) "/" (substring hex 2)))

(defun ngit-fsck--display-path (path)
  "Return PATH in the form git uses from the command's working directory."
  (let ((relative (file-relative-name path default-directory)))
    (if (string-prefix-p "../" relative) path relative)))

(defun ngit-fsck--object-id (obj)
  (let ((type (symbol-name (car obj)))
        (data (cdr obj)))
    (secure-hash 'sha1
                 (concat type " " (number-to-string (length data)) "\0" data))))

(defconst ngit-fsck--crc32-table
  (let ((table (make-vector 256 0)))
    (dotimes (i 256)
      (let ((value i))
        (dotimes (_ 8)
          (setq value (if (/= 0 (logand value 1))
                          (logxor #xedb88320 (ash value -1))
                        (ash value -1))))
        (aset table i (logand value #xffffffff))))
    table))

(defun ngit-fsck--crc32 (bytes)
  "Return the unsigned zlib CRC-32 of BYTES."
  (let ((crc #xffffffff))
    (dotimes (i (length bytes))
      (setq crc
            (logxor (aref ngit-fsck--crc32-table
                          (logand (logxor crc (aref bytes i)) 255))
                    (ash crc -8))))
    (logand (logxor crc #xffffffff) #xffffffff)))

(defun ngit-fsck--pack-records (pack)
  "Return PACK index records [HEX CRC OFFSET], sorted by pack offset."
  (let* ((count (aref pack 2))
         (idx (ngit--read-file (aref pack 1)))
         (names-at 1032)
         (crc-at (+ names-at (* count 20)))
         (offset-at (+ crc-at (* count 4))) out)
    (dotimes (i count)
      (let* ((raw-off (ngit--be32 idx (+ offset-at (* i 4))))
             (offset
              (if (= 0 (logand raw-off #x80000000))
                  raw-off
                (let ((large-at (+ offset-at (* count 4)
                                   (* 8 (logand raw-off #x7fffffff)))))
                  (logior (ash (ngit--be32 idx large-at) 32)
                          (ngit--be32 idx (+ large-at 4)))))))
        (push (vector (ngit-hex
                       (substring idx (+ names-at (* i 20))
                                  (+ names-at (* (1+ i) 20))))
                      (ngit--be32 idx (+ crc-at (* i 4))) offset)
              out)))
    (sort out (lambda (a b) (< (aref a 2) (aref b 2))))))

(defun ngit-fsck--validate-packs (repo)
  "Validate pack checksums and entry CRCs in REPO; return fsck status bits."
  (let ((status 0))
    (dolist (pack (ngit-repo-packs repo))
      (let* ((path (aref pack 0))
             (shown (ngit-fsck--display-path path))
             (idx-raw (ngit--read-file (aref pack 1)))
             (idx-end (- (length idx-raw) 20))
             (raw (ngit--read-file path))
             (end (- (length raw) 20))
             (records (ngit-fsck--pack-records pack)))
        (unless (and (>= idx-end 0)
                     (string= (substring idx-raw idx-end)
                              (ngit-unhex
                               (secure-hash 'sha1
                                            (substring idx-raw 0 idx-end)))))
          (ngit-fsck--stderr "error: Packfile index for %s hash mismatch" shown)
          (setq status (logior status ngit-fsck--error-pack)))
        (unless (and (>= end 12)
                     (string= (substring raw end)
                              (ngit-unhex (secure-hash 'sha1 (substring raw 0 end)))))
          (ngit-fsck--stderr "error: %s pack checksum mismatch" shown)
          (setq status (logior status ngit-fsck--error-pack)))
        (let ((rest records))
          (while rest
            (let* ((record (car rest))
                   (offset (aref record 2))
                   (next (if (cdr rest) (aref (cadr rest) 2) end))
                   (actual (and (<= offset next) (<= next end)
                                (ngit-fsck--crc32 (substring raw offset next)))))
              (unless (and actual (= actual (aref record 1)))
                (ngit-fsck--stderr
                 "error: index CRC mismatch for object %s from %s at offset %d"
                 (aref record 0) shown offset)
                (setq status (logior status ngit-fsck--error-pack))
                (condition-case nil
                    (progn
                      (remhash offset (aref pack 5))
                      (ngit--pack-entry repo pack offset))
                  (error
                   (ngit-fsck--stderr
                    "error: inflate: data stream error (incorrect header check)")
                   (ngit-fsck--stderr "error: cannot unpack %s from %s at offset %d"
                                      (aref record 0) shown offset))))
              (setq rest (cdr rest)))))))
    status))

(defun ngit-fsck--record-type (types hex type)
  "Record TYPE for HEX unless a concrete type is already known."
  (let ((old (gethash hex types)))
    (when (or (null old) (eq old 'unknown))
      (puthash hex (or type 'unknown) types))))

(defun ngit-fsck--add-oid (hex seen universe)
  "Add HEX to UNIVERSE unless already in SEEN, returning the new list."
  (if (gethash hex seen)
      universe
    (puthash hex t seen)
    (cons hex universe)))

(defun ngit-fsck--cache-tree-roots (repo good types used)
  "Check REPO's index cache-tree and return (STATUS . ROOTS).
Git stops below a cache-tree node whose object cannot be read, so descendants
of such a node are deliberately not retained as roots."
  (let ((table (ngit-index-cache-tree (ngit-read-index repo))) paths blocked roots
        (status 0))
    (when table
      (maphash (lambda (path _node) (push path paths)) table)
      (setq paths
            (sort paths
                  (lambda (a b)
                    (or (< (length a) (length b))
                        (and (= (length a) (length b)) (string< a b))))))
      (dolist (path paths)
        (let ((skip nil))
          (dolist (prefix blocked)
            (when (string-prefix-p prefix path) (setq skip t)))
          (unless skip
            (let* ((node (gethash path table)) (bin (cdr node)))
              (when (and bin (= (length bin) 20))
                (let ((hex (ngit-hex bin)))
                  (ngit-fsck--record-type types hex 'tree)
                  (if (gethash hex good)
                      (progn (puthash hex t used) (push hex roots))
                    (ngit-fsck--stderr
                     "error: %s: invalid sha1 pointer in cache-tree of %s"
                     hex (ngit-fsck--display-path (ngit-index-file repo)))
                    (setq status (logior status ngit-fsck--error-refs))
                    (push path blocked)))))))))
    (cons status (nreverse roots))))

(defun ngit-fsck--index-roots (repo types used)
  "Return index entry object names, including missing ones."
  (let* ((index (ngit-read-index repo)) (entries (aref index 0)) roots)
    (dotimes (i (length entries))
      (let ((entry (aref entries i)))
        (unless (= (aref entry 2) #o160000)
          (let ((hex (ngit-hex (aref entry 1))))
            (ngit-fsck--record-type types hex 'blob)
            (puthash hex t used)
            (push hex roots)))))
    (dolist (entry (aref index 9))
      (unless (= (aref entry 2) #o160000)
        (let ((hex (ngit-hex (aref entry 1))))
          (ngit-fsck--record-type types hex 'blob)
          (puthash hex t used)
          (push hex roots))))
    (nreverse roots)))

(defun ngit-fsck--reflog-roots (repo good)
  "Check REPO's reflogs and return (STATUS . ROOTS)."
  (let ((status 0) roots)
    (dolist (log (ngit-object-log-files repo))
      (dolist (line (split-string (ngit--read-file (cdr log)) "\n" t))
        (when (>= (length line) 81)
          (dolist (hex (list (substring line 0 40) (substring line 41 81)))
            (unless (string= hex "0000000000000000000000000000000000000000")
              (if (gethash hex good)
                  (push hex roots)
                (ngit-fsck--stderr "error: %s: invalid reflog entry %s"
                                   (car log) hex)
                (setq status (logior status ngit-fsck--error-reachable))))))))
    (cons status (nreverse roots))))

(defun ngit-fsck--ref-roots (repo good)
  "Check REPO's refs and HEAD and return (STATUS COUNT ROOTS)."
  (let ((status 0) (count 0) roots)
    (dolist (ref (ngit-list-refs repo))
      (let ((name (nth 0 ref)) (hex (nth 1 ref)))
        (if (gethash hex good)
            (progn (setq count (1+ count)) (push hex roots))
          (ngit-fsck--bad-commit hex)
          (ngit-fsck--stderr "error: %s: invalid sha1 pointer %s" name hex)
          (setq status (logior status ngit-fsck--error-reachable)))))
    (let ((hex (ngit-resolve-ref repo "HEAD")))
      (when hex
        (if (gethash hex good)
            (progn (setq count (1+ count)) (push hex roots))
          (ngit-fsck--bad-commit hex)
          (ngit-fsck--stderr "error: HEAD: invalid sha1 pointer %s" hex)
          (setq status (logior status ngit-fsck--error-reachable)))))
    (list status count (nreverse roots))))

(defun ngit-fsck--hash-word (hex)
  "Return git's little-endian object-table hash word for HEX."
  (+ (string-to-number (substring hex 0 2) 16)
     (ash (string-to-number (substring hex 2 4) 16) 8)
     (ash (string-to-number (substring hex 4 6) 16) 16)
     (ash (string-to-number (substring hex 6 8) 16) 24)))

(defun ngit-fsck--table-place (slots hex)
  "Insert HEX into SLOTS by git's linear probing rule."
  (let* ((size (length slots))
         (pos (logand (ngit-fsck--hash-word hex) (1- size))))
    (while (aref slots pos)
      (setq pos (mod (1+ pos) size)))
    (aset slots pos hex)))

(defun ngit-fsck--table-grow (slots)
  "Grow Git object table SLOTS, reinserting old slots in address order."
  (let ((larger (make-vector (if (< (length slots) 32) 32
                               (* 2 (length slots))) nil)))
    (dotimes (i (length slots))
      (when (aref slots i)
        (ngit-fsck--table-place larger (aref slots i))))
    larger))

(defun ngit-fsck--table-lookup (state hex)
  "Look up or create HEX in Git object-table STATE.
Successful colliding lookups swap the found slot with the initial bucket."
  (let* ((slots (aref state 0))
         (size (length slots))
         (first (and (> size 0)
                     (logand (ngit-fsck--hash-word hex) (1- size))))
         (pos first))
    (while (and pos (aref slots pos) (not (string= (aref slots pos) hex)))
      (setq pos (mod (1+ pos) size)))
    (if (and pos (aref slots pos))
        (unless (= pos first)
          (let ((at-first (aref slots first)))
            (aset slots first hex)
            (aset slots pos at-first)))
      ;; create_object grows after its unsuccessful lookup and before insert.
      (when (or (= size 0) (<= (1- size) (* 2 (aref state 1))))
        (setq slots (ngit-fsck--table-grow slots))
        (aset state 0 slots))
      (ngit-fsck--table-place slots hex)
      (aset state 1 (1+ (aref state 1))))))

(defun ngit-fsck--loose-scan-order (repo wanted)
  "Return loose WANTED names in Git's directory traversal order."
  (let ((objects (ngit--objects-dir repo))
        (wanted-set (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal)) out)
    (dolist (hex wanted) (puthash hex t wanted-set))
    (dotimes (i 256)
      (let* ((sub (format "%02x" i)) (dir (concat objects sub)))
        (when (file-directory-p dir)
          ;; readdir order within one fanout directory is intentionally raw.
          (dolist (name (directory-files dir nil nil t))
            (let ((hex (concat sub name)))
              (when (and (ngit-hex-p name 38) (gethash hex wanted-set)
                         (not (gethash hex seen)))
                (puthash hex t seen)
                (push hex out)))))))
    (cons (nreverse out) seen)))

(defun ngit-fsck--scan-events (repo oids connectivity-only)
  "Return (EVENTS . SEEN) in Git's loose-then-packed scan order.
EVENTS retains duplicate names found in both storage forms because their
lookups can swap colliding object-table slots."
  (let* ((loose (ngit-fsck--loose-scan-order repo oids))
         (events (reverse (car loose)))
         (seen (cdr loose)))
    (dolist (pack (ngit-repo-packs repo))
      (let ((records (ngit-fsck--pack-records pack)))
        (when connectivity-only
          ;; for_each_packed_object follows the pack index's name order.
          (setq records
                (sort records
                      (lambda (a b) (string< (aref a 0) (aref b 0))))))
        (dolist (record records)
          (let ((hex (aref record 0)))
            (push hex events)
            (puthash hex t seen)))))
    ;; Missing explicit roots have no storage event but still get an object
    ;; table entry when refs or command-line arguments look them up.
    (dolist (hex oids)
      (unless (gethash hex seen)
        (push hex events)
        (puthash hex t seen)))
    (cons (nreverse events) seen)))

(defun ngit-fsck--ordered (repo oids links connectivity-only reachable)
  "Order OIDS by simulating Git's parsed-object hash table."
  (let* ((scan-info (ngit-fsck--scan-events repo oids connectivity-only))
         (scan (car scan-info))
         (state (vector [] 0)))
    (dolist (hex scan)
      (ngit-fsck--table-lookup state hex)
      ;; Full fsck resolves typed links while scanning each object.
      (unless connectivity-only
        (dolist (edge (gethash hex links))
          (ngit-fsck--table-lookup state (cdr edge)))))
    (when connectivity-only
      ;; Reachability parses root-side links after the name-only object scan.
      (dolist (hex scan)
        (when (gethash hex reachable)
          (dolist (edge (gethash hex links))
            (ngit-fsck--table-lookup state (cdr edge)))))
      ;; Dangling detection performs a second loose scan.  Its lookup swaps
      ;; happen even for reachable objects; only unreachable links walk.
      (dolist (hex scan)
        (ngit-fsck--table-lookup state hex)
        (unless (gethash hex reachable)
          (dolist (edge (gethash hex links))
            (ngit-fsck--table-lookup state (cdr edge))))))
    (let (out)
      (dotimes (i (length (aref state 0)))
        (when (aref (aref state 0) i)
          (push (aref (aref state 0) i) out)))
      (nreverse out))))

(defun ngit-fsck--read-error (repo hex)
  "Report a corrupt loose object HEX in REPO using git's usual wording."
  (let* ((path (ngit-fsck--loose-path repo hex))
         (shown (ngit-fsck--display-path path)))
    (if (file-exists-p path)
        (progn
          (if (= (file-attribute-size (file-attributes path)) 0)
              (progn
                (ngit-fsck--stderr "error: object file %s is empty" shown)
                (ngit-fsck--stderr
                 "error: unable to mmap %s: No such file or directory" shown))
            (ngit-fsck--stderr
             "error: inflate: data stream error (incorrect header check)")
            (ngit-fsck--stderr "error: unable to unpack header of %s" shown))
          (ngit-fsck--stderr "error: %s: object corrupt or missing: %s" hex shown))
      (ngit-fsck--stderr "error: %s: object corrupt or missing" hex))))

(defun ngit-fsck--scan-object
    (repo hex connectivity-only good types links used)
  "Read and classify HEX, returning nonzero when full verification fails."
  (condition-case _err
      (let* ((obj (ngit-read-object repo (ngit-unhex hex)))
             (type (car obj))
             (actual (and (not connectivity-only) (ngit-fsck--object-id obj))))
        (ngit-fsck--record-type types hex type)
        (cond
         ((and actual (not (string= actual hex)))
          (let ((path (ngit-fsck--loose-path repo hex)))
            (if (file-exists-p path)
                (ngit-fsck--stderr
                 "error: %s: hash-path mismatch, found at: %s"
                 actual (ngit-fsck--display-path path))
              (ngit-fsck--stderr "error: %s: object corrupt or missing" hex)))
          ngit-fsck--error-object)
         (t
          (puthash hex t good)
          (condition-case _parse-error
              (let ((edges (ngit-object-links obj)) bad)
                (when (eq type 'commit)
                  (dolist (edge edges)
                    (let ((actual-type
                           (condition-case nil
                               (car (ngit-read-object repo (ngit-unhex (cdr edge))))
                             (error nil))))
                      (when (and actual-type (not (eq actual-type (car edge))))
                        (if connectivity-only
                            (ngit-fsck--stderr "error: Object %s not a %s"
                                               (cdr edge) (car edge))
                          (push (list (car edge) (cdr edge) actual-type) bad))))))
                (if bad
                    (progn
                      ;; Pointer lookups already happened before commit
                      ;; validation rejected the object.
                      (puthash hex edges links)
                      (puthash hex (nreverse bad) ngit-fsck--bad-commits)
                      (ngit-fsck--bad-commit hex)
                      (ngit-fsck--stderr "error: %s: object could not be parsed: %s"
                                         hex (ngit-fsck--display-path
                                              (ngit-fsck--loose-path repo hex)))
                      (remhash hex good))
                  (puthash hex edges links)
                  (dolist (edge edges)
                    (ngit-fsck--record-type types (cdr edge) (car edge))
                    (puthash (cdr edge) t used))))
            (error
             (remhash hex good)
             (if connectivity-only
                 nil
               (ngit-fsck--stderr "error: %s: object could not be parsed" hex))))
          (if (gethash hex good) 0 ngit-fsck--error-object))))
    (error
     (if connectivity-only
         ;; Blob contents are immaterial, but a reachable commit or tree
         ;; must still parse so its outgoing links can be checked.
         (progn (puthash hex t ngit-fsck--unreadable) (puthash hex t good) 0)
       (ngit-fsck--read-error repo hex)
       ngit-fsck--error-object))))

(defun ngit-fsck--walk (roots links good reachable)
  "Mark the closure of ROOTS through LINKS in REACHABLE.
Only GOOD objects are expanded; absent and corrupt targets remain marked so
the reporting pass can describe them as missing."
  (let ((pending (copy-sequence roots)))
    (while pending
      (let ((hex (pop pending)))
        (unless (gethash hex reachable)
          (puthash hex t reachable)
          (when (gethash hex good)
            (dolist (edge (gethash hex links))
              (push (cdr edge) pending))))))))

(defun ngit-fsck--broken-links (links good reachable index-roots types)
  "Report missing edges from reachable objects.
INDEX-ROOTS suppress an edge report because git marks index blobs reachable
before traversing trees."
  (let ((index-set (make-hash-table :test 'equal))
        (best (make-hash-table :test 'equal)) (broken nil))
    (dolist (hex index-roots) (puthash hex t index-set))
    (maphash
     (lambda (from edges)
       (when (and (gethash from reachable) (gethash from good))
         (dolist (edge edges)
           (let ((to (cdr edge)))
             (when (and (not (gethash to good))
                        (not (gethash to index-set)))
               ;; Git marks an absent object reachable at the first edge, so
               ;; only that edge is printed.  Its LIFO walk reaches the parent
               ;; with the greatest object-hash word first for normal roots.
               (let ((old (gethash to best)))
                 (when (or (null old)
                           (> (ngit-fsck--hash-word from)
                              (ngit-fsck--hash-word (car old))))
                   (puthash to (list from to (car edge)) best))))))))
     links)
    (maphash (lambda (_to item) (push item broken)) best)
    (setq broken
          (sort broken
                (lambda (a b)
                  (< (ngit-fsck--hash-word (car a))
                     (ngit-fsck--hash-word (car b))))))
    (dolist (item broken)
      (let ((from (nth 0 item)) (to (nth 1 item)) (to-type (nth 2 item)))
        (ngit--emit (format "broken link from %7s %s\n              to %7s %s\n"
                            (symbol-name (or (gethash from types) 'unknown))
                            from (symbol-name (or to-type 'unknown)) to))))
    (and broken ngit-fsck--error-reachable)))

(defun ngit-fsck--explicit-roots (repo names)
  "Resolve command-line object NAMES in REPO."
  (let (roots)
    (dolist (name names)
      (condition-case nil
          (push (ngit-rev-parse repo name) roots)
        (error
         (ngit-fsck--stderr "error: invalid parameter: expected sha1, got '%s'" name))))
    (nreverse roots)))

(defun ngit-fsck--parse-options (args)
  "Parse the supported fsck ARGS into a seven-slot vector."
  (let ((unreachable nil) (dangling t) (connectivity-only nil)
        (reflogs t) (cache nil) (explicit nil) (options t))
    (dolist (arg args)
      (cond
       ((and options (string= arg "--")) (setq options nil))
       ((and options (string= arg "--unreachable")) (setq unreachable t))
       ((and options (string= arg "--no-unreachable")) (setq unreachable nil))
       ((and options (string= arg "--dangling")) (setq dangling t))
       ((and options (string= arg "--no-dangling")) (setq dangling nil))
       ((and options (string= arg "--connectivity-only")) (setq connectivity-only t))
       ((and options (string= arg "--no-connectivity-only")) (setq connectivity-only nil))
       ((and options (string= arg "--reflogs")) (setq reflogs t))
       ((and options (string= arg "--no-reflogs")) (setq reflogs nil))
       ((and options (string= arg "--cache")) (setq cache t))
       ((and options (string= arg "--no-cache")) (setq cache nil))
       ((and options (member arg '("--no-progress" "--progress" "--full"
                                    "--no-full" "--strict"))) nil)
       ((and options (string-prefix-p "-" arg))
        (ngit--die-with 129 "error: unknown option `%s'" arg))
       (t (push arg explicit))))
    (vector unreachable dangling connectivity-only reflogs cache
            (nreverse explicit) (null explicit))))

(defun ngit-cmd-fsck (repo args)
  "Check REPO's object database using git-fsck-compatible ARGS."
  (let* ((opts (ngit-fsck--parse-options args))
         (show-unreachable (aref opts 0))
         (show-dangling (aref opts 1))
         (connectivity-only (aref opts 2))
         (include-reflogs (aref opts 3))
         (include-index (or (aref opts 4) (aref opts 6)))
         (default-heads (aref opts 6))
         (explicit (aref opts 5))
         (all (ngit-pack-all-oids repo))
         (good (make-hash-table :test 'equal))
         (types (make-hash-table :test 'equal))
         (links (make-hash-table :test 'equal))
         (used (make-hash-table :test 'equal))
         (reachable (make-hash-table :test 'equal))
         (universe-seen (make-hash-table :test 'equal))
         (universe nil)
         (index-roots nil)
         (status 0))
    (let ((ngit-fsck--bad-commits (make-hash-table :test 'equal))
          (ngit-fsck--unreadable (make-hash-table :test 'equal)))
    (dolist (hex all)
      (setq universe (ngit-fsck--add-oid hex universe-seen universe))
      (setq status
            (logior status
                    (ngit-fsck--scan-object
                     repo hex connectivity-only good types links used))))
    (unless connectivity-only
      (setq status (logior status (ngit-fsck--validate-packs repo))))
    (let ((roots nil) (default-count 0))
      (if default-heads
          (let ((refs (ngit-fsck--ref-roots repo good)))
            (setq status (logior status (nth 0 refs))
                  default-count (nth 1 refs)
                  roots (nth 2 refs))
            (when include-reflogs
              (let ((logs (ngit-fsck--reflog-roots repo good)))
                (setq status (logior status (car logs))
                      roots (append roots (cdr logs))))))
        (setq roots (ngit-fsck--explicit-roots repo explicit)))
      (when include-index
        (setq index-roots (ngit-fsck--index-roots repo types used)
              roots (append roots index-roots))
        (let ((cache (ngit-fsck--cache-tree-roots repo good types used)))
          (setq status (logior status (car cache))
                roots (append roots (cdr cache)))))
      (when (and default-heads (= default-count 0))
        (let ((head (ngit-read-ref repo "HEAD")))
          (when (and head (string-prefix-p "ref: refs/heads/" head)
                     (null (ngit-resolve-ref repo "HEAD")))
            (ngit-fsck--stderr "notice: HEAD points to an unborn branch (%s)"
                               (substring head 16))))
        (ngit-fsck--stderr "notice: No default references")
        (setq show-unreachable nil))
      (dolist (hex roots)
        (puthash hex t used)
        (setq universe (ngit-fsck--add-oid hex universe-seen universe)))
      ;; Edges discovered while scanning may name objects absent from storage.
      (maphash
       (lambda (_from edges)
         (dolist (edge edges)
           (setq universe
                 (ngit-fsck--add-oid (cdr edge) universe-seen universe))))
       links)
      (ngit-fsck--walk roots links good reachable)
      (when connectivity-only
        (dolist (hex all)
          (when (and (gethash hex ngit-fsck--unreadable)
                     (gethash hex reachable) (not (eq (gethash hex types) 'blob)))
            (ngit-fsck--stderr "error: inflate: data stream error (incorrect header check)")
            (ngit-fsck--stderr "error: unable to unpack %s header" hex)
            (ngit--die "fatal: loose object %s (stored in %s) is corrupt"
                       hex (ngit-fsck--display-path (ngit-fsck--loose-path repo hex))))))
      (setq status
            (logior status
                    (or (ngit-fsck--broken-links
                         links good reachable index-roots types)
                        0))))
    (dolist (hex (ngit-fsck--ordered
                  repo universe links connectivity-only reachable))
      (let ((type (or (gethash hex types) 'unknown)))
        (cond
         ((and (gethash hex reachable) (not (gethash hex good)))
          (ngit--emit "missing " (symbol-name type) " " hex "\n")
          (setq status (logior status ngit-fsck--error-reachable)))
         ((and (gethash hex good) (not (gethash hex reachable)))
          (cond
           (show-unreachable
            (ngit--emit "unreachable " (symbol-name type) " " hex "\n"))
           ((and show-dangling (not (gethash hex used)))
            (ngit--emit "dangling " (symbol-name type) " " hex "\n")))))))
    status)))

(provide 'ngit-fsck)

;;; ngit-fsck.el ends here
