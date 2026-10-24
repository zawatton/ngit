;;; ngit-objects.el --- object enumeration and reachability -*- lexical-binding: t; -*-

;;; Commentary:

;; Maintenance walks objects, rather than paths.  Cache commit/tag edges,
;; avoid retaining historical trees' duplicate edges, and never inflate a
;; known blob just to discover that it has no outgoing edges.

;;; Code:

(require 'ngit-rev)
(require 'ngit-index)

(defvar ngit-object--links (make-hash-table :test 'equal))
(defvar ngit-object--hex-cache (make-hash-table :test 'equal))
(defvar ngit-object--roots-valid nil
  "Canonical OIDs already validated while collecting ref/index roots.")
(defvar ngit-object--chunk-cache-limit 32768
  "Maximum processed tree windows retained by one reachability walk.")
(defvar ngit-object--chunk-cache-clears 0)
(defvar ngit-object--closure nil
  "Last successful (OID-LIST . MEMBERSHIP-TABLE), for the pack writer.
The list and table are immutable after publication; only exact list identity
permits reuse, so filtered or independently assembled lists take the normal
writer validation path.")

(defun ngit-object--hex (bin)
  "Intern the hex spelling of BIN across repeated entries in old trees."
  (or (gethash bin ngit-object--hex-cache)
      (puthash bin (ngit-hex bin) ngit-object--hex-cache)))

(defun ngit-object-tree-each (data function &optional chunks)
  "Call FUNCTION with (TREEP BIN) for each non-gitlink entry in DATA.
Search small byte windows.  Native substring/search convert their input
strings, so passing a large flat tree for every entry costs quadratically.
CHUNKS, when supplied, belongs to one reachability walk: identical windows
have already contributed all their complete entries to that walk."
  (let ((at 0) (end (length data)) (window 2048))
    (while (< at end)
      (let* ((chunk (substring data at (min end (+ at window))))
             (known (and chunks (gethash chunk chunks)))
             (limit (- (length chunk) 21)) (pos 0) nul)
        (if known (setq pos known)
          (while (and (< pos limit)
                      (setq nul (string-search "\0" chunk pos)) (<= nul limit))
            (unless (= (aref chunk (1+ pos)) ?6)
              (funcall function (= (aref chunk pos) ?4)
                       (substring chunk (1+ nul) (+ nul 21))))
            (setq pos (+ nul 21)))
          (when (and chunks (> pos 0) (<= window 8192))
            (when (>= (hash-table-count chunks) ngit-object--chunk-cache-limit)
              (clrhash chunks)
              (setq ngit-object--chunk-cache-clears
                    (1+ ngit-object--chunk-cache-clears)))
            (puthash chunk pos chunks)))
        (if (> pos 0) (setq at (+ at pos) window 2048)
          (if (= (+ at (length chunk)) end)
              (error "ngit: malformed tree object")
            (setq window (* 2 window))))))))

(defun ngit-object-links (obj)
  "Return typed (TYPE . HEX) edges from (TYPE . DATA) OBJ.
Gitlinks point into another repository and are not graph edges here."
  (let ((type (car obj)) (data (cdr obj)) out)
    (cond
     ((eq type 'tree)
      (ngit-object-tree-each
       data (lambda (tree bin)
              (push (cons (if tree 'tree 'blob) (ngit-object--hex bin)) out))))
     ((eq type 'commit)
      (let ((parsed (ngit-parse-commit data)))
        (when (ngit-header parsed "tree")
          (push (cons 'tree (ngit-header parsed "tree")) out))
        (dolist (hex (ngit-header-all parsed "parent"))
          (push (cons 'commit hex) out))))
     ((eq type 'tag)
      (let* ((parsed (ngit-parse-commit data))
             (hex (ngit-header parsed "object"))
             (kind (ngit-header parsed "type")))
        (when hex (push (cons (and kind (intern kind)) hex) out)))))
    (nreverse out)))

(defun ngit-pack-loose-oids (repo)
  "Return sorted names of the loose objects in REPO."
  (let ((dir (ngit--objects-dir repo)) out)
    (dolist (sub (directory-files dir nil nil t))
      (when (and (ngit-hex-p sub 2) (file-directory-p (concat dir sub)))
        (dolist (name (directory-files (concat dir sub) nil nil t))
          (when (ngit-hex-p name 38) (push (concat sub name) out)))))
    (sort out #'string<)))

(defun ngit-pack-all-oids (repo)
  "Return sorted distinct names of loose and packed objects in REPO."
  (let ((seen (make-hash-table :test 'equal)) out)
    (dolist (pack (ngit-repo-packs repo))
      (let* ((n (aref pack 2))
             (names (ngit-native-read-bytes (aref pack 1) 1032 (* 20 n)))
             (i 0))
        (while (< i n)
          (let ((hex (ngit-hex (substring names (* i 20) (* (1+ i) 20)))))
            (unless (gethash hex seen)
              (puthash hex t seen) (push hex out)))
          (setq i (1+ i)))))
    (dolist (hex (ngit-pack-loose-oids repo))
      (unless (gethash hex seen) (puthash hex t seen) (push hex out)))
    (sort out #'string<)))

(defun ngit-object-log-files (repo)
  "Return (REFNAME . PATH) entries for REPO's reflogs, in name order."
  (let ((dir (expand-file-name "logs" (ngit-repo-commondir repo))) out)
    (when (file-directory-p dir)
      (dolist (name (ngit--loose-ref-names dir "" nil))
        (push (cons name (expand-file-name name dir)) out)))
    (let ((head (expand-file-name "logs/HEAD" (ngit-repo-gitdir repo))))
      (when (and (file-exists-p head) (not (assoc "HEAD" out)))
        (push (cons "HEAD" head) out)))
    (sort out (lambda (a b) (string< (car a) (car b))))))

(defun ngit-object-worktrees (repo)
  "Return repository views for the main and linked worktree gitdirs."
  (let* ((common (ngit-repo-commondir repo))
         (dir (expand-file-name "worktrees" common))
         (out (list (ngit--make-repo common nil))))
    (when (file-directory-p dir)
      (dolist (name (directory-files dir nil nil t))
        (let ((path (expand-file-name name dir)))
          (when (and (not (member name '("." "..")))
                     (file-directory-p path) (file-exists-p (expand-file-name "HEAD" path)))
            (push (ngit--make-repo (file-name-as-directory path) nil) out)))))
    out))

(defun ngit-pack-roots (repo &optional reflogs index)
  "Return object names rooted by refs and HEAD in REPO.
Include reflog old/new names with REFLOGS, and index objects with INDEX."
  (let ((seen (make-hash-table :test 'equal)) roots)
    (dolist (ref (ngit-list-refs repo)) (push (nth 1 ref) roots))
    (dolist (view (ngit-object-worktrees repo))
      (let ((head (ngit-resolve-ref view "HEAD"))) (when head (push head roots)))
      ;; Worktree-private bisect refs and detached HEADs are roots too.
      (let ((dir (expand-file-name "refs" (ngit-repo-gitdir view))))
        (when (file-directory-p dir)
          (dolist (name (ngit--loose-ref-names dir "refs/" nil))
            (let ((hex (ngit-resolve-ref view name))) (when hex (push hex roots)))))))
    (when reflogs
      (let ((logs (mapcar #'cdr (ngit-object-log-files repo))))
        (dolist (view (ngit-object-worktrees repo))
          (let ((path (expand-file-name "logs/HEAD" (ngit-repo-gitdir view))))
            (when (and (file-exists-p path) (not (member path logs))) (push path logs))))
        (dolist (path logs)
          (dolist (line (split-string (ngit--read-file path) "\n" t))
            (when (>= (length line) 81)
              (let ((old (substring line 0 40)) (new (substring line 41 81)))
                (when (ngit-hex-p old 40) (push (downcase old) roots))
                (when (ngit-hex-p new 40) (push (downcase new) roots))))))))
    (when index
      (dolist (view (ngit-object-worktrees repo))
        (let* ((ix (ngit-read-index view)) (entries (aref ix 0)))
          (dotimes (i (length entries))
            (let ((e (aref entries i)))
              (unless (= (aref e 2) #o160000)
                (push (ngit-object--hex (aref e 1)) roots)))))))
    (let (out)
      ;; Traverse refs before the index: their typed tree edges mark blobs
      ;; without reading them, including the usual clean index's contents.
      (dolist (hex (nreverse roots))
        ;; Ref resolution validates textual names; index names are exactly
        ;; twenty binary bytes.  Avoid a regexp for every index entry here.
        (unless (or (string= hex "0000000000000000000000000000000000000000")
                    (gethash hex seen))
          (puthash hex t seen) (push hex out)))
      (setq ngit-object--roots-valid seen)
      (nreverse out))))

(defun ngit-pack-reachable (repo roots)
  "Return the object closure of ROOTS in REPO, failing on broken edges."
  (let ((ngit--pack-cache-limit 32768)
        (ngit-object--chunk-cache-clears 0)
        (pending (mapcar (lambda (hex)
                           (unless (or (and ngit-object--roots-valid
                                            (gethash hex ngit-object--roots-valid))
                                       (and ngit-object--closure
                                            (gethash hex (cdr ngit-object--closure))))
                             (unless (ngit-hex-p hex 40)
                               (error "fatal: bad object %s" hex))
                             (setq hex (downcase hex)))
                           (cons nil hex)) roots))
        (seen (make-hash-table :test 'equal))
        (binary-seen (make-hash-table :test 'equal))
        (chunks (make-hash-table :test 'equal))
        (prefix (ngit-repo-commondir repo))
        (profile (equal (getenv "NGIT_PROFILE") "1"))
        (start (float-time)) (count 0) out)
    (when profile (ngit-native-write-stderr "profile/reachable-start\n"))
    (while pending
      (let* ((edge (pop pending)) (hex (cdr edge)))
        (unless (gethash hex seen)
          (puthash hex t seen) (push hex out)
          (unless (eq (car edge) 'blob)
            (let* ((bin (ngit-unhex hex))
                   (obj (ngit-read-object repo bin)))
              (setq count (1+ count))
              (when (and profile (= (% count 1000) 0))
                (ngit-native-write-stderr
                 (format "profile/reachable %d objects, %.3fs\n"
                         count (- (float-time) start))))
              (puthash bin t binary-seen)
              (if (eq (car obj) 'tree)
                  ;; Most entries in a historical tree have already been
                  ;; visited.  Test their binary names before allocating hex
                  ;; strings or graph edges, and do not retain whole trees'
                  ;; duplicate edge lists for the lifetime of gc.
                  (ngit-object-tree-each
                   (cdr obj)
                   (lambda (tree child)
                     (unless (gethash child binary-seen)
                       (puthash child t binary-seen)
                       (let ((name (ngit-object--hex child)))
                         (unless (gethash name seen)
                           (if tree (push (cons 'tree name) pending)
                             (puthash name t seen) (push name out))))))
                   chunks)
                (let* ((key (concat prefix hex))
                       (links (gethash key ngit-object--links 'unknown)))
                  (when (eq links 'unknown)
                    (setq links (ngit-object-links obj))
                    ;; The pack writer trusts this closure.  Commit/tag
                    ;; headers are textual input, unlike binary tree/index
                    ;; names, and must be checked before hex decoding.
                    (dolist (link links)
                      (unless (ngit-hex-p (cdr link) 40)
                        (error "fatal: bad object %s" (cdr link)))
                      (setcdr link (downcase (cdr link))))
                    (puthash key links ngit-object--links))
                  (dolist (link links) (push link pending)))))))))
    (when profile
      (ngit-native-write-stderr
       (format "profile/tree-windows: %d cached, %d clears\n"
               (hash-table-count chunks) ngit-object--chunk-cache-clears)))
    (setq out (nreverse out)
          ngit-object--closure (cons out seen))
    out))

(provide 'ngit-objects)

;;; ngit-objects.el ends here
