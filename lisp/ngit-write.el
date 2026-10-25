;;; ngit-write.el --- writing objects, the index and trees -*- lexical-binding: t; -*-

;;; Commentary:

;; Loose objects are deflated at core.looseCompression (default 1,
;; Z_BEST_SPEED, as in git) and land through a temporary file and a
;; rename.  The index writer reproduces git's do_write_index byte for
;; byte: version 2 or 3 chosen by the extended flags (4 is kept), v4
;; prefix compression, the racy-clean smudge and the TREE extension.
;;
;; Entries are the vectors of `ngit-read-index'.  An entry built here has
;; a seventh slot holding its 40-byte stat block and a nil POS; entries
;; read from disk take theirs from the raw index.
;;
;; A cache tree node is a vector [COUNT BIN SUBS]: COUNT is the number of
;; index entries covered, or -1 when the node is invalid; SUBS is an
;; alist (NAME . NODE) in git's subtree order (name length, then bytes).

;;; Code:

(require 'ngit-worktree)
(require 'ngit-config)
(require 'ngit-cli)
(require 'ngit-changes)

;;;; Files and locks

(defun ngit-write-file (path data &optional excl append mode)
  "Write unibyte DATA to PATH; with EXCL, fail when PATH exists.
With APPEND, append to PATH.  Preserve existing file permissions.
MODE overrides new file permissions; `defer' leaves them for the caller."
  (let ((new (and (not (eq mode 'defer)) (or excl (not (file-exists-p path)))))
        (coding-system-for-write 'no-conversion))
    (write-region data nil path append 'silent nil (and excl 'excl))
    (when new
      (let ((mode (or mode (ngit-native-creation-mode #o666))))
        ;; Standalone write-region creates files as 0644, ignoring umask.
        (unless (= (file-modes path) mode) (set-file-modes path mode))))))

(defun ngit-make-directory (path &optional parents)
  "Create PATH, optionally with PARENTS, using the process umask."
  ;; Unlike write-region, make-directory already honors the process umask.
  (make-directory path parents))

(defun ngit-lock (path &optional mode)
  "Create PATH.lock exclusively and return its name, or die as git does.
MODE controls permissions as in `ngit-write-file'."
  (let ((lock (concat path ".lock")))
    (condition-case nil
        (ngit-write-file lock "" t nil mode)
      (file-already-exists
       (ngit--die (concat "fatal: Unable to create '%s': File exists.\n\n"
                          "Another git process seems to be running in this repository, e.g.\n"
                          "an editor opened by 'git commit'. Please make sure all processes\n"
                          "are terminated then try again. If it still fails, a git process\n"
                          "may have crashed in this repository earlier:\n"
                          "remove the file manually to continue.")
                  (expand-file-name lock))))
    lock))

(defun ngit-unlock (lock)
  (when (file-exists-p lock) (delete-file lock)))

(defun ngit-commit-lock (lock data &optional mode)
  "Write DATA to LOCK and rename it over the file it guards.
With MODE, set final permissions after writing, before the rename."
  (ngit-write-file lock data)
  (when (and mode (/= (file-modes lock) mode)) (set-file-modes lock mode))
  (rename-file lock (substring lock 0 -5) t))

;;;; Objects

(defconst ngit-empty-tree-bin (ngit-unhex "4b825dc642cb6eb9a060e54bf8d69288fbee4904"))

(defun ngit--loose-level (repo)
  (ngit-config-compression-level repo "core.loosecompression"
                   (ngit-config-compression-level repo "core.compression" 1)))

(defun ngit-write-object (repo type data &optional dry-run)
  "Store DATA as a loose object of TYPE (a string); return its 20-byte name.
With DRY-RUN, only compute the name."
  (let* ((obj (concat type " " (number-to-string (length data)) "\0" data))
         (bin (ngit-native-sha1 obj))
         (hex (ngit-hex bin))
         (level (unless dry-run (ngit--loose-level repo))))
    (unless (or dry-run (ngit-object-exists-p repo bin))
      (let* ((path (ngit--loose-path repo hex))
             (dir (file-name-directory path))
             (tmp (concat dir "tmp_obj_" (number-to-string (emacs-pid))
                          "_" (number-to-string (random 1000000000)))))
        (ngit-make-directory dir t)
        (ngit-write-file tmp obj t nil 'defer)
        (ngit-write-file tmp (ngit-native-deflate-file tmp (length obj) level))
        (set-file-modes tmp #o444)
        (rename-file tmp path t)
        (ngit-abbrev-forget)))
    bin))

;;;; Index entries

(defun ngit--be32-bytes (v)
  (unibyte-string (logand (ash v -24) 255) (logand (ash v -16) 255)
                  (logand (ash v -8) 255) (logand v 255)))

(defun ngit-wt-stat-block (mode)
  "Return the 40-byte index stat block for the last lstat and git MODE."
  (let ((sa (nl-ffi-memory-address ngit-wt--stat)))
    (concat (ngit--be32-bytes (ptr-read-u64 sa 104)) (ngit--be32-bytes (ptr-read-u64 sa 112))
            (ngit--be32-bytes (ptr-read-u64 sa 88)) (ngit--be32-bytes (ptr-read-u64 sa 96))
            (ngit--be32-bytes (ptr-read-u64 sa 0)) (ngit--be32-bytes (ptr-read-u64 sa 8))
            (ngit--be32-bytes mode) (ngit--be32-bytes (ptr-read-u32 sa 28))
            (ngit--be32-bytes (ptr-read-u32 sa 32)) (ngit--be32-bytes (ptr-read-u64 sa 48)))))

(defun ngit-make-entry (name bin mode stat &optional flags)
  "Return a new stage-0 index entry with stat block STAT."
  (vector name bin mode 0 nil flags stat))

(defun ngit--entry-less (a b)
  (let ((na (aref a 0)) (nb (aref b 0)))
    (or (string< na nb) (and (string= na nb) (< (aref a 3) (aref b 3))))))

(defun ngit-index-overlay (old targets dropped &optional removed)
  "Patch OLD with TARGETS and exact names in DROPPED, without sorting OLD.
Remove conflicting ancestors and descendants of TARGETS too.  Call REMOVED
for topology conflicts, but not for same-name replacements.  Sorted runs of
unchanged entries are copied as vectors rather than visited individually."
  (if (= (length old) 0)
      (if (null targets) old
        (vconcat
         (sort (nreverse (copy-sequence targets))
               (lambda (a b) (string< (aref a 0) (aref b 0))))))
    (let ((patches nil) (parts nil) (cursor 0)
          (topology (and removed (make-hash-table :test 'equal))))
      (let ((drop (lambda (name topology-p)
                    (let ((lo (ngit-index-find old name)) (hi nil))
                      (setq hi lo)
                      (while (and (< hi (length old))
                                  (string= (aref (aref old hi) 0) name))
			(when (and topology-p topology)
                          (puthash name (aref old hi) topology))
			(setq hi (1+ hi)))
                      (when (> hi lo) (push (vector lo hi nil) patches))))))
	(when dropped (maphash (lambda (name _) (funcall drop name nil)) dropped))
	(dolist (entry targets)
          (let* ((name (aref entry 0)) (lo (ngit-index-find old name))
		 (start (ngit-index-find old (concat name "/")))
		 (end (ngit-index-find old (concat name "0")))
		 (pos 0) (slash nil))
            (funcall drop name nil)
            (push (vector lo lo entry) patches)
            (when (> end start)
              (push (vector start end nil) patches)
              (when topology
		(while (< start end)
                  (puthash (aref (aref old start) 0) (aref old start) topology)
                  (setq start (1+ start)))))
            (while (setq slash (ngit--byte-pos name ?/ pos))
              (funcall drop (substring name 0 slash) t)
              (setq pos (1+ slash))))))
      (setq patches
            (sort patches
                  (lambda (a b)
                    (if (= (aref a 0) (aref b 0))
			(and (aref b 2)
                             (or (not (aref a 2))
				 (string< (aref (aref a 2) 0) (aref (aref b 2) 0))))
                      (< (aref a 0) (aref b 0))))))
      (dolist (patch patches)
	(when (> (aref patch 0) cursor)
          (push (substring old cursor (aref patch 0)) parts))
	(setq cursor (max cursor (aref patch 1)))
	(when (aref patch 2) (push (vector (aref patch 2)) parts)))
      (when (< cursor (length old)) (push (substring old cursor) parts))
      (when topology (maphash (lambda (_ entry) (funcall removed entry)) topology))
      (if patches (apply #'vconcat (nreverse parts)) old))))

(defun ngit-entry-lstat (index e)
  "lstat the work tree path of E.  Version 2/3 entries read from disk
pass their NUL-padded name in the native copy of the index to fstatat,
which avoids copying the path (~1.6 ms per call in NeLisp)."
  (let ((pos (aref e 4)) (native (aref index 6)))
    (if (and pos native (/= (aref index 2) 4))
        (ngit-wt-lstat-at (+ native pos (if (= 0 (logand (aref (aref index 1) (+ pos 60)) #x40))
                                             62 64)))
      (ngit-wt-lstat (aref e 0)))))

(defun ngit--stat-match-p (raw pos st)
  "Non-nil when the stat block at POS in RAW matches lstat ST
(git's ce_match_stat_basic without the racy test)."
  (and (= (ngit--be32 raw (+ pos 8)) (logand (aref st 3) #xffffffff))
       (= (ngit--be32 raw (+ pos 36)) (logand (aref st 2) #xffffffff))
       (= (ngit--be32 raw pos) (logand (aref st 5) #xffffffff))
       (= (ngit--be32 raw (+ pos 20)) (logand (aref st 1) #xffffffff))))

(defun ngit--racily-modified-p (repo index e filemode)
  "Non-nil when E is racily clean: its stat matches the work tree, its
mtime is not older than the index file, and its content differs."
  (let ((raw (aref index 1)) (pos (aref e 4)) (ts (aref index 4)))
    (and pos (/= ts 0)
         (/= (aref e 2) ngit-mode-gitlink)
         (<= ts (ngit--be32 raw (+ pos 8)))
         (let ((st (ngit-wt-lstat (aref e 0))))
           (and st (ngit--stat-match-p raw pos st)
                (let ((wmode (ngit-wt-mode st)))
                  (when (and (not filemode) (= (logand wmode #o170000) #o100000)
                             (= (logand (aref e 2) #o170000) #o100000))
                    (setq wmode (aref e 2)))
                  ;; A mode or type change is a stat mismatch in git's
                  ;; ce_match_stat_basic, so such entries are not smudged.
                  (and (= wmode (aref e 2))
                       (not (equal (ngit-hash-blob (ngit-wt-content repo (aref e 0) st))
                                   (aref e 1))))))))))

(defun ngit--encode-varint (v)
  (let ((bytes (list (logand v 127))))
    (setq v (ash v -7))
    (while (> v 0)
      (setq v (1- v))
      (push (logior 128 (logand v 127)) bytes)
      (setq v (ash v -7)))
    (apply #'unibyte-string bytes)))

(defun ngit--common-prefix-length (a b)
  (let ((n (min (length a) (length b))) (i 0))
    (while (and (< i n) (= (aref a i) (aref b i))) (setq i (1+ i)))
    i))

;;;; Cache tree

(defun ngit-ct-new () (vector -1 nil nil))

(defun ngit--ct-parse (s pos)
  "Parse the node at POS of TREE data S; return (NAME NODE . NEXT-POS)."
  (let* ((nul (ngit--byte-pos s 0 pos))
         (name (substring s pos nul))
         (cnt (ngit--parse-ascii-int s (1+ nul)))
         (subn (ngit--parse-ascii-int s (1+ (cdr cnt))))
         (p (1+ (cdr subn)))
         (bin nil) (subs nil))
    (when (>= (car cnt) 0)
      (setq bin (substring s p (+ p 20)) p (+ p 20)))
    (dotimes (_ (car subn))
      (let ((r (ngit--ct-parse s p)))
        (push (cons (car r) (cadr r)) subs)
        (setq p (cddr r))))
    (cons name (cons (vector (car cnt) bin (nreverse subs)) p))))

(defun ngit-ct-root (index)
  "Return a fresh copy of INDEX's cache tree as nodes, or nil."
  (let ((range (aref index 7)))
    (and range (> (cdr range) 0)
         (cadr (ngit--ct-parse (aref index 1) (car range))))))

(defun ngit--ct-less (a b)
  (or (< (length a) (length b))
      (and (= (length a) (length b)) (string< a b))))

(defun ngit--ct-sub (node name create)
  "Return NODE's subtree NAME, creating it in order when CREATE."
  (or (cdr (assoc name (aref node 2)))
      (and create
           (let ((sub (ngit-ct-new)))
             (aset node 2 (sort (cons (cons name sub) (aref node 2))
                                (lambda (a b) (ngit--ct-less (car a) (car b)))))
             sub))))

(defun ngit-ct-invalidate (node path)
  "Invalidate NODE and its subtrees along PATH (git's
cache_tree_invalidate_path); a subtree named exactly PATH is dropped."
  (let ((start 0))
    (while node
      (aset node 0 -1)
      (let ((slash (ngit--byte-pos path ?/ start)))
        (if slash
            (setq node (ngit--ct-sub node (substring path start slash) nil)
                  start (1+ slash))
          (let ((name (substring path start)))
            (aset node 2 (seq-remove (lambda (s) (string= (car s) name)) (aref node 2))))
          (setq node nil))))))

(defun ngit-ct-fully-valid-p (node)
  (and node (>= (aref node 0) 0)
       (seq-every-p (lambda (s) (ngit-ct-fully-valid-p (cdr s))) (aref node 2))))

(defun ngit--ct-serialize (node name)
  (apply #'concat name "\0" (number-to-string (aref node 0)) " "
         (number-to-string (length (aref node 2))) "\n"
         (if (>= (aref node 0) 0) (aref node 1) "")
         (mapcar (lambda (s) (ngit--ct-serialize (cdr s) (car s))) (aref node 2))))

(defconst ngit--mode-strings
  '((#o100644 . "100644") (#o100755 . "100755") (#o120000 . "120000")
    (#o160000 . "160000") (#o040000 . "40000")))

(defun ngit--range-end (entries lo hi base)
  "Return the end of the run of ENTRIES in [LO, HI) under directory BASE."
  (if (string= base "")
      hi
    (let ((key (concat (substring base 0 -1) "0")) (h hi))
      ;; "0" is the byte after "/": the first name past BASE's subtree.
      (while (< lo h)
        (let ((mid (ash (+ lo h) -1)))
          (if (string< (aref (aref entries mid) 0) key) (setq lo (1+ mid)) (setq h mid))))
      lo)))

(defun ngit--ct-update (repo node entries lo hi base)
  "Bring NODE, the cache tree of directory BASE, up to date for ENTRIES
from LO (git's update_one); write missing trees; return the number of
entries it covers."
  (if (and (>= (aref node 0) 0) (aref node 1) (ngit-object-exists-p repo (aref node 1)))
      (aref node 0)
    (let* ((blen (length base))
           (end (ngit--range-end entries lo hi base))
           (i lo) (counts nil) (parts nil) (invalid nil))
      (while (< i end)
        (let* ((name (aref (aref entries i) 0))
               (slash (ngit--byte-pos name ?/ blen)))
          (if (not slash)
              (setq i (1+ i))
            (let* ((sname (substring name blen slash))
                   (cnt (ngit--ct-update repo (ngit--ct-sub node sname t) entries i end
                                         (substring name 0 (1+ slash)))))
              (push (cons sname cnt) counts)
              (setq i (+ i cnt))))))
      (aset node 2 (seq-filter (lambda (s) (assoc (car s) counts)) (aref node 2)))
      (setq i lo)
      (while (< i end)
        (let* ((e (aref entries i))
               (name (aref e 0))
               (slash (ngit--byte-pos name ?/ blen)))
          (if slash
              (let* ((sname (substring name blen slash))
                     (sub (ngit--ct-sub node sname nil)))
                (setq i (+ i (cdr (assoc sname counts))))
                (when (< (aref sub 0) 0) (setq invalid t))
                (unless (and (< (aref sub 0) 0) (equal (aref sub 1) ngit-empty-tree-bin))
                  (push (concat "40000 " sname "\0" (aref sub 1)) parts)))
            (setq i (1+ i))
            (if (memq 'ita (aref e 5))
                (setq invalid t)
              (push (concat (or (cdr (assq (aref e 2) ngit--mode-strings))
                                (format "%o" (aref e 2)))
                            " " (substring name blen) "\0" (aref e 1))
                    parts)))))
      (aset node 1 (ngit-write-object repo "tree" (apply #'concat (nreverse parts))))
      (aset node 0 (if invalid -1 (- end lo)))
      (- end lo))))

(defun ngit-ct-update (repo root entries)
  "Update cache tree ROOT for the vector ENTRIES; return the root tree name."
  (let ((n (length entries)) (i 0))
    (while (< i n)
      (when (> (aref (aref entries i) 3) 0)
        (ngit--die "error: '%s' is unmerged" (aref (aref entries i) 0)))
      (setq i (1+ i)))
    (ngit--ct-update repo root entries 0 n "")
    (aref root 1)))

;;;; Writing the index

(defun ngit--entry-bytes (raw e version prev smudge)
  (let* ((name (aref e 0))
         (nlen (length name))
         (pos (aref e 4))
         (eflags (aref e 5))
         (extended-flags (or (memq 'ita eflags) (memq 'skip-worktree eflags)))
         (stat (if pos (substring raw pos (+ pos 40)) (aref e 6)))
         (flags (logior (if (or (memq 'assume-unchanged eflags)
                                 (and pos (/= 0 (logand (aref raw (+ pos 60)) #x80))))
                             #x8000 0)
                        (if extended-flags #x4000 0)
                        (ash (aref e 3) 12) (min nlen #xfff)))
         (head (concat (if smudge (concat (substring stat 0 36) "\0\0\0\0") stat)
                       (aref e 1)
                       (unibyte-string (ash flags -8) (logand flags 255))
                       (if extended-flags
                           (let ((x (logior (if (memq 'ita eflags) #x2000 0)
                                            (if (memq 'skip-worktree eflags) #x4000 0))))
                             (unibyte-string (ash x -8) (logand x 255)))
                         ""))))
    (if (= version 4)
        (let ((common (ngit--common-prefix-length prev name)))
          (concat head (ngit--encode-varint (- (length prev) common))
                  (substring name common) "\0"))
      (concat head name (make-string (- (logand (+ (length head) nlen 8) -8)
                                        (length head) nlen)
                                     0)))))

(defun ngit--resolve-undo-record (name stages)
  "Encode the REUC record for NAME and its original STAGES."
  (let ((modes nil) (bins nil))
    (dotimes (i 3)
      (let ((e (aref stages (1+ i))))
        (push (concat (if e (format "%o" (aref e 2)) "0") "\0") modes)
        (when e (push (aref e 1) bins))))
    (concat name "\0" (apply #'concat (nreverse modes))
            (apply #'concat (nreverse bins)))))

(defun ngit-index-resolve-undo (index entries)
  "Keep INDEX's REUC data and record unmerged paths resolved in ENTRIES."
  (let ((undo (aref index 8)) (updates nil) (previous nil) (stages nil))
    (dolist (e (aref index 9))
      (let ((name (aref e 0)))
        (unless (equal name previous)
          (setq previous name stages nil)
          (let ((new (ngit-index-lookup entries name)))
            (when (or (null new) (= (aref new 3) 0))
              (setq stages (make-vector 4 nil))
              (push (cons name stages) updates))))
        ;; Callers may already have installed ENTRIES into INDEX.  The
        ;; saved conflict list still owns the original stage objects.
        (when stages (aset stages (aref e 3) e))))
    (if (null updates)
        undo
      (let ((records nil) (pos 0) (n (length (or undo ""))))
        (while (< pos n)
          (let* ((start pos) (nul (ngit--byte-pos undo 0 pos))
                 (name (substring undo pos nul))
                 (update (cdr (assoc name updates))) (modes (make-vector 4 0)))
            (setq pos (1+ nul))
            (dotimes (i 3)
              (setq nul (ngit--byte-pos undo 0 pos))
              (aset modes (1+ i) (string-to-number (substring undo pos nul) 8))
              (setq pos (1+ nul)))
            (dotimes (i 3)
              (let* ((stage (1+ i)) (mode (aref modes stage)))
                (unless (= mode 0)
                  ;; record_resolve_undo only overwrites present stages.
                  (when (and update (null (aref update stage)))
                    (aset update stage (vector nil (substring undo pos (+ pos 20)) mode)))
                  (setq pos (+ pos 20)))))
            (unless update
              (push (cons name (substring undo start pos)) records))))
        (mapconcat (lambda (record)
                     (if (stringp (cdr record)) (cdr record)
                       (ngit--resolve-undo-record (car record) (cdr record))))
                   (sort (append records updates)
                         (lambda (a b) (string< (car a) (car b)))) "")))))

(defun ngit-index-bytes (repo index entries root &optional checked)
  "Return the index file for ENTRIES (a sorted vector) and cache tree ROOT.
INDEX is the index they came from (raw data, version, timestamp).
Entries read from disk whose raw position is not a key of the `eql'
hash table CHECKED get git's racy-clean smudge test.  (Positions, not
the entry vectors: `puthash' of a vector key costs ~1 ms in NeLisp.)"
  (let* ((raw (aref index 1))
         (n (length entries))
         (extended (seq-some (lambda (e)
                               (let ((flags (aref e 5)))
                                 (or (memq 'ita flags) (memq 'skip-worktree flags))))
                             entries))
         (version (cond ((= (aref index 2) 4) 4) (extended 3) (t 2)))
         (reuse (/= version 4))
         (ts (aref index 4))
         (filemode (ngit-config-bool repo "core.filemode" t))
         (parts (list (concat "DIRC" (ngit--be32-bytes version) (ngit--be32-bytes n))))
         (chunk-start nil) (chunk-end nil)
         (prev "") (i 0))
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (while (< i n)
      (let* ((e (aref entries i))
             (pos (aref e 4))
             ;; Inlined racy test (mtime >= index mtime): a function call
             ;; per entry costs a quarter of a millisecond here.
             (smudge (and pos (/= ts 0)
                          (<= ts (logior (ash (aref raw (+ pos 8)) 24) (ash (aref raw (+ pos 9)) 16)
                                         (ash (aref raw (+ pos 10)) 8) (aref raw (+ pos 11))))
                          (not (and checked (gethash (aref e 4) checked)))
                          (ngit--racily-modified-p repo index e filemode))))
        (if (and reuse pos (not smudge))
            ;; Unchanged v2/v3 entries are copied from the old index in
            ;; runs: one `substring' per run instead of ~30 primitives
            ;; per entry.
            (let ((end (+ pos (logand (+ (if (= 0 (logand (aref raw (+ pos 60)) #x40)) 62 64)
                                         (length (aref e 0)) 8)
                                      -8))))
              (if (eql chunk-end pos)
                  (setq chunk-end end)
                (when chunk-start (push (substring raw chunk-start chunk-end) parts))
                (setq chunk-start pos chunk-end end)))
          (when chunk-start
            (push (substring raw chunk-start chunk-end) parts)
            (setq chunk-start nil chunk-end nil))
          (push (ngit--entry-bytes raw e version prev smudge) parts))
        (setq prev (aref e 0) i (1+ i))))
    (when chunk-start (push (substring raw chunk-start chunk-end) parts))
    (when root
      (let ((tree (ngit--ct-serialize root "")))
        (push (concat "TREE" (ngit--be32-bytes (length tree)) tree) parts)))
    (let ((undo (ngit-index-resolve-undo index entries)))
      (when undo
        (push (concat "REUC" (ngit--be32-bytes (length undo)) undo) parts)))
    (let ((body (apply #'concat (nreverse parts))))
      (concat body (ngit-native-sha1 body)))))

(defun ngit-write-index (repo index entries root &optional checked)
  "Write ENTRIES and ROOT as REPO's index through index.lock."
  (let* ((file (ngit-index-file repo))
         (lock (ngit-lock file)))
    (condition-case err
        (ngit-commit-lock lock (ngit-index-bytes repo index entries root checked))
      (error (ngit-unlock lock) (signal (car err) (cdr err))))
    (aset repo 6 nil)))

;;;; Commands

(defun ngit-cmd-hash-object (repo args)
  (let ((write nil) (type "blob") (filters t) (files nil))
    (while args
      (let ((a (pop args)))
        (cond ((string= a "-w") (setq write t))
              ((string= a "-t") (setq type (pop args)))
              ((string= a "--no-filters") (setq filters nil))
              ((string= a "--") (setq files (append (reverse args) files) args nil))
              (t (push a files)))))
    (dolist (f (nreverse files))
      (let* ((data (ngit--read-file (expand-file-name f)))
             (data (if (and filters (string= type "blob"))
                       (ngit-convert-to-git repo (encode-coding-string f 'utf-8) data)
                     data)))
        (ngit--emit (ngit-hex (ngit-write-object repo type data (not write))) "\n")))
    0))

(defun ngit-cmd-write-tree (repo _args)
  (let* ((index (ngit-read-index repo))
         (entries (aref index 0))
         (_ (when (aref index 9)
              (let ((n 0))
                (dolist (e (aref index 9))
                  (when (< n 10)
                    (ngit-native-write-stderr
                     (concat (aref e 0) ": unmerged (" (ngit-hex (aref e 1)) ")\n")))
                  (when (= n 10) (ngit-native-write-stderr "...\n"))
                  (setq n (1+ n))))
              (ngit--die "fatal: git-write-tree: error building trees")))
         (root (or (ngit-ct-root index) (ngit-ct-new)))
         (was-valid (ngit-ct-fully-valid-p root))
         (bin (ngit-ct-update repo root entries)))
    (unless was-valid
      (ngit-write-index repo index entries root))
    (ngit--emit (ngit-hex bin) "\n")
    0))

(defun ngit-cmd-update-index (repo args)
  "Only `--force-write-index': rewrite the index as git does."
  (unless (equal args '("--force-write-index"))
    (ngit--die "ngit: update-index supports only --force-write-index"))
  (let ((index (ngit-read-index repo)))
    (ngit-write-index repo index (aref index 0) (ngit-ct-root index))
    0))

(provide 'ngit-write)

;;; ngit-write.el ends here
