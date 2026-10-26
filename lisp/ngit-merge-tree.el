;;; ngit-merge-tree.el --- Three-tree merges with file renames -*- lexical-binding: t; -*-

;;; Commentary:

;; The tree result is separate from its installation, as in merge-ort.c.
;; Callers can reuse it for merge, stash, and later sequencer commands.
;; A result carries the conflicted index and a stage-zero work-tree image;
;; the latter is also the tree recorded by AUTO_MERGE.

;;; Code:

(require 'ngit-switch)
(require 'ngit-merge-file)
(require 'ngit-merge-rename)

(defvar ngit-merge-ancestor-label "merged common ancestors")
(defvar ngit-merge-marker-extra 0)
(defvar ngit-merge-unique-worktree-root nil
  "When bound by checkout, avoid existing work-tree paths in conflict names.
Other merge callers retain their tree-only naming policy.")

(defun ngit-merge-tree-entries (repo tree)
  "Return sorted stage-zero entries of binary TREE, or an empty vector."
  (let ((out nil))
    (when tree
      (ngit--flatten-tree repo tree ""
                          (lambda (path mode bin)
                            (push (ngit--switch-entry path mode bin nil) out))))
    (vconcat (sort out #'ngit--entry-less))))

(defun ngit-merge--same (a b)
  (or (eq a b)
      (and a b (= (aref a 2) (aref b 2)) (equal (aref a 1) (aref b 1)))))

(defun ngit-merge--copy (entry path &optional stage)
  (when entry
    (let ((out (ngit--switch-entry path (aref entry 2) (aref entry 1) nil)))
      (aset out 3 (or stage 0))
      out)))

(defun ngit-merge--blob (repo entry)
  (if entry (cdr (ngit-read-object repo (aref entry 1))) ""))

(defun ngit-merge--binary (data)
  (and (ngit--byte-pos (if (> (length data) 8000) (substring data 0 8000) data)
                       0 0) t))

(defun ngit-merge--unique (path label names)
  (let* ((stem (concat path "~" (string-replace "/" "_" label)))
         (out stem) (n 0))
    (while (or (gethash out names)
               (and ngit-merge-unique-worktree-root
                    (let ((file (expand-file-name
                                 (decode-coding-string out 'utf-8)
                                 ngit-merge-unique-worktree-root)))
                      (or (file-exists-p file) (file-symlink-p file)))))
      (setq out (concat stem "_" (number-to-string n)) n (1+ n)))
    (puthash out t names)
    out))

(defun ngit-merge--content (repo path base ours theirs ours-label theirs-label depth)
  "Return (ENTRY CLEAN MESSAGES) for two present, equally typed sides."
  (let* ((bm (if base (aref base 2) 0))
         (om (aref ours 2)) (tm (aref theirs 2))
         (bb (and base (aref base 1))) (ob (aref ours 1)) (tb (aref theirs 1))
         (mode (if (or (= om tm) (= om bm)) tm om))
         (clean (or (= om tm) (= om bm) (= tm bm)))
         (bin nil) (messages nil) (kind (logand om #o170000)))
    (cond
     ((or (equal ob tb) (equal ob bb)) (setq bin tb))
     ((equal tb bb) (setq bin ob))
     ((= kind #o100000)
      (let* ((base-data (if (= (logand bm #o170000) kind)
                            (ngit-merge--blob repo base) ""))
             (ours-data (ngit-merge--blob repo ours))
             (theirs-data (ngit-merge--blob repo theirs))
             (attrs (ngit-attrs repo path '("merge" "conflict-marker-size")))
             (driver (cdr (assoc "merge" attrs)))
             (marker (cdr (assoc "conflict-marker-size" attrs)))
             (style (ngit-config-get repo "merge.conflictstyle"))
             (binary (or (eq driver 'unset) (equal driver "binary")
                         (and (not (eq driver 'set)) (not (equal driver "text"))
                              (or (ngit-merge--binary base-data)
                                  (ngit-merge--binary ours-data)
                                  (ngit-merge--binary theirs-data)))))
             (merged nil))
        (if binary
            (progn
              (if (> depth 0)
                  (setq bin (ngit-write-object repo "blob" base-data))
                (setq clean nil bin ob)
                (push (concat "warning: Cannot merge binary files: " path " ("
                              ours-label " vs. " theirs-label ")") messages)))
          (setq merged
                (ngit-merge-file
                 base-data ours-data theirs-data
                 :labels (list ngit-merge-ancestor-label ours-label theirs-label)
                 :style (cond ((equal style "diff3") 'diff3)
                              ((equal style "zdiff3") 'zdiff3) (t 'merge))
                 :favor (if (equal driver "union") 'union 'none)
                 :marker-size (+ (if (and (stringp marker)
                                          (> (string-to-number marker) 0))
                                     (string-to-number marker) 7)
                                 (* depth 2) ngit-merge-marker-extra)))
          (setq bin (ngit-write-object repo "blob" (car merged)))
          (when (> (cdr merged) 0) (setq clean nil)))
        (push (concat "Auto-merging " path) messages)))
     ;; Symlinks are opaque strings.  Recursive merges retain the base.
     (t (setq clean nil bin (if (> depth 0) bb ob))
        (when (> depth 0) (setq mode bm))))
    (list (and bin (/= mode 0) (ngit--switch-entry path mode bin nil))
          clean (nreverse messages))))

(defun ngit-merge--disjoint (repo base-tree ours-tree theirs-tree)
  "Reuse the index and cache tree for small, disjoint changes.
Tree diffs skip equal subtrees.  This avoids flattening an entire large
repository merely to replay a few unrelated paths."
  (let* ((left (ngit-diff-trees repo base-tree ours-tree))
         (right (ngit-diff-trees repo base-tree theirs-tree))
         (index (ngit-read-index repo))
         (root (ngit-ct-root index))
         ;; Filename nonoverlap does not prove independence: a deleted file
         ;; on either side can acquire its new name on an unrelated path.
         (overlap (and (ngit-merge-renames-p repo)
                       (seq-some (lambda (c) (= (aref c 1) ?D)) (append left right)))))
    (when (and root (not (aref index 9))
               (not (seq-some (lambda (e) (memq 'ita (aref e 5))) (aref index 0)))
               (<= (length left) 64) (<= (length right) 64)
               (not (ngit-diff-tree-index repo ours-tree)))
      (dolist (a left)
        (dolist (b right)
          (let ((ap (aref a 0)) (bp (aref b 0)))
            (when (or (equal ap bp) (string-prefix-p (concat ap "/") bp)
                      (string-prefix-p (concat bp "/") ap))
              (setq overlap t)))))
      (unless overlap
        (let ((targets nil) (dropped (make-hash-table :test 'equal)))
          (dolist (c right)
            (puthash (aref c 0) t dropped)
            (ngit-ct-invalidate root (aref c 0))
            (unless (= (aref c 4) 0)
              (push (ngit--switch-entry (aref c 0) (aref c 4) (aref c 5) nil) targets)))
          (let* ((entries (ngit-index-overlay (aref index 0) targets dropped))
                 (tree (ngit-ct-update repo root entries)))
            (list :entries entries :worktree entries :tree tree :cache-tree root
                  :sparse t :paths (mapcar (lambda (c) (aref c 0)) right))))))))

(defun ngit-merge-trees (repo base-tree ours-tree theirs-tree ours-label theirs-label
                            &optional virtual)
  (or (and (not virtual) (ngit-merge--disjoint repo base-tree ours-tree theirs-tree))
      (ngit-merge--full repo base-tree ours-tree theirs-tree ours-label theirs-label virtual)))

(defun ngit-merge--full (repo base-tree ours-tree theirs-tree ours-label theirs-label
                            &optional virtual)
  "Merge three binary trees with file renames, returning a result plist.
The :entries vector is a complete index, including stages 1-3 on conflicts.
The :worktree vector contains stage-zero entries with conflict marker blobs.
The :tree is the materialized tree, :conflicts is a sorted list of paths,
and :messages contains ordered diagnostic strings without newlines.
:conflicted also records conflicts which leave every index entry at stage zero.
VIRTUAL is a positive recursive-ancestor depth, or nil for the real merge."
  (let* ((base (ngit-merge-tree-entries repo base-tree))
         (ours (ngit-merge-tree-entries repo ours-tree))
         (theirs (ngit-merge-tree-entries repo theirs-tree))
         (table (make-hash-table :test 'equal))
         (names (make-hash-table :test 'equal))
         (paths nil) (records nil) (depth (if (numberp virtual) virtual (if virtual 1 0)))
         (entries nil) (worktree nil) (conflicts nil) (messages nil)
         (directory-metadata nil) (directory-conflict nil))
    ;; Keep a side list: enumerating hash tables is expensive in NeLisp.
    (let ((side 1))
      (dolist (vector (list base ours theirs))
        (let ((i 0))
          (while (< i (length vector))
            (let* ((e (aref vector i)) (path (aref e 0)) (rec (gethash path table)))
              (unless rec
                ;; [PATH BASE OURS THEIRS RESULT CONFLICT MESSAGES DONE LABELS]
                (setq rec (vector path nil nil nil nil nil nil nil nil))
                (puthash path rec table)
                (puthash path t names)
                (let ((start 0) slash)
                  (while (setq slash (ngit--byte-pos path ?/ start))
                    (puthash (substring path 0 slash) t names)
                    (setq start (1+ slash))))
                (push path paths))
              (aset rec side e))
            (setq i (1+ i))))
        (setq side (1+ side))))
    (setq paths (sort paths #'string<))
    (when (ngit-merge-renames-p repo)
      (let* ((left (ngit-merge-rename-map repo base-tree ours-tree))
             (right (ngit-merge-rename-map repo base-tree theirs-tree))
             (ld (ngit-merge-directory-map left ours))
             (rd (ngit-merge-directory-map right theirs)))
        (ngit-merge-rename-reconcile repo table left right ours-label theirs-label depth)
        (let ((dirs (ngit-merge-directory-reconcile
                     repo table paths ld rd ours theirs ours-label theirs-label depth)))
          (setq paths (nth 0 dirs) directory-metadata (nth 1 dirs)
                directory-conflict (nth 2 dirs))
          ;; Reserve implicit destinations before choosing conflict backups.
          (dolist (path paths)
            (unless (gethash path names)
              (puthash path t names)
              (let ((start 0) slash)
                (while (setq slash (ngit--byte-pos path ?/ start))
                  (puthash (substring path 0 slash) t names)
                  (setq start (1+ slash)))))))))
    (dolist (path paths)
      (let* ((rec (gethash path table))
             (b (aref rec 1)) (o (aref rec 2)) (th (aref rec 3)))
        (cond
         ((eq (aref rec 7) t) nil)
         ((ngit-merge--same o th) (aset rec 4 o))
         ((ngit-merge--same b o) (aset rec 4 th))
         ((ngit-merge--same b th) (aset rec 4 o))
         ((and o th
               (/= (logand (aref o 2) #o170000) (logand (aref th 2) #o170000)))
          (if (> depth 0)
              (progn (aset rec 4 b) (aset rec 5 t))
            ;; Git keeps a nonregular side at PATH and moves the regular side.
            (let* ((rename-o (= (logand (aref o 2) #o170000) #o100000))
                   (rename-th (= (logand (aref th 2) #o170000) #o100000))
                   (both (not (or rename-o rename-th)))
                   (op (if (or rename-o both) (ngit-merge--unique path ours-label names) path))
                   (tp (if (or rename-th both) (ngit-merge--unique path theirs-label names) path))
                   (ob (and b (= (logand (aref b 2) #o170000)
                                (logand (aref o 2) #o170000)) b))
                   (tb (and b (= (logand (aref b 2) #o170000)
                                (logand (aref th 2) #o170000)) b))
                   (other (vector tp (ngit-merge--copy tb tp) nil
                                  (ngit-merge--copy th tp) (ngit-merge--copy th tp) t nil)))
              (aset rec 0 op) (aset rec 1 (ngit-merge--copy ob op))
              (aset rec 2 (ngit-merge--copy o op)) (aset rec 3 nil)
              (aset rec 4 (ngit-merge--copy o op)) (aset rec 5 t)
              (aset rec 6 (list (concat "CONFLICT (distinct types): " path
                                       " had different types on each side; renamed "
                                       (if both "both of them" "one of them")
                                       " so each can be recorded somewhere.")))
              (push other records))))
         ((and o th)
          (let* ((labels (aref rec 8))
                 (ngit-merge-ancestor-label (if labels (car labels) ngit-merge-ancestor-label))
                 (content (ngit-merge--content repo path b o th
                                             (if labels (nth 1 labels) ours-label)
                                             (if labels (nth 2 labels) theirs-label) depth))
                 (clean (nth 1 content)))
            (aset rec 4 (car content))
            (aset rec 5 (or (eq (aref rec 7) 'path-conflict) (not clean)))
            (aset rec 6 (nth 2 content))
            (unless clean
              (aset rec 6 (append (aref rec 6)
                                  (list (concat "CONFLICT (" (if b "content" "add/add")
                                                "): Merge conflict in " path)))))))
         ((or o th)
          (let ((modified (if o ours-label theirs-label))
                (deleted (if o theirs-label ours-label)))
            (aset rec 4 (if (> depth 0) b (or o th)))
            (aset rec 5 t)
            (aset rec 6 (list (concat "CONFLICT (modify/delete): " path " deleted in "
                                     deleted " and modified in " modified ".  Version "
                                     modified " of " path " left in tree."))))))
        (let ((directory (and directory-metadata (gethash path directory-metadata))))
          (when (car directory) (aset rec 5 t))
          (when directory (aset rec 6 (append (cdr directory) (aref rec 6)))))
        (push rec records)))
    ;; A surviving directory wins the original pathname.  Its competing file
    ;; moves to the same unique branch-labelled path merge-ort would choose.
    (setq records (sort records (lambda (a b) (string< (aref a 0) (aref b 0)))))
    (let ((surviving (make-hash-table :test 'equal)))
      (dolist (rec records)
        (when (aref rec 4)
          (let ((path (aref rec 0)) (start 0) slash)
            (while (setq slash (ngit--byte-pos path ?/ start))
              (puthash (substring path 0 slash) t surviving)
              (setq start (1+ slash))))))
      (dolist (rec records)
        (when (and (aref rec 4) (gethash (aref rec 0) surviving))
          (let* ((path (aref rec 0))
                 (label (if (aref rec 2) ours-label theirs-label))
                 (new (ngit-merge--unique path label names)))
            (aset rec 0 new)
            (let ((i 1))
              (while (<= i 4)
                (aset rec i (ngit-merge--copy (aref rec i) new))
                (setq i (1+ i))))
            (aset rec 5 t)
            (let ((directory-messages
                   (and directory-metadata (cdr (gethash path directory-metadata)))))
             (aset rec 6
                  (append directory-messages
                          (list (concat "CONFLICT (file/directory): directory in the way of "
                                        path " from " label "; moving it to " new " instead."))
                        (if (and (aref rec 1) (not (and (aref rec 2) (aref rec 3))))
                            (let ((modified (if (aref rec 2) ours-label theirs-label))
                                  (deleted (if (aref rec 2) theirs-label ours-label)))
                              (list (concat "CONFLICT (modify/delete): " new " deleted in "
                                            deleted " and modified in " modified ".  Version "
                                            modified " of " new " left in tree.")))
                          (nthcdr (length directory-messages) (aref rec 6))))))))))
    (setq records (sort records (lambda (a b) (string< (aref a 0) (aref b 0)))))
    (dolist (rec records)
      (let ((path (aref rec 0)) (result (aref rec 4)))
        (when result (push (ngit-merge--copy result path) worktree))
        (if (and (aref rec 5) (= depth 0))
            (progn
              (push path conflicts)
              (let ((stage 1))
                (while (<= stage 3)
                  (when (aref rec stage)
                    (push (ngit-merge--copy (aref rec stage) path stage) entries))
                  (setq stage (1+ stage)))))
          (when result (push (ngit-merge--copy result path) entries)))
        (when (= depth 0) (setq messages (append messages (aref rec 6))))))
    (setq entries (vconcat (sort entries #'ngit--entry-less))
          worktree (vconcat (sort worktree #'ngit--entry-less)))
    (let* ((root (ngit-ct-new)) (tree (ngit-ct-update repo root worktree)))
      (list :entries entries :worktree worktree :conflicts (nreverse conflicts)
            :conflicted (and (or conflicts directory-conflict) t)
            :messages messages :tree tree :cache-tree root))))

(defun ngit-merge--base (repo one two depth &optional labelled)
  "Compute a recursive ancestor tree for parsed commit vectors ONE and TWO.
With LABELLED, return (TREE . ANCESTOR-LABEL) for an internal content merge."
  (let* ((bases (reverse (ngit-merge-bases repo one (list two))))
         (merged (car bases))
         (label (cond ((null bases) "empty tree")
                      ((cdr bases) "merged common ancestors")
                      (t (ngit-abbrev repo (ngit--c-hex merged))))))
    (dolist (next (cdr bases))
      (let* ((ancestor (ngit-merge--base repo merged next (1+ depth) t))
             (ngit-merge-ancestor-label (cdr ancestor))
             (result (ngit-merge-trees repo (car ancestor)
                                       (ngit-unhex (ngit--c-tree merged))
                                       (ngit-unhex (ngit--c-tree next))
                                       "Temporary merge branch 1"
                                       "Temporary merge branch 2" (1+ depth)))
             ;; Virtual commits live only in memory; Git persists their trees.
             (parents (list merged next)))
        (setq merged (vector "virtual merge base" 0 parents 0
                             (ngit-hex (plist-get result :tree)) nil nil nil
                             "merged tree" nil t parents))))
    (let ((tree (when merged
                  (ngit-parse-commit-object repo merged)
                  (ngit-unhex (ngit--c-tree merged)))))
      (if labelled (cons tree label) tree))))

(defun ngit-merge-base-tree (repo ours theirs)
  "Return recursive binary merge-base tree for two hexadecimal commits."
  (let ((one (ngit-commit ours)) (two (ngit-commit theirs)))
    (ngit-parse-commit-object repo one)
    (ngit-parse-commit-object repo two)
    (ngit-merge--base repo one two 0)))

(defun ngit-merge--refuse (local untracked dirs)
  (when local
    (ngit-native-write-stderr
     (concat "error: Your local changes to the following files would be overwritten by merge:\n"
             (mapconcat (lambda (p) (concat "\t" p "\n"))
                        (sort (delete-dups local) #'string<) "")
             "Please commit your changes or stash them before you merge.\n")))
  (when untracked
    (ngit-native-write-stderr
     (concat "error: The following untracked working tree files would be overwritten by merge:\n"
             (mapconcat (lambda (p) (concat "\t" p "\n"))
                        (sort (delete-dups untracked) #'string<) "")
             "Please move or remove them before you merge.\n")))
  (when dirs
    (ngit-native-write-stderr
     (concat "error: Updating the following directories would lose untracked files in them:\n"
             (mapconcat (lambda (p) (concat "\t" p "\n"))
                        (sort (delete-dups dirs) #'string<) "") "\n")))
  (when (or local untracked dirs) (ngit--die-with 1 "Aborting")))

(defun ngit-merge--untracked-targets (repo writes old)
  "Find untracked paths that can overlap WRITES without walking unrelated dirs."
  (let ((items nil) (seen (make-hash-table :test 'equal)))
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (dolist (e writes)
      (let* ((path (aref e 0)) (target path) (start 0) (blocked nil) slash)
        (while (and (not blocked) (setq slash (ngit--byte-pos path ?/ start)))
          (let* ((prefix (substring path 0 slash)) (st (ngit-wt-lstat prefix)))
            ;; A file or symlink above the target also blocks checkout.
            ;; Never follow a symlink into content outside this target.
            (when (and st (/= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir))
              (setq target prefix blocked t)))
          (setq start (1+ slash)))
        (unless (gethash target seen) (puthash target t seen) (push target items))))
    (car (ngit-add-untracked repo items (ngit--tracked-set old)))))

(defun ngit-merge-apply (repo result ours-tree &optional quiet)
  "Safely install RESULT relative to OURS-TREE and return RESULT.
Refuse dirty and untracked paths before changing any work-tree path or the
index.  QUIET does not hide ort diagnostics for merge -q.  The symbol
`rebase' hides clean replay messages, matching the sequencer backend."
  (let* ((index (ngit-read-index repo)) (old (aref index 0))
         (image (plist-get result :worktree))
         (merged (plist-get result :entries))
         (sparse (plist-get result :sparse))
         (local nil) (collisions nil) (dirs nil) (writes nil) (deletes nil)
         (all-untracked nil) (i 0))
    ;; merge-ort's wrappers require the index to match the supplied head tree.
    (dolist (c (ngit-diff-tree-index repo ours-tree)) (push (aref c 0) local))
    (if sparse
        (dolist (path (plist-get result :paths))
          (let ((e (ngit-index-lookup old path)) (next (ngit-index-lookup image path)))
            (unless (ngit-merge--same e next)
              (when e
                (push e deletes)
                (unless (ngit--switch-worktree-clean-p repo index e) (push path local)))
              (when next (push next writes)))))
     (while (< i (length old))
      (let* ((e (aref old i)) (path (aref e 0))
             (next (ngit-index-lookup image path)))
        (unless (= (aref e 3) 0)
          (ngit--die-with 1 "error: you need to resolve your current index first"))
        (unless (ngit-merge--same e next)
          (push e deletes)
          (unless (ngit--switch-worktree-clean-p repo index e) (push path local))))
      (setq i (1+ i)))
     (setq i 0)
     (while (< i (length image))
      (let* ((e (aref image i)) (path (aref e 0)) (prev (ngit-index-lookup old path)))
        (unless (ngit-merge--same e prev)
          (push e writes)))
      (setq i (1+ i))))
    (when writes
      (setq all-untracked (ngit-merge--untracked-targets repo writes old))
      (dolist (e writes)
        (let* ((path (aref e 0)) (prefix (concat path "/")))
          (dolist (u all-untracked)
            (cond ((string-prefix-p prefix u) (push path dirs))
                  ((or (string= path u)
                       (string-prefix-p (concat (string-remove-suffix "/" u) "/") path))
                   (push u collisions)))))))
    (ngit-merge--refuse local collisions dirs)
    (let ((lock (ngit-lock (ngit-index-file repo)))
          (fresh (make-hash-table :test 'equal))
          (checked (make-hash-table :test 'eql))
          (root (or (plist-get result :cache-tree) (ngit-ct-new)))
          (final nil)
          (ngit--checkout-attr-direction 'index-first)
          (ngit--checkout-attr-cache (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            ;; Attribute resolution during checkout sees the complete target.
            (aset index 0 merged)
            (dolist (e deletes) (ngit--unlink-entry repo e))
            (ngit--switch-clear-target-directories repo (vconcat writes))
            (dolist (e writes)
              (puthash (aref e 0) (ngit-checkout-entry repo e) fresh))
            (setq i 0)
            (while (< i (length merged))
              (let* ((e (aref merged i)) (path (aref e 0))
                     (prev (unless sparse (ngit-index-lookup old path))))
                (push (if (= (aref e 3) 0)
                          (or (gethash path fresh)
                              (and (not sparse) (ngit-merge--same e prev) prev) e)
                        e) final))
              (setq i (1+ i)))
            (setq final (vconcat (nreverse final)))
            (unless (plist-get result :cache-tree) (ngit-ct-update repo root image))
            (dolist (path (plist-get result :conflicts)) (ngit-ct-invalidate root path))
            (ngit-commit-lock lock (ngit-index-bytes repo index final root checked))
            (setq lock nil)
            (aset repo 6 nil) (aset repo 7 nil))
        (when lock (ngit-unlock lock))))
    (ngit-ref-write repo "AUTO_MERGE" (ngit-hex (plist-get result :tree)))
    (unless (and (eq quiet 'rebase) (not (or (plist-get result :conflicted)
                                          (plist-get result :conflicts))))
      (dolist (msg (plist-get result :messages)) (ngit--emit msg "\n")))
    result))

(provide 'ngit-merge-tree)
;;; ngit-merge-tree.el ends here
