;;; ngit-changes.el --- tree, index and work tree comparisons -*- lexical-binding: t; -*-

;;; Commentary:

;; The comparisons behind status and diff.  A change is a vector
;; [PATH STATUS OLD-MODE OLD-BIN NEW-MODE NEW-BIN EXTRA DATA OLD-PATH]:
;; STATUS is one of ?A ?D ?M ?T ?R ?U, modes are integers (0 when absent),
;; BIN 20-byte names (nil for an absent side), EXTRA a list of submodule
;; flags (`new-commits' `modified' `untracked') or `ita', DATA the new
;; side's content when it came from the work tree, OLD-PATH the rename
;; source.  Lists of changes are in git's path order.
;; For ?U, EXTRA is the four-slot stage vector from `ngit-index-group'.
;;
;; HEAD versus index uses the index's cache-tree to skip unchanged
;; subtrees, as git's unpack_trees does; tree versus tree skips equal
;; subtree names.  Both matter: a full tree walk of a 20k-file tree takes
;; minutes in NeLisp.

;;; Code:

(require 'ngit-rev)
(require 'ngit-worktree)

(defun ngit-change (path status old-mode old-bin new-mode new-bin &optional extra data)
  (vector path status old-mode old-bin new-mode new-bin extra data nil))

(defconst ngit-mode-tree #o040000)
(defconst ngit-mode-gitlink #o160000)
(defconst ngit-mode-symlink #o120000)

(defun ngit-tree-items (repo bin)
  "Return the entries of tree BIN as a list of (MODE NAME BIN), MODE an integer."
  (let ((obj (ngit-read-object repo bin)))
    (unless (eq (car obj) 'tree) (error "fatal: %s is not a tree" (ngit-hex bin)))
    (mapcar (lambda (e) (list (string-to-number (nth 0 e) 8) (nth 1 e) (nth 2 e)))
            (ngit-parse-tree (cdr obj)))))

(defun ngit--flatten-tree (repo bin prefix fn)
  "Call FN with (PATH MODE BIN) for every non-tree entry below tree BIN."
  (dolist (e (ngit-tree-items repo bin))
    (let ((path (concat prefix (nth 1 e))))
      (if (= (nth 0 e) ngit-mode-tree)
          (ngit--flatten-tree repo (nth 2 e) (concat path "/") fn)
        (funcall fn path (nth 0 e) (nth 2 e))))))

(defun ngit--type-class (mode)
  (let ((fmt (logand mode #o170000)))
    (if (= fmt #o100000) 'file fmt)))

(defun ngit--pair (path old-mode old-bin new-mode new-bin)
  "Return the change between two present sides, or nil when equal."
  (cond ((and (equal old-bin new-bin) (= old-mode new-mode)) nil)
        ((not (eq (ngit--type-class old-mode) (ngit--type-class new-mode)))
         (ngit-change path ?T old-mode old-bin new-mode new-bin))
        (t (ngit-change path ?M old-mode old-bin new-mode new-bin))))

;;;; Tree versus tree

(defun ngit-diff-trees (repo a b &optional prefix)
  "Return the changes from tree A to tree B (20-byte names, nil = empty)."
  (let ((out nil) (prefix (or prefix "")))
    (ngit--diff-trees repo a b prefix (lambda (c) (push c out)))
    (nreverse out)))

(defun ngit--tree-key (e)
  (if (= (nth 0 e) ngit-mode-tree) (concat (nth 1 e) "/") (nth 1 e)))

(defun ngit--diff-trees (repo a b prefix emit)
  (unless (equal a b)
    (let ((as (and a (ngit-tree-items repo a)))
          (bs (and b (ngit-tree-items repo b))))
      (while (or as bs)
        (let* ((ea (car as)) (eb (car bs))
               (ka (and ea (ngit--tree-key ea))) (kb (and eb (ngit--tree-key eb))))
          (cond
           ((and ea eb (string= ka kb))
            (if (= (nth 0 ea) ngit-mode-tree)
                (ngit--diff-trees repo (nth 2 ea) (nth 2 eb) (concat prefix ka) emit)
              (let ((c (ngit--pair (concat prefix ka) (nth 0 ea) (nth 2 ea)
                                   (nth 0 eb) (nth 2 eb))))
                (when c (funcall emit c))))
            (setq as (cdr as) bs (cdr bs)))
           ((or (null eb) (and ea (string< ka kb)))
            (ngit--emit-side repo ea prefix ?D emit)
            (setq as (cdr as)))
           (t
            (ngit--emit-side repo eb prefix ?A emit)
            (setq bs (cdr bs)))))))))

(defun ngit--emit-side (repo e prefix status emit)
  "Emit E (and everything below it) as added or deleted."
  (let ((one (lambda (path mode bin)
               (funcall emit (if (= status ?A)
                                 (ngit-change path ?A 0 nil mode bin)
                               (ngit-change path ?D mode bin 0 nil))))))
    (if (= (nth 0 e) ngit-mode-tree)
        (ngit--flatten-tree repo (nth 2 e) (concat prefix (nth 1 e) "/") one)
      (funcall one (concat prefix (nth 1 e)) (nth 0 e) (nth 2 e)))))

;;;; Tree versus index

(defun ngit--index-live-p (e)
  (and (= (aref e 3) 0) (not (memq 'ita (aref e 5)))))

(defun ngit-diff-tree-index (repo tree)
  "Return the changes from tree TREE (or nil) to REPO's index."
  (let* ((index (ngit-read-index repo))
         (entries (aref index 0))
         (out nil)
         (emit (lambda (c) (push c out))))
    (ngit--diff-tree-index repo tree "" entries 0 (length entries)
                           (ngit-index-cache-tree index) emit)
    (nreverse out)))

(defun ngit--emit-index-added (e emit)
  (when (ngit--index-live-p e)
    (funcall emit (ngit-change (aref e 0) ?A 0 nil (aref e 2) (aref e 1)))))

(defun ngit--emit-index-next (entries i emit &optional old-mode old-bin)
  "Emit the added or unmerged path at I; return the next entry position."
  (let ((e (aref entries i)))
    (if (= (aref e 3) 0)
        (progn (ngit--emit-index-added e emit) (1+ i))
      (let ((group (ngit-index-group entries i)))
        (funcall emit (ngit-change (aref e 0) ?U (or old-mode 0) old-bin 0 nil (cdr group)))
        (car group)))))

(defun ngit--diff-tree-index (repo tree prefix entries lo hi cache-tree emit)
  (let ((ct (and cache-tree tree (gethash prefix cache-tree))))
    (unless (and ct (cdr ct) (= (car ct) (- hi lo)) (equal (cdr ct) tree))
      (let ((items (and tree (ngit-tree-items repo tree)))
            (i lo))
        (dolist (te items)
          (let* ((isdir (= (nth 0 te) ngit-mode-tree))
                 (name (concat prefix (nth 1 te)))
                 (key (if isdir (concat name "/") name)))
            (while (and (< i hi) (string< (aref (aref entries i) 0) key))
              (setq i (ngit--emit-index-next entries i emit)))
            (if isdir
                (let ((sub-hi (min hi (ngit-index-find entries (concat name "0")))))
                  (if (> sub-hi i)
                      (ngit--diff-tree-index repo (nth 2 te) key entries i sub-hi
                                             cache-tree emit)
                    (ngit--emit-side repo te prefix ?D emit))
                  (setq i (max i sub-hi)))
              (let ((e (and (< i hi) (aref entries i))))
                (cond
                 ((and e (string= (aref e 0) key) (> (aref e 3) 0))
                  (setq i (ngit--emit-index-next entries i emit (nth 0 te) (nth 2 te))))
                 ((and e (string= (aref e 0) key) (ngit--index-live-p e))
                    (let ((c (ngit--pair key (nth 0 te) (nth 2 te) (aref e 2) (aref e 1))))
                      (when c (funcall emit c))
                      (setq i (1+ i))))
                 (t
                  (when (and e (string= (aref e 0) key)) (setq i (1+ i)))
                  (ngit--emit-side repo te prefix ?D emit)))))))
        (while (< i hi)
          (setq i (ngit--emit-index-next entries i emit)))))))

;;;; Index versus work tree

(defun ngit--stat-clean-p (index e st)
  "Non-nil when the cached stat data of E matches lstat ST and is not racy.
Compares mtime, ctime, size and inode (a file replaced within the same
second keeps the first three)."
  (let* ((raw (aref index 1)) (pos (aref e 4))
         (mtime (logior (ash (aref raw (+ pos 8)) 24) (ash (aref raw (+ pos 9)) 16)
                        (ash (aref raw (+ pos 10)) 8) (aref raw (+ pos 11)))))
    (and (= mtime (aref st 3))
         (< mtime (aref index 4))
         (= (logior (ash (aref raw (+ pos 36)) 24) (ash (aref raw (+ pos 37)) 16)
                    (ash (aref raw (+ pos 38)) 8) (aref raw (+ pos 39)))
            (logand (aref st 2) #xffffffff))
         (= (logior (ash (aref raw pos) 24) (ash (aref raw (+ pos 1)) 16)
                    (ash (aref raw (+ pos 2)) 8) (aref raw (+ pos 3)))
            (aref st 5))
         (= (logior (ash (aref raw (+ pos 20)) 24) (ash (aref raw (+ pos 21)) 16)
                    (ash (aref raw (+ pos 22)) 8) (aref raw (+ pos 23)))
            (logand (aref st 1) #xffffffff)))))

(defun ngit-submodule-state (repo path index-bin)
  "Return (HEAD-BIN . FLAGS) for the submodule checked out at PATH."
  (let* ((top (ngit-repo-toplevel repo))
         (dir (expand-file-name (decode-coding-string path 'utf-8) top)))
    (if (not (file-exists-p (expand-file-name ".git" dir)))
        (cons index-bin nil)
      (let* ((sub (ngit-discover dir))
             (head (ngit-resolve-ref sub "HEAD"))
             (head-bin (and head (ngit-unhex head)))
             (flags nil))
        (unless (equal head-bin index-bin) (push 'new-commits flags))
        (let ((dirty (unwind-protect (ngit-repo-dirtiness sub)
                       (ngit-wt-setup top))))
          (when (memq 'untracked dirty) (push 'untracked flags))
          (when (memq 'modified dirty) (push 'modified flags)))
        (cons (or head-bin index-bin) flags)))))

(defun ngit-wt-changes (repo &optional want-data size-shortcut selected)
  "Return the changes from REPO's index to its work tree.
With WANT-DATA, changed regular files and symlinks carry their content.
With SIZE-SHORTCUT (git's refresh logic, read-cache.c ie_modified), a
size that differs from a nonzero cached size counts as modified without
reading the file, even when end-of-line conversion would make the
contents equal; diff compares contents instead.  SELECTED, when non-nil,
is a vector of entries still requiring comparison after a refresh."
  (let* ((index (ngit-read-index repo))
         (entries (or selected (aref index 0)))
         (filemode (ngit-config-bool repo "core.filemode" t))
         (raw (aref index 1))
         ;; Version 2/3 names are NUL-terminated in place (see ngit-index).
         (native (and (/= (aref index 2) 4) (aref index 6)))
         (out nil) (k 0) (n (length entries)))
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (while (< k n)
      (let* ((e (aref entries k))
             (name (aref e 0))
             (mode (aref e 2)))
        (if (> (aref e 3) 0)
            (let* ((group (ngit-index-group entries k))
                   (st (ngit-wt-lstat name))
                   (wmode (if st (ngit-wt-mode st) 0))
                   (data (and want-data st (/= wmode ngit-mode-tree)
                              (/= wmode ngit-mode-gitlink) (ngit-wt-content repo name st))))
              (when (= wmode ngit-mode-tree)
                (setq wmode (if (= mode ngit-mode-gitlink) ngit-mode-gitlink 0)))
              (when (and (not filemode) (= (logand wmode #o170000) #o100000)
                         (= (logand mode #o170000) #o100000))
                (setq wmode mode))
              (push (ngit-change name ?U 0 nil wmode nil (cdr group) data) out)
              (setq k (1- (car group))))
          (when (not (memq 'skip-worktree (aref e 5)))
          (let ((st (if native
                        (let ((pos (aref e 4)))
                          (ngit-wt-lstat-at
                           (+ native pos (if (= 0 (logand (aref raw (+ pos 60)) #x40)) 62 64))))
                      (ngit-wt-lstat name))))
            (cond
             ((null st)
              (push (ngit-change name ?D mode (aref e 1) 0 nil) out))
             ((= mode ngit-mode-gitlink)
              (when (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir)
                (let ((state (ngit-submodule-state repo name (aref e 1))))
                  (when (cdr state)
                    (push (ngit-change name ?M mode (aref e 1) mode (car state) (cdr state))
                          out)))))
             (t
              (let ((wmode (ngit-wt-mode st)))
                (when (and (not filemode) (= (logand wmode #o170000) #o100000)
                           (= (logand mode #o170000) #o100000))
                  (setq wmode mode))
                (cond
                 ((= wmode ngit-mode-tree)
                  (push (ngit-change name ?D mode (aref e 1) 0 nil) out))
                 ((memq 'ita (aref e 5))
                  (let ((data (ngit-wt-content repo name st)))
                    (push (ngit-change name ?A 0 nil wmode (ngit-hash-blob data) 'ita
                                       (and want-data data))
                          out)))
                 ((and (= wmode mode) (ngit--stat-clean-p index e st)))
                 ((and size-shortcut (= wmode mode)
                       (let ((cached (aref (ngit-index-stat index e) 4)))
                         (and (/= cached 0) (/= cached (logand (aref st 2) #xffffffff)))))
                  (push (ngit-change name ?M mode (aref e 1) wmode nil) out))
                 (t
                  (let* ((data (ngit-wt-content repo name st))
                         (bin (ngit-hash-blob data)))
                    (unless (and (equal bin (aref e 1)) (= wmode mode))
                      (push (ngit-change name
                                         (if (eq (ngit--type-class wmode) (ngit--type-class mode)) ?M ?T)
                                         mode (aref e 1) wmode bin nil (and want-data data))
                            out))))))))))))
      (setq k (1+ k)))
    (nreverse out)))

;;;; Untracked files

(defun ngit--dir-names (abs)
  "Return the entry names of directory ABS as unibyte strings."
  (let ((names nil))
    (dolist (n (directory-files abs nil nil t))
      (unless (member n '("." ".." ".git"))
        (push (if (multibyte-string-p n) (encode-coding-string n 'utf-8) n) names)))
    names))

(defun ngit-untracked (repo &optional stop-at-first)
  "Return the untracked paths of REPO (directories as \"dir/\"), sorted.
With STOP-AT-FIRST, return as soon as one is found."
  (let* ((top (file-name-as-directory (ngit-repo-toplevel repo)))
         (entries (ngit-index-entries repo))
         (global (ngit-ignore-global-patterns repo))
         (tracked (ngit--tracked-set entries))
         (out nil))
    (ngit-wt-setup top)
    (catch 'found
      (ngit--walk-untracked repo top tracked "" global
                            (lambda (p)
                              (push p out)
                              (when stop-at-first (throw 'found nil)))
                            nil))
    (sort out #'string<)))

(defun ngit--tracked-set (entries)
  "Return a hash table of the tracked paths in ENTRIES."
  (let ((set (make-hash-table :test 'equal :size (* 2 (length entries))))
        (i 0) (n (length entries)))
    (while (< i n)
      (puthash (aref (aref entries i) 0) t set)
      (setq i (1+ i)))
    set))

(defvar ngit--no-excludes nil "Non-nil to ignore .gitignore rules (add -f).")

(defun ngit--walk-untracked (repo top tracked dir stack emit check-only &optional expand)
  ;; TRACKED is `ngit--tracked-set'; directories holding tracked files
  ;; are found by binary search, which is cheaper than precomputing them.
  "Report untracked paths under DIR (\"\" or \"a/b/\").  STACK holds the
ignore pattern lists that apply.  In CHECK-ONLY mode, throw `found' at
the first untracked path instead of emitting.  With EXPAND, report the
files of untracked directories instead of the directories; nested
repositories are still reported as \"dir/\"."
  ;; TOP ends in a slash; `expand-file-name' costs ~2.5 ms in NeLisp.
  (let* ((abs (concat top (decode-coding-string dir 'utf-8)))
         (own (ngit-read-ignore-file (concat (file-name-as-directory abs) ".gitignore") dir))
         (stack (if own (cons (ngit-compile-ignore own) stack) stack))
         (blen (length dir)))
    (dolist (name (ngit--dir-names abs))
      (let ((path (concat dir name)))
        (unless (eq (gethash path tracked) t)
          (let* ((st (ngit-wt-lstat path))
                 (isdir (and st (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir))))
            (when (and st (or ngit--no-excludes (not (ngit-excluded-p stack path blen isdir))))
              (cond
               ((not isdir)
                (if check-only (throw 'found t) (funcall emit path)))
               ((ngit-index-dir-p (ngit-index-entries repo) (concat path "/"))
                (ngit--walk-untracked repo top tracked (concat path "/") stack emit check-only
                                      expand))
               ((and expand (not (ngit-wt-lstat (concat path "/.git"))))
                (ngit--walk-untracked repo top tracked (concat path "/") stack emit nil t))
               ((or (ngit-wt-lstat (concat path "/.git"))
                    (catch 'found
                      (ngit--walk-untracked repo top tracked (concat path "/") stack nil t)
                      nil))
                (if check-only (throw 'found t) (funcall emit (concat path "/"))))))))))))

;;;; Whole-repository dirtiness (for submodules)

(defun ngit-repo-dirtiness (repo)
  "Return a list with `modified' and/or `untracked' for REPO's work tree."
  (let ((flags nil)
        (head (ngit-resolve-ref repo "HEAD")))
    (when (or (ngit-diff-tree-index repo (and head (ngit-unhex (ngit-peel repo head 'tree))))
              (ngit-wt-changes repo nil t))
      (push 'modified flags))
    (when (ngit-untracked repo t) (push 'untracked flags))
    flags))

;;;; Rename detection (exact)

(defun ngit--basename (path)
  (let ((i (1- (length path))))
    (while (and (>= i 0) (/= (aref path i) ?/)) (setq i (1- i)))
    (substring path (1+ i))))

(defun ngit-detect-renames (changes)
  "Pair deleted and added entries with identical content as renames."
  (let ((sources (make-hash-table :test 'equal))
        (renamed (make-hash-table :test 'eq))
        (has-add nil))
    (dolist (c changes)
      (cond ((= (aref c 1) ?D) (push c (gethash (aref c 3) sources)))
            ((= (aref c 1) ?A) (setq has-add t))))
    (if (or (not has-add) (= (hash-table-count sources) 0))
        changes
      (let ((out nil))
        (dolist (c changes)
          (if (/= (aref c 1) ?A)
              (push c out)
            (let ((best nil) (best-score -1))
              (dolist (s (reverse (gethash (aref c 5) sources)))
                (when (eq (= (logand (aref s 2) #o170000) #o100000)
                          (= (logand (aref c 4) #o170000) #o100000))
                  (let ((score (+ (if (gethash s renamed) 0 2)
                                  (if (string= (ngit--basename (aref s 0))
                                               (ngit--basename (aref c 0)))
                                      1 0))))
                    (when (> score best-score) (setq best s best-score score)))))
              (if (null best)
                  (push c out)
                (puthash best t renamed)
                (let ((r (copy-sequence c)))
                  (aset r 1 ?R) (aset r 2 (aref best 2)) (aset r 3 (aref best 3))
                  (aset r 8 (aref best 0))
                  (push r out))))))
        (delq nil (mapcar (lambda (c) (unless (gethash c renamed) c)) (nreverse out)))))))

(provide 'ngit-changes)

;;; ngit-changes.el ends here
