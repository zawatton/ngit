;;; ngit-add.el --- git add -*- lexical-binding: t; -*-

;;; Commentary:

;; git add with literal pathspecs (files, directories, ".").  As in git
;; 2.x, a pathspec stages new, modified and deleted paths below it; -u
;; limits that to tracked paths and -A/-u without pathspecs cover the
;; whole tree.  Tracked entries whose stat data no longer match (or that
;; are racily clean) are rehashed and replaced, which invalidates their
;; cache-tree path even when the content is unchanged, as in git.
;;
;; The walk over tracked entries is shared with commit -a through
;; `ngit-add-tracked'.

;;; Code:

(require 'ngit-write)

(defun ngit--pathspec-item (repo arg)
  "Return ARG as a unibyte path relative to the top (\"\" for the top)."
  (let* ((top (file-name-as-directory (ngit-repo-toplevel repo)))
         (abs (directory-file-name (expand-file-name arg default-directory)))
         ;; NeLisp has no `file-relative-name'.
         (rel (cond ((string= (file-name-as-directory abs) top) "")
                    ((string-prefix-p top abs) (substring abs (length top)))
                    (t "../"))))
    (when (string-prefix-p "../" rel)
      (ngit--die "fatal: %s: '%s' is outside repository at '%s'"
                 arg arg (directory-file-name top)))
    (encode-coding-string rel 'utf-8)))

(defun ngit--item-covers-p (item path)
  (or (string= item "") (string= item path)
      (and (> (length path) (length item))
           (= (aref path (length item)) ?/)
           (string-prefix-p item path))))

(defun ngit--matching-items (items path)
  (let ((found nil))
    (dolist (item items)
      (when (ngit--item-covers-p item path) (push item found)))
    found))

(defun ngit--gitlink-head (repo path)
  "Return the HEAD commit of the repository checked out at PATH, or nil."
  (let ((dir (concat (file-name-as-directory (ngit-repo-toplevel repo))
                     (decode-coding-string path 'utf-8))))
    (and (file-exists-p (concat dir "/.git"))
         (let ((head (ngit-resolve-ref (ngit-discover dir) "HEAD")))
           (and head (ngit-unhex head))))))

(defun ngit--new-entry (repo path st filemode old-mode dry-run)
  "Hash work tree PATH (lstat ST) and return a new entry for it."
  (let ((mode (ngit-wt-mode st)))
    (when (and (not filemode) old-mode (= (logand mode #o170000) #o100000)
               (= (logand old-mode #o170000) #o100000))
      (setq mode old-mode))
    (let* ((block (ngit-wt-stat-block mode))
           (bin (ngit-write-object repo "blob" (ngit-wt-content repo path st) dry-run)))
      (ngit-make-entry path bin mode block))))

(defun ngit--intent-entry (repo path mode dry-run)
  "Return an intent-to-add entry, creating the empty blob as git does."
  (ngit-make-entry path (ngit-write-object repo "blob" "" dry-run) mode
                   (concat (make-string 24 0) (ngit--be32-bytes mode) (make-string 12 0))
                   '(ita)))

(defun ngit-add-tracked (repo index entries items root checked log dry-run &optional intent)
  "Stage changes to the tracked ENTRIES (a vector, updated in place)
covered by ITEMS: removed paths become nil, changed ones new entries.
Invalidate ROOT along changed paths and put clean entries in CHECKED.
Call LOG with \"add\" or \"remove\" and the path for each visible change.
Return the list of items that matched an entry."
  (let* ((filemode (ngit-config-bool repo "core.filemode" t))
         (all (member "" items))
         (matched nil) (k 0) (n (length entries)))
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (while (< k n)
      (let* ((e (aref entries k))
             (name (aref e 0))
             (covering (or all (ngit--matching-items items name))))
        (when (and covering (not (memq 'skip-worktree (aref e 5))))
          (when (> (aref e 3) 0)
            (let* ((group (ngit-index-group entries k)) (j (1+ k))
                   (ours (aref (cdr group) 2)))
              ;; Git prefers stage 2's mode, then 1, then 3.
              (when ours (setq e ours))
              (while (< j (car group))
                (aset entries j nil)
                (setq j (1+ j)))))
          (unless all
            (dolist (item covering)
              (unless (member item matched) (push item matched))))
          (let* ((mode (aref e 2))
                 (st (ngit-entry-lstat index e))
                 (fmt (and st (logand (aref st 0) ngit-s-ifmt))))
            (cond
             ;; Fast path for the common clean entry: no flags, stage 0,
             ;; same mode, matching stat data.
             ((and st (null (aref e 5)) (= (aref e 3) 0) filemode
                   (= mode (ngit-wt-mode st)) (ngit--stat-clean-p index e st))
              (puthash (aref e 4) t checked))
             ((or (null st)
                  (and (= fmt ngit-s-ifdir) (/= mode ngit-mode-gitlink)))
              (aset entries k nil)
              (when root (ngit-ct-invalidate root name))
              (funcall log "remove" name))
             ((and (> (aref e 3) 0) (= mode ngit-mode-gitlink)
                   (= fmt ngit-s-ifdir) (null (ngit--gitlink-head repo name)))
              (ngit--die "error: '%s' does not have a commit checked out\nfatal: updating files failed"
                         name))
             ((and intent (> (aref e 3) 0))
              (let ((wmode (if (= fmt ngit-s-ifdir) ngit-mode-gitlink (ngit-wt-mode st))))
                (when (and (not filemode) (= (logand wmode #o170000) #o100000)
                           (= (logand mode #o170000) #o100000))
                  (setq wmode mode))
                (aset entries k (ngit--intent-entry repo name wmode dry-run)))
              (when root (ngit-ct-invalidate root name))
              (funcall log "add" name))
             ((and (= mode ngit-mode-gitlink) (= fmt ngit-s-ifdir))
              (let ((head (ngit--gitlink-head repo name)))
                (if (and (= (aref e 3) 0) (or (null head) (equal head (aref e 1))))
                    (puthash (aref e 4) t checked)
                  (aset entries k (ngit-make-entry name (or head (aref e 1)) mode
                                                 (ngit-wt-stat-block mode)))
                  (when root (ngit-ct-invalidate root name))
                  (funcall log "add" name))))
             ((and (not (memq 'ita (aref e 5))) (= (aref e 3) 0)
                   (let ((wmode (ngit-wt-mode st)))
                     (when (and (not filemode) (= (logand wmode #o170000) #o100000)
                                (= (logand mode #o170000) #o100000))
                       (setq wmode mode))
                     (= wmode mode))
                   (ngit--stat-clean-p index e st))
              (puthash (aref e 4) t checked))
             (t
              (let ((new (ngit--new-entry repo name st filemode mode dry-run)))
                (aset entries k new)
                (when root (ngit-ct-invalidate root name))
                (unless (and (equal (aref new 1) (aref e 1)) (= (aref new 2) mode)
                             (= (aref e 3) 0) (not (memq 'ita (aref e 5))))
                  (funcall log "add" name))))))))
      (setq k (1+ k))
      (while (and (< k n) (null (aref entries k))) (setq k (1+ k))))
    (if (and all (> n 0)) '("") matched)))

(defun ngit--ignore-context (repo dir)
  "Return (STACK . IGNORED) for paths in DIR (\"\" or \"a/b/\"): the
ignore lists of the top and of every directory down to DIR inclusive,
and the first of those directories that is itself ignored, if any."
  (let* ((top (file-name-as-directory (ngit-repo-toplevel repo)))
         (stack (ngit-ignore-global-patterns repo))
         (own (ngit-read-ignore-file (concat top ".gitignore") "" t))
         (start 0) (ignored nil))
    (when own (push (ngit-compile-ignore own) stack))
    (while (and (not ignored) (< start (length dir)))
      (let* ((slash (ngit--byte-pos dir ?/ start))
             (sub (substring dir 0 slash)))
        (if (and (not ngit--no-excludes) (ngit-excluded-p stack sub start t))
            (setq ignored sub)
          (setq own (ngit-read-ignore-file
                     (concat top (decode-coding-string sub 'utf-8) "/.gitignore")
                     (concat sub "/") t))
          (when own (push (ngit-compile-ignore own) stack)))
        (setq start (1+ slash))))
    (cons stack ignored)))

(defun ngit-add-untracked (repo items tracked)
  "Return (NEW . IGNORED): the untracked paths below ITEMS, files and
nested repositories (as \"dir/\"), and the ignored paths that ITEMS
name.  Items that matched something are pushed onto the third element."
  (let* ((top (file-name-as-directory (ngit-repo-toplevel repo)))
         (entries (ngit-index-entries repo))
         (new nil) (ignored nil) (matched nil))
    (ngit-wt-setup top)
    (dolist (item items)
      (let ((emit (lambda (p) (push p new) (push item matched))))
        (if (string= item "")
            (ngit--walk-untracked repo top tracked "" (ngit-ignore-global-patterns repo)
                                  emit nil t)
          (let* ((st (ngit-wt-lstat item))
                 (isdir (and st (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir)))
                 (slash (let ((i (1- (length item))))
                          (while (and (>= i 0) (/= (aref item i) ?/)) (setq i (1- i)))
                          i))
                 (ctx (ngit--ignore-context repo (substring item 0 (1+ slash)))))
            (cond
             ((or (null st) (eq (gethash item tracked) t)))
             ((cdr ctx) (push (cdr ctx) ignored) (push item matched))
             ((and (not ngit--no-excludes)
                   (ngit-excluded-p (car ctx) item (1+ slash) isdir))
              (push item ignored) (push item matched))
             ((not isdir) (funcall emit item))
             ((and (not (ngit-index-dir-p entries (concat item "/")))
                   (ngit-wt-gitdir-lstat item))
              (funcall emit (concat item "/")))
             (t
              (ngit--walk-untracked repo top tracked (concat item "/") (car ctx)
                                    emit nil t)))))))
    (list (sort (delete-dups new) #'string<) (delete-dups (nreverse ignored)) matched)))

(defun ngit--check-pathspecs (repo items)
  "Die before any object is written when an item names neither a work
tree path nor an index entry, as git does."
  (let ((entries (ngit-index-entries repo)))
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (dolist (item items)
      (unless (or (string= item "") (ngit-wt-lstat item)
                  (ngit-index-lookup entries item)
                  (ngit-index-dir-p entries (concat item "/")))
        (ngit--die "fatal: pathspec '%s' did not match any files"
                   (decode-coding-string item 'utf-8))))))

(defun ngit--merge-entries (old new)
  "Merge vector OLD (nil slots dropped) with sorted list NEW."
  (let ((out nil) (i 0) (n (length old)))
    (while (or (< i n) new)
      (let ((a (and (< i n) (aref old i))))
        (cond ((and (< i n) (null a)) (setq i (1+ i)))
              ((and a (or (null new) (not (ngit--entry-less (car new) a))))
               (push a out) (setq i (1+ i)))
              (t (push (pop new) out)))))
    (vconcat (nreverse out))))

(defun ngit-cmd-add (repo args)
  (let ((all nil) (update nil) (verbose nil) (dry-run nil) (force nil) (ita nil)
        (paths nil))
    (while args
      (let ((a (pop args)))
        (cond ((member a '("-A" "--all")) (setq all t))
              ((member a '("-u" "--update")) (setq update t))
              ((member a '("-v" "--verbose")) (setq verbose t))
              ((member a '("-n" "--dry-run")) (setq dry-run t))
              ((member a '("-f" "--force")) (setq force t))
              ((member a '("-N" "--intent-to-add")) (setq ita t))
              ((string= a "--") (setq paths (append (reverse args) paths) args nil))
              ((string-prefix-p "-" a) (ngit--die "ngit: add: unsupported option %s" a))
              (t (push a paths)))))
    (setq paths (nreverse paths))
    (if (and (null paths) (not all) (not update))
        (progn
          (ngit-native-write-stderr
           "Nothing specified, nothing added.\nhint: Maybe you wanted to say 'git add .'?\nhint: Disable this message with \"git config set advice.addEmptyPathspec false\"\n")
          0)
      (let* ((ngit--no-excludes force)
             (items (if paths (ngit-pathspec-write-items repo paths) '("")))
             (index (ngit-read-index repo))
             (entries (copy-sequence (aref index 0)))
             (root (ngit-ct-root index))
             (checked (make-hash-table :test 'eql))
             (filemode (ngit-config-bool repo "core.filemode" t))
             (log (lambda (what path)
                    (when (or verbose dry-run)
                      (ngit--emit what " '" (ngit-quote-path path) "'\n"))))
             (_ (ngit--check-pathspecs repo items))
             (matched (ngit-add-tracked repo index entries items root checked log dry-run ita))
             (new-entries nil) (ignored nil))
        (unless update
          (let ((r (ngit-add-untracked repo items (ngit--tracked-set (aref index 0)))))
            (setq ignored (nth 1 r) matched (append (nth 2 r) matched))
            (dolist (p (car r))
              (let* ((nested (= (aref p (1- (length p))) ?/))
                     (name (if nested (substring p 0 -1) p))
                     (st (ngit-wt-lstat name)))
                (if nested
                    (let ((head (ngit--gitlink-head repo name)))
                      (unless head
                        (ngit-native-write-stderr
                         (format "error: '%s' does not have a commit checked out\n" p))
                        (ngit--die "fatal: adding files failed"))
                      (ngit-native-write-stderr
                       (format "warning: adding embedded git repository: %s\n" name))
                      (push (ngit-make-entry name head ngit-mode-gitlink
                                             (ngit-wt-stat-block ngit-mode-gitlink))
                            new-entries))
                  ;; Intent-to-add entries carry no stat data but the mode.
                  (push (if ita
                            (ngit--intent-entry repo name (ngit-wt-mode st) dry-run)
                          (ngit--new-entry repo name st filemode nil dry-run))
                        new-entries))
                (when root (ngit-ct-invalidate root name))
                (funcall log "add" p)))))
        (dolist (item items)
          (unless (member item matched)
            (ngit--die "fatal: pathspec '%s' did not match any files"
                       (decode-coding-string item 'utf-8))))
        (unless dry-run
          (ngit-write-index repo index
                            (ngit--merge-entries entries (sort new-entries #'ngit--entry-less))
                            root checked))
        (if (null ignored)
            0
          (ngit--flush)
          (ngit-native-write-stderr
           (concat "The following paths are ignored by one of your .gitignore files:\n"
                   (mapconcat (lambda (p) (concat (decode-coding-string p 'utf-8) "\n")) ignored "")
                   "hint: Use -f if you really want to add them.\n"
                   "hint: Disable this message with \"git config advice.addIgnoredFile false\"\n"))
          1)))))

(provide 'ngit-add)

;;; ngit-add.el ends here
