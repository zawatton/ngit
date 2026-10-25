;;; ngit-rm.el --- remove and move tracked paths -*- lexical-binding: t; -*-

;;; Commentary:

;; `rm' and `mv' operate on selected index ranges.  Unchanged index runs
;; remain shared through `ngit-index-overlay', which matters for large
;; repositories and also keeps the original stat blocks available.

;;; Code:

(require 'ngit-command-paths)
(require 'ngit-checkout)

(defun ngit-rm--path-match-p (item path)
  (let ((ngit--rv-paths (list item)))
    (eq (ngit-command-path-class (list item) path nil) 'full)))

(defun ngit-rm--exact-item-p (item path)
  "Whether ITEM matched PATH exactly, rather than through wildcards."
  (let* ((compiled (car item))
         (pattern (if (vectorp compiled) (aref compiled 0) compiled)))
    (if (and (vectorp compiled) (aref compiled 3))
        (let ((prefix (aref compiled 7)))
          (and (= (length pattern) (length path))
               (string= (substring pattern 0 prefix) (substring path 0 prefix))
               (string= (downcase (substring pattern prefix))
                        (downcase (substring path prefix)))))
      (string= pattern path))))

(defun ngit-rm--decode (path)
  (decode-coding-string path 'utf-8))

(defun ngit-rm--worktree-matches-p (repo entry path st)
  "Non-nil when PATH at ST has ENTRY's indexed mode and contents."
  (let* ((mode (aref entry 2))
         (wmode (ngit-wt-mode st))
         (filemode (ngit-config-bool repo "core.filemode" t))
         (symlinks (ngit-config-bool repo "core.symlinks" t)))
    (when (or (and (not filemode)
                   (= (logand mode #o170000) #o100000)
                   (= (logand wmode #o170000) #o100000))
              (and (not symlinks) (= mode ngit-mode-symlink)
                   (= (logand wmode #o170000) #o100000))
              (and (= mode ngit-mode-gitlink)
                   (= (logand wmode #o170000) ngit-s-ifdir)))
      (setq wmode mode))
    (and (= mode wmode)
         (if (= mode ngit-mode-gitlink)
             (let ((head (ngit--gitlink-head repo path)))
               (or (null head) (equal head (aref entry 1))))
           (equal (ngit-hash-blob (ngit-wt-content repo path st))
                  (aref entry 1))))))

(defun ngit-rm--head-entry (repo tree path)
  "Return TREE's entry for PATH without expanding unrelated subtrees."
  (and tree (ngit--literal-entry repo tree path)))

(defun ngit-rm--print-files (files singular plural hint)
  (when files
    (ngit-native-write-stderr
     (concat "error: " (if (cdr files) plural singular)
             (mapconcat (lambda (path) (concat "\n    " (ngit-rm--decode path)))
                        (nreverse files) "")
             hint "\n"))))

(defun ngit-rm--remove-worktree-path (repo entry)
  "Remove ENTRY's work-tree path and empty parents; return nil on failure."
  (let* ((path (aref entry 0))
         (top (ngit-repo-toplevel repo))
         (st (ngit-wt-lstat path))
         (ok t))
    (when st
      ;; remove_path uses unlink for an ordinary indexed entry, so a tracked
      ;; file replaced by a directory fails even when that directory is empty.
      (if (and (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir)
               (/= (aref entry 2) ngit-mode-gitlink))
          (setq ok nil)
        (condition-case nil
            (ngit--checkout-delete-path repo path st)
          (file-error (setq ok nil)))))
    (when ok
      (let ((slash (let ((i (1- (length path))))
                     (while (and (>= i 0) (/= (aref path i) ?/))
                       (setq i (1- i)))
                     i)))
        (while (>= slash 0)
          (condition-case nil
              (progn
                (delete-directory
                 (expand-file-name (ngit-rm--decode (substring path 0 slash)) top))
                (setq slash
                      (let ((i (1- slash)))
                        (while (and (>= i 0) (/= (aref path i) ?/))
                          (setq i (1- i)))
                        i)))
            (file-error (setq slash -1))))))
    ok))

(defun ngit-rm--check-local (repo index entries cached)
  "Return non-nil after printing any unsafe removals in ENTRIES."
  (let* ((head (ngit-resolve-ref repo "HEAD"))
         (tree (and head (condition-case nil
                             (ngit-unhex (ngit-peel repo head 'tree))
                           (error nil))))
         (staged nil) (cached-files nil) (local nil))
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (dolist (entry entries)
      ;; check_local_mod ignores unmerged entries.  Only a stage-zero entry
      ;; can be compared to the work tree and HEAD here.
      (when (= (aref entry 3) 0)
        (let* ((path (aref entry 0))
               (st (ngit-entry-lstat index entry))
               (directory (and st (= (logand (aref st 0) ngit-s-ifmt)
                                      ngit-s-ifdir)))
               ;; A missing indexed path, or a regular path replaced by a
               ;; directory, is already removed for rm's safety check.
               (present (and st (or (= (aref entry 2) ngit-mode-gitlink)
                                    (not directory))))
               (local-change
                (and present
                     (not (ngit-rm--worktree-matches-p repo entry path st))))
               (he (ngit-rm--head-entry repo tree path))
               (staged-change
                (not (and he (= (car he) (aref entry 2))
                          (equal (cdr he) (aref entry 1))))))
          (when present
            (cond
             ((and local-change staged-change)
              (unless (and cached (memq 'ita (aref entry 5)))
                (push path staged)))
             ((not cached)
              (when staged-change (push path cached-files))
              (when local-change (push path local))))))))
    (ngit-rm--print-files
     staged
     "the following file has staged content different from both the\nfile and the HEAD:"
     "the following files have staged content different from both the\nfile and the HEAD:"
     "\n(use -f to force removal)")
    (ngit-rm--print-files
     cached-files "the following file has changes staged in the index:"
     "the following files have changes staged in the index:"
     "\n(use --cached to keep the file, or -f to force removal)")
    (ngit-rm--print-files
     local "the following file has local modifications:"
     "the following files have local modifications:"
     "\n(use --cached to keep the file, or -f to force removal)")
    (or staged cached-files local)))

(defun ngit-rm--parse (args)
  (let ((cached nil) (recursive nil) (force nil) (dry nil) (quiet nil)
        (ignore nil) (paths nil) (options t))
    (while args
      (let ((arg (pop args)))
        (cond
         ((and options (string= arg "--")) (setq options nil))
         ((and options (string= arg "--cached")) (setq cached t))
         ((and options (member arg '("-r" "--recursive"))) (setq recursive t))
         ((and options (member arg '("-f" "--force"))) (setq force t))
         ((and options (member arg '("-n" "--dry-run"))) (setq dry t))
         ((and options (member arg '("-q" "--quiet"))) (setq quiet t))
         ((and options (string= arg "--ignore-unmatch")) (setq ignore t))
         ((and options (string-prefix-p "-" arg) (> (length arg) 2)
               (not (string-prefix-p "--" arg)))
          (let ((i 1))
            (while (< i (length arg))
              (cond ((= (aref arg i) ?r) (setq recursive t))
                    ((= (aref arg i) ?f) (setq force t))
                    ((= (aref arg i) ?n) (setq dry t))
                    ((= (aref arg i) ?q) (setq quiet t))
                    (t (ngit--die-with 129 "error: unknown switch `%c'" (aref arg i))))
              (setq i (1+ i)))))
         ((and options (string-prefix-p "-" arg))
          (ngit--die-with 129 "error: unknown option `%s'" arg))
         (t (push arg paths)))))
    (list cached recursive force dry quiet ignore (nreverse paths))))

(defun ngit-cmd-rm (repo args)
  (let* ((parsed (ngit-rm--parse args))
         (cached (nth 0 parsed)) (recursive (nth 1 parsed))
         (force (nth 2 parsed)) (dry (nth 3 parsed))
         (quiet (nth 4 parsed)) (ignore (nth 5 parsed))
         (paths (nth 6 parsed)))
    (unless paths
      (ngit--die "fatal: No pathspec was given. Which files should I remove?"))
    (let* ((items (ngit-command-path-items repo paths t))
           (seen (make-vector (length items) nil))
           (recur (make-vector (length items) nil))
           (index-file (ngit-index-file repo))
           (lock (ngit-lock index-file))
           (index nil) (old nil) (selected nil)
           (selected-names (make-hash-table :test 'equal)))
      (catch 'ngit-rm-finish
        (unwind-protect
            (progn
            (setq index (ngit-read-index repo) old (aref index 0))
            (ngit-wt-setup (ngit-repo-toplevel repo))
            (let ((j 0))
              (dolist (item items)
                (let* ((literal (if (vectorp (car item)) (aref (car item) 0) (car item)))
                       (prefix (if (string= literal "") "" (concat literal "/")))
                       (i (if (cdr item) 0 (ngit-index-find old literal)))
                       (exact nil)
                       (ngit--rv-paths (and (cdr item) (list item))))
                  (when (and (vectorp (car item)) (aref (car item) 4))
                    (aset seen j t))
                  (unless (and (vectorp (car item)) (aref (car item) 4))
                   (if (cdr item)
                      (while (and (< i (length old)) (not exact))
                        (let* ((entry (aref old i)) (path (aref entry 0)))
                          (when (and (not (memq 'skip-worktree (aref entry 5)))
                                     (or (and (not (vectorp (car item)))
                                              (ngit-paths-covers-p literal path))
                                         (eq (ngit-command-path-class (list item) path nil) 'full)))
                            (aset seen j t)
                            ;; Git's seen[MATCHED_EXACTLY] skips this item on
                            ;; later index entries, preserving earlier matches.
                            (when (and (not (and (vectorp (car item)) (aref (car item) 4)))
                                       (ngit-rm--exact-item-p item path))
                              (setq exact t))
                            (when (and (not (string= literal path))
                                       (ngit-paths-covers-p literal path))
                              (aset recur j t))
                            (when (and (vectorp (car item))
                                       (not (aref (car item) 4))
                                       (not (ngit--glob-special-p literal (aref (car item) 7)))
                                       (or (string= literal "")
                                           (and (> (length path) (length literal))
                                                (= (aref path (length literal)) ?/))))
                              (aset recur j t))
                            (unless (gethash path selected-names)
                              (puthash path t selected-names)
                              (push entry selected))))
                        (setq i (1+ i)))
                    ;; Exact and descendant ranges need separate lower bounds:
                    ;; names such as "dir-extra" sort between "dir" and
                    ;; "dir/child".  Literal real-repository paths therefore
                    ;; visit only their selected ranges.
                    (if (string= literal "")
                        (while (< i (length old))
                          (let* ((entry (aref old i)) (path (aref entry 0)))
                            (unless (memq 'skip-worktree (aref entry 5))
                              (aset seen j t) (aset recur j t)
                              (unless (gethash path selected-names)
                                (puthash path t selected-names)
                                (push entry selected))))
                          (setq i (1+ i)))
                      (while (and (< i (length old))
                                  (string= (aref (aref old i) 0) literal))
                        (let* ((entry (aref old i)) (path (aref entry 0)))
                          (unless (memq 'skip-worktree (aref entry 5))
                            (aset seen j t)
                            (unless (gethash path selected-names)
                              (puthash path t selected-names)
                              (push entry selected))))
                        (setq i (1+ i)))
                      (setq i (ngit-index-find old prefix))
                      (while (and (< i (length old))
                                  (string-prefix-p prefix (aref (aref old i) 0)))
                        (let* ((entry (aref old i)) (path (aref entry 0)))
                          (unless (memq 'skip-worktree (aref entry 5))
                            (aset seen j t) (aset recur j t)
                            (unless (gethash path selected-names)
                              (puthash path t selected-names)
                              (push entry selected))))
                        (setq i (1+ i)))))))
                  (setq j (1+ j))))
            ;; Exclusions inspect only paths admitted by a positive selector.
            ;; Their seen-exact state likewise applies to later index entries.
            (let ((exact-excludes (make-hash-table :test 'eq)))
              (setq selected
                    (seq-filter
                     (lambda (entry)
                       (let ((path (aref entry 0)) (excluded nil))
                         (dolist (item items)
                           (when (and (vectorp (car item)) (aref (car item) 4)
                                      (not (gethash item exact-excludes))
                                      (ngit-pathspec-item-match-p (car item) path))
                             (setq excluded t)
                             (when (ngit-rm--exact-item-p item path)
                               (puthash item t exact-excludes))))
                         (not excluded)))
                     (sort selected #'ngit--entry-less))))
            (let ((i 0))
              (dolist (original paths)
                (cond
                 ((and (not (aref seen i)) (not ignore))
                  (ngit-unlock lock)
                  (setq lock nil)
                  (ngit--die "fatal: pathspec '%s' did not match any files" original))
                 ((and (aref recur i) (not recursive))
                  (ngit-unlock lock)
                  (setq lock nil)
                  (ngit--die "fatal: not removing '%s' recursively without -r"
                             (if (string= original "") "." original))))
                (setq i (1+ i))))
            (when (and (> (length items) (length paths)) (not recursive)
                       (aref recur (length paths)))
              (ngit--die "fatal: not removing '.' recursively without -r"))
            (when (and selected (not force)
                       (ngit-rm--check-local repo index selected cached))
              (ngit-unlock lock)
              (setq lock nil)
              (throw 'ngit-rm-finish 1))
            (dolist (entry selected)
              (unless quiet
                (ngit--emit "rm '" (ngit-rm--decode (aref entry 0)) "'\n")))
            (unless dry
              (let ((dropped (make-hash-table :test 'equal))
                    (root (ngit-ct-root index)))
                (dolist (entry selected)
                  (puthash (aref entry 0) t dropped)
                  (when root (ngit-ct-invalidate root (aref entry 0))))
                (unless cached
                  (let ((removed nil))
                    (dolist (entry selected)
                      (if (ngit-rm--remove-worktree-path repo entry)
                          (setq removed t)
                        (unless removed
                          (ngit-unlock lock)
                          (setq lock nil)
                          (ngit--die "fatal: git rm: '%s': Is a directory"
                                     (ngit-rm--decode (aref entry 0))))))))
                (ngit-commit-lock lock
                                  (ngit-index-bytes repo index
                                                    (ngit-index-overlay old nil dropped)
                                                    root))
                (setq lock nil)
                (aset repo 6 nil)))
              0)
          (when lock (ngit-unlock lock)))))))

(defun ngit-mv--basename (path)
  (let ((i (1- (length path))))
    (while (and (>= i 0) (/= (aref path i) ?/)) (setq i (1- i)))
    (substring path (1+ i))))

(defun ngit-mv--renamed-entry (repo index entry destination)
  "Rename ENTRY to DESTINATION, refreshing stat data only when clean."
  (let* ((st (ngit-wt-lstat destination))
         (pos (aref entry 4))
         (flags (aref entry 5))
         (flags (if (and pos (/= 0 (logand (aref (aref index 1) (+ pos 60)) #x80))
                         (not (memq 'assume-unchanged flags)))
                    (cons 'assume-unchanged flags)
                  flags)))
    (if (and (= (aref entry 3) 0) st
             (ngit-rm--worktree-matches-p repo entry destination st))
        (ngit-make-entry destination (aref entry 1) (aref entry 2)
                         (prog1 (ngit-wt-stat-block (aref entry 2))
                           (ngit-wt-lstat destination))
                         flags)
      ;; A raw-position entry cannot be reused after its name changes: the
      ;; index writer may otherwise copy the old record, including its name.
      (let ((stat (if (aref entry 4)
                      (substring (aref index 1) (aref entry 4)
                                 (+ (aref entry 4) 40))
                    (aref entry 6))))
        (vector destination (aref entry 1) (aref entry 2) (aref entry 3)
                nil flags stat)))))

(defun ngit-mv--error (ignore fmt &rest args)
  (unless ignore (apply #'ngit--die fmt args))
  nil)

(defun ngit-mv--emit-top (plan prefix)
  (ngit--emit prefix (nth 0 plan) " to " (nth 1 plan) "\n"))

(defun ngit-mv--emit-children (plans checking)
  "Emit appended directory-member actions after the original PLANS."
  (dolist (plan plans)
    (when (nth 2 plan)
      (let ((source (nth 0 plan)) (dest (nth 1 plan)))
        (dolist (entry (nth 3 plan))
          (let ((from (aref entry 0))
                (to (concat dest (substring (aref entry 0) (length source)))))
            (if checking
                (ngit--emit "Checking rename of '" from "' to '" to "'\n")
              (ngit--emit "Renaming " from " to " to "\n"))))))))

(defun ngit-mv--parse (args)
  (let ((force nil) (ignore nil) (dry nil) (verbose nil) (paths nil) (options t))
    (while args
      (let ((arg (pop args)))
        (cond
         ((and options (string= arg "--")) (setq options nil))
         ((and options (member arg '("-f" "--force"))) (setq force t))
         ((and options (string= arg "-k")) (setq ignore t))
         ((and options (member arg '("-n" "--dry-run"))) (setq dry t))
         ((and options (member arg '("-v" "--verbose"))) (setq verbose t))
         ((and options (string-prefix-p "-" arg) (> (length arg) 2)
               (not (string-prefix-p "--" arg)))
          (let ((i 1))
            (while (< i (length arg))
              (cond ((= (aref arg i) ?f) (setq force t))
                    ((= (aref arg i) ?k) (setq ignore t))
                    ((= (aref arg i) ?n) (setq dry t))
                    ((= (aref arg i) ?v) (setq verbose t))
                    (t (ngit--die-with 129 "error: unknown switch `%c'" (aref arg i))))
              (setq i (1+ i)))))
         ((and options (string-prefix-p "-" arg))
          (ngit--die-with 129 "error: unknown option `%s'" arg))
         (t (push arg paths)))))
    (list force ignore dry verbose (nreverse paths))))

(defun ngit-mv--path (repo arg)
  "Normalize a filename, retaining mv's outside-repository diagnostic."
  (condition-case err
      (ngit--pathspec-item repo arg)
    (ngit-fatal
     (if (string-search "is outside repository at" (cadr err))
         (ngit--die "fatal: '%s' is outside repository at '%s'"
                    arg (directory-file-name (ngit-repo-toplevel repo)))
       (signal (car err) (cdr err))))))

(defun ngit-cmd-mv (repo args)
  (let* ((parsed (ngit-mv--parse args))
         (force (nth 0 parsed)) (ignore (nth 1 parsed))
         (dry (nth 2 parsed)) (verbose (nth 3 parsed))
         (argv (nth 4 parsed)))
    (when (< (length argv) 2)
      (ngit--die-with 129 "fatal: git mv [<options>] <source>... <destination>"))
    (let* ((destination-arg (car (last argv)))
           (source-args (butlast argv))
           (destination (ngit-mv--path repo destination-arg))
           (top (ngit-repo-toplevel repo))
           (index-file (ngit-index-file repo))
           (lock (ngit-lock index-file))
           (index nil) (old nil) (plans nil)
           (destinations (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            (setq index (ngit-read-index repo) old (aref index 0))
            (ngit-wt-setup top)
            (let* ((dst-st (ngit-wt-lstat destination))
                   (dst-dir (and dst-st (= (logand (aref dst-st 0) ngit-s-ifmt)
                                           ngit-s-ifdir))))
              (when (and (> (length source-args) 1) (not dst-dir))
                (ngit-unlock lock)
                (setq lock nil)
                (ngit--die "fatal: destination '%s' is not a directory" destination-arg))
              (dolist (source-arg source-args)
                (let* ((source (ngit-mv--path repo source-arg))
                       (dest (if (or dst-dir (string= destination ""))
                                 (concat (if (string= destination "") ""
                                           (concat destination "/"))
                                         (ngit-mv--basename source))
                               destination))
                       (st (ngit-wt-lstat source))
                       (dirp (and st (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir)))
                       (bad nil) (members nil))
                  (when dry
                    (ngit--emit "Checking rename of '" source "' to '" dest "'\n"))
                  (cond
                   ((null st) (setq bad "bad source"))
                   ((and (string-prefix-p source dest)
                         (or (= (length source) (length dest))
                             (= (aref dest (length source)) ?/)))
                    (setq bad "can not move directory into itself"))
                   ((and dirp (ngit-wt-lstat dest))
                    (setq bad "destination already exists")))
                  (unless bad
                    (if dirp
                        (let* ((prefix (concat source "/"))
                               (i (ngit-index-find old prefix)))
                          (while (and (< i (length old))
                                      (string-prefix-p prefix (aref (aref old i) 0)))
                            (push (aref old i) members)
                            (setq i (1+ i)))
                          (setq members (nreverse members))
                          (cond ((null members) (setq bad "source directory is empty"))
                                ((seq-some (lambda (entry) (/= (aref entry 3) 0)) members)
                                 (setq bad "conflicted"))))
                      (let ((entry (ngit-index-lookup old source)))
                        (cond ((null entry) (setq bad "not under version control"))
                              ((/= (aref entry 3) 0) (setq bad "conflicted"))
                              (t (setq members (list entry)))))))
                  (unless bad
                    (let ((dest-st (ngit-wt-lstat dest)))
                      (when dest-st
                        (if (not force)
                            (setq bad "destination exists")
                          (unless (memq (logand (aref dest-st 0) ngit-s-ifmt)
                                        (list ngit-s-ifreg ngit-s-iflnk))
                            (setq bad "Cannot overwrite"))
                          (when (and (not bad) verbose)
                            (ngit-native-write-stderr
                             (format "warning: overwriting '%s'\n" (ngit-rm--decode dest))))))))
                  (when (and (not bad) (gethash dest destinations))
                    (setq bad "multiple sources for the same target"))
                  (when (and (not bad) (string-suffix-p "/" destination-arg)
                             (not dst-dir)
                             (not (and dirp (= (length source-args) 1))))
                    (setq bad "destination directory does not exist"))
                  (if bad
                      (unless ignore
                        (ngit-unlock lock)
                        (setq lock nil)
                        (ngit--die "fatal: %s, source=%s, destination=%s"
                                   bad (ngit-rm--decode source)
                                   (ngit-rm--decode
                                    (if (and (string= bad "destination directory does not exist")
                                             (string-suffix-p "/" destination-arg))
                                        (concat dest "/")
                                      dest))))
                    (puthash dest t destinations)
                    (push (list source dest dirp members) plans)))))
            (setq plans (nreverse plans))
            (when dry
              (ngit-mv--emit-children plans t))
            (unless dry
              (let ((done nil))
                (dolist (plan plans)
                  (let ((source (nth 0 plan)) (dest (nth 1 plan)))
                    (when verbose (ngit-mv--emit-top plan "Renaming "))
                    (condition-case err
                        (progn
                          (rename-file
                           (expand-file-name (ngit-rm--decode source) top)
                           (expand-file-name (ngit-rm--decode dest) top) force)
                          (push plan done))
                      (file-error
                       (unless ignore
                         (ngit-unlock lock)
                         (setq lock nil)
                         (ngit--die "fatal: renaming '%s' failed: %s"
                                    (ngit-rm--decode source)
                                    (if (stringp (nth 2 err))
                                        (nth 2 err)
                                      (error-message-string err))))))))
                (setq plans (nreverse done))))
            (if dry
                (progn
                  (dolist (plan plans) (ngit-mv--emit-top plan "Renaming "))
                  (ngit-mv--emit-children plans nil))
              (when verbose (ngit-mv--emit-children plans nil)))
            (unless dry
              (let ((dropped (make-hash-table :test 'equal))
                    (targets nil) (root (ngit-ct-root index)))
                (dolist (plan plans)
                  (let ((source (nth 0 plan)) (dest (nth 1 plan))
                        (dirp (nth 2 plan)))
                    (dolist (entry (nth 3 plan))
                      (let ((new-name
                             (if dirp
                                 (concat dest (substring (aref entry 0) (length source)))
                               dest)))
                        (puthash (aref entry 0) t dropped)
                        (when root
                          (ngit-ct-invalidate root (aref entry 0))
                          (ngit-ct-invalidate root new-name))
                        (push (ngit-mv--renamed-entry repo index entry new-name) targets)))))
                (setq targets (sort targets #'ngit--entry-less))
                (ngit-commit-lock lock
                                  (ngit-index-bytes repo index
                                                    (ngit-index-overlay old targets dropped)
                                                    root))
                (setq lock nil)
                (aset repo 6 nil)))
            0)
        (when lock (ngit-unlock lock))))))

(provide 'ngit-rm)

;;; ngit-rm.el ends here
