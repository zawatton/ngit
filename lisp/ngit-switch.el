;;; ngit-switch.el --- switch branches with a two-tree merge -*- lexical-binding: t; -*-

;;; Commentary:

;; Branch switching shared by `switch' and the branch form of `checkout'.
;; The index merge follows unpack-trees.c's twoway_merge rules.  It starts
;; with the current index and visits only paths changed between the old and
;; new trees, so equal subtrees in a large repository remain untouched.

;;; Code:

(require 'ngit-checkout)
(require 'ngit-reset)

(defun ngit--switch-hook-oid (hex)
  (or hex (make-string 40 ?0)))
(require 'ngit-branch)
(require 'ngit-commit)

(defun ngit--switch-same-entry-p (entry mode bin)
  (and entry (= (aref entry 3) 0) (= (aref entry 2) mode)
       (equal (aref entry 1) bin)))

(defun ngit--switch-tree (repo commit)
  (and commit (ngit-unhex (ngit-peel repo commit 'tree))))

(defun ngit--switch-entry (path mode bin old)
  (ngit-make-entry
   path bin mode
   (concat (make-string 24 0) (ngit--be32-bytes mode) (make-string 12 0))
   (and old (memq 'skip-worktree (aref old 5)) '(skip-worktree))))

(defun ngit--switch-worktree-clean-p (repo index entry)
  "Non-nil when ENTRY is clean or absent in the work tree."
  (or (ngit--checkout-stat-clean-p repo index entry)
      (ngit--checkout-clean-p repo index entry)
      (progn
        (ngit-wt-setup (ngit-repo-toplevel repo))
        (and (ngit--checkout-leading-clear-p (aref entry 0))
             (null (ngit-wt-lstat (aref entry 0)))))))

(defun ngit--switch-error-paths (command local untracked &optional removed dirs)
  (when local
    (ngit-native-write-stderr
     (concat "error: Your local changes to the following files would be overwritten by "
             (if (string= command "merge") "merge" "checkout") ":\n"
             (mapconcat (lambda (p) (concat "\t" p "\n"))
                        (sort (delete-dups local) #'string<) "")
             (if (string= command "merge")
                 "Please commit your changes or stash them before you merge.\n"
               "Please commit your changes or stash them before you switch branches.\n"))))
  (when untracked
    (ngit-native-write-stderr
     (concat "error: The following untracked working tree files would be overwritten by "
             (if (string= command "merge") "merge" "checkout") ":\n"
             (mapconcat (lambda (p) (concat "\t" p "\n"))
                        (sort (delete-dups untracked) #'string<) "")
             (if (string= command "merge")
                 "Please move or remove them before you merge.\n"
               "Please move or remove them before you switch branches.\n"))))
  (when removed
    (ngit-native-write-stderr
     (concat "error: The following untracked working tree files would be removed by "
             (if (string= command "merge") "merge" "checkout") ":\n"
             (mapconcat (lambda (p) (concat "\t" p "\n"))
                        (sort (delete-dups removed) #'string<) "")
             (if (string= command "merge")
                 "Please move or remove them before you merge.\n"
               "Please move or remove them before you switch branches.\n"))))
  (when dirs
    (ngit-native-write-stderr
     (concat "error: Updating the following directories would lose untracked files in them:\n"
             (mapconcat (lambda (p) (concat "\t" p "\n"))
                        (sort (delete-dups dirs) #'string<) "") "\n")))
  (when (or local untracked removed dirs)
    (ignore command)
    (ngit--die-with 1 "Aborting")))

(defun ngit--switch-untracked-conflicts (untracked path)
  "Return untracked names that conflict with writing PATH."
  (let ((out nil) (dir (concat path "/")))
    (dolist (u untracked)
      (when (or (string= u path)
                (string-prefix-p (concat (string-remove-suffix "/" u) "/") path)
                (string-prefix-p dir u))
        (push u out)))
    out))

(defun ngit--switch-overlay (old dropped targets)
  "Overlay TARGETS on OLD after omitting exact names in DROPPED."
  (ngit-index-overlay old targets dropped))

(defun ngit--switch-tree-changes (repo old-tree new-tree initial)
  "Return two-tree changes, adding every target path for INITIAL checkout."
  (let ((changes (ngit-diff-trees repo old-tree new-tree)))
    (when initial
      (let ((seen (make-hash-table :test 'equal)))
        (dolist (change changes) (puthash (aref change 0) t seen))
        (dolist (change (ngit-diff-trees repo nil new-tree))
          (unless (gethash (aref change 0) seen) (push change changes)))
        (setq changes
              (sort changes
                    (lambda (left right)
                      (string< (aref left 0) (aref right 0)))))))
    changes))

(defvar ngit--switch-allow-merge nil)

(defun ngit--switch-merge-local (repo base target label quiet style ancestor)
  "Merge local work-tree edits across a branch change after two-tree refusal."
  (require 'ngit-merge-tree)
  (let* ((index (ngit-read-index repo)) (old (aref index 0))
         (staged (ngit-diff-tree-index repo base))
         (wt (ngit-wt-changes repo t))
         (drop (make-hash-table :test 'equal)) (local nil))
    (when staged
      (ngit--die "fatal: cannot continue with staged changes in the following files:\n%s"
                 (mapconcat (lambda (c) (aref c 0)) staged "\n")))
    (dolist (c wt)
      (puthash (aref c 0) t drop)
      (when (/= (aref c 4) 0)
        (push (ngit--switch-entry (aref c 0) (aref c 4)
                                  (if (aref c 7) (ngit-write-object repo "blob" (aref c 7)) (aref c 5)) nil) local)))
    (let* ((work (ngit-ct-update repo (ngit-ct-new) (ngit-index-overlay old local drop)))
           (ngit--config-cache (cons repo (append (ngit-config-entries repo)
                                                  (and style (list (cons "merge.conflictstyle" style))))))
           (ngit-merge-ancestor-label ancestor)
           (ngit-merge-unique-worktree-root (ngit-repo-toplevel repo))
           (result (ngit-merge-trees repo base target work label "local"))
           (image (plist-get result :worktree))
           (conflicts (plist-get result :conflicts))
           (final (ngit-merge-tree-entries repo target))
           (relocations (make-hash-table :test 'equal))
           (directory-conflicts nil)
           (unmerged nil) (dropped (make-hash-table :test 'equal))
           (lock nil) (i 0))
      (ngit--switch-protect-untracked repo base (plist-get result :tree) "checkout")
      ;; Checkout's recursive merge keeps D/F conflict stages at the original
      ;; path.  Only the work-tree copy moves aside; reset_tree then skips
      ;; target descendants obstructed by those unmerged stages.
      (dolist (e local)
        (let* ((path (aref e 0)) (stem (concat path "~local")))
          (when (ngit-index-dir-p final (concat path "/"))
            (dolist (c conflicts)
              (when (and (or (equal c stem)
                             (and (string-prefix-p (concat stem "_") c)
                                  (string-match-p "\\`[0-9]+\\'" (substring c (1+ (length stem))))))
                         (seq-some (lambda (entry)
                                     (and (equal (aref entry 0) c) (= (aref entry 3) 3)
                                          (equal (aref entry 1) (aref e 1))))
                                   (append (plist-get result :entries) nil)))
                (puthash c path relocations)
                (push path directory-conflicts))))))
      (when directory-conflicts
        (setq final (vconcat
                     (seq-remove (lambda (e)
                                   (seq-some (lambda (path) (string-prefix-p (concat path "/") (aref e 0)))
                                             directory-conflicts))
                                 (append final nil)))))
      (dolist (path conflicts) (puthash path t dropped))
      (dolist (e (append (plist-get result :entries) nil))
        (when (gethash (aref e 0) dropped)
          (let ((original (gethash (aref e 0) relocations)))
            (when original
              (setq e (copy-sequence e))
              (aset e 0 original)
              (puthash original t dropped))
            (push e unmerged))))
      (setq final (ngit-index-overlay final unmerged dropped)
            lock (ngit-lock (ngit-index-file repo)))
      (unwind-protect
          (progn
            (while (< i (length old))
              (let ((e (aref old i)))
                (unless (ngit-index-lookup image (aref e 0)) (ngit--unlink-entry repo e)))
              (setq i (1+ i)))
            (aset index 0 final)
            (dolist (e (append image nil))
              (unless (ngit--checkout-clean-p repo index e) (ngit-checkout-entry repo e)))
            (ngit-commit-lock lock (ngit-index-bytes repo index final (ngit-ct-new) nil))
            (setq lock nil) (aset repo 6 nil) (aset repo 7 nil))
        (when lock (ngit-unlock lock)))
      (ngit--switch-show-local repo target quiet)
      'merged)))

(defun ngit--switch-twoway (repo old-tree new-tree command &optional known-untracked)
  "Merge OLD-TREE and NEW-TREE through the current index and work tree.
KNOWN-UNTRACKED, when supplied, wraps an already expanded list of paths."
  (let* ((index (ngit-read-index repo))
         (old (aref index 0))
         (initial (string= (aref index 1) ""))
         (changes (ngit--switch-tree-changes repo old-tree new-tree initial))
         (dropped (make-hash-table :test 'equal))
         (planned nil) (deletes nil) (writes nil)
         (local nil) (df-blockers nil) (need-untracked nil) (removed-check nil)
         (root (ngit-ct-root index)) (i 0)
         (candidates (list nil)))
    (while (< i (length old))
      (when (/= (aref (aref old i) 3) 0)
        (ngit--die-with 1 "error: you need to resolve your current index first"))
      (setq i (1+ i)))
    ;; Decide every path before touching either the work tree or the index.
    (dolist (c changes)
      (let* ((path (aref c 0))
             (old-mode (aref c 2)) (old-bin (aref c 3))
             (new-mode (aref c 4)) (new-bin (aref c 5))
             (current (ngit-index-lookup old path))
             (same-old (and (/= old-mode 0)
                            (ngit--switch-same-entry-p current old-mode old-bin)))
             (same-new (and (/= new-mode 0)
                            (ngit--switch-same-entry-p current new-mode new-bin))))
        (cond
         ;; twoway_merge keeps an index entry that already equals the target.
         (same-new nil)
         ;; A staged deletion carries only when both trees agree.  CHANGES
         ;; means they do not, so it conflicts whenever the new tree has data.
         ((null current)
          (if (= new-mode 0)
              (progn (setq need-untracked t)
                     (push path removed-check))
            (if (and (/= old-mode 0) (not initial))
                (push path local)
              (setq need-untracked t)
              (push (list 'write
                          (ngit--switch-entry path new-mode new-bin nil)) planned))))
         ;; An unchanged index entry may advance with the target tree.
         (same-old
          ;; A verified tracked leaf cannot hide untracked siblings.  Gitlinks
          ;; are directories and still require the untracked preflight.
          (when (= (aref current 2) ngit-mode-gitlink) (setq need-untracked t))
          (if (ngit--switch-worktree-clean-p repo index current)
              (if (= new-mode 0)
                  (progn
                    (puthash path t dropped)
                    (push (list 'delete current) planned))
                (progn
                  (puthash path t dropped)
                  (push (list 'write
                              (ngit--switch-entry path new-mode new-bin current)) planned)))
            (push path local)))
         ;; All other index states are staged changes that would be lost.
         (t (push path local)))))
    ;; A target file may replace an indexed directory.  twoway_merge rejects
    ;; staged descendants that were not independently removed above.
    (dolist (one planned)
      (when (eq (car one) 'write)
        (let* ((target (cadr one)) (path (aref target 0))
               (prefix (concat path "/")) (j (ngit-index-find old prefix)))
          (while (and (< j (length old))
                      (string-prefix-p prefix (aref (aref old j) 0)))
            (unless (gethash (aref (aref old j) 0) dropped)
              (push (aref (aref old j) 0) local))
            (setq j (1+ j)))
          (let ((start 0) (slash nil))
            (while (setq slash (ngit--byte-pos path ?/ start))
              (let* ((ancestor (substring path 0 slash))
                     (entry (ngit-index-lookup old ancestor)))
                (when (and entry (not (gethash ancestor dropped)))
                  (push ancestor df-blockers)))
              (setq start (1+ slash)))))))
    (let ((collisions nil) (dirs nil)
          (removed nil)
          (all-untracked (and need-untracked
                              (if known-untracked (car known-untracked)
                                (if (string= command "merge")
                                    (car (ngit-add-untracked repo '("") (ngit--tracked-set old)))
                                  (ngit-untracked repo))))))
      (setq collisions (append df-blockers collisions))
      (dolist (one planned)
        (when (and all-untracked (eq (car one) 'write))
          (let* ((path (aref (cadr one) 0)) (prefix (concat path "/")))
            (dolist (u all-untracked)
              (cond ((string-prefix-p prefix u) (push path dirs))
                    ((or (string= u path)
                         (string-prefix-p
                          (concat (string-remove-suffix "/" u) "/") path))
                     (push u collisions)))))))
      (dolist (path removed-check)
        (when (member path all-untracked) (push path removed)))
      (when (and ngit--switch-allow-merge local (not (or removed dirs))
                 (seq-every-p (lambda (path) (member path local)) collisions))
        (throw 'ngit-switch-merge-needed 'merge))
      (ngit--switch-error-paths command local collisions removed dirs))
    (let ((lock (ngit-lock (ngit-index-file repo)))
          (fresh nil)
          (checked (make-hash-table :test 'eql))
          (ngit--checkout-attr-direction 'index-first)
          (ngit--checkout-attr-cache (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            ;; Attribute lookup for work-tree writes must see the target
            ;; index, including a newly checked out .gitattributes file.
            (let ((targets nil))
              (dolist (one planned)
                (when (eq (car one) 'write) (push (cadr one) targets)))
              (setq targets (nreverse targets))
              (aset index 0 (ngit--switch-overlay old dropped targets))
              ;; Ignored-only directories may be replaced after the complete
              ;; untracked-file preflight, while the index lock is held.
              (ngit--switch-clear-target-directories repo (vconcat targets))
              (aset repo 7 nil))
            ;; Remove old paths before creating replacements so file/directory
            ;; transitions do not delete unrelated untracked siblings.
            (dolist (one planned)
              (when (eq (car one) 'delete)
                (let ((e (cadr one)))
                  (puthash (aref e 0) t dropped)
                  (push e deletes)
                  (when root (ngit-ct-invalidate root (aref e 0))))))
            (dolist (e deletes) (ngit--unlink-entry repo e))
            (dolist (one planned)
              (when (eq (car one) 'write)
                (let ((e (cadr one)))
                  (puthash (aref e 0) t dropped)
                  (push (ngit-checkout-entry repo e) fresh)
                  (when root (ngit-ct-invalidate root (aref e 0))))))
            (let ((entries (ngit--switch-overlay old dropped (nreverse fresh))))
              (aset index 0 entries)
              (when (and (null new-tree) (= (length entries) 0))
                (unless root (setq root (ngit-ct-new)))
                (aset root 0 0)
                (aset root 1 ngit-empty-tree-bin))
              ;; unpack_trees refreshes carried entries too.  This updates
              ;; clean entries whose work-tree stat changed while retaining
              ;; the old stat data for genuinely dirty entries.
              (ngit--reset-refresh repo index entries checked t candidates)
              (ngit-commit-lock lock (ngit-index-bytes repo index entries root checked))
              (setq lock nil)
              (aset repo 6 nil)
              candidates))
        (when lock (ngit-unlock lock))))))

(defun ngit--switch-protect-untracked (repo old-tree new-tree command)
  "Refuse untracked files that discard-changes would overwrite."
  (let* ((entries (ngit-index-entries repo))
         (tracked (ngit--tracked-set entries))
         (untracked nil) (collisions nil) (removed nil) (dirs nil))
    ;; Expand untracked directories so a target directory can be diagnosed
    ;; precisely while ignored-only directories remain replaceable.
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (ngit--walk-untracked
     repo (file-name-as-directory (ngit-repo-toplevel repo)) tracked ""
     (ngit-ignore-global-patterns repo)
     (lambda (path) (push path untracked)) nil t)
    (dolist (c (ngit-diff-trees repo old-tree new-tree))
      (let ((path (aref c 0)))
        (if (= (aref c 4) 0)
            (when (member path untracked) (push path removed))
          (let* ((prefix (concat path "/"))
                 (descendant (seq-find
                              (lambda (name) (string-prefix-p prefix name))
                              untracked))
                 (st (and descendant (ngit--checkout-leading-clear-p path)
                          (ngit-wt-lstat path))))
            (if (and st (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir))
                (push path dirs)
              (setq collisions
                    (append (ngit--switch-untracked-conflicts untracked path)
                            collisions)))))))
    (cond
     (dirs
      (ngit--die "error: Updating '%s' would lose untracked files in it"
                 (car (sort (delete-dups dirs) #'string<))))
     (collisions
      (ngit--die "error: Untracked working tree file '%s' would be overwritten by merge."
                 (car (sort (delete-dups collisions) #'string<))))
     (removed
      (ngit--die "error: Untracked working tree file '%s' would be removed by merge."
                 (car (sort (delete-dups removed) #'string<)))))
    (ignore command)))

(defun ngit--switch-delete-directory-tree (directory)
  "Delete DIRECTORY recursively without following nested symbolic links."
  (dolist (path (directory-files directory t nil t))
    (unless (member (file-name-nondirectory path) '("." ".."))
      (cond
       ((file-symlink-p path) (delete-file path))
       ((file-directory-p path) (ngit--switch-delete-directory-tree path))
       (t (delete-file path)))))
  (delete-directory directory))

(defun ngit--switch-clear-target-directories (repo entries)
  "Remove safe work-tree directories replaced by file ENTRIES."
  (let ((top (ngit-repo-toplevel repo)))
    (ngit-wt-setup top)
    (let ((i 0))
      (while (< i (length entries))
        (let* ((entry (aref entries i)) (path (aref entry 0))
               (mode (aref entry 2)) (st (ngit-wt-lstat path)))
          (unless (or (= mode ngit-mode-gitlink)
                      (and (aref entry 4)
                           (memq 'skip-worktree (aref entry 5))))
            (when (and (ngit--checkout-leading-clear-p path) st
                       (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir))
              (ngit--switch-delete-directory-tree
               (expand-file-name (decode-coding-string path 'utf-8) top)))))
        (setq i (1+ i))))))

(defun ngit--switch-show-local (repo tree quiet &optional candidates)
  (unless quiet
    (dolist (c (ngit--compose-tree-wt repo
                (ngit-diff-tree-index repo tree)
                (ngit-wt-changes
                 repo t nil
                 (and candidates
                      (let ((entries (ngit-index-entries repo)))
                        (vconcat (delq nil
                                       (mapcar (lambda (path)
                                                 (ngit-index-lookup entries path))
                                               (sort (car candidates) #'string<)))))))))
      (ngit--emit (string (aref c 1)) "\t" (ngit-quote-path (aref c 0)) "\n"))))

(defun ngit--switch-ahead-behind (repo ours theirs)
  "Return exact (AHEAD . BEHIND) counts without assuming date topology."
  (if (equal ours theirs)
      (cons 0 0)
    (let ((flags (make-hash-table :test 'equal)) (keys nil))
      (dolist (start (list (cons ours 1) (cons theirs 2)))
        (let ((stack (list start)))
          (while stack
            (let* ((item (pop stack)) (bin (car item)) (side (cdr item))
                   (old (gethash bin flags 0)))
              (when (= 0 (logand old side))
                (when (= old 0) (push bin keys))
                (puthash bin (logior old side) flags)
                (dolist (parent (ngit-commit-parents repo (ngit-hex bin)))
                  (push (cons (ngit-unhex parent) side) stack)))))))
      (let ((ahead 0) (behind 0))
        (dolist (bin keys)
          (pcase (gethash bin flags)
            (1 (setq ahead (1+ ahead)))
            (2 (setq behind (1+ behind)))))
        (cons ahead behind)))))

(defun ngit--switch-tracking-info (repo branch)
  "Return checkout's tracking paragraph for BRANCH."
  (let ((up (ngit--upstream repo branch)))
    (when up
      (let ((base (ngit-shorten-ref repo up))
            (theirs (ngit-resolve-ref repo up))
            (ours (ngit-resolve-ref repo (concat "refs/heads/" branch))))
        (if (null theirs)
            (concat "Your branch is based on '" base "', but the upstream is gone.\n"
                    "  (use \"git branch --unset-upstream\" to fixup)\n")
          (let* ((ab (ngit--switch-ahead-behind
                      repo (ngit-unhex ours) (ngit-unhex theirs)))
                 (ahead (car ab)) (behind (cdr ab)))
            (cond
             ((and (= ahead 0) (= behind 0))
              (concat "Your branch is up to date with '" base "'.\n"))
             ((= behind 0)
              (concat "Your branch is ahead of '" base "' by "
                      (number-to-string ahead)
                      (if (= ahead 1) " commit.\n" " commits.\n")
                      "  (use \"git push\" to publish your local commits)\n"))
             ((= ahead 0)
              (concat "Your branch is behind '" base "' by "
                      (number-to-string behind)
                      (if (= behind 1) " commit, and can be fast-forwarded.\n"
                        " commits, and can be fast-forwarded.\n")
                      "  (use \"git pull\" to update your local branch)\n"))
             (t
              (concat "Your branch and '" base "' have diverged,\n"
                      "and have " (number-to-string ahead) " and "
                      (number-to-string behind)
                      (if (= (+ ahead behind) 1)
                          " different commit each, respectively.\n"
                        " different commits each, respectively.\n")
                      "  (use \"git pull\" if you want to integrate the remote branch with yours)\n")))))))))

(defun ngit--switch-previous (repo)
  "Return the previous checkout name from HEAD's reflog."
  (let ((file (expand-file-name "logs/HEAD" (ngit-repo-gitdir repo)))
        (answer nil))
    (when (file-exists-p file)
      (dolist (line (reverse (split-string (ngit--read-file file) "\n" t)))
        (unless answer
          (let* ((needle "\tcheckout: moving from ")
                 (begin (string-search needle line))
                 (from (and begin (+ begin (length needle))))
                 (to (and from (string-search " to " line from))))
            (when to (setq answer (substring line from to)))))))
    (or answer (ngit--die "fatal: invalid reference: @{-1}"))))

(defun ngit--switch-remote-candidate (repo name)
  "Return the unique remote ref ending in /NAME, honoring defaultRemote."
  (let ((hits nil) (preferred nil)
        (default (ngit-config-get repo "checkout.defaultremote")))
    (dolist (r (ngit-list-refs repo "refs/remotes/"))
      (let ((ref (car r)))
        (when (not (nth 2 r))
          (let* ((short (substring ref 13))
                 (slash (string-search "/" short))
                 (branch (and slash (substring short (1+ slash)))))
            (when (and branch (string= branch name))
              (push ref hits)
              (when (and (stringp default)
                         (string-prefix-p (concat "refs/remotes/" default "/") ref))
                (setq preferred ref)))))))
    (cond (preferred preferred)
          ((null (cdr hits)) (car hits))
          ((cdr hits)
           (ngit-native-write-stderr
            "hint: If you meant to check out a remote tracking branch on, e.g. 'origin',\nhint: you can do so by fully qualifying the name with the --track option:\nhint:\nhint:     git checkout --track origin/<name>\nhint:\nhint: If you'd like to always have checkouts of an ambiguous <name> prefer\nhint: one remote, e.g. the 'origin' remote, consider setting\nhint: checkout.defaultRemote=origin in your config.\n")
           (ngit--die "fatal: '%s' matched multiple (%d) remote tracking branches"
                      name (length hits)))
          (t nil))))

(defun ngit--switch-parse (args checkout)
  "Parse switch-style ARGS.  CHECKOUT selects -b/-B spellings."
  (let ((quiet nil) (create nil) (force-create nil) (detach nil)
        (force nil) (discard nil) (orphan nil) (track 'default) (guess t)
        (pos nil) (merge nil) (style nil))
    (while args
      (let ((a (pop args)))
        (cond
         ((string= a "--") (setq pos (append pos args) args nil))
         ((string= a "-") (setq pos (append pos (list a))))
         ((member a '("-q" "--quiet")) (setq quiet t))
         ((member a '("-f" "--force")) (setq force t discard t))
         ((member a '("-m" "--merge")) (setq merge t))
         ((string-prefix-p "--conflict=" a) (setq merge t style (substring a 11)))
         ((string= a "--track=inherit") (setq track 'inherit))
         ((string= a "--discard-changes") (setq discard t))
         ((member a '("-d" "--detach")) (setq detach t))
         ((string= a "--no-guess") (setq guess nil))
         ((string= a "--guess") (setq guess t))
         ((member a '("-t" "--track" "--track=direct")) (setq track 'explicit))
         ((string= a "--no-track") (setq track 'none))
         ((or (and checkout (string= a "-b"))
              (and (not checkout) (member a '("-c" "--create"))))
          (setq create (or (pop args) (ngit--die "error: switch `c' requires a value"))))
         ((or (and checkout (string= a "-B"))
              (and (not checkout) (member a '("-C" "--force-create"))))
          (setq force-create (or (pop args) (ngit--die "error: switch `C' requires a value"))))
         ((string-prefix-p "--create=" a) (setq create (substring a 9)))
         ((string-prefix-p "--force-create=" a) (setq force-create (substring a 15)))
         ((and checkout (string-prefix-p "-b" a) (> (length a) 2))
          (setq create (substring a 2)))
         ((and checkout (string-prefix-p "-B" a) (> (length a) 2))
          (setq force-create (substring a 2)))
         ((and (not checkout) (string-prefix-p "-c" a) (> (length a) 2))
          (setq create (substring a 2)))
         ((and (not checkout) (string-prefix-p "-C" a) (> (length a) 2))
          (setq force-create (substring a 2)))
         ((string= a "--orphan")
          (setq orphan (or (pop args) (ngit--die "error: option `orphan' requires a value"))))
         ((string-prefix-p "--orphan=" a) (setq orphan (substring a 9)))
         ((string-prefix-p "-" a)
          (ngit--die-with 129 "error: unknown option `%s'"
                          (string-remove-prefix "--" (string-remove-prefix "-" a))))
         (t (setq pos (append pos (list a)))))))
    (when (> (+ (if create 1 0) (if force-create 1 0) (if orphan 1 0)) 1)
      (ngit--die "fatal: branch creation options cannot be used together"))
    (when (and style (not (member style '("merge" "diff3" "zdiff3"))))
      (ngit--die "fatal: unknown style '%s' given for 'merge.conflictstyle'" style))
    (when (and merge discard) (ngit--die "fatal: options '-f' and '-m' cannot be used together"))
    (list :merge merge :style style :quiet quiet :create create :force-create force-create
          :detach detach :force force :discard discard :orphan orphan :track track
          :guess guess :pos pos :checkout checkout)))

(defun ngit--switch-resolve (repo opts old-name old-hex)
  "Resolve OPTS to (DISPLAY REF HEX START NEW FORCE ORPHAN DETACH)."
  (let* ((pos (plist-get opts :pos))
         (create (or (plist-get opts :create) (plist-get opts :force-create)))
         (forcing (and (plist-get opts :force-create) t))
         (orphan (plist-get opts :orphan))
         (detach (plist-get opts :detach))
         (track (plist-get opts :track))
         (arg (car pos))
         (start (or arg "HEAD"))
         (new create))
    (when (cdr pos) (ngit--die "fatal: only one reference expected"))
    (when (and orphan arg (not (plist-get opts :checkout)))
      (ngit--die "fatal: '--orphan' cannot take <start-point>"))
    (when (and detach (or create orphan))
      (ngit--die "fatal: '--detach' cannot be used with branch creation"))
    (when (string= start "-") (setq start (ngit--switch-previous repo)))
    (when (and (eq track 'explicit) (not new) (not orphan))
      (unless arg (ngit--die "fatal: --track needs a branch name"))
      (let ((slash (string-search "/" arg)))
        (unless (and slash (< (1+ slash) (length arg)))
          (ngit--die "fatal: missing branch name; try -%s"
                     (if (plist-get opts :checkout) "b" "c")))
        (setq new (substring arg (1+ slash)))))
    (if orphan
        (let ((hex (and (plist-get opts :checkout)
                        (condition-case nil
                            (ngit-peel repo (ngit-rev-parse repo start) 'commit)
                          (error (ngit--die "fatal: invalid reference: %s" start))))))
          (list orphan (concat "refs/heads/" orphan) hex start t nil t nil))
      (let* ((local-name (or new start))
             (local-ref (concat "refs/heads/" local-name))
             (local-hex (and (not new) (ngit-resolve-ref repo local-ref)))
             (remote nil))
        (when (and (not local-hex) (not new) (not detach)
                   (plist-get opts :guess))
          (setq remote (ngit--switch-remote-candidate repo start))
          (when remote
            (setq new start start remote local-ref (concat "refs/heads/" new))))
        (let* ((hex (condition-case nil
                        (ngit-peel repo (ngit-rev-parse repo start) 'commit)
                      (error nil)))
               (ref (cond ((and (plist-get opts :checkout) (string= start "HEAD")
                                (not detach) (not new) old-name)
                           (concat "refs/heads/" old-name))
                          ((and (not detach) (not new) local-hex) local-ref))))
          (unless hex
            (ngit--die "fatal: invalid reference: %s" start))
          (when (and (not (plist-get opts :checkout)) (not detach) (not new) (null ref))
            (ngit-native-write-stderr
             (format "fatal: a branch is expected, got commit '%s'\n" start))
            (ngit--die-with 128
                            "hint: If you want to detach HEAD at the commit, try again with the --detach option."))
          (list (or new (and ref local-name) start)
                (or (and new (concat "refs/heads/" new)) ref)
                hex start (and new t) forcing nil
               (or detach (and (plist-get opts :checkout) (null ref) (null new)
                               (not (string= start "HEAD"))))))))))

(defun ngit--switch-write-head (repo ref hex old-hex msg &optional expected)
  (let* ((file (ngit--ref-file repo "HEAD"))
         (lock (ngit-lock file)) input)
    (unwind-protect
        (progn
          (setq input (ngit-ref-prepare repo (list (list expected (if ref (concat "ref:" ref) hex)
                                                           "HEAD")) nil (list lock)))
          (ngit-commit-lock lock (if ref (concat "ref: " ref "\n") (concat hex "\n")))
          (setq lock nil)
          (ngit--append-reflog repo "HEAD" old-hex hex (ngit-ident repo "COMMITTER") msg)
          (ngit-ref-finish repo input))
      (when lock (ngit-unlock lock)))))

(defun ngit--switch-write-orphan-head (repo ref)
  "Point HEAD symbolically at unborn REF without adding a reflog record."
  (ngit-ref-write repo "HEAD" (concat "ref: " ref)))

(defun ngit--switch-detach-advice (name)
  (ngit-native-write-stderr
   (concat "Note: switching to '" name "'.\n\n"
           "You are in 'detached HEAD' state. You can look around, make experimental\n"
           "changes and commit them, and you can discard any commits you make in this\n"
           "state without impacting any branches by switching back to a branch.\n\n"
           "If you want to create a new branch to retain commits you create, you may\n"
           "do so (now or later) by using -c with the switch command. Example:\n\n"
           "  git switch -c <new-branch-name>\n\n"
           "Or undo this operation with:\n\n"
           "  git switch -\n\n"
           "Turn off this advice by setting config variable advice.detachedHead to false\n\n")))

(defun ngit--switch-reachable-p (repo commit targets)
  (let ((found nil) (c (ngit--commit-of repo commit)))
    (dolist (hex targets)
      (let ((target (ngit--commit-of repo hex)))
        (when (and (not found) c target
                   (ngit--in-merge-bases repo c target))
          (setq found t))))
    found))

(defun ngit--switch-unreachable-commits (repo old-hex targets)
  "Return commits from OLD-HEX outside TARGETS in Git's date order."
  (let ((starts (list (cons (ngit--commit-of repo old-hex) 0))))
    (dolist (hex targets)
      (let ((commit (ngit--commit-of repo hex)))
        (when commit (push (cons commit ngit-uninteresting) starts))))
    ;; Revision state and cached object flags belong to this one walk.
    (let ((ngit--commit-table (make-hash-table :test 'equal))
          (ngit--rv-repo nil) (ngit--rv-commits nil)
          (ngit--rv-limited nil) (ngit--rv-first-parent nil)
          (ngit--rv-paths nil) (ngit--rv-dense t)
          (ngit--rv-simplify-history t) (ngit--rv-rewrite-parents nil)
          (ngit--rv-topo nil) (ngit--rv-max-age nil) (ngit--rv-min-age nil)
          (ngit--rv-min-parents 0) (ngit--rv-max-parents -1)
          (ngit--rv-grep nil) (ngit--rv-max-count -1) (ngit--rv-skip 0)
          (ngit--rv-reverse nil) (ngit--rv-boundary nil)
          (ngit--rv-no-walk nil) (lost nil) commit)
      ;; Recreate STARTS in the walk-local commit table.
      (setq starts
            (mapcar (lambda (start)
                      (cons (ngit-commit (ngit--c-hex (car start))) (cdr start)))
                    starts))
      (ngit-walk-prepare repo (nreverse starts))
      (while (setq commit (ngit-walk-next))
        (push (ngit--c-hex commit) lost))
      (nreverse lost))))

(defun ngit--switch-leaving-detached (repo old-hex new-hex quiet)
  (unless (or quiet (null old-hex) (equal old-hex new-hex))
    (let ((targets nil))
      (dolist (r (ngit-list-refs repo))
        (push (nth 1 r) targets))
      (when new-hex (push new-hex targets))
      (if (ngit--switch-reachable-p repo old-hex targets)
          (ngit-native-write-stderr
           (concat "Previous HEAD position was " (ngit-abbrev repo old-hex) " "
                   (decode-coding-string (ngit--subject-of repo old-hex) 'utf-8) "\n"))
        (let ((abbrev (ngit-abbrev repo old-hex))
              (lost (ngit--switch-unreachable-commits repo old-hex targets)))
          (let* ((count (length lost)) (shown nil) (i 0))
            (dolist (hex lost)
              (when (< i 4)
                (push (concat "  " (ngit-abbrev repo hex) " "
                              (decode-coding-string (ngit--subject-of repo hex) 'utf-8) "\n")
                      shown))
              (setq i (1+ i)))
            (setq shown (nreverse shown))
            (when (= count 5)
              (setq shown (append shown
                                  (list (concat "  " (ngit-abbrev repo (car (last lost))) " "
                                                (decode-coding-string
                                                 (ngit--subject-of repo (car (last lost))) 'utf-8)
                                                "\n")))))
            (when (> count 5)
              (setq shown (append shown (list (format " ... and %d more.\n" (- count 4))))))
          (ngit-native-write-stderr
             (concat "Warning: you are leaving " (number-to-string count) " "
                     (if (= count 1) "commit" "commits") " behind, not connected to\n"
                     "any of your branches:\n\n" (apply #'concat shown) "\n"
                     "If you want to keep " (if (= count 1) "it" "them")
                     " by creating a new branch, this may be a good time\n"
                     "to do so with:\n\n git branch <new-branch-name> " abbrev "\n\n"))))))))

(defun ngit--switch-create-branch (repo name start force quiet track old-ref)
  (let ((ref (concat "refs/heads/" name)))
    (if (and force (equal ref old-ref))
        (let ((r (ngit--dwim-start repo start track))
              (previous (ngit-resolve-ref repo ref)))
          (ngit--write-ref repo ref (car r) previous
                           (concat "branch: Reset to " start) t)
          (ngit--append-reflog repo "HEAD" previous (car r)
                               (ngit-ident repo "COMMITTER")
                               (concat "branch: Reset to " start))
          (when (and (cdr r) (not (eq track 'none)))
            (ngit--setup-tracking repo name (cdr r) track quiet)))
      (ngit--branch-create repo name start force quiet
                           (unless (eq track 'none) track)
                           (or (and old-ref (substring old-ref 11)) "HEAD")))))

(defun ngit--switch-run (repo args checkout)
  (unless (ngit-repo-toplevel repo)
    (ngit--die "fatal: this operation must be run in a work tree"))
  (catch 'ngit-switch-done
   (let* ((opts (ngit--switch-parse args checkout))
         (quiet (plist-get opts :quiet))
         (raw-head (ngit-read-ref repo "HEAD"))
         (old-ref (and raw-head (string-prefix-p "ref: refs/heads/" raw-head)
                       (substring raw-head 5)))
         (old-name (and old-ref (substring old-ref 11)))
         (old-hex (ngit-resolve-ref repo "HEAD")))
    (when (and old-ref (null old-hex)
               (or (plist-get opts :create) (plist-get opts :force-create))
               (null (plist-get opts :pos)))
      (let ((name (or (plist-get opts :create) (plist-get opts :force-create))))
        (ngit--check-branch-name name)
        (when (ngit-resolve-ref repo (concat "refs/heads/" name))
          (ngit--die "fatal: a branch named '%s' already exists" name))
        (ngit--switch-write-orphan-head repo (concat "refs/heads/" name))
        (unless quiet
          (ngit-native-write-stderr (format "Switched to a new branch '%s'\n" name)))
        (throw 'ngit-switch-done
               (if (= (ngit-hook-run repo "post-checkout"
                                     (list (ngit--switch-hook-oid old-hex)
                                           (make-string 40 ?0) "1"))
                      0)
                   0 1))))
    (when (and (not checkout) (null (plist-get opts :pos))
               (null (plist-get opts :create))
               (null (plist-get opts :force-create))
               (null (plist-get opts :orphan))
               (not (plist-get opts :detach)))
      (ngit--die "fatal: missing branch or commit argument"))
    (let* ((resolved (ngit--switch-resolve repo opts old-name old-hex))
           (display (nth 0 resolved)) (ref (nth 1 resolved))
           (hex (nth 2 resolved)) (start (nth 3 resolved))
           (new (nth 4 resolved)) (force-create (nth 5 resolved))
           (orphan (nth 6 resolved)) (detach (nth 7 resolved))
           (noop-head (and checkout (string= display "HEAD") (not detach) (not new)))
           (branch-existed (and ref (ngit-resolve-ref repo ref)))
           (new-tree (and hex (ngit--switch-tree repo hex)))
           (old-tree (and old-hex (ngit--switch-tree repo old-hex)))
           (candidates nil))
      ;; Reject branch/ref problems before the tree merge can write anything.
      (when new
        (ngit--check-branch-name display)
        (when (and branch-existed (not force-create))
          (ngit--die "fatal: a branch named '%s' already exists" display)))
      (when (and ref branch-existed (not (equal ref old-ref)))
        (let ((path (ngit--branch-checked-out repo ref)))
          (when path
            (ngit--die "fatal: '%s' is already used by worktree at '%s'" display path))))
      (if (plist-get opts :discard)
          (progn
            (unless (plist-get opts :force)
              (ngit--switch-protect-untracked
               repo old-tree new-tree (if checkout "checkout" "switch")))
            (ngit--reset-hard
             repo new-tree start
             (lambda (entries)
               (ngit--switch-clear-target-directories repo entries))))
        (let ((ngit--switch-allow-merge (plist-get opts :merge)))
          (setq candidates
                (catch 'ngit-switch-merge-needed
                  (ngit--switch-twoway repo old-tree new-tree (if checkout "checkout" "switch")))))
        (when (eq candidates 'merge)
          (setq candidates (ngit--switch-merge-local repo old-tree new-tree display quiet (plist-get opts :style) (or old-name (ngit-abbrev repo old-hex))))))
      (unless (eq candidates 'merged)
        (ngit--switch-show-local repo new-tree (or quiet (plist-get opts :discard)) candidates))
      (ngit--flush)
      (when (and (null old-ref) old-hex)
        (ngit--switch-leaving-detached repo old-hex hex quiet))
      (when new
        (unless orphan
          (ngit--switch-create-branch repo display start force-create quiet
                                      (plist-get opts :track) old-ref)
          (ngit--flush)))
      (let* ((old-desc (or old-name old-hex "(invalid)"))
             (action (getenv "GIT_REFLOG_ACTION"))
             (msg (or action (concat "checkout: moving from " old-desc " to " display))))
        ;; `checkout HEAD' is the one no-op expression that does not detach.
        (if orphan
            (ngit--switch-write-orphan-head repo ref)
          (unless (and checkout (string= display "HEAD") (not detach) (not new))
            (ngit--switch-write-head
             repo (and (not detach) ref) hex
             (if (and force-create (equal ref old-ref)) hex old-hex) msg))))
      (unless quiet
        (unless noop-head
          (cond
           (detach
            (when (and checkout old-ref (not (plist-get opts :detach))
                       (ngit-config-bool repo "advice.detachedhead" t))
              (ngit--switch-detach-advice start))
            (ngit-native-write-stderr
             (concat "HEAD is now at " (ngit-abbrev repo hex) " "
                     (decode-coding-string (ngit--subject-of repo hex) 'utf-8) "\n")))
           ((and old-ref (equal old-ref ref))
            (ngit-native-write-stderr
             (format (if force-create "Reset branch '%s'\n" "Already on '%s'\n") display)))
           (new
            (ngit-native-write-stderr
             (format (if (and force-create branch-existed)
                         "Switched to and reset branch '%s'\n"
                       "Switched to a new branch '%s'\n") display)))
           (t (ngit-native-write-stderr (format "Switched to branch '%s'\n" display)))))
        (when (and ref (not detach)
                   (or (not new) (and force-create branch-existed)))
          (let ((info (ngit--switch-tracking-info
                       repo (if noop-head old-name display))))
            (when info (ngit--emit info)))))
      (ngit-commit-clear-merge-state repo)
      (ngit-ref-delete-state repo "AUTO_MERGE")
      (if (= (ngit-hook-run repo "post-checkout"
                            (list (ngit--switch-hook-oid old-hex)
                                  (ngit--switch-hook-oid hex) "1"))
             0)
          0 1)))))

(defun ngit-cmd-switch (repo args)
  (ngit--switch-run repo args nil))

(defun ngit--checkout-option-p (a)
  (or (member a '("-b" "-B" "--orphan" "-d" "--detach" "-t" "--track"
                   "--track=direct" "--track=inherit" "--no-track"))
      (string-prefix-p "-b" a) (string-prefix-p "-B" a)
      (string-prefix-p "--orphan=" a)))

(defun ngit-dispatch-checkout (repo args)
  "Dispatch checkout ARGS between path checkout and branch switching."
  (let ((before nil) (after nil) (explicit nil) (scan args)
        (switch-option nil) (guess t))
    (while scan
      (let ((a (pop scan)))
        (if (string= a "--")
            (setq explicit t after scan scan nil)
          (when (ngit--checkout-option-p a) (setq switch-option t))
          (when (string= a "--no-guess") (setq guess nil))
          (push a before))))
    (setq before (nreverse before))
    (let ((plain nil) (skip nil))
      (dolist (a before)
        (cond (skip (setq skip nil))
              ((member a '("-b" "-B" "--orphan")) (setq skip t))
              ((member a '("-q" "--quiet" "-d" "--detach" "-t" "--track"
                           "--track=direct" "--track=inherit" "--no-track" "-f" "--force")))
              ((string-prefix-p "-" a) nil)
              (t (push a plain))))
      (setq plain (nreverse plain))
      (cond
       ((and explicit after) (ngit--checkout-path-command repo args))
       ((and explicit (null after) (= (length plain) 1)
             (condition-case nil (ngit-peel repo (ngit-rev-parse repo (car plain)) 'commit)
               (error nil)))
        (ngit--switch-run repo before t))
       ((and explicit (null plain)) (ngit--checkout-path-command repo args))
       (switch-option (ngit--switch-run repo args t))
       ((null plain) (ngit--switch-run repo args t))
       ((> (length plain) 1)
        (let ((tree (condition-case nil
                        (ngit-peel repo (ngit-rev-parse repo (car plain)) 'tree)
                      (error nil))))
          (if tree
              (progn
                (when (or (file-exists-p (car plain)) (file-symlink-p (car plain)))
                  (ngit--die "fatal: ambiguous argument '%s': both revision and filename\nUse '--' to separate paths from revisions, like this:\n'git <command> [<revision>...] -- [<file>...]'"
                             (car plain)))
                (ngit--checkout-path-command repo args))
            (ngit--checkout-path-command repo args))))
       (t
        (let* ((name (car plain))
               (commit (condition-case nil
                           (ngit-peel repo (ngit-rev-parse repo name) 'commit)
                         (error nil)))
               (file (or (file-exists-p name) (file-symlink-p name))))
          (if commit
              (ngit--switch-run repo args t)
            (let ((remote (and guess (ngit--switch-remote-candidate repo name))))
              (cond ((and remote file)
                     (ngit--die "fatal: '%s' could be both a local file and a tracking branch.\nPlease use -- (and optionally --no-guess) to disambiguate" name))
                    (remote (ngit--switch-run repo args t))
                    (t (ngit--checkout-path-command repo args)))))))))))

(provide 'ngit-switch)

;;; ngit-switch.el ends here
