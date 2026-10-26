;;; ngit-status.el --- status, ls-files, config and check-attr -*- lexical-binding: t; -*-

;;; Commentary:

;; `status' in the long, short (-s) and porcelain v1 formats with git's
;; wording (wt-status.c, remote.c format_tracking_info), plus the small
;; inspection commands used to verify the index, config and attribute
;; layers against git.

;;; Code:

(require 'ngit-cli)
(require 'ngit-changes)

;;;; ls-files / config / check-attr

(defun ngit--octal6 (mode)
  (let ((s (format "%o" mode)))
    (concat (make-string (max 0 (- 6 (length s))) ?0) s)))

(defconst ngit--quote-specials
  (let ((l (list "\"" "\\" (string 127))) (c 0))
    (while (< c 32) (push (string c) l) (setq c (1+ c)))
    l)
  "Single-byte strings whose presence forces C-style quoting (besides
bytes >= 128).")

(defun ngit--any-literal-p (literals s)
  "Non-nil when any string in LITERALS occurs in S.
NeLisp scans literal regexps quickly but character classes at ~0.5 ms
per byte, so each literal is searched on its own."
  (let ((found nil))
    (while (and literals (not found))
      (setq found (string-match-p (regexp-quote (car literals)) s)
            literals (cdr literals)))
    found))

(defun ngit-quote-paths (names)
  "Return NAMES (a vector) with each element quoted like `ngit-quote-path'.
Scanning every path byte by byte costs ~2.5 ms each in NeLisp, so valid
UTF-8 is detected by decoding and the remaining paths are searched as
one concatenation for each byte that needs quoting."
  (let* ((n (length names))
         (out (make-vector n nil))
         (plain nil) (i 0))
    (while (< i n)
      (let ((name (aref names i)))
        (if (= (length (decode-coding-string name 'utf-8)) (length name))
            (push name plain)
          (aset out i (ngit-quote-path name))))
      (setq i (1+ i)))
    (let* ((blob (apply #'concat plain))
           (special (ngit--any-literal-p ngit--quote-specials blob)))
      (setq i 0)
      (while (< i n)
        (unless (aref out i)
          (aset out i (if special (ngit-quote-path (aref names i)) (aref names i))))
        (setq i (1+ i))))
    out))

(defun ngit-cmd-ls-files (repo args)
  "Use the shared selector and invocation-relative listing implementation."
  (require 'ngit-ls-files)
  (ngit-cmd-list-files repo args))

(defun ngit-cmd-config (repo args)
  (let ((key (car (last args))))
    (unless (member "--get" args) (ngit--die "only config --get is supported"))
    (let ((v (ngit-config-get repo key)))
      (cond ((null v) 1)
            (t (ngit--emit (if (eq v t) "true" v) "\n") 0)))))

(defun ngit-cmd-check-attr (repo args)
  (let* ((sep (member "--" args))
         (names (if sep (butlast args (length sep)) (list (car args))))
         (paths (if sep (cdr sep) (cdr args))))
    (dolist (p paths)
      (let ((bp (encode-coding-string p 'utf-8)))
        (dolist (a (ngit-attrs repo bp names))
          (ngit--emit (ngit-quote-path bp) ": " (car a) ": "
                      (let ((v (cdr a)))
                        (cond ((eq v 'set) "set") ((eq v 'unset) "unset")
                              ((eq v 'unspecified) "unspecified") (t v)))
                      "\n"))))
    0))

;;;; Branch and tracking information

(defun ngit--upstream (repo branch)
  "Return the upstream ref of BRANCH (a short name) or nil."
  (let ((remote (ngit-config-get repo (concat "branch." branch ".remote")))
        (merge (ngit-config-get repo (concat "branch." branch ".merge"))))
    (when (and (stringp merge) (stringp remote))
      (if (string= remote ".")
          merge
        (let ((found nil))
          (dolist (spec (ngit-config-get-all repo (concat "remote." remote ".fetch")))
            (when (and (not found) (stringp spec))
              (let* ((spec (string-remove-prefix "+" spec))
                     (colon (string-search ":" spec))
                     (src (and colon (substring spec 0 colon)))
                     (dst (and colon (substring spec (1+ colon)))))
                (when src
                  (let ((star (string-search "*" src)))
                    (if star
                        (let ((pre (substring src 0 star)) (suf (substring src (1+ star))))
                          (when (and (string-prefix-p pre merge) (string-suffix-p suf merge)
                                     (>= (length merge) (+ (length pre) (length suf))))
                            (let ((mid (substring merge (length pre) (- (length merge) (length suf))))
                                  (dstar (string-search "*" dst)))
                              (setq found (if dstar
                                              (concat (substring dst 0 dstar) mid
                                                      (substring dst (1+ dstar)))
                                            dst)))))
                      (when (string= src merge) (setq found dst))))))))
          found)))))

(defun ngit-shorten-ref (repo ref)
  "Shorten REF unambiguously, like git's shorten_unambiguous_ref."
  (let ((rules (reverse ngit--ref-rules)) (result nil) (n (length ngit--ref-rules)))
    (while (and rules (not result))
      (let* ((rule (car rules))
             (pos (string-search "%s" rule))
             (pre (substring rule 0 pos)) (suf (substring rule (+ pos 2)))
             (i (1- (length rules))))
        (when (and (> i 0) (string-prefix-p pre ref) (string-suffix-p suf ref)
                   (> (length ref) (+ (length pre) (length suf))))
          (let ((short (substring ref (length pre) (- (length ref) (length suf))))
                (ambiguous nil) (j 0))
            (while (and (< j n) (not ambiguous))
              (unless (= j i)
                (when (< j i)
                  (when (ngit-resolve-ref repo (format (nth j ngit--ref-rules) short))
                    (setq ambiguous t))))
              (setq j (1+ j)))
            (unless ambiguous (setq result short)))))
      (setq rules (cdr rules)))
    (or result ref)))

(defun ngit--commit-time (repo bin)
  (ngit--commit-date (ngit-parse-commit (cdr (ngit-read-object repo bin)))))

(defun ngit-ahead-behind (repo ours theirs)
  "Return (AHEAD . BEHIND) between commits OURS and THEIRS (20-byte names)."
  (if (equal ours theirs)
      (cons 0 0)
    (let ((flags (make-hash-table :test 'equal))
          (keys nil)
          (queue nil))
      (let ((push-q
             (lambda (bin)
               (let ((item (cons (ngit--commit-time repo bin) bin)))
                 (if (or (null queue) (> (car item) (caar queue)))
                     (push item queue)
                   (let ((cell queue))
                     (while (and (cdr cell) (<= (car item) (car (cadr cell))))
                       (setq cell (cdr cell)))
                     (setcdr cell (cons item (cdr cell)))))))))
        (puthash ours 1 flags) (push ours keys) (funcall push-q ours)
        (unless (gethash theirs flags) (push theirs keys))
        (puthash theirs (logior 2 (gethash theirs flags 0)) flags) (funcall push-q theirs)
        (while (let ((live nil))
                 (dolist (q queue) (unless (= (gethash (cdr q) flags) 3) (setq live t)))
                 live)
          (let* ((item (pop queue)) (f (gethash (cdr item) flags)))
            (dolist (p (ngit-commit-parents repo (ngit-hex (cdr item))))
              (let* ((pb (ngit-unhex p)) (old (gethash pb flags 0)) (new (logior old f)))
                (unless (= old new)
                  (when (= old 0) (push pb keys))
                  (puthash pb new flags)
                  (funcall push-q pb))))))
        (let ((ahead 0) (behind 0))
          ;; `maphash' is slow in NeLisp; walk the recorded keys.
          (dolist (k keys)
            (let ((f (gethash k flags)))
              (cond ((= f 1) (setq ahead (1+ ahead)))
                    ((= f 2) (setq behind (1+ behind))))))
          (cons ahead behind))))))

(defun ngit--plural (n one many) (if (= n 1) one many))

(defun ngit-tracking-info (repo branch)
  "Return the tracking paragraph for BRANCH, or nil."
  (let ((up (ngit--upstream repo branch)))
    (when up
      (let ((base (ngit-shorten-ref repo up))
            (theirs (ngit-resolve-ref repo up))
            (ours (ngit-resolve-ref repo (concat "refs/heads/" branch))))
        (if (null theirs)
            (concat "Your branch is based on '" base "', but the upstream is gone.\n"
                    "  (use \"git branch --unset-upstream\" to fixup)\n")
          (let* ((ab (ngit-ahead-behind repo (ngit-unhex ours) (ngit-unhex theirs)))
                 (a (car ab)) (b (cdr ab)))
            (cond
             ((and (= a 0) (= b 0))
              (concat "Your branch is up to date with '" base "'.\n"))
             ((= b 0)
              (concat "Your branch is ahead of '" base "' by " (number-to-string a)
                      (ngit--plural a " commit.\n" " commits.\n")
                      "  (use \"git push\" to publish your local commits)\n"))
             ((= a 0)
              (concat "Your branch is behind '" base "' by " (number-to-string b)
                      (ngit--plural b " commit, and can be fast-forwarded.\n"
                                    " commits, and can be fast-forwarded.\n")
                      "  (use \"git pull\" to update your local branch)\n"))
             (t
              (concat "Your branch and '" base "' have diverged,\n"
                      "and have " (number-to-string a) " and " (number-to-string b)
                      (ngit--plural (+ a b) " different commit each, respectively.\n"
                                    " different commits each, respectively.\n")
                      "  (use \"git pull\" if you want to integrate the remote branch with yours)\n")))))))))

(defun ngit-detached-from (repo head)
  "Return (NAME . AT-P) describing a detached HEAD from the HEAD reflog."
  (let ((log (expand-file-name "logs/HEAD" (ngit-repo-gitdir repo)))
        (found nil))
    (when (file-exists-p log)
      (dolist (line (reverse (split-string (ngit--read-file log) "\n" t)))
        (when (and (not found)
                   (string-match "\\`[0-9a-f]+ \\([0-9a-f]+\\) [^\t]*\tcheckout: moving from .* to \\(.*\\)\\'" line))
          (setq found (cons (match-string 1 line) (match-string 2 line))))))
    (when found
      (let* ((noid (car found)) (target (cdr found))
             (dwim (and (not (equal target "HEAD")) (ngit-dwim-ref repo target)))
             (name (if (and dwim
                            (or (string= (cdr dwim) noid)
                                (equal (ignore-errors (ngit-peel repo (cdr dwim) 'commit)) noid)))
                       (let ((r (car dwim)))
                         (cond ((string-prefix-p "refs/tags/" r) (substring r 10))
                               ((string-prefix-p "refs/remotes/" r) (substring r 13))
                               (t r)))
                     (ngit-abbrev repo noid))))
        (cons name (string= noid head))))))

;;;; Collecting

(defvar ngit--status-rename-override 'default
  "Command-local status rename setting, or default to use configuration.")

(defun ngit--status-renames-p (repo)
  (if (not (eq ngit--status-rename-override 'default))
      ngit--status-rename-override
    (let ((key (if (ngit-config-get repo "status.renames") "status.renames" "diff.renames")))
      (ngit-config-renames-p repo key t))))

(defvar ngit--status-untracked-mode "normal")
(defvar ngit--status-ignored-mode "no")

(defun ngit--status-other-files (repo items ignored)
  "Collect expanded or collapsed other paths for this status invocation."
  (require 'ngit-ls-files)
  (let* ((top (file-name-as-directory (ngit-repo-toplevel repo)))
         (entries (ngit-index-entries repo))
         (directory (and (not (equal ngit--status-untracked-mode "all")) t)))
    (ngit-wt-setup top)
    (sort (seq-filter
           (lambda (p) (or (ngit-paths-match-p items p)
                           (and (string-suffix-p "/" p)
                                (ngit-pathspec-untracked-class items (substring p 0 -1)))))
           (ngit-ls-files--walk repo top entries "" (ngit-ignore-global-patterns repo)
                                t ignored nil nil directory
                                (not (and ignored (equal ngit--status-ignored-mode "matching")))
                                (and ignored (if (equal ngit--status-ignored-mode "matching") t 'traditional))))
          #'string<)))

(defun ngit-status-collect (repo &optional items)
  "Return branch, merge state and staged, unmerged, unstaged and untracked paths."
  (let* ((head-raw (ngit-read-ref repo "HEAD"))
         (branch (and head-raw (string-prefix-p "ref: refs/heads/" head-raw)
                      (substring head-raw 16)))
         (head (ngit-resolve-ref repo "HEAD"))
         (tree (and head (ngit-unhex (ngit-peel repo head 'tree))))
         (renames (ngit--status-renames-p repo))
         (staged (ngit-paths-filter-changes items (ngit-diff-tree-index repo tree)))
         (unstaged (ngit-paths-filter-changes items (ngit-wt-changes repo renames t)))
         (untracked (unless (equal ngit--status-untracked-mode "no")
                      (if (equal ngit--status-untracked-mode "all")
                          (ngit--status-other-files repo items nil)
                        (ngit--status-untracked repo items))))
         (ignored (when (and (not (equal ngit--status-untracked-mode "no"))
                             (not (equal ngit--status-ignored-mode "no")))
                    (ngit--status-other-files repo items t)))
         (unmerged nil))
    (when renames
      (require 'ngit-rename)
      (let ((limit (ngit-config-int repo "status.renameLimit" nil)))
        (setq staged (ngit-diffcore-rename repo staged limit)
              unstaged (ngit-diffcore-rename repo unstaged limit))))
    (setq staged (delq nil (mapcar (lambda (c)
                                    (if (= (aref c 1) ?U)
                                        (progn (push c unmerged) nil)
                                      c)) staged))
          unstaged (seq-filter (lambda (c) (/= (aref c 1) ?U)) unstaged))
    (list :branch branch :head head :staged staged :unstaged unstaged
          :untracked untracked :ignored ignored :unmerged (nreverse unmerged)
          :merging (file-exists-p (expand-file-name "MERGE_HEAD" (ngit-repo-gitdir repo))))))

(defun ngit--status-directory-warning (repo items)
  "Report an inaccessible common literal directory prefix as Git does."
  (when (and items (not (ngit-pathspec-magic-p items)))
    (let ((prefix (car items)))
      (dolist (item (cdr items))
        (let ((n 0) (limit (min (length prefix) (length item))))
          (while (and (< n limit) (= (aref prefix n) (aref item n)))
            (setq n (1+ n)))
          (setq prefix (substring prefix 0 n))))
      (let ((slash (string-match "/[^/]*\\'" prefix)))
        (when slash
          (setq prefix (substring prefix 0 (1+ slash)))
          (ngit-wt-setup (ngit-repo-toplevel repo))
          (let ((path "") (reason nil))
            (catch 'done
              (dolist (part (split-string prefix "/" t))
                (setq path (concat path part))
                (let* ((st (ngit-wt-lstat path))
                       (mode (and st (logand (aref st 0) ngit-s-ifmt))))
                  ;; Git never traverses a symbolic link in the prefix.
                  (when (eq mode ngit-s-iflnk) (throw 'done nil))
                  (unless (eq mode ngit-s-ifdir)
                    (setq reason (if st "Not a directory" "No such file or directory"))
                    (throw 'done nil)))
                (setq path (concat path "/"))))
            (when reason
              (ngit-native-write-stderr
               (format "warning: could not open directory '%s': %s\n"
                       (decode-coding-string prefix 'utf-8) reason)))))))))

(defun ngit--status-untracked (repo items)
  "Select untracked paths, expanding a collapsed ancestor of an ITEM."
  (ngit--status-directory-warning repo items)
  (if (ngit-pathspec-magic-p items)
      (let* ((top (file-name-as-directory (ngit-repo-toplevel repo)))
             (ngit--untracked-pathspecs (and (ngit-pathspec-attr-p items) items))
             (tracked (ngit--tracked-set (ngit-index-entries repo))) (out nil))
        (ngit-wt-setup top)
        (dolist (path (ngit-untracked repo))
          (if (ngit-pathspec-match-p items path)
              (push path out)
            (when (string-suffix-p "/" path)
              (ngit--walk-untracked repo top tracked path (ngit-ignore-global-patterns repo)
                                    (lambda (file) (when (ngit-pathspec-match-p items file) (push file out))) nil t))))
        (sort out #'string<))
   (let ((paths (ngit-untracked repo)) (expanded nil) (scanned nil) (out nil))
    (dolist (path paths)
      (let ((directory (string-suffix-p "/" path)))
        (if (and directory
                 (seq-some (lambda (item) (and (> (length item) (length path))
                                               (string-prefix-p path item))) items))
            (progn
              (unless scanned
                (let* ((top (file-name-as-directory (ngit-repo-toplevel repo)))
                       (tracked (ngit--tracked-set (ngit-index-entries repo))))
                  (ngit-wt-setup top)
                  (ngit--walk-untracked repo top tracked "" (ngit-ignore-global-patterns repo)
                                        (lambda (p) (push p expanded)) nil t))
                (setq scanned t))
              (dolist (p expanded)
                (when (and (string-prefix-p path p) (ngit-paths-match-p items p))
                  (let ((dir path))
                    ;; An explicit descendant forces traversal even when
                    ;; a second pathspec also selects its parent.
                    (while (and dir (seq-some (lambda (item) (and (> (length item) (length dir))
                                                                 (string-prefix-p dir item))) items))
                      (let ((slash (string-search "/" p (length dir))))
                        (setq dir (and slash (substring p 0 (1+ slash))))))
                    (push (or dir p) out)))))
          (when (ngit-paths-match-p items path) (push path out)))))
    (sort (delete-dups out) #'string<))))

(defvar ngit--status-prefix "" "Root-relative prefix for human status output.")

(defun ngit--status-path (path)
  "Convert PATH only for the current human status invocation."
  (ngit-paths-relative ngit--status-prefix path))

(defun ngit--status-quote (path)
  "quote_path with QUOTE_PATH_QUOTE_SP: paths with a space are quoted."
  (let ((q (ngit-quote-path path)))
    (if (and (eq q path) (string-search " " path))
        (concat "\"" path "\"")
      q)))

;;;; Short and porcelain formats

(defun ngit--status-short (st porcelain)
  (let ((table (make-hash-table :test 'equal)) (paths nil))
    (dolist (c (plist-get st :staged))
      (puthash (aref c 0) (vector (aref c 1) ?\s (aref c 8)) table)
      (push (aref c 0) paths))
    (dolist (c (plist-get st :unstaged))
      (let* ((slot (gethash (aref c 0) table))
             (flags (aref c 6))
             (y (cond ((= (aref c 1) ?R) ?R)
                      ((memq 'ita (if (listp flags) flags (list flags))) ?A)
                      ((and (not porcelain) (listp flags) flags)
                       (cond ((memq 'new-commits flags) ?M)
                             ((memq 'modified flags) ?m)
                             ((memq 'untracked flags) ??)
                             (t (aref c 1))))
                      (t (aref c 1)))))
        (if slot
            (aset slot 1 y)
          (puthash (aref c 0) (vector ?\s y (aref c 8)) table)
          (push (aref c 0) paths))))
    (dolist (c (plist-get st :unmerged))
      (let ((xy (aref [nil "DD" "AU" "UD" "UA" "DU" "AA" "UU"]
                      (ngit-index-stage-mask (aref c 6)))))
        (puthash (aref c 0) (vector (aref xy 0) (aref xy 1) nil) table)
        (push (aref c 0) paths)))
    (dolist (p (sort paths #'string<))
      (let ((v (gethash p table)))
        (ngit--emit (string (aref v 0) (aref v 1)) " "
                    (if (aref v 2) (concat (ngit--status-quote (ngit--status-path (aref v 2))) " -> ") "")
                    (ngit--status-quote (ngit--status-path p)) "\n")))
    (dolist (u (plist-get st :untracked))
      (ngit--emit "?? " (ngit--status-quote (ngit--status-path u)) "\n"))
    (dolist (u (plist-get st :ignored))
      (ngit--emit "!! " (ngit--status-quote (ngit--status-path u)) "\n"))))

;;;; Long format

(defconst ngit--status-labels
  '((?A . "new file:") (?C . "copied:") (?D . "deleted:") (?M . "modified:")
    (?R . "renamed:") (?T . "typechange:") (?X . "unknown:") (?U . "unmerged:")))

(defun ngit--status-line (status path old extra)
  (let ((label (cdr (assq status ngit--status-labels))))
    (concat "\t" label (make-string (- 12 (length label)) ?\s)
            (if old (concat (ngit-quote-path (ngit--status-path old)) " -> ") "")
            (ngit-quote-path (ngit--status-path path)) extra "\n")))

(defun ngit--submodule-extra (flags)
  (if (and (listp flags) flags)
      (let ((parts nil))
        (when (memq 'new-commits flags) (push "new commits" parts))
        (when (memq 'modified flags) (push "modified content" parts))
        (when (memq 'untracked flags) (push "untracked content" parts))
        (concat " (" (mapconcat #'identity (nreverse parts) ", ") ")"))
    ""))

(defun ngit--status-long (repo st)
  (let* ((branch (plist-get st :branch))
         (head (plist-get st :head))
         (staged (plist-get st :staged))
         (unstaged (plist-get st :unstaged))
         (unmerged (plist-get st :unmerged))
         (merging (plist-get st :merging))
         (picking (let ((path (expand-file-name "CHERRY_PICK_HEAD" (ngit-repo-gitdir repo))))
                    (and (file-exists-p path) (string-trim (ngit--read-file path)))))
         (reverting (or (file-exists-p (expand-file-name "REVERT_HEAD" (ngit-repo-gitdir repo)))
                        ;; sequencer_get_last_command: a range in progress
                        ;; whose next todo command is a revert.
                        (let ((todo (expand-file-name "sequencer/todo" (ngit-repo-gitdir repo))))
                          (and (file-exists-p todo)
                               (string-match-p "\\`[ \t\r\n]*r\\(?:evert\\)?[ \t]"
                                               (ngit--read-file todo))))))
         (untracked (plist-get st :untracked)))
    (if branch
        (progn
          (ngit--emit "On branch " branch "\n")
          (when head
            (let ((info (ngit-tracking-info repo branch)))
              (when info (ngit--emit info "\n")))))
      (let ((d (ngit-detached-from repo head)))
        (if d
            (ngit--emit (if (cdr d) "HEAD detached at " "HEAD detached from ") (car d) "\n")
          (ngit--emit "Not currently on any branch.\n"))))
    (unless head
      (ngit--emit "\nNo commits yet\n\n"))
    (when merging
      (ngit--emit
       (if unmerged
           "You have unmerged paths.\n  (fix conflicts and run \"git commit\")\n  (use \"git merge --abort\" to abort the merge)\n\n"
         "All conflicts fixed but you are still merging.\n  (use \"git commit\" to conclude merge)\n\n")))
    (when picking
      (ngit--emit "You are currently cherry-picking commit " (ngit-abbrev repo picking) ".\n"
                  (if unmerged "  (fix conflicts and run \"git cherry-pick --continue\")\n"
                    "  (all conflicts fixed: run \"git cherry-pick --continue\")\n")
                  "  (use \"git cherry-pick --skip\" to skip this patch)\n"
                  "  (use \"git cherry-pick --abort\" to cancel the cherry-pick operation)\n\n"))
    (when reverting
      (ngit--emit "Revert currently in progress.\n"
                  (if unmerged "  (fix conflicts and run \"git revert --continue\")\n"
                    "  (run \"git revert --continue\" to continue)\n")
                  "  (use \"git revert --skip\" to skip this patch)\n"
                  "  (use \"git revert --abort\" to cancel the revert operation)\n\n"))
    (when staged
      (ngit--emit "Changes to be committed:\n"
                  (cond (merging "")
                        (head "  (use \"git restore --staged <file>...\" to unstage)\n")
                        (t "  (use \"git rm --cached <file>...\" to unstage)\n")))
      (dolist (c staged)
        (ngit--emit (ngit--status-line (aref c 1) (aref c 0) (aref c 8) "")))
      (ngit--emit "\n"))
    (when unmerged
      (let ((deleted nil) (only-deleted t))
        (dolist (c unmerged)
          (let ((mask (ngit-index-stage-mask (aref c 6))))
            (when (memq mask '(1 3 5)) (setq deleted t))
            (unless (= mask 1) (setq only-deleted nil))))
        (ngit--emit "Unmerged paths:\n"
                    (cond (merging "")
                          (head "  (use \"git restore --staged <file>...\" to unstage)\n")
                          (t "  (use \"git rm --cached <file>...\" to unstage)\n"))
                    (cond (only-deleted "  (use \"git rm <file>...\" to mark resolution)\n")
                          (deleted "  (use \"git add/rm <file>...\" as appropriate to mark resolution)\n")
                          (t "  (use \"git add <file>...\" to mark resolution)\n")))
        (dolist (c unmerged)
          (let ((label (aref [nil "both deleted:" "added by us:" "deleted by them:"
                                 "added by them:" "deleted by us:" "both added:" "both modified:"]
                             (ngit-index-stage-mask (aref c 6)))))
            (ngit--emit "\t" label (make-string (- 17 (length label)) ?\s)
                        (ngit-quote-path (ngit--status-path (aref c 0))) "\n")))
        (ngit--emit "\n")))
    (when unstaged
      (let ((has-deleted nil) (dirty-sub nil))
        (dolist (c unstaged)
          (when (= (aref c 1) ?D) (setq has-deleted t))
          (let ((f (aref c 6)))
            (when (and (listp f) (or (memq 'modified f) (memq 'untracked f)))
              (setq dirty-sub t))))
        (ngit--emit "Changes not staged for commit:\n"
                    (if has-deleted
                        "  (use \"git add/rm <file>...\" to update what will be committed)\n"
                      "  (use \"git add <file>...\" to update what will be committed)\n")
                    "  (use \"git restore <file>...\" to discard changes in working directory)\n")
        (when dirty-sub
          (ngit--emit "  (commit or discard the untracked or modified content in submodules)\n"))
        (dolist (c unstaged)
          (let ((f (aref c 6)))
            (ngit--emit (ngit--status-line (if (and (eq f 'ita) (/= (aref c 1) ?R)) ?A (aref c 1))
                                           (aref c 0) (aref c 8)
                                           (ngit--submodule-extra f)))))
        (ngit--emit "\n")))
    (when untracked
      (ngit--emit "Untracked files:\n"
                  "  (use \"git add <file>...\" to include in what will be committed)\n")
      (dolist (u untracked) (ngit--emit "\t" (ngit-quote-path (ngit--status-path u)) "\n"))
      (ngit--emit "\n"))
    (when (plist-get st :ignored)
      (ngit--emit "Ignored files:\n"
                  "  (use \"git add -f <file>...\" to include in what will be committed)\n")
      (dolist (p (plist-get st :ignored))
        (ngit--emit "\t" (ngit-quote-path (ngit--status-path p)) "\n"))
      (ngit--emit "\n"))
    (unless (or staged (and merging (null unmerged)))
      (ngit--emit
       (cond ((or unstaged unmerged) "no changes added to commit (use \"git add\" and/or \"git commit -a\")\n")
             (untracked "nothing added to commit but untracked files present (use \"git add\" to track)\n")
             ((equal ngit--status-untracked-mode "no")
              "nothing to commit (use -u to show untracked files)\n")
             ((null head) "nothing to commit (create/copy files and use \"git add\" to track)\n")
             (t "nothing to commit, working tree clean\n"))))))

(defun ngit--status-v2 (repo st)
  "Emit porcelain v2 records from the same collected status as v1."
  (let ((staged (make-hash-table :test 'equal))
        (unstaged (make-hash-table :test 'equal)) (paths nil)
        (entries (ngit-index-entries repo)) (zero (make-string 40 ?0)))
    (dolist (c (plist-get st :staged))
      (puthash (aref c 0) c staged) (push (aref c 0) paths))
    (dolist (c (plist-get st :unstaged))
      (puthash (aref c 0) c unstaged) (push (aref c 0) paths))
    (dolist (path (sort (delete-dups paths) #'string<))
      (let* ((x (gethash path staged)) (y (gethash path unstaged))
             (e (ngit-index-lookup entries path))
             (ita (and e (memq 'ita (aref e 5))))
             (hm (if ita 0 (if x (aref x 2) (if e (aref e 2) 0))))
             (im (if ita 0 (if e (aref e 2) 0)))
             (wm (if y (aref y 4) im))
             (hb (unless ita (if x (aref x 3) (and e (aref e 1)))))
             (ib (and e (not ita) (aref e 1)))
             (old (or (and x (aref x 8)) (and y (aref y 8))))
             (flags (and y (aref y 6)))
             (sub (if (= im ngit-mode-gitlink)
                      (concat "S" (if (and (listp flags) (memq 'new-commits flags)) "C" ".")
                              (if (and (listp flags) (memq 'modified flags)) "M" ".")
                              (if (and (listp flags) (memq 'untracked flags)) "U" ".")) "N...")))
        (ngit--emit (if old "2 " "1 ")
                    (string (if x (aref x 1) ?.) (if ita ?A (if y (aref y 1) ?.)))
                    " " sub " " (ngit--octal6 hm) " " (ngit--octal6 im) " " (ngit--octal6 wm)
                    " " (if hb (ngit-hex hb) zero) " " (if ib (ngit-hex ib) zero) " "
                    (if old (let ((rename (if (and x (aref x 8)) x y)))
                              (concat (string (aref rename 1)) (number-to-string (ngit-similarity-percent rename)) " ")) "")
                    (ngit-quote-path path)
                    (if old (concat "\t" (ngit-quote-path old)) "") "\n")))
    (dolist (c (plist-get st :unmerged))
      (let* ((g (aref c 6)) (path (aref c 0)) (stat (ngit-wt-lstat path))
             (xy (aref [nil "DD" "AU" "UD" "UA" "DU" "AA" "UU"] (ngit-index-stage-mask g))))
        (ngit--emit "u " xy " N...")
        (dolist (stage '(1 2 3))
          (ngit--emit " " (ngit--octal6 (if (aref g stage) (aref (aref g stage) 2) 0))))
        (ngit--emit " " (ngit--octal6 (if stat (ngit-wt-mode stat) 0)))
        (dolist (stage '(1 2 3))
          (ngit--emit " " (if (aref g stage) (ngit-hex (aref (aref g stage) 1)) zero)))
        (ngit--emit " " (ngit-quote-path path) "\n")))
    (dolist (p (plist-get st :untracked)) (ngit--emit "? " (ngit-quote-path p) "\n"))
    (dolist (p (plist-get st :ignored)) (ngit--emit "! " (ngit-quote-path p) "\n"))))

(defun ngit-cmd-status (repo args)
  (let ((fmt 'long) (paths nil) (ngit--status-rename-override 'default)
        (ngit--status-untracked-mode (or (ngit-config-get repo "status.showuntrackedfiles") "normal"))
        (ngit--status-ignored-mode "no"))
    (while args
      (let ((a (pop args)))
        (cond ((string= a "--") (setq paths (append paths args) args nil))
              ((member a '("-s" "--short")) (setq fmt 'short))
              ((member a '("--porcelain" "--porcelain=v1")) (setq fmt 'porcelain))
              ((string= a "--porcelain=v2") (setq fmt 'v2))
              ((member a '("-u" "--untracked-files")) (setq ngit--status-untracked-mode "all"))
              ((or (string-prefix-p "--untracked-files=" a) (string-prefix-p "-u" a))
               (setq ngit--status-untracked-mode (substring a (if (string-prefix-p "--" a) 18 2)))
               (unless (member ngit--status-untracked-mode '("all" "normal" "no"))
                 (ngit--die "fatal: Invalid untracked files mode '%s'" ngit--status-untracked-mode)))
              ((or (equal a "--ignored") (string-prefix-p "--ignored=" a))
               (setq ngit--status-ignored-mode (if (equal a "--ignored") "traditional" (substring a 10)))
               (unless (member ngit--status-ignored-mode '("traditional" "matching" "no"))
                 (ngit--die "fatal: Invalid ignored mode '%s'" ngit--status-ignored-mode)))
              ((string= a "--long") (setq fmt 'long))
              ((string= a "--no-renames") (setq ngit--status-rename-override nil))
              ((string= a "--renames") (setq ngit--status-rename-override t))
              ((string-prefix-p "-" a) (ngit--die "unsupported status option %s" a))
              (t (setq paths (append paths (list a)))))))
    (when (and (equal ngit--status-untracked-mode "no") (equal ngit--status-ignored-mode "matching"))
      (ngit--die "fatal: Unsupported combination of ignored and untracked-files arguments"))
    (unless (ngit-repo-toplevel repo)
      (ngit--die "fatal: this operation must be run in a work tree"))
    (let ((st (ngit-status-collect repo (ngit-pathspec-items repo paths)))
          (ngit--status-prefix
           (if (or (memq fmt '(porcelain v2)) (not (ngit-config-bool repo "status.relativepaths" t)))
               "" (ngit-paths-prefix repo))))
      (cond ((eq fmt 'long) (ngit--status-long repo st))
            ((eq fmt 'v2) (ngit--status-v2 repo st))
            (t (ngit--status-short st (eq fmt 'porcelain)))))
    0))

(provide 'ngit-status)

;;; ngit-status.el ends here
