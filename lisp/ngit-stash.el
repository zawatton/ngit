;;; ngit-stash.el --- save and restore local changes -*- lexical-binding: t; -*-

;;; Commentary:

;; Stash objects follow builtin/stash.c.  Temporary indexes share unchanged
;; entries and cache-tree subtrees; only differing paths need tree lookups.
;; Simple applications merge entry modes and object names directly.
;; Divergent paths use the shared tree/content merge engine.

;;; Code:

(require 'ngit-switch)

(defun ngit--stash-parents (repo commit)
  (ngit-parse-commit-object repo commit)
  (mapcar (lambda (c) (aref c 0)) (aref commit 2)))

(defun ngit--stash-log-file (repo)
  (expand-file-name "logs/refs/stash" (ngit-repo-commondir repo)))

(defun ngit--stash-log (repo)
  "Return chronological reflog lines."
  (let ((file (ngit--stash-log-file repo)))
    (and (file-exists-p file) (split-string (ngit--read-file file) "\n" t))))

(defun ngit--stash-commit (repo tree parents message)
  (ngit-hex
   (ngit-write-object
    repo "commit"
    (concat "tree " (ngit-hex tree) "\n"
            (mapconcat (lambda (p) (concat "parent " p "\n")) parents "")
            "author " (ngit-ident repo "AUTHOR") "\n"
            "committer " (ngit-ident repo "COMMITTER") "\n\n" message))))

(defun ngit--stash-covered-p (items path)
  (or (null items) (ngit--checkout-match-p items path)))

(defun ngit--stash-index-matches-p (entries item)
  "Check one pathspec without collecting its matching index entries."
  (let ((path (car item)))
    (if (not (cdr item))
        (or (and (string= path "") (> (length entries) 0))
            (ngit-index-lookup entries path) (ngit-index-dir-p entries (concat path "/")))
      (let ((ngit--rv-paths (list item)) (i 0) (found nil))
        (while (and (not found) (< i (length entries)))
          (setq found (eq (ngit--ps-class (aref (aref entries i) 0) nil) 'full)
                i (1+ i)))
        found))))

(defun ngit--stash-tree (repo index entries changes)
  "Write ENTRIES, reusing INDEX's cache tree except at CHANGES."
  (let ((root (or (ngit-ct-root index) (ngit-ct-new))))
    (dolist (c changes) (ngit-ct-invalidate root (aref c 0)))
    (ngit-ct-update repo root entries)))

(defun ngit--stash-select (repo args)
  "Return (OID REFLOG-INDEX DISPLAY), accepting stash selectors or commits."
  (when (cdr args) (ngit--die-with 1 "Too many revisions specified:%s" (mapconcat #'identity args " ")))
  (let* ((arg (or (car args) "stash@{0}"))
         (n (cond ((member arg '("stash" "refs/stash")) 0)
                  ((string-match "\\`[0-9]+\\'" arg) (string-to-number arg))
                  ((string-match "\\`\\(?:refs/\\)?stash@{\\([0-9]+\\)}\\'" arg)
                   (string-to-number (match-string 1 arg)))))
         (logs (and n (reverse (ngit--stash-log repo))))
         (line (and n (nth n logs)))
         (hex (if n
                  (if line (substring line 41 81)
                    (if (null logs) (ngit--die-with 1 "No stash entries found.")
                      (ngit--die-with 128 "fatal: log for 'stash' only has %d entries" (length logs))))
                (ngit-rev-parse repo arg))))
    (let* ((commit (ngit--commit-of repo hex))
           (parents (ngit--stash-parents repo commit)))
      (unless (and (car parents) (cadr parents))
        (ngit--die-with 1 "'%s' is not a stash-like commit" arg)))
    (list hex n (if (and n (or (null args) (string-match "\\`[0-9]+\\'" arg)))
                    (format "refs/stash@{%d}" n) arg))))

(defun ngit--stash-refresh (repo)
  (let* ((index (ngit-read-index repo)) (entries (aref index 0))
         (checked (make-hash-table :test 'eql)))
    (let ((i 0))
      (while (< i (length entries))
        (when (/= (aref (aref entries i) 3) 0)
          (ngit--die-with 1 "%s: needs merge" (aref (aref entries i) 0)))
        (setq i (1+ i))))
    (ngit--reset-refresh repo index entries checked t)
    (ngit-write-index repo index entries (ngit-ct-root index) checked)
    (ngit-read-index repo)))

(defun ngit--stash-restore-paths (repo tree names)
  "Force TREE onto changed NAMES, leaving all other entries alone."
  (let* ((index (ngit-read-index repo)) (old (aref index 0))
         (root (or (ngit-ct-root index) (ngit-ct-new)))
         (dropped (make-hash-table :test 'equal))
         (targets nil) (removed nil) (fresh nil)
         (checked (make-hash-table :test 'eql))
         (ngit--checkout-attr-direction 'index-first)
         (ngit--checkout-attr-cache (make-hash-table :test 'equal))
         (lock (ngit-lock (ngit-index-file repo))))
    (unwind-protect
        (progn
          (dolist (path (delete-dups names))
            (let ((target (ngit--literal-entry repo tree path))
                  (current (ngit-index-lookup old path)))
              (puthash path t dropped)
              (ngit-ct-invalidate root path)
              (when current (push current removed))
              (when (and target (/= (car target) ngit-mode-tree))
                (push (ngit--switch-entry path (car target) (cdr target) current) targets))))
          (setq targets (sort targets #'ngit--entry-less))
          (aset index 0 (ngit-index-overlay old targets dropped))
          (aset repo 7 nil)
          (dolist (e removed) (ngit--unlink-entry repo e))
          (dolist (e targets) (push (ngit-checkout-entry repo e) fresh))
          (let ((entries (ngit-index-overlay old (nreverse fresh) dropped)))
            (aset index 0 entries)
            (ngit--reset-refresh repo index entries checked t)
            (ngit-commit-lock lock (ngit-index-bytes repo index entries root checked))
            (setq lock nil)
            (aset repo 6 nil)))
      (when lock (ngit-unlock lock)))))

(defun ngit--stash-push (repo args)
  (let ((message nil) (keep nil) (untracked nil) (quiet nil) (paths nil))
    (while args
      (let ((a (pop args)))
        (cond ((string= a "--") (setq paths args args nil))
              ((member a '("-m" "--message"))
               (setq message (or (pop args) (ngit--die-with 129 "error: switch `m' requires a value"))))
              ((string-prefix-p "--message=" a) (setq message (substring a 10)))
              ((member a '("-k" "--keep-index")) (setq keep t))
              ((member a '("-u" "--include-untracked")) (setq untracked t))
              ((member a '("-q" "--quiet")) (setq quiet t))
              ((string-prefix-p "-" a) (ngit--die-with 129 "error: stash option '%s' is not supported" a))
              (t (setq paths (cons a args) args nil)))))
    (when (member "" paths)
      (ngit--die "fatal: empty string is not a valid pathspec. please use . instead if you meant to match all paths"))
    (let* ((head (or (ngit-resolve-ref repo "HEAD")
                     (ngit--die-with 1 "You do not have the initial commit yet")))
           (base (ngit--switch-tree repo head))
           (items (mapcar (lambda (p) (ngit--ps-item (ngit--pathspec-item repo p))) paths))
           (index (ngit--stash-refresh repo)) (old (aref index 0))
           (staged (ngit-diff-tree-index repo base))
           (wt (seq-filter (lambda (c) (ngit--stash-covered-p items (aref c 0)))
                           (ngit-wt-changes repo t)))
           (unames (and untracked
                        (seq-filter (lambda (p) (ngit--stash-covered-p items p))
                                    (car (ngit-add-untracked repo '("") (ngit--tracked-set old)))))))
      (when (and paths (not untracked))
        (let ((remaining items))
          (dolist (p paths)
            (unless (ngit--stash-index-matches-p old (car remaining))
              (ngit--die-with 1 "error: pathspec ':(prefix:0)%s' did not match any file(s) known to git\nDid you forget to 'git add'?" p))
            (setq remaining (cdr remaining)))))
      (if (not (or wt unames
                   (seq-some (lambda (c) (ngit--stash-covered-p items (aref c 0))) staged)))
          (unless quiet (ngit--emit "No local changes to save\n"))
        (let* ((ref (ngit-read-ref repo "HEAD"))
               (branch (if (and ref (string-prefix-p "ref: refs/heads/" ref))
                           (substring ref 16) "(no branch)"))
               (label (concat branch ": " (ngit-abbrev repo head) " " (ngit--subject-of repo head)))
               (iroot (or (ngit-ct-root index) (ngit-ct-new)))
               (itree (ngit-ct-update repo iroot old))
               (icommit (ngit--stash-commit repo itree (list head) (concat "index on " label "\n")))
               (ucommit nil) (targets nil) (dropped (make-hash-table :test 'equal))
               (names nil))
          ;; Prime the real cache tree before deriving the work-tree index.
          (ngit-write-index repo index old iroot)
          (setq index (ngit-read-index repo) old (aref index 0))
          (when untracked
            (let ((entries nil))
              (ngit-wt-setup (ngit-repo-toplevel repo))
              (dolist (p unames)
                (unless (string-suffix-p "/" p)
                  (push (ngit--new-entry repo p (ngit-wt-lstat p) t nil nil) entries)))
              (setq ucommit
                    (ngit--stash-commit repo (ngit-ct-update repo (ngit-ct-new) (vconcat (nreverse entries)))
                                        nil (concat "untracked files on " label "\n")))))
          (dolist (c wt)
            (let ((path (aref c 0)))
              (push path names)
              (puthash path t dropped)
              (when (/= (aref c 4) 0)
                (let ((bin (if (aref c 7) (ngit-write-object repo "blob" (aref c 7)) (aref c 5))))
                  (push (ngit--switch-entry path (aref c 4) bin nil) targets)))))
          (dolist (c staged)
            (when (ngit--stash-covered-p items (aref c 0)) (push (aref c 0) names)))
          (let* ((entries (ngit-index-overlay old targets dropped))
                 (tree (ngit--stash-tree repo index entries wt))
                 (msg (if (and message (not (string= message "")))
                          (concat "On " branch ": " (encode-coding-string message 'utf-8))
                        (concat "WIP on " label)))
                 (hex (ngit--stash-commit repo tree (append (list head icommit) (and ucommit (list ucommit))) msg))
                 (log (ngit--stash-log-file repo)))
            ;; refs/stash is always logged, even with logAllRefUpdates=false.
            (ngit-make-directory (file-name-directory log) t)
            (unless (file-exists-p log) (ngit-write-file log ""))
            (ngit--write-ref repo "refs/stash" hex (ngit-resolve-ref repo "refs/stash") msg t)
            (unless quiet (ngit--emit "Saved working directory and index state " (decode-coding-string msg 'utf-8) "\n"))
            (dolist (p unames)
              (unless (string-suffix-p "/" p)
                (ngit--unlink-entry repo (ngit--switch-entry p #o100644 nil nil))))
            (if paths
                (ngit--stash-restore-paths repo base (append names unames))
              (if (and ref (string-prefix-p "ref: " ref))
                  (ngit-cmd-reset repo '("--hard" "-q"))
                (ngit--reset-hard repo base "HEAD")
                (ngit--write-ref repo "ORIG_HEAD" head (ngit-resolve-ref repo "ORIG_HEAD") nil)))
            (when (and keep (not (equal itree ngit-empty-tree-bin)))
              (if paths (ngit--stash-restore-paths repo itree names)
                (ngit--reset-hard repo itree head)))))))
    0))

(defun ngit--stash-ref-lock (repo &optional revision)
  "Lock refs/stash, reporting the nonfatal ref error used by stash."
  (condition-case err
      (ngit-lock (ngit--ref-file repo "refs/stash"))
    (ngit-fatal
     (let ((message (cadr err)))
       (ngit--die-with
        1 "%s%s"
        (concat "error: cannot lock ref 'refs/stash': "
                (if (string-prefix-p "fatal: " message) (substring message 7) message))
        (if revision (concat "\nerror: " revision ": Could not drop stash entry") ""))))))

(defun ngit--stash-drop (repo selected quiet)
  (let* ((n (nth 1 selected)) (hex (car selected)))
    (unless n (ngit--die-with 1 "error: '%s' is not a stash reference" (nth 2 selected)))
    (let ((ref-lock (ngit--stash-ref-lock repo (nth 2 selected))))
      (unwind-protect
          (let* ((lines (ngit--stash-log repo)) (i 0) (out nil)
                 (remove (- (length lines) 1 n)) (prev (make-string 40 ?0))
                 (selected-line (nth remove lines)))
            (unless (and (>= remove 0) selected-line (string= (substring selected-line 41 81) hex))
              (ngit--die-with 1 "error: stash reflog changed while dropping entry"))
            (dolist (line lines)
              (unless (= i remove)
                (push (concat prev (substring line 40) "\n") out)
                (setq prev (substring line 41 81)))
              (setq i (1+ i)))
            (let ((lock (ngit-lock (ngit--stash-log-file repo))))
              (ngit-commit-lock lock (apply #'concat (nreverse out))))
            (if out
                (ngit-commit-lock ref-lock (concat prev "\n"))
              (ngit-unlock ref-lock))
            (setq ref-lock nil)
            (aset repo 4 nil)
            (unless quiet (ngit--emit "Dropped " (nth 2 selected) " (" hex ")\n"))
            ;; reflog_delete finishes before do_clear_stash starts its
            ;; reference transaction, including when that hook rejects it.
            (unless out (ngit--stash-clear repo)))
        (when ref-lock (ngit-unlock ref-lock))))
    0))

(defun ngit--stash-clear (repo)
  (let ((hex (ngit-resolve-ref repo "refs/stash")))
    (when hex
      (let ((lock (ngit--stash-ref-lock repo)))
        (unwind-protect (ngit--delete-ref repo "refs/stash" lock hex)
          (ngit-unlock lock)))))
  0)

(defun ngit--stash-reset-refs (repo)
  "Update the references touched by stash's internal mixed reset."
  (condition-case err
      (let* ((head (ngit-resolve-ref repo "HEAD"))
             (orig (ngit-resolve-ref repo "ORIG_HEAD"))
             (action (getenv "GIT_REFLOG_ACTION")))
        (ngit--write-ref repo "ORIG_HEAD" head orig
                         (unless (equal head orig)
                           (concat (or action "reset") ": updating ORIG_HEAD")))
        (ngit-update-head repo head head (ngit-ident repo "COMMITTER")
                          (if action (concat action ": updating HEAD")
                            "reset: moving to HEAD"))
        (ngit-commit-clear-merge-state repo)
        (ngit-ref-delete-state repo "AUTO_MERGE"))
    (ngit-fatal
     ;; git-stash ignores the child reset's status and continues applying.
     (if (equal (cadr err) "fatal: ref updates aborted by hook")
         (ngit-native-write-stderr (concat (cadr err) "\n"))
       (signal (car err) (cdr err))))))

(defun ngit--stash-merge (repo base theirs index)
  "Return (MERGED-ENTRIES CHANGES), or nil when a tree merge is needed."
  (catch 'stash-needs-merge
   (let* ((old (aref index 0)) (targets nil) (dropped (make-hash-table :test 'equal))
         (changes nil))
    (dolist (c (ngit-diff-trees repo base theirs))
      (let* ((path (aref c 0)) (ours (ngit-index-lookup old path))
             (om (if ours (aref ours 2) 0)) (ob (and ours (aref ours 1)))
             (bm (aref c 2)) (bb (aref c 3)) (tm (aref c 4)) (tb (aref c 5))
             (mode (cond ((= om tm) om) ((= om bm) tm) ((= tm bm) om)))
             (bin (cond ((equal ob tb) (cons t ob))
                        ((equal ob bb) (cons t tb))
                        ((equal tb bb) (cons t ob)))))
        (unless (and mode bin)
          (throw 'stash-needs-merge nil))
        (unless (or (and (= om bm) (equal ob bb))
                    (and (= om tm) (equal ob tb))
                    (and (= (logand bm #o170000) #o100000)
                         (= (logand om #o170000) #o100000)
                         (= (logand tm #o170000) #o100000)))
          (throw 'stash-needs-merge nil))
        (unless (and (= om mode) (equal ob (cdr bin)))
          (unless (or (= mode 0) (cdr bin))
            (throw 'stash-needs-merge nil))
          (setq c (copy-sequence c))
          (aset c 4 mode)
          (aset c 5 (cdr bin))
          (push c changes)
          (puthash path t dropped)
          (unless (= mode 0) (push (ngit--switch-entry path mode (cdr bin) ours) targets)))))
    ;; Independent additions can collide at a file/directory boundary even
    ;; though their literal names do not occur on both sides of the diff.
    (dolist (e targets)
      (let* ((path (aref e 0)) (prefix (concat path "/"))
             (i (ngit-index-find old prefix)) (start 0) (slash nil))
        (while (and (< i (length old)) (string-prefix-p prefix (aref (aref old i) 0)))
          (unless (gethash (aref (aref old i) 0) dropped)
            (throw 'stash-needs-merge nil))
          (setq i (1+ i)))
        (while (setq slash (ngit--byte-pos path ?/ start))
          (let ((ancestor (substring path 0 slash)))
            (when (and (ngit-index-lookup old ancestor) (not (gethash ancestor dropped)))
              (throw 'stash-needs-merge nil)))
          (setq start (1+ slash)))))
    (list (ngit-index-overlay old targets dropped) (nreverse changes)))))

(defun ngit--stash-overwrite-error (paths &optional untracked)
  (when paths
    (ngit-native-write-stderr
     (concat "error: Your local changes to the following files would be overwritten by merge:\n"
             (mapconcat (lambda (p) (concat "\t" p "\n")) (sort paths #'string<) "")
             "Please commit your changes or stash them before you merge.\n")))
  (when untracked
    (ngit-native-write-stderr
     (concat "error: The following untracked working tree files would be overwritten by merge:\n"
             (mapconcat (lambda (p) (concat "\t" p "\n")) (sort (delete-dups untracked) #'string<) "")
             "Please move or remove them before you merge.\n")))
  (when (or paths untracked) (ngit-native-write-stderr "Aborting\n")))

(defun ngit--stash-restore-untracked (repo entries)
  "Restore untracked ENTRIES without replacing existing files."
  (let ((failed nil))
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (dolist (e entries)
      (if (or (not (ngit--checkout-leading-clear-p (aref e 0)))
              (ngit-wt-lstat (aref e 0)))
          (progn
            (ngit-native-write-stderr (concat (aref e 0) " already exists, no checkout\n"))
            (setq failed t))
        (ngit-checkout-entry repo e)))
    (when failed (ngit-native-write-stderr "error: could not restore untracked files from stash\n"))
    failed))

(defun ngit--stash-index-conflict (repo base itree index)
  "Report a stash index patch that cannot apply to INDEX."
  (let ((entries (aref index 0)))
    (dolist (c (ngit-diff-trees repo base itree))
      (let ((e (ngit-index-lookup entries (aref c 0))))
        (unless (or (ngit--switch-same-entry-p e (aref c 2) (aref c 3))
                    (and (null e) (= (aref c 2) 0)))
          (ngit-native-write-stderr
           (concat "error: patch failed: " (aref c 0) ":1\n"
                   "error: " (aref c 0) ": patch does not apply\n"))))))
  (ngit--die-with 1 "error: conflicts in index. Try without --index."))

(defun ngit--stash-find-hunk (text ours starts expected cursor beginning ending)
  "Find TEXT at the nearest eligible line to EXPECTED in OURS.
STARTS holds byte positions of line starts.  As in apply.c's find_pos,
equally distant candidates prefer the forward match."
  (let* ((n (1- (length starts)))
         (wanted (max 0 (min expected n)))
         (pos (if (= (length text) 0) (aref starts wanted)
                (string-search text ours cursor)))
         (best nil) (distance nil))
    (while pos
      (let ((lo 0) (hi n))
        (while (< lo hi)
          (let ((mid (ash (+ lo hi) -1)))
            (if (< (aref starts mid) pos) (setq lo (1+ mid))
              (setq hi mid))))
        (when (and (= (aref starts lo) pos) (>= pos cursor)
                   (or (not beginning) (= pos 0))
                   (or (not ending) (= (+ pos (length text)) (length ours))))
          (let ((d (abs (- lo wanted))))
            (when (or (null distance) (< d distance)
                      (and (= d distance) (> lo (cdr best))))
              (setq best (cons pos lo) distance d)))))
      (setq pos (and (> (length text) 0)
                     (string-search text ours (1+ pos)))))
    best))

(defun ngit--stash-patch-data (base target ours)
  "Apply BASE-to-TARGET text hunks to OURS, with Git's three-line context.
Return (t . DATA) on success, or nil when a hunk cannot be found."
  (require 'ngit-xdiff)
  (let* ((before (ngit-xdiff--split base))
         (after (ngit-xdiff--split target))
         (current (ngit-xdiff--split ours))
         (starts (make-vector (1+ (length current)) 0))
         (script (ngit-xdiff--diff-script before after t))
         (cursor 0) (line-offset 0) (chunks nil) (failed nil) (i 0))
    (while (< i (length current))
      (aset starts (1+ i) (+ (aref starts i) (length (aref current i))))
      (setq i (1+ i)))
    (while (and script (not failed))
      (let* ((first (pop script)) (last first)
             (begin (max 0 (- (aref first 0) 3)))
             (end nil) (new-begin nil) (new-end nil)
             (old-text nil) (new-text nil) (match nil) (pos nil))
        (while (and script
                    (<= (aref (car script) 0)
                        (+ (aref last 0) (aref last 2) 6)))
          (setq last (pop script)))
        (setq end (min (length before) (+ (aref last 0) (aref last 2) 3))
              new-begin (- (aref first 1) (- (aref first 0) begin))
              new-end (+ (aref last 1) (aref last 3)
                         (- end (aref last 0) (aref last 2)))
              old-text (mapconcat #'identity (substring before begin end) "")
              new-text (mapconcat #'identity (substring after new-begin new-end) "")
              match (ngit--stash-find-hunk
                     old-text ours starts (+ begin line-offset) cursor
                     (= begin 0) (= end (+ (aref last 0) (aref last 2)))))
        (if match
            (progn
              (setq pos (car match) line-offset (- (cdr match) begin))
              (push (substring ours cursor pos) chunks)
              (push new-text chunks)
              (setq cursor (+ pos (length old-text))))
          (setq failed t))))
    (unless failed
      (push (substring ours cursor) chunks)
      (cons t (apply #'concat (nreverse chunks))))))

(defun ngit--stash-restore-index (repo base itree index)
  "Return the index restored by the saved BASE-to-ITREE patch, or nil."
  (or (ngit--stash-merge repo base itree index)
      (let ((old (aref index 0)) (targets nil)
            (dropped (make-hash-table :test 'equal)) (failed nil)
            (changes (ngit-diff-trees repo base itree)))
        (dolist (c changes)
          (let* ((path (aref c 0)) (e (ngit-index-lookup old path))
                 (bm (aref c 2)) (bb (aref c 3))
                 (tm (aref c 4)) (tb (aref c 5)) (target nil))
            (cond
             ((or (ngit--switch-same-entry-p e bm bb) (and (null e) (= bm 0)))
              (when (/= tm 0) (setq target (ngit--switch-entry path tm tb e))))
             ((and e (= (logand bm #o170000) #o100000)
                   (= (logand tm #o170000) #o100000) (= (aref e 2) bm))
              (let* ((bdata (cdr (ngit-read-object repo bb)))
                     (tdata (cdr (ngit-read-object repo tb)))
                     (odata (cdr (ngit-read-object repo (aref e 1))))
                     (patch (and (not (ngit--byte-pos bdata 0 0))
                                 (not (ngit--byte-pos tdata 0 0))
                                 (ngit--stash-patch-data bdata tdata odata))))
                (if patch
                    (setq target (ngit--switch-entry
                                  path tm (ngit-write-object repo "blob" (cdr patch)) e))
                  (setq failed t))))
             (t (setq failed t)))
            (puthash path t dropped)
            (when target (push target targets))))
        (unless failed
          (list (ngit-index-overlay old targets dropped) changes)))))

(defun ngit--stash-apply-content (repo base itree theirs ours index restore-index quiet utree)
  "Apply a stash needing a tree merge, preserving its index policy."
  (require 'ngit-merge-tree)
  (let* ((old (aref index 0))
         (has-index (and restore-index (not (equal base itree))
                         (not (equal ours itree))))
         (saved (and has-index (ngit--stash-restore-index repo base itree index)))
         (result nil) (untracked nil) (failed nil))
    (when (and has-index (null saved))
      (ngit--stash-index-conflict repo base itree index))
    (when utree
      (ngit--flatten-tree repo utree ""
                          (lambda (path mode bin)
                            (push (ngit--switch-entry path mode bin nil) untracked)))
      (setq untracked (nreverse untracked)))
    (when has-index
      (ngit--stash-tree repo index (car saved) (ngit-diff-trees repo base itree))
      (ngit--stash-reset-refs repo))
    (let ((ngit-merge-ancestor-label "Stash base"))
      (setq result
            (ngit-merge-trees repo base ours theirs
                              (if (equal base ours) "Version stash was based on" "Updated upstream")
                              "Stashed changes")))
    (condition-case err
        (ngit-merge-apply repo result ours)
      (ngit-fatal
       (ngit-native-write-stderr (concat (cadr err) "\n"))
       (aset repo 6 nil)
       (aset repo 7 nil)
       (setq failed t)))
    (if (or failed (plist-get result :conflicts))
        (progn
          (when restore-index (ngit-native-write-stderr "Index was not unstashed.\n"))
          (ngit--stash-restore-untracked repo untracked)
          (unless quiet (ngit-cmd-status repo nil))
          1)
      ;; A clean merge stages every result; stash puts existing paths back
      ;; to the pre-merge index and retains only newly introduced entries.
      (let* ((current (ngit-read-index repo)) (entries (aref current 0))
             (final (and has-index (car saved)))
             (root (or (ngit-ct-root index) (ngit-ct-new)))
             (checked (make-hash-table :test 'eql)) (new nil) (i 0))
        (unless has-index
          (while (< i (length entries))
            (let ((e (aref entries i)))
              (unless (ngit-index-lookup old (aref e 0))
                (let ((pos (aref e 4)))
                  (push (ngit-make-entry
                         (aref e 0) (aref e 1) (aref e 2)
                         (if pos (substring (aref current 1) pos (+ pos 40)) (aref e 6))
                         (aref e 5)) new))))
            (setq i (1+ i)))
          (setq final (ngit-index-overlay old (nreverse new) nil)))
        (dolist (c (ngit-diff-trees repo ours (plist-get result :tree)))
          (ngit-ct-invalidate root (aref c 0)))
        (when has-index
          (dolist (c (ngit-diff-trees repo base itree))
            (ngit-ct-invalidate root (aref c 0)))
          (ngit-ct-update repo root final))
        (ngit--reset-refresh repo index final checked t)
        (ngit-write-index repo index final root checked))
      (setq failed (ngit--stash-restore-untracked repo untracked))
      (unless quiet (ngit-cmd-status repo nil))
      (if failed 1 0))))

(defun ngit--stash-apply (repo selected restore-index quiet)
  (catch 'stash-result
   (let* ((hex (car selected)) (commit (ngit--commit-of repo hex))
         (parents (ngit--stash-parents repo commit))
         (base (ngit--switch-tree repo (car parents)))
         (itree (ngit--switch-tree repo (cadr parents)))
         (theirs (ngit--switch-tree repo hex))
         (utree (and (nth 2 parents) (ngit--switch-tree repo (nth 2 parents))))
         (index (ngit--stash-refresh repo)) (old (aref index 0))
         (root (or (ngit-ct-root index) (ngit-ct-new)))
         (ours (ngit-ct-update repo root old))
         (merged (ngit--stash-merge repo base theirs index))
         (changes (cadr merged)) (bad nil) (collisions nil) (new-paths nil) (loose nil)
         (saved-index (if (and restore-index merged)
                          (car (ngit--stash-restore-index repo base itree index)) old))
         (untracked nil))
    (unless merged
      (throw 'stash-result
             (ngit--stash-apply-content repo base itree theirs ours index
                                        restore-index quiet utree)))
    (when (and restore-index (null saved-index))
      (ngit--stash-index-conflict repo base itree index))
    ;; Preflight every tracked write before updating the index or work tree.
    (dolist (c changes)
      (let ((e (ngit-index-lookup old (aref c 0))))
        (when (and (null e) (/= (aref c 4) 0)) (push (aref c 0) new-paths))
        (when (and e (not (ngit--switch-worktree-clean-p repo index e)))
          (push (aref c 0) bad))))
    (when new-paths
      (setq loose (car (ngit-add-untracked repo '("") (ngit--tracked-set old))))
      (dolist (path new-paths)
        (setq collisions (append (ngit--switch-untracked-conflicts loose path) collisions))))
    (when utree
      (ngit--flatten-tree repo utree ""
                          (lambda (path mode bin)
                            (push (ngit--switch-entry path mode bin nil) untracked)))
      (setq untracked (nreverse untracked)))
    (when (and restore-index (not (equal base itree)) (not (equal ours itree)))
      (ngit--stash-tree repo index saved-index (ngit-diff-trees repo base itree))
      (ngit--stash-reset-refs repo))
    (when (or bad collisions)
      (ngit--stash-overwrite-error bad collisions)
      (when restore-index (ngit-native-write-stderr "Index was not unstashed.\n"))
      (ngit--stash-restore-untracked repo untracked)
      (unless quiet (ngit-cmd-status repo nil))
      (throw 'stash-result 1))
    (let ((tree (ngit--stash-tree repo index (car merged) changes)))
      (ngit--switch-twoway repo ours tree "merge" (and new-paths (list loose)))
      (unless (equal base theirs)
        (ngit--write-ref repo "AUTO_MERGE" (ngit-hex tree)
                         (ngit-resolve-ref repo "AUTO_MERGE") nil t)))
    (when (equal base theirs) (ngit--emit "Already up to date.\n"))
    ;; By default existing paths are unstaged; newly added paths stay staged.
    (let* ((current (ngit-read-index repo)) (entries (aref current 0))
           (new nil) (final saved-index) (df-reported nil)
           (checked (make-hash-table :test 'eql)))
      (unless restore-index
        (dolist (c changes)
          (when (and (= (aref c 2) 0) (/= (aref c 4) 0))
            (let* ((e (ngit-index-lookup entries (aref c 0))) (pos (aref e 4)))
              (let* ((start 0) (slash nil) (path (aref e 0))
                     (prefix (concat path "/")) (j (ngit-index-find old prefix)))
                (while (setq slash (ngit--byte-pos path ?/ start))
                  (let ((ancestor (substring path 0 slash)))
                    (when (and (ngit-index-lookup old ancestor) (not (member ancestor df-reported)))
                      (push ancestor df-reported)
                      (ngit-native-write-stderr
                       (concat "error: '" ancestor "' appears as both a file and as a directory\n"))))
                  (setq start (1+ slash)))
                (while (and (< j (length old)) (string-prefix-p prefix (aref (aref old j) 0)))
                  (let ((descendant (aref (aref old j) 0)))
                    (unless (member descendant df-reported)
                      (push descendant df-reported)
                      (ngit-native-write-stderr
                       (concat "error: '" descendant "' appears as both a file and as a directory\n"))))
                  (setq j (1+ j))))
              (push (ngit-make-entry (aref e 0) (aref e 1) (aref e 2)
                                     (if pos (substring (aref current 1) pos (+ pos 40)) (aref e 6))
                                     (aref e 5)) new))))
        (setq final (ngit-index-overlay old new nil)))
      (let ((final-root (or (ngit-ct-root index) (ngit-ct-new))))
        (dolist (c changes) (ngit-ct-invalidate final-root (aref c 0)))
        (when restore-index
          (dolist (c (ngit-diff-trees repo base itree))
            (ngit-ct-invalidate final-root (aref c 0)))
          (ngit-ct-update repo final-root final))
        (ngit--reset-refresh repo index final checked t)
        (ngit-write-index repo index final final-root checked)))
    (let ((failed (ngit--stash-restore-untracked repo untracked)))
      (unless quiet (ngit-cmd-status repo nil))
      (if failed 1 0)))))

;;;###autoload
(defun ngit-cmd-stash (repo args)
  (let ((command (if (and args (not (string-prefix-p "-" (car args)))) (pop args) "push")))
    (cond
     ((string= command "push") (ngit--stash-push repo args))
     ((string= command "clear")
      (when args (ngit--die-with 129 "error: too many arguments"))
      (ngit--stash-clear repo))
     ((string= command "list")
      (let ((i 0))
        (dolist (line (reverse (ngit--stash-log repo)))
          (let ((tab (ngit--byte-pos line ?\t 81)))
            (ngit--emit (format "stash@{%d}: " i) (decode-coding-string (substring line (1+ tab)) 'utf-8) "\n"))
          (setq i (1+ i)))) 0)
     ((string= command "show")
      (let ((patch nil) (stat nil) (revs nil))
        (dolist (a args)
          (cond ((member a '("-p" "--patch")) (setq patch t))
                ((string= a "--stat") (setq stat t))
                (t (push a revs))))
        (let* ((selected (ngit--stash-select repo (nreverse revs)))
               (hex (car selected)) (commit (ngit--commit-of repo hex))
               (changes (ngit-detect-renames
                         (ngit-diff-trees repo (ngit--switch-tree repo (car (ngit--stash-parents repo commit)))
                                          (ngit--switch-tree repo hex)))))
          (when (or stat (not patch)) (ngit--emit-stat (ngit--stat-entries repo changes)))
          (when patch
            (when (and stat changes) (ngit--emit "\n"))
            (dolist (c changes) (ngit--emit-patch repo c 3)))) 0))
     ((member command '("apply" "pop" "drop"))
      (let ((restore-index nil) (quiet nil) (revs nil))
        (dolist (a args)
          (cond ((string= a "--index") (setq restore-index t))
                ((member a '("-q" "--quiet")) (setq quiet t))
                (t (push a revs))))
        (let ((selected (ngit--stash-select repo (nreverse revs))))
          (if (string= command "drop") (ngit--stash-drop repo selected quiet)
            (when (and (string= command "pop") (null (nth 1 selected)))
              (ngit--die-with 1 "error: '%s' is not a stash reference" (nth 2 selected)))
            (let ((result (ngit--stash-apply repo selected restore-index quiet)))
              (when (string= command "pop")
                (if (= result 0) (ngit--stash-drop repo selected quiet)
                  (ngit--emit "The stash entry is kept in case you need it again.\n")))
              result)))))
     ((string= command "branch")
      (let* ((name (or (pop args) (ngit--die-with 1 "No branch name specified")))
             (selected (ngit--stash-select repo args))
             (base (car (ngit--stash-parents repo (ngit--commit-of repo (car selected))))))
        (ngit-cmd-switch repo (list "-c" name base))
        (let ((result (ngit--stash-apply repo selected t nil)))
          (when (and (= result 0) (nth 1 selected)) (ngit--stash-drop repo selected nil))
          result)))
     (t (ngit--die-with 129 "error: stash subcommand '%s' is not supported" command)))))

(provide 'ngit-stash)

;;; ngit-stash.el ends here
