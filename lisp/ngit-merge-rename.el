;;; ngit-merge-rename.el --- File rename reconciliation for tree merges -*- lexical-binding: t; -*-

;;; Commentary:

;; Reconcile file identities before the pathname merge.  Rename detection
;; receives fresh diff records: diffcore mutates its candidates in place.
;; Infer directory moves only from established file rename pairs.

;;; Code:

(require 'ngit-rename)

;; Declare shared dynamic bindings before this module is byte-compiled.
;; The tree engine loads this module before installing its own defaults.
(defvar ngit-merge-ancestor-label)
(defvar ngit-merge-marker-extra)

(defun ngit-merge-renames-p (repo)
  "Return whether the default ort-compatible merge detects file renames.
Git 2.47 ort ignores detect_renames, including merge.renames=false;
merge.renameLimit still bounds its similarity search."
  (ignore repo)
  t)

(defun ngit-merge-rename-map (repo base side)
  "Return fresh (SOURCE . DESTINATION) pairs from BASE to SIDE."
  (let* ((limit (or (ngit-config-int repo "merge.renameLimit" nil)
                    (ngit-config-int repo "diff.renameLimit" 7000)))
         ;; Zero means unlimited, as in diffcore-rename.
         (limit (if (<= limit 0) 32767 limit))
         (ngit-rename-limit-warning nil)
         (changes (ngit-diffcore-rename repo (ngit-diff-trees repo base side) limit))
         (out nil))
    (dolist (c changes)
      (when (= (aref c 1) ?R) (push (cons (aref c 8) (aref c 0)) out)))
    (when ngit-rename-limit-warning
      (ngit-native-write-stderr
       (format "warning: exhaustive rename detection was skipped due to too many files.\nwarning: you may want to set your merge.renamelimit variable to at least %d and retry the command.\n"
               ngit-rename-limit-warning)))
    (nreverse out)))

(defun ngit-merge-rename--clear (rec)
  "Remove all sides of REC before reconciling their new names."
  (dotimes (i 6) (aset rec (1+ i) nil)))

(defun ngit-merge-rename--labels (base ours theirs ol tl)
  "Return merge-file labels, decorating differing original paths."
  (let ((bp (and base (aref base 0)))
        (op (and ours (aref ours 0))) (tp (and theirs (aref theirs 0))))
    (list (if (and bp (or (not (equal bp op)) (not (equal bp tp))))
              (concat ngit-merge-ancestor-label ":" bp) ngit-merge-ancestor-label)
          (if (and op tp (not (equal op tp))) (concat ol ":" op) ol)
          (if (and op tp (not (equal op tp))) (concat tl ":" tp) tl))))

(defun ngit-merge-rename--content (repo path b o th ol tl depth)
  "Merge one renamed identity using its original-path marker labels."
  (let* ((labels (ngit-merge-rename--labels b o th ol tl))
         (ngit-merge-ancestor-label (car labels)))
    (ngit-merge--content repo path b o th (nth 1 labels) (nth 2 labels) depth)))

(defun ngit-merge-rename--merged (repo path b o th ol tl depth)
  "Return the content result for an identity, retaining the present side."
  (cond ((not o) th)
        ((not th) o)
        ((ngit-merge--same o th) o)
        ((ngit-merge--same b o) th)
        ((ngit-merge--same b th) o)
        (t (car (ngit-merge-rename--content repo path b o th ol tl depth)))))

(defun ngit-merge-rename--identity (original source left right)
  "Normalize SOURCE from immutable ORIGINAL records and both rename maps.
Return [BASE OURS THEIRS REPLACEMENT-OURS REPLACEMENT-THEIRS OURS-DEST
THEIRS-DEST].  A different object type at the old name is an independent
addition and must never become content of the renamed identity."
  (let* ((old (gethash source original)) (b (aref old 1))
         (od (cdr (assoc source left))) (td (cdr (assoc source right)))
         (o (if od (aref (gethash od original) 2) (aref old 2)))
         (th (if td (aref (gethash td original) 3) (aref old 3)))
         (ro (and (not od) o
                  (/= (logand (aref b 2) #o170000) (logand (aref o 2) #o170000)) o))
         (rt (and (not td) th
                  (/= (logand (aref b 2) #o170000) (logand (aref th 2) #o170000)) th)))
    (vector b (unless ro o) (unless rt th) ro rt od td)))

(defun ngit-merge-rename--source (record identity)
  "Remove renamed content from RECORD while retaining independent additions."
  (ngit-merge-rename--clear record)
  (aset record 2 (aref identity 3)) (aset record 3 (aref identity 4))
  (aset record 4 (or (aref identity 3) (aref identity 4)))
  (aset record 7 t))

(defun ngit-merge-rename-reconcile (repo table left right ol tl depth)
  "Reconcile LEFT and RIGHT rename maps in pathname records in TABLE.
Record slots 7 and 8 carry processing state and content labels.  State
`path-conflict' leaves content processing to the pathname merger while
retaining unmerged stages even when that content merge is clean.
Original entries remain immutable, so both independent rename maps can
consume a source even when two different sources meet at one destination."
  (let ((original (make-hash-table :test 'equal))
        (identities (make-hash-table :test 'equal))
        (done (make-hash-table :test 'equal)) (sources nil))
    (dolist (pair (append left right))
      (unless (member (car pair) sources) (push (car pair) sources))
      (dolist (path (list (car pair) (cdr pair)))
        (let ((rec (gethash path table)))
          (unless (gethash path original) (puthash path (copy-sequence rec) original)))))
    ;; Normalize every source before consuming any record: a destination
    ;; collision may consume both source records in a single iteration.
    (setq sources (sort sources #'string<))
    (dolist (s sources)
      (puthash s (ngit-merge-rename--identity original s left right) identities))
    (dolist (s sources)
      (unless (gethash s done)
        (let* ((src (gethash s table)) (identity (gethash s identities))
               (b (aref identity 0)) (od (aref identity 5)) (td (aref identity 6))
               (d (or od td)) (dest (gethash d table))
               (o (aref identity 1)) (th (aref identity 2))
               (replacement-o (aref identity 3)) (replacement-th (aref identity 4))
               (other (car (rassoc d (if od right left)))))
          (puthash s t done)
          (ngit-merge-rename--source src identity)
          (cond
           ;; One source, two destinations: merge content before splitting
           ;; the merged version across stage 2 and stage 3 destinations.
           ((and od td (not (equal od td)))
            (let* ((content (let ((ngit-merge-marker-extra (1+ ngit-merge-marker-extra)))
                              (ngit-merge-rename--content repo s b o th ol tl depth)))
                   (merged (car content))
                   ;; An opaque merge can only select a side, not synthesize
                   ;; shared content.  Preserve the other side at its name.
                   (theirs-merged (if (and (not (nth 1 content))
                                          (ngit-merge--same merged o)) th merged))
                   (orec (gethash od table)) (trec (gethash td table)))
              (aset src 1 b) (aset src 5 t)
              (aset src 6
                    (append (nth 2 content)
                            (list (concat "CONFLICT (rename/rename): " s " renamed to " od
                                          " in " ol " and to " td " in " tl "."))))
              (dolist (rec (list orec trec))
                (ngit-merge-rename--clear rec)
                (aset rec 5 t) (aset rec 7 'path-conflict))
              ;; Opposite-side entries at either destination are independent
              ;; additions.  Let the ordinary merge handle their collisions.
              (aset orec 2 (ngit-merge--copy merged od))
              (aset orec 3 (aref (gethash od original) 3))
              (aset trec 2 (aref (gethash td original) 2))
              (aset trec 3 (ngit-merge--copy theirs-merged td))))
           ;; Different sources reaching one destination become an add/add
           ;; after each source has incorporated its opposite-side edits.
           ((and other (not (equal s other)))
            (let* ((other-identity (gethash other identities))
                   (oi (if od identity other-identity))
                   (ti (if td identity other-identity))
                   (mo (ngit-merge-rename--merged repo d (aref oi 0)
                                                  (aref oi 1) (aref oi 2) ol tl depth))
                   (mt (ngit-merge-rename--merged repo d (aref ti 0)
                                                  (aref ti 1) (aref ti 2) ol tl depth))
                   (anchor nil))
              (puthash other t done)
              (ngit-merge-rename--source (gethash other table) other-identity)
              ;; ort treats a type replacement plus a destination collision
              ;; as an undetected same-destination rename.  Its sorted source
              ;; processing installs the affected source's base at the target.
              (dolist (candidate (if (string< s other)
                                    (list identity other-identity)
                                  (list other-identity identity)))
                (when (or (aref candidate 3) (aref candidate 4))
                  (setq anchor (aref candidate 0))))
              (ngit-merge-rename--clear dest)
              (aset dest 1 anchor)
              (aset dest 2 (ngit-merge--copy mo d))
              (aset dest 3 (ngit-merge--copy mt d))
              (when anchor
                (aset dest 8 (ngit-merge-rename--labels anchor (aref dest 2)
                                                      (aref dest 3) ol tl)))))
           (t
            (let* ((prior (gethash d original))
                   (collision (if od (aref prior 3) (aref prior 2))))
              (ngit-merge-rename--clear dest)
              (cond
               ;; A non-renamed addition at the destination competes with
               ;; the renamed file after its own content merge.
               ((and collision (not (and od td)))
                (let ((merged (ngit-merge-rename--merged repo d b o th ol tl depth)))
                  (aset dest (if od 2 3) (ngit-merge--copy merged d))
                  (aset dest (if od 3 2) collision)
                  ;; Type replacement plus an occupied destination uses the
                  ;; same base rule as the two-source collision above.
                  (when (or replacement-o replacement-th)
                    (aset dest 1 b)
                    (aset dest 8 (ngit-merge-rename--labels b (aref dest 2)
                                                          (aref dest 3) ol tl)))))
               (t
                (aset dest 1 b) (aset dest 2 o) (aset dest 3 th)
                (aset dest 8 (ngit-merge-rename--labels b o th ol tl))
                (when (not (and o th))
                  (aset dest 4 (or o th)) (aset dest 5 t) (aset dest 7 t)
                  (unless (or replacement-o replacement-th)
                    (aset dest 6
                          (list (concat "CONFLICT (rename/delete): " s " renamed to " d
                                        " in " (if od ol tl) ", but deleted in "
                                        (if od tl ol) "."))))
                  (when (or replacement-o replacement-th
                            (not (ngit-merge--same b (or o th))))
                    (aset dest 6
                          (append (aref dest 6)
                                  (list (concat "CONFLICT (modify/delete): " d " deleted in "
                                                (if od tl ol) " and modified in " (if od ol tl)
                                                ".  Version " (if od ol tl) " of " d
                                                " left in tree."))))))))))))))))

(defun ngit-merge-directory--parent (path)
  "Return the non-root parent directory of PATH, without a trailing slash."
  (let ((dir (file-name-directory path)))
    (and dir (> (length dir) 1) (substring dir 0 -1))))

(defun ngit-merge-directory--present (path entries)
  "Return whether directory PATH still exists in tree ENTRIES."
  (let ((prefix (concat path "/")) (i 0) (found nil))
    (while (and (< i (length entries)) (not found))
      (when (string-prefix-p prefix (aref (aref entries i) 0)) (setq found t))
      (setq i (1+ i)))
    found))

(defun ngit-merge-directory-map (renames entries)
  "Infer source directories and majority destinations from RENAMES.
ENTRIES is the immutable tree of the renaming side.  A surviving source
directory is not a directory rename.  A nil destination denotes a tie;
the empty string denotes the repository root."
  (let ((votes nil) (result nil))
    (dolist (pair renames)
      (let ((src (ngit-merge-directory--parent (car pair)))
            (dst (or (ngit-merge-directory--parent (cdr pair)) "")) (again t))
        (while (and src again (not (equal src dst)))
          (unless (ngit-merge-directory--present src entries)
            (let ((vote (assoc src votes)))
              (unless vote (setq vote (list src)) (push vote votes))
              (let ((count (assoc dst (cdr vote))))
                (if count (setcdr count (1+ (cdr count)))
                  (setcdr vote (cons (cons dst 1) (cdr vote)))))))
          (setq again (equal (file-name-nondirectory src) (file-name-nondirectory dst))
                src (ngit-merge-directory--parent src)
                dst (or (ngit-merge-directory--parent dst) "")))))
    (dolist (vote votes)
      (let ((best nil) (count 0) (tie nil))
        (dolist (candidate (cdr vote))
          (cond ((> (cdr candidate) count)
                 (setq best (car candidate) count (cdr candidate) tie nil))
                ((= (cdr candidate) count) (setq tie t))))
        (push (cons (car vote) (unless tie best)) result)))
    (sort result (lambda (a b) (> (length (car a)) (length (car b)))))))

(defun ngit-merge-directory--occupied (path entries)
  "Return whether immutable ENTRIES obstruct an implicit move to PATH."
  (let ((i 0) (found nil))
    (while (and (< i (length entries)) (not found))
      (let ((other (aref (aref entries i) 0)))
        (when (or (equal path other) (string-prefix-p (concat path "/") other)
                  (string-prefix-p (concat other "/") path))
          (setq found t)))
      (setq i (1+ i)))
    found))

(defun ngit-merge-directory-reconcile (repo table paths left right ours theirs ol tl depth)
  "Relocate opposite-side additions using directory maps LEFT and RIGHT.
Return (PATHS METADATA CONFLICT), where METADATA associates paths with
(FORCE-UNMERGED . MESSAGES).  Plan all moves before mutating any record."
  (let* ((config (downcase (or (ngit-config-get repo "merge.directoryRenames") "conflict")))
         (enabled (not (member config '("false" "no" "off" "0"))))
         (suggest (equal config "conflict"))
         (metadata (make-hash-table :test 'equal))
         (plans nil) (conflict nil) (reported nil) (skipped nil))
    (when enabled
      (dolist (item (list (list left 3 theirs tl ol) (list right 2 ours ol tl)))
        (let ((mapping (nth 0 item)) (side (nth 1 item)) (tree (nth 2 item))
              (added-label (nth 3 item)) (renamed-label (nth 4 item)))
          (dolist (path paths)
            (let ((rec (gethash path table)) (match nil))
              (when (and (aref rec side) (not (eq (aref rec 7) t))
                         (or (and (not (aref rec 1)) (not (aref rec (- 5 side))))
                             ;; A rename into the moved directory has already
                             ;; incorporated the opposite edit at its old name.
                             (and (aref rec 1) (aref rec (- 5 side))
                                  (not (equal path (aref (aref rec 1) 0)))
                                  (equal (aref (aref rec (- 5 side)) 0)
                                         (aref (aref rec 1) 0)))))
                (dolist (pair mapping)
                  (when (and (not match) (string-prefix-p (concat (car pair) "/") path))
                    (setq match pair)))
                (when match
                  (cond
                   ;; Git excludes implicit moves into a directory that the
                   ;; receiving side itself renamed.  Preserve both layouts.
                   ((and (cdr match) (assoc (cdr match) (if (= side 3) right left)))
                    (unless (member match skipped)
                      (push match skipped)
                      (puthash path
                               (cons nil (list (concat "WARNING: Avoiding applying "
                                                      (car match) " -> " (cdr match) " rename to "
                                                      path ", because " (cdr match)
                                                      " itself was renamed."))) metadata)))
                   ((not (cdr match))
                      (unless (member (car match) reported)
                        (push (car match) reported)
                        (setq conflict t)
                        (puthash path
                                 (cons nil (list (concat "CONFLICT (directory rename split): Unclear where to rename "
                                                        (car match) " to; it was renamed to multiple other directories, with no destination getting a majority of the files."))) metadata)))
                   (t
                    (let ((dest (if (equal (cdr match) "")
                                    (substring path (1+ (length (car match))))
                                  (concat (cdr match) (substring path (length (car match)))))))
                      (push (list path dest side tree added-label renamed-label
                                  (copy-sequence rec)) plans))))))))))
      ;; A source is retained if its destination was independently occupied on
      ;; that same side, or if multiple implicit renames target one pathname.
      (let ((accepted nil))
       (dolist (plan plans)
        (let* ((src (nth 0 plan)) (dst (nth 1 plan)) (side (nth 2 plan))
               (blocked (ngit-merge-directory--occupied dst (nth 3 plan)))
               (colliders nil))
          (dolist (other plans)
            (when (and (equal dst (nth 1 other)) (= side (nth 2 other)))
              (push (car other) colliders)))
          (setq colliders (sort colliders #'string<))
          (if (or blocked (> (length colliders) 1))
              (progn
                (setq conflict t)
                (unless (member dst reported)
                  (push dst reported)
                  (puthash src
                           (cons nil (list (concat "CONFLICT (implicit dir rename): "
                                                  (if blocked
                                                      (concat "Existing file/dir at " dst " in the way of implicit directory rename(s) putting the following path(s) there: ")
                                                    (concat "Cannot map more than one path to " dst "; implicit directory renames tried to put these paths there: "))
                                                  (mapconcat #'identity colliders ", ")
                                                  (if blocked "." "")))) metadata)))
            (push plan accepted))))
       (setq plans (nreverse accepted)))
      ;; Consume all accepted sources before installing any destinations.
      ;; A destination may also be another plan's source; every read below
      ;; uses the immutable record snapshot retained by that plan.
      (dolist (plan plans)
        (let ((source (gethash (car plan) table)))
          (ngit-merge-rename--clear source)
          (aset source 7 t)))
      (dolist (plan plans)
        (let* ((src (nth 0 plan)) (dst (nth 1 plan)) (side (nth 2 plan))
               (source (nth 6 plan)) (target (gethash dst table))
                   (base (aref source 1))
                   (relevant (and base (not (ngit-merge--same base (aref source (- 5 side))))))
                   (description (if relevant
                                    (concat (aref base 0) " renamed to " src " in " (nth 4 plan) ",")
                                  (concat src " added in " (nth 4 plan))))
                   (message (concat (if suggest "CONFLICT (file location): " "Path updated: ")
                                    description
                                    " inside a directory that was renamed in " (nth 5 plan)
                                    (if suggest ", suggesting it should perhaps be moved to " "; moving it to ")
                                    dst ".")))
              (unless target
                (setq target (vector dst nil nil nil nil nil nil nil nil))
                (puthash dst target table) (push dst paths))
              (aset target 7 nil)
              (if (and relevant (not (aref target (- 5 side))))
                  (progn
                    (dotimes (i 3) (aset target (1+ i) (aref source (1+ i))))
                    (aset target 8 (aref source 8)))
                (aset target 8
                      (ngit-merge-rename--labels
                       nil (if (= side 2) (aref source side) (aref target 2))
                       (if (= side 3) (aref source side) (aref target 3)) ol tl))
                (aset target side
                      (ngit-merge--copy
                       (if relevant
                           (ngit-merge-rename--merged repo dst base (aref source 2)
                                                      (aref source 3) ol tl depth)
                         (aref source side)) dst)))
              (when suggest (setq conflict t))
              (puthash dst (cons (and suggest (= depth 0)) (list message)) metadata))))
    (list (sort paths #'string<) metadata (and (= depth 0) conflict))))

(provide 'ngit-merge-rename)
;;; ngit-merge-rename.el ends here
