;;; ngit-log.el --- revision walking and git log -*- lexical-binding: t; -*-

;;; Commentary:

;; A port of the parts of git's revision.c, commit.c, pretty.c and
;; log-tree.c that `git log' needs: revision arguments and ranges, the
;; date-ordered walk with limiting (limit_list), history simplification
;; for pathspecs, parent rewriting, topological order, the built-in and
;; user pretty formats, dates and ref decorations.
;;
;; Commits are vectors (see `ngit--commit') cached per hex name, with the
;; same flag bits as git's object flags, so the walk can follow git's
;; algorithms step by step; output order is what makes log byte-identical.

;;; Code:

(require 'ngit-cli)

;;;; Commit objects

;; A commit is [HEX FLAGS PARENTS DATE TREE DATA AUTHOR COMMITTER MESSAGE
;; AUX PARSED ORIG-PARENTS INDEX].  PARENTS is a list of commits that
;; simplification and parent rewriting may change; ORIG-PARENTS keeps the
;; parsed list.  AUX holds the in-degree during a topological sort.

(defmacro ngit--c-hex (c) `(aref ,c 0))
(defmacro ngit--c-flags (c) `(aref ,c 1))
(defmacro ngit--c-parents (c) `(aref ,c 2))
(defmacro ngit--c-date (c) `(aref ,c 3))
(defmacro ngit--c-tree (c) `(aref ,c 4))
(defmacro ngit--c-data (c) `(aref ,c 5))
(defmacro ngit--c-author (c) `(aref ,c 6))
(defmacro ngit--c-committer (c) `(aref ,c 7))
(defmacro ngit--c-message (c) `(aref ,c 8))
(defmacro ngit--c-aux (c) `(aref ,c 9))
(defmacro ngit--c-parsed (c) `(aref ,c 10))
(defmacro ngit--c-orig-parents (c) `(aref ,c 11))

(defmacro ngit--has (c flag) `(/= 0 (logand (aref ,c 1) ,flag)))
(defmacro ngit--set (c flag) `(aset ,c 1 (logior (aref ,c 1) ,flag)))
(defmacro ngit--clear (c flag) `(aset ,c 1 (logand (aref ,c 1) (lognot ,flag))))

;; Object flag bits, as in git's revision.h.
(defconst ngit-seen 1)
(defconst ngit-uninteresting 2)
(defconst ngit-treesame 4)
(defconst ngit-shown 8)
(defconst ngit-tmp-mark 16)
(defconst ngit-boundary 32)
(defconst ngit-child-shown 64)
(defconst ngit-added 128)
(defconst ngit-symmetric-left 256)
(defconst ngit-patchsame 512)
(defconst ngit-bottom 1024)
(defconst ngit-pull-merge 2048)
;; Merge-base painting (commit-reach.c).
(defconst ngit-parent1 65536)
(defconst ngit-parent2 131072)
(defconst ngit-stale 262144)
(defconst ngit-result 524288)

(defvar ngit--commit-table (make-hash-table :test 'equal)
  "Commit vectors by hex name.")

(defun ngit-commit (hex)
  "Return the (possibly unparsed) commit vector for HEX."
  (or (gethash hex ngit--commit-table)
      (puthash hex (vector hex 0 nil 0 nil nil nil nil nil nil nil nil)
               ngit--commit-table)))

(defun ngit--ident-date (ident)
  "The timestamp in IDENT (\"Name <email> STAMP TZ\"), or 0."
  (let* ((n (length ident))
         ;; The zone is almost always five characters ("+0900").
         (e (if (and (> n 6) (= (aref ident (- n 6)) ?\s)) (- n 6) (1- n))))
    (while (and (>= e 0) (/= (aref ident e) ?\s)) (setq e (1- e)))
    (let ((s (1- e)))
      (while (and (>= s 0) (/= (aref ident s) ?\s)) (setq s (1- s)))
      (if (and (> s 0) (= (aref ident (1- s)) ?>))
          (string-to-number (substring ident (1+ s) e))
        0))))

(defun ngit-parse-commit-object (repo c)
  "Parse commit C (once) from REPO's object store; return C."
  (unless (ngit--c-parsed c)
    (let* ((obj (ngit-read-object repo (ngit-unhex (ngit--c-hex c))))
           (data (cdr obj)))
      (unless (eq (car obj) 'commit)
        (ngit--die "fatal: object %s is a %s, not a commit" (ngit--c-hex c) (car obj)))
      (let* ((parsed (ngit-parse-commit data))
             (parents (mapcar #'ngit-commit (ngit-header-all parsed "parent")))
             (committer (or (ngit-header parsed "committer") "")))
        (aset c 2 parents)
        (aset c 11 parents)
        (aset c 3 (ngit--ident-date committer))
        (aset c 4 (ngit-header parsed "tree"))
        (aset c 5 data)
        (aset c 6 (or (ngit-header parsed "author") ""))
        (aset c 7 committer)
        (aset c 8 (plist-get parsed :message))
        (aset c 10 t))))
  c)

;;;; Commit lists

(defun ngit--insert-by-date (c list)
  "Insert C into LIST (newest first) after the entries not older than C."
  (let ((d (ngit--c-date c)))
    (if (or (null list) (< (ngit--c-date (car list)) d))
        (cons c list)
      (let ((cell list))
        (while (and (cdr cell) (>= (ngit--c-date (cadr cell)) d))
          (setq cell (cdr cell)))
        (setcdr cell (cons c (cdr cell)))
        list))))

(defun ngit--sort-by-date (list)
  "Stable sort of LIST by date, newest first."
  (sort list (lambda (a b) (> (ngit--c-date a) (ngit--c-date b)))))

;; A priority queue ordered by date (ties in insertion order), or a stack
;; when there is no comparison, as git's prio_queue.  [ARRAY COUNT CTR DATEP]

(defun ngit--pq-new (by-date) (vector (make-vector 64 nil) 0 0 by-date))

(defun ngit--pq-less (a b)
  "Non-nil when queue entry A comes out before B."
  (let ((da (ngit--c-date (cdr a))) (db (ngit--c-date (cdr b))))
    (or (> da db) (and (= da db) (< (car a) (car b))))))

(defun ngit--pq-put (q c)
  (let ((arr (aref q 0)) (n (aref q 1)))
    (when (= n (length arr))
      (let ((bigger (make-vector (* 2 n) nil)) (i 0))
        (while (< i n) (aset bigger i (aref arr i)) (setq i (1+ i)))
        (aset q 0 bigger)
        (setq arr bigger)))
    (aset arr n (cons (aref q 2) c))
    (aset q 2 (1+ (aref q 2)))
    (aset q 1 (1+ n))
    (when (aref q 3)
      (let ((i n))
        (while (> i 0)
          (let ((parent (ash (1- i) -1)))
            (if (ngit--pq-less (aref arr i) (aref arr parent))
                (let ((tmp (aref arr i)))
                  (aset arr i (aref arr parent))
                  (aset arr parent tmp)
                  (setq i parent))
              (setq i 0))))))))

(defun ngit--pq-get (q)
  (let ((arr (aref q 0)) (n (aref q 1)))
    (cond
     ((= n 0) nil)
     ((not (aref q 3))
      (aset q 1 (1- n))
      (prog1 (cdr (aref arr (1- n))) (aset arr (1- n) nil)))
     (t
      (let ((top (cdr (aref arr 0))))
        (setq n (1- n))
        (aset q 1 n)
        (aset arr 0 (aref arr n))
        (aset arr n nil)
        (let ((i 0) (done nil))
          (while (not done)
            (let* ((l (1+ (* 2 i))) (r (1+ l)) (m i))
              (when (and (< l n) (ngit--pq-less (aref arr l) (aref arr m))) (setq m l))
              (when (and (< r n) (ngit--pq-less (aref arr r) (aref arr m))) (setq m r))
              (if (= m i)
                  (setq done t)
                (let ((tmp (aref arr i)))
                  (aset arr i (aref arr m))
                  (aset arr m tmp)
                  (setq i m))))))
        top)))))

(defun ngit--pq-reverse (q)
  (let ((arr (aref q 0)) (i 0) (j (1- (aref q 1))))
    (while (< i j)
      (let ((tmp (aref arr i)))
        (aset arr i (aref arr j))
        (aset arr j tmp))
      (setq i (1+ i) j (1- j)))))

;;;; Walk state (git's rev_info)

(defvar ngit--rv-repo nil)
(defvar ngit--rv-commits nil "Pending commits, newest first.")
(defvar ngit--rv-limited nil)
(defvar ngit--rv-first-parent nil)
(defvar ngit--rv-paths nil "Pathspec items when pruning by path, else nil.")
(defvar ngit--rv-dense t)
(defvar ngit--rv-simplify-history t)
(defvar ngit--rv-rewrite-parents nil)
(defvar ngit--rv-topo nil "nil, `graph' (topo order) or `date' (date order).")
(defvar ngit--rv-max-age nil)
(defvar ngit--rv-min-age nil)
(defvar ngit--rv-min-parents 0)
(defvar ngit--rv-max-parents -1)
(defvar ngit--rv-grep nil "Compiled grep filter, or nil.")
(defvar ngit--rv-max-count -1)
(defvar ngit--rv-skip 0)
(defvar ngit--rv-reverse nil)
(defvar ngit--rv-boundary nil)
(defvar ngit--rv-no-walk nil)

(defun ngit--parse (c) (ngit-parse-commit-object ngit--rv-repo c))

(defun ngit--mark-parents-uninteresting (c)
  (let ((stack nil))
    (dolist (p (ngit--c-parents c)) (push p stack))
    (setq stack (nreverse stack))
    (while stack
      (let ((p (pop stack)))
        (unless (ngit--has p ngit-uninteresting)
          (ngit--set p ngit-uninteresting)
          ;; Unparsed parents have no parents recorded yet; they are marked
          ;; when parsed (see `ngit--process-parents').
          (when (ngit--c-parsed p)
            (setq stack (append (ngit--c-parents p) stack))))))))

(defun ngit--relevant-p (c)
  (/= (logand (ngit--c-flags c) (logior ngit-uninteresting ngit-bottom))
      ngit-uninteresting))

(defun ngit--one-relevant-parent (parents)
  (cond ((null parents) nil)
        ((or ngit--rv-first-parent (null (cdr parents))) (car parents))
        (t (let ((relevant nil) (many nil))
             (dolist (p parents)
               (when (ngit--relevant-p p)
                 (if relevant (setq many t) (setq relevant p))))
             (and (not many) relevant)))))

;;;; Pathspecs and tree comparison

(defun ngit--ps-item (spec)
  "Compile pathspec SPEC (relative to the top) into (LITERAL . GLOB-P)."
  (let ((s (if (string-suffix-p "/" spec) (substring spec 0 -1) spec)))
    (cons s (and (ngit--glob-special-p s 0) t))))

(defun ngit--ps-literal-prefix (item)
  (let* ((s (car item)) (n (length s)) (i 0))
    (if (not (cdr item))
        s
      (while (and (< i n) (not (memq (aref s i) '(?* ?? ?\[ ?\\)))) (setq i (1+ i)))
      (substring s 0 i))))

(defun ngit--ps-class (path dirp)
  "How the pathspec relates to PATH: `full', `partial' (descend) or nil."
  (let ((result nil) (items ngit--rv-paths))
    (while (and items (not (eq result 'full)))
      (let* ((item (car items)) (s (car item)))
        (cond
         ((not (cdr item))
          (cond ((or (string= s "") (string= s path)
                     (and (> (length path) (length s))
                          (= (aref path (length s)) ?/)
                          (string-prefix-p s path)))
                 (setq result 'full))
                ((and dirp (> (length s) (length path))
                      (= (aref s (length path)) ?/)
                      (string-prefix-p path s))
                 (setq result 'partial))))
         (t
          (let ((lit (ngit--ps-literal-prefix item)))
            (cond ((and (not dirp) (string-prefix-p lit path)
                        (ngit-wildmatch s path))
                   (setq result 'full))
                  ((and dirp (or (string-prefix-p lit (concat path "/"))
                                 (string-prefix-p (concat path "/") lit)))
                   (setq result 'partial)))))))
      (setq items (cdr items)))
    result))

(defun ngit--log-path-class (path dirp gitlink)
  "Classify log PATH with directory literal intent, including GITLINK entries."
  (let ((items nil))
    (dolist (item ngit--rv-paths)
      (if (eq (cdr item) 'directory)
          (unless (and (string= (car item) path) (not dirp) (not gitlink))
            (push (cons (car item) nil) items))
        (push item items)))
    (let ((ngit--rv-paths (nreverse items)))
      (ngit--ps-class path dirp))))

(defun ngit--ps-tree-has-match (repo tree prefix)
  "Non-nil when tree TREE (20-byte name) has an entry the pathspec matches."
  (if (and (string= prefix "") (ngit--ps-literal-only-p))
      (ngit--ps-literal-differ repo nil tree)
   (let ((found nil) (items (ngit-tree-items repo tree)))
    (while (and items (not found))
      (let* ((e (car items))
             (dirp (= (nth 0 e) ngit-mode-tree))
             (path (concat prefix (nth 1 e)))
             (class (ngit--log-path-class path dirp (= (nth 0 e) #o160000))))
        (cond ((eq class 'full) (setq found t))
              ((eq class 'partial)
               (setq found (ngit--ps-tree-has-match repo (nth 2 e) (concat path "/"))))))
      (setq items (cdr items)))
    found)))

(defvar ngit--tree-data-cache (make-hash-table :test 'equal)
  "Raw tree objects by 20-byte name, for repeated pathspec lookups.")

(defun ngit--tree-data (repo bin)
  (or (gethash bin ngit--tree-data-cache)
      (progn
        (when (> (hash-table-count ngit--tree-data-cache) 512)
          (clrhash ngit--tree-data-cache))
        (puthash bin (cdr (ngit-read-object repo bin)) ngit--tree-data-cache))))

(defun ngit--tree-entry (repo tree name)
  "(MODE . BIN) of entry NAME in tree TREE (20-byte name), or nil.
The raw object is searched for \" NAME\\0\" natively instead of being
parsed entry by entry."
  (let* ((data (ngit--tree-data repo tree))
         (key (concat " " name "\0"))
         (pos 0) (found nil) (n (length data)))
    (while (and (not found) pos (< pos n))
      (let ((hit (string-search key data pos)))
        (if (null hit)
            (setq pos nil)
          ;; The match must start an entry: a tree mode that begins where
          ;; the previous entry ended.  Object ids may end in digits, so
          ;; only git's five modes are accepted.
          (let ((m (cond ((and (>= hit 6) (ngit--tree-entry-start-p data (- hit 6))
                               (member (substring data (- hit 6) hit)
                                       '("100644" "100755" "120000" "160000")))
                          (- hit 6))
                         ((and (>= hit 5) (ngit--tree-entry-start-p data (- hit 5))
                               (string= (substring data (- hit 5) hit) "40000"))
                          (- hit 5)))))
            (if m
                (let ((oid-at (+ hit (length key))))
                  (setq found (cons (string-to-number (substring data m hit) 8)
                                    (substring data oid-at (+ oid-at 20)))))
              (setq pos (1+ hit)))))))
    found))

(defun ngit--tree-entry-start-p (data m)
  "Non-nil when an entry of tree DATA starts at M."
  (or (= m 0)
      (and (>= m 22)
           ;; The previous entry ends with NUL and a 20-byte name; check the
           ;; NUL that precedes the previous name's object id.
           (= (aref data (- m 21)) 0))))

(defun ngit--ps-literal-only-p ()
  (not (seq-find (lambda (item) (cdr item)) ngit--rv-paths)))

(defun ngit--literal-entry (repo tree path)
  "(MODE . BIN) at PATH (\"a/b\") below TREE, or nil; TREE when PATH is \"\"."
  (if (string= path "")
      (cons ngit-mode-tree tree)
    (let ((parts (split-string path "/")) (cur (cons ngit-mode-tree tree)))
      (while (and parts cur)
        (setq cur (and (= (car cur) ngit-mode-tree)
                       (ngit--tree-entry repo (cdr cur) (car parts))))
        (setq parts (cdr parts)))
      cur)))

(defun ngit--ps-literal-differ (repo a b)
  "Pathspec difference for literal items: compare the entries they name."
  (let ((differ nil) (items ngit--rv-paths))
    (while (and items (not differ))
      (let ((ea (and a (ngit--literal-entry repo a (car (car items)))))
            (eb (and b (ngit--literal-entry repo b (car (car items))))))
        (unless (equal ea eb) (setq differ t)))
      (setq items (cdr items)))
    differ))

(defun ngit--ps-trees-differ (repo a b prefix)
  "Non-nil when trees A and B (20-byte names or nil) differ inside the pathspec."
  (cond
   ((equal a b) nil)
   ((and (string= prefix "") (ngit--ps-literal-only-p))
    (ngit--ps-literal-differ repo a b))
   ((null a) (ngit--ps-tree-has-match repo b prefix))
   ((null b) (ngit--ps-tree-has-match repo a prefix))
   (t
    (let ((as (ngit-tree-items repo a))
          (bs (ngit-tree-items repo b))
          (differ nil))
      (while (and (or as bs) (not differ))
        (let* ((ea (car as)) (eb (car bs))
               (ka (and ea (ngit--tree-key ea))) (kb (and eb (ngit--tree-key eb))))
          (cond
           ((and ea eb (string= ka kb))
            (unless (and (equal (nth 2 ea) (nth 2 eb)) (= (nth 0 ea) (nth 0 eb)))
              (let* ((dirp (= (nth 0 ea) ngit-mode-tree))
                     (path (concat prefix (nth 1 ea)))
                     (class (ngit--log-path-class path dirp (or (= (nth 0 ea) #o160000)
                                                        (= (nth 0 eb) #o160000)))))
                (cond ((eq class 'full) (setq differ t))
                      ((eq class 'partial)
                       (setq differ (ngit--ps-trees-differ repo (nth 2 ea) (nth 2 eb)
                                                           (concat path "/")))))))
            (setq as (cdr as) bs (cdr bs)))
           ((or (null eb) (and ea (string< ka kb)))
            (setq differ (ngit--ps-entry-matters repo ea prefix))
            (setq as (cdr as)))
           (t
            (setq differ (ngit--ps-entry-matters repo eb prefix))
            (setq bs (cdr bs))))))
      differ))))

(defun ngit--ps-entry-matters (repo e prefix)
  "Non-nil when the one-sided entry E has something inside the pathspec."
  (let* ((dirp (= (nth 0 e) ngit-mode-tree))
         (path (concat prefix (nth 1 e)))
         (class (ngit--log-path-class path dirp (= (nth 0 e) #o160000))))
    (cond ((eq class 'full) t)
          ((eq class 'partial) (ngit--ps-tree-has-match repo (nth 2 e) (concat path "/"))))))

(defun ngit--tree-bin (c) (ngit-unhex (ngit--c-tree c)))

;;;; Simplification (try_to_simplify_commit)

(defun ngit--try-to-simplify (c)
  (when ngit--rv-paths
    (let ((repo ngit--rv-repo))
      (if (null (ngit--c-parents c))
          (unless (ngit--ps-tree-has-match repo (ngit--tree-bin c) "")
            (ngit--set c ngit-treesame))
        (when (or ngit--rv-dense (cdr (ngit--c-parents c)))
          (let ((relevant-change nil) (irrelevant-change nil)
                (relevant-parents 0) (nth 0)
                (rest (ngit--c-parents c)) (done nil))
            (while (and rest (not done))
              (let ((p (car rest)))
                (when (ngit--relevant-p p)
                  (setq relevant-parents (1+ relevant-parents)))
                (if (and (= nth 1) ngit--rv-first-parent)
                    (setq rest nil)
                  (ngit--parse p)
                  (if (not (ngit--ps-trees-differ repo (ngit--tree-bin p) (ngit--tree-bin c) ""))
                      (if (or (not ngit--rv-simplify-history) (not (ngit--relevant-p p)))
                          (setq rest (cdr rest) nth (1+ nth))
                        ;; TREESAME to this parent: keep only it.
                        (aset c 2 (list p))
                        (ngit--set c ngit-treesame)
                        (setq done t))
                    (if (ngit--relevant-p p)
                        (setq relevant-change t)
                      (setq irrelevant-change t))
                    (when (= nth 0) (ngit--set c ngit-pull-merge))
                    (setq rest (cdr rest) nth (1+ nth))))))
            (unless done
              (when (if (> relevant-parents 0) (not relevant-change) (not irrelevant-change))
                (ngit--set c ngit-treesame)))))))))

;;;; Parents (process_parents)

(defun ngit--process-parents (c use-list)
  "Queue the parents of C onto `ngit--rv-commits' when USE-LIST."
  (unless (ngit--has c ngit-added)
    (ngit--set c ngit-added)
    (if (ngit--has c ngit-uninteresting)
        (let ((parents (ngit--c-parents c)) (stop nil))
          (while (and parents (not stop))
            (let ((p (car parents)))
              (setq parents (cdr parents))
              (ngit--set p ngit-uninteresting)
              (ngit--parse p)
              (when (ngit--c-parents p)
                (ngit--mark-parents-uninteresting p))
              (unless (ngit--has p ngit-seen)
                (ngit--set p ngit-seen)
                (when use-list
                  (setq ngit--rv-commits (ngit--insert-by-date p ngit--rv-commits)))))))
      (ngit--try-to-simplify c)
      (unless ngit--rv-no-walk
        (let ((pass (logand (ngit--c-flags c) ngit-symmetric-left))
              (parents (ngit--c-parents c)))
          (while parents
            (let ((p (car parents)))
              (ngit--parse p)
              (ngit--set p pass)
              (unless (ngit--has p ngit-seen)
                (ngit--set p ngit-seen)
                (when use-list
                  (setq ngit--rv-commits (ngit--insert-by-date p ngit--rv-commits)))))
            (setq parents (if ngit--rv-first-parent nil (cdr parents)))))))))

;;;; limit_list

(defconst ngit--slop 5)

(defun ngit--everybody-uninteresting (list)
  (let ((all t))
    (while (and list all)
      (unless (ngit--has (car list) ngit-uninteresting) (setq all nil))
      (setq list (cdr list)))
    all))

(defun ngit--limit-list ()
  (let ((slop ngit--slop)
        (date most-positive-fixnum)
        (newlist nil)
        (stop nil))
    (while (and ngit--rv-commits (not stop))
      (let ((c (pop ngit--rv-commits)))
        (when (and ngit--rv-max-age (< (ngit--c-date c) ngit--rv-max-age))
          (ngit--set c ngit-uninteresting))
        (ngit--process-parents c t)
        (if (ngit--has c ngit-uninteresting)
            (progn
              (ngit--mark-parents-uninteresting c)
              (setq slop (cond ((null ngit--rv-commits) 0)
                               ((<= date (ngit--c-date (car ngit--rv-commits))) ngit--slop)
                               ((not (ngit--everybody-uninteresting ngit--rv-commits)) ngit--slop)
                               (t (1- slop))))
              (when (= slop 0) (setq stop t)))
          (unless (and ngit--rv-min-age (> (ngit--c-date c) ngit--rv-min-age))
            (setq date (ngit--c-date c))
            (push c newlist)))))
    (setq ngit--rv-commits (nreverse newlist))))

;;;; Topological order (sort_in_topological_order)

(defun ngit--topo-sort (list)
  (let ((q (ngit--pq-new (eq ngit--rv-topo 'date)))
        (out nil))
    (dolist (c list) (aset c 9 1))
    (dolist (c list)
      (dolist (p (ngit--c-parents c))
        (let ((d (ngit--c-aux p)))
          (when (and d (> d 0)) (aset p 9 (1+ d))))))
    (dolist (c list)
      (when (eql (ngit--c-aux c) 1) (ngit--pq-put q c)))
    (unless (eq ngit--rv-topo 'date) (ngit--pq-reverse q))
    (let ((c (ngit--pq-get q)))
      (while c
        (dolist (p (ngit--c-parents c))
          (let ((d (ngit--c-aux p)))
            (when (and d (> d 0))
              (aset p 9 (1- d))
              (when (= d 2) (ngit--pq-put q p)))))
        (aset c 9 0)
        (push c out)
        (setq c (ngit--pq-get q))))
    (dolist (c list) (aset c 9 nil))
    (nreverse out)))

;;;; Commit selection (get_commit_action, simplify_commit)

(defun ngit--want-ancestry () ngit--rv-rewrite-parents)

(defun ngit--commit-action (c)
  "Return `show' or `ignore' for C."
  (cond
   ((ngit--has c ngit-shown) 'ignore)
   ((ngit--has c ngit-uninteresting) 'ignore)
   ((and ngit--rv-min-age (> (ngit--c-date c) ngit--rv-min-age)) 'ignore)
   ((let ((n (length (ngit--c-parents c))))
      (or (< n ngit--rv-min-parents)
          (and (>= ngit--rv-max-parents 0) (> n ngit--rv-max-parents))))
    'ignore)
   ((and ngit--rv-grep (not (ngit--grep-match ngit--rv-grep c))) 'ignore)
   ((and ngit--rv-paths ngit--rv-dense (ngit--has c ngit-treesame))
    (if (not (ngit--want-ancestry))
        'ignore
      (let ((n 0))
        (dolist (p (ngit--c-parents c))
          (when (ngit--relevant-p p) (setq n (1+ n))))
        (if (>= n 2) 'show 'ignore))))
   (t 'show)))

(defun ngit--rewrite-one (p)
  "Follow TREESAME commits from parent P; return (RESULT . COMMIT)."
  (let ((result nil) (queue nil))
    (while (not result)
      (unless ngit--rv-limited
        (let ((ngit--rv-commits nil))
          (ngit--process-parents p t)
          (setq queue (append queue ngit--rv-commits))))
      (cond ((ngit--has p ngit-uninteresting) (setq result 'ok))
            ((not (ngit--has p ngit-treesame)) (setq result 'ok))
            ((null (ngit--c-parents p)) (setq result 'noparents))
            (t (let ((next (ngit--one-relevant-parent (ngit--c-parents p))))
                 (if next (setq p next) (setq result 'ok))))))
    ;; merge_queue_into_list: the discovered commits join the pending list.
    (dolist (q (ngit--sort-by-date queue))
      (setq ngit--rv-commits (ngit--merge-into-list q ngit--rv-commits)))
    (cons result p)))

(defun ngit--merge-into-list (c list)
  ;; C was just marked SEEN, so it is not in LIST yet.
  (ngit--insert-by-date c list))

(defun ngit--rewrite-parents (c)
  (let ((out nil))
    (dolist (p (ngit--c-parents c))
      (let ((r (ngit--rewrite-one p)))
        (unless (eq (car r) 'noparents)
          (push (cdr r) out))))
    ;; remove_duplicate_parents
    (let ((seen nil) (uniq nil))
      (dolist (p (nreverse out))
        (unless (memq p seen) (push p seen) (push p uniq)))
      (aset c 2 (nreverse uniq)))))

(defun ngit--simplify-commit (c)
  (let ((action (ngit--commit-action c)))
    (when (and (eq action 'show) ngit--rv-paths ngit--rv-dense (ngit--want-ancestry))
      (ngit--rewrite-parents c))
    action))

;;;; get_revision

(defun ngit--get-revision-1 ()
  (let ((found nil))
    (while (and (not found) ngit--rv-commits)
      (let ((c (pop ngit--rv-commits)))
        (unless (and (not ngit--rv-limited) ngit--rv-max-age
                     (< (ngit--c-date c) ngit--rv-max-age))
          (unless ngit--rv-limited
            (ngit--process-parents c t))
          (when (eq (ngit--simplify-commit c) 'show)
            (setq found c)))))
    found))

(defun ngit--get-revision-internal ()
  (let ((c nil))
    (unless (= ngit--rv-max-count 0)
      (setq c (ngit--get-revision-1))
      (when c
        (while (and c (> ngit--rv-skip 0))
          (setq ngit--rv-skip (1- ngit--rv-skip))
          (setq c (ngit--get-revision-1))))
      (when (> ngit--rv-max-count 0)
        (setq ngit--rv-max-count (1- ngit--rv-max-count))))
    (when c (ngit--set c ngit-shown))
    c))

(defun ngit-walk-prepare (repo starts)
  "Start a walk in REPO from STARTS, a list of (COMMIT . FLAGS)."
  (setq ngit--rv-repo repo)
  (let ((list nil))
    (dolist (s starts)
      (let ((c (car s)))
        (ngit--parse c)
        (ngit--set c (cdr s))
        (when (ngit--has c ngit-uninteresting)
          (ngit--mark-parents-uninteresting c)
          (setq ngit--rv-limited t))
        (unless (ngit--has c ngit-seen)
          (ngit--set c ngit-seen)
          (push c list))))
    (setq ngit--rv-commits (ngit--sort-by-date (nreverse list))))
  (when ngit--rv-topo (setq ngit--rv-limited t))
  (when ngit--rv-limited
    (ngit--limit-list)
    (when ngit--rv-topo
      (setq ngit--rv-commits (ngit--topo-sort ngit--rv-commits)))))

(defun ngit-walk-next ()
  "Return the next commit of the walk, or nil."
  (ngit--get-revision-internal))

;;;; Grep filter (--author, --committer, --grep)

(defvar ngit--grep-icase nil)

(defun ngit--grep-compile (opts)
  "OPTS is a plist (:author LIST :committer LIST :grep LIST :all-match B
:invert B :icase B :fixed B)."
  (and (or (plist-get opts :author) (plist-get opts :committer) (plist-get opts :grep))
       opts))

(defun ngit--grep-re (pat mode)
  "Translate grep pattern PAT (MODE `fixed', `extended' or `basic') to an
Emacs regexp."
  (if (eq mode 'fixed)
      (regexp-quote pat)
    (let ((out nil) (i 0) (n (length pat)) (ere (eq mode 'extended)))
      (while (< i n)
        (let ((ch (aref pat i)))
          (cond
           ((and (= ch ?\\) (< (1+ i) n))
            (let ((nx (aref pat (1+ i))))
              (push (cond ((memq nx '(?\( ?\) ?\{ ?\} ?|)) (if ere (string nx) (string ?\\ nx)))
                          ((memq nx '(?+ ?\?)) (if ere (concat "\\" (string nx)) (string nx)))
                          (t (string ?\\ nx)))
                    out)
              (setq i (+ i 2))))
           ((memq ch '(?\( ?\) ?\{ ?\} ?|))
            (push (if ere (string ?\\ ch) (string ch)) out)
            (setq i (1+ i)))
           ((memq ch '(?+ ?\?))
            (push (if ere (string ch) (concat "[" (string ch) "]")) out)
            (setq i (1+ i)))
           (t (push (string ch) out) (setq i (1+ i))))))
      (apply #'concat (nreverse out)))))

(defun ngit--grep-match (opts c)
  (let* ((case-fold-search (plist-get opts :icase))
         (fixed (plist-get opts :mode))
         (hit (lambda (pats text)
                (let ((any nil))
                  (dolist (p pats)
                    (when (string-match-p (ngit--grep-re p fixed) text) (setq any t)))
                  any)))
         (authors (plist-get opts :author))
         (committers (plist-get opts :committer))
         (greps (plist-get opts :grep))
         (header-ok (and (or (null authors)
                             (funcall hit authors (car (ngit--split-ident (ngit--c-author c)))))
                         (or (null committers)
                             (funcall hit committers
                                      (car (ngit--split-ident (ngit--c-committer c)))))))
         (body-ok
          (cond ((null greps) t)
                ((plist-get opts :all-match)
                 (let ((all t))
                   (dolist (p greps)
                     (unless (funcall hit (list p) (ngit--c-message c)) (setq all nil)))
                   all))
                (t (funcall hit greps (ngit--c-message c))))))
    (when (and greps (plist-get opts :invert))
      (setq body-ok (not (funcall hit greps (ngit--c-message c)))))
    (and header-ok body-ok)))

;;;; Dates

(defun ngit--tz-seconds (tz)
  (let ((sign (if (= (aref tz 0) ?-) -1 1)))
    (* sign (+ (* 3600 (string-to-number (substring tz 1 3)))
               (* 60 (string-to-number (substring tz 3 5)))))))

(defun ngit--broken-down (stamp offset)
  "Return [YEAR MONTH DAY HOUR MIN SEC WDAY YDAY] for STAMP shifted by OFFSET."
  (let* ((local (+ stamp offset))
         (days (floor local 86400))
         (secs (- local (* days 86400)))
         (ymd (ngit--civil days)))
    (vector (nth 0 ymd) (nth 1 ymd) (nth 2 ymd)
            (/ secs 3600) (/ (% secs 3600) 60) (% secs 60)
            (mod (+ days 4) 7)
            (- days (ngit--days-from-civil (nth 0 ymd) 1 1)))))

(defconst ngit--full-days ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday"
                           "Friday" "Saturday"])
(defconst ngit--full-months ["January" "February" "March" "April" "May" "June" "July"
                             "August" "September" "October" "November" "December"])

(defun ngit--pad2 (n) (if (< n 10) (concat "0" (number-to-string n)) (number-to-string n)))

(defun ngit--strftime (fmt tm tz-string)
  "Expand strftime FMT for broken-down time TM (see `ngit--broken-down')."
  (let ((out nil) (i 0) (n (length fmt)))
    (while (< i n)
      (let ((ch (aref fmt i)))
        (if (or (/= ch ?%) (= (1+ i) n))
            (progn (push (string ch) out) (setq i (1+ i)))
          (let ((d (aref fmt (1+ i))))
            (push
             (pcase d
               (?Y (number-to-string (aref tm 0)))
               (?y (ngit--pad2 (% (aref tm 0) 100)))
               (?C (ngit--pad2 (/ (aref tm 0) 100)))
               (?m (ngit--pad2 (aref tm 1)))
               (?d (ngit--pad2 (aref tm 2)))
               (?e (let ((s (number-to-string (aref tm 2)))) (if (< (aref tm 2) 10) (concat " " s) s)))
               (?H (ngit--pad2 (aref tm 3)))
               (?I (ngit--pad2 (let ((h (% (aref tm 3) 12))) (if (= h 0) 12 h))))
               (?M (ngit--pad2 (aref tm 4)))
               (?S (ngit--pad2 (aref tm 5)))
               (?p (if (< (aref tm 3) 12) "AM" "PM"))
               (?a (aref ngit--days (aref tm 6)))
               (?A (aref ngit--full-days (aref tm 6)))
               (?b (aref ngit--months (1- (aref tm 1))))
               (?h (aref ngit--months (1- (aref tm 1))))
               (?B (aref ngit--full-months (1- (aref tm 1))))
               (?u (number-to-string (if (= (aref tm 6) 0) 7 (aref tm 6))))
               (?w (number-to-string (aref tm 6)))
               (?j (let ((s (number-to-string (1+ (aref tm 7)))))
                     (concat (make-string (- 3 (length s)) ?0) s)))
               (?F (concat (number-to-string (aref tm 0)) "-" (ngit--pad2 (aref tm 1))
                           "-" (ngit--pad2 (aref tm 2))))
               (?T (concat (ngit--pad2 (aref tm 3)) ":" (ngit--pad2 (aref tm 4)) ":"
                           (ngit--pad2 (aref tm 5))))
               (?R (concat (ngit--pad2 (aref tm 3)) ":" (ngit--pad2 (aref tm 4))))
               (?D (concat (ngit--pad2 (aref tm 1)) "/" (ngit--pad2 (aref tm 2)) "/"
                           (ngit--pad2 (% (aref tm 0) 100))))
               (?z tz-string)
               (?Z "")
               (?n "\n")
               (?t "\t")
               (?% "%")
               (_ (string ?% d)))
             out)
            (setq i (+ i 2))))))
    (apply #'concat (nreverse out))))

(defun ngit-show-date (stamp tz mode)
  "Format STAMP (with zone string TZ) in date MODE, a cons (TYPE . LOCAL)
with TYPE one of normal, iso, iso-strict, rfc, short, raw, unix or a
strftime format string."
  (let* ((type (car mode))
         (local (cdr mode))
         (tz (if local (ngit--tz-string (ngit-local-offset stamp)) tz))
         (off (ngit--tz-seconds tz)))
    (cond
     ((eq type 'unix) (number-to-string stamp))
     ((eq type 'raw) (concat (number-to-string stamp) " " tz))
     ((eq type 'normal)
      (let ((s (ngit-format-date stamp tz)))
        (if local (substring s 0 -6) s)))
     (t
      (let ((tm (ngit--broken-down stamp off)))
        (cond
         ((eq type 'short)
          (concat (number-to-string (aref tm 0)) "-" (ngit--pad2 (aref tm 1)) "-"
                  (ngit--pad2 (aref tm 2))))
         ((eq type 'iso)
          (concat (number-to-string (aref tm 0)) "-" (ngit--pad2 (aref tm 1)) "-"
                  (ngit--pad2 (aref tm 2)) " " (ngit--pad2 (aref tm 3)) ":"
                  (ngit--pad2 (aref tm 4)) ":" (ngit--pad2 (aref tm 5)) " " tz))
         ((eq type 'iso-strict)
          (concat (number-to-string (aref tm 0)) "-" (ngit--pad2 (aref tm 1)) "-"
                  (ngit--pad2 (aref tm 2)) "T" (ngit--pad2 (aref tm 3)) ":"
                  (ngit--pad2 (aref tm 4)) ":" (ngit--pad2 (aref tm 5))
                  (if (= off 0) "Z" (concat (substring tz 0 3) ":" (substring tz 3 5)))))
         ((eq type 'rfc)
          (concat (aref ngit--days (aref tm 6)) ", " (number-to-string (aref tm 2)) " "
                  (aref ngit--months (1- (aref tm 1))) " " (number-to-string (aref tm 0))
                  " " (ngit--pad2 (aref tm 3)) ":" (ngit--pad2 (aref tm 4)) ":"
                  (ngit--pad2 (aref tm 5)) " " tz))
         ((stringp type) (ngit--strftime type tm (if local tz tz)))
         (t (ngit--die "fatal: unsupported date format"))))))))

(defun ngit-parse-date-mode (spec)
  "Parse a --date= value into a date mode (TYPE . LOCAL)."
  (let ((s (if (string= spec "local") "default-local" spec))
        (type nil) (rest nil))
    (dolist (pair '(("iso8601-strict" . iso-strict) ("iso-strict" . iso-strict)
                    ("iso8601" . iso) ("iso" . iso) ("rfc2822" . rfc) ("rfc" . rfc)
                    ("short" . short) ("default" . normal) ("raw" . raw) ("unix" . unix)
                    ("format" . format) ("relative" . relative) ("human" . human)))
      (when (and (null type) (string-prefix-p (car pair) s))
        (setq type (cdr pair) rest (substring s (length (car pair))))))
    (unless type (ngit--die "fatal: unknown date format %s" spec))
    (when (memq type '(relative human))
      (ngit--die "fatal: ngit does not support --date=%s" spec))
    (let ((local (string-prefix-p "-local" rest)))
      (when local (setq rest (substring rest 6)))
      (if (eq type 'format)
          (progn
            (unless (string-prefix-p ":" rest)
              (ngit--die "fatal: date format missing colon separator: %s" spec))
            (cons (substring rest 1) local))
        (unless (string= rest "") (ngit--die "fatal: unknown date format %s" spec))
        (cons type local)))))

;;;; Decorations

(defvar ngit--decorations nil "Hash: commit hex -> list of (TYPE . REFNAME), git's order.")
(defvar ngit--decorate-full nil)

(defun ngit--load-decorations (repo)
  (let ((table (make-hash-table :test 'equal))
        (add (lambda (table hex item) (puthash hex (cons item (gethash hex table)) table))))
    (dolist (r (ngit-list-refs repo "refs/"))
      (let* ((name (nth 0 r))
             (type (cond ((string-prefix-p "refs/heads/" name) 'local)
                         ((string-prefix-p "refs/tags/" name) 'tag)
                         ((string-prefix-p "refs/remotes/" name) 'remote)
                         ((string= name "refs/stash") 'stash))))
        (when type
          (let ((hex (nth 1 r)))
            (funcall add table hex (cons type name))
            (let ((obj (ngit-read-object repo (ngit-unhex hex))))
              (while (eq (car obj) 'tag)
                (setq hex (ngit-header (ngit-parse-commit (cdr obj)) "object"))
                (funcall add table hex (cons 'tag name))
                (setq obj (ngit-read-object repo (ngit-unhex hex)))))))))
    (let ((head (ngit-resolve-ref repo "HEAD")))
      (when head (funcall add table head (cons 'head "HEAD"))))
    (setq ngit--decorations table)))

(defun ngit--short-refname (name)
  (cond (ngit--decorate-full name)
        ((string-prefix-p "refs/heads/" name) (substring name 11))
        ((string-prefix-p "refs/tags/" name) (substring name 10))
        ((string-prefix-p "refs/remotes/" name) (substring name 13))
        (t name)))

(defun ngit-format-decorations (repo c prefix suffix)
  "Return C's decorations as git's format_decorations, or \"\"."
  (let ((decos (and ngit--decorations (gethash (ngit--c-hex c) ngit--decorations))))
    (if (null decos)
        ""
      (let* ((head-item (assq 'head decos))
             (target (and head-item
                          (let ((raw (ngit-read-ref repo "HEAD")))
                            (and raw (string-prefix-p "ref: refs/" raw) (substring raw 5)))))
             (current (and target
                           (seq-find (lambda (d) (and (eq (car d) 'local) (string= (cdr d) target)))
                                     decos)))
             (parts nil))
        (dolist (d decos)
          (unless (eq d current)
            (push (concat (if (eq (car d) 'tag) "tag: " "")
                          (ngit--short-refname (cdr d))
                          (if (and current (eq (car d) 'head))
                              (concat " -> " (ngit--short-refname (cdr current)))
                            ""))
                  parts)))
        (concat prefix (mapconcat #'identity (nreverse parts) ", ") suffix)))))

;;;; Pretty printing

(defvar ngit--pp-abbrev nil "Abbreviation length for %h etc., or nil for full names.")

(defun ngit--abbrev (hex)
  (if ngit--pp-abbrev (ngit-abbrev ngit--rv-repo hex ngit--pp-abbrev) hex))

(defun ngit--message-lines (msg)
  "Split MSG into lines (without newlines); a final newline adds nothing."
  (let ((lines (split-string msg "\n")))
    (if (and lines (string= (car (last lines)) "")) (butlast lines) lines)))

(defun ngit--rtrim (s)
  (let ((e (length s)))
    (while (and (> e 0) (memq (aref s (1- e)) '(?\s ?\t ?\n ?\r ?\v ?\f))) (setq e (1- e)))
    (if (= e (length s)) s (substring s 0 e))))

(defun ngit--skip-blank (lines)
  (while (and lines (string= (ngit--rtrim (car lines)) "")) (setq lines (cdr lines)))
  lines)

(defun ngit--subject (msg)
  "The subject of MSG: its first paragraph, lines joined with spaces."
  (let ((n (length msg)) (pos 0))
    ;; Fast path: a non-blank first line followed by a blank line or the end.
    (let ((eol (ngit--byte-pos msg ?\n 0)))
      (if (and (> n 0) (not (memq (aref msg 0) '(?\s ?\t ?\n ?\r)))
               (or (null eol) (= (1+ eol) n) (= (aref msg (1+ eol)) ?\n)))
          (ngit--rtrim (if eol (substring msg 0 eol) msg))
        (ignore pos)
        (mapconcat #'identity (car (ngit--subject-and-rest msg)) " ")))))

(defun ngit--subject-and-rest (msg)
  "Return (SUBJECT-LINES . REST-LINES) of commit message MSG."
  (let ((lines (ngit--skip-blank (ngit--message-lines msg)))
        (subject nil))
    (while (and lines (not (string= (ngit--rtrim (car lines)) "")))
      (push (ngit--rtrim (car lines)) subject)
      (setq lines (cdr lines)))
    (cons (nreverse subject) lines)))

(defun ngit--expand-tabs (line)
  "Expand tabs to 8 columns the way git's strbuf_add_tabexpand does."
  (if (not (ngit--byte-pos line ?\t 0))
      line
    (let ((out nil) (col 0) (i 0) (n (length line)) (start 0))
      (while (< i n)
        (when (= (aref line i) ?\t)
          (let ((seg (substring line start i)))
            (setq col (+ col (ngit--display-width seg)))
            (push seg out)
            (push (make-string (- 8 (% col 8)) ?\s) out)
            (setq col 0 start (1+ i))))
        (setq i (1+ i)))
      (push (substring line start) out)
      (apply #'concat (nreverse out)))))

(defun ngit--pp-remainder (lines indent short expand)
  "Lines of the body after the subject, as git's pp_remainder."
  (let ((out nil) (first t) (stop nil))
    (while (and lines (not stop))
      (let ((l (ngit--rtrim (car lines))))
        (setq lines (cdr lines))
        (if (string= l "")
            (cond (first nil)
                  (short (setq stop t))
                  (t (push (concat indent "\n") out)))
          (setq first nil)
          (push (concat indent (if expand (ngit--expand-tabs l) l) "\n") out))))
    (apply #'concat (nreverse out))))

(defun ngit--pp-user (what ident fmt date-mode)
  (let* ((split (ngit--split-ident ident))
         (out (concat what ": " (if (eq fmt 'fuller) "    " "") (nth 0 split) "\n")))
    (pcase fmt
      ('medium (concat out "Date:   " (ngit-show-date (nth 1 split) (nth 2 split) date-mode) "\n"))
      ('fuller (concat out what "Date: " (ngit-show-date (nth 1 split) (nth 2 split) date-mode) "\n"))
      (_ out))))

(defun ngit-pretty-builtin (c fmt date-mode)
  "Message part of a built-in format FMT (medium, short, full, fuller, raw,
oneline) for commit C, as git's pretty_print_commit."
  (if (eq fmt 'oneline)
      (ngit--subject (ngit--c-message c))
    (let ((head
           (if (eq fmt 'raw)
               (let ((data (ngit--c-data c)))
                 (substring data 0 (- (length data) (length (ngit--c-message c)) 1)))
             (concat
              (if (cdr (ngit--c-parents c))
                  (concat "Merge:"
                          (mapconcat (lambda (p) (concat " " (ngit--abbrev-merge (ngit--c-hex p))))
                                     (ngit--c-parents c) "")
                          "\n")
                "")
              (ngit--pp-user "Author" (ngit--c-author c) fmt date-mode)
              (if (memq fmt '(full fuller))
                  (ngit--pp-user "Commit" (ngit--c-committer c) fmt date-mode)
                ""))))
          (lines (ngit--skip-blank (ngit--message-lines (ngit--c-message c)))))
      (let ((body (ngit--pp-remainder lines "    " (eq fmt 'short)
                                      (memq fmt '(medium full fuller)))))
        (concat (ngit--rtrim (concat head "\n" body)) "\n")))))

(defvar ngit--merge-abbrev 'default)

(defun ngit--abbrev-merge (hex)
  (if (eq ngit--merge-abbrev 'full) hex (ngit-abbrev ngit--rv-repo hex)))

(defun ngit--person-part (ident part date-mode)
  "Expand %aX / %cX for IDENT; return (STRING . CONSUMED) or nil."
  (let* ((split (ngit--split-ident ident))
         (ne (nth 0 split))
         (lt (ngit--byte-pos ne ?< 0))
         (name (if lt (ngit--rtrim (substring ne 0 lt)) ne))
         (mail (if lt (substring ne (1+ lt) (max (1+ lt) (1- (length ne)))) "")))
    (pcase part
      ((or ?n ?N) name)
      ((or ?e ?E) mail)
      ((or ?l ?L) (let ((at (ngit--byte-pos mail ?@ 0))) (if at (substring mail 0 at) mail)))
      (?t (number-to-string (nth 1 split)))
      (?d (ngit-show-date (nth 1 split) (nth 2 split) date-mode))
      (?D (ngit-show-date (nth 1 split) (nth 2 split) '(rfc)))
      (?i (ngit-show-date (nth 1 split) (nth 2 split) '(iso)))
      (?I (ngit-show-date (nth 1 split) (nth 2 split) '(iso-strict)))
      (?s (ngit-show-date (nth 1 split) (nth 2 split) '(short)))
      (_ nil))))

(defun ngit--sanitized-subject (s)
  (let ((out nil) (space 2) (i 0) (n (length s)))
    (while (< i n)
      (let ((ch (aref s i)))
        (if (or (and (>= ch ?a) (<= ch ?z)) (and (>= ch ?A) (<= ch ?Z))
                (and (>= ch ?0) (<= ch ?9)) (= ch ?.) (= ch ?_))
            (progn
              (when (= space 1) (push "-" out))
              (setq space 0)
              (push (string ch) out)
              (when (= ch ?.)
                (while (and (< (1+ i) n) (= (aref s (1+ i)) ?.)) (setq i (1+ i)))))
          (setq space (logior space 1))))
      (setq i (1+ i)))
    (let ((r (apply #'concat (nreverse out))))
      (while (and (> (length r) 0) (memq (aref r (1- (length r))) '(?. ?-)))
        (setq r (substring r 0 -1)))
      r)))

(defun ngit--body-text (c)
  "%b: the message after the subject paragraph and following blank lines."
  (let* ((msg (ngit--c-message c))
         (n (length msg)) (pos 0))
    ;; Skip blank lines, the subject paragraph, then blank lines.
    (let ((phase 0) (done nil))
      (while (and (not done) (< pos n))
        (let* ((eol (or (ngit--byte-pos msg ?\n pos) n))
               (blank (string= (ngit--rtrim (substring msg pos eol)) "")))
          (cond ((and (= phase 0) blank) (setq pos (min n (1+ eol))))
                ((= phase 0) (setq phase 1 pos (min n (1+ eol))))
                ((and (= phase 1) (not blank)) (setq pos (min n (1+ eol))))
                ((= phase 1) (setq phase 2 pos (min n (1+ eol))))
                ((and (= phase 2) blank) (setq pos (min n (1+ eol))))
                (t (setq done t))))))
    (substring msg pos)))

(defun ngit--format-one (c fmt i date-mode)
  "Expand the placeholder of user format FMT at I (after %); return
(STRING . CONSUMED) or nil when it is not a placeholder."
  (let* ((n (length fmt))
         (ch (aref fmt i))
         (next (and (< (1+ i) n) (aref fmt (1+ i)))))
    (pcase ch
      (?n '("\n" . 1))
      (?x (if (and next (< (+ i 2) n))
              (let ((v (string-to-number (substring fmt (1+ i) (+ i 3)) 16)))
                (cons (string v) 3))
            nil))
      (?H (cons (ngit--c-hex c) 1))
      (?h (cons (ngit-abbrev ngit--rv-repo (ngit--c-hex c) ngit--pp-abbrev-format) 1))
      (?T (cons (ngit--c-tree c) 1))
      (?t (cons (ngit-abbrev ngit--rv-repo (ngit--c-tree c) ngit--pp-abbrev-format) 1))
      (?P (cons (mapconcat (lambda (p) (ngit--c-hex p)) (ngit--c-parents c) " ") 1))
      (?p (cons (mapconcat (lambda (p) (ngit-abbrev ngit--rv-repo (ngit--c-hex p) ngit--pp-abbrev-format))
                           (ngit--c-parents c) " ")
                1))
      (?d (cons (ngit-format-decorations ngit--rv-repo c " (" ")") 1))
      (?D (cons (ngit-format-decorations ngit--rv-repo c "" "") 1))
      (?m (cons (cond ((ngit--has c ngit-boundary) "-")
                      ((ngit--has c ngit-uninteresting) "^")
                      ((ngit--has c ngit-symmetric-left) "<")
                      (t ">"))
                1))
      ((or ?a ?c)
       (and next
            (let ((s (ngit--person-part (if (= ch ?a) (ngit--c-author c) (ngit--c-committer c))
                                        next date-mode)))
              (and s (cons s 2)))))
      (?e '("" . 1))
      (?B (cons (ngit--c-message c) 1))
      (?s (cons (ngit--subject (ngit--c-message c)) 1))
      (?f (cons (ngit--sanitized-subject
                 (let* ((m (ngit--c-message c))
                        (lines (ngit--skip-blank (ngit--message-lines m))))
                   (or (car lines) "")))
                1))
      (?b (cons (ngit--body-text c) 1))
      (?C (cond ((string-prefix-p "(auto)" (substring fmt (1+ i))) '("" . 7))
                ((string-prefix-p "reset" (substring fmt (1+ i))) '("" . 6))
                ((string-prefix-p "red" (substring fmt (1+ i))) '("" . 4))
                ((string-prefix-p "green" (substring fmt (1+ i))) '("" . 6))
                ((string-prefix-p "blue" (substring fmt (1+ i))) '("" . 5))
                ((and next (= next ?\())
                 (let ((close (ngit--byte-pos fmt ?\) i)))
                   (and close (cons "" (- close i -1)))))
                (t nil)))
      (_ nil))))

(defvar ngit--pp-abbrev-format nil "Abbreviation length for user formats (nil = default).")

(defun ngit-format-user (c fmt date-mode)
  "Expand user format FMT for commit C, as git's repo_format_commit_message."
  (let ((out nil) (i 0) (n (length fmt)))
    (while (< i n)
      (let ((pct (ngit--byte-pos fmt ?% i)))
        (if (null pct)
            (progn (push (substring fmt i) out) (setq i n))
          (when (> pct i) (push (substring fmt i pct) out))
          (setq i (1+ pct))
          (cond
           ((>= i n) (push "%" out))
           ((= (aref fmt i) ?%) (push "%" out) (setq i (1+ i)))
           (t
            (let* ((magic (and (memq (aref fmt i) '(?+ ?- ?\s)) (< (1+ i) n) (aref fmt i)))
                   (at (if magic (1+ i) i))
                   (r (ngit--format-one c fmt at date-mode)))
              (if (null r)
                  (push "%" out)
                (let ((s (car r)))
                  (cond
                   ((null magic) (push s out))
                   ((= magic ?-)
                    (if (string= s "")
                        ;; Delete the line feeds before an empty expansion.
                        (let ((acc (apply #'concat (nreverse out))))
                          (while (and (> (length acc) 0) (= (aref acc (1- (length acc))) ?\n))
                            (setq acc (substring acc 0 -1)))
                          (setq out (list acc)))
                      (push s out)))
                   ((string= s "") nil)
                   ((= magic ?+) (push (concat "\n" s) out))
                   (t (push (concat " " s) out)))
                  (setq i (+ at (cdr r)))))))))))
    (apply #'concat (nreverse out))))

;;;; Merge bases (commit-reach.c)

(defconst ngit--all-paint-flags (logior ngit-parent1 ngit-parent2 ngit-stale ngit-result))

(defun ngit--queue-has-nonstale (q)
  (let ((arr (aref q 0)) (i 0) (n (aref q 1)) (found nil))
    (while (and (< i n) (not found))
      (unless (ngit--has (cdr (aref arr i)) ngit-stale) (setq found t))
      (setq i (1+ i)))
    found))

(defun ngit--paint-down-to-common (one twos)
  "Port of paint_down_to_common; return the common commits, newest first."
  (let ((q (ngit--pq-new t)) (result nil))
    (ngit--set one ngit-parent1)
    (if (null twos)
        (list one)
      (ngit--pq-put q one)
      (dolist (c twos) (ngit--set c ngit-parent2) (ngit--pq-put q c))
      (while (ngit--queue-has-nonstale q)
        (let* ((c (ngit--pq-get q))
               (flags (logand (ngit--c-flags c)
                              (logior ngit-parent1 ngit-parent2 ngit-stale))))
          (when (= flags (logior ngit-parent1 ngit-parent2))
            (unless (ngit--has c ngit-result)
              (ngit--set c ngit-result)
              (setq result (ngit--insert-by-date c result)))
            (setq flags (logior flags ngit-stale)))
          (dolist (p (ngit--c-parents c))
            (unless (= (logand (ngit--c-flags p) flags) flags)
              (ngit--parse p)
              (ngit--set p flags)
              (ngit--pq-put q p)))))
      result)))

(defun ngit--clear-marks (c flags)
  (let ((stack (list c)))
    (while stack
      (let ((x (pop stack)))
        (when (/= 0 (logand (ngit--c-flags x) flags))
          (ngit--clear x flags)
          (dolist (p (ngit--c-parents x)) (push p stack)))))))

(defun ngit--remove-redundant (commits)
  "Port of remove_redundant_no_gen: drop commits reachable from others."
  (let* ((arr (vconcat commits)) (cnt (length arr))
         (redundant (make-vector cnt nil)) (i 0))
    (while (< i cnt)
      (unless (aref redundant i)
        (let ((work nil) (idx nil) (j 0))
          (while (< j cnt)
            (unless (or (= i j) (aref redundant j))
              (push (aref arr j) work) (push j idx))
            (setq j (1+ j)))
          (setq work (nreverse work) idx (nreverse idx))
          (ngit--paint-down-to-common (aref arr i) work)
          (when (ngit--has (aref arr i) ngit-parent2) (aset redundant i t))
          (let ((w work) (k idx))
            (while w
              (when (ngit--has (car w) ngit-parent1) (aset redundant (car k) t))
              (setq w (cdr w) k (cdr k))))
          (ngit--clear-marks (aref arr i) ngit--all-paint-flags)
          (dolist (x work) (ngit--clear-marks x ngit--all-paint-flags))))
      (setq i (1+ i)))
    (let ((out nil) (k 0))
      (while (< k cnt)
        (unless (aref redundant k) (push (aref arr k) out))
        (setq k (1+ k)))
      (nreverse out))))

(defun ngit-merge-bases (repo one twos)
  "Return the merge bases of commit ONE and the list TWOS, newest first."
  (setq ngit--rv-repo repo)
  (ngit--parse one)
  (dolist (c twos) (ngit--parse c))
  (cond
   ((or (null twos) (memq one twos)) (list one))
   ;; A direct parent or a shared sole parent is already the unique best
   ;; base, even when commit timestamps run backwards.  Date queues would
   ;; otherwise visit a large older history before processing the other tip.
   ((and (null (cdr twos)) (memq one (ngit--c-parents (car twos)))) (list one))
   ((and (null (cdr twos)) (memq (car twos) (ngit--c-parents one))) (list (car twos)))
   ((and (null (cdr twos)) (ngit--c-parents one)
         (null (cdr (ngit--c-parents one)))
         (null (cdr (ngit--c-parents (car twos))))
         (eq (car (ngit--c-parents one)) (car (ngit--c-parents (car twos)))))
    (list (ngit--parse (car (ngit--c-parents one)))))
   (t (let* ((list (ngit--paint-down-to-common one twos))
           (result nil))
      (dolist (c list)
        (unless (ngit--has c ngit-stale)
          (setq result (ngit--insert-by-date c result))))
      (ngit--clear-marks one ngit--all-paint-flags)
      (dolist (c twos) (ngit--clear-marks c ngit--all-paint-flags))
      (if (or (null result) (null (cdr result)))
          result
        (let ((out nil))
          (dolist (c (ngit--remove-redundant result))
            (setq out (ngit--insert-by-date c out)))
          out))))))

;;;; Revision arguments

(defun ngit--rev-commit (repo rev)
  "The commit vector REV names (peeling tags), or nil when REV is unknown."
  (let ((hex (condition-case nil (ngit-rev-parse repo rev) (error nil))))
    (and hex
         (let ((peeled (condition-case nil (ngit-peel repo hex 'commit) (error nil))))
           (and peeled (ngit-commit peeled))))))

(defun ngit--bad-revision (rev)
  (ngit--die "fatal: ambiguous argument '%s': unknown revision or path not in the working tree.
Use '--' to separate paths from revisions, like this:
'git <command> [<revision>...] -- [<file>...]'" rev))

(defun ngit--handle-revision (repo arg flags pending)
  "Add the commits named by ARG with FLAGS to PENDING (reversed); return
the new PENDING, or the symbol `not-a-rev'."
  (let ((dots (string-search ".." arg)))
    (cond
     ((and dots (not (string-prefix-p "^" arg)))
      (let* ((symmetric (and (< (+ dots 2) (length arg)) (= (aref arg (+ dots 2)) ?.)))
             (a-name (substring arg 0 dots))
             (b-name (substring arg (+ dots (if symmetric 3 2))))
             (a-name (if (string= a-name "") "HEAD" a-name))
             (b-name (if (string= b-name "") "HEAD" b-name))
             (a (ngit--rev-commit repo a-name))
             (b (ngit--rev-commit repo b-name))
             (exclude (logxor flags (logior ngit-uninteresting ngit-bottom))))
        (if (not (and a b))
            'not-a-rev
          (if symmetric
              (progn
                (dolist (base (ngit-merge-bases repo a (list b)))
                  (push (cons base exclude) pending))
                (push (cons a (logior flags ngit-symmetric-left)) pending)
                (push (cons b flags) pending))
            (push (cons a exclude) pending)
            (push (cons b flags) pending))
          pending)))
     ((string-suffix-p "^@" arg)
      (let ((c (ngit--rev-commit repo (substring arg 0 -2))))
        (if (null c) 'not-a-rev
          (ngit--parse c)
          (dolist (p (ngit--c-parents c)) (push (cons p flags) pending))
          pending)))
     ((string-suffix-p "^!" arg)
      (let ((c (ngit--rev-commit repo (substring arg 0 -2))))
        (if (null c) 'not-a-rev
          (ngit--parse c)
          (push (cons c flags) pending)
          (dolist (p (ngit--c-parents c))
            (push (cons p (logxor flags (logior ngit-uninteresting ngit-bottom))) pending))
          pending)))
     ((string-prefix-p "^" arg)
      (let ((c (ngit--rev-commit repo (substring arg 1))))
        (if (null c) 'not-a-rev
          (push (cons c (logxor flags (logior ngit-uninteresting ngit-bottom))) pending))))
     (t
      (let ((c (ngit--rev-commit repo arg)))
        (if (null c) 'not-a-rev (push (cons c flags) pending)))))))

(defun ngit--ref-glob-match (pattern name)
  "git's refs glob: a pattern without wildcards matches as a prefix."
  (if (ngit--glob-special-p pattern 0)
      (ngit-wildmatch pattern name t)
    (let ((p (if (string-suffix-p "/" pattern) pattern (concat pattern "/"))))
      (string-prefix-p p name))))

(defun ngit--add-refs (repo prefix pattern flags pending)
  "Add the refs below PREFIX (matching PATTERN when given) to PENDING."
  (dolist (r (ngit-list-refs repo prefix))
    (when (or (null pattern)
              (ngit--ref-glob-match
               (if (string-prefix-p "refs/" pattern) pattern (concat prefix pattern))
               (nth 0 r)))
      (let ((obj (ngit-read-object repo (ngit-unhex (nth 1 r)))))
        (let ((hex (nth 1 r)))
          (while (eq (car obj) 'tag)
            (setq hex (ngit-header (ngit-parse-commit (cdr obj)) "object")
                  obj (ngit-read-object repo (ngit-unhex hex))))
          (when (eq (car obj) 'commit)
            (push (cons (ngit-commit hex) flags) pending))))))
  pending)

;;;; History graph (graph.c), without colors

(defvar ngit--graph nil "Non-nil while --graph output is active.")
(defvar ngit--g-commit nil)
(defvar ngit--g-num-parents 0)
(defvar ngit--g-width 0)
(defvar ngit--g-expansion-row 0)
(defvar ngit--g-state 'padding)
(defvar ngit--g-prev-state 'padding)
(defvar ngit--g-commit-index 0)
(defvar ngit--g-prev-commit-index 0)
(defvar ngit--g-merge-layout 0)
(defvar ngit--g-edges-added 0)
(defvar ngit--g-prev-edges-added 0)
(defvar ngit--g-num-columns 0)
(defvar ngit--g-num-new-columns 0)
(defvar ngit--g-mapping-size 0)
(defvar ngit--g-columns nil)
(defvar ngit--g-new-columns nil)
(defvar ngit--g-mapping nil)
(defvar ngit--g-old-mapping nil)
(defvar ngit--gl nil "The line being built: (REVERSED-STRINGS . WIDTH).")

(defun ngit-graph-init ()
  (setq ngit--graph t ngit--g-commit nil ngit--g-num-parents 0 ngit--g-width 0
        ngit--g-expansion-row 0 ngit--g-state 'padding ngit--g-prev-state 'padding
        ngit--g-commit-index 0 ngit--g-prev-commit-index 0 ngit--g-merge-layout 0
        ngit--g-edges-added 0 ngit--g-prev-edges-added 0 ngit--g-num-columns 0
        ngit--g-num-new-columns 0 ngit--g-mapping-size 0
        ngit--g-columns (make-vector 30 nil) ngit--g-new-columns (make-vector 30 nil)
        ngit--g-mapping (make-vector 60 -1) ngit--g-old-mapping (make-vector 60 -1)))

(defun ngit--gl-addch (ch)
  (setcar ngit--gl (cons (string ch) (car ngit--gl)))
  (setcdr ngit--gl (1+ (cdr ngit--gl))))

(defun ngit--gl-addchars (ch n)
  (when (> n 0)
    (setcar ngit--gl (cons (make-string n ch) (car ngit--gl)))
    (setcdr ngit--gl (+ n (cdr ngit--gl)))))

(defun ngit--gl-addstr (s)
  (setcar ngit--gl (cons s (car ngit--gl)))
  (setcdr ngit--gl (+ (length s) (cdr ngit--gl))))

(defun ngit--g-update-state (s)
  (setq ngit--g-prev-state ngit--g-state ngit--g-state s))

(defun ngit--g-ensure-capacity (n)
  (when (< (length ngit--g-columns) n)
    (let ((cap (length ngit--g-columns)))
      (while (< cap n) (setq cap (* 2 cap)))
      (let ((grow (lambda (v size fill)
                    (let ((new (make-vector size fill)) (i 0))
                      (while (< i (length v)) (aset new i (aref v i)) (setq i (1+ i)))
                      new))))
        (setq ngit--g-columns (funcall grow ngit--g-columns cap nil)
              ngit--g-new-columns (funcall grow ngit--g-new-columns cap nil)
              ngit--g-mapping (funcall grow ngit--g-mapping (* 2 cap) -1)
              ngit--g-old-mapping (funcall grow ngit--g-old-mapping (* 2 cap) -1))))))

(defun ngit--g-interesting-p (c)
  (ngit--parse c)
  (eq (ngit--commit-action c) 'show))

(defun ngit--g-next-interesting (rest)
  "The cell after REST (a tail of the parent list) holding an interesting parent."
  (unless ngit--rv-first-parent
    (let ((l (cdr rest)) (found nil))
      (while (and l (not found))
        (if (ngit--g-interesting-p (car l)) (setq found l) (setq l (cdr l))))
      found)))

(defun ngit--g-first-interesting ()
  (let ((parents (ngit--c-parents ngit--g-commit)))
    (cond ((null parents) nil)
          ((ngit--g-interesting-p (car parents)) parents)
          (t (ngit--g-next-interesting parents)))))

(defun ngit--g-find-new-column (c)
  (let ((i 0) (found -1))
    (while (and (< i ngit--g-num-new-columns) (< found 0))
      (when (eq (aref ngit--g-new-columns i) c) (setq found i))
      (setq i (1+ i)))
    found))

(defun ngit--g-insert-into-new-columns (c idx)
  (let ((i (ngit--g-find-new-column c)) (mapping-idx nil))
    (when (< i 0)
      (setq i ngit--g-num-new-columns
            ngit--g-num-new-columns (1+ ngit--g-num-new-columns))
      (aset ngit--g-new-columns i c))
    (cond
     ((and (> ngit--g-num-parents 1) (> idx -1) (= ngit--g-merge-layout -1))
      (let* ((dist (- idx i))
             (shift (if (> dist 1) (- (* 2 dist) 3) 1)))
        (setq ngit--g-merge-layout (if (> dist 0) 0 1))
        (setq ngit--g-edges-added (+ ngit--g-num-parents ngit--g-merge-layout -2))
        (setq mapping-idx (+ ngit--g-width (* (1- ngit--g-merge-layout) shift)))
        (setq ngit--g-width (+ ngit--g-width (* 2 ngit--g-merge-layout)))))
     ((and (> ngit--g-edges-added 0) (= i (aref ngit--g-mapping (- ngit--g-width 2))))
      (setq mapping-idx (- ngit--g-width 2) ngit--g-edges-added -1))
     (t (setq mapping-idx ngit--g-width ngit--g-width (+ ngit--g-width 2))))
    (aset ngit--g-mapping mapping-idx i)))

(defun ngit--g-update-columns ()
  (let ((tmp ngit--g-columns))
    (setq ngit--g-columns ngit--g-new-columns ngit--g-new-columns tmp))
  (setq ngit--g-num-columns ngit--g-num-new-columns ngit--g-num-new-columns 0)
  (let ((max-new (+ ngit--g-num-columns ngit--g-num-parents)))
    (ngit--g-ensure-capacity max-new)
    (setq ngit--g-mapping-size (* 2 max-new)))
  (dotimes (i ngit--g-mapping-size) (aset ngit--g-mapping i -1))
  (setq ngit--g-width 0
        ngit--g-prev-edges-added ngit--g-edges-added
        ngit--g-edges-added 0)
  (let ((seen-this nil) (in-columns t) (i 0) (stop nil))
    (while (and (not stop) (<= i ngit--g-num-columns))
      (let ((col-commit nil))
        (if (= i ngit--g-num-columns)
            (if seen-this
                (setq stop t)
              (setq in-columns nil col-commit ngit--g-commit))
          (setq col-commit (aref ngit--g-columns i)))
        (unless stop
          (if (eq col-commit ngit--g-commit)
              (progn
                (setq seen-this t ngit--g-commit-index i ngit--g-merge-layout -1)
                (let ((parent (ngit--g-first-interesting)))
                  (while parent
                    (ngit--g-insert-into-new-columns (car parent) i)
                    (setq parent (ngit--g-next-interesting parent))))
                (when (= ngit--g-num-parents 0)
                  (setq ngit--g-width (+ ngit--g-width 2))))
            (ngit--g-insert-into-new-columns col-commit -1))))
      (setq i (1+ i)))
    (ignore in-columns))
  (while (and (> ngit--g-mapping-size 1)
              (< (aref ngit--g-mapping (1- ngit--g-mapping-size)) 0))
    (setq ngit--g-mapping-size (1- ngit--g-mapping-size))))

(defun ngit--g-num-dashed-parents () (+ ngit--g-num-parents ngit--g-merge-layout -3))

(defun ngit--g-needs-pre-commit-line ()
  (and (>= ngit--g-num-parents 3)
       (< ngit--g-commit-index (1- ngit--g-num-columns))
       (< ngit--g-expansion-row (* 2 (ngit--g-num-dashed-parents)))))

(defun ngit-graph-update (c)
  (setq ngit--g-commit c ngit--g-num-parents 0)
  (let ((parent (ngit--g-first-interesting)))
    (while parent
      (setq ngit--g-num-parents (1+ ngit--g-num-parents))
      (setq parent (ngit--g-next-interesting parent))))
  (setq ngit--g-prev-commit-index ngit--g-commit-index)
  (ngit--g-update-columns)
  (setq ngit--g-expansion-row 0)
  (setq ngit--g-state (cond ((not (eq ngit--g-state 'padding)) 'skip)
                            ((ngit--g-needs-pre-commit-line) 'pre-commit)
                            (t 'commit))))

(defun ngit--g-mapping-correct-p ()
  (let ((ok t) (i 0))
    (while (and ok (< i ngit--g-mapping-size))
      (let ((target (aref ngit--g-mapping i)))
        (unless (or (< target 0) (= target (/ i 2))) (setq ok nil)))
      (setq i (1+ i)))
    ok))

(defun ngit--g-pad-horizontally ()
  (when (< (cdr ngit--gl) ngit--g-width)
    (ngit--gl-addchars ?\s (- ngit--g-width (cdr ngit--gl)))))

(defun ngit--g-output-padding-line ()
  (dotimes (_ ngit--g-num-new-columns)
    (ngit--gl-addch ?|)
    (ngit--gl-addch ?\s)))

(defun ngit--g-output-skip-line ()
  (ngit--gl-addstr "...")
  (ngit--g-update-state (if (ngit--g-needs-pre-commit-line) 'pre-commit 'commit)))

(defun ngit--g-output-pre-commit-line ()
  (let ((seen-this nil))
    (dotimes (i ngit--g-num-columns)
      (let ((col (aref ngit--g-columns i)))
        (cond
         ((eq col ngit--g-commit)
          (setq seen-this t)
          (ngit--gl-addch ?|)
          (ngit--gl-addchars ?\s ngit--g-expansion-row))
         ((and seen-this (= ngit--g-expansion-row 0))
          (ngit--gl-addch (if (and (eq ngit--g-prev-state 'post-merge)
                                   (< ngit--g-prev-commit-index i))
                              ?\\ ?|)))
         ((and seen-this (> ngit--g-expansion-row 0)) (ngit--gl-addch ?\\))
         (t (ngit--gl-addch ?|)))
        (ngit--gl-addch ?\s))))
  (setq ngit--g-expansion-row (1+ ngit--g-expansion-row))
  (unless (ngit--g-needs-pre-commit-line) (ngit--g-update-state 'commit)))

(defun ngit--g-output-commit-char ()
  (ngit--gl-addstr (if (ngit--has ngit--g-commit ngit-boundary) "o" "*")))

(defun ngit--g-draw-octopus-merge ()
  (let ((dashed (ngit--g-num-dashed-parents)))
    (dotimes (i dashed)
      (ngit--gl-addch ?-)
      (ngit--gl-addch (if (= i (1- dashed)) ?. ?-)))))

(defun ngit--g-output-commit-line ()
  (let ((seen-this nil) (i 0) (stop nil))
    (while (and (not stop) (<= i ngit--g-num-columns))
      (let ((col-commit nil))
        (if (= i ngit--g-num-columns)
            (if seen-this (setq stop t) (setq col-commit ngit--g-commit))
          (setq col-commit (aref ngit--g-columns i)))
        (unless stop
          (cond
           ((eq col-commit ngit--g-commit)
            (setq seen-this t)
            (ngit--g-output-commit-char)
            (when (> ngit--g-num-parents 2) (ngit--g-draw-octopus-merge)))
           ((and seen-this (> ngit--g-edges-added 1)) (ngit--gl-addch ?\\))
           ((and seen-this (= ngit--g-edges-added 1))
            (ngit--gl-addch (if (and (eq ngit--g-prev-state 'post-merge)
                                     (> ngit--g-prev-edges-added 0)
                                     (< ngit--g-prev-commit-index i))
                                ?\\ ?|)))
           ((and (eq ngit--g-prev-state 'collapsing)
                 (= (aref ngit--g-old-mapping (1+ (* 2 i))) i)
                 (< (aref ngit--g-mapping (* 2 i)) i))
            (ngit--gl-addch ?/))
           (t (ngit--gl-addch ?|)))
          (ngit--gl-addch ?\s)))
      (setq i (1+ i))))
  (ngit--g-update-state (cond ((> ngit--g-num-parents 1) 'post-merge)
                              ((ngit--g-mapping-correct-p) 'padding)
                              (t 'collapsing))))

(defconst ngit--g-merge-chars [?/ ?| ?\\])

(defun ngit--g-output-post-merge-line ()
  (let* ((seen-this nil) (first-parent (ngit--g-first-interesting))
         (parent-col nil) (i 0) (stop nil))
    (while (and (not stop) (<= i ngit--g-num-columns))
      (let ((col-commit nil))
        (if (= i ngit--g-num-columns)
            (if seen-this (setq stop t) (setq col-commit ngit--g-commit))
          (setq col-commit (aref ngit--g-columns i)))
        (unless stop
          (cond
           ((eq col-commit ngit--g-commit)
            (let ((parents first-parent) (idx ngit--g-merge-layout))
              (setq seen-this t)
              (dotimes (j ngit--g-num-parents)
                (ngit--gl-addch (aref ngit--g-merge-chars idx))
                (if (= idx 2)
                    (when (or (> ngit--g-edges-added 0) (< j (1- ngit--g-num-parents)))
                      (ngit--gl-addch ?\s))
                  (setq idx (1+ idx)))
                (setq parents (ngit--g-next-interesting parents)))
              (when (= ngit--g-edges-added 0) (ngit--gl-addch ?\s))))
           (seen-this
            (ngit--gl-addch (if (> ngit--g-edges-added 0) ?\\ ?|))
            (ngit--gl-addch ?\s))
           (t
            (ngit--gl-addch ?|)
            (when (or (/= ngit--g-merge-layout 0) (/= i (1- ngit--g-commit-index)))
              (ngit--gl-addch (if parent-col ?_ ?\s)))))
          (when (eq col-commit (car first-parent)) (setq parent-col t))))
      (setq i (1+ i))))
  (ngit--g-update-state (if (ngit--g-mapping-correct-p) 'padding 'collapsing)))

(defun ngit--g-output-collapsing-line ()
  (let ((used-horizontal nil) (horizontal-edge -1) (horizontal-edge-target -1))
    (let ((tmp ngit--g-mapping))
      (setq ngit--g-mapping ngit--g-old-mapping ngit--g-old-mapping tmp))
    (dotimes (i ngit--g-mapping-size) (aset ngit--g-mapping i -1))
    (dotimes (i ngit--g-mapping-size)
      (let ((target (aref ngit--g-old-mapping i)))
        (when (>= target 0)
          (cond
           ((= (* 2 target) i) (aset ngit--g-mapping i target))
           ((< (aref ngit--g-mapping (1- i)) 0)
            (aset ngit--g-mapping (1- i) target)
            (when (= horizontal-edge -1)
              (setq horizontal-edge i horizontal-edge-target target)
              (let ((j (+ (* 2 target) 3)))
                (while (< j (- i 2)) (aset ngit--g-mapping j target) (setq j (+ j 2))))))
           ((= (aref ngit--g-mapping (1- i)) target) nil)
           (t
            (aset ngit--g-mapping (- i 2) target)
            (when (= horizontal-edge -1)
              (setq horizontal-edge-target target horizontal-edge (1- i))
              (let ((j (+ (* 2 target) 3)))
                (while (< j (- i 2)) (aset ngit--g-mapping j target) (setq j (+ j 2))))))))))
    (dotimes (i ngit--g-mapping-size)
      (aset ngit--g-old-mapping i (aref ngit--g-mapping i)))
    (when (< (aref ngit--g-mapping (1- ngit--g-mapping-size)) 0)
      (setq ngit--g-mapping-size (1- ngit--g-mapping-size)))
    (dotimes (i ngit--g-mapping-size)
      (let ((target (aref ngit--g-mapping i)))
        (cond
         ((< target 0) (ngit--gl-addch ?\s))
         ((= (* 2 target) i) (ngit--gl-addch ?|))
         ((and (= target horizontal-edge-target) (/= i (1- horizontal-edge)))
          (unless (= i (+ (* 2 target) 3)) (aset ngit--g-mapping i -1))
          (setq used-horizontal t)
          (ngit--gl-addch ?_))
         (t
          (when (and used-horizontal (< i horizontal-edge)) (aset ngit--g-mapping i -1))
          (ngit--gl-addch ?/)))))
    (when (ngit--g-mapping-correct-p) (ngit--g-update-state 'padding))))

(defun ngit--g-next-line ()
  "Build the next graph line; return (STRING . SHOWN-COMMIT-LINE)."
  (let ((ngit--gl (cons nil 0)) (shown nil))
    (pcase ngit--g-state
      ('padding (ngit--g-output-padding-line))
      ('skip (ngit--g-output-skip-line))
      ('pre-commit (ngit--g-output-pre-commit-line))
      ('commit (ngit--g-output-commit-line) (setq shown t))
      ('post-merge (ngit--g-output-post-merge-line))
      ('collapsing (ngit--g-output-collapsing-line)))
    (ngit--g-pad-horizontally)
    (cons (apply #'concat (nreverse (car ngit--gl))) shown)))

(defun ngit--g-padding-line ()
  (if (not (eq ngit--g-state 'commit))
      (car (ngit--g-next-line))
    (let ((ngit--gl (cons nil 0)))
      (dotimes (i ngit--g-num-columns)
        (ngit--gl-addch ?|)
        (if (and (eq (aref ngit--g-columns i) ngit--g-commit) (> ngit--g-num-parents 2))
            (ngit--gl-addchars ?\s (* 2 (- ngit--g-num-parents 2)))
          (ngit--gl-addch ?\s)))
      (ngit--g-pad-horizontally)
      (setq ngit--g-prev-state 'padding)
      (apply #'concat (nreverse (car ngit--gl))))))

(defun ngit--g-finished-p () (eq ngit--g-state 'padding))

(defun ngit-graph-show-commit ()
  (when ngit--graph
    (let ((shown nil))
      (when (ngit--g-finished-p)
        (ngit-graph-show-padding)
        (setq shown t))
      (while (and (not shown) (not (ngit--g-finished-p)))
        (let ((r (ngit--g-next-line)))
          (setq shown (cdr r))
          (ngit--emit (car r))
          (unless shown (ngit--emit "\n")))))))

(defun ngit-graph-show-oneline ()
  (when ngit--graph (ngit--emit (car (ngit--g-next-line)))))

(defun ngit-graph-show-padding ()
  (when ngit--graph (ngit--emit (ngit--g-padding-line))))

(defun ngit-graph-show-remainder ()
  (when (and ngit--graph (not (ngit--g-finished-p)))
    (let ((done nil))
      (while (not done)
        (ngit--emit (car (ngit--g-next-line)))
        (if (ngit--g-finished-p) (setq done t) (ngit--emit "\n"))))
    t))

(defun ngit-graph-show-commit-msg (msg)
  "graph_show_commit_msg: MSG with the graph before each continuation line."
  (let ((pos 0) (n (length msg)))
    (while (< pos n)
      (let* ((nl (ngit--byte-pos msg ?\n pos))
             (end (if nl (1+ nl) n)))
        (ngit--emit (substring msg pos end))
        (when (and nl (< end n)) (ngit-graph-show-oneline))
        (setq pos end))))
  (when (and ngit--graph (not (ngit--g-finished-p)))
    (let ((terminated (and (> (length msg) 0) (= (aref msg (1- (length msg))) ?\n))))
      (unless terminated (ngit--emit "\n"))
      (ngit-graph-show-remainder)
      (when terminated (ngit--emit "\n")))))

(defun ngit--graph-prefix-lines (text)
  "Emit TEXT (diff output) with the graph padding before each line."
  (let ((pos 0) (n (length text)))
    (while (< pos n)
      (let* ((nl (ngit--byte-pos text ?\n pos))
             (end (if nl (1+ nl) n)))
        (ngit-graph-show-padding)
        (ngit--emit (substring text pos end))
        (setq pos end)))))

;;;; Diff output for log (log-tree.c, diff.c diff_flush)

(defvar ngit--ld-formats nil "Diff output formats requested for log.")
(defvar ngit--ld-context 3)
(defvar ngit--ld-renames t)
(defvar ngit--ld-full-diff nil)
(defvar ngit--ld-show-root t)
(defvar ngit--log-show-state nil
  "Show's shared [SHOWN-ONE NO-WALK] state, or nil for log.")

(defun ngit--log-changes (repo c)
  "The changes log shows for commit C, or nil when it shows no diff."
  (let* ((parents (ngit--c-parents c))
         (base (cond ((null parents) (if ngit--ld-show-root 'root 'none))
                     ((and (cdr parents) (not ngit--rv-first-parent)) 'none)
                     (t (car parents)))))
    (unless (eq base 'none)
      (unless (eq base 'root) (ngit--parse base))
      (let ((changes (ngit-diff-trees repo (and (not (eq base 'root)) (ngit--tree-bin base))
                                      (ngit--tree-bin c))))
        (when (and ngit--rv-paths (not ngit--ld-full-diff))
          (setq changes (seq-filter (lambda (ch) (eq (ngit--log-path-class (aref ch 0) nil
                                                                  (or (= (aref ch 2) #o160000)
                                                                      (= (aref ch 4) #o160000))) 'full))
                                    changes)))
        (when ngit--ld-renames
          (setq changes (ngit-diffcore-rename repo changes)))
        changes))))

(defun ngit--decimal-width (n)
  (length (number-to-string n)))

(defun ngit--char-width (ch)
  "Columns of character CH, after git's utf8_width (wcwidth) for the common
cases: combining marks are zero and East Asian wide characters two."
  (cond ((< ch #x300) 1)
        ((<= ch #x36f) 0)
        ((< ch #x1100) 1)
        ((or (<= ch #x115f)
             (and (>= ch #x2e80) (<= ch #xa4cf) (/= ch #x303f))
             (and (>= ch #xac00) (<= ch #xd7a3))
             (and (>= ch #xf900) (<= ch #xfaff))
             (and (>= ch #xfe30) (<= ch #xfe4f))
             (and (>= ch #xff00) (<= ch #xff60))
             (and (>= ch #xffe0) (<= ch #xffe6))
             (and (>= ch #x1f300) (<= ch #x1f64f))
             (and (>= ch #x20000) (<= ch #x3fffd)))
         2)
        (t 1)))

(defun ngit--display-width (s)
  "Display columns of the UTF-8 byte string S."
  (let ((w 0) (i 0) (n (length s)) (ascii t))
    (while (and ascii (< i n))
      (when (>= (aref s i) 128) (setq ascii nil))
      (setq i (1+ i)))
    (if ascii
        n
      (dolist (ch (string-to-list (decode-coding-string s 'utf-8)))
        (setq w (+ w (ngit--char-width ch))))
      w)))

(defun ngit--stat-print-name (c)
  (if (= (aref c 1) ?R)
      (ngit--pprint-rename (aref c 8) (aref c 0))
    (ngit-quote-path (aref c 0))))

(defun ngit--stat-entries (repo changes)
  "diffstat entries [CHANGE ADDED DELETED BINARY] as compute_diffstat."
  (let ((out nil))
    (dolist (c changes)
      (let* ((path (aref c 0)) (old-path (or (aref c 8) path))
             (old-bin (aref c 3)) (new-bin (aref c 5))
             (may-differ (not (and old-bin new-bin (equal old-bin new-bin))))
             (old (ngit--diff-blob repo (aref c 2) old-bin))
             (new (ngit--diff-blob repo (aref c 4) new-bin))
             (binary (or (and old-bin (/= (aref c 2) ngit-mode-gitlink)
                              (ngit--binary-p repo old-path old))
                         (and new-bin (/= (aref c 4) ngit-mode-gitlink)
                              (ngit--binary-p repo path new)))))
        (cond
         (binary
          (push (vector c (if may-differ (length new) 0) (if may-differ (length old) 0) t) out))
         (may-differ
          (let ((r (ngit--count-changed-lines old new)))
            (unless (and (= (aref c 1) ?M) (= (car r) 0) (= (cdr r) 0)
                         (= (aref c 2) (aref c 4)))
              (push (vector c (car r) (cdr r) nil) out))))
         (t (push (vector c 0 0 nil) out)))))
    (nreverse out)))

(defun ngit--stat-summary-line (files adds dels)
  (concat (format (if (= files 1) " %d file changed" " %d files changed") files)
          (if (or (> adds 0) (= dels 0))
              (format (if (= adds 1) ", %d insertion(+)" ", %d insertions(+)") adds)
            "")
          (if (or (> dels 0) (= adds 0))
              (format (if (= dels 1) ", %d deletion(-)" ", %d deletions(-)") dels)
            "")
          "\n"))

(defun ngit--scale-linear (it width max-change)
  (if (= it 0) 0 (+ 1 (/ (* it (1- width)) max-change))))

(defun ngit--stat-width ()
  (let* ((env (getenv "COLUMNS"))
         (n (and env (string-to-number env))))
    ;; show_stats subtracts the line prefix (the graph) from the terminal width.
    (- (if (and n (> n 0)) n 80) (if ngit--graph ngit--g-width 0))))

(defun ngit--emit-stat (entries)
  "git's show_stats for ENTRIES."
  (when entries
    (let ((max-change 0) (max-len 0) (bin-width 0) (number-width 0)
          (names (make-hash-table :test 'eq)))
      (dolist (e entries)
        (let ((name (ngit--stat-print-name (aref e 0))))
          (puthash e name names)
          (setq max-len (max max-len (ngit--display-width name)))
          (if (aref e 3)
              (setq bin-width (max bin-width (+ 14 (ngit--decimal-width (aref e 1))
                                                (ngit--decimal-width (aref e 2))))
                    number-width 3)
            (setq max-change (max max-change (+ (aref e 1) (aref e 2)))))))
      (let* ((width (ngit--stat-width))
             (number-width (max number-width (ngit--decimal-width max-change)))
             (width (max width (+ 16 6 number-width)))
             (graph-width (if (> (+ max-change 4) bin-width) max-change (- bin-width 4)))
             (name-width max-len))
        (when (> (+ name-width number-width 6 graph-width) width)
          (when (> graph-width (- (/ (* width 3) 8) number-width 6))
            (setq graph-width (max 6 (- (/ (* width 3) 8) number-width 6))))
          (if (> name-width (- width number-width 6 graph-width))
              (setq name-width (- width number-width 6 graph-width))
            (setq graph-width (- width number-width 6 name-width))))
        (let ((adds 0) (dels 0))
          (dolist (e entries)
            (let* ((name (gethash e names))
                   (len name-width)
                   (prefix "")
                   (added (aref e 1)) (deleted (aref e 2))
                   (name-len (ngit--display-width name)))
              (when (< name-width name-len)
                (setq prefix "..." len (- len 3))
                ;; As show_stats: skip NAME-LEN - LEN bytes, then cut at a slash.
                (setq name (substring name (min (length name) (max 0 (- name-len len)))))
                (let ((slash (ngit--byte-pos name ?/ 0)))
                  (when slash (setq name (substring name slash)))))
              (let ((padding (make-string (max 0 (- len (ngit--display-width name))) ?\s)))
                (if (aref e 3)
                    (ngit--emit " " prefix name padding " | "
                                (format (format "%%%ds" number-width) "Bin")
                                (if (and (= added 0) (= deleted 0))
                                    "\n"
                                  (format " %d -> %d bytes\n" deleted added)))
                  (let ((add added) (del deleted))
                    (when (<= graph-width max-change)
                      (let ((total (ngit--scale-linear (+ add del) graph-width max-change)))
                        (when (and (< total 2) (> add 0) (> del 0)) (setq total 2))
                        (if (< add del)
                            (setq add (ngit--scale-linear add graph-width max-change)
                                  del (- total add))
                          (setq del (ngit--scale-linear del graph-width max-change)
                                add (- total del)))))
                    (ngit--emit " " prefix name padding " | "
                                (format (format "%%%dd" number-width) (+ added deleted))
                                (if (> (+ added deleted) 0) " " "")
                                (make-string (max 0 add) ?+) (make-string (max 0 del) ?-) "\n"))))
              (unless (aref e 3)
                (setq adds (+ adds added) dels (+ dels deleted)))))
          (ngit--emit (ngit--stat-summary-line (length entries) adds dels)))))))

(defun ngit--emit-summary (changes)
  "diff_summary lines (--summary)."
  (dolist (c changes)
    (let ((status (aref c 1)) (path (aref c 0)) (om (aref c 2)) (nm (aref c 4)))
      (pcase status
        (?A (ngit--emit (format " create mode %06o %s\n" nm (ngit-quote-path path))))
        (?D (ngit--emit (format " delete mode %06o %s\n" om (ngit-quote-path path))))
        (?R (ngit--emit (format " rename %s (%d%%)\n" (ngit--pprint-rename (aref c 8) path)
                                (ngit-similarity-percent c)))
            (unless (= om nm)
              (ngit--emit (format " mode change %06o => %06o\n" om nm))))
        (_ (unless (= om nm)
             (ngit--emit (format " mode change %06o => %06o %s\n" om nm
                                 (ngit-quote-path path)))))))))

(defun ngit--emit-log-diff (repo changes)
  "diff_flush for the requested formats."
  (let ((sep 0) (fmts ngit--ld-formats))
    (when (or (memq 'name-only fmts) (memq 'name-status fmts))
      (dolist (c changes)
        (if (memq 'name-status fmts)
            (ngit--emit (if (= (aref c 1) ?R)
                            (format "R%03d\t%s\t" (ngit-similarity-percent c)
                                    (ngit-quote-path (aref c 8)))
                          (concat (string (aref c 1)) "\t"))
                        (ngit-quote-path (aref c 0)) "\n")
          (ngit--emit (ngit-quote-path (aref c 0)) "\n")))
      (setq sep (1+ sep)))
    (when (or (memq 'stat fmts) (memq 'shortstat fmts) (memq 'numstat fmts))
      (let ((entries (ngit--stat-entries repo changes)))
        (when (memq 'numstat fmts)
          (dolist (e entries)
            (ngit--emit (if (aref e 3) "-\t-\t" (format "%d\t%d\t" (aref e 1) (aref e 2)))
                        (ngit--stat-print-name (aref e 0)) "\n")))
        (when (memq 'stat fmts) (ngit--emit-stat entries))
        (when (and (memq 'shortstat fmts) entries)
          (let ((adds 0) (dels 0))
            (dolist (e entries)
              (unless (aref e 3) (setq adds (+ adds (aref e 1)) dels (+ dels (aref e 2)))))
            (ngit--emit (ngit--stat-summary-line (length entries) adds dels)))))
      (setq sep (1+ sep)))
    (when (and (memq 'summary fmts)
               (seq-find (lambda (c) (or (memq (aref c 1) '(?A ?D ?R)) (/= (aref c 2) (aref c 4))))
                         changes))
      (ngit--emit-summary changes)
      (setq sep (1+ sep)))
    (when (memq 'patch fmts)
      (when (> sep 0) (ngit--emit "\n"))
      (dolist (c changes) (ngit--emit-patch repo c ngit--ld-context)))))

;;;; log

(defun ngit--log-opt-value (a name args)
  "Value of option NAME given as A (\"NAME=V\" or \"NAME\" V); return
(VALUE . REST-ARGS) or nil."
  (cond ((string-prefix-p (concat name "=") a)
         (cons (substring a (1+ (length name))) args))
        ((string= a name)
         (unless args (ngit--die "fatal: option '%s' requires a value" (substring name 2)))
         (cons (car args) (cdr args)))))

(defun ngit--parse-approxidate (s)
  "Parse a --since/--until value: \"@STAMP\", \"STAMP [TZ]\" or ISO 8601 with a time."
  (let ((parsed (ngit--parse-date (if (string-prefix-p "@" s) (substring s 1) s))))
    (string-to-number (car (split-string parsed " ")))))

(defun ngit--log-pathspec (repo paths)
  "Pathspec items for PATHS given relative to the current directory."
  (mapcar (lambda (p)
            ;; Keep log's glob compiler; only normalize the path prefix.
            ;; The shared API also retains the invocation cwd after -C.
            (let ((item (car (ngit-paths-items
                              repo (list (if (string-prefix-p ":/" p)
                                             (concat (file-name-as-directory (ngit-repo-toplevel repo))
                                                     (substring p 2))
                                           p))))))
              ;; Directory literals also select gitlinks, but never regular
              ;; files. Keep glob directory matching on the existing matcher.
              (if (string-suffix-p "/" item)
                  (let ((compiled (ngit--ps-item item)))
                    (if (cdr compiled) (ngit--ps-item (concat item "*"))
                      (cons (car compiled) 'directory)))
                (ngit--ps-item item))))
          paths))

(defun ngit-cmd-log (repo args)
  (let ((pending nil) (flags 0) (paths nil) (seen-rev nil)
        (no-output (or (member "-q" args) (member "--quiet" args)))
        (fmt (let ((cfg (ngit-config-get repo "format.pretty"))) (if cfg (list 'spec cfg) nil)))
        (abbrev-commit (ngit-config-bool repo "log.abbrevCommit" nil))
        (abbrev-given nil)
        (date-mode (let ((cfg (ngit-config-get repo "log.date")))
                     (if cfg (ngit-parse-date-mode cfg) '(normal))))
        (date-given nil)
        (decorate (let ((cfg (ngit-config-get repo "log.decorate")))
                    (cond ((null cfg) nil)
                          ((member cfg '("full")) 'full)
                          ((member cfg '("short" "true" "yes" "on" "1")) 'short)
                          (t nil))))
        (decorate-given nil)
        (grep nil) (grep-mode 'basic)
        (print-parents nil) (graph nil))
    (setq ngit--rv-repo repo
          ngit--rv-no-walk (and ngit--log-show-state (aref ngit--log-show-state 1))
          ngit--rv-commits nil ngit--rv-limited nil ngit--rv-first-parent nil
          ngit--rv-paths nil ngit--rv-dense t ngit--rv-simplify-history t
          ngit--rv-rewrite-parents nil ngit--rv-topo nil ngit--rv-max-age nil
          ngit--rv-min-age nil ngit--rv-min-parents 0 ngit--rv-max-parents -1
          ngit--rv-grep nil ngit--rv-max-count -1 ngit--rv-skip 0 ngit--rv-reverse nil
          ngit--decorations nil ngit--decorate-full nil
          ngit--ld-formats nil ngit--ld-context 3
          ngit--ld-renames (let ((v (ngit-config-get repo "diff.renames")))
                             (or (null v) (ngit-config-bool repo "diff.renames" t)
                                 (member v '("copies" "copy"))))
          ngit--ld-full-diff nil
          ngit--ld-show-root (ngit-config-bool repo "log.showRoot" t))
    (while args
      (let* ((a (pop args)) (v nil))
        (cond
         ((string= a "--") (setq paths (append paths args) args nil))
         ((and (string-match "\\`-n?\\([0-9]+\\)\\'" a))
          (setq ngit--rv-max-count (string-to-number (match-string 1 a))))
         ((setq v (or (ngit--log-opt-value a "--max-count" args)
                      (and (string= a "-n") (ngit--log-opt-value a "-n" args))))
          (setq ngit--rv-max-count (string-to-number (car v)) args (cdr v)))
         ((setq v (ngit--log-opt-value a "--skip" args))
          (setq ngit--rv-skip (string-to-number (car v)) args (cdr v)))
         ((string= a "--reverse") (setq ngit--rv-reverse t))
         ((string= a "--oneline") (setq fmt '(oneline) abbrev-commit t))
         ((member a '("--pretty" "--format")) (setq fmt '(medium)))
         ((or (string-prefix-p "--pretty=" a) (string-prefix-p "--format=" a))
          (setq fmt (list 'spec (substring a (1+ (string-search "=" a))))))
         ((string= a "--abbrev-commit") (setq abbrev-commit t abbrev-given t))
         ((string= a "--no-abbrev-commit") (setq abbrev-commit nil abbrev-given t))
         ((string-prefix-p "--abbrev=" a) (setq ngit--pp-abbrev (string-to-number (substring a 9))))
         ((string= a "--no-abbrev") (setq ngit--merge-abbrev 'full))
         ((setq v (ngit--log-opt-value a "--date" args))
          (setq date-mode (ngit-parse-date-mode (car v)) date-given t args (cdr v)))
         ((string= a "--decorate") (setq decorate 'short decorate-given t))
         ((string-prefix-p "--decorate=" a)
          (let ((d (substring a 11)))
            (setq decorate-given t
                  decorate (cond ((string= d "full") 'full)
                                 ((member d '("short" "true" "yes" "on" "1")) 'short)
                                 ((member d '("no" "false" "off" "0" "auto")) nil)
                                 (t (ngit--die "fatal: invalid --decorate option: %s" d))))))
         ((string= a "--no-decorate") (setq decorate nil decorate-given t))
         ((string= a "--all")
          (setq seen-rev t)
          (setq pending (ngit--add-refs repo "refs/" nil flags pending))
          (let ((head (ngit-resolve-ref repo "HEAD")))
            (when head
              (let ((c (ngit--rev-commit repo head)))
                (when c (push (cons c flags) pending))))))
         ((or (string= a "--branches") (string-prefix-p "--branches=" a))
          (setq seen-rev t)
          (setq pending (ngit--add-refs repo "refs/heads/" (and (> (length a) 10) (substring a 11))
                                        flags pending)))
         ((or (string= a "--tags") (string-prefix-p "--tags=" a))
          (setq seen-rev t)
          (setq pending (ngit--add-refs repo "refs/tags/" (and (> (length a) 6) (substring a 7))
                                        flags pending)))
         ((or (string= a "--remotes") (string-prefix-p "--remotes=" a))
          (setq seen-rev t)
          (setq pending (ngit--add-refs repo "refs/remotes/" (and (> (length a) 9) (substring a 10))
                                        flags pending)))
         ((string= a "--not") (setq flags (logxor flags (logior ngit-uninteresting ngit-bottom))))
         ((string= a "--first-parent") (setq ngit--rv-first-parent t))
         ((string= a "--merges") (setq ngit--rv-min-parents 2))
         ((string= a "--no-merges") (setq ngit--rv-max-parents 1))
         ((string-prefix-p "--min-parents=" a)
          (setq ngit--rv-min-parents (string-to-number (substring a 14))))
         ((string-prefix-p "--max-parents=" a)
          (setq ngit--rv-max-parents (string-to-number (substring a 14))))
         ((string= a "--no-min-parents") (setq ngit--rv-min-parents 0))
         ((string= a "--no-max-parents") (setq ngit--rv-max-parents -1))
         ((setq v (or (ngit--log-opt-value a "--since" args) (ngit--log-opt-value a "--after" args)))
          (setq ngit--rv-max-age (ngit--parse-approxidate (car v)) args (cdr v)))
         ((setq v (or (ngit--log-opt-value a "--until" args) (ngit--log-opt-value a "--before" args)))
          (setq ngit--rv-min-age (ngit--parse-approxidate (car v)) args (cdr v)))
         ((string-prefix-p "--max-age=" a) (setq ngit--rv-max-age (string-to-number (substring a 10))))
         ((string-prefix-p "--min-age=" a) (setq ngit--rv-min-age (string-to-number (substring a 10))))
         ((setq v (ngit--log-opt-value a "--author" args))
          (setq grep (plist-put grep :author (append (plist-get grep :author) (list (car v))))
                args (cdr v)))
         ((setq v (ngit--log-opt-value a "--committer" args))
          (setq grep (plist-put grep :committer (append (plist-get grep :committer) (list (car v))))
                args (cdr v)))
         ((setq v (ngit--log-opt-value a "--grep" args))
          (setq grep (plist-put grep :grep (append (plist-get grep :grep) (list (car v))))
                args (cdr v)))
         ((string= a "--all-match") (setq grep (plist-put grep :all-match t)))
         ((string= a "--invert-grep") (setq grep (plist-put grep :invert t)))
         ((member a '("-i" "--regexp-ignore-case")) (setq grep (plist-put grep :icase t)))
         ((member a '("-E" "--extended-regexp")) (setq grep-mode 'extended))
         ((member a '("-F" "--fixed-strings")) (setq grep-mode 'fixed))
         ((member a '("-G" "--basic-regexp")) (setq grep-mode 'basic))
         ((string= a "--topo-order") (setq ngit--rv-topo 'graph))
         ((string= a "--date-order") (setq ngit--rv-topo 'date))
         ((string= a "--full-history") (setq ngit--rv-simplify-history nil))
         ((string= a "--dense") (setq ngit--rv-dense t))
         ((string= a "--sparse") (setq ngit--rv-dense nil))
         ((string= a "--parents") (setq ngit--rv-rewrite-parents t print-parents t))
         ((string= a "--graph") (setq graph t))
         ((string= a "--no-graph") (setq graph nil))
         ((member a '("-p" "-u" "--patch")) (push 'patch ngit--ld-formats) (setq no-output nil))
         ((member a '("-s" "--no-patch")) (setq ngit--ld-formats nil no-output t))
         ((member a '("-q" "--quiet")))
         ((string= a "--stat") (push 'stat ngit--ld-formats))
         ((string= a "--shortstat") (push 'shortstat ngit--ld-formats))
         ((string= a "--numstat") (push 'numstat ngit--ld-formats))
         ((string= a "--name-only") (push 'name-only ngit--ld-formats))
         ((string= a "--name-status") (push 'name-status ngit--ld-formats))
         ((string= a "--summary") (push 'summary ngit--ld-formats))
         ((string-match "\\`\\(?:-U\\|--unified=\\)\\([0-9]+\\)\\'" a)
          (setq ngit--ld-context (string-to-number (match-string 1 a)))
          (push 'patch ngit--ld-formats) (setq no-output nil))
         ((string= a "--no-renames") (setq ngit--ld-renames nil))
         ((member a '("-M" "--find-renames")) (setq ngit--ld-renames t))
         ((string= a "--full-diff") (setq ngit--ld-full-diff t))
         ((string= a "--root") (setq ngit--ld-show-root t))
         ((string-prefix-p "-" a) (ngit--die "fatal: unrecognized argument: %s" a))
         (t
          (let ((r (ngit--handle-revision repo a flags pending)))
            (cond ((not (eq r 'not-a-rev)) (setq pending r seen-rev t))
                  ((file-exists-p (expand-file-name a (or ngit--invocation-directory default-directory)))
                   (setq paths (append paths (list a)))
                   ;; Everything after the first path is a path too.
                   (dolist (rest args)
                     (when (string= rest "--") (ngit--die "fatal: bad revision '%s'" rest))
                     (unless (file-exists-p (expand-file-name rest (or ngit--invocation-directory default-directory)))
                       (ngit--die "fatal: %s: no such path in the working tree.\nUse 'git <command> -- <path>...' to specify paths that do not exist locally." rest)))
                   (setq paths (append paths (delete "--" args)) args nil))
                  (t (ngit--bad-revision a))))))))
    (when (or (memq 'name-only ngit--ld-formats) (memq 'name-status ngit--ld-formats))
      (when (or no-output (and (memq 'name-only ngit--ld-formats) (memq 'name-status ngit--ld-formats)))
        (ngit--die "fatal: options '--name-only', '--name-status', '--check', and '-s' cannot be used together"))
      (setq ngit--ld-formats (list (if (memq 'name-only ngit--ld-formats) 'name-only 'name-status))))
    (setq ngit--graph nil)
    (when graph
      (when ngit--rv-reverse
        (ngit--die "fatal: options '--graph' and '--reverse' cannot be used together"))
      (ngit-graph-init)
      (unless ngit--rv-topo (setq ngit--rv-topo 'graph))
      (setq ngit--rv-rewrite-parents t))
    (when grep
      (setq ngit--rv-grep (plist-put grep :mode grep-mode)))
    (when paths
      (setq ngit--rv-paths (ngit--log-pathspec repo paths)))
    ;; Default revision, unless a revision option was given (even one that
    ;; named no commits).
    (unless (or pending seen-rev)
      (let ((head (ngit-resolve-ref repo "HEAD")))
        (if head
            (push (cons (ngit-commit head) 0) pending)
          (let ((raw (ngit-read-ref repo "HEAD")))
            (if (and raw (string-prefix-p "ref: refs/heads/" raw))
                (ngit--die "fatal: your current branch '%s' does not have any commits yet"
                           (substring raw 16))
              (ngit--bad-revision "HEAD"))))))
    ;; Format.
    (let* ((spec (cadr fmt))
           (kind (car fmt))
           (pretty nil) (user nil) (terminator nil))
      (cond
       ((null kind) (setq pretty 'medium))
       ((eq kind 'oneline) (setq pretty 'oneline terminator t))
       ((eq kind 'medium) (setq pretty 'medium))
       ((string-prefix-p "format:" spec) (setq user (substring spec 7)))
       ((or (string= spec "") (string-prefix-p "tformat:" spec) (string-search "%" spec))
        (setq user (if (string-prefix-p "tformat:" spec) (substring spec 8) spec) terminator t))
       (t
        (let ((found nil))
          (dolist (name '("raw" "medium" "short" "email" "mboxrd" "fuller" "full" "oneline" "reference"))
            ;; git picks the shortest name the value is a prefix of.
            (when (and (string-prefix-p (downcase spec) name)
                       (or (null found) (< (length name) (length found))))
              (setq found name)))
          (pcase found
            ((or "raw" "medium" "short" "fuller" "full") (setq pretty (intern found)))
            ("oneline" (setq pretty 'oneline terminator t))
            ("reference" (setq user "%C(auto)%h (%s, %ad)" terminator t)
             (unless date-given (setq date-mode '(short))))
            (_ (ngit--die "fatal: invalid --pretty format: %s" spec))))))
      (when (and (eq pretty 'raw) (not decorate-given)) (setq decorate nil))
      (when (and (eq pretty 'raw) (not abbrev-given) (not (eq kind 'oneline))) (setq abbrev-commit nil))
      (when user
        (if (or (string-search "%d" user) (string-search "%D" user)
                (string-search "%+d" user) (string-search "%-d" user) (string-search "% d" user)
                (string-search "%+D" user) (string-search "%-D" user) (string-search "% D" user))
            (unless decorate (setq decorate 'short))
          (setq decorate nil)))
      (when decorate
        (setq ngit--decorate-full (eq decorate 'full))
        (ngit--load-decorations repo))
      (setq ngit--pp-abbrev-format ngit--pp-abbrev)
      (let ((commit-abbrev (and abbrev-commit (or ngit--pp-abbrev (ngit-default-abbrev repo)))))
        (ngit-walk-prepare repo (nreverse pending))
        (let ((commits nil) (c nil)
              (shown-one (and ngit--log-show-state (aref ngit--log-show-state 0)))
              (missing-newline nil))
          (if ngit--rv-reverse
              (progn
                (while (setq c (ngit-walk-next)) (push c commits))
                (setq c (pop commits)))
            (setq c (ngit-walk-next)))
          (while c
            (when ngit--graph (ngit-graph-update c))
            (let* ((hex (ngit--c-hex c))
                   (combined (and ngit--log-show-state ngit--ld-formats
                                  (cdr (ngit--c-parents c)) (not ngit--rv-first-parent)))
                   (changes (and ngit--ld-formats (ngit--log-changes repo c))))
              ;; show_log
              (when (and shown-one (not terminator))
                (unless missing-newline (ngit-graph-show-padding))
                (ngit--emit "\n"))
              (setq shown-one t)
              (when ngit--log-show-state (aset ngit--log-show-state 0 t))
              (ngit-graph-show-commit)
              (unless user
                (ngit--emit
                 (if (eq pretty 'oneline) "" "commit ")
                 (if commit-abbrev (ngit-abbrev repo hex commit-abbrev) hex)
                 (if print-parents
                     (mapconcat (lambda (p) (concat " " (if commit-abbrev
                                                             (ngit-abbrev repo (ngit--c-hex p) commit-abbrev)
                                                           (ngit--c-hex p))))
                                (ngit--c-parents c) "")
                   "")
                 (if decorate (ngit-format-decorations repo c " (" ")") ""))
                (if (eq pretty 'oneline)
                    (ngit--emit " ")
                  (ngit--emit "\n")
                  (ngit-graph-show-oneline)))
              (let ((msg (if user (ngit-format-user c user date-mode)
                           (ngit-pretty-builtin c pretty date-mode))))
                (setq missing-newline (or (= (length msg) 0)
                                          (/= (aref msg (1- (length msg))) ?\n)))
                (ngit-graph-show-commit-msg msg)
                (when (and terminator (not (and user (string= user ""))))
                  (unless missing-newline (ngit-graph-show-padding))
                  (ngit--emit "\n")))
              (when (or changes combined)
                ;; log_tree_diff_flush: a separator after a verbose header,
                ;; then the diff, each line behind the graph padding.
                (let ((text (let ((ngit--out nil))
                              (unless (or (and (not combined) (eq pretty 'oneline))
                                          (and user (string= user "")))
                                (ngit--emit (if (and (not combined) (memq 'stat ngit--ld-formats)
                                                     (memq 'patch ngit--ld-formats))
                                                "---" "")
                                            "\n"))
                              (if combined (ngit--show-merge-diff repo c)
                                (ngit--emit-log-diff repo changes))
                              (apply #'concat (nreverse ngit--out)))))
                  (if ngit--graph (ngit--graph-prefix-lines text) (ngit--emit text)))))
            (setq c (if ngit--rv-reverse (pop commits) (ngit-walk-next)))))))
    0))

(provide 'ngit-log)

;;; ngit-log.el ends here
