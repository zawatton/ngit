;;; ngit-rename.el --- rename detection (diffcore-rename) -*- lexical-binding: t; -*-

;;; Commentary:

;; A port of git's diffcore-rename.c and diffcore-delta.c for the default
;; rename detection (no copies, no break pairs): exact renames, then
;; same-basename renames above 75% similarity, then the inexact matrix of
;; the remaining deleted and added files above 50%, limited by
;; diff.renameLimit.  Similarity follows diffcore-delta's span hashes so
;; scores (and the percentages git prints) come out identical.
;;
;; Changes are the vectors of `ngit-change'; a detected rename becomes a
;; change with status ?R, slot 8 the old path and slot 9 the score
;; (0..60000).

;;; Code:

(require 'ngit-changes)

(defconst ngit-max-score 60000)
(defconst ngit--default-rename-score 30000)
(defconst ngit--hashbase 107927)

(defvar ngit--span-cache (make-hash-table :test 'equal)
  "Sorted span-hash vectors per blob (hex name and text flag).")

(defun ngit--hash-chars (data is-text)
  "diffcore-delta's hash_chars: [HASHES COUNTS] sorted by hash value."
  ;; One flat `setq' per byte and no per-byte `let': the interpreter's
  ;; cost is per form, and this loop runs over every byte of the blob.
  (let ((tbl (make-hash-table :test 'eql))
        (keys nil)
        (i 0) (n (length data))
        (a1 0) (a2 0) (cnt 0) (c 0) (o1 0) (h 0) (old nil)
        (cr (and is-text (string-search "\r" data) t)))
    (while (< i n)
      (setq c (aref data i) i (1+ i))
      (unless (and cr (= c 13) (< i n) (= (aref data i) 10))
        (setq o1 a1
              a1 (logand (+ (logxor (logand (ash a1 7) 4294967295) (ash a2 -25)) c) 4294967295)
              a2 (logand (logxor (ash a2 7) (ash o1 -25)) 4294967295)
              cnt (1+ cnt))
        (when (or (= cnt 64) (= c 10))
          (setq h (% (logand (+ a1 (* a2 97)) 4294967295) ngit--hashbase)
                old (gethash h tbl))
          (if old
              (puthash h (+ old cnt) tbl)
            (puthash h cnt tbl)
            (push h keys))
          (setq cnt 0 a1 0 a2 0))))
    (when (> cnt 0)
      (let* ((h (% (logand (+ a1 (* a2 97)) #xffffffff) ngit--hashbase))
             (old (gethash h tbl)))
        (if old (puthash h (+ old cnt) tbl) (puthash h cnt tbl) (push h keys))))
    (let* ((sorted (vconcat (sort keys #'<)))
           (counts (make-vector (length sorted) 0))
           (k 0))
      (while (< k (length sorted))
        (aset counts k (gethash (aref sorted k) tbl))
        (setq k (1+ k)))
      (vector sorted counts))))

(defun ngit--count-copied (src dst)
  "diffcore_count_changes: bytes of SRC's spans found in DST."
  (let* ((sh (aref src 0)) (sc (aref src 1)) (dh (aref dst 0)) (dc (aref dst 1))
         (ns (length sh)) (nd (length dh)) (i 0) (j 0) (copied 0))
    (while (< i ns)
      (let ((h (aref sh i)) (scnt (aref sc i)) (dcnt 0))
        (while (and (< j nd) (< (aref dh j) h)) (setq j (1+ j)))
        (when (and (< j nd) (= (aref dh j) h))
          (setq dcnt (aref dc j) j (1+ j)))
        (setq copied (+ copied (min scnt dcnt))))
      (setq i (1+ i)))
    copied))

;;;; File specs: [PATH MODE BIN DATA SPANS]

(defun ngit--spec (path mode bin) (vector path mode bin nil nil))

(defun ngit--spec-data (repo spec)
  (or (aref spec 3) (aset spec 3 (cdr (ngit-read-object repo (aref spec 2))))))

(defun ngit--spec-spans (repo spec)
  (or (aref spec 4)
      (aset spec 4
            (let* ((data (ngit--spec-data repo spec))
                   (text (not (ngit--binary-p repo (aref spec 0) data)))
                   (key (concat (ngit-hex (aref spec 2)) (if text "t" "b"))))
              (or (gethash key ngit--span-cache)
                  (puthash key (ngit--hash-chars data text) ngit--span-cache))))))

(defun ngit--regular-p (mode) (= (logand mode #o170000) #o100000))

(defun ngit--estimate-similarity (repo src dst minimum)
  (if (not (and (ngit--regular-p (aref src 1)) (ngit--regular-p (aref dst 1))))
      0
    (let* ((ssize (length (ngit--spec-data repo src)))
           (dsize (length (ngit--spec-data repo dst)))
           (max-size (max ssize dsize))
           (delta (- max-size (min ssize dsize))))
      (if (< (* max-size (- ngit-max-score minimum)) (* delta ngit-max-score))
          0
        (if (= dsize 0)
            0
          (/ (* (ngit--count-copied (ngit--spec-spans repo src) (ngit--spec-spans repo dst))
                ngit-max-score)
             max-size))))))

(defun ngit--basename-same (a b)
  "diffcore-rename's basename_same for paths A and B."
  (let ((i (length a)) (j (length b)) (result nil) (done nil))
    (while (and (not done) (> i 0) (> j 0))
      (setq i (1- i) j (1- j))
      (let ((c1 (aref a i)) (c2 (aref b j)))
        (cond ((/= c1 c2) (setq done t result nil))
              ((= c1 ?/) (setq done t result t)))))
    (if done
        (if result 1 0)
      (if (and (or (= i 0) (= (aref a (1- i)) ?/))
               (or (= j 0) (= (aref b (1- j)) ?/)))
          1 0))))

(defun ngit--basename-of (path)
  (let ((i (length path)))
    (while (and (> i 0) (/= (aref path (1- i)) ?/)) (setq i (1- i)))
    (substring path i)))

;;;; Matrix scores: [SCORE NAME-SCORE SRC DST], DST -1 when empty

(defun ngit--score-compare (a b)
  "git's score_compare: negative when A sorts before B."
  (cond ((< (aref a 3) 0) (if (>= (aref b 3) 0) 1 0))
        ((< (aref b 3) 0) -1)
        ((= (aref a 0) (aref b 0)) (- (aref b 1) (aref a 1)))
        (t (- (aref b 0) (aref a 0)))))

(defvar ngit-rename-limit-warning nil
  "Set to the rename limit git would ask for when detection was skipped.")

(defun ngit-diffcore-rename (repo changes &optional limit)
  "Return CHANGES with git's default rename detection applied."
  (let* ((limit (or limit
                    (let ((v (ngit-config-get repo "diff.renameLimit")))
                      (and v (string-to-number v)))
                    1000))
         (srcs nil) (dsts nil))
    ;; rename_src: deleted entries; rename_dst: added entries, in order.
    (dolist (c changes)
      (cond ((= (aref c 1) ?D)
             (push (vector (ngit--spec (aref c 0) (aref c 2) (aref c 3)) c 0) srcs))
            ((= (aref c 1) ?A)
             (push (vector (ngit--spec (aref c 0) (aref c 4) (aref c 5)) c nil nil) dsts))))
    ;; srcs: [SPEC CHANGE USED]; dsts: [SPEC CHANGE RENAMED-FROM SCORE]
    (if (or (null srcs) (null dsts))
        changes
      (let ((src-v (vconcat (nreverse srcs)))
            (dst-v (vconcat (nreverse dsts))))
        (ngit--find-exact-renames src-v dst-v)
        (setq src-v (ngit--unused-sources src-v))
        (let ((min-basename (+ ngit--default-rename-score
                               (/ (- ngit-max-score ngit--default-rename-score) 2))))
          (ngit--find-basename-matches repo src-v dst-v min-basename))
        (setq src-v (ngit--unused-sources src-v))
        (let ((num-dst 0))
          (dotimes (i (length dst-v))
            (unless (aref (aref dst-v i) 2) (setq num-dst (1+ num-dst))))
          (when (and (> num-dst 0) (> (length src-v) 0))
            (if (> (* num-dst (length src-v)) (* limit limit))
                (setq ngit-rename-limit-warning (max num-dst (length src-v)))
              (ngit--find-inexact-renames repo src-v dst-v))))
        (ngit--rename-write-back changes dst-v)))))

(defun ngit--record-rename (src dst score)
  (aset src 2 (1+ (aref src 2)))
  (aset dst 2 src)
  (aset dst 3 score))

(defun ngit--find-exact-renames (src-v dst-v)
  (dotimes (i (length dst-v))
    (let* ((dst (aref dst-v i))
           (target (aref dst 0))
           (best nil) (best-score -1) (j 0) (stop nil))
      (while (and (not stop) (< j (length src-v)))
        (let* ((src (aref src-v j)) (source (aref src 0)))
          (when (and (equal (aref source 2) (aref target 2))
                     (or (and (ngit--regular-p (aref source 1)) (ngit--regular-p (aref target 1)))
                         (= (aref source 1) (aref target 1)))
                     (= (aref src 2) 0))
            (let ((score (+ 1 (ngit--basename-same (aref source 0) (aref target 0)))))
              (when (> score best-score)
                (setq best src best-score score)
                (when (= score 2) (setq stop t))))))
        (setq j (1+ j)))
      (when best (ngit--record-rename best dst ngit-max-score)))))

(defun ngit--unused-sources (src-v)
  (vconcat (seq-filter (lambda (s) (= (aref s 2) 0)) (append src-v nil))))

(defun ngit--find-basename-matches (repo src-v dst-v minimum)
  (let ((sources (make-hash-table :test 'equal))
        (dests (make-hash-table :test 'equal)))
    (dotimes (i (length src-v))
      (let ((base (ngit--basename-of (aref (aref (aref src-v i) 0) 0))))
        (puthash base (if (gethash base sources) -1 i) sources)))
    (dotimes (i (length dst-v))
      (let ((dst (aref dst-v i)))
        (unless (aref dst 2)
          (let ((base (ngit--basename-of (aref (aref dst 0) 0))))
            (puthash base (if (gethash base dests) -1 i) dests)))))
    (dotimes (i (length src-v))
      (let* ((src (aref src-v i))
             (base (ngit--basename-of (aref (aref src 0) 0)))
             (src-index (gethash base sources))
             (dst-index (gethash base dests)))
        (when (and dst-index (/= src-index -1) (/= dst-index -1))
          (let ((dst (aref dst-v dst-index)))
            (unless (aref dst 2)
              (let ((score (ngit--estimate-similarity repo (aref src 0) (aref dst 0) minimum)))
                (when (>= score minimum)
                  (ngit--record-rename src dst score))))))))))

(defun ngit--find-inexact-renames (repo src-v dst-v)
  (let ((mx nil) (minimum ngit--default-rename-score))
    (dotimes (i (length dst-v))
      (let ((dst (aref dst-v i)))
        (unless (aref dst 2)
          (let ((m (vector (vector 0 0 -1 -1) (vector 0 0 -1 -1)
                           (vector 0 0 -1 -1) (vector 0 0 -1 -1))))
            (dotimes (j (length src-v))
              (let* ((src (aref src-v j))
                     (o (vector (ngit--estimate-similarity repo (aref src 0) (aref dst 0) minimum)
                                (ngit--basename-same (aref (aref src 0) 0) (aref (aref dst 0) 0))
                                j i))
                     (worst 0))
                ;; record_if_better
                (dotimes (k 3)
                  (when (> (ngit--score-compare (aref m (1+ k)) (aref m worst)) 0)
                    (setq worst (1+ k))))
                (when (> (ngit--score-compare (aref m worst) o) 0)
                  (aset m worst o))))
            (dotimes (k 4) (push (aref m k) mx))))))
    (setq mx (sort (nreverse mx) (lambda (a b) (< (ngit--score-compare a b) 0))))
    (let ((done nil))
      (while (and mx (not done))
        (let ((s (pop mx)))
          (if (or (< (aref s 3) 0) (< (aref s 0) minimum))
              (setq done t)
            (let ((dst (aref dst-v (aref s 3)))
                  (src (aref src-v (aref s 2))))
              (unless (or (aref dst 2) (> (aref src 2) 0))
                (ngit--record-rename src dst (aref s 0))))))))))

(defun ngit--rename-write-back (changes dst-v)
  (let ((by-change (make-hash-table :test 'eq))
        (used (make-hash-table :test 'eq))
        (out nil))
    (dotimes (i (length dst-v))
      (let ((dst (aref dst-v i)))
        (when (aref dst 2)
          (puthash (aref dst 1) dst by-change)
          (puthash (aref (aref dst 2) 1) t used))))
    (dolist (c changes)
      (cond
       ((gethash c used) nil)
       ((gethash c by-change)
        (let* ((dst (gethash c by-change))
               (src-change (aref (aref dst 2) 1))
               (r (vconcat c (vector nil))))
          (aset r 1 ?R)
          (aset r 2 (aref src-change 2))
          (aset r 3 (aref src-change 3))
          (aset r 8 (aref src-change 0))
          (aset r 9 (aref dst 3))
          (push r out)))
       (t (push c out))))
    (nreverse out)))

(defun ngit-similarity-percent (change)
  "Similarity index git prints for a rename CHANGE."
  (/ (* (or (and (> (length change) 9) (aref change 9)) ngit-max-score) 100) ngit-max-score))

(provide 'ngit-rename)

;;; ngit-rename.el ends here
