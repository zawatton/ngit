;;; ngit-negotiate.el --- default fetch negotiator -*- lexical-binding: t; -*-

;;; Commentary:
;; Git 2.47.3 negotiator/default.c and fetch-pack.c's flush schedule.
;; Commit vectors and the stable commit-date priority queue come from ngit-log.

;;; Code:

(require 'ngit-log)

(defconst ngit-neg--common 4)
(defconst ngit-neg--advertised 8)
(defconst ngit-neg--seen 16)
(defconst ngit-neg--popped 32)
(defconst ngit-neg--complete 64)
(defconst ngit-neg--initial-flush 16)
(defconst ngit-neg--pipesafe-flush 32)
(defconst ngit-neg--large-flush 16384)
(defconst ngit-neg--max-in-vain 256)

(defun ngit-neg-next-flush (stateless count)
  (if stateless
      (if (< count ngit-neg--large-flush) (* count 2) (/ (* count 11) 10))
    (if (< count ngit-neg--pipesafe-flush) (* count 2)
      (+ count ngit-neg--pipesafe-flush))))

;; State is [REPO QUEUE NON-COMMON-REVS].  The caller binds a fresh
;; ngit--commit-table, so negotiation flags never escape into revision walks.
(defun ngit-neg--push (state c mark)
  (unless (ngit--has c mark)
    (ngit--set c mark)
    (ngit-parse-commit-object (aref state 0) c)
    (ngit--pq-put (aref state 1) c)
    (unless (ngit--has c ngit-neg--common)
      (aset state 2 (1+ (aref state 2))))))

(defun ngit-neg--mark-common (state c ancestors-only dont-parse)
  (unless (or (null c) (ngit--has c ngit-neg--common))
    (let ((queue (list c)))
      (unless ancestors-only
        (ngit--set c ngit-neg--common)
        (when (and (ngit--has c ngit-neg--seen) (not (ngit--has c ngit-neg--popped)))
          (aset state 2 (1- (aref state 2)))))
      (while queue
        (setq c (pop queue))
        (if (not (ngit--has c ngit-neg--seen))
            (ngit-neg--push state c ngit-neg--seen)
          (unless dont-parse (ngit-parse-commit-object (aref state 0) c))
          (dolist (p (ngit--c-parents c))
            (unless (ngit--has p ngit-neg--common)
              (ngit--set p ngit-neg--common)
              (when (and (ngit--has p ngit-neg--seen) (not (ngit--has p ngit-neg--popped)))
                (aset state 2 (1- (aref state 2))))
              (push p queue))))))))

(defun ngit-neg--peel (repo oid &optional mark-tags)
  (when (and oid (ngit-object-exists-p repo (ngit-unhex oid)))
    (let ((object (ngit-read-object repo (ngit-unhex oid))))
      (cond
       ((eq (car object) 'tag)
        (when mark-tags (ngit--set (ngit-commit oid) ngit-neg--complete))
        (ngit-neg--peel repo (ngit-header (ngit-parse-commit (cdr object)) "object") mark-tags))
       ((eq (car object) 'commit)
        (ngit-parse-commit-object repo (ngit-commit oid)))))))

(defun ngit-neg-init (repo refs)
  "Mark complete local history and advertised common refs, then seed tips."
  (let ((state (vector repo (ngit--pq-new t) 0))
        (complete (ngit--pq-new t)) (tips (ngit-list-refs repo))
        (cutoff 0) c)
    (dolist (ref refs)
      (let ((oid (nth 1 ref)))
        (when (and oid (ngit-object-exists-p repo (ngit-unhex oid))
                   (eq (ngit--object-type repo oid) 'commit))
          (setq c (ngit-parse-commit-object repo (ngit-commit oid))
                cutoff (max cutoff (ngit--c-date c))))))
    (dolist (ref tips)
      (setq c (ngit-neg--peel repo (nth 1 ref) t))
      (when (and c (not (ngit--has c ngit-neg--complete)))
        (ngit--set c ngit-neg--complete)
        (ngit--pq-put complete c)))
    (while (and (> cutoff 0) (> (aref complete 1) 0)
                (>= (ngit--c-date (cdr (aref (aref complete 0) 0))) cutoff))
      (setq c (ngit--pq-get complete))
      (dolist (p (ngit--c-parents c))
        (unless (ngit--has p ngit-neg--complete)
          (ngit-parse-commit-object repo p)
          (ngit--set p ngit-neg--complete)
          (ngit--pq-put complete p))))
    (dolist (ref refs)
      (setq c (ngit-neg--peel repo (nth 1 ref)))
      (when (and c (ngit--has c ngit-neg--complete) (not (ngit--has c ngit-neg--seen)))
        (ngit-neg--push state c (logior ngit-neg--advertised ngit-neg--seen))
        (ngit-neg--mark-common state c t t)))
    (dolist (ref tips)
      (setq c (ngit-neg--peel repo (nth 1 ref)))
      (when c (ngit-neg--push state c ngit-neg--seen)))
    state))

(defun ngit-neg-complete-p (oid)
  (let ((c (gethash oid ngit--commit-table)))
    (and c (ngit--has c ngit-neg--complete))))

(defun ngit-neg-next (state)
  "Return the next have, ordered by commit date with stable ties."
  (let (result c mark)
    (while (and (not result) (> (aref state 2) 0)
                (setq c (ngit--pq-get (aref state 1))))
      (ngit-parse-commit-object (aref state 0) c)
      (ngit--set c ngit-neg--popped)
      (unless (ngit--has c ngit-neg--common)
        (aset state 2 (1- (aref state 2))))
      (setq mark (if (or (ngit--has c ngit-neg--common)
                         (ngit--has c ngit-neg--advertised))
                     (logior ngit-neg--common ngit-neg--seen) ngit-neg--seen))
      (unless (ngit--has c ngit-neg--common) (setq result (ngit--c-hex c)))
      (dolist (p (ngit--c-parents c))
        (unless (ngit--has p ngit-neg--seen) (ngit-neg--push state p mark))
        (when (/= 0 (logand mark ngit-neg--common))
          (ngit-neg--mark-common state p t nil))))
    result))

(defun ngit-neg-ack (state oid)
  "Mark OID and its parents common; return whether OID was already common."
  (let* ((c (ngit-commit oid)) (known (ngit--has c ngit-neg--common)))
    (ngit-neg--mark-common state c nil t)
    known))

(defun ngit-neg--oid-bucket (oid mask)
  ;; oidhash reads the first native unsigned word; ngit targets x86-64.
  (logand mask (+ (string-to-number (substring oid 0 2) 16)
                 (ash (string-to-number (substring oid 2 4) 16) 8)
                 (ash (string-to-number (substring oid 4 6) 16) 16)
                 (ash (string-to-number (substring oid 6 8) 16) 24))))

(defun ngit-neg--common-grow (old)
  "Grow an oidset in khash's in-place rehash order."
  (let* ((size (length old)) (new-size (max 4 (* 2 size)))
         (slots (make-vector new-size nil))
         (live (make-vector size nil)) (used (make-vector new-size nil))
         (mask (1- new-size)))
    (dotimes (i size)
      (aset slots i (aref old i)) (aset live i (and (aref old i) t)))
    (dotimes (j size)
      (when (aref live j)
        (let ((key (aref slots j)))
          (aset live j nil)
          (while key
            (let ((i (ngit-neg--oid-bucket key mask)) (step 0))
              (while (aref used i)
                (setq step (1+ step) i (logand (+ i step) mask)))
              (aset used i t)
              (let ((displaced (and (< i size) (aref live i) (aref slots i))))
                (when displaced (aset live i nil))
                (aset slots i key) (setq key displaced)))))))
    (dotimes (i new-size) (unless (aref used i) (aset slots i nil)))
    slots))

(defun ngit-neg-common-add (set oid)
  "Insert OID into SET, retaining Git oidset's wire iteration order."
  (let* ((slots (aref set 0)) (size (length slots)))
    (when (>= (aref set 1) (/ (+ (* size 77) 50) 100))
      (setq slots (ngit-neg--common-grow slots) size (length slots))
      (aset set 0 slots))
    (let ((i (ngit-neg--oid-bucket oid (1- size))) (step 0))
      (while (and (aref slots i) (not (equal (aref slots i) oid)))
        (setq step (1+ step) i (logand (+ i step) (1- size))))
      (unless (aref slots i)
        (aset slots i oid) (aset set 1 (1+ (aref set 1)))))))

(provide 'ngit-negotiate)
;;; ngit-negotiate.el ends here
