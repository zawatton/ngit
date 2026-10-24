;;; ngit-bisect.el --- binary search for a bad commit -*- lexical-binding: t; -*-

;;; Commentary:

;; Implements the stateful porcelain used by `git bisect'.  Candidate
;; weights follow bisect.c: a commit's weight is the number of interesting
;; commits reachable from it, including itself.

;;; Code:

(require 'ngit-log)
(require 'ngit-branch)
(require 'ngit-switch)

(defconst ngit-bisect--state-files
  '("BISECT_ANCESTORS_OK" "BISECT_LOG" "BISECT_NAMES" "BISECT_RUN"
    "BISECT_TERMS" "BISECT_FIRST_PARENT" "BISECT_START"))

(defun ngit-bisect--path (repo name)
  (expand-file-name name (ngit-repo-gitdir repo)))

(defun ngit-bisect--write-file (repo name data &optional append)
  (ngit-write-file (ngit-bisect--path repo name) data nil append))

(defun ngit-bisect--read-file (repo name)
  (let ((path (ngit-bisect--path repo name)))
    (and (file-exists-p path) (ngit--trim-eol (ngit--read-file path)))))

(defun ngit-bisect--terms (repo)
  (let ((data (ngit-bisect--read-file repo "BISECT_TERMS")))
    (if data
        (let ((lines (split-string data "\n" t)))
          (cons (or (car lines) "bad") (or (cadr lines) "good")))
      (cons "bad" "good"))))

(defun ngit-bisect--subject (repo hex)
  (decode-coding-string (ngit--subject-of repo hex) 'utf-8))

(defun ngit-bisect--quote (s)
  "Quote S like Git's sq_quote_buf."
  (concat "'" (replace-regexp-in-string "'" "'\\''" s t t) "'"))

(defun ngit-bisect--start-line (args)
  (concat "git bisect start"
          (if args
              (concat " " (mapconcat #'ngit-bisect--quote args " "))
            "")
          "\n"))

(defun ngit-bisect--log-mark (repo state hex &optional command)
  (ngit-bisect--write-file
   repo "BISECT_LOG"
   (concat "# " state ": [" hex "] " (ngit-bisect--subject repo hex) "\n"
           (if command (concat "git bisect " state " " hex "\n") ""))
   t))

(defun ngit-bisect--delete-ref (repo ref)
  (when (ngit-read-ref repo ref) (ngit--delete-ref repo ref)))

(defun ngit-bisect--clean (repo)
  (dolist (r (ngit-list-refs repo "refs/bisect/"))
    (ngit--delete-ref repo (car r)))
  (dolist (ref '("BISECT_HEAD" "BISECT_EXPECTED_REV"))
    (ngit-bisect--delete-ref repo ref))
  (dolist (name ngit-bisect--state-files)
    (let ((path (ngit-bisect--path repo name)))
      (when (file-exists-p path) (delete-file path))))
  0)

(defun ngit-bisect--resolve-commit (repo rev)
  (condition-case nil
      (ngit-peel repo (ngit-rev-parse repo rev) 'commit)
    (error (ngit--die "Bad rev input: %s" rev))))

(defun ngit-bisect--write-mark (repo state hex terms &optional command)
  (let ((ref (if (string= state (cdr terms))
                 (concat "refs/bisect/" state "-" hex)
               (if (string= state "skip")
                   (concat "refs/bisect/skip-" hex)
                 (concat "refs/bisect/" state)))))
    (ngit--write-ref repo ref hex (ngit-resolve-ref repo ref) nil t)
    (ngit-bisect--log-mark repo state hex command)))

(defun ngit-bisect--reachable (repo starts &optional stop)
  "Return reachable commit names from STARTS, excluding names in STOP."
  (let ((seen (make-hash-table :test 'equal)) (out nil) (stack (copy-sequence starts)))
    (while stack
      (let ((hex (pop stack)))
        (unless (or (gethash hex seen) (and stop (gethash hex stop)))
          (puthash hex t seen)
          (push hex out)
          (dolist (p (ngit-commit-parents repo hex)) (push p stack)))))
    (nreverse out)))

(defun ngit-bisect--interesting (repo bad good)
  (let ((excluded (make-hash-table :test 'equal)))
    (dolist (hex (ngit-bisect--reachable repo good)) (puthash hex t excluded))
    (ngit-bisect--reachable repo (list bad) excluded)))

(defun ngit-bisect--walk-order (repo bad good)
  "Return bisect.c's reversed prepared revision-list order."
  (let ((ngit--commit-table (make-hash-table :test 'equal))
        (ngit--rv-repo nil) (ngit--rv-commits nil)
        (ngit--rv-limited nil) (ngit--rv-first-parent nil)
        (ngit--rv-paths nil) (ngit--rv-dense t)
        (ngit--rv-simplify-history t) (ngit--rv-rewrite-parents nil)
        (ngit--rv-topo nil) (ngit--rv-max-age nil) (ngit--rv-min-age nil)
        (ngit--rv-min-parents 0) (ngit--rv-max-parents -1)
        (ngit--rv-grep nil) (ngit--rv-max-count -1) (ngit--rv-skip 0)
        (ngit--rv-reverse nil) (ngit--rv-boundary nil)
        (ngit--rv-no-walk nil) (starts nil) (out nil))
    (push (cons (ngit-commit bad) 0) starts)
    (dolist (hex good) (push (cons (ngit-commit hex) ngit-uninteresting) starts))
    (ngit-walk-prepare repo (nreverse starts))
    (dolist (c ngit--rv-commits)
      (unless (ngit--has c ngit-uninteresting) (push (ngit--c-hex c) out)))
    out))

(defun ngit-bisect--weight (repo hex interesting)
  (let ((seen (make-hash-table :test 'equal)) (stack (list hex)) (n 0))
    (while stack
      (let ((one (pop stack)))
        (when (and (gethash one interesting) (not (gethash one seen)))
          (puthash one t seen)
          (setq n (1+ n))
          (dolist (p (ngit-commit-parents repo one)) (push p stack)))))
    n))

(defun ngit-bisect--weights (repo names interesting)
  "Return memoized bisect weights for NAMES.
Single-parent chains are filled once; only merge commits need a reachability
count, matching bisect.c's cheap path for ordinary history."
  (let ((weights (make-hash-table :test 'equal)))
    (dolist (start names)
      (unless (gethash start weights)
        (let ((path nil) (hex start) (base nil) (done nil))
          (while (not done)
            (let ((known (gethash hex weights)))
              (if known
                  (setq base known done t)
                (let ((parents nil))
                  (dolist (p (ngit-commit-parents repo hex))
                    (when (gethash p interesting) (push p parents)))
                  (cond
                   ((null parents) (push hex path) (setq base 0 done t))
                   ((null (cdr parents)) (push hex path) (setq hex (car parents)))
                   (t
                    (puthash hex (ngit-bisect--weight repo hex interesting) weights)
                    (setq base (gethash hex weights) done t)))))))
          (dolist (one path)
            (setq base (1+ base))
            (puthash one base weights)))))
    weights))

(defun ngit-bisect--isqrt (n)
  (if (= n 0) 0
    (let ((x n) (y (/ (1+ n) 2)))
      (while (< y x) (setq x y y (/ (+ y (/ n y)) 2)))
      x)))

(defun ngit-bisect--skip-away (items bad)
  "Choose from non-skipped ITEMS with Git's deterministic skip offset."
  (let* ((count (length items))
         (u32 (logand #xffffffff (+ (* count 1103515245) 12345)))
         (prn (% (/ u32 65536) 32768))
         (index (/ (* (/ (* count prn) 32768) (ngit-bisect--isqrt prn))
                   (ngit-bisect--isqrt 32768)))
         (choice (nth index items)))
    (if (and (equal choice bad) (> index 0)) (nth (1- index) items) choice)))

(defun ngit-bisect--choose (repo bad good skipped)
  "Return (HEX WEIGHT TOTAL TRIED) for the next candidate."
  (let* ((names (ngit-bisect--interesting repo bad good))
         (order (ngit-bisect--walk-order repo bad good))
         (set (make-hash-table :test 'equal)) (weighted nil) weights)
    (dolist (hex names) (puthash hex t set))
    (setq weights (ngit-bisect--weights repo names set))
    (let ((total (length names)))
      (dolist (hex names)
        (let* ((weight (gethash hex weights))
               (distance (min weight (- total weight))))
          (push (list hex weight distance) weighted)))
      (if skipped
          (let ((ordered (sort weighted
                               (lambda (a b)
                                 (if (= (nth 2 a) (nth 2 b))
                                     (string< (car a) (car b))
                                   (> (nth 2 a) (nth 2 b))))))
                (tried nil) (kept nil) (reported-weight nil))
            (setq reported-weight (nth 1 (car ordered)))
            (if (not (gethash (caar ordered) skipped))
                (list (caar ordered) reported-weight total nil)
              (dolist (item ordered)
                (if (gethash (car item) skipped) (push (car item) tried)
                  (push item kept)))
              (setq kept (nreverse kept) tried (nreverse tried))
              (let* ((chosen (ngit-bisect--skip-away (mapcar #'car kept) bad))
                     (item (assoc chosen kept)))
                (ignore item)
                (list chosen reported-weight total tried))))
        (let ((best nil) (best-distance -1)
              (assigned (make-hash-table :test 'equal)) (counted 0))
          ;; Seed roots exactly as do_find_bisection does.  Their weights do
          ;; not take the early halfway shortcut.
          (dolist (hex order)
            (let ((parents 0))
              (dolist (p (ngit-commit-parents repo hex))
                (when (gethash p set) (setq parents (1+ parents))))
              (when (= parents 0)
                (puthash hex t assigned)
                (setq counted (1+ counted)))))
          ;; Merge weights are counted before the single-parent fill loop.
          (dolist (hex order)
            (let ((parents 0))
              (dolist (p (ngit-commit-parents repo hex))
                (when (gethash p set) (setq parents (1+ parents))))
              (when (> parents 1)
                (puthash hex t assigned)
                (setq counted (1+ counted))
                (when (and (not best)
                           (<= (abs (- (* 2 (gethash hex weights)) total)) 1))
                  (setq best (assoc hex weighted))))))
          ;; A single-parent weight becomes known as soon as one interesting
          ;; parent is known.  Scan in the reversed prepared-list order.
          (while (and (not best) (< counted total))
            (dolist (hex order)
              (unless (gethash hex assigned)
                (let ((known nil))
                  (dolist (p (ngit-commit-parents repo hex))
                    (when (and (not known) (gethash p set) (gethash p assigned))
                      (setq known t)))
                  (when known
                    (puthash hex t assigned)
                    (setq counted (1+ counted))
                    (when (and (not best)
                               (<= (abs (- (* 2 (gethash hex weights)) total)) 1))
                      (setq best (assoc hex weighted))))))))
          (unless best
            (dolist (hex order)
              (let ((item (assoc hex weighted)))
                (when (> (nth 2 item) best-distance)
                  (setq best item best-distance (nth 2 item))))))
          (list (car best) (nth 1 best) total nil))))))

(defun ngit-bisect--refs (repo terms)
  (let ((bad (ngit-resolve-ref repo (concat "refs/bisect/" (car terms))))
        (good nil) (skipped (make-hash-table :test 'equal)) (has-skips nil))
    (dolist (r (ngit-list-refs repo "refs/bisect/"))
      (let ((name (car r)) (hex (nth 1 r)))
        (cond ((string-prefix-p (concat "refs/bisect/" (cdr terms) "-") name)
               (push hex good))
              ((string-prefix-p "refs/bisect/skip-" name)
               (puthash hex t skipped) (setq has-skips t)))))
    (list bad good (and has-skips skipped))))

(defun ngit-bisect--steps (all)
  (if (< all 3) 0
    (let ((n 0) (e 1))
      (while (<= (* e 2) all) (setq e (* e 2) n (1+ n)))
      (if (< e (* 3 (- all e))) n (1- n)))))

(defun ngit-bisect--show-first-bad (repo hex term)
  (ngit-bisect--write-file
   repo "BISECT_LOG"
   (concat "# first " term " commit: [" hex "] "
           (ngit-bisect--subject repo hex) "\n") t)
  (ngit--emit hex " is the first " term " commit\n")
  (let ((ngit--commit-table (make-hash-table :test 'equal)))
    (ngit-cmd-log repo (list "--stat" "-1" hex))))

(defun ngit-bisect--checkout (repo hex)
  (ngit--write-ref repo "BISECT_EXPECTED_REV" hex
                   (ngit-resolve-ref repo "BISECT_EXPECTED_REV") nil t)
  (ngit--switch-run repo (list "-q" "--detach" hex) t)
  (ngit--emit "[" hex "] " (ngit-bisect--subject repo hex) "\n"))

(defun ngit-bisect--only-skipped (repo term bad tried)
  (let ((log "# only skipped commits left to test\n"))
    (dolist (one (cons bad (reverse tried)))
      (setq log (concat log "# possible first " term " commit: [" one "] "
                        (ngit-bisect--subject repo one) "\n")))
    (ngit-bisect--write-file repo "BISECT_LOG" log t))
  (ngit--emit "There are only 'skip'ped commits left to test.\nThe first "
              term " commit could be any of:\n")
  (dolist (one tried) (ngit--emit one "\n"))
  (ngit--emit bad "\nWe cannot bisect more!\n")
  (throw 'ngit-bisect-status 2))

(defun ngit-bisect--check-ancestors (repo bad good skipped)
  "Verify GOOD are ancestors of BAD, testing an unknown merge base first."
  (let ((ok-file (ngit-bisect--path repo "BISECT_ANCESTORS_OK")))
    (if (file-exists-p ok-file)
        'ok
      (let ((all-ancestors t) (unknown nil))
        (let ((ngit--commit-table (make-hash-table :test 'equal)))
          (dolist (hex good)
            (unless (ngit--in-merge-bases repo (ngit-commit hex) (ngit-commit bad))
              (setq all-ancestors nil)))
          (unless all-ancestors
            (let ((good-set (make-hash-table :test 'equal)))
              (dolist (hex good) (puthash hex t good-set))
              (dolist (base (ngit-merge-bases
                             repo (ngit-commit bad) (mapcar #'ngit-commit good)))
                (let ((hex (ngit--c-hex base)))
                  (cond
                   ((equal hex bad)
                    (ngit--die "The merge base %s is bad.\nThis means the bug has been fixed between %s and %s."
                               hex hex bad))
                   ((gethash hex good-set))
                   ((and skipped (gethash hex skipped))
                    (ngit-native-write-stderr
                     (format "warning: the merge base between %s and the good revisions must be skipped.\nWe continue anyway.\n"
                             bad)))
                   ((not unknown) (setq unknown hex))))))))
        (if unknown
            (progn
              (ngit--emit "Bisecting: a merge base must be tested\n")
              (ngit-bisect--checkout repo unknown)
              'merge-base)
          (ngit-bisect--write-file repo "BISECT_ANCESTORS_OK" "")
          'ok)))))

(defun ngit-bisect--next (repo terms)
  (let* ((refs (ngit-bisect--refs repo terms))
         (bad (nth 0 refs)) (good (nth 1 refs)) (skipped (nth 2 refs)))
    (unless (and bad good)
      (let ((status
             (cond ((and (not bad) (not good))
                    "status: waiting for both good and bad commits\n")
                   ((not good)
                    "status: waiting for good commit(s), bad commit known\n")
                   (t (format "status: waiting for bad commit, %d good %s known\n"
                              (length good) (if (= (length good) 1) "commit" "commits"))))))
        (ngit--emit status)
        (ngit-bisect--write-file repo "BISECT_LOG" (concat "# " status) t)
        (throw 'ngit-bisect-next 'continue)))
    (when (eq (ngit-bisect--check-ancestors repo bad good skipped) 'merge-base)
      (throw 'ngit-bisect-next 'merge-base))
    (let* ((choice (ngit-bisect--choose repo bad good skipped))
           (hex (nth 0 choice)) (weight (nth 1 choice)) (all (nth 2 choice))
           (tried (nth 3 choice)))
      (unless hex
        (ngit-bisect--only-skipped repo (car terms) bad tried))
      (if (equal hex bad)
          (if tried
              (ngit-bisect--only-skipped repo (car terms) bad tried)
            (ngit-bisect--show-first-bad repo bad (car terms))
            'found)
        (let ((left (- all weight 1)) (steps (ngit-bisect--steps all)))
          (ngit--emit (format (if (= left 1)
                                 "Bisecting: %d revision left to test after this (roughly %d %s)\n"
                               "Bisecting: %d revisions left to test after this (roughly %d %s)\n")
                             left steps (if (= steps 1) "step" "steps")))
          (ngit-bisect--checkout repo hex)
          'continue)))))

(defun ngit-bisect--start (repo args)
  (let ((raw-head nil) (revs nil))
    (dolist (a args)
      (when (string-prefix-p "-" a) (ngit--die "error: unrecognized option: '%s'" a))
      (push (ngit-bisect--resolve-commit repo a) revs))
    (setq revs (nreverse revs))
    (when (ngit-bisect--read-file repo "BISECT_START")
      (ngit--switch-run repo (list (ngit-bisect--read-file repo "BISECT_START")) t))
    (setq raw-head (ngit-read-ref repo "HEAD"))
    (ngit-bisect--clean repo)
    (ngit-bisect--write-file
     repo "BISECT_START"
     (concat (if (and raw-head (string-prefix-p "ref: refs/heads/" raw-head))
                 (substring raw-head 16)
               (or (ngit-resolve-ref repo "HEAD") "")) "\n"))
    (ngit-bisect--write-file repo "BISECT_NAMES" "\n")
    (when revs
      (let ((terms (cons "bad" "good")) (first t))
        (ngit-bisect--write-file repo "BISECT_TERMS" "bad\ngood\n")
        (dolist (hex revs)
          (ngit-bisect--write-mark repo (if first "bad" "good") hex terms nil)
          (setq first nil))))
    (ngit-bisect--write-file repo "BISECT_LOG" (ngit-bisect--start-line args) t)
    (catch 'ngit-bisect-next
      (ngit-bisect--next repo (cons "bad" "good")))))

(defun ngit-bisect--state (repo state revs)
  (unless (ngit-bisect--read-file repo "BISECT_START")
    (ngit--die-with 1 "You need to start by \"git bisect start\"\n"))
  (let* ((terms (ngit-bisect--terms repo))
         (resolved (if revs (mapcar (lambda (r) (ngit-bisect--resolve-commit repo r)) revs)
                     (list (or (ngit-resolve-ref repo "BISECT_HEAD")
                               (ngit-resolve-ref repo "HEAD"))))))
    (when (and (member state '("good" "bad"))
               (not (file-exists-p (ngit-bisect--path repo "BISECT_TERMS"))))
      (ngit-bisect--write-file repo "BISECT_TERMS" "bad\ngood\n"))
    (when (and (string= state (car terms)) (cdr resolved))
      (ngit--die "'git bisect %s' can take only one argument." state))
    (dolist (hex resolved)
      (ngit-bisect--write-mark repo state hex terms t)
      (let ((expected (ngit-resolve-ref repo "BISECT_EXPECTED_REV")))
        (when (and expected (not (equal expected hex)))
          (ngit-bisect--delete-ref repo "BISECT_EXPECTED_REV")
          (let ((path (ngit-bisect--path repo "BISECT_ANCESTORS_OK")))
            (when (file-exists-p path) (delete-file path))))))
    (catch 'ngit-bisect-next (ngit-bisect--next repo terms))))

(defun ngit-bisect--reset (repo args)
  (when (cdr args) (ngit--die "'git bisect reset' requires either no argument or a commit"))
  (let ((target (or (car args) (ngit-bisect--read-file repo "BISECT_START"))))
    (when target (ngit--switch-run repo (list target) t))
    (ngit-bisect--clean repo)))

(defun ngit-bisect--run-command (args)
  (let* ((display (mapconcat #'ngit-bisect--quote args " "))
         (stdout (make-temp-file "ngit-bisect-out-"))
         (stderr (make-temp-file "ngit-bisect-err-"))
         (shell-line (concat display " >" (ngit-bisect--quote stdout)
                             " 2>" (ngit-bisect--quote stderr)))
         (status nil))
    (ngit--emit "running " display "\n")
    (ngit--flush)
    ;; The standalone runtime does not reliably honor call-process buffer
    ;; destinations in batch mode, so split the two child channels in the
    ;; shell and replay them through ngit's normal writers.
    (setq status (call-process "sh" nil nil nil "-c" shell-line display))
    (let ((out (ngit--read-file stdout)) (err (ngit--read-file stderr)))
      (when (> (length out) 0) (princ out))
      (when (> (length err) 0) (ngit-native-write-stderr err)))
    (delete-file stdout)
    (delete-file stderr)
    status))

(defun ngit-bisect--capture-state (repo state)
  (let ((outer ngit--out) (status nil) (text ""))
    (setq ngit--out nil)
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (setq status (ngit-bisect--state repo state nil))
        (ngit--flush)
        (setq text (buffer-string))))
    (setq ngit--out outer)
    (ngit-bisect--write-file repo "BISECT_RUN" text)
    (ngit--emit text)
    status))

(defun ngit-bisect--run (repo args)
  (unless args (ngit--die "bisect run failed: no command provided."))
  (let ((done nil) (result 0) (first t))
    (while (not done)
      (let ((code (ngit-bisect--run-command args)))
        (when (and first (memq code '(126 127)))
          (let* ((terms (ngit-bisect--terms repo))
                 (refs (ngit-bisect--refs repo terms))
                 (good (car (nth 1 refs)))
                 (current (ngit-resolve-ref repo "HEAD")))
            (ngit-bisect--checkout repo good)
            (let ((verify (ngit-bisect--run-command args)))
              (ngit-bisect--checkout repo current)
              (when (or (not (integerp verify)) (>= verify 128))
                (ngit--die-with 1 "error: unable to verify %s on good revision"
                                (mapconcat #'ngit-bisect--quote args " ")))
              (when (= verify code)
                (ngit--die-with 1 "error: bogus exit code %d for good revision" code)))))
        (setq first nil)
        (if (or (not (integerp code)) (>= code 128))
            (ngit--die-with
             126 "error: bisect run failed: exit code %d from %s is < 0 or >= 128"
             (if (integerp code) code -1) (mapconcat #'ngit-bisect--quote args " "))
          (let* ((terms (ngit-bisect--terms repo))
                 (state (cond ((= code 0) (cdr terms))
                              ((= code 125) "skip")
                              (t (car terms))))
                 (next (ngit-bisect--capture-state repo state)))
            (cond ((eq next 'found)
                   (ngit--emit "bisect found first bad commit\n")
                   (setq done t))
                  ((eq next 'merge-base)
                   (ngit--emit "bisect run success\n")
                   (setq done t)))))))
    result))

;;;###autoload
(defun ngit-cmd-bisect (repo args)
  "Run `git bisect' porcelain with ARGS."
  (catch 'ngit-bisect-status
    (let ((sub (car args)) (rest (cdr args)))
      (cond
       ((or (null sub) (string= sub "start"))
        (ngit-bisect--start repo (if sub rest nil)) 0)
       ((member sub '("good" "bad" "skip"))
        (ngit-bisect--state repo sub rest) 0)
       ((string= sub "reset") (ngit-bisect--reset repo rest))
       ((string= sub "log")
        (let ((log (ngit-bisect--read-file repo "BISECT_LOG")))
          (unless log (ngit--die-with 1 "error: We are not bisecting."))
          (ngit--emit log "\n"))
        0)
       ((string= sub "run") (ngit-bisect--run repo rest))
       (t (ngit--die "error: unknown subcommand: `%s'" sub))))))

(provide 'ngit-bisect)

;;; ngit-bisect.el ends here
