;;; ngit-commit.el --- git commit -*- lexical-binding: t; -*-

;;; Commentary:

;; git commit with -m/-F messages (no editor and no signing).
;; The sequence follows builtin/commit.c: refresh the index (and stage
;; tracked changes for -a), update the cache tree, write the index
;; (through index.lock for -a, committed only on success), write
;; COMMIT_EDITMSG, then the commit object, the ref, the reflogs and the
;; summary.
;;
;; NeLisp's `format-time-string' always reports UTC, so the local offset
;; comes from the TZif file named by TZ or /etc/localtime, with its POSIX
;; footer rule (Mm.w.d DST transitions) beyond the last transition.

;;; Code:

(require 'ngit-add)
(require 'ngit-status)
(require 'ngit-xdiff)
(require 'ngit-diff)

(defun ngit-commit-author-env (author)
  "Return the GIT_AUTHOR_* environment represented by AUTHOR."
  (let (env)
    (when author
      (let* ((open (string-search " <" author))
             (close (and open (string-search "> " author (+ open 2)))))
        (when close
          (setq env (list (concat "GIT_AUTHOR_NAME=" (substring author 0 open))
                          (concat "GIT_AUTHOR_EMAIL=" (substring author (+ open 2) close))
                          (concat "GIT_AUTHOR_DATE=@" (substring author (+ close 2))))))))
    env))

(defun ngit-commit-hook-env (index-file author &optional reflog-action)
  "Return Git's commit-hook environment for INDEX-FILE and AUTHOR."
  (let ((env (append (list (concat "GIT_INDEX_FILE=" index-file) "GIT_EDITOR=:")
                     (ngit-commit-author-env author))))
    (when reflog-action
      (setq env (append env (list (concat "GIT_REFLOG_ACTION=" reflog-action)))))
    env))

(defun ngit-hook-path-arg (repo path)
  "Return PATH in the form Git passes to hooks from REPO's hook cwd."
  (let* ((root (ngit-repo-toplevel repo))
         (gitdir (directory-file-name (expand-file-name (ngit-repo-gitdir repo))))
         (ordinary (and root
                        (equal gitdir (directory-file-name
                                       (expand-file-name ".git" root))))))
    (if (and ordinary (file-name-absolute-p path))
        (ngit--hook-relative-path path root)
      path)))

(defun ngit-commit-run-hook (repo name index-file author &optional args input reflog-action)
  "Run commit hook NAME with Git's index, author and editor environment."
  (if (not (ngit-hook-exists-p repo name)) 0
    (when (and args (member name '("prepare-commit-msg" "commit-msg")))
      (setq args (cons (ngit-hook-path-arg repo (car args)) (cdr args))))
    (ngit-hook--run repo name args input
                    (ngit-commit-hook-env
                     (if (string-suffix-p ".lock" index-file)
                         index-file
                       (ngit-hook-path-arg repo index-file))
                     author reflog-action))))

(defun ngit-commit-read-hook-index (repo index-file)
  "Read INDEX-FILE after a hook, without retaining it in REPO's cache."
  (let ((saved (getenv "GIT_INDEX_FILE")) index)
    (unwind-protect
        (progn
          (setenv "GIT_INDEX_FILE" index-file)
          (aset repo 6 nil)
          (setq index (ngit-read-index repo)))
      (setenv "GIT_INDEX_FILE" saved)
      (aset repo 6 nil))
    index))

(defun ngit-commit-clean-hook-message (repo data comments)
  "Clean hook-written message DATA, removing comment lines when COMMENTS."
  (ngit-stripspace
   (if (not comments) data
     (let ((prefix (or (ngit-config-get repo "core.commentchar") "#")))
       (mapconcat #'identity
                  (seq-remove (lambda (line) (string-prefix-p prefix line))
                              (split-string data "\n"))
                  "\n")))))

;;;; Time zones

(defun ngit--days-from-civil (y m d)
  (let* ((y (if (<= m 2) (1- y) y))
         (era (/ (if (>= y 0) y (- y 399)) 400))
         (yoe (- y (* era 400)))
         (doy (+ (/ (+ (* 153 (if (> m 2) (- m 3) (+ m 9))) 2) 5) (1- d)))
         (doe (+ (* yoe 365) (/ yoe 4) (- (/ yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

(defun ngit--posix-offset (s pos)
  "Parse a POSIX TZ offset ([+-]hh[:mm[:ss]]) at POS; return (SECONDS . NEXT).
SECONDS is west of UTC, as written."
  (let ((sign 1) (parts nil) (n (length s)))
    (when (and (< pos n) (memq (aref s pos) '(?+ ?-)))
      (when (= (aref s pos) ?-) (setq sign -1))
      (setq pos (1+ pos)))
    (while (progn
             (let ((v 0))
               (while (and (< pos n) (<= ?0 (aref s pos) ?9))
                 (setq v (+ (* v 10) (- (aref s pos) ?0)) pos (1+ pos)))
               (push v parts))
             (and (< pos n) (= (aref s pos) ?:) (setq pos (1+ pos)))))
    (setq parts (nreverse parts))
    (cons (* sign (+ (* 3600 (or (nth 0 parts) 0)) (* 60 (or (nth 1 parts) 0)) (or (nth 2 parts) 0)))
          pos)))

(defun ngit--posix-name-end (s pos)
  (if (and (< pos (length s)) (= (aref s pos) ?<))
      (1+ (or (ngit--byte-pos s ?> pos) (1- (length s))))
    (while (and (< pos (length s))
                (let ((c (aref s pos))) (or (<= ?a c ?z) (<= ?A c ?Z))))
      (setq pos (1+ pos)))
    pos))

(defun ngit--mrule-day (year spec)
  "Return the day number (days since epoch) of rule Mm.w.d in YEAR."
  (let* ((f (mapcar #'string-to-number (split-string spec "\\.")))
         (m (nth 0 f)) (w (nth 1 f)) (d (nth 2 f))
         (first (ngit--days-from-civil year m 1))
         (dow (mod (+ first 4) 7))
         (day (+ 1 (mod (- d dow) 7) (* 7 (1- w))))
         (mdays (- (ngit--days-from-civil (if (= m 12) (1+ year) year) (if (= m 12) 1 (1+ m)) 1)
                   first)))
    (while (> day mdays) (setq day (- day 7)))
    (+ first (1- day))))

(defun ngit--posix-tz-offset (s time)
  "Return the UTC offset in seconds east for POSIX TZ string S at TIME."
  (let* ((p (ngit--posix-name-end s 0))
         (std (ngit--posix-offset s p))
         (stdoff (- (car std)))
         (p (cdr std)))
    (if (>= p (length s))
        stdoff
      (let* ((p2 (ngit--posix-name-end s p))
             (dstoff (if (and (< p2 (length s)) (/= (aref s p2) ?,))
                         (- (car (ngit--posix-offset s p2)))
                       (+ stdoff 3600)))
             (comma (ngit--byte-pos s ?, p2))
             (rules (and comma (split-string (substring s (1+ comma)) ","))))
        (if (not (and (= (length rules) 2)
                      (string-prefix-p "M" (car rules)) (string-prefix-p "M" (cadr rules))))
            stdoff
          (let* ((year (car (ngit--civil (floor (+ time stdoff) 86400))))
                 (edge (lambda (rule off)
                         (let* ((slash (ngit--byte-pos rule ?/ 0))
                                (at (if slash (car (ngit--posix-offset rule (1+ slash))) 7200)))
                           (- (+ (* 86400 (ngit--mrule-day year (substring rule 1 slash))) at) off))))
                 (start (funcall edge (car rules) stdoff))
                 (end (funcall edge (cadr rules) dstoff)))
            (if (if (< start end) (and (<= start time) (< time end))
                  (not (and (<= end time) (< time start))))
                dstoff
              stdoff)))))))

(defun ngit--be32s (s pos)
  (let ((v (ngit--be32 s pos))) (if (>= v #x80000000) (- v #x100000000) v)))

(defun ngit--be64s (s pos)
  (let ((v (logior (ash (ngit--be32 s pos) 32) (ngit--be32 s (+ pos 4)))))
    (if (>= (aref s pos) 128) (- v (ash 1 64)) v)))

(defun ngit--tzif-offset (data time)
  "Return the UTC offset (seconds east) at TIME from TZif DATA, or nil."
  (when (and (> (length data) 44) (string= (substring data 0 4) "TZif"))
    (let* ((v2 (/= (aref data 4) 0))
           (counts (lambda (p) (mapcar (lambda (i) (ngit--be32 data (+ p 20 (* 4 i)))) '(0 1 2 3 4 5))))
           (c (funcall counts 0))
           (base 44) (tsize 4))
      (when v2
        ;; Skip the 32-bit block to the 64-bit header.
        (setq base (+ 44 (* 5 (nth 3 c)) (* 6 (nth 4 c)) (nth 5 c) (* 8 (nth 2 c))
                      (nth 1 c) (nth 0 c)))
        (setq c (funcall counts base) base (+ base 44) tsize 8))
      (let* ((timecnt (nth 3 c)) (typecnt (nth 4 c))
             (times base)
             (idxs (+ times (* tsize timecnt)))
             (types (+ idxs timecnt))
             (last -1) (i 0))
        (while (and (< i timecnt)
                    (<= (if v2 (ngit--be64s data (+ times (* 8 i))) (ngit--be32s data (+ times (* 4 i))))
                        time))
          (setq last i i (1+ i)))
        (let* ((footer (and v2 (= last (1- timecnt))
                            (let* ((p (+ types (* 6 typecnt) (nth 5 c) (* 12 (nth 2 c))
                                         (nth 1 c) (nth 0 c))))
                              (and (< (1+ p) (length data)) (= (aref data p) ?\n)
                                   (let ((e (ngit--byte-pos data ?\n (1+ p))))
                                     (and e (> e (1+ p)) (substring data (1+ p) e))))))))
          (if footer
              (ngit--posix-tz-offset footer time)
            (ngit--be32s data (+ types (* 6 (if (>= last 0) (aref data (+ idxs last)) 0))))))))))

(defun ngit-local-offset (time)
  "Return the local UTC offset in seconds east at epoch TIME."
  (let* ((tz (getenv "TZ"))
         (tz (if (and tz (string-prefix-p ":" tz)) (substring tz 1) tz))
         (file (cond ((null tz) "/etc/localtime")
                     ((string= tz "") nil)
                     ((file-name-absolute-p tz) tz)
                     ((file-exists-p (concat "/usr/share/zoneinfo/" tz))
                      (concat "/usr/share/zoneinfo/" tz)))))
    (or (and file (file-readable-p file) (ngit--tzif-offset (ngit--read-file file) time))
        (and tz (not (string= tz "")) (ngit--posix-tz-offset tz time))
        0)))

(defun ngit--tz-string (off)
  (let ((a (abs off)))
    (format "%s%02d%02d" (if (< off 0) "-" "+") (/ a 3600) (/ (% a 3600) 60))))

;;;; Identities

(defun ngit--parse-date (s)
  "Parse a GIT_*_DATE value into \"STAMP TZ\": raw (\"[@]STAMP [TZ]\") or
ISO 8601 (\"YYYY-MM-DD[T ]HH:MM[:SS][ ][TZ]\")."
  (let ((case-fold-search nil))
    (cond
     ((string-match "\\`@?\\([0-9]+\\)\\(?: +\\([-+][0-9][0-9]:?[0-9][0-9]\\)\\)?\\'" s)
      (let ((stamp (string-to-number (match-string 1 s))) (tz (match-string 2 s)))
        (concat (number-to-string stamp) " "
                (if tz (replace-regexp-in-string ":" "" tz)
                  (ngit--tz-string (ngit-local-offset stamp))))))
     ((string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\(?::\\([0-9][0-9]\\)\\)? *\\(Z\\|[-+][0-9][0-9]:?[0-9][0-9]\\)?\\'" s)
      (let* ((n (lambda (i) (string-to-number (or (match-string i s) "0"))))
             (local (+ (* 86400 (ngit--days-from-civil (funcall n 1) (funcall n 2) (funcall n 3)))
                       (* 3600 (funcall n 4)) (* 60 (funcall n 5)) (funcall n 6)))
             (tz (match-string 7 s))
             (off (cond ((null tz) (ngit-local-offset (- local (ngit-local-offset local))))
                        ((string= tz "Z") 0)
                        (t (let ((d (replace-regexp-in-string ":" "" tz)))
                             (* (if (= (aref d 0) ?-) -1 1)
                                (+ (* 3600 (string-to-number (substring d 1 3)))
                                   (* 60 (string-to-number (substring d 3 5))))))))))
        (concat (number-to-string (- local off)) " " (ngit--tz-string off))))
     (t (ngit--die "fatal: invalid date format: %s" s)))))

(defun ngit--now-date ()
  (let ((now (floor (float-time))))
    (concat (number-to-string now) " " (ngit--tz-string (ngit-local-offset now)))))

(defun ngit-ident (repo who)
  "Return \"Name <email> STAMP TZ\" for WHO, \"AUTHOR\" or \"COMMITTER\"."
  (let* ((role (downcase who))
         (name (or (getenv (concat "GIT_" who "_NAME"))
                   (ngit-config-get repo (concat role ".name"))
                   (ngit-config-get repo "user.name")))
         (email (or (getenv (concat "GIT_" who "_EMAIL"))
                    (ngit-config-get repo (concat role ".email"))
                    (ngit-config-get repo "user.email")
                    (getenv "EMAIL")))
         (date (getenv (concat "GIT_" who "_DATE"))))
    (unless (and (stringp name) (stringp email) (not (string= name "")))
      (ngit-native-write-stderr
       (concat (if (string= who "AUTHOR") "Author" "Committer") " identity unknown\n\n"
               "*** Please tell me who you are.\n\nRun\n\n"
               "  git config --global user.email \"you@example.com\"\n"
               "  git config --global user.name \"Your Name\"\n\n"
               "to set your account's default identity.\n"
               "Omit --global to set the identity only in this repository.\n\n"))
      (ngit--die "fatal: no name or email configured (ngit does not guess them)"))
    (concat (encode-coding-string name 'utf-8) " <" (encode-coding-string email 'utf-8) "> "
            (if date (ngit--parse-date date) (ngit--now-date)))))

;;;; Messages

(defun ngit-stripspace (msg)
  "git's strbuf_stripspace without comment stripping."
  (let ((lines (split-string msg "\n")) (out nil) (empties 0))
    (when (string-suffix-p "\n" msg) (setq lines (butlast lines)))
    (dolist (line lines)
      (let ((len (length line)))
        (while (and (> len 0) (memq (aref line (1- len)) '(?\s ?\t ?\r ?\v ?\f)))
          (setq len (1- len)))
        (if (= len 0)
            (setq empties (1+ empties))
          (when (and (> empties 0) out) (push "\n" out))
          (setq empties 0)
          (push (substring line 0 len) out)
          (push "\n" out))))
    (apply #'concat (nreverse out))))

(defun ngit--message-body (data)
  "Return the message of commit DATA (after the header)."
  (let ((i (string-match "\n\n" data)))
    (if i (substring data (+ i 2)) "")))

(defun ngit--subject (msg)
  "Return the first paragraph of MSG with its lines joined by spaces."
  (let ((out nil))
    (catch 'done
      (dolist (line (split-string msg "\n"))
        (let ((len (length line)))
          (while (and (> len 0) (memq (aref line (1- len)) '(?\s ?\t ?\r ?\v ?\f)))
            (setq len (1- len)))
          (when (= len 0) (throw 'done nil))
          (push (substring line 0 len) out))))
    (mapconcat #'identity (nreverse out) " ")))

(defun ngit--reflog-message (msg)
  "git's copy_reflog_msg: collapse whitespace runs, trim both ends."
  (let ((out nil) (wasspace t))
    (dotimes (i (length msg))
      (let* ((c (aref msg i)) (sp (memq c '(?\s ?\t ?\n ?\r ?\v ?\f))))
        (unless (and wasspace sp)
          (setq wasspace sp)
          (push (if sp ?\s c) out))))
    (while (and out (= (car out) ?\s)) (pop out))
    (apply #'unibyte-string (nreverse out))))

;;;; Refs

(defun ngit--append-reflog (repo ref old new ident msg)
  (let* ((dir (if (or (string= ref "HEAD") (not (string-prefix-p "refs/" ref)))
                  (ngit-repo-gitdir repo)
                (ngit-repo-commondir repo)))
         (file (expand-file-name (concat "logs/" ref) dir))
         (auto (let ((v (ngit-config-get repo "core.logallrefupdates")))
                 (if (null v) (ngit-repo-toplevel repo)
                   (or (equal v "always") (ngit-config-bool repo "core.logallrefupdates" nil))))))
    (when (or (file-exists-p file)
              (and auto (or (string= ref "HEAD") (string-prefix-p "refs/heads/" ref)
                            (string-prefix-p "refs/remotes/" ref)
                            (string-prefix-p "refs/notes/" ref))))
      (ngit-make-directory (file-name-directory file) t)
      (ngit-write-file
       file (concat (or old (make-string 40 ?0)) " " new " " ident "\t"
                    (ngit--reflog-message msg) "\n")
       nil t))))

(defun ngit-update-head (repo new old ident msg &optional check-unborn)
  "Point HEAD (or the branch it names) at NEW, logging MSG.
Check a non-nil OLD under lock.  CHECK-UNBORN also checks that a nil OLD
still denotes an absent ref; otherwise a nil OLD is an unchecked update."
  (let* ((head (ngit-read-ref repo "HEAD"))
         (ref (if (and head (string-prefix-p "ref: " head)) (substring head 5) "HEAD"))
         (file (ngit--ref-file repo ref))
         (lock (progn (ngit-make-directory (file-name-directory file) t) (ngit-lock file)))
         input head-lock)
    (unwind-protect
        (progn
          (unless (string= ref "HEAD")
            (setq head-lock (ngit-lock (ngit--ref-file repo "HEAD"))))
          ;; Check the expected value while both files are locked, before
          ;; invoking the prepared transaction hook.  A prior hook may have
          ;; moved the ref (including its packed value) since commit began.
          (aset repo 4 nil)
          (when (or old check-unborn)
            (let ((current (ngit-resolve-ref repo ref)))
              (unless (equal current old)
                (ngit--die "fatal: cannot lock ref '%s': is at %s but expected %s"
                           ref (or current (make-string 40 ?0))
                           (or old (make-string 40 ?0))))))
          (setq input
                (ngit-ref-prepare
                 repo (if (string= ref "HEAD") (list (list old new "HEAD"))
                        (list (list nil new "HEAD") (list old new ref)))
                 nil (delq nil (list lock head-lock))))
          ;; The files backend neither rewrites nor logs a ref that
          ;; already has the value; a symbolic HEAD still logs below.
          (if (equal (ngit-read-ref repo ref) new)
              (ngit-unlock lock)
            (ngit-commit-lock lock (concat new "\n")))
          (setq lock nil)
          (unless (equal old new)
            (ngit--append-reflog repo ref old new ident msg))
          (unless (string= ref "HEAD")
            (ngit--append-reflog repo "HEAD" old new ident msg))
          (when head-lock (ngit-unlock head-lock) (setq head-lock nil))
          (ngit-ref-finish repo input))
      (when lock (ngit-unlock lock))
      (when head-lock (ngit-unlock head-lock)))
    ref))

;;;; Summary

(defun ngit--pprint-rename (a b)
  (if (or (ngit--needs-quote-p a) (ngit--needs-quote-p b))
      (concat (ngit-quote-path a) " => " (ngit-quote-path b))
    (let* ((la (length a)) (lb (length b))
           (at (lambda (s i) (if (< i (length s)) (aref s i) 0)))
           (pfx 0) (i 0))
      (while (and (< i la) (< i lb) (= (aref a i) (aref b i)))
        (when (= (aref a i) ?/) (setq pfx (1+ i)))
        (setq i (1+ i)))
      (let* ((adj (if (> pfx 0) 1 0)) (oa la) (ob lb) (sfx 0))
        (while (and (<= (- pfx adj) oa) (<= (- pfx adj) ob)
                    (= (funcall at a oa) (funcall at b ob)))
          (when (= (funcall at a oa) ?/) (setq sfx (- la oa)))
          (setq oa (1- oa) ob (1- ob)))
        (let ((am (max 0 (- la pfx sfx))) (bm (max 0 (- lb pfx sfx))))
          (if (> (+ pfx sfx) 0)
              (concat (substring a 0 pfx) "{" (substring a pfx (+ pfx am)) " => "
                      (substring b pfx (+ pfx bm)) "}" (substring a (- la sfx)))
            (concat (substring a pfx (+ pfx am)) " => " (substring b pfx (+ pfx bm)))))))))

(defun ngit--count-lines (s)
  "Number of lines in S, a final unterminated line included."
  (if (< (length s) 4096)
      (let ((n 0) (i 0) (len (length s)))
        (while (< i len)
          (when (= (aref s i) ?\n) (setq n (1+ n)))
          (setq i (1+ i)))
        (if (and (> len 0) (/= (aref s (1- len)) ?\n)) (1+ n) n))
    ;; `count-lines' scans natively; an aref loop costs microseconds a byte.
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert s)
      (count-lines (point-min) (point-max)))))

(defun ngit--common-prefix-bytes (a b)
  "Length of the common prefix of A and B, found with native `string='."
  (let* ((n (min (length a) (length b))) (i 0) (blk 4096))
    (while (and (<= (+ i blk) n) (string= (substring a i (+ i blk)) (substring b i (+ i blk))))
      (setq i (+ i blk)))
    (let ((lo i) (hi (min n (+ i blk))))
      (while (< lo hi)
        (let ((mid (/ (+ lo hi 1) 2)))
          (if (string= (substring a i mid) (substring b i mid))
              (setq lo mid)
            (setq hi (1- mid)))))
      lo)))

(defun ngit--common-suffix-bytes (a b limit)
  "Length (at most LIMIT) of the common suffix of A and B."
  (let ((la (length a)) (lb (length b)) (i 0) (blk 4096))
    (while (and (<= (+ i blk) limit)
                (string= (substring a (- la i blk) (- la i)) (substring b (- lb i blk) (- lb i))))
      (setq i (+ i blk)))
    (let ((lo i) (hi (min limit (+ i blk))))
      (while (< lo hi)
        (let ((mid (/ (+ lo hi 1) 2)))
          (if (string= (substring a (- la mid) (- la i)) (substring b (- lb mid) (- lb i)))
              (setq lo mid)
            (setq hi (1- mid)))))
      lo)))

(defun ngit--count-changed-lines (old new)
  "Return (ADDED . DELETED) lines between OLD and NEW.
Identical leading and trailing lines are cut off first (xdiff trims them
too), so only the changed middle is split into lines, which costs
microseconds per byte in NeLisp."
  (let* ((p (ngit--common-prefix-bytes old new)))
    (while (and (> p 0) (/= (aref old (1- p)) ?\n)) (setq p (1- p)))
    (let* ((la (length old)) (lb (length new))
           (sfx (ngit--common-suffix-bytes old new (- (min la lb) p))))
      (while (and (> sfx 0)
                  (not (and (or (= (- la sfx) p) (= (aref old (- la sfx 1)) ?\n))
                            (or (= (- lb sfx) p) (= (aref new (- lb sfx 1)) ?\n)))))
        (setq sfx (1- sfx)))
      (let ((mo (substring old p (- la sfx))) (mn (substring new p (- lb sfx))))
        (cond ((and (string= mo "") (string= mn "")) (cons 0 0))
              ((string= mo "") (cons (ngit--count-lines mn) 0))
              ((string= mn "") (cons 0 (ngit--count-lines mo)))
              (t (ngit--count-changed-lines-xdiff mo mn)))))))

(defun ngit--count-changed-lines-xdiff (old new)
  "Return (ADDED . DELETED) lines between OLD and NEW, from xdiff."
  (let ((hunks (ngit-xdiff-unified old new 0)) (add 0) (del 0) (pos 0))
    (while (< pos (length hunks))
      (let ((c (aref hunks pos)))
        (cond ((= c ?+) (setq add (1+ add)))
              ((= c ?-) (setq del (1+ del)))))
      (setq pos (1+ (or (ngit--byte-pos hunks ?\n pos) (1- (length hunks))))))
    (cons add del)))

(defun ngit--commit-diffstat (repo old-tree new-tree)
  "Emit git's shortstat and summary from OLD-TREE (nil = empty) to NEW-TREE."
  (require 'ngit-rename)
  (let ((changes (ngit-diffcore-rename repo (ngit-diff-trees repo old-tree new-tree)))
        (files 0) (adds 0) (dels 0) (summary nil))
    (dolist (c changes)
      (let* ((path (aref c 0)) (status (aref c 1))
             (old-mode (aref c 2)) (new-mode (aref c 4)))
        (setq files (1+ files))
        (unless (and (= status ?R) (= (ngit-similarity-percent c) 100))
          (let ((old (ngit--diff-blob repo old-mode (aref c 3)))
                (new (ngit--diff-blob repo new-mode (aref c 5))))
            (unless (or (ngit--binary-p repo path old) (ngit--binary-p repo path new))
              (let ((r (ngit--count-changed-lines old new)))
                (setq adds (+ adds (car r)) dels (+ dels (cdr r)))))))
        (pcase status
          (?A (push (format " create mode %06o %s\n" new-mode (ngit-quote-path path)) summary))
          (?D (push (format " delete mode %06o %s\n" old-mode (ngit-quote-path path)) summary))
          (?R (push (format " rename %s (%d%%)\n" (ngit--pprint-rename (aref c 8) path)
                            (ngit-similarity-percent c)) summary)
              (unless (= old-mode new-mode)
                (push (format " mode change %06o => %06o\n" old-mode new-mode) summary)))
          (_ (unless (= old-mode new-mode)
               (push (format " mode change %06o => %06o %s\n" old-mode new-mode
                             (ngit-quote-path path))
                     summary))))))
    (when (> files 0)
      (ngit--emit (format (if (= files 1) " %d file changed" " %d files changed") files)
                  (if (or (> adds 0) (= dels 0))
                      (format (if (= adds 1) ", %d insertion(+)" ", %d insertions(+)") adds)
                    "")
                  (if (or (> dels 0) (= adds 0))
                      (format (if (= dels 1) ", %d deletion(-)" ", %d deletions(-)") dels)
                    "")
                  "\n")
      (apply #'ngit--emit (nreverse summary)))))

;;;; Refreshing the index

(defun ngit-refresh-entries (repo index entries checked)
  "Refresh the stat data of unchanged ENTRIES in place (git's
refresh_cache); mark up-to-date entries in CHECKED.  Return non-nil when
an entry was replaced."
  (let* ((filemode (ngit-config-bool repo "core.filemode" t))
         (raw (aref index 1)) (ts (aref index 4))
         (changed nil) (k 0) (n (length entries)))
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (while (< k n)
      (let* ((e (aref entries k)) (pos (aref e 4)) (mode (aref e 2)))
        (when (and pos (not (gethash (aref e 4) checked)) (= (aref e 3) 0)
                   (not (aref e 5)) (/= mode ngit-mode-gitlink))
          (let ((st (ngit-entry-lstat index e)))
            (when st
              (let ((wmode (ngit-wt-mode st)))
                (when (and (not filemode) (= (logand wmode #o170000) #o100000)
                           (= (logand mode #o170000) #o100000))
                  (setq wmode mode))
                (when (= wmode mode)
                  (let ((match (ngit--stat-match-p raw pos st)))
                    (cond
                     ((and match (not (<= ts (ngit--be32 raw (+ pos 8)))))
                      (puthash (aref e 4) t checked))
                     ((and (not match)
                           (/= (ngit--be32 raw (+ pos 36)) (logand (aref st 2) #xffffffff))
                           (/= (ngit--be32 raw (+ pos 36)) 0)))
                     ((equal (ngit-hash-blob
                              ;; Re-stat: the stat buffer feeds the new stat block.
                              (prog1 (ngit-wt-content repo (aref e 0) st) (ngit-entry-lstat index e)))
                             (aref e 1))
                      (if match
                          (puthash (aref e 4) t checked)
                        (let ((new (vector (aref e 0) (aref e 1) mode 0 nil nil
                                           (ngit-wt-stat-block mode))))
                          (aset entries k new)
                          (setq changed t))))))))))))
      (setq k (1+ k)))
    changed))

;;;; The command

(defun ngit--commit-tree-of (repo hex)
  (ngit-unhex (ngit-peel repo hex 'tree)))

(defun ngit--commit-check-unmerged (repo entries)
  "Refuse a commit of unmerged ENTRIES with git's refresh diagnostics."
  (let ((i 0) (n (length entries)) (unmerged nil))
    (while (< i n)
      (let ((e (aref entries i)))
        (if (= (aref e 3) 0)
            (setq i (1+ i))
          (setq unmerged t)
          (ngit--emit "U\t" (aref e 0) "\n")
          (setq i (car (ngit-index-group entries i))))))
    (when unmerged
      (ngit--die
       (concat "error: Committing is not possible because you have unmerged files.\n"
               (if (ngit-config-bool repo "advice.resolveConflict" t)
                   "hint: Fix them up in the work tree, and then use 'git add/rm <file>'\nhint: as appropriate to mark resolution and make a commit.\n"
                 "")
               "fatal: Exiting because of an unresolved conflict.")))))

(defun ngit-commit-clear-merge-state (repo)
  "Remove state files consumed by a completed or aborted merge."
  (dolist (name '("CHERRY_PICK_HEAD" "REVERT_HEAD"))
    (when (ngit-read-ref repo name) (ngit-ref-delete-state repo name)))
  (ngit-ref-delete-state repo "AUTO_MERGE")
  (dolist (name '("MERGE_HEAD" "MERGE_MSG" "MERGE_MODE" "MERGE_RR"
                  "SQUASH_MSG"))
    (let ((path (expand-file-name name (ngit-repo-gitdir repo))))
      (when (file-exists-p path) (delete-file path)))))

(defun ngit--commit-merge-editor (repo message)
  "Return (CLEANED . TEMPLATE) for a merge using an unchanged editor."
  (let* ((comment (or (ngit-config-get repo "core.commentchar") "#"))
         (clean nil) (ngit--out nil) (status nil))
    (dolist (line (split-string message "\n"))
      (unless (string-prefix-p comment line) (push line clean)))
    (ngit--status-long repo (ngit-status-collect repo))
    (dolist (line (split-string (apply #'concat (nreverse ngit--out)) "\n"))
      (unless (or (string-prefix-p "  (" line)
                  (string-prefix-p "no changes added" line))
        (push (concat comment
                      (if (or (string= line "") (= (aref line 0) ?\t)) "" " ")
                      line "\n") status)))
    ;; status has one empty split element after its final blank line.
    (setq status (nreverse status))
    (when (and status (equal (car (last status)) (concat comment "\n")))
      (setq status (butlast status)))
    (cons
     (ngit-stripspace (mapconcat #'identity (nreverse clean) "\n"))
     (concat message comment "\n"
             comment " It looks like you may be committing a merge.\n"
             comment " If this is not correct, please run\n"
             comment "\tgit update-ref -d MERGE_HEAD\n"
             comment " and try again.\n\n\n"
             comment " Please enter the commit message for your changes. Lines starting\n"
             comment " with '" comment "' will be ignored, and an empty message aborts the commit.\n"
             comment "\n" (apply #'concat status)))))

(defun ngit-cmd-commit (repo args)
  (let ((messages nil) (file nil) (all nil) (quiet nil) (allow-empty nil)
        (allow-empty-message nil) (amend nil) (no-edit nil)
        (no-verify nil) (no-post-rewrite nil) (reuse nil))
    (while args
      (let ((a (pop args)))
        (cond ((string= a "-m") (push (or (pop args) (ngit--die "error: switch `m' requires a value")) messages))
              ((string-prefix-p "--message=" a) (push (substring a 10) messages))
              ((and (string-prefix-p "-m" a) (> (length a) 2)) (push (substring a 2) messages))
              ((string= a "-F") (setq file (pop args)))
              ((string-prefix-p "--file=" a) (setq file (substring a 7)))
              ((string= a "-C") (setq reuse (or (pop args) (ngit--die "error: switch `C' requires a value"))))
              ((string-prefix-p "--reuse-message=" a) (setq reuse (substring a 16)))
              ((member a '("-a" "--all")) (setq all t))
              ((member a '("-q" "--quiet")) (setq quiet t))
              ((string= a "--allow-empty") (setq allow-empty t))
              ((string= a "--allow-empty-message") (setq allow-empty-message t))
              ((string= a "--amend") (setq amend t))
              ((string= a "--no-edit") (setq no-edit t))
              ((member a '("-n" "--no-verify")) (setq no-verify t))
              ((string= a "--no-post-rewrite") (setq no-post-rewrite t))
              ((string-prefix-p "-am" a) (setq all t) (push (substring a 3) messages))
              (t (ngit--die "ngit: commit: unsupported argument %s" a)))))
    (when (> (+ (if messages 1 0) (if file 1 0) (if reuse 1 0)) 1)
      (ngit--die "fatal: options '-m', '-F', and '-C' cannot be used together"))
    (unless all
      (when (aref (ngit-read-index repo) 9)
        (ngit--commit-check-unmerged repo (ngit-index-entries repo))))
    (let* ((head (ngit-resolve-ref repo "HEAD"))
           (pick-head (ngit-resolve-ref repo "CHERRY_PICK_HEAD"))
           (revert-head (ngit-resolve-ref repo "REVERT_HEAD"))
           (pick-parsed (and pick-head (ngit-parse-commit (cdr (ngit-read-object repo (ngit-unhex pick-head))))))
           (reuse-hex (and reuse (ngit-peel repo (ngit-rev-parse repo reuse) 'commit)))
           (reuse-data (and reuse-hex (cdr (ngit-read-object repo (ngit-unhex reuse-hex)))))
           (reuse-parsed (and reuse-data (ngit-parse-commit reuse-data)))
           (ngit-commit-replay-action (and pick-head "commit (cherry-pick)"))
           (ngit-commit-replay-date (or pick-head reuse-parsed))
           (merge-head (let ((path (expand-file-name "MERGE_HEAD" (ngit-repo-gitdir repo))))
                         (and (file-exists-p path) (ngit--read-file path))))
           (head-data (and head (cdr (ngit-read-object repo (ngit-unhex head)))))
           (parsed (and head-data (ngit-parse-commit head-data)))
           (parents (cond ((not head) nil)
                          (amend (ngit-header-all parsed "parent"))
                          (t (cons head (and merge-head (split-string merge-head "\n" t))))))
           (message (cond (messages
                           (mapconcat (lambda (m) (if (string-suffix-p "\n" m) m (concat m "\n")))
                                      (reverse messages) "\n"))
                          (file (ngit--read-file (expand-file-name file)))
                          (reuse-data (ngit--message-body reuse-data))
                          ((and amend head) (ngit--message-body head-data))
                          ((or merge-head pick-head revert-head)
                           (or (ngit--read-file (expand-file-name "MERGE_MSG" (ngit-repo-gitdir repo))) ""))
                          (no-edit
                           (let ((path (expand-file-name "SQUASH_MSG" (ngit-repo-gitdir repo))))
                             (if (file-exists-p path) (ngit--read-file path)
                               (ngit--die "ngit: no editor support; use -m or -F"))))
                          (t (ngit--die "ngit: no editor support; use -m or -F"))))
           (edit-message nil)
           (message-source (cond ((or messages file) "message")
                                 (reuse-data "commit")
                                 ((and amend head) "commit")
                                 ((file-exists-p (expand-file-name "SQUASH_MSG" (ngit-repo-gitdir repo))) "squash")
                                 ((or merge-head
                                      (file-exists-p (expand-file-name "MERGE_MSG" (ngit-repo-gitdir repo)))) "merge")
                                 ((or pick-head revert-head) "commit")
                                 (t nil)))
           (message-source-id (and (equal message-source "commit")
                                   (cond (reuse reuse)
                                         ((and amend head) "HEAD")
                                         (pick-head "CHERRY_PICK_HEAD")
                                         (revert-head "REVERT_HEAD"))))
           (message (ngit-stripspace (if (multibyte-string-p message)
                                         (encode-coding-string message 'utf-8)
                                       message))))
      (when (and amend (not head))
        (ngit--die "fatal: You have nothing to amend."))
      (when (and amend merge-head)
        (ngit--die "fatal: You are in the middle of a merge -- cannot amend."))
      (when (and merge-head (not no-edit) (not messages) (not file))
        (let ((edited (ngit--commit-merge-editor repo message)))
          (setq message (car edited) edit-message (cdr edited))))
      (ignore no-edit)
      (let* ((index (ngit-read-index repo))
             (entries (copy-sequence (aref index 0)))
             (root (or (ngit-ct-root index) (ngit-ct-new)))
             (checked (make-hash-table :test 'eql))
             (index-file (ngit-index-file repo))
             (message-file (expand-file-name "COMMIT_EDITMSG" (ngit-repo-gitdir repo)))
             (committer nil) (author nil)
             (lock nil))
        (when all
          (ngit-add-tracked repo index entries '("") root checked #'ignore nil)
          (setq entries (vconcat (delq nil (append entries nil))))
          (ngit--commit-check-unmerged repo entries))
        (let* ((refreshed (ngit-refresh-entries repo index entries checked))
               (valid (ngit-ct-fully-valid-p root))
               (bytes (and (or all refreshed (not valid))
                           (progn
                             (ngit-ct-update repo root entries)
                             (ngit-index-bytes repo index entries root checked)))))
          (when bytes
            (setq lock (ngit-lock index-file))
            (ngit-write-file lock bytes)
            (unless all
              (rename-file lock index-file t)
              (setq lock nil)))
          (aset repo 6 nil)
          ;; Git establishes the author environment before pre-commit.
          (setq author (cond (amend (ngit-header parsed "author"))
                             (reuse-parsed (ngit-header reuse-parsed "author"))
                             (pick-parsed (ngit-header pick-parsed "author"))
                             (t
                         (condition-case err (ngit-ident repo "AUTHOR")
                           (error (when lock (ngit-unlock lock)) (signal (car err) (cdr err)))))))
          ;; determine_author_info exports these for every later hook, including
          ;; reference-transaction hooks reached through the ref update.  Avoid
          ;; rebuilding the process environment when no hook directory exists.
          (when (aref (ngit--hook-root-info repo) 0)
            (dolist (entry (ngit-commit-author-env author))
              (let ((eq (string-search "=" entry)))
                (ngit-hook-setenv (substring entry 0 eq) (substring entry (1+ eq))))))
          (let* ((hook-index (or lock index-file))
                 (pre-ran (and (not no-verify)
                               (ngit-hook-exists-p repo "pre-commit"))))
            (when (and pre-ran
                       (/= (ngit-commit-run-hook repo "pre-commit" hook-index author) 0))
              (when lock (ngit-unlock lock))
              (ngit--die-with 1 ""))
            (when pre-ran
              (setq index (ngit-commit-read-hook-index repo hook-index)
                    entries (aref index 0)
                    root (or (ngit-ct-root index) (ngit-ct-new))
                    checked (make-hash-table :test 'eql))))
          (ngit-write-file message-file (or edit-message message))
          ;; The strict committer check follows the message file.
          (setq committer (condition-case err (ngit-ident repo "COMMITTER")
                            (error (when lock (ngit-unlock lock)) (signal (car err) (cdr err)))))
          (let* ((tree (ngit-ct-update repo root entries))
                 (committable
                 (if (cdr parents) t
                  (if (null parents)
                     (seq-some (lambda (e) (not (memq 'ita (aref e 5)))) entries)
                   (not (equal tree (ngit--commit-tree-of repo (car parents))))))))
            (if (and (not committable) (not allow-empty))
                (let ((saved (getenv "GIT_INDEX_FILE")))
                  (unwind-protect
                      (progn
                        (when lock (setenv "GIT_INDEX_FILE" lock))
                        (ngit-cmd-status repo nil))
                    (setenv "GIT_INDEX_FILE" saved)
                    (when lock (ngit-unlock lock)))
                  1)
              (progn
                (when (/= (ngit-commit-run-hook
                           repo "prepare-commit-msg" (or lock index-file) author
                           (delq nil (list message-file message-source message-source-id))) 0)
                  (when lock (ngit-unlock lock))
                  (ngit--die-with 1 ""))
                (when (and (not no-verify)
                           (/= (ngit-commit-run-hook repo "commit-msg" (or lock index-file)
                                                    author (list message-file)) 0))
                  (when lock (ngit-unlock lock))
                  (ngit--die-with 1 ""))
                (setq message
                      (ngit-commit-clean-hook-message
                       repo (ngit--read-file message-file) (and edit-message t)))
              (if (and (string= message "") (not allow-empty-message))
                  (progn
                    (when lock (ngit-unlock lock))
                    (ngit-native-write-stderr "Aborting commit due to empty commit message.\n")
                    1)
                (condition-case err
                    (ngit--finish-commit repo head parents tree message amend quiet lock
                                         index-file committer author no-post-rewrite)
                  (error (when lock (ngit-unlock lock))
                         (signal (car err) (cdr err)))))))))))))

(defvar ngit-commit-replay-action nil)
(defvar ngit-commit-replay-date nil)

(defun ngit--finish-commit (repo head parents tree message amend quiet lock index-file
                                   committer author &optional no-post-rewrite no-post-commit
                                   no-hook-author no-state-cleanup)
  (let* ((body (concat "tree " (ngit-hex tree) "\n"
                       (mapconcat (lambda (p) (concat "parent " p "\n")) parents "")
                       "author " author "\n"
                       "committer " committer "\n\n"
                       message))
         (hex (ngit-hex (ngit-write-object repo "commit" body)))
         (first-line (substring message 0 (or (ngit--byte-pos message ?\n 0) (length message))))
         (ref (ngit-update-head repo hex head committer
                                (concat (or ngit-commit-replay-action
                                            (cond (amend "commit (amend)")
                                              ((null head) "commit (initial)")
                                              ((cdr parents) "commit (merge)")
                                              (t "commit")))
                                        ": " first-line) t)))
    (when lock (rename-file lock index-file t))
    (unless no-state-cleanup (ngit-commit-clear-merge-state repo))
    (unless no-post-commit
      (ngit-commit-run-hook repo "post-commit" index-file
                            (unless no-hook-author author)))
    (when (and amend (not no-post-rewrite))
      (ngit-hook-run repo "post-rewrite" '("amend")
                     (concat head " " hex "\n")
                     (ngit-commit-author-env author)))
    (unless quiet
      (let ((a (ngit--split-ident author)) (c (ngit--split-ident committer)))
        (ngit--emit "[" (if (string-prefix-p "refs/heads/" ref) (substring ref 11)
                          (if (string= ref "HEAD") "detached HEAD" ref))
                    (if (null head) " (root-commit)" "")
                    " " (ngit-abbrev repo hex) "] "
                    (decode-coding-string (ngit--subject message) 'utf-8) "\n")
        (unless (string= (car a) (car c))
          (ngit--emit " Author: " (decode-coding-string (car a) 'utf-8) "\n"))
        (when (or amend ngit-commit-replay-date)
          (ngit--emit " Date: " (ngit-format-date (nth 1 a) (nth 2 a)) "\n")))
      (unless (cdr parents)
        (ngit--commit-diffstat repo (and parents (ngit--commit-tree-of repo (car parents))) tree)))
    0))

(provide 'ngit-commit)

;;; ngit-commit.el ends here
