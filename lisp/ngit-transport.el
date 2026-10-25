;;; ngit-transport.el --- smart Git transport client -*- lexical-binding: t; -*-

;;; Commentary:
;; The installed Git supplies only upload-pack and receive-pack.  Requests,
;; negotiation, object installation and ref transactions belong to this client.

;;; Code:

(require 'ngit-pack)
(require 'ngit-transport-index)
(require 'ngit-transport-checkout)
(require 'ngit-branch)
(require 'ngit-switch)
(require 'ngit-transport-process)
(require 'ngit-remote)

(defconst ngit-tr--zero "0000000000000000000000000000000000000000")
(defvar ngit-tr--action "fetch")
(defvar ngit-tr--merge-heads nil)
(defvar ngit-tr--pull-rebase-mode nil
  "Special pull mode whose ancestry must be checked before publishing fetch.")

(defun ngit-tr--pkt (text)
  (concat (format "%04x" (+ 4 (length text))) text))

(defun ngit-tr--read (conn &optional pack-fd)
  "Read one packet, returning a payload, flush, delim or response-end."
  (let ((header (ngit-transport-read conn 4)))
    (unless header (ngit--die "fatal: the remote end hung up unexpectedly"))
    (unless (and (= (length header) 4) (ngit-hex-p header 4))
      (ngit--die "fatal: protocol error: bad line length character: %.4s" header))
    (let ((n (string-to-number header 16)))
      (cond ((= n 0) 'flush) ((= n 1) 'delim) ((= n 2) 'response-end)
            ((or (< n 4) (> n 65520)) (ngit--die "fatal: protocol error: bad packet length %d" n))
            (t (let ((line
                      (if (and pack-fd (> n 4))
                          (let ((channel (ngit-transport-read conn 1)))
                            (unless channel (ngit--die "fatal: early EOF"))
                            (if (= (aref channel 0) 1)
                                (progn (ngit-transport-copy conn pack-fd (- n 5)) 'pack-data)
                              (concat channel (or (ngit-transport-read conn (- n 5))
                                                  (ngit--die "fatal: early EOF")))))
                        (ngit-transport-read conn (- n 4)))))
                 (unless line (ngit--die "fatal: early EOF"))
                 (when (and (stringp line) (string-prefix-p "ERR " line))
                   (ngit--die "fatal: remote error: %s" (string-trim-right (substring line 4))))
                 line))))))

(defun ngit-tr--lines (conn)
  (let (packet lines)
    (while (stringp (setq packet (ngit-tr--read conn)))
      (push (ngit--trim-eol packet) lines))
    (nreverse lines)))

(defun ngit-tr--display-remote-output (data)
  "Write receive-pack sideband DATA with Git's `remote: ' prefix."
  (let ((suffix (if (and (ngit-transport-tty-p)
                         (getenv "TERM") (not (equal (getenv "TERM") "dumb")))
                    "\33[K" "        "))
        (pos 0) (size (length data)) end)
    (while (< pos size)
      (let ((lf (string-search "\n" data pos))
            (cr (string-search "\r" data pos)))
        (setq end (if lf (if cr (min lf cr) lf) cr)))
      (ngit-native-write-stderr
       (concat "remote: " (substring data pos end)
               (if (and end (> end pos)) suffix "")
               (if end (substring data end (1+ end)) "\n")))
      (setq pos (if end (1+ end) size)))))

(defun ngit-tr--packet-lines-string (data)
  "Decode nested pkt-lines from receive-pack primary sideband DATA."
  (let ((pos 0) (n (length data)) lines done)
    (while (and (not done) (<= (+ pos 4) n))
      (let* ((header (substring data pos (+ pos 4)))
             (size (and (ngit-hex-p header 4) (string-to-number header 16))))
        (unless size (ngit--die "fatal: protocol error: bad line length character: %.4s" header))
        (cond
         ((= size 0) (setq done t pos (+ pos 4)))
         ((or (< size 4) (> (+ pos size) n))
          (ngit--die "fatal: protocol error: bad receive-pack status"))
         (t
          (push (ngit--trim-eol (substring data (+ pos 4) (+ pos size))) lines)
          (setq pos (+ pos size))))))
    (unless (or done (= pos n))
      (ngit--die "fatal: protocol error: truncated receive-pack status"))
    (nreverse lines)))

(defun ngit-tr--push-report (conn sideband)
  "Read a push status report, forwarding receive-pack SIDEBAND output."
  (if (not sideband) (ngit-tr--lines conn)
    (let (packet primary remote done)
      (while (not done)
        (setq packet (ngit-tr--read conn))
        (cond
         ((memq packet '(flush response-end)) (setq done t))
         ((eq packet 'delim) nil)
         ((stringp packet)
          (let ((channel (aref packet 0)) (payload (substring packet 1)))
            (cond ((= channel 1) (push payload primary))
                  ((= channel 2) (push payload remote))
                  ((= channel 3) (ngit--die "fatal: %s" (ngit--trim-eol payload)))
                  (t (ngit--die "fatal: protocol error: bad sideband channel")))))))
      (when remote
        (ngit-tr--display-remote-output (apply #'concat (nreverse remote))))
      (ngit-tr--packet-lines-string (apply #'concat (nreverse primary))))))

(defun ngit-tr--v0-refs (lines)
  (let (refs caps)
    (dolist (line lines)
      (let ((nul (string-search "\0" line)))
        (when nul (setq caps (split-string (substring line (1+ nul)) " " t)
                        line (substring line 0 nul)))
        (when (and (> (length line) 41) (ngit-hex-p (substring line 0 40) 40)
                   (not (string= (substring line 0 40) ngit-tr--zero)))
          (push (list (substring line 41) (substring line 0 40)) refs))))
    (setq refs (nreverse refs))
    (dolist (ref refs)
      (when (string-suffix-p "^{}" (car ref))
        (let ((base (assoc (substring (car ref) 0 -3) refs)))
          (when base (setcdr (cdr base) (list nil (nth 1 ref)))))))
    (setq refs (seq-remove (lambda (ref) (string-suffix-p "^{}" (car ref))) refs))
    (dolist (cap caps)
      (when (string-prefix-p "symref=HEAD:" cap)
        (let ((head (assoc "HEAD" refs)))
          (when head (setcdr (cdr head) (list (substring cap 12)))))))
    (list refs caps)))

(defun ngit-tr--advertise (conn &optional receive)
  "Return (REFS CAPABILITIES VERSION); refs are (NAME OID SYMREF PEELED)."
  (let ((lines (ngit-tr--lines conn)))
    (if (and (not receive) (equal (car lines) "version 2"))
        (let ((caps (cdr lines)) refs)
          (ngit-transport-write
           conn (concat (ngit-tr--pkt "command=ls-refs\n") "0001"
                        (ngit-tr--pkt "peel\n") (ngit-tr--pkt "symrefs\n")
                        (if (member "ls-refs=unborn" caps) (ngit-tr--pkt "unborn\n") "") "0000"))
          (dolist (line (ngit-tr--lines conn))
            (let* ((fields (split-string line " " t))
                   (oid (pop fields)) (name (pop fields)) sym peeled)
              (unless (or (equal oid "unborn") (ngit-hex-p oid 40))
                (ngit--die "fatal: protocol error: invalid advertised object name"))
              (dolist (field fields)
                (cond ((string-prefix-p "symref-target:" field) (setq sym (substring field 14)))
                      ((string-prefix-p "peeled:" field) (setq peeled (substring field 7)))))
              (push (list name (unless (equal oid "unborn") oid) sym peeled) refs)))
          (list (nreverse refs) caps 2))
      (when (equal (car lines) "version 1") (setq lines (cdr lines)))
      (append (ngit-tr--v0-refs lines) (list 0)))))

(defun ngit-tr--short (ref)
  (cond ((string-prefix-p "refs/heads/" ref) (substring ref 11))
        ((string-prefix-p "refs/tags/" ref) (substring ref 10))
        ((string-prefix-p "refs/remotes/" ref) (substring ref 13)) (t ref)))

(defun ngit-tr--url (repo remote &optional push)
  (or (and push (ngit-config-get repo (concat "remote." remote ".pushurl")))
      (ngit-config-get repo (concat "remote." remote ".url")) remote))

(defun ngit-tr--display-url (url)
  "Remove HTTP credentials before writing URL to a diagnostic."
  (replace-regexp-in-string "\\`\\(https?://\\)[^/@]+@" "\\1" url))

(defun ngit-tr--branch (repo)
  (let ((head (ngit-read-ref repo "HEAD")))
    (and head (string-prefix-p "ref: refs/heads/" head) (substring head 16))))

(defun ngit-tr--default-remote (repo)
  (or (and (ngit-tr--branch repo)
           (ngit-config-get repo (concat "branch." (ngit-tr--branch repo) ".remote"))) "origin"))

(defun ngit-tr--config-all (repo key)
  (let (values)
    (dolist (entry (ngit-config-entries repo))
      (when (equal (car entry) key) (push (cdr entry) values)))
    (nreverse values)))

(defun ngit-tr--match (pattern name)
  "Return the wildcard capture, t for an exact match, or nil."
  (let ((star (string-search "*" pattern)))
    (if star
        (let ((prefix (substring pattern 0 star)) (suffix (substring pattern (1+ star))))
          (and (>= (length name) (+ (length prefix) (length suffix)))
               (string-prefix-p prefix name) (string-suffix-p suffix name)
               (substring name (length prefix) (- (length name) (length suffix)))))
      (and (equal pattern name) t))))

(defun ngit-tr--expand (pattern capture)
  (let ((star (string-search "*" pattern)))
    (if star (concat (substring pattern 0 star) capture (substring pattern (1+ star))) pattern)))

(defun ngit-tr--refspec (spec)
  (let* ((force (string-prefix-p "+" spec))
         (spec (if force (substring spec 1) spec)) (colon (string-search ":" spec)))
    (list (if colon (substring spec 0 colon) spec)
          (and colon (substring spec (1+ colon))) force)))

(defun ngit-tr--find (refs name)
  (or (assoc name refs) (assoc (concat "refs/heads/" name) refs)
      (assoc (concat "refs/tags/" name) refs)))

(defun ngit-tr--safe-ref (ref)
  (unless (and (string-prefix-p "refs/" ref) (ngit--valid-branch-name-p (substring ref 5)))
    (ngit--die "fatal: invalid refspec destination '%s'" ref))
  ref)

(defun ngit-tr--install-pack (repo path &optional check)
  (let* ((dir (expand-file-name "objects/pack/" (ngit-repo-commondir repo)))
         (idx (concat path ".idx"))
         (indexed (ngit-transport-index-pack path idx))
         (base (concat dir "pack-" (car indexed))))
    (when check
      ;; Read the received objects alongside the original store without
      ;; installing anything until pull knows that no replay is required.
      (let ((checked (copy-sequence repo)) (pack (ngit--open-pack idx))
            (ngit--pack-order nil) (ngit--rv-repo ngit--rv-repo))
        (aset pack 0 path)
        (aset checked 3 (cons pack (ngit-repo-packs repo)))
        (unwind-protect (funcall check checked)
          (ngit-native-close idx))))
    (make-directory dir t)
    (ngit-native-close path)
    (rename-file path (concat base ".pack") t)
    (rename-file idx (concat base ".idx") t)
    (aset repo 3 nil)
    (clrhash ngit-native--sizes)))

(defun ngit-tr--receive (repo conn adv wants &optional check)
  "Fetch WANTS using complete packs; omitted thin-pack avoids external bases."
  (let ((seen (make-hash-table :test 'equal)) needed)
    (dolist (oid wants)
      (when (and oid (not (gethash oid seen))
                 (not (ngit-object-exists-p repo (ngit-unhex oid))))
        (puthash oid t seen) (push oid needed)))
    (if (null needed) (when check (funcall check repo))
      (let* ((v2 (= (nth 2 adv) 2)) (parts nil)
             (progress (ngit-transport-tty-p)) (first t)
             (tmp (if check (make-temp-file "ngit-pull-")
                    (expand-file-name (concat "objects/pack/incoming-" (number-to-string (emacs-pid)))
                                      (ngit-repo-commondir repo)))) fd)
        (when v2 (push (ngit-tr--pkt "command=fetch\n") parts) (push "0001" parts))
        (dolist (oid (nreverse needed))
          (push (ngit-tr--pkt (concat "want " oid
                                     (if (or v2 (not first)) "" " side-band-64k ofs-delta no-progress include-tag") "\n")) parts)
          (setq first nil))
        (when v2
          (push (ngit-tr--pkt "ofs-delta\n") parts)
          (push (ngit-tr--pkt "include-tag\n") parts)
          (unless progress (push (ngit-tr--pkt "no-progress\n") parts)))
        (unless v2 (push "0000" parts))
        ;; Ref tips are enough to avoid sending the existing history again.
        (dolist (ref (ngit-list-refs repo))
          (push (ngit-tr--pkt (concat "have " (nth 1 ref) "\n")) parts))
        (push (ngit-tr--pkt "done\n") parts)
        (when v2 (push "0000" parts))
        (ngit-transport-write conn (apply #'concat (nreverse parts)))
        (make-directory (file-name-directory tmp) t)
        (setq fd (ngit-pack--open-output tmp))
        (unwind-protect
            (let ((in-pack (not v2)) packet finished)
              (while (not finished)
                (setq packet (ngit-tr--read conn (and in-pack fd)))
                (cond
                 ((eq packet 'pack-data) nil)
                 ((memq packet '(flush response-end)) (setq finished t))
                 ((eq packet 'delim) nil)
                 ((equal packet "packfile\n") (setq in-pack t))
                 (in-pack
                  (let ((channel (aref packet 0)))
                    (cond ((= channel 1) (ngit-pack--write-bytes fd (substring packet 1)))
                          ((= channel 2) (when progress (ngit-native-write-stderr (concat "remote: " (substring packet 1)))))
                          ((= channel 3) (ngit--die "fatal: %s" (substring packet 1)))
                          ((and (not v2) (or (string-prefix-p "NAK" packet) (string-prefix-p "ACK " packet))) nil)
                          (t (ngit--die "fatal: protocol error in pack stream")))))))
              (syscall-direct 3 fd 0 0 0 0 0) (setq fd nil)
              (ngit-tr--install-pack repo tmp check))
          (when fd (syscall-direct 3 fd 0 0 0 0 0))
          (when check
            (ngit-native-close tmp)
            (when (file-exists-p (concat tmp ".idx")) (delete-file (concat tmp ".idx"))))
          (when (file-exists-p tmp) (delete-file tmp)))))))

(defun ngit-cmd-ls-remote (repo args)
  (let (heads tags symrefs refs-only quiet exit-code names remote sort-key)
    (while args
      (let ((a (pop args)))
        (cond ((member a '("--heads" "-h" "--branches")) (setq heads t))
              ((member a '("--tags" "-t")) (setq tags t))
              ((equal a "--refs") (setq refs-only t))
              ((equal a "--symref") (setq symrefs t))
              ((equal a "--exit-code") (setq exit-code t))
              ((member a '("-q" "--quiet")) (setq quiet t))
              ((equal a "--get-url") (setq sort-key 'url))
              ((string-prefix-p "--sort=" a) nil)
              ((string-prefix-p "-" a) (ngit--die-with 129 "error: unknown option `%s'" a))
              ((null remote) (setq remote a)) (t (push a names)))))
    (let* ((implicit (null remote)) (remote (or remote (if repo (ngit-tr--default-remote repo) "origin")))
           (url (ngit-tr--url repo remote)))
      (if (eq sort-key 'url) (progn (ngit--emit url "\n") 0)
        (let ((conn (ngit-transport-open repo url "git-upload-pack")) (count 0))
          (unwind-protect
              (let* ((adv (ngit-tr--advertise conn)) (refs (car adv)))
                (when (and implicit (not quiet)) (ngit-native-write-stderr (concat "From " (ngit-tr--display-url url) "\n")))
                (dolist (ref refs)
                  (let ((name (car ref)) (oid (nth 1 ref)))
                    (when (and oid (or (not (or heads tags))
                                       (and heads (string-prefix-p "refs/heads/" name))
                                       (and tags (string-prefix-p "refs/tags/" name)))
                               (or (not refs-only) (string-prefix-p "refs/" name))
                               (or (null names)
                                   (seq-some (lambda (pattern)
                                               (or (ngit-wildmatch pattern name)
                                                   (ngit-wildmatch (concat "*/" pattern) name))) names)))
                      (setq count (1+ count))
                      (when (and symrefs (nth 2 ref)) (ngit--emit "ref: " (nth 2 ref) "\t" name "\n"))
                      (ngit--emit oid "\t" name "\n")
                      (when (and (nth 3 ref) (not refs-only)) (ngit--emit (nth 3 ref) "\t" name "^{}\n")))))
                (if (and exit-code (= count 0)) 2 0))
            (ngit-transport-finish-write conn)
            (ngit-transport-close conn)))))))

(defun ngit-cmd-clone (_repo args)
  (let (bare branch no-checkout quiet source dest template (origin "origin"))
    (while args
      (let ((a (pop args)))
        (cond ((equal a "--bare") (setq bare t))
              ((member a '("-b" "--branch")) (setq branch (pop args)))
              ((string-prefix-p "--branch=" a) (setq branch (substring a 9)))
              ((member a '("-o" "--origin")) (setq origin (pop args)))
              ((string-prefix-p "--origin=" a) (setq origin (substring a 9)))
              ((member a '("-n" "--no-checkout")) (setq no-checkout t))
              ((member a '("-q" "--quiet")) (setq quiet t))
              ((equal a "--template")
               (unless args (ngit--die-with 129 "error: option `template' requires a value"))
               (setq template (pop args)))
              ((string-prefix-p "--template=" a) (setq template (substring a 11)))
              ((member a '("--no-local" "--no-hardlinks")) nil)
              ((string-prefix-p "-" a) (ngit--die-with 129 "error: unknown option `%s'" a))
              ((null source) (setq source a)) ((null dest) (setq dest a))
              (t (ngit--die-with 129 "fatal: Too many arguments.")))))
    (unless source (ngit--die-with 129 "fatal: You must specify a repository to clone."))
    (unless dest
      (setq dest (file-name-nondirectory (directory-file-name source)))
      ;; Git treats colons as separators when guessing a clone directory,
      ;; including relative scp paths such as host:repository.git.
      (setq dest (car (last (split-string dest ":"))))
      (when (string-suffix-p ".git" dest) (setq dest (substring dest 0 -4)))
      (when bare (setq dest (concat dest ".git"))))
    (when (and (file-exists-p dest)
               (or (not (file-directory-p dest)) (> (length (directory-files dest)) 2)))
      (ngit--die "fatal: destination path '%s' already exists and is not an empty directory." dest))
    (unless quiet (ngit-native-write-stderr (concat "Cloning into " (if bare "bare repository " "") "'" dest "'...\n")))
    (let* ((url (if (or (string-search ":" source) (string-prefix-p "/" source)) source (expand-file-name source)))
           (existed (file-directory-p dest))
           (ngit-ref-hook-env
            (list (concat "GIT_DIR=" (directory-file-name
                                     (expand-file-name (if bare dest (concat dest "/.git")))))
                  "GIT_PREFIX"))
           (ngit-ref-hook-cwd default-directory)
           repo conn success (hook-status 0))
      (unwind-protect
          (progn
            (setq repo (let ((ngit-ref-inhibit-hooks t))
                         (ngit-init-repository dest bare nil template))
                  conn (ngit-transport-open repo url "git-upload-pack"))
            (let* ((adv (ngit-tr--advertise conn)) (refs (car adv))
                 (head (assoc "HEAD" refs))
                 (target (if branch (ngit-tr--find refs branch) (and (nth 2 head) (assoc (nth 2 head) refs))))
                 (oid (or (nth 1 target) (nth 1 head)))
                 (head-ref (and target (string-prefix-p "refs/heads/" (car target)) (car target)))
                 (msg (concat "clone: from " (ngit-tr--display-url url))) wants packed)
            (when (and branch (null target)) (ngit--die "fatal: Remote branch %s not found in upstream %s" branch origin))
            (dolist (ref refs)
              (when (and (nth 1 ref) (string-prefix-p "refs/" (car ref)) (not (string-suffix-p "^{}" (car ref))))
                (push (nth 1 ref) wants)
                (let ((name (car ref)))
                  (when (or (string-prefix-p "refs/heads/" name) (string-prefix-p "refs/tags/" name))
                    (setq name (if (and (not bare) (string-prefix-p "refs/heads/" name))
                                   (concat "refs/remotes/" origin "/" (substring name 11)) name))
                    (ngit-tr--safe-ref name)
                    (push (cons name (concat (nth 1 ref) " " name "\n" (if (nth 3 ref) (concat "^" (nth 3 ref) "\n") ""))) packed)))))
            (ngit-tr--receive repo conn adv wants)
            ;; Check only on the normal path; cleanup must preserve an
            ;; earlier protocol error.  Do not publish refs or run checkout
            ;; hooks after a failed transport.
            (ngit-transport-finish-write conn)
            (unless (= 0 (ngit-transport-close conn))
              (ngit--die "fatal: remote transport failed"))
            (when (and oid (null head-ref)) (setq oid (ngit-peel repo oid 'commit)))
            (when (and oid (not quiet) (not (string-search ":" source)))
              (ngit-native-write-stderr "done.\n"))
            (ngit-config-set repo (concat "remote." origin ".url") url)
            (unless bare (ngit-config-set repo (concat "remote." origin ".fetch") (concat "+refs/heads/*:refs/remotes/" origin "/*")))
            (when packed
              (setq packed (sort packed (lambda (a b) (string< (car a) (car b)))))
              (let ((lock (ngit-lock (expand-file-name "packed-refs" (ngit-repo-gitdir repo)))) input)
                (unwind-protect
                    (progn
                      (setq input
                            (ngit-ref-prepare
                             repo (mapcar (lambda (ref)
                                            (list nil (substring (cdr ref) 0 40) (car ref)))
                                          packed)
                             nil (list lock)))
                      (ngit-commit-lock lock
                                        (concat "# pack-refs with: peeled fully-peeled sorted \n"
                                                (mapconcat #'cdr packed "")))
                      (setq lock nil)
                      (ngit-ref-finish repo input))
                  (when lock (ngit-unlock lock)))))
            (aset repo 4 nil)
            (when (and (not bare) (nth 2 head) (nth 1 head))
              (let ((name (concat "refs/remotes/" origin "/HEAD"))
                    (target-name (concat "refs/remotes/" origin "/" (substring (nth 2 head) 11))))
                (ngit-ref-write repo name (concat "ref: " target-name))
                (ngit--append-reflog repo name nil (nth 1 head) (ngit--branch-ident repo) msg)))
            (cond
             ((null oid)
              (when (nth 2 head)
                (ngit-ref-write repo "HEAD" (concat "ref: " (nth 2 head)))
                (unless bare
                  (ngit--install-tracking repo (substring (nth 2 head) 11) origin (nth 2 head) t)))
              (unless quiet
                (ngit-native-write-stderr
                 (if wants "warning: remote HEAD refers to nonexistent ref, unable to checkout\n"
                   "warning: You appear to have cloned an empty repository.\n"))))
             (head-ref
              (ngit-ref-write repo "HEAD" (concat "ref: " head-ref))
              (unless bare
                (ngit-update-head repo oid nil (ngit--branch-ident repo) msg)
                (ngit--install-tracking repo (substring head-ref 11) origin head-ref t)))
             (t (ngit-ref-write repo "HEAD" oid)
                (when (and (not bare) (not quiet) (ngit-config-bool repo "advice.detachedHead" t))
                  (ngit--switch-detach-advice oid))
                (unless bare (ngit--append-reflog repo "HEAD" nil oid (ngit--branch-ident repo) msg))))
            (let ((checkout-oid (and oid (ngit-peel repo oid 'commit))))
              (when (and (not bare) (not no-checkout) checkout-oid)
                (ngit-transport-checkout repo (ngit--switch-tree repo checkout-oid))
                (setq hook-status
                      (ngit-hook-run
                       repo "post-checkout" (list ngit-tr--zero checkout-oid "1") nil
                       (list (concat "GIT_DIR="
                                     (directory-file-name
                                      (expand-file-name (ngit-repo-gitdir repo))))
                             "GIT_PREFIX" "GIT_INDEX_FILE")))))
            (when (and (null oid) (not quiet) (not (string-search ":" source)))
              (ngit-native-write-stderr "done.\n"))
            (setq success t)
            hook-status))
        (when conn
          (ngit-transport-finish-write conn)
          (ngit-transport-close conn))
        (unless success
          (when (file-directory-p dest) (delete-directory dest t))
          (when existed (make-directory dest)))))))

(defun ngit-tr--fetch-map (refs specs tracking explicit tags)
  "Return (SOURCE OID DEST FORCE MERGE IGNORE-HEAD) entries in fetch order."
  (let (out seen)
    (dolist (spec specs)
      (let* ((parsed (ngit-tr--refspec spec)) (src (car parsed)) (dst (nth 1 parsed))
             (force (nth 2 parsed)) (wild (string-search "*" src)) (found nil))
        (dolist (ref refs)
          (let* ((name (car ref))
                 (capture (if wild (ngit-tr--match src name)
                            (and (equal ref (ngit-tr--find refs src)) t))))
            (when (and capture (nth 1 ref) (not (string-suffix-p "^{}" name)))
              (setq found t)
              (unless (member name seen) (push name seen))
              (let ((dest (and dst (ngit-tr--expand dst capture))))
                (when dest (ngit-tr--safe-ref dest))
                (push (list name (nth 1 ref) dest force (and explicit (not wild))) out)))))
        (unless (or found wild) (ngit--die "fatal: couldn't find remote ref %s" src))))
    (when tags
      (dolist (ref refs)
        (when (and (string-prefix-p "refs/tags/" (car ref)) (not (string-suffix-p "^{}" (car ref)))
                   (not (member (car ref) seen)))
          (push (list (car ref) (nth 1 ref) (car ref) nil nil) out))))
    ;; Explicit refspecs still update matching configured remote-tracking
    ;; refs.  Git appends these after explicit and tag mappings, displays and
    ;; stores them, but excludes them from FETCH_HEAD and merge candidates.
    (when explicit
      (dolist (name (nreverse (copy-sequence seen)))
        (let ((ref (assoc name refs)))
          (dolist (track tracking)
            (let* ((p (ngit-tr--refspec track))
                   (capture (ngit-tr--match (car p) name))
                   (dest (and capture (nth 1 p)
                              (ngit-tr--expand (nth 1 p) capture))))
              (when (and dest
                         (not (seq-some
                               (lambda (item)
                                 (and (equal (car item) name)
                                      (equal (nth 2 item) dest)))
                               out)))
                (ngit-tr--safe-ref dest)
                (push (list name (nth 1 ref) dest (nth 2 p) nil t) out)))))))
    (setq out (nreverse out))
    (let ((destinations (make-hash-table :test 'equal)) result)
      (dolist (item out)
        (let* ((src (car item)) (dest (nth 2 item))
               (prior (and dest (gethash dest destinations))))
          (cond
           ((null dest) (push item result))
           ((null prior)
            (puthash dest item destinations)
            (push item result))
           ((equal src (car prior)) nil)
           ((and (nth 5 item) (not (nth 5 prior)))
            (ngit-native-write-stderr
             (format "warning: %s usually tracks %s, not %s\n"
                     dest src (car prior))))
           ((and (nth 5 item) (nth 5 prior))
            (ngit--die "fatal: %s tracks both %s and %s"
                       dest (car prior) src))
           (t
            (ngit--die "fatal: Cannot fetch both %s and %s to %s"
                       (car prior) src dest)))))
      (nreverse result))))

(defun ngit-tr--ff-p (repo old new)
  (and old new
       (condition-case nil
           (ngit--in-merge-bases repo (ngit-commit (ngit-peel repo old 'commit))
                                 (ngit-commit (ngit-peel repo new 'commit)))
         (error nil))))

(defun ngit-tr--merge-candidates (mapping explicit merge &optional duplicates)
  "Select fetch merge candidates, deduplicating sources unless DUPLICATES."
  (let ((seen (make-hash-table :test 'equal)) incoming)
    (dolist (item mapping)
      (when (and (not (nth 5 item))
                 (or (nth 4 item) (and (not explicit) (equal (car item) merge))
                     (and (not explicit) (equal (car item) "HEAD")))
                 (or duplicates (not (gethash (car item) seen))))
        (puthash (car item) t seen)
        (push (nth 1 item) incoming)))
    (nreverse incoming)))

(defun ngit-tr--check-pull-rebase (repo mapping explicit merge mode)
  "Refuse unsupported replay before publishing any fetched object or ref."
  (let ((head (ngit-resolve-ref repo "HEAD"))
        (incoming (ngit-tr--merge-candidates mapping explicit merge)))
    ;; pull.c rejects multiple FETCH_HEAD lines after fetch, even when two
    ;; refspecs name the same source. Preserve that ordinary failure below.
    (when (and (not (cdr (ngit-tr--merge-candidates mapping explicit merge t)))
               (= (length incoming) 1)
               (not (ngit-tr--ff-p repo head (car incoming)))
               (or (eq mode 'interactive)
                   (not (ngit-tr--ff-p repo (car incoming) head))))
      (ngit--die "fatal: pull with rebase=%s is not supported" mode))))

(defun ngit-tr--display-fetch (url rows)
  (when rows
    (ngit-native-write-stderr (concat "From " (ngit-tr--display-url (if (string-suffix-p ".git" url) (substring url 0 -4) url)) "\n"))
    (let ((source-width 10) (summary-width 17))
      (dolist (row rows)
        (setq source-width (max source-width (length (nth 2 row)))
              summary-width (max summary-width (length (nth 1 row)))))
      (dolist (row rows)
        (ngit-native-write-stderr
         (concat " " (car row) " " (ngit--pad-to (nth 1 row) summary-width) " "
                 (ngit--pad-to (nth 2 row) source-width) " -> " (nth 3 row)
                 (if (nth 4 row) (concat "  (" (nth 4 row) ")") "") "\n"))))))

(defun ngit-cmd-fetch (repo args)
  (let ((ngit-tr--action
         (if (equal ngit-tr--action "fetch")
             (or (getenv "GIT_REFLOG_ACTION")
                 (concat "fetch" (if args (concat " " (mapconcat #'identity args " ")) "")))
           ngit-tr--action))
        remote specs prune tags no-tags quiet force append verbose)
    (while args
      (let ((a (pop args)))
        (cond ((member a '("--prune" "-p")) (setq prune t))
              ((member a '("--tags" "-t")) (setq tags t))
              ((equal a "--no-tags") (setq no-tags t))
              ((member a '("-q" "--quiet")) (setq quiet t))
              ((member a '("-v" "--verbose")) (setq verbose t))
              ((member a '("-f" "--force")) (setq force t))
              ((member a '("-a" "--append")) (setq append t))
              ((member a '("--no-recurse-submodules" "--no-write-commit-graph")) nil)
              ((string-prefix-p "-" a) (ngit--die-with 129 "error: unknown option `%s'" a))
              ((null remote) (setq remote a)) (t (push a specs)))))
    (setq remote (or remote (ngit-tr--default-remote repo)) specs (nreverse specs))
    (let* ((url (ngit-tr--url repo remote))
           (tracking (ngit-tr--config-all repo (concat "remote." remote ".fetch")))
           (explicit (not (null specs)))
           (specs (or specs tracking '("HEAD")))
           (conn (ngit-transport-open repo url "git-upload-pack"))
           (branch (ngit-tr--branch repo))
           (merge (and branch (ngit-config-get repo (concat "branch." branch ".merge"))))
           (fetch-head nil) (rows nil) (failed nil) (missing-tags nil))
      (setq ngit-tr--merge-heads nil)
      (unwind-protect
          (let* ((adv (progn
                        ;; Git clears FETCH_HEAD before reading advertisements
                        ;; or validating ref mappings, even when fetch fails.
                        (unless (or append ngit-tr--pull-rebase-mode)
                          (ngit-write-file
                           (expand-file-name "FETCH_HEAD" (ngit-repo-gitdir repo)) ""))
                        (ngit-tr--advertise conn)))
                 (refs (car adv))
                 (mapping (ngit-tr--fetch-map refs specs tracking explicit tags))
                 (check (when ngit-tr--pull-rebase-mode
                          (lambda (checked)
                            (ngit-tr--check-pull-rebase
                             checked mapping explicit merge ngit-tr--pull-rebase-mode)))) wants)
            (dolist (item mapping)
              (when (and (nth 2 item) (string-prefix-p "refs/heads/" (nth 2 item)))
                (let ((where (ngit--branch-checked-out repo (nth 2 item))))
                  (when where
                    (ngit--die "fatal: refusing to fetch into branch '%s' checked out at '%s'"
                               (nth 2 item) (directory-file-name where)))))
              (push (nth 1 item) wants))
            (let ((needed (seq-some
                           (lambda (oid)
                             (and oid (not (ngit-object-exists-p repo (ngit-unhex oid)))))
                           wants)))
              (ngit-tr--receive repo conn adv wants check)
              ;; Git checks upload-pack's status when it requested a pack,
              ;; but accepts a complete advertisement for a no-op fetch.
              (when needed
                (ngit-transport-finish-write conn)
                (unless (= 0 (ngit-transport-close conn))
                  (ngit--die-with 1 "error: remote transport failed"))))
            (when (and ngit-tr--pull-rebase-mode (not append))
              (ngit-write-file (expand-file-name "FETCH_HEAD" (ngit-repo-gitdir repo)) ""))
            ;; Auto-follow only tags whose peeled targets arrived with this fetch.
            (unless (or no-tags tags)
              (dolist (ref refs)
                (when (and (string-prefix-p "refs/tags/" (car ref))
                           (not (string-suffix-p "^{}" (car ref)))
                           (not (assoc (car ref) mapping))
                           (not (ngit-resolve-ref repo (car ref)))
                           (nth 1 ref)
                           (ngit-object-exists-p repo (ngit-unhex (or (nth 3 ref) (nth 1 ref)))))
                  (unless (ngit-object-exists-p repo (ngit-unhex (nth 1 ref)))
                    (push (nth 1 ref) missing-tags))
                  (setq mapping (append mapping (list (list (car ref) (nth 1 ref) (car ref) nil nil)))))))
            ;; A protocol-v0 upload-pack connection is complete after one
            ;; request.  Fetch all omitted auto-follow tag objects together
            ;; from a fresh session; this is also valid for protocol v2.
            (when missing-tags
              (ngit-transport-finish-write conn)
              (ngit-transport-close conn)
              (setq conn nil)
              (setq conn (ngit-transport-open repo url "git-upload-pack"))
              (let ((backfill-adv (ngit-tr--advertise conn)))
                (ngit-tr--receive repo conn backfill-adv (nreverse missing-tags))
                (ngit-transport-finish-write conn)
                (unless (= 0 (ngit-transport-close conn))
                  (ngit--die-with 1 "error: remote transport failed"))))
            ;; Auto-follow tags are discovered after the initial map.  Keep
            ;; every ordinary mapping ahead of opportunistic IGNORE-HEAD
            ;; tracking updates, as Git's final ref-map order requires.
            (setq mapping
                  (append (seq-remove (lambda (item) (nth 5 item)) mapping)
                          (seq-filter (lambda (item) (nth 5 item)) mapping)))
            (setq ngit-tr--merge-heads (ngit-tr--merge-candidates mapping explicit merge))
            (when (or prune (ngit-config-bool repo (concat "remote." remote ".prune")
                                            (ngit-config-bool repo "fetch.prune" nil)))
              (dolist (local (ngit-list-refs repo))
                (dolist (spec (if explicit specs tracking))
                  (let* ((p (ngit-tr--refspec spec))
                         (capture (and (nth 1 p) (ngit-tr--match (nth 1 p) (car local)))))
                    (when (and capture
                               (not (string-prefix-p "ref: " (or (ngit-read-ref repo (car local)) "")))
                               (not (assoc (ngit-tr--expand (car p) capture) refs)))
                      (ngit--delete-ref repo (car local))
                      (push (list "-" "[deleted]" "(none)" (ngit-tr--short (car local))) rows))))))
            (dolist (item mapping)
              (let* ((src (car item)) (oid (nth 1 item)) (dest (nth 2 item))
                     (old (and dest (ngit-resolve-ref repo dest)))
                     (mergep (or (nth 4 item) (and (not explicit) (equal src merge))
                                 (and (not explicit) (equal src "HEAD"))))
                     (tag (string-prefix-p "refs/tags/" src))
                     (dest-tag (and dest (string-prefix-p "refs/tags/" dest)))
                     (ignore-head (nth 5 item))
                     (short (ngit-tr--short src)) code summary message reason)
                (unless ignore-head
                  (push (cons mergep (concat oid "\t" (if mergep "" "not-for-merge") "\t"
                                             (cond ((equal src "HEAD") "") (tag (concat "tag '" short "' of "))
                                                   ((string-prefix-p "refs/heads/" src) (concat "branch '" short "' of "))
                                                   (t (concat "'" src "' of ")))
                                             (ngit-tr--display-url (if (string-suffix-p ".git" url) (substring url 0 -4) url)) "\n")) fetch-head))
                (cond
                 ((null dest) (setq code "*" summary "branch"))
                 ((equal old oid) (when verbose (setq code "=" summary "[up to date]")))
                 ((and dest (string-prefix-p "refs/heads/" dest)
                       (not (eq (car (ngit-read-object repo (ngit-unhex oid))) 'commit)))
                  (setq code "!" summary "[rejected]" reason "non-commit object" failed t))
                 ((and old dest-tag (not (or force (nth 3 item))))
                  (setq code "!" summary "[rejected]" reason "would clobber existing tag" failed t))
                 ((null old) (setq code "*" summary (if tag "[new tag]" "[new branch]")
                                  message (if tag "storing tag" "storing head")))
                 (dest-tag (setq code "t" summary "[tag update]" message "updating tag"))
                 ((ngit-tr--ff-p repo old oid)
                  (setq code " " summary (concat (ngit-abbrev repo old) ".." (ngit-abbrev repo oid)) message "fast-forward"))
                 ((or force (nth 3 item))
                  (setq code "+" summary (concat (ngit-abbrev repo old) "..." (ngit-abbrev repo oid))
                        reason "forced update" message "forced-update"))
                 (t (setq code "!" summary "[rejected]" reason "non-fast-forward" failed t)))
                (when message
                  (ngit--write-ref repo dest oid old (concat ngit-tr--action ": " message)))
                (when code
                  (push (list code summary short (if dest (ngit-tr--short dest) "FETCH_HEAD") reason) rows))))
            (setq fetch-head (nreverse fetch-head))
            (ngit-write-file
             (expand-file-name "FETCH_HEAD" (ngit-repo-gitdir repo))
             (apply #'concat (mapcar #'cdr
                                     (append (seq-filter #'car fetch-head)
                                             (seq-remove #'car fetch-head)))) nil append)
            (unless quiet (ngit-tr--display-fetch url (nreverse rows)))
            (if failed 1 0))
        (when conn
          (ngit-transport-finish-write conn)
          (ngit-transport-close conn))))))

(defun ngit-tr--config-rebase (repo key)
  "Read KEY as a rebase mode, retaining Git's special-value diagnostics."
  (let ((value (ngit-config-get repo key)))
    (cond ((member value '("merges" "m")) 'merges)
          ((member value '("interactive" "i")) 'interactive)
          (t (condition-case nil
                 (ngit--config-boolean-value value key)
               (ngit-fatal
                (when (member value '("preserve" "p"))
                  (ngit-native-write-stderr
                   (concat "error: " value ": 'preserve' superseded by 'merges'\n")))
                (ngit--die "fatal: invalid value for '%s': '%s'" key value)))))))

(defun ngit-tr--pull-up-to-date (repo head incoming mode quiet action)
  "Finish a special-mode pull whose existing linear commits need no replay."
  (let* ((branch (ngit-tr--branch repo))
         (ref (and branch (concat "refs/heads/" branch))))
    (if (eq mode 'interactive)
        (progn
          ;; Git skips every unnecessary pick, then checks out that same
          ;; tip and finishes. Only HEAD's start/finish reflogs are added.
          (ngit--write-ref repo "ORIG_HEAD" head (ngit-resolve-ref repo "ORIG_HEAD") nil)
          (ngit--switch-write-head repo nil head head (concat action " (start): checkout " incoming))
          (when ref
            (ngit--switch-write-head repo ref head head
                                     (concat action " (finish): returning to " ref)))
          (unless quiet
            (ngit-native-write-stderr
             (concat "Successfully rebased and updated " (or ref "detached HEAD") ".\n"))))
      (unless quiet
        (ngit--emit (if branch (concat "Current branch " branch " is up to date.\n")
                      "HEAD is up to date.\n"))))
    0))

(defun ngit-tr--pull-changes (repo)
  "Return unstaged and staged changes, ignoring submodules as pull.c does."
  (let ((ordinary (lambda (c) (and (/= (aref c 2) ngit-mode-gitlink)
                                   (/= (aref c 4) ngit-mode-gitlink)))))
    (list (seq-filter ordinary (ngit-wt-changes repo nil t))
          (seq-filter ordinary (ngit-diff-tree-index
                                repo (ngit--switch-tree repo (ngit-resolve-ref repo "HEAD")))))))

(defun ngit-tr--check-pull-worktree (repo autostash)
  "Check the requested rebase before fetch or ff-only mode selection."
  (when (and (not (ngit-resolve-ref repo "HEAD"))
             (> (length (ngit-index-entries repo)) 0))
    (ngit--die "fatal: Updating an unborn branch with changes added to the index."))
  (unless autostash
    (let* ((changes (ngit-tr--pull-changes repo))
           (unstaged (car changes)) (staged (cadr changes)))
      (when (or unstaged staged)
        (ngit--die-with
         128 "%s%s%s"
         (if unstaged "error: cannot pull with rebase: You have unstaged changes.\n" "")
         (if staged (if unstaged "error: additionally, your index contains uncommitted changes.\n"
                      "error: cannot pull with rebase: Your index contains uncommitted changes.\n") "")
         "error: Please commit or stash them.")))))

(defun ngit-tr--create-autostash (repo head)
  "Create merge's temporary stash without changing refs/stash or HEAD."
  (let* ((changes (ngit-tr--pull-changes repo)) (wt (car changes)))
    (when (or wt (cadr changes))
      (require 'ngit-stash)
      (let* ((index (ngit--stash-refresh repo)) (old (aref index 0))
             (branch (or (ngit-tr--branch repo) "(no branch)"))
             (label (concat branch ": " (ngit-abbrev repo head) " " (ngit--subject-of repo head)))
             (root (or (ngit-ct-root index) (ngit-ct-new)))
             (itree (ngit-ct-update repo root old))
             (icommit (ngit--stash-commit repo itree (list head) (concat "index on " label "\n")))
             (dropped (make-hash-table :test 'equal)) targets)
        (ngit-write-index repo index old root)
        (setq index (ngit-read-index repo) old (aref index 0))
        (dolist (c wt)
          (let ((path (aref c 0)))
            (puthash path t dropped)
            (when (/= (aref c 4) 0)
              (push (ngit--switch-entry
                     path (aref c 4)
                     (ngit-write-object repo "blob" (ngit-wt-content repo path (ngit-wt-lstat path)))
                     nil) targets))))
        (let* ((tree (ngit--stash-tree repo index (ngit-index-overlay old targets dropped) wt))
               (hex (ngit--stash-commit repo tree (list head icommit) (concat "On " branch ": autostash"))))
          (ngit--write-ref repo "MERGE_AUTOSTASH" hex nil nil)
          (ngit--reset-hard repo (ngit--switch-tree repo head) "HEAD")
          hex)))))

(defun ngit-tr--apply-autostash (repo hex)
  "Restore HEX quietly, storing it in refs/stash on conflicts."
  (let ((ngit--out nil) (stderr (symbol-function 'ngit-native-write-stderr)) status)
    (unwind-protect
        (progn
          (fset 'ngit-native-write-stderr (lambda (&rest ignored) nil))
          (setq status (condition-case nil (ngit--stash-apply repo (list hex nil hex) nil t)
                         (ngit-fatal 1))))
      (fset 'ngit-native-write-stderr stderr))
    (if (= status 0)
        (ngit-native-write-stderr "Applied autostash.\n")
      (let ((log (ngit--stash-log-file repo)))
        (ngit-make-directory (file-name-directory log) t)
        (unless (file-exists-p log) (ngit-write-file log ""))
        (ngit--write-ref repo "refs/stash" hex (ngit-resolve-ref repo "refs/stash") "autostash" t))
      (ngit-native-write-stderr
       "Applying autostash resulted in conflicts.\nYour changes are safe in the stash.\nYou can run \"git stash pop\" or \"git stash drop\" at any time.\n"))
    (ngit--delete-ref repo "MERGE_AUTOSTASH")))

(defun ngit-tr--pull-merge (repo args autostash head incoming)
  "Run merge, applying the selected autostash for a dirty fast-forward."
  ;; merge.c refuses a resolved cherry-pick before creating its autostash.
  ;; Unlike pull.c's index/MERGE_HEAD checks, this check follows fetch.
  (when (ngit-resolve-ref repo "CHERRY_PICK_HEAD")
    (ngit--die
     (concat "fatal: You have not concluded your cherry-pick (CHERRY_PICK_HEAD exists)."
             (if (ngit-config-bool repo "advice.resolveConflict" t)
                 "\nPlease, commit your changes before you merge." ""))))
  (let ((stash (and autostash head (not (equal head incoming))
                    (ngit-tr--ff-p repo head incoming) (ngit-tr--create-autostash repo head))))
    (if (not stash) (ngit-cmd-merge repo args)
      (let (output status)
        (unwind-protect
            (let ((ngit--out nil))
              (setq status (condition-case err (ngit-cmd-merge repo args)
                             (ngit-fatal (ngit-native-write-stderr (concat (cadr err) "\n"))
                                         (or (nth 2 err) 128))))
              (setq output (apply #'concat (nreverse ngit--out))))
          ;; merge prints its Updating line before creating the autostash.
          (let ((end (and output (string-prefix-p "Updating " output) (string-search "\n" output))))
            (when end (ngit--emit (substring output 0 (1+ end))))
            (ngit--emit "Created autostash: " (ngit-abbrev repo stash) "\n"
                        (if end (substring output (1+ end)) (or output ""))))
          (ngit-tr--apply-autostash repo stash))
        status))))

(defun ngit-cmd-pull (repo args)
  (let ((fetch-args nil) (merge-args nil) (rebase nil) (rebase-set nil) ff-option
        autostash
        (action (concat "pull" (if args (concat " " (mapconcat #'identity args " ")) ""))))
    (dolist (a args)
      (cond ((or (equal a "--rebase") (equal a "--rebase=true") (equal a "-r")) (setq rebase t rebase-set t))
            ((member a '("--rebase=merges" "--rebase=m")) (setq rebase 'merges rebase-set t))
            ((member a '("--rebase=interactive" "--rebase=i")) (setq rebase 'interactive rebase-set t))
            ((or (equal a "--no-rebase") (equal a "--rebase=false")) (setq rebase nil rebase-set t))
            ((equal a "--autostash") (setq autostash 'yes))
            ((equal a "--no-autostash") (setq autostash 'no))
            ((member a '("--ff-only" "--ff" "--no-ff")) (setq ff-option a))
            ((member a '("--no-edit" "--no-stat" "--stat")) (push a merge-args))
            ((member a '("-q" "--quiet")) (push a merge-args) (push a fetch-args))
            (t (push a fetch-args))))
    (unless rebase-set
      (let ((key (concat "branch." (or (ngit-tr--branch repo) "") ".rebase")))
        (setq rebase (ngit-tr--config-rebase
                      repo (if (ngit-config-get repo key) key "pull.rebase")))))
    (unless ff-option
      (let ((ff (ngit-config-get repo "pull.ff")))
        ;; pull.c lets any explicit rebase option override configured only,
        ;; before reading the branch/pull rebase configuration.
        (setq ff-option (cond ((equal ff "only") (if rebase-set "--ff" "--ff-only"))
                              ((equal ff "false") "--no-ff")
                              ((equal ff "true") "--ff")))))
    (when ff-option (push ff-option merge-args))
    (setq autostash (if autostash (eq autostash 'yes)
                      (ngit-config-bool repo (if rebase "rebase.autostash" "merge.autostash") nil)))
    ;; pull.c checks these before require_clean_work_tree and run_fetch,
    ;; including when ff-only will select merge instead of rebase.
    (when (aref (ngit-read-index repo) 9)
      (ngit--die
       (concat "error: Pulling is not possible because you have unmerged files.\n"
               (if (ngit-config-bool repo "advice.resolveConflict" t)
                   "hint: Fix them up in the work tree, and then use 'git add/rm <file>'\nhint: as appropriate to mark resolution and make a commit.\n" "")
               "fatal: Exiting because of an unresolved conflict.")))
    (when (file-exists-p (expand-file-name "MERGE_HEAD" (ngit-repo-gitdir repo)))
      (ngit--die
       (concat "error: You have not concluded your merge (MERGE_HEAD exists).\n"
               (if (ngit-config-bool repo "advice.resolveConflict" t)
                   "hint: Please, commit your changes before merging.\n" "")
               "fatal: Exiting because of unfinished merge.")))
    (when rebase (ngit-tr--check-pull-worktree repo autostash))
    (let ((ngit-tr--action action) (ngit-tr--merge-heads nil)
          (ngit-tr--pull-rebase-mode (and (memq rebase '(merges interactive))
                                         (not (equal ff-option "--ff-only")) rebase)))
      (let ((status (ngit-cmd-fetch repo (nreverse fetch-args))))
        (if (/= status 0) status
          (unless ngit-tr--merge-heads (ngit--die "There is no tracking information for the current branch.\nPlease specify which branch you want to merge with."))
          (let ((heads (split-string
                        (ngit--read-file (expand-file-name "FETCH_HEAD" (ngit-repo-gitdir repo))) "\n" t)))
            (when (and rebase (> (length (seq-filter
                                        (lambda (line) (string-search "\t\t" line)) heads)) 1))
              (ngit--die "fatal: Cannot rebase onto multiple branches.")))
          (when (cdr ngit-tr--merge-heads) (ngit--die "fatal: Cannot merge multiple branches with ngit pull."))
          (let ((old-action (getenv "GIT_REFLOG_ACTION"))
                (head (ngit-resolve-ref repo "HEAD")) (incoming (car ngit-tr--merge-heads)))
            ;; pull.c refuses a divergent --ff-only before invoking merge,
            ;; so this failure must not publish merge's ORIG_HEAD either.
            (when (and head (equal ff-option "--ff-only")
                       (not (ngit-tr--ff-p repo head incoming))
                       (not (ngit-tr--ff-p repo incoming head)))
              (ngit--die
               (concat
                (if (ngit-config-bool repo "advice.diverging" t)
                    "hint: Diverging branches can't be fast-forwarded, you need to either:\nhint:\nhint: \tgit merge --no-ff\nhint:\nhint: or:\nhint:\nhint: \tgit rebase\nhint:\nhint: Disable this message with \"git config advice.diverging false\"\n" "")
                "fatal: Not possible to fast-forward, aborting.")))
            (when (and (not rebase-set)
                       (not (ngit-config-get repo "pull.rebase"))
                       (not (ngit-config-get repo (concat "branch." (or (ngit-tr--branch repo) "") ".rebase")))
                       (not (ngit-config-get repo "pull.ff"))
                       (not (seq-some (lambda (arg) (member arg '("--ff" "--no-ff" "--ff-only"))) merge-args))
                       (not (equal head incoming))
                       (not (ngit-tr--ff-p repo head incoming))
                       (not (ngit-tr--ff-p repo incoming head)))
              (ngit--die
               "hint: You have divergent branches and need to specify how to reconcile them.\nhint: You can do so by running one of the following commands sometime before\nhint: your next pull:\nhint:\nhint:   git config pull.rebase false  # merge\nhint:   git config pull.rebase true   # rebase\nhint:   git config pull.ff only       # fast-forward only\nhint:\nhint: You can replace \"git config\" with \"git config --global\" to set a default\nhint: preference for all repositories. You can also pass --rebase, --no-rebase,\nhint: or --ff-only on the command line to override the configured default per\nhint: invocation.\nfatal: Need to specify how to reconcile divergent branches."))
            (unwind-protect
                (progn
                  (setenv "GIT_REFLOG_ACTION" action)
                  (when (and ngit-tr--pull-rebase-mode (ngit-tr--ff-p repo head incoming))
                    ;; builtin/pull.c uses merge --ff-only even with pull.ff=false.
                    (setq merge-args (cons "--ff-only" (seq-remove
                                                       (lambda (arg) (member arg '("--ff" "--no-ff" "--ff-only")))
                                                       merge-args))))
                  (if (and rebase (not (member "--ff-only" merge-args))
                           (not (ngit-tr--ff-p repo head incoming)))
                      (if ngit-tr--pull-rebase-mode
                          (ngit-tr--pull-up-to-date repo head incoming rebase
                                                   (or (member "-q" merge-args) (member "--quiet" merge-args)) action)
                        (ngit-cmd-rebase repo (list "--onto" incoming incoming)))
                    (let* ((text (ngit--read-file (expand-file-name "FETCH_HEAD" (ngit-repo-gitdir repo))))
                           (line (car (split-string text "\n" t)))
                           (fields (split-string line "\t"))
                           (title (concat "Merge " (nth 2 fields))))
                      (ngit-tr--pull-merge repo
                                      (append (nreverse merge-args)
                                              (unless (or (equal head incoming) (ngit-tr--ff-p repo head incoming))
                                                (list "-m" title))
                                              (list "FETCH_HEAD")) autostash head incoming))))
              (setenv "GIT_REFLOG_ACTION" old-action))))))))

(defun ngit-tr--push-map (repo refs specs all tags delete)
  "Build updates as [SOURCE DEST NEW OLD FORCE REASON]."
  (let (out seen expanded)
    (when all
      (dolist (ref (ngit-list-refs repo))
        (when (string-prefix-p "refs/heads/" (car ref)) (push (car ref) specs))))
    (when tags
      (dolist (ref (ngit-list-refs repo))
        (when (string-prefix-p "refs/tags/" (car ref)) (push (car ref) specs))))
    (dolist (spec specs)
      (let* ((p (ngit-tr--refspec spec)) (src (car p)) (dst (nth 1 p)))
        (cond
         ((equal spec ":")
          (dolist (ref (ngit-list-refs repo))
            (when (and (string-prefix-p "refs/heads/" (car ref)) (assoc (car ref) refs))
              (push (car ref) expanded))))
         ((string-search "*" src)
          (unless (and dst (string-search "*" dst)) (ngit--die "fatal: invalid refspec '%s'" spec))
          (dolist (ref (ngit-list-refs repo))
            (let ((capture (ngit-tr--match src (car ref))))
              (when capture
                (push (concat (if (nth 2 p) "+" "") (car ref) ":" (ngit-tr--expand dst capture)) expanded)))))
         (t (push spec expanded)))))
    (setq specs (nreverse expanded))
    (dolist (spec specs)
      (let* ((p (ngit-tr--refspec spec))
             (src (if delete "" (car p)))
             (dst (if delete (car p) (or (nth 1 p) (car p))))
             (local (and (not (equal src "")) (ngit-dwim-ref repo src)))
             (oid (if (equal src "") ngit-tr--zero
                    (or (cdr local) (condition-case nil (ngit-rev-parse repo src) (error nil))))))
        (unless oid (ngit--die-with 1 "error: src refspec %s does not match any" src))
        (unless (string-prefix-p "refs/" dst)
          (setq dst (or (car (ngit-tr--find refs dst))
                        (and local (if (string-prefix-p "refs/tags/" (car local))
                                       (concat "refs/tags/" dst) (concat "refs/heads/" dst)))
                        (concat "refs/heads/" dst))))
        (ngit-tr--safe-ref dst)
        (unless (member dst seen)
          (push dst seen)
          (push (vector (if delete "" (or (car local) src)) dst oid
                        (or (nth 1 (assoc dst refs)) ngit-tr--zero) (nth 2 p) nil) out))))
    (sort out (lambda (a b) (string< (aref a 1) (aref b 1))))))

(defun ngit-tr--push-hint (reason)
  (concat
   (cond ((equal reason "fetch first")
          "hint: Updates were rejected because the remote contains work that you do not\nhint: have locally. This is usually caused by another repository pushing to\nhint: the same ref. If you want to integrate the remote changes, use\nhint: 'git pull' before pushing again.\n")
         ((equal reason "already exists") "hint: Updates were rejected because the tag already exists in the remote.\n")
         (t "hint: Updates were rejected because the tip of your current branch is behind\nhint: its remote counterpart. If you want to integrate the remote changes,\nhint: use 'git pull' before pushing again.\n"))
   (if (equal reason "already exists") ""
     "hint: See the 'Note about fast-forwards' in 'git push --help' for details.\n")))

(defun ngit-tr--push-objects (repo wants haves)
  "Collect new objects, stopping at advertised tips and their current trees.
An ordinary one-commit push need not traverse every historical tree twice."
  (let ((known (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal)) trees pending out)
    (dolist (oid haves)
      (puthash oid t known)
      (let ((commit (condition-case nil (ngit-peel repo oid 'commit) (error nil))))
        (when commit
          (puthash commit t known)
          (push (ngit-peel repo commit 'tree) trees))))
    (when trees
      (dolist (oid (ngit-pack-reachable repo (delete-dups trees))) (puthash oid t known)))
    (setq pending (mapcar (lambda (oid)
                           (unless (ngit-hex-p oid 40) (ngit--die "fatal: bad object %s" oid))
                           (cons nil (downcase oid))) wants))
    (while pending
      (let* ((entry (pop pending)) (oid (cdr entry)))
        (unless (or (gethash oid known) (gethash oid seen))
          (puthash oid t seen) (push oid out)
          (unless (eq (car entry) 'blob)
            (let* ((object (ngit-read-object repo (ngit-unhex oid)))
                   (links (ngit-object-links object)))
              (dolist (link links)
                (unless (eq (car object) 'tree)
                  (unless (ngit-hex-p (cdr link) 40) (ngit--die "fatal: bad object %s" (cdr link)))
                  (setcdr link (downcase (cdr link))))
                (push link pending)))))))
    (nreverse out)))

(defun ngit-tr--lease-ref-p (name ref)
  "Match a lease NAME using Git's refname disambiguation rules."
  (or (equal name ref) (equal (concat "refs/" name) ref)
      (equal (concat "refs/tags/" name) ref)
      (equal (concat "refs/heads/" name) ref)
      (equal (concat "refs/remotes/" name) ref)
      (equal (concat "refs/remotes/" name "/HEAD") ref)))

(defun ngit-tr--lease-tracking (repo remote ref)
  "Resolve REF through REMOTE's fetch mappings, or expect an absent ref.
Missing tracking information must never authorize overwriting an existing ref."
  (let (tracking)
    (dolist (spec (ngit-tr--config-all repo (concat "remote." remote ".fetch")))
      (let* ((p (ngit-tr--refspec spec)) (capture (ngit-tr--match (car p) ref)))
        (when (and (not tracking) capture (nth 1 p))
          (setq tracking (ngit-tr--expand (nth 1 p) capture)))))
    (or (and tracking (ngit-resolve-ref repo tracking)) ngit-tr--zero)))

(defun ngit-tr--push-validate (repo remote updates force leases lease-all)
  "Validate all UPDATES before constructing or sending any commands.
LEASES are ordered (REF . EXPECT) entries; `tracking' requests a fetch mapping.
The first matching named lease wins, and explicit force overrides leases."
  (dolist (update updates)
    (let* ((new (aref update 2)) (old (aref update 3)) (dst (aref update 1))
           (forced (or force (aref update 4)))
           (lease (seq-find (lambda (entry) (ngit-tr--lease-ref-p (car entry) dst)) leases))
           (leased (or lease lease-all))
           (expect (if lease (cdr lease) 'tracking)))
      (cond
       ((equal old new) (aset update 5 'uptodate))
       (forced nil)
       (leased
        (when (eq expect 'tracking) (setq expect (ngit-tr--lease-tracking repo remote dst)))
        (unless (equal old expect) (aset update 5 "stale info")))
       ((or (equal old ngit-tr--zero) (equal new ngit-tr--zero)) nil)
       ((string-prefix-p "refs/tags/" dst) (aset update 5 "already exists"))
       ((not (ngit-object-exists-p repo (ngit-unhex old))) (aset update 5 "fetch first"))
       ((not (ngit-tr--ff-p repo old new)) (aset update 5 "non-fast-forward"))))))

(defun ngit-cmd-push (repo args)
  (let (remote specs force tags all delete upstream quiet verbose no-verify push-options
                atomic leases lease-all)
    (while args
      (let ((a (pop args)))
        (cond ((member a '("-f" "--force")) (setq force t))
              ((equal a "--no-force") (setq force nil))
              ((equal a "--atomic") (setq atomic t))
              ((equal a "--no-atomic") (setq atomic nil))
              ((equal a "--force-with-lease") (setq lease-all t))
              ((equal a "--no-force-with-lease") (setq leases nil lease-all nil))
              ((string-prefix-p "--force-with-lease=" a)
               (let* ((value (substring a 19)) (colon (string-search ":" value))
                      (name (if colon (substring value 0 colon) value))
                      (expect (if colon (substring value (1+ colon)) 'tracking)))
                 (when (stringp expect)
                   (setq expect (if (equal expect "") ngit-tr--zero
                                  (condition-case nil (ngit-rev-parse repo expect)
                                    (error (ngit--die-with 129 "error: cannot parse expected object name '%s'" expect))))))
                 (push (cons name expect) leases)))
              ((equal a "--tags") (setq tags t))
              ((equal a "--all") (setq all t))
              ((member a '("--delete" "-d")) (setq delete t))
              ((member a '("--set-upstream" "-u")) (setq upstream t))
              ((member a '("--quiet" "-q")) (setq quiet t))
              ((member a '("--verbose" "-v")) (setq verbose t))
              ((equal a "--no-verify") (setq no-verify t))
              ((equal a "--verify") (setq no-verify nil))
              ((member a '("-o" "--push-option"))
               (unless args (ngit--die-with 129 "error: option `%s' requires a value" a))
               (push (pop args) push-options))
              ((string-prefix-p "--push-option=" a)
               (push (substring a 14) push-options))
              ((string-prefix-p "-" a) (ngit--die-with 129 "error: unknown option `%s'" a))
              ((null remote) (setq remote a)) (t (push a specs)))))
    (let* ((branch (ngit-tr--branch repo))
           (remote (or remote (and branch (ngit-config-get repo (concat "branch." branch ".pushremote")))
                       (ngit-config-get repo "remote.pushdefault") (ngit-tr--default-remote repo)))
           (url (ngit-tr--url repo remote t))
           (specs (or (nreverse specs) (ngit-tr--config-all repo (concat "remote." remote ".push"))))
           (conn nil) failed sent updates rows started)
      (when (and delete (or (null specs) tags all))
        (ngit--die "fatal: --delete doesn't make sense without any refs"))
      (unless (or specs tags all)
        (unless branch (ngit--die "fatal: You are not currently on a branch."))
        (let ((merge (ngit-config-get repo (concat "branch." branch ".merge")))
              (mode (or (ngit-config-get repo "push.default") "simple")))
          (when (and (not upstream) (null merge) (member mode '("simple" "upstream")))
            (ngit--die "fatal: The current branch %s has no upstream branch.\nTo push the current branch and set the remote as upstream, use\n\n    git push --set-upstream %s %s\n\nTo have this happen automatically for branches without a tracking\nupstream, see 'push.autoSetupRemote' in 'git help config'." branch remote branch))
          (when (and (equal mode "simple") merge (not (equal merge (concat "refs/heads/" branch))))
            (ngit--die "fatal: The upstream branch of your current branch does not match\nthe name of your current branch.  To push to the upstream branch\non the remote, use\n\n    git push %s HEAD:%s\n\nTo push to the branch of the same name on the remote, use\n\n    git push %s HEAD\n\nTo choose either option permanently, see push.default in 'git help config'.\n\nTo avoid automatically configuring an upstream branch when its name\nwon't match the local branch, see option 'simple' of branch.autoSetupMerge\nin 'git help config'." remote (ngit-tr--short merge) remote))
          (setq specs (list (concat "refs/heads/" branch ":" (if (and merge (equal mode "upstream")) merge (concat "refs/heads/" branch)))))))
      (setq conn (ngit-transport-open repo url "git-receive-pack"))
      (unwind-protect
          (let* ((adv (ngit-tr--advertise conn t)) (refs (car adv))
                 (caps (nth 1 adv))
                 (sideband (member "side-band-64k" caps))
                 (first t) wants haves (parts nil))
            (setq push-options (nreverse push-options))
            (when (and atomic (not (member "atomic" caps)))
              (ngit--die "fatal: the receiving end does not support --atomic push\nfatal: the remote end hung up unexpectedly"))
            (when (and push-options (not (member "push-options" caps)))
              (ngit--die "fatal: the receiving end does not support push options"))
            (setq updates (ngit-tr--push-map repo refs specs all tags delete))
            (dolist (update updates)
              (let ((old (aref update 3)) (dst (aref update 1)))
                (when (and delete (equal old ngit-tr--zero))
                  (ngit--die-with 1 "error: unable to delete '%s': remote ref does not exist\nerror: failed to push some refs to '%s'"
                                  (ngit-tr--short dst) (ngit-tr--display-url url)))))
            (ngit-tr--push-validate repo remote updates force (nreverse leases) lease-all)
            (dolist (update updates)
              (when (stringp (aref update 5)) (setq failed (or failed (aref update 5)))))
            (when (and (not no-verify)
                       (ngit-hook-exists-p repo "pre-push"))
              (let (hook-lines)
                (dolist (update updates)
                  (unless (or (eq (aref update 5) 'uptodate)
                              (member (aref update 5) '("non-fast-forward" "stale info")))
                    (push (concat (if (equal (aref update 2) ngit-tr--zero)
                                      "(delete)" (aref update 0))
                                  " " (aref update 2) " " (aref update 1)
                                  " " (aref update 3) "\n")
                          hook-lines)))
                (when (/= (ngit-hook--run repo "pre-push" (list remote url)
                                          (apply #'concat (nreverse hook-lines)) nil)
                           0)
                  (ngit--die-with 1 "error: failed to push some refs to '%s'" url))))
            (when (and atomic failed)
              (let* ((rejected (seq-find (lambda (u) (stringp (aref u 5))) updates))
                     (status (cdr (assoc (aref rejected 5)
                                         '(("non-fast-forward" . 2) ("already exists" . 3)
                                           ("fetch first" . 5) ("stale info" . 7))))))
                (ngit-native-write-stderr
                 (format "error: atomic push failed for ref %s. status: %d\nfatal: the remote end hung up unexpectedly\n"
                         (aref rejected 1) status)))
              (dolist (update updates)
                (unless (aref update 5) (aset update 5 "atomic push failed"))))
            (dolist (update updates)
              (unless (aref update 5)
                (setq sent t)
                (push (ngit-tr--pkt
                       (concat (aref update 3) " " (aref update 2) " " (aref update 1)
                               (if first
                                   (concat "\0report-status"
                                           (if sideband " side-band-64k" "")
                                           (if atomic " atomic" "")
                                           (if push-options " push-options" "")
                                           (if (member "ofs-delta" caps) " ofs-delta" ""))
                                 "") "\n")) parts)
                (setq first nil)
                (unless (equal (aref update 2) ngit-tr--zero) (push (aref update 2) wants))))
            (push "0000" parts)
            (when (and sent push-options)
              (dolist (option push-options)
                (when (or (string-search "\n" option) (string-search "\0" option))
                  (ngit--die "fatal: push options must not have new line characters"))
                (push (ngit-tr--pkt (concat option "\n")) parts))
              (push "0000" parts))
            (ngit-transport-write conn (apply #'concat (nreverse parts)))
            (setq started t)
            (when wants
              ;; Exclude only advertised histories actually present locally.
              (dolist (ref refs)
                (when (ngit-object-exists-p repo (ngit-unhex (nth 1 ref))) (push (nth 1 ref) haves)))
              (let* ((reachable (ngit-tr--push-objects repo wants haves))
                     (ngit-pack--loose-only t)
                     (base (expand-file-name (concat "objects/pack/push-" (number-to-string (emacs-pid))) (ngit-repo-commondir repo)))
                     (sha nil) path)
                (dolist (oid reachable)
                  (unless (file-regular-p (ngit--loose-path repo oid)) (setq ngit-pack--loose-only nil)))
                (setq sha (ngit-pack-write repo reachable base 0 0 t)
                      path (concat base "-" sha ".pack"))
                (unwind-protect (ngit-transport-send-file conn path)
                  (ngit-native-close path)
                  (delete-file path)
                  (delete-file (concat base "-" sha ".idx")))))
            (ngit-transport-finish-write conn)
            (when sent
              (let ((report (ngit-tr--push-report conn sideband))
                    (expected (make-hash-table :test 'equal))
                    (reported (make-hash-table :test 'equal)))
                (dolist (u updates)
                  (unless (aref u 5) (puthash (aref u 1) u expected)))
                (unless (equal (car report) "unpack ok")
                  (ngit--die-with 1 "error: remote unpack failed: %s" (car report)))
                (dolist (line (cdr report))
                  (let* ((ok (string-prefix-p "ok " line))
                         (ng (string-prefix-p "ng " line))
                         (space (and ng (string-search " " line 3)))
                         (dst (cond (ok (substring line 3))
                                    (space (substring line 3 space))))
                         (u (and dst (gethash dst expected))))
                    (unless (and dst (or ok (and space (< (1+ space) (length line)))))
                      (ngit--die-with 1 "error: invalid remote status report: %s" line))
                    (cond
                     ((null u)
                      (ngit-native-write-stderr
                       (format "warning: remote reported status on unknown ref: %s\n" dst)))
                     ;; Git uses the first result and tolerates a repeated
                     ;; acknowledgement; neither can cover a missing ref.
                     ((gethash dst reported) nil)
                     (t
                      (puthash dst t reported)
                      (when ng
                        (let ((reason (cons 'remote (substring line (1+ space)))))
                          (setq failed (or failed reason))
                          (aset u 5 reason)))))))
                (maphash (lambda (dst _update)
                           (unless (gethash dst reported)
                             (ngit--die-with 1 "error: remote failed to report status for %s" dst)))
                         expected)))
            (unless (= 0 (ngit-transport-close conn))
              (ngit--die-with 1 "error: remote transport failed"))
            (dolist (u updates)
              (let* ((src (ngit-tr--short (aref u 0))) (dst (ngit-tr--short (aref u 1)))
                     (new (aref u 2)) (old (aref u 3)) (reason (aref u 5))
                     code summary suffix)
                (cond
                 ((eq reason 'uptodate) (when verbose (setq code "=" summary "[up to date]")))
                 ((consp reason)
                  (setq code "!" summary "[remote rejected]" suffix (cdr reason)))
                 (reason (setq code "!" summary "[rejected]" suffix reason)
                         (when (equal new ngit-tr--zero) (setq src "(delete)")))
                 ((equal new ngit-tr--zero) (setq code "-" summary "[deleted]" src ""))
                 ((equal old ngit-tr--zero) (setq code "*" summary (if (string-prefix-p "refs/tags/" (aref u 1)) "[new tag]" "[new branch]")))
                 ((ngit-tr--ff-p repo old new) (setq code " " summary (concat (ngit-abbrev repo old) ".." (ngit-abbrev repo new))))
                 (t (setq code "+" summary (concat (ngit-abbrev repo old) "..." (ngit-abbrev repo new)) suffix "forced update")))
                (when code (push (list code summary src dst suffix) rows))
                (when (or (null reason) (eq reason 'uptodate))
                  (dolist (spec (ngit-tr--config-all repo (concat "remote." remote ".fetch")))
                    (let* ((p (ngit-tr--refspec spec)) (capture (ngit-tr--match (car p) (aref u 1))))
                      (when (and capture (nth 1 p))
                        (let ((tracking (ngit-tr--expand (nth 1 p) capture)))
                          (if (equal new ngit-tr--zero) (ngit--delete-ref repo tracking)
                            (unless (equal (ngit-resolve-ref repo tracking) new)
                              (ngit--write-ref repo tracking new (ngit-resolve-ref repo tracking) "update by push" t)))))))
                  (when (and upstream (not (equal new ngit-tr--zero))
                             (string-prefix-p "refs/heads/" (aref u 0))
                             (string-prefix-p "refs/heads/" (aref u 1)))
                    (ngit--install-tracking repo (substring (aref u 0) 11) remote (aref u 1) quiet)))))
            (when (or (not quiet) failed)
              (if rows
                  (progn
                    (ngit-native-write-stderr (concat "To " (ngit-tr--display-url url) "\n"))
                    (let ((abbrev 7) width)
                      (dolist (ref refs)
                        (setq abbrev (max abbrev (length (ngit-abbrev repo (nth 1 ref))))))
                      (dolist (update updates)
                        (setq abbrev (max abbrev
                                          (length (ngit-abbrev repo (aref update 2)))
                                          (length (ngit-abbrev repo (aref update 3))))))
                      (setq width (+ (* 2 abbrev) 3))
                      ;; Git prints up-to-date refs, successful updates, then
                      ;; rejected updates, preserving ref order within each.
                      (setq rows (nreverse rows))
                      (dolist (row (append
                                    (seq-filter (lambda (r) (equal (car r) "=")) rows)
                                    (seq-filter (lambda (r) (not (member (car r) '("=" "!")))) rows)
                                    (seq-filter (lambda (r) (equal (car r) "!")) rows)))
                        (ngit-native-write-stderr
                         (concat " " (car row) " " (ngit--pad-to (nth 1 row) width) " "
                                 (if (equal (car row) "-") (nth 3 row)
                                   (concat (nth 2 row) " -> " (nth 3 row)))
                                 (if (nth 4 row) (concat " (" (nth 4 row) ")") "") "\n")))))
                (ngit-native-write-stderr "Everything up-to-date\n")))
            (when failed
              (ngit-native-write-stderr (concat "error: failed to push some refs to '" (ngit-tr--display-url url) "'\n"))
              (when (and (member failed '("fetch first" "already exists" "non-fast-forward"))
                         (ngit-config-bool repo "advice.pushUpdateRejected" t))
                (ngit-native-write-stderr (ngit-tr--push-hint failed))))
            (if failed 1 0))
        (unless started (ngit-transport-write conn "0000"))
        (ngit-transport-finish-write conn)
        (ngit-transport-close conn)))))

(provide 'ngit-transport)
;;; ngit-transport.el ends here
