;;; ngit-checkout.el --- checkout index and tree paths -*- lexical-binding: t; -*-

;;; Commentary:

;; Path checkout and the work-tree update primitive shared by hard reset.
;; Checkout uses work-tree attributes with an index fallback; hard reset
;; dynamically selects index-first attributes while installing its target
;; entries in the repository index.

;;; Code:

(require 'ngit-command-paths)
(require 'ngit-log)
(require 'ngit-add)
(require 'ngit-write)

(defvar ngit--checkout-attr-direction 'worktree-first
  "Attribute source order for work-tree writes.
Hard reset binds this to `index-first'; path checkout uses worktree first.")

(defvar ngit--checkout-attr-cache nil)

(defconst ngit--checkout-sys-write 1)
(defconst ngit--checkout-o-wronly-creat-excl-cloexec #o2000301)

(defun ngit--checkout-parse-attrs (data base)
  "Parse gitattributes DATA defined at BASE, last rule first."
  (let ((pats nil))
    (dolist (line (ngit--pattern-lines data))
      (let* ((fields (split-string line "[ \t]+" t))
             (text (car fields)))
        (when (and text (not (string-prefix-p "[attr]" text))
                   (not (string-prefix-p "!" text)))
          (let ((pat (ngit-parse-pattern text base)))
            (aset pat 3 (ngit--parse-attr-states (cdr fields)))
            (push pat pats)))))
    pats))

(defun ngit--checkout-index-attr-data (repo name)
  (let ((e (ngit-index-lookup (ngit-index-entries repo) name)))
    (when (and e (= (aref e 3) 0)
               (= (logand (aref e 2) #o170000) #o100000))
      (let ((obj (ngit-read-object repo (aref e 1))))
        (and (eq (car obj) 'blob) (cdr obj))))))

(defun ngit--checkout-worktree-attr-data (repo name)
  (let* ((top (ngit-repo-toplevel repo))
         (file (and top (expand-file-name (decode-coding-string name 'utf-8) top))))
    (and file (ngit--worktree-nofollow-p file name)
         (file-regular-p file) (ngit--read-worktree-file file name))))

(defun ngit--checkout-attr-data (repo name)
  (if (eq ngit--checkout-attr-direction 'index-first)
      (or (ngit--checkout-index-attr-data repo name)
          (ngit--checkout-worktree-attr-data repo name))
    (or (ngit--checkout-worktree-attr-data repo name)
        (ngit--checkout-index-attr-data repo name))))

(defun ngit--checkout-attr-stack (repo dir)
  "Return checkout attribute rules for DIR, highest precedence first."
  (or (and ngit--checkout-attr-cache (gethash dir ngit--checkout-attr-cache))
      (let ((stack
             (if (string= dir "")
                 (let* ((af (ngit-config-get repo "core.attributesfile"))
                        (xdg (getenv "XDG_CONFIG_HOME"))
                        (global (if (stringp af) (expand-file-name af)
                                  (expand-file-name
                                   "git/attributes"
                                   (if (and xdg (not (string= xdg ""))) xdg
                                     (expand-file-name ".config" (getenv "HOME"))))))
                        (data (ngit--checkout-attr-data repo ".gitattributes")))
                   ;; Keep the info slot even when empty: nested stacks insert
                   ;; their own rules after it, ahead of parent rules.
                   (list (ngit-read-attr-file
                          (expand-file-name "info/attributes"
                                            (ngit-repo-commondir repo)) "")
                         (and data (ngit--checkout-parse-attrs data ""))
                         (ngit-read-attr-file global "")))
               (let* ((parent-dir (or (file-name-directory (substring dir 0 -1)) ""))
                      (parent (ngit--checkout-attr-stack repo parent-dir))
                      (name (concat dir ".gitattributes"))
                      (data (ngit--checkout-attr-data repo name))
                      (own (and data (ngit--checkout-parse-attrs data dir))))
                 (if own (cons (car parent) (cons own (cdr parent))) parent)))))
        (when ngit--checkout-attr-cache (puthash dir stack ngit--checkout-attr-cache))
        stack)))

(defun ngit--checkout-attrs (repo path names)
  (let* ((slash (let ((i (1- (length path))))
                  (while (and (>= i 0) (/= (aref path i) ?/)) (setq i (1- i))) i))
         (dir (if (< slash 0) "" (substring path 0 (1+ slash))))
         (bstart (1+ slash))
         (result (mapcar (lambda (name) (cons name nil)) names)))
    (catch 'done
      (dolist (pats (ngit--checkout-attr-stack repo dir))
        (dolist (pat pats)
          (when (ngit-pattern-match-p pat path bstart nil)
            (dolist (state (reverse (aref pat 3)))
              (let ((expanded
                     (cons state (and (eq (cdr state) 'set)
                                      (cdr (assoc (car state) ngit--attr-macros))))))
                (dolist (one expanded)
                  (let ((slot (assoc (car one) result)))
                    (when (and slot (null (cdr slot)))
                      (setcdr slot (cdr one)))))))
            (unless (rassq nil result) (throw 'done nil))))))
    (mapcar (lambda (one) (cons (car one) (or (cdr one) 'unspecified))) result)))

(defun ngit--checkout-eol-action (repo path)
  "Return (EOL . AUTO-P) for PATH; EOL is `crlf', `lf' or nil."
  (let* ((attrs (ngit--checkout-attrs repo path '("text" "crlf" "eol")))
         (text (cdr (assoc "text" attrs)))
         (text (if (eq text 'unspecified) (cdr (assoc "crlf" attrs)) text))
         (eol (cdr (assoc "eol" attrs)))
         (auto (equal text "auto"))
         (autocrlf (ngit-config-get repo "core.autocrlf"))
         (core-eol (ngit-config-get repo "core.eol")))
    (cond
     ((eq text 'unset) (cons nil nil))
     ((or (equal eol "lf") (equal text "input")) (cons 'lf auto))
     ((equal eol "crlf") (cons 'crlf auto))
     ((and (eq text 'unspecified)
           (not (or (equal autocrlf "input")
                    (and autocrlf (ngit-config-bool repo "core.autocrlf" nil)))))
      (cons nil nil))
     ((equal autocrlf "input") (cons 'lf (or auto (eq text 'unspecified))))
     ((and autocrlf (ngit-config-bool repo "core.autocrlf" nil))
      (cons 'crlf (or auto (eq text 'unspecified))))
     ((equal core-eol "crlf") (cons 'crlf auto))
     (t (cons 'lf auto)))))

(defun ngit--checkout-add-crlf (data)
  "Replace naked LF bytes in DATA with CRLF."
  (string-replace "\n" "\r\n" (string-replace "\r\n" "\n" data)))

(defun ngit--checkout-smudge (repo path data)
  "Apply Git's supported checkout-side EOL conversion to DATA."
  (let ((action (ngit--checkout-eol-action repo path)))
    (if (or (not (eq (car action) 'crlf))
            (not (string-match-p "\n" data))
            (and (cdr action)
                 (let ((stats (ngit--text-stats data)))
                   (or (nth 0 stats) (nth 1 stats) (nth 3 stats)))))
        data
      (ngit--checkout-add-crlf data))))

(defvar ngit--checkout-leading-cache nil
  "Operation-local cache of leading directory checks, cleared on mutations.")

(defun ngit--checkout-leading-clear-p (path)
  "Non-nil when every existing leading component of PATH is a directory."
  (let* ((dir (file-name-directory path))
         (cached (and ngit--checkout-leading-cache dir
                      (gethash dir ngit--checkout-leading-cache))))
    (if cached
        (eq cached 'clear)
      (let ((start 0) (slash nil) (clear t))
        (while (and clear (setq slash (ngit--byte-pos path ?/ start)))
          (let ((st (ngit-wt-lstat (substring path 0 slash))))
            (when (and st (/= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir))
              (setq clear nil)))
          (setq start (1+ slash)))
        (when (and ngit--checkout-leading-cache dir)
          (puthash dir (if clear 'clear 'blocked) ngit--checkout-leading-cache))
        clear))))

(defun ngit--checkout-delete-path (repo path st)
  "Delete PATH itself without following it.  Directories must be empty."
  (when (ngit-windows-p) (ngit-win-validate-worktree-path path))
  (let ((abs (expand-file-name (decode-coding-string path 'utf-8)
                               (ngit-repo-toplevel repo))))
    (if (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir)
        (ngit-delete-directory abs)
      (ngit-delete-file abs))))

(defun ngit--checkout-leading-dirs (repo path)
  "Create PATH's leading directories, replacing file and symlink blockers."
  (when (ngit-windows-p) (ngit-win-validate-worktree-path path))
  (let ((start 0) (slash nil) (top (ngit-repo-toplevel repo)))
    (while (setq slash (ngit--byte-pos path ?/ start))
      (let* ((prefix (substring path 0 slash))
             (st (ngit-wt-lstat prefix)))
        (when (and st (/= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir))
          (ngit--checkout-delete-path repo prefix st)
          (setq st nil))
        (unless st
          (ngit-make-directory (expand-file-name (decode-coding-string prefix 'utf-8) top))))
      (setq start (1+ slash)))))

(defun ngit--checkout-entry-data (repo entry)
  (let ((obj (ngit-read-object repo (aref entry 1))))
    (unless (eq (car obj) 'blob)
      (error "fatal: object %s is not a blob" (ngit-hex (aref entry 1))))
    (cdr obj)))

(defun ngit--checkout-write-file (path data mode)
  "Create work-tree PATH exclusively with DATA and creation MODE."
  (if (ngit-windows-p)
      (ngit-win-write-file (ngit-wt-absolute path) data t nil)
   (let ((fd (syscall-direct ngit-native--sys-open (ngit-wt--set-path path)
                            ngit--checkout-o-wronly-creat-excl-cloexec
                            (ngit-native-creation-mode mode) 0 0 0)))
    (when (< fd 0)
      (error "ngit: cannot create %s (errno %d)" path (- fd)))
    (unwind-protect
        (let* ((owner (ngit-native--buffer 0 (length data)))
               (addr (nl-ffi-memory-address owner))
               (done 0))
          (ptr-write-bytes addr data)
          (while (< done (length data))
            (let ((n (syscall-direct ngit--checkout-sys-write fd (+ addr done)
                                     (- (length data) done) 0 0 0)))
              (when (<= n 0)
                (error "ngit: cannot write %s (errno %d)" path (if (< n 0) (- n) 0)))
              (setq done (+ done n)))))
      (syscall-direct ngit-native--sys-close fd 0 0 0 0 0)))))

(defun ngit-checkout-entry (repo entry)
  "Force ENTRY into REPO's work tree and return a refreshed new entry."
  (when (ngit-windows-p) (ngit-win-validate-worktree-path (aref entry 0)))
  (when ngit--checkout-leading-cache (clrhash ngit--checkout-leading-cache))
  (when (and (ngit-windows-p) (= (aref entry 2) ngit-mode-symlink))
    (error "ngit: Windows symbolic link checkout is unsupported"))
  (let* ((path (aref entry 0)) (mode (aref entry 2))
         (fmt (logand mode #o170000))
         (top (ngit-repo-toplevel repo))
         (abs (expand-file-name (decode-coding-string path 'utf-8) top))
         (symlinks (ngit-config-bool repo "core.symlinks" t)))
    (ngit-wt-setup top)
    (ngit--checkout-leading-dirs repo path)
    (let ((st (ngit-wt-lstat path)))
      (unless (and (= fmt ngit-mode-gitlink) st
                   (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir))
        (when st (ngit--checkout-delete-path repo path st))))
    (cond
     ((= fmt ngit-mode-gitlink)
      (unless (ngit-wt-lstat path) (ngit-make-directory abs)))
     ((and (= fmt ngit-mode-symlink) symlinks)
      (make-symbolic-link (decode-coding-string (ngit--checkout-entry-data repo entry) 'utf-8) abs))
     (t
      (let ((data (ngit--checkout-entry-data repo entry)))
        (when (= fmt #o100000) (setq data (ngit--checkout-smudge repo path data)))
        (ngit--checkout-write-file
         path data (if (= mode #o100755) #o777 #o666)))))
    (let ((st (or (ngit-wt-lstat path) (error "ngit: checkout did not create %s" path))))
      (ngit-make-entry path (aref entry 1) mode (ngit-wt-stat-block mode) (aref entry 5)))))

(defun ngit--checkout-stat-clean-p (repo index entry &optional options)
  "Check ENTRY's mode, full stat data and racy contents against INDEX.
OPTIONS, when non-nil, is [FILEMODE SYMLINKS] with work-tree setup done."
  (unless options (ngit-wt-setup (ngit-repo-toplevel repo)))
  (let ((pos (aref entry 4)) (raw (aref index 1))
        (mode (aref entry 2))
        (filemode (if options (aref options 0)
                    (ngit-config-bool repo "core.filemode" t))))
    (and pos (ngit--checkout-leading-clear-p (aref entry 0))
         (let ((st (ngit-entry-lstat index entry)))
           (and st
                (let ((wmode (ngit-wt-mode st)))
                  (when (or (and (not filemode)
                                 (= (logand mode #o170000) #o100000)
                                 (= (logand wmode #o170000) #o100000))
                            (and (= mode ngit-mode-symlink) (= wmode #o100644)
                                 (not (if options (aref options 1)
                                        (ngit-config-bool repo "core.symlinks" t))))
                            (and (= mode ngit-mode-gitlink) (= wmode ngit-mode-tree)))
                    (setq wmode mode))
                  (and (= wmode mode)
                       (ngit--stat-match-p raw pos st)
                       (= (ngit--be32 raw (+ pos 12))
                          (ptr-read-u64 (nl-ffi-memory-address ngit-wt--stat) 96))
                       (= (ngit--be32 raw (+ pos 4))
                          (ptr-read-u64 (nl-ffi-memory-address ngit-wt--stat) 112))
                       (not (ngit--racily-modified-p repo index entry filemode)))))))))

(defun ngit--checkout-stat-identical-p (index entry)
  "After a clean stat check, test the remaining on-disk stat fields."
  (let ((raw (aref index 1)) (pos (aref entry 4))
        (sa (nl-ffi-memory-address ngit-wt--stat)))
    (and (= (ngit--be32 raw (+ pos 16)) (logand (ptr-read-u64 sa 0) #xffffffff))
         (= (ngit--be32 raw (+ pos 28)) (ptr-read-u32 sa 28))
         (= (ngit--be32 raw (+ pos 32)) (ptr-read-u32 sa 32)))))

(defun ngit--checkout-clean-p (repo _index entry)
  "Non-nil when ENTRY already matches the work tree.
On success, leave `ngit-wt--stat' holding ENTRY's lstat result."
  (let* ((path (aref entry 0)) (mode (aref entry 2))
         (fmt (logand mode #o170000))
         (symlinks (ngit-config-bool repo "core.symlinks" t)))
    (ngit-wt-setup (ngit-repo-toplevel repo))
    (and (ngit--checkout-leading-clear-p path)
         (let ((st (ngit-wt-lstat path)))
           (and st
                (cond
                 ((= fmt ngit-mode-gitlink)
                  (and (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifdir)
                       (let ((head (ngit--gitlink-head repo path)))
                         (or (null head) (equal head (aref entry 1))))))
                 ((= fmt ngit-mode-symlink)
                  (if symlinks
                      (and (= (logand (aref st 0) ngit-s-ifmt) ngit-s-iflnk)
                           (equal (ngit-wt-readlink path)
                                  (ngit--checkout-entry-data repo entry)))
                    (and (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifreg)
                         (equal (ngit-wt-read path (aref st 2))
                                (ngit--checkout-entry-data repo entry)))))
                 (t
                  (and (= (logand (aref st 0) ngit-s-ifmt) ngit-s-ifreg)
                       (eq (/= 0 (logand (aref st 0) #o100)) (= mode #o100755))
                       (equal (ngit-wt-read path (aref st 2))
                              (ngit--checkout-smudge
                               repo path (ngit--checkout-entry-data repo entry)))))))))))

(defun ngit--unlink-entry (repo entry)
  "Delete ENTRY's work-tree path and now-empty parent directories safely."
  (when ngit--checkout-leading-cache (clrhash ngit--checkout-leading-cache))
  (let ((path (aref entry 0)) (top (ngit-repo-toplevel repo)))
    (ngit-wt-setup top)
    (when (ngit--checkout-leading-clear-p path)
      (let ((st (ngit-wt-lstat path)))
        (when st
          (condition-case nil
              (ngit--checkout-delete-path repo path st)
            (file-error nil))))
      (let ((slash (let ((i (1- (length path))))
                     (while (and (>= i 0) (/= (aref path i) ?/)) (setq i (1- i))) i)))
        (while (>= slash 0)
          (let ((dir (substring path 0 slash)))
            (condition-case nil
                (progn
                  (ngit-delete-directory
                   (expand-file-name (decode-coding-string dir 'utf-8) top))
                  (setq slash
                        (let ((i (1- slash)))
                          (while (and (>= i 0) (/= (aref path i) ?/))
                            (setq i (1- i)))
                          i)))
              (file-error (setq slash -1)))))))))

(defun ngit--checkout-update-index-stat (index updates)
  "Patch INDEX's old entry stat blocks from (POS . STAT) UPDATES."
  (when updates
    (let ((raw (aref index 1)) (parts nil) (at 0))
      (dolist (one (sort (copy-sequence updates)
                         (lambda (a b) (< (car a) (car b)))))
        (push (substring raw at (car one)) parts)
        (push (cdr one) parts)
        (setq at (+ (car one) 40)))
      (push (substring raw at) parts)
      (aset index 1 (apply #'concat (nreverse parts))))))

(defun ngit--checkout-match-p (items path)
  (eq (ngit-command-path-class items path nil) 'full))

(defun ngit--checkout-item-matches (items path matched)
  (let ((i 0))
    (dolist (item items)
      (when (eq (ngit-command-path-class (list item) path nil) 'full)
        (aset matched i t))
      (setq i (1+ i)))))

(defun ngit--checkout-walk-source (repo tree prefix emit)
  "Visit only source subtrees relevant to `ngit--rv-paths'."
  (dolist (entry (ngit-tree-items repo tree))
    (let* ((mode (nth 0 entry)) (path (concat prefix (nth 1 entry)))
           (dirp (= mode ngit-mode-tree)) (class (ngit-command-path-class ngit--rv-paths path dirp)))
      (cond
       ((and dirp (eq class 'full))
        (ngit--flatten-tree repo (nth 2 entry) (concat path "/") emit))
       ((and dirp class)
        (ngit--checkout-walk-source repo (nth 2 entry) (concat path "/") emit))
       (class (funcall emit path mode (nth 2 entry)))))))

(defun ngit--checkout-source-entries (repo tree items matched)
  (let* ((out nil) (old (ngit-index-entries repo))
         (seen (make-hash-table :test 'equal))
         (ngit--rv-paths items)
         (emit
          (lambda (path mode bin)
            (unless (gethash path seen)
              (puthash path t seen)
              (let ((old-e (ngit-index-lookup old path)))
                (unless (and old-e (= (aref old-e 3) 0)
                             (not (memq 'ita (aref old-e 5)))
                             (memq 'skip-worktree (aref old-e 5))
                             (= (aref old-e 2) mode) (equal (aref old-e 1) bin))
                  (ngit--checkout-item-matches items path matched)
                  (push (ngit-make-entry path bin mode (make-string 40 0)) out)))))))
    (if (seq-some #'cdr items)
        (ngit--checkout-walk-source repo tree "" emit)
      ;; Literal paths are resolved directly, including directory pathspecs.
      (dolist (item items)
        (let* ((path (car item)) (entry (ngit--literal-entry repo tree path)))
          (when entry
            (if (= (car entry) ngit-mode-tree)
                (ngit--flatten-tree repo (cdr entry)
                                    (if (string= path "") "" (concat path "/")) emit)
              (funcall emit path (car entry) (cdr entry)))))))
    (sort out #'ngit--entry-less)))

(defun ngit--checkout-index-entries (entries items matched)
  (let ((out nil) (i 0))
    (while (< i (length entries))
      (let ((e (aref entries i)))
        (when (and (= (aref e 3) 0) (not (memq 'skip-worktree (aref e 5))))
          (ngit--checkout-item-matches items (aref e 0) matched)
          (when (ngit--checkout-match-p items (aref e 0)) (push e out))))
      (setq i (1+ i)))
    (nreverse out)))

(defun ngit--checkout-overlay (old targets &optional removed)
  "Overlay sorted TARGETS on OLD, removing only topology conflicts."
  (ngit-index-overlay old targets nil removed))

(defvar ngit--checkout-as-restore nil)
(defvar ngit--checkout-stage-removals nil)

(defun ngit--checkout-unresolve (repo index items)
  "Recreate selected conflict stages from resolve-undo, consuming their records."
  (let* ((undo (aref index 8)) (old (aref index 0))
         (root (ngit-ct-root index))
         (pos 0) (limit (length (or undo "")))
         (targets nil) (kept nil) (drop (make-hash-table :test 'equal)))
    (while (< pos limit)
      (let* ((start pos) (nul (ngit--byte-pos undo 0 pos))
             (name (substring undo pos nul)) (current (ngit-index-lookup old name))
             (selected (and (or (null current) (= (aref current 3) 0))
                            (ngit--checkout-match-p items name)))
             (modes (make-vector 4 0)))
        (setq pos (1+ nul))
        (dotimes (i 3)
          (setq nul (ngit--byte-pos undo 0 pos))
          (aset modes (1+ i) (string-to-number (substring undo pos nul) 8))
          (setq pos (1+ nul)))
        (dotimes (i 3)
          (let* ((stage (1+ i)) (mode (aref modes stage)))
            (unless (= mode 0)
              (when selected
                (let ((entry (ngit-make-entry name (substring undo pos (+ pos 20)) mode
                                               (concat (make-string 24 0) (ngit--be32-bytes mode)
                                                       (make-string 12 0)))))
                  (aset entry 3 stage)
                  (push entry targets)))
              (setq pos (+ pos 20)))))
        (if selected (progn (puthash name t drop) (when root (ngit-ct-invalidate root name)))
          (push (substring undo start pos) kept))))
    (when targets
      (aset index 8 (apply #'concat (nreverse kept)))
      (aset index 0 (ngit-index-overlay old targets drop)))
    (aref index 0)))

(defun ngit--checkout-conflict-entry (repo path stages style)
  "Recreate a work-tree conflict from the index's three versions."
  (require 'ngit-merge-file)
  (let* ((base (aref stages 1)) (ours (aref stages 2)) (theirs (aref stages 3))
         (attrs (ngit-attrs repo path '("conflict-marker-size" "merge")))
         (marker (cdr (assoc "conflict-marker-size" attrs)))
         (b (if base (ngit--checkout-entry-data repo base) ""))
         (o (ngit--checkout-entry-data repo ours))
         (tdata (ngit--checkout-entry-data repo theirs))
         (binary (or (eq (cdr (assoc "merge" attrs)) 'unset)
                     (equal (cdr (assoc "merge" attrs)) "binary")
                     (ngit--byte-pos b 0 0) (ngit--byte-pos o 0 0) (ngit--byte-pos tdata 0 0)))
         (data (if binary o
                 (car (ngit-merge-file b o tdata :labels '("base" "ours" "theirs")
                                      :favor (if (equal (cdr (assoc "merge" attrs)) "union") 'union 'none)
                                      :style (intern style)
                                      :marker-size (if (and (stringp marker) (> (string-to-number marker) 0))
                                                       (string-to-number marker) 7))))))
    (when binary (ngit-native-write-stderr (concat "warning: Cannot merge binary files: " path " (ours vs. theirs)\n")))
    (ngit-make-entry path (ngit-write-object repo "blob" data) (aref ours 2) (make-string 40 0))))

(defun ngit--checkout-unmerged-targets (repo old items matched stage merge style overlay)
  "Preflight selected unmerged entries and return their work-tree versions."
  (let ((i 0) (targets nil) (errors nil))
    (while (< i (length old))
      (let* ((e (aref old i)) (path (aref e 0)))
        (if (= (aref e 3) 0) (setq i (1+ i))
          (let* ((group (ngit-index-group old i)) (stages (cdr group)))
            (setq i (car group))
            (when (ngit--checkout-match-p items path)
              (ngit--checkout-item-matches items path matched)
              (cond
               (stage
                (if (aref stages stage) (push (aref stages stage) targets)
                  (if (or ngit--checkout-as-restore (not overlay))
                      (push e ngit--checkout-stage-removals)
                    (push (format "error: path '%s' does not have %s version\n" path (if (= stage 2) "our" "their")) errors))))
               (merge
                (if (and (aref stages 2) (aref stages 3))
                    (push (ngit--checkout-conflict-entry repo path stages style) targets)
                  (push (format "error: path '%s' does not have all necessary versions\n" path) errors)))
               (t (push (format "error: path '%s' is unmerged\n" path) errors))))))))
    (when errors
      (ngit-native-write-stderr (apply #'concat (nreverse errors)))
      (ngit--die-with 1 ""))
    (nreverse targets)))

(defun ngit--checkout-parse (repo args)
  "Return (TREE SOURCE PATHS EXPLICIT QUIET) for checkout ARGS."
  (let ((quiet nil) (before nil) (after nil) (explicit nil) (overlay t) (stage nil) (merge nil) (force nil) (style (or (ngit-config-get repo "merge.conflictstyle") "merge")))
    (while args
      (let ((a (pop args)))
        (cond ((string= a "--") (setq explicit t after args args nil))
              ((member a '("-q" "--quiet")) (setq quiet t))
              ((string= a "--overlay") (setq overlay t))
              ((string= a "--no-overlay") (setq overlay nil))
              ((member a '("-2" "--ours")) (setq stage 2))
              ((member a '("-3" "--theirs")) (setq stage 3))
              ((member a '("-m" "--merge")) (setq merge t))
              ((string-prefix-p "--conflict=" a) (setq merge t style (substring a 11)))
              ((member a '("-f" "--force")) (setq force t))
              ((member a '("--guess" "--no-guess")) nil)
              ((string-prefix-p "-" a)
               (ngit--die-with 129 "error: checkout option '%s' is not supported" a))
              (t (push a before)))))
    (when (or (and stage merge) (and force (or stage merge)))
      (ngit--die "fatal: %s: --ours/--theirs, --force and --merge are incompatible when\nchecking out of the index."
                 (if ngit--checkout-as-restore "git restore" "git checkout")))
    (unless (member style '("merge" "diff3" "zdiff3"))
      (ngit--die "fatal: unknown style '%s' given for 'merge.conflictstyle'" style))
    (setq before (nreverse before))
    (let ((result (if explicit
        (if before
            (let ((tree (condition-case nil
                            (ngit-unhex (ngit-peel repo (ngit-rev-parse repo (car before)) 'tree))
                          (error (ngit--die "fatal: invalid reference: %s" (car before))))))
              (when (cdr before) (ngit--die "fatal: only one reference expected"))
              (list tree (ngit-hex tree) after t quiet))
          (list nil nil after t quiet))
      (let ((tree (and before
                       (condition-case nil
                           (ngit-unhex (ngit-peel repo (ngit-rev-parse repo (car before)) 'tree))
                         (error nil)))))
        (cond ((and tree (null (cdr before)))
               (ngit--die "fatal: checkout branch switching is not supported"))
              (tree (list tree (ngit-hex tree) (cdr before) nil quiet))
              (t (list nil nil before nil quiet)))))))
      (when (and (car result) (or stage merge))
        (ngit--die "fatal: '%s' cannot be used with switching branches" (if stage "--ours/--theirs" "--merge")))
      (append result (list overlay stage merge style)))))

(defun ngit--checkout-path-command (repo args)
  (let* ((parsed (ngit--checkout-parse repo args))
         (tree (nth 0 parsed)) (source (nth 1 parsed))
         (paths (nth 2 parsed)) (explicit (nth 3 parsed)) (quiet (nth 4 parsed))
         (overlay (nth 5 parsed)) (stage (nth 6 parsed)) (merge (nth 7 parsed)) (style (nth 8 parsed))
         (ngit--checkout-stage-removals nil))
    (when (member "" paths)
      (ngit--die "fatal: empty string is not a valid pathspec. please use . instead if you meant to match all paths"))
    (unless paths (ngit--die-with 1 "fatal: you must specify path(s) to restore"))
    (let* ((items (ngit-command-path-items repo paths))
           (matched (vconcat (mapcar (lambda (item)
                                       (and (not tree) (vectorp (car item))
                                            (aref (car item) 4))) items)))
           (index (ngit-read-index repo))
           (old (if (and merge (not tree) (aref index 8))
                    (ngit--checkout-unresolve repo index items)
                  (aref index 0)))
           (targets (if tree
                        (ngit--checkout-source-entries repo tree items matched)
                      (ngit--checkout-index-entries old items matched)))
           (selected (and tree (not overlay) (ngit--checkout-index-entries old items matched)))
           (mi 0) (misses nil))
      (unless tree
        (setq targets (append targets (ngit--checkout-unmerged-targets repo old items matched stage merge style overlay))))
      (dolist (path paths)
        (unless (aref matched mi)
          (push (format "error: pathspec '%s' did not match any file(s) known to git\n" path) misses))
        (setq mi (1+ mi)))
      (when misses
        (ngit-native-write-stderr (apply #'concat (nreverse misses)))
        (ngit--die-with 1 ""))
      (let* ((removed ngit--checkout-stage-removals)
             (drop (and tree (not overlay) (make-hash-table :test 'equal)))
             (_ (when drop
                  (dolist (e selected)
                    (unless (seq-find (lambda (target) (string= (aref target 0) (aref e 0))) targets)
                      (puthash (aref e 0) t drop)
                      (push e removed)))))
             (entries (if tree
                          (ngit-index-overlay old targets drop (lambda (e) (push e removed)))
                        old))
             (root (ngit-ct-root index))
             (checked (make-hash-table :test 'eql))
             (updates nil) (count (if drop (hash-table-count drop) 0))
             (_ (progn (ngit-win-validate-worktree-records old)
                       (ngit-win-validate-worktree-records removed)
                       (ngit-win-validate-worktree-records targets)
                       (ngit-win-validate-worktree-records entries)))
             (lock (ngit-lock (ngit-index-file repo)))
             (ngit--checkout-attr-cache (make-hash-table :test 'equal)))
        (unwind-protect
            (progn
              ;; Topology conflicts disappear from the overlay.  Remove
              ;; their tracked work-tree paths before creating replacements.
              (dolist (e (sort (delete-dups removed) #'ngit--entry-less))
                (when root (ngit-ct-invalidate root (aref e 0)))
                (ngit--unlink-entry repo e))
              (aset index 0 entries)
              (dolist (target targets)
                (let* ((name (aref target 0))
                       (pos (ngit-index-find entries name))
                       (old-pos (ngit-index-find old name))
                       (old-e (and (< old-pos (length old)) (aref old old-pos)))
                       (same (and old-e (= (aref old-e 3) 0)
                                  (not (memq 'ita (aref old-e 5)))
                                  (string= (aref old-e 0) name)
                                  (= (aref old-e 2) (aref target 2))
                                  (equal (aref old-e 1) (aref target 1))))
                       (stat-clean (and same (ngit--checkout-stat-clean-p repo index old-e)))
                       (clean (or stat-clean (ngit--checkout-clean-p repo index target)))
                       (fresh (if clean
                                  (prog1
                                      (ngit-make-entry name (aref target 1) (aref target 2)
                                                       (ngit-wt-stat-block (aref target 2))
                                                       (aref target 5))
                                    (unless stat-clean
                                      (setq count (1+ count))))
                                (setq count (1+ count))
                                (ngit-checkout-entry repo target))))
                  (if (and (not tree) old-e (/= (aref old-e 3) 0)) nil
                  (if (and same (aref old-e 4))
                      (progn
                        (push (cons (aref old-e 4) (aref fresh 6)) updates)
                        (puthash (aref old-e 4) t checked)
                        (aset entries pos old-e))
                    (aset entries pos fresh)
                    (when root (ngit-ct-invalidate root name))))))
              (ngit--checkout-update-index-stat index updates)
              (ngit-commit-lock lock (ngit-index-bytes repo index entries root checked))
              (setq lock nil)
              (aset repo 6 nil))
          (when lock (ngit-unlock lock)))
        (when (and (not quiet) (not explicit))
          (ngit-native-write-stderr
           (format "Updated %d %s from %s\n" count
                   (if (= count 1) "path" "paths")
                   (if tree (ngit-abbrev repo source) "the index"))))
        (let ((head (ngit-resolve-ref repo "HEAD")))
          (unless ngit--checkout-as-restore (ngit-hook-run repo "post-checkout"
                         (list (or head (make-string 40 ?0))
                               (or head (make-string 40 ?0)) "0"))))))))

(defun ngit-cmd-checkout (repo args)
  "Run checkout, loading branch switching only when this command needs it."
  (require 'ngit-switch)
  (ngit-dispatch-checkout repo args))

(provide 'ngit-checkout)

;;; ngit-checkout.el ends here
