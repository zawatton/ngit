;;; ngit-main.el --- command-line entry point for ngit -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage: nelisp --load ngit-main.el -- COMMAND ARGS...
;; Exits with the command's status via `kill-emacs'.

;;; Code:

(defconst ngit-main--lazy-modules
  '(("init" "ngit-rename" "ngit-log" "ngit-branch" "ngit-remote")
    ("remote" "ngit-rename" "ngit-log" "ngit-branch" "ngit-remote")
    ("clone" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-objects" "ngit-pack" "ngit-transport-kernel" "ngit-transport-delta" "ngit-transport-index" "ngit-transport-checkout" "ngit-remote" "ngit-transport-process" "ngit-negotiate" "ngit-transport")
    ("fetch" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-objects" "ngit-pack" "ngit-transport-kernel" "ngit-transport-delta" "ngit-transport-index" "ngit-transport-checkout" "ngit-remote" "ngit-transport-process" "ngit-negotiate" "ngit-transport")
    ("ls-remote" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-objects" "ngit-pack" "ngit-transport-kernel" "ngit-transport-delta" "ngit-transport-index" "ngit-transport-checkout" "ngit-remote" "ngit-transport-process" "ngit-negotiate" "ngit-transport")
    ("push" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-objects" "ngit-pack" "ngit-transport-kernel" "ngit-transport-delta" "ngit-transport-index" "ngit-transport-checkout" "ngit-remote" "ngit-transport-process" "ngit-negotiate" "ngit-transport")
    ("pull" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-merge-file" "ngit-merge-rename" "ngit-merge-tree" "ngit-merge" "ngit-sequencer" "ngit-objects" "ngit-pack" "ngit-transport-kernel" "ngit-transport-delta" "ngit-transport-index" "ngit-transport-checkout" "ngit-remote" "ngit-transport-process" "ngit-negotiate" "ngit-transport")
    ("pack-objects" "ngit-objects" "ngit-pack")
    ("index-pack" "ngit-objects" "ngit-pack")
    ("verify-pack" "ngit-objects" "ngit-pack")
    ("repack" "ngit-objects" "ngit-pack" "ngit-maint")
    ("gc" "ngit-objects" "ngit-pack" "ngit-maint")
    ("prune" "ngit-objects" "ngit-pack" "ngit-maint")
    ("pack-refs" "ngit-objects" "ngit-maint")
    ("count-objects" "ngit-objects" "ngit-maint")
    ("reflog" "ngit-objects" "ngit-maint")
    ("fsck" "ngit-objects" "ngit-pack" "ngit-fsck")
    ("log" "ngit-rename" "ngit-log")
    ("blame" "ngit-rename" "ngit-log" "ngit-lines" "ngit-blame")
    ("grep" "ngit-rename" "ngit-log" "ngit-lines" "ngit-grep")
    ("describe" "ngit-rename" "ngit-log" "ngit-describe")
    ("name-rev" "ngit-rename" "ngit-log" "ngit-describe")
    ("shortlog" "ngit-rename" "ngit-log" "ngit-describe")
    ("rev-list" "ngit-rename" "ngit-log" "ngit-describe")
    ("merge-file" "ngit-merge-file")
    ("merge" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-merge-file" "ngit-merge-rename" "ngit-merge-tree" "ngit-merge")
    ("cherry-pick" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-merge-file" "ngit-merge-rename" "ngit-merge-tree" "ngit-merge" "ngit-sequencer")
    ("revert" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-merge-file" "ngit-merge-rename" "ngit-merge-tree" "ngit-merge" "ngit-sequencer")
    ("rebase" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-merge-file" "ngit-merge-rename" "ngit-merge-tree" "ngit-merge" "ngit-sequencer")
    ("show" "ngit-rename" "ngit-log" "ngit-show")
    ("format-patch" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-merge-file" "ngit-merge-rename" "ngit-merge-tree" "ngit-merge" "ngit-sequencer" "ngit-patch")
    ("apply" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-merge-file" "ngit-merge-rename" "ngit-merge-tree" "ngit-merge" "ngit-sequencer" "ngit-patch")
    ("am" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-merge-file" "ngit-merge-rename" "ngit-merge-tree" "ngit-merge" "ngit-sequencer" "ngit-patch")
    ("clean" "ngit-clean")
    ("worktree" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-worktree-cmd")
    ("config" "ngit-rename" "ngit-log" "ngit-branch" "ngit-config-cmd")
    ("bisect" "ngit-rename" "ngit-log" "ngit-show" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-bisect")
    ("branch" "ngit-rename" "ngit-log" "ngit-branch")
    ("reset" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset")
    ("switch" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch")
    ("checkout" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch")
    ("rm" "ngit-rename" "ngit-log" "ngit-checkout" "ngit-rm")
    ("mv" "ngit-rename" "ngit-log" "ngit-checkout" "ngit-rm")
    ("restore" "ngit-rename" "ngit-log" "ngit-checkout" "ngit-restore")
    ("stash" "ngit-rename" "ngit-log" "ngit-branch" "ngit-checkout" "ngit-reset" "ngit-switch" "ngit-merge-file" "ngit-merge-rename" "ngit-merge-tree" "ngit-stash")
    ("tag" "ngit-rename" "ngit-log" "ngit-branch" "ngit-tag"))
  "Commands whose large modules load only when an argument names them.
A false hit (a path argument spelled like a command) only costs time.")

(let ((dir (file-name-directory (or load-file-name buffer-file-name)))
      (args (cdr (member "--" command-line-args)))
      (files (list "ngit-native" "ngit-store" "ngit-rev" "ngit-pathspec" "ngit-paths" "ngit-cli" "ngit-config"
                   "ngit-match" "ngit-index" "ngit-worktree" "ngit-changes"
                   "ngit-status" "ngit-xdiff" "ngit-diff" "ngit-write" "ngit-add"
                   "ngit-commit")))
  (setq load-path (cons dir load-path))
  (dolist (entry ngit-main--lazy-modules)
    (when (or (member (car entry) args) (member (concat "@" (car entry)) args))
      (dolist (f (cdr entry))
        (unless (member f files) (setq files (append files (list f)))))))
  ;; tools/build-elc.sh compiles every module and then touches .elc-stamp.
  ;; Byte code skips NeLisp's first-call cost for each function (tens of
  ;; ms), so use it only when the stamp is newer than every source file;
  ;; otherwise load all sources.  NeLisp's `require' always reads the .el,
  ;; so ngit features are routed to their .elc too.
  (let* ((stamp (file-attributes (expand-file-name ".elc-stamp" dir)))
         (elc (and stamp (not (getenv "NGIT_NO_ELC"))
                   (let ((time (float-time (nth 5 stamp))) (fresh t))
                     (dolist (el (directory-files dir t "\\.el\\'") fresh)
                       (when (> (float-time (nth 5 (file-attributes el))) time)
                         (setq fresh nil)))))))
    (when elc
      (let ((require-function (symbol-function 'require)))
        (fset 'require
              (lambda (feature &optional filename noerror)
                (let ((file (and (not filename) (not (featurep feature))
                                 (expand-file-name (concat (symbol-name feature) ".elc") dir))))
                  (if (and file (string-prefix-p "ngit-" (symbol-name feature))
                           (file-exists-p file))
                      (progn (load file nil t) feature)
                    (funcall require-function feature filename noerror)))))))
    (dolist (f files)
      (let ((source (expand-file-name (concat f ".el") dir))
            (compiled (expand-file-name (concat f ".elc") dir)))
        (unless (or (featurep (intern f)) (not (file-exists-p source)))
          (load (if (and elc (file-exists-p compiled)) compiled source) nil t)))))
  (when (equal (getenv "NGIT_PROFILE") "1")
    (load (expand-file-name "ngit-profile.el" dir) nil t)))

(let ((args (cdr (member "--" command-line-args))))
  ;; bin/ngit prefixes each argument with "@" (see there).
  (when (equal (car args) "--@")
    (setq args (mapcar (lambda (a) (substring a 1)) (cdr args))))
  (kill-emacs (ngit-main args)))

;;; ngit-main.el ends here
