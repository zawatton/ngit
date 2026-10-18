;;; ngit-config.el --- git config files -*- lexical-binding: t; -*-

;;; Commentary:

;; Reads the system, global and repository config files in git's order and
;; answers last-one-wins lookups.  Keys are normalized like git: section and
;; variable names lowercased, subsection kept verbatim.  `include' and
;; `includeIf' are not followed.

;;; Code:

(require 'ngit-store)

(defun ngit--config-files (repo)
  "Return the config files of REPO, lowest precedence first."
  (let ((home (getenv "HOME"))
        (xdg (getenv "XDG_CONFIG_HOME"))
        (global (getenv "GIT_CONFIG_GLOBAL"))
        (files nil))
    (unless (member (getenv "GIT_CONFIG_NOSYSTEM") '("1" "true" "yes"))
      (push (or (getenv "GIT_CONFIG_SYSTEM") "/etc/gitconfig") files))
    (if global
        (push global files)
      (push (expand-file-name "git/config"
                              (if (and xdg (not (string= xdg "")))
                                  xdg
                                (expand-file-name ".config" home)))
            files)
      (when home (push (expand-file-name ".gitconfig" home) files)))
    (when repo
      (push (expand-file-name "config" (ngit-repo-commondir repo)) files))
    (nreverse files)))

(defun ngit--config-value (line pos)
  "Parse a config value in LINE from POS: quotes, escapes, comments."
  (let ((n (length line)) (out nil) (quoted nil) (done nil) (pending-space nil))
    (while (and (< pos n) (not done))
      (let ((c (aref line pos)))
        (cond
         ((and (not quoted) (memq c '(?\; ?#))) (setq done t))
         ((= c ?\") (setq quoted (not quoted)))
         ((= c ?\\)
          (setq pos (1+ pos))
          (when (< pos n)
            (when pending-space (push pending-space out) (setq pending-space nil))
            (push (pcase (aref line pos) (?n "\n") (?t "\t") (?b "\b")
                         (c2 (string c2)))
                  out)))
         ((and (not quoted) (memq c '(?\s ?\t)))
          (when out (setq pending-space (concat pending-space (string c)))))
         (t
          (when pending-space (push pending-space out) (setq pending-space nil))
          (push (string c) out)))
        (setq pos (1+ pos))))
    (apply #'concat (nreverse out))))

(defun ngit--config-parse (text)
  "Parse config TEXT into a list of (KEY . VALUE), in file order."
  (let ((section nil) (entries nil)
        (lines (split-string (replace-regexp-in-string "\\\\\r?\n" "" text) "\n")))
    (dolist (raw lines)
      (let ((line (string-trim-left raw)))
        (cond
         ((or (string= line "") (memq (aref line 0) '(?# ?\;))))
         ((string-match "\\`\\[\\([-A-Za-z0-9.]+\\)\\(?:[ \t]+\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"\\)?\\][ \t]*\\(.*\\)\\'" line)
          (let ((name (downcase (match-string 1 line)))
                (sub (match-string 2 line))
                (rest (match-string 3 line)))
            (setq section
                  (cond (sub (concat name "." (replace-regexp-in-string "\\\\\\(.\\)" "\\1" sub)))
                        ((string-match "\\`\\([^.]+\\)\\.\\(.*\\)\\'" name)
                         (concat (match-string 1 name) "." (match-string 2 name)))
                        (t name)))
            (setq line rest))
          (when (string-match "\\`\\([A-Za-z][-A-Za-z0-9]*\\)[ \t]*\\(=\\)?" line)
            (push (cons (concat section "." (downcase (match-string 1 line)))
                        (if (match-beginning 2)
                            (ngit--config-value line (match-end 0))
                          t))
                  entries)))
         ((and section (string-match "\\`\\([A-Za-z][-A-Za-z0-9]*\\)[ \t]*\\(=\\)?" line))
          (push (cons (concat section "." (downcase (match-string 1 line)))
                      (if (match-beginning 2)
                          (ngit--config-value line (match-end 0))
                        t))
                entries)))))
    (nreverse entries)))

(defvar ngit--config-cache nil "(REPO . ENTRIES) of the last config read.")

(defun ngit-config-entries (repo)
  "Return all config entries of REPO, lowest precedence first."
  (if (and ngit--config-cache (eq (car ngit--config-cache) repo))
      (cdr ngit--config-cache)
    (let ((entries nil))
      (dolist (f (ngit--config-files repo))
        (when (file-readable-p f)
          (setq entries (append entries
                                (ngit--config-parse
                                 (decode-coding-string (ngit--read-file f) 'utf-8))))))
      (setq ngit--config-cache (cons repo entries))
      entries)))

(defun ngit--config-normalize (key)
  "Lowercase the section and variable parts of KEY, keep the subsection."
  (let ((first (string-search "." key))
        (last (let ((i (1- (length key)))) (while (and (>= i 0) (/= (aref key i) ?.)) (setq i (1- i))) i)))
    (if (or (null first) (= first last))
        (downcase key)
      (concat (downcase (substring key 0 first)) (substring key first last)
              (downcase (substring key last))))))

(defun ngit-config-get (repo key)
  "Return the last value of KEY in REPO's config: a string, t, or nil."
  (let ((key (ngit--config-normalize key)) (val nil))
    (dolist (e (ngit-config-entries repo))
      (when (string= (car e) key) (setq val (cdr e))))
    val))

(defun ngit-config-get-all (repo key)
  (let ((key (ngit--config-normalize key)) (vals nil))
    (dolist (e (ngit-config-entries repo))
      (when (string= (car e) key) (push (cdr e) vals)))
    (nreverse vals)))

(defun ngit-config-bool (repo key default)
  "Return KEY as a boolean, DEFAULT when unset."
  (let ((v (ngit-config-get repo key)))
    (cond ((null v) default)
          ((eq v t) t)
          ((member (downcase v) '("true" "yes" "on" "1")) t)
          ((member (downcase v) '("false" "no" "off" "0" "")) nil)
          (t default))))

(provide 'ngit-config)

;;; ngit-config.el ends here
