;;; ngit-config-parameters.el --- command-scope config -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded only for -c or the GIT_CONFIG parameter environment variables.

;;; Code:

(defvar ngit--config-parameters-quiet nil)

(defun ngit-config-parameters--quote (text)
  "Quote TEXT with Git's sq_quote_buf format, including exclamation marks."
  (concat "'" (replace-regexp-in-string "['!]" (lambda (match) (concat "'\\" match "'")) text t t) "'"))

(defun ngit-config-parameters--push-pair (key value)
  "Export KEY and VALUE for hooks and child Git with Git's split quoting."
  (let ((old (getenv "GIT_CONFIG_PARAMETERS")))
    (ngit-hook-setenv
     "GIT_CONFIG_PARAMETERS"
     (concat (if (and old (not (string= old ""))) (concat old " ") "")
             (ngit-config-parameters--quote key) "="
             (if value (ngit-config-parameters--quote value) "")))))

(defun ngit-config-parameters--push (text)
  "Split a -c TEXT at the first equals, preserving the rest of its value."
  (let ((equal (string-search "=" text)))
    (ngit-config-parameters--push-pair
     (if equal (substring text 0 equal) text)
     (and equal (substring text (1+ equal))))))

(defun ngit-config-parameters--usage (message)
  "Report a global-option MESSAGE followed by Git's usage."
  (ngit--die-with
   129 "%s" (concat message "\n"
               "usage: git [-v | --version] [-h | --help] [-C <path>] [-c <name>=<value>]\n"
               "           [--exec-path[=<path>]] [--html-path] [--man-path] [--info-path]\n"
               "           [-p | --paginate | -P | --no-pager] [--no-replace-objects] [--no-lazy-fetch]\n"
               "           [--no-optional-locks] [--no-advice] [--bare] [--git-dir=<path>]\n"
               "           [--work-tree=<path>] [--namespace=<name>] [--config-env=<name>=<envvar>]\n"
               "           <command> [<args>]")))

(defun ngit-config-parameters--missing-argument ()
  (ngit-config-parameters--usage "-c expects a configuration string"))

(defun ngit-config-parameters--push-env (spec)
  "Split a --config-env SPEC at the last equals and export its value."
  (let ((last (1- (length spec))))
    (while (and (>= last 0) (/= (aref spec last) ?=)) (setq last (1- last)))
    (when (< last 0) (ngit--die "fatal: invalid config format: %s" spec))
    (let* ((key (substring spec 0 last)) (name (substring spec (1+ last)))
           (value (getenv name)))
      (when (string= name "")
        (ngit--die "fatal: missing environment variable name for configuration '%s'" key))
      (unless value
        (ngit--die "fatal: missing environment variable '%s' for configuration '%s'" name key))
      (ngit-config-parameters--push-pair key value))))

(defun ngit-config-parameters--pair (text)
  "Split the first equals in a -c or legacy environment parameter TEXT."
  (let ((equal (string-search "=" text)))
    (cons (ngit-config-parameters--key (string-trim (if equal (substring text 0 equal) text)))
          (if equal (substring text (1+ equal)) t))))

(defun ngit-config-parameters--key (key)
  "Validate and normalize a command-scope KEY like config_parse_pair."
  (cond ((string= key "") (ngit-config-parameters--error "empty config key"))
        ((not (string-match "\\.[^.]*\\'" key))
         (ngit-config-parameters--error (concat "key does not contain a section: " key)))
        ((= (match-beginning 0) 0)
         (ngit-config-parameters--error (concat "key does not contain a section: " key)))
        ((string-suffix-p "." key)
         (ngit-config-parameters--error (concat "key does not contain variable name: " key))))
  (unless (and (string-match-p "\\`[-A-Za-z0-9]+\\." key)
               (string-match-p "\\.[A-Za-z][-A-Za-z0-9]*\\'" key))
    (ngit-config-parameters--error (concat "invalid key: " key)))
  (when (string-search "\n" key)
    (ngit-config-parameters--error (concat "invalid key (newline): " key)))
  (ngit--config-normalize key))

(defun ngit-config-parameters--unquote (text start)
  "Read Git's single-quoted word at START; return (VALUE . END)."
  (unless (and (< start (length text)) (= (aref text start) ?'))
    (ngit-config-parameters--bad-format))
  (let ((position (1+ start)) (parts nil) (done nil))
    (while (not done)
      (let ((end (string-search "'" text position)))
        (unless end (ngit-config-parameters--bad-format))
        (push (substring text position end) parts)
        (setq position (1+ end))
        (if (and (< (+ position 2) (length text))
                 (= (aref text position) ?\\)
                 (memq (aref text (1+ position)) '(?' ?!))
                 (= (aref text (+ position 2)) ?'))
            (progn
              (push (substring text (1+ position) (+ position 2)) parts)
              (setq position (+ position 3)))
          (setq done t))))
    (cons (apply #'concat (nreverse parts)) position)))

(defun ngit-config-parameters--error (message)
  (unless ngit--config-parameters-quiet
    (ngit-native-write-stderr (concat "error: " message "\n")))
  (ngit--die "fatal: unable to parse command-line config"))

(defun ngit-config-parameters--bad-format ()
  (ngit-config-parameters--error "bogus format in GIT_CONFIG_PARAMETERS"))

(defun ngit-config-parameters--environment ()
  "Read count/key/value entries followed by Git's quoted parameter list."
  (let ((count (getenv "GIT_CONFIG_COUNT")) (text (getenv "GIT_CONFIG_PARAMETERS"))
        (entries nil))
    (when count
      ;; strtoul uses the C whitespace set, unlike Git's sane isspace
      ;; used below (which excludes vertical tab and form feed).
      (unless (or (string= count "") (string-match-p "\\`[ \t\n\r\f\013]*[-+]?[0-9]+\\'" count))
        (ngit-config-parameters--error "bogus count in GIT_CONFIG_COUNT"))
      (let ((number (string-to-number (string-trim-left count "[ \t\n\r\f\013]+"))))
        (when (or (< number 0) (> number 2147483647))
          (ngit-config-parameters--error "too many entries in GIT_CONFIG_COUNT"))
        (dotimes (index number)
          (let ((key (getenv (format "GIT_CONFIG_KEY_%d" index)))
                (value (getenv (format "GIT_CONFIG_VALUE_%d" index))))
            (unless key (ngit-config-parameters--error (format "missing config key GIT_CONFIG_KEY_%d" index)))
            (unless value (ngit-config-parameters--error (format "missing config value GIT_CONFIG_VALUE_%d" index)))
            (push (cons (ngit-config-parameters--key key) value) entries)))))
    (when text
      (let ((position 0) (size (length text)))
        (while (< position size)
          (let* ((word (ngit-config-parameters--unquote text position))
                 (key (car word)))
            (setq position (cdr word))
            (if (and (< position size) (= (aref text position) ?=))
                (progn
                  (setq position (1+ position))
                  (if (and (< position size) (= (aref text position) ?'))
                      (let ((value (ngit-config-parameters--unquote text position)))
                        (push (cons (ngit-config-parameters--key key) (car value)) entries)
                        (setq position (cdr value)))
                    (push (cons (ngit-config-parameters--key key) t) entries)))
              (push (ngit-config-parameters--pair key) entries))
            (when (and (< position size) (not (memq (aref text position) '(?\s ?\t ?\n ?\r))))
              (ngit-config-parameters--bad-format))
            (while (and (< position size) (memq (aref text position) '(?\s ?\t ?\n ?\r)))
              (setq position (1+ position)))))))
    (nreverse entries)))

(provide 'ngit-config-parameters)
;;; ngit-config-parameters.el ends here
