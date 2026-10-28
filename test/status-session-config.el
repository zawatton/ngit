;;; status-session-config.el --- public session policy probes -*- lexical-binding: t; -*-

(let ((directory (expand-file-name "lisp/" (getenv "NGIT_CONFIG_SOURCE_ROOT"))))
  (setq load-path (cons directory load-path))
  (dolist (module '("ngit-native" "ngit-store" "ngit-rev" "ngit-pathspec"
                    "ngit-paths" "ngit-cli" "ngit-config" "ngit-match"
                    "ngit-index" "ngit-worktree" "ngit-changes" "ngit-status"
                    "ngit-status-watch" "ngit-status-session"))
    (load (expand-file-name (concat module ".el") directory) nil t)))

(defvar ngit-test-config-loads 0)
(let ((reader (symbol-function 'ngit--config-load)))
  (fset 'ngit--config-load
        (lambda (&rest args)
          (setq ngit-test-config-loads (1+ ngit-test-config-loads))
          (apply reader args))))

;; Every public operation must restore these unrelated caller bindings.
(defvar ngit-test-caller-cache (list 'private-caller-cache))
(setq ngit--config-cache ngit-test-caller-cache)
(defvar ngit-test-caller-text (make-hash-table :test 'equal))
(setq ngit--config-text-cache ngit-test-caller-text)
(puthash "private-marker" "unchanged" ngit-test-caller-text)
(defvar ngit-test-caller-entries (list 'private-caller-entries))
(setq ngit--config-startup-entries ngit-test-caller-entries)
(defvar ngit-test-caller-validation (list 'private-validation))
(setq ngit--config-validated-cache ngit-test-caller-validation)

(defun ngit-test-context ()
  (and (eq ngit--config-cache ngit-test-caller-cache)
       (eq ngit--config-text-cache ngit-test-caller-text)
       (= (hash-table-count ngit-test-caller-text) 1)
       (equal (gethash "private-marker" ngit-test-caller-text) "unchanged")
       (eq ngit--config-startup-entries ngit-test-caller-entries)
       (eq ngit--config-validated-cache ngit-test-caller-validation)))

(let ((stats (symbol-function 'ngit-status-session-stats)))
  (fset 'ngit-status-session-stats
        (lambda (session)
          (append (funcall stats session)
                  (list :config_loads ngit-test-config-loads
                        :caller_context_ok (if (ngit-test-context) 1 0))))))

(defvar ngit-test-assertions 0)
(defun ngit-test-assert (value message)
  (setq ngit-test-assertions (1+ ngit-test-assertions))
  (unless value (error "acceptance: %s" message)))

(defun ngit-test-rejected (function)
  (condition-case nil (progn (funcall function) nil) (error t)))

(defun ngit-test-lifecycle ()
  (let* ((directory default-directory)
         (original-config (ngit--read-file ".git/config"))
         (session (ngit-status-session-open directory))
         old first second)
    (unwind-protect
        (progn
          (setq first (ngit-status-session-status session))
          (ngit-test-assert (ngit-test-context) "open/status changed caller globals")
          (ngit-test-assert
           (ngit-test-rejected (lambda () (ngit-status-session-open directory)))
           "second simultaneous open accepted")
          (setq old session)
          (ngit-status-session-close session)
          (ngit-status-session-close session)
          (ngit-test-assert (null ngit-status-session--active) "close retained active owner")
          (ngit-test-assert (ngit-test-context) "close changed caller globals")
          (ngit-test-assert
           (ngit-test-rejected (lambda () (ngit-status-session-status old)))
           "closed handle accepted")
          ;; A new invocation must capture its own command policy, then retain
          ;; that policy even when the caller changes its invocation globals.
          (let ((ngit--config-cli-parameters '(("core.quotepath" . "false"))))
            (setq session (ngit-status-session-open directory)))
          (setq second (ngit-status-session-status session))
          (ngit-test-assert (not (equal first second)) "reopen reused prior command policy")
          (ngit-test-assert (string-search "日本語" (decode-coding-string second 'utf-8))
                            "fixed command policy missing")
          (ngit-test-assert (equal second (ngit-status-session-refresh session))
                            "refresh lost fixed command policy")
          (ngit-test-assert (ngit-test-context) "refresh changed caller globals")
          (ngit-status-session-close session)
          (setq session nil)
          (write-region "[broken\n" nil ".git/config" nil 'silent)
          (ngit-test-assert
           (ngit-test-rejected (lambda () (ngit-status-session-open directory)))
           "failed initialization unexpectedly succeeded")
          (ngit-test-assert (null ngit-status-session--active) "failed initialization retained owner")
          (ngit-test-assert (ngit-test-context) "failed initialization changed caller globals")
          (write-region original-config nil ".git/config" nil 'silent)
          (setq session (ngit-status-session-open directory))
          (ngit-test-assert (equal first (ngit-status-session-status session))
                            "failed initialization retained command policy")
          (ngit-status-session-close session)
          (setq session (ngit-status-session-open (getenv "NGIT_CONFIG_SECOND_REPO")))
          (ngit-test-assert (equal second (ngit-status-session-status session))
                            "second repository inherited previous repository policy")
          (ngit-test-assert (ngit-test-context) "second repository changed caller globals")
          (ngit-status-session-close session)
          (setq session (ngit-status-session-open directory))
          (ngit-test-assert (equal first (ngit-status-session-status session))
                            "return to first repository retained second policy")
          (princ (format "ownership lifecycle assertions=%d PASS\n" ngit-test-assertions)))
      (when session (ngit-status-session-close session))
      (write-region original-config nil ".git/config" nil 'silent))))

(defun ngit-test-nonlocal (mode)
  "Interrupt a real calculation once and catch outside the public API."
  (let* ((opening (string-search "open" mode))
         (quitting (string-search "quit" mode))
         (original (symbol-function 'ngit-wt-changes))
         session observed stopped initial next discarded)
    (unwind-protect
        (progn
          (unless opening
            (setq session (ngit-status-session-open default-directory)
                  initial (ngit-status-session-status session))
            (ngit-test-assert (equal initial "") "interruption fixture was not clean")
            (write-region "interrupted dirty content\n" nil "f00000" nil 'silent))
          (fset 'ngit-wt-changes
                (lambda (&rest _args)
                  (setq observed ngit-status-session--active)
                  (fset 'ngit-wt-changes original)
                  (if quitting (signal 'quit nil)
                    (throw 'ngit-test-stop 'interrupted))))
          (setq stopped
                (if quitting
                    (condition-case nil
                        (if opening (ngit-status-session-open default-directory)
                          (ngit-status-session-status session))
                      (quit 'interrupted))
                  (catch 'ngit-test-stop
                    (if opening (ngit-status-session-open default-directory)
                      (ngit-status-session-status session)))))
          (ngit-test-assert (eq stopped 'interrupted) "one-shot nonlocal exit was not reached")
          (ngit-test-assert (ngit-test-context) "nonlocal exit changed caller globals")
          (if opening
              (progn
                (ngit-test-assert (null ngit-status-session--active) "failed open retained active owner")
                (ngit-test-assert (and observed (null (aref observed 5))
                                       (null (aref observed 7))
                                       (or (<= (length observed) 20) (null (aref observed 20))))
                                  "failed open retained watch/output/configuration")
                (setq session (ngit-status-session-open default-directory)))
            (setq discarded (and (null (aref session 8))
                                 (or (<= (length session) 20) (null (aref session 20))))))
          (setq next (ngit-status-session-status session))
          (ngit-test-assert (equal next (getenv "NGIT_CONFIG_EXPECTED"))
                            "nonlocal recovery returned stale output instead of Git oracle")
          (unless opening
            (ngit-test-assert discarded "interrupted status retained coverage/configuration"))
          (ngit-test-assert (equal next (ngit-status-session-refresh session))
                            "nonlocal recovery differs from fresh public refresh")
          (ngit-test-assert (ngit-test-context) "nonlocal recovery changed caller globals")
          (princ (format "nonlocal %s assertions=%d PASS\n" mode ngit-test-assertions)))
      (fset 'ngit-wt-changes original)
      (when ngit-status-session--active
        (ngit-status-session-close ngit-status-session--active)))))

(condition-case err
    (let ((mode (getenv "NGIT_CONFIG_PROBE_MODE")))
      (cond
       ((equal mode "ownership") (ngit-test-lifecycle) (kill-emacs 0))
       ((string-prefix-p "nonlocal-" mode) (ngit-test-nonlocal mode) (kill-emacs 0))
       (t (kill-emacs (ngit-cmd-status-server nil nil)))))
  (error (ngit-native-write-stderr (concat (error-message-string err) "\n"))
         (kill-emacs 1)))
