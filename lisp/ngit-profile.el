;;; ngit-profile.el --- optional command phase timings -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded only with NGIT_PROFILE=1, after the command modules.  Wrappers
;; report inclusive times; nested phases account for their parent's time.

;;; Code:

(dolist (symbol (append '(ngit-read-index ngit-ct-root ngit-diff-tree-index
                  ngit-diff-trees ngit--reset-refresh ngit-ct-update
                  ngit-index-bytes ngit--checkout-source-entries
                  ngit-index-overlay
                  ngit--checkout-overlay ngit--switch-overlay ngit-untracked
                  ngit--switch-show-local ngit--reset-index ngit--reset-hard
                  ngit--switch-twoway ngit--checkout-path-command
                  ngit-maint--pack-refs ngit-maint--reflog-expire
                  ngit-pack-reachable ngit-maint--protected-reachable
                  ngit-pack--wanted ngit-pack--idx-info ngit-pack--validate-source
                  ngit-pack--idx-bytes ngit-pack--create-delta
                  ngit-pack-write ngit-maint--repack ngit-maint--prune
                  ngit-transport-index-pack ngit-transport-checkout
                  ngit-transport-checkout--parallel ngit-tr--receive ngit-main)
                (mapcar #'intern (split-string (or (getenv "NGIT_PROFILE_FNS") "")
                                               "[ ,]+" t))))
  (when (fboundp symbol)
    (let ((original (symbol-function symbol)) (phase (symbol-name symbol))
          (cached-index (eq symbol 'ngit-read-index)))
      (fset symbol
            (lambda (&rest args)
              (if (and cached-index (aref (car args) 6))
                  (apply original args)
                (let ((start (float-time)))
                  (unwind-protect
                      (apply original args)
                    (ngit-native-write-stderr
                     (format "profile/%s: %.6f\n" phase (- (float-time) start)))))))))))

(provide 'ngit-profile)

;;; ngit-profile.el ends here
