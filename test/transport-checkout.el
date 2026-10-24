;;; transport-checkout.el --- forced parallel clone checkout probe -*- lexical-binding: t; -*-

(add-to-list 'load-path
             (expand-file-name "../lisp/" (file-name-directory load-file-name)))
(require 'ngit-transport-checkout)

(condition-case err
    (let* ((repo (ngit-discover (getenv "NGIT_TRANSPORT_CHECKOUT_REPO")))
           (tree (ngit-unhex (ngit-peel repo (ngit-resolve-ref repo "HEAD") 'tree)))
           (ngit-transport-checkout-threshold 1)
           (ngit-transport-checkout-workers 4))
      (ngit-transport-checkout repo tree)
      (princ "transport-checkout: PASS\n")
      (kill-emacs 0))
  (error (ngit-native-write-stderr (concat (error-message-string err) "\n"))
         (kill-emacs 1)))
