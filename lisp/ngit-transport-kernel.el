;;; ngit-transport-kernel.el --- compiled delta application -*- lexical-binding: t; -*-

;;; Commentary:

;; Compile the small, Lisp-authored delta loop to NeLisp's strict raw SysV
;; ABI once in the parent process.  Forked index workers inherit the executable
;; mapping and can apply deltas without interpreting every copy instruction.

;;; Code:

(require 'ngit-native)

(defvar ngit-transport-kernel--directory
  (file-name-directory (or load-file-name buffer-file-name default-directory)))
(defvar ngit-transport-kernel--attempted nil)
(defvar ngit-transport-kernel--ready nil)
(defvar ngit-transport-kernel--error nil)
(defvar ngit-transport-kernel--unit-id nil)
(defvar ngit-transport-kernel--address nil)

(defun ngit-transport-kernel--executable ()
  "Return the absolute executable image backing this process."
  (or (file-symlink-p "/proc/self/exe")
      (file-truename "/proc/self/exe")))

(defun ngit-transport-kernel--load-compiler (binary)
  "Load the NeLisp raw compiler and publication service from their checkout."
  (let* ((root (file-name-as-directory (file-truename ngit-nelisp-root)))
         (lisp (expand-file-name "lisp/" root)))
    ;; These runtime variables are lexical in this image, so a LET does not
    ;; affect the startup service.  Canonicalize them before loading it.
    (setq invocation-directory (file-name-directory binary)
          invocation-name (file-name-nondirectory binary))
    (setcar command-line-args binary)
    ;; Let the authenticated startup service load its sidecar exactly once.
    (when (fboundp 'nelisp-native-startup-ensure)
      (nelisp-native-startup-ensure))
    (add-to-list 'load-path lisp)
    (load (expand-file-name "nelisp-native-load.el" lisp) nil t)
    (load (expand-file-name "nelisp-native-unit.el" lisp) nil t)
    binary))

(defun ngit-transport-kernel-prepare ()
  "Compile and publish the raw delta kernel once; return non-nil when ready.
Failure is remembered so callers can use their ordinary Lisp fallback."
  (unless ngit-transport-kernel--attempted
    (setq ngit-transport-kernel--attempted t)
    (condition-case err
        (let* ((binary (ngit-transport-kernel--executable))
               (source (expand-file-name "ngit-transport-kernel-source.el"
                                         ngit-transport-kernel--directory))
               (prefix (make-temp-file "ngit-delta-kernel-"))
               (artifact (concat prefix ".nelr"))
               staged published unit address)
          (unwind-protect
              (let ((invocation-directory (file-name-directory binary))
                    (invocation-name (file-name-nondirectory binary))
                    (command-line-args (cons binary (cdr command-line-args))))
                (progn
                  (ngit-transport-kernel--load-compiler binary)
                  (when (file-exists-p prefix) (ngit-delete-file prefix))
                  (nelisp-native-load-raw-compile-file
                   source artifact nil "ngit-transport-kernel"
                   (nelisp-native-load--running-binary-sha256))
                  (setq staged
                        (nelisp-native-unit-stage artifact nil '("ngitDeltaKernel")))
                  (unless (eq (plist-get staged :status) 'staged)
                    (error "ngit: delta kernel stage failed: %s"
                           (or (plist-get staged :reason) staged)))
                  (setq published
                        (nelisp-native-unit-publish (plist-get staged :candidate-id)))
                  (unless (eq (plist-get published :status) 'published)
                    (error "ngit: delta kernel publish failed: %s"
                           (or (plist-get published :reason) published)))
                  (setq unit (plist-get published :unit-id)
                        address (nelisp-native-unit-address unit "ngitDeltaKernel"))
                  (unless (and (integerp address) (> address 0))
                    (error "ngit: delta kernel has no callable address"))
                  (setq ngit-transport-kernel--unit-id unit
                        ngit-transport-kernel--address address
                        ngit-transport-kernel--ready t)))
            (when (file-exists-p prefix) (ngit-delete-file prefix))
            (when (file-exists-p artifact) (ngit-delete-file artifact))))
      (error (setq ngit-transport-kernel--error (error-message-string err)
                   ngit-transport-kernel--ready nil))))
  ngit-transport-kernel--ready)

(defun ngit-transport-kernel-apply (base-address base-length delta output-length)
  "Apply DELTA to native BASE-ADDRESS and return [OWNER ADDRESS LENGTH].
Return nil when the optional compiled kernel is unavailable.  A nonzero kernel
status is malformed input and is reported after discarding partial output."
  (unless (and (integerp base-address) (> base-address 0)
               (integerp base-length) (>= base-length 0)
               (stringp delta)
               (integerp output-length) (>= output-length 0))
    (error "ngit: invalid native delta kernel arguments"))
  (when (ngit-transport-kernel-prepare)
    (let* ((delta-length (length delta))
           (delta-owner (nl-ffi-memory-allocate (max 1 delta-length)))
           (delta-address (nl-ffi-memory-address delta-owner))
           (output-owner nil)
           result)
      (unwind-protect
          (progn
            (ptr-write-bytes delta-address delta)
            (setq output-owner (nl-ffi-memory-allocate (max 1 output-length)))
            (let* ((output-address (nl-ffi-memory-address output-owner))
                   (status (ptr-call ngit-transport-kernel--address
                                     base-address base-length
                                     delta-address delta-length
                                     output-address output-length)))
              (unless (= status 0)
                (error "ngit: malformed delta (native kernel status %d)" status))
              (setq result (vector output-owner output-address output-length)
                    output-owner nil)
              result))
        (when output-owner (nl-ffi-memory-release output-owner))
        (nl-ffi-memory-release delta-owner)))))

(provide 'ngit-transport-kernel)
;;; ngit-transport-kernel.el ends here
