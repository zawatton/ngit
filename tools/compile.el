;;; compile.el --- byte-compile ngit with the host Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage: emacs --batch -Q -L lisp -l tools/compile.el OUTDIR FILE.el...
;; Called by tools/build-elc.sh.  NeLisp runs Emacs byte code, and loading
;; it avoids NeLisp's per-function cost on the first call of source code.
;; The settings below keep the output inside what NeLisp reads and runs
;; correctly.

;;; Code:

(require 'bytecomp)

;; Modules call zlib through nl-ffi at load time, which the host lacks.
(advice-add 'ffi:library :override #'ignore)

;; NeLisp's Bsubstring opcode rejects vectors; compile a plain call.
(put 'substring 'byte-opcode nil)
(put 'substring 'byte-compile nil)

;; NeLisp's reader handles neither #@ lazy docstrings nor the #N= / #N#
;; labels that print-circle emits for shared constants.
(setq byte-compile-dynamic-docstrings nil
      load-prefer-newer t)
(defun byte-compile-output-file-form (form)
  (let ((print-escape-newlines t) (print-length nil) (print-level nil)
        (print-quoted t) (print-gensym t) (print-circle nil))
    (princ "\n" byte-compile--outbuffer)
    (prin1 form byte-compile--outbuffer)
    nil))

(let* ((outdir (file-name-as-directory (pop command-line-args-left)))
       (byte-compile-dest-file-function
        (lambda (file) (concat outdir (file-name-nondirectory file) "c")))
       (failed 0))
  (dolist (file command-line-args-left)
    (unless (byte-compile-file file)
      (setq failed (1+ failed))
      (message "FAILED %s" file)))
  (setq command-line-args-left nil)
  (kill-emacs (if (> failed 0) 1 0)))

;;; compile.el ends here
