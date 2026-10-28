;;; ngit-windows.el --- Native Windows application OS adapter -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep Windows resource ownership here; no Git operation is delegated.
;; CRT descriptors always belong to ucrtbase and are opened in binary mode.

;;; Code:

(defun ngit-windows-p () (eq system-type 'windows-nt))

(defvar ngit-win--case-keys (make-hash-table :test 'equal))

(defun ngit-win--case-key (name)
  "Return Windows invariant uppercase NAME using its native Unicode table."
  (or (gethash name ngit-win--case-keys)
      (let ((source (ngit-win-wide name)) destination)
        (unwind-protect
            (let ((count (ngit-win-call "kernel32.dll" "LCMapStringW"
                                       #x7f #x200 source -1 0 0)))
              (unless (> count 0) (error "ngit: Windows invariant case mapping failed"))
              (setq destination (ngit-map-anon (* count 2)))
              (unless (= count (ngit-win-call "kernel32.dll" "LCMapStringW"
                                              #x7f #x200 source -1 destination count))
                (error "ngit: Windows invariant case mapping failed"))
              (when (>= (hash-table-count ngit-win--case-keys) 4096)
                (clrhash ngit-win--case-keys))
              (puthash name (ngit-win-read-wide destination (1- count)) ngit-win--case-keys))
          (when destination (ngit-win-free destination))
          (ngit-win-free source)))))

(defun ngit-win--protected-key (name)
  "Conservatively compare protected names beyond the observed case table."
  (let ((key (copy-sequence (ngit-win--case-key name))))
    ;; These protected-component restrictions are intentionally stronger
    ;; than Wine's observed mapping, without changing ordinary Unicode keys.
    (dotimes (index (length key))
      (cond ((= (aref key index) #x131) (aset key index ?I))
            ((= (aref key index) #x17f) (aset key index ?S))))
    (downcase key)))

(defun ngit-win-validate-worktree-path (relative)
  "Validate raw UTF-8 worktree RELATIVE before any Windows path expansion.
Return the decoded spelling.  Internal absolute repository paths do not
use this entry point.  Reserve git~N names conservatively as metadata aliases."
  (unless (and (stringp relative) (> (length relative) 0))
    (error "ngit: unsafe Windows worktree path %S" relative))
  (let* ((bytes (if (multibyte-string-p relative)
                    (encode-coding-string relative 'utf-8) relative))
         (length (length bytes)) (position 0))
    ;; Reject malformed, overlong and surrogate encodings before a decoder
    ;; could give different byte names the same Windows spelling.
    (while (< position length)
      (let* ((lead (aref bytes position))
             (count (cond ((< lead 128) 1) ((and (>= lead 194) (<= lead 223)) 2)
                          ((and (>= lead 224) (<= lead 239)) 3)
                          ((and (>= lead 240) (<= lead 244)) 4) (t 0)))
             (code (if (= count 1) lead (logand lead (1- (ash 1 (- 7 count)))))))
        (when (or (= count 0) (> (+ position count) length))
          (error "ngit: invalid UTF-8 Windows worktree path"))
        (dotimes (offset (1- count))
          (let ((byte (aref bytes (+ position offset 1))))
            (unless (and (>= byte 128) (<= byte 191))
              (error "ngit: invalid UTF-8 Windows worktree path"))
            (setq code (+ (ash code 6) (logand byte 63)))))
        (when (or (and (= count 2) (< code 128))
                  (and (= count 3) (< code 2048))
                  (and (= count 4) (< code 65536))
                  (> code #x10ffff) (and (>= code #xd800) (<= code #xdfff))
                  (< code 32) (and (>= code 127) (<= code 159))
                  (memq code '(34 42 58 60 62 63 92 124)))
          (error "ngit: unsafe Windows worktree path %S" relative))
        (setq position (+ position count))))
    (let ((decoded (decode-coding-string bytes 'utf-8)))
      (when (= (aref decoded 0) ?~)
        (error "ngit: unsafe Windows worktree path %S" relative))
      (dolist (part (split-string decoded "/" nil))
        (let* ((name (ngit-win--protected-key part))
               (dot (string-match "\\." name))
               (stem (substring name 0 dot)))
          (while (and (> (length stem) 0) (= (aref stem (1- (length stem))) ?\s))
            (setq stem (substring stem 0 -1)))
          (when (or (member name '("" "." ".." ".git"))
                    (memq (aref name (1- (length name))) '(32 46))
                    (string-match-p "\\`git~[0-9]+\\(?:\\..*\\)?\\'" name)
                    (member stem '("con" "prn" "aux" "nul" "conin$" "conout$"))
                    (and (= (length stem) 4)
                         (member (substring stem 0 3) '("com" "lpt"))
                         (memq (aref stem 3) '(49 50 51 52 53 54 55 56 57 185 178 179))))
            (error "ngit: unsafe Windows worktree path %S" relative))))
      decoded)))

(defun ngit-win-validate-worktree-records (records)
  "Preflight every path in entry/change RECORDS before publication."
  (when (ngit-windows-p)
    (let ((spellings (make-hash-table :test 'equal))
          (directories (make-hash-table :test 'equal)))
      (mapc (lambda (record)
              (let* ((path (ngit-win-validate-worktree-path (aref record 0)))
                     (folded (ngit-win--case-key path)) (previous (gethash folded spellings)))
                (when (or (gethash folded directories)
                          (and previous (not (equal previous path))))
                  (error "ngit: colliding Windows worktree paths %S and %S" previous path))
                (let ((start 0) slash)
                  (while (setq slash (string-match "/" folded start))
                    (let ((prefix (substring folded 0 slash)))
                      (when (gethash prefix spellings)
                        (error "ngit: colliding Windows worktree paths %S and %S"
                               (gethash prefix spellings) path))
                      (puthash prefix path directories))
                    (setq start (1+ slash))))
                (puthash folded path spellings)))
            records))))
(defvar ngit-win--allocations (make-hash-table :test 'eql))
(defvar ngit-win--modules (make-hash-table :test 'equal))
(defvar ngit-win--symbols (make-hash-table :test 'equal))
(defvar ngit-win--owners (make-hash-table :test 'eq))

(defun ngit-map-anon (size)
  "Allocate SIZE bytes, returning an owned page-aligned base address."
  (unless (and (integerp size) (>= size 0) (<= size #x3fffffff))
    (error "ngit: invalid allocation size %S" size))
  (let* ((rounded (* 4096 (/ (+ (max size 1) 4095) 4096)))
         (address (nl-ffi-call "VirtualAlloc" 0 rounded #x3000 4)))
    (unless (and (integerp address) (> address 4095))
      (error "ngit: VirtualAlloc failed"))
    (puthash address rounded ngit-win--allocations)
    address))

(defun ngit-win-free (address &optional size)
  "Release an owned ADDRESS exactly once; validate optional rounded SIZE."
  (let ((allocated (gethash address ngit-win--allocations)))
    (unless (and allocated (or (null size) (= size allocated)))
      (error "ngit: invalid or released allocation"))
    (unless (/= 0 (nl-ffi-call "VirtualFree" address 0 #x8000))
      (error "ngit: VirtualFree failed"))
    (remhash address ngit-win--allocations)
    0))
(defalias 'ngit-unmap #'ngit-win-free)

(defun ngit-win-wide (string)
  "Return an owned NUL-terminated UTF-16LE pointer for STRING."
  (when (string-match-p "\0" string) (error "ngit: NUL in Windows path"))
  (let ((address (ngit-map-anon (+ 2 (* 4 (length string))))) (offset 0))
    (dotimes (index (length string))
      (let ((character (aref string index)))
        (when (> character #xffff)
          (let ((high (+ #xd800 (ash (- character #x10000) -10))))
            (ptr-write-u8 address offset (logand high 255))
            (ptr-write-u8 address (1+ offset) (ash high -8))
            (setq offset (+ offset 2)
                  character (+ #xdc00 (logand (- character #x10000) #x3ff)))))
        (ptr-write-u8 address offset (logand character 255))
        (ptr-write-u8 address (1+ offset) (ash character -8))
        (setq offset (+ offset 2))))
    (ptr-write-u8 address offset 0)
    (ptr-write-u8 address (1+ offset) 0)
    address))

(defun ngit-win--ascii (string)
  (let ((address (ngit-map-anon (1+ (length string)))))
    (ptr-write-bytes address string)
    (ptr-write-u8 address (length string) 0)
    address))

(defun ngit-win-symbol (dll name)
  "Resolve NAME in DLL, caching both module and symbol ownership."
  (let ((key (concat dll ":" name)))
    (or (gethash key ngit-win--symbols)
        (let ((module (gethash dll ngit-win--modules)))
          (unless module
            (let ((wide (ngit-win-wide dll)))
              (unwind-protect
                  (setq module (nl-ffi-call "GetModuleHandleW" wide))
                (ngit-win-free wide)))
            (when (= module 0)
              (let ((loader (ngit-win-symbol "kernel32.dll" "LoadLibraryW"))
                    (wide (ngit-win-wide dll)))
                (unwind-protect
                    (setq module (ptr-call loader wide 0 0 0 0 0))
                  (ngit-win-free wide))))
            (when (= module 0) (error "ngit: cannot load DLL %s" dll))
            (puthash dll module ngit-win--modules))
          (let ((ascii (ngit-win--ascii name)) address)
            (unwind-protect
                (setq address (nl-ffi-call "GetProcAddress" module ascii))
              (ngit-win-free ascii))
            (when (= address 0) (error "ngit: missing %s in %s" name dll))
            (puthash key address ngit-win--symbols))))))

(defun ngit-win-call (dll name &rest args)
  "Call DLL's NAME with at most six integer or pointer ARGS."
  (when (> (length args) 6) (error "ngit: Windows call exceeds six arguments"))
  (apply #'ptr-call (ngit-win-symbol dll name)
         (append args (make-list (- 6 (length args)) 0))))

(defun ngit-win-signed32 (value)
  (let ((low (logand value #xffffffff)))
    (if (>= low #x80000000) (- low #x100000000) low)))

(defun ngit-win-memory-allocate (size)
  (let* ((address (ngit-map-anon size))
         (owner (vector 'ngit-win-memory-owner address
                        (gethash address ngit-win--allocations) size)))
    (puthash owner (copy-sequence owner) ngit-win--owners)
    owner))

(defun ngit-win-memory-address (owner)
  (let* ((record (gethash owner ngit-win--owners))
         (address (and record (aref record 1))))
    (unless (and record (equal owner record)
                 (equal (aref record 2) (gethash address ngit-win--allocations)))
      (error "ngit: invalid or released memory owner"))
    address))

(defun ngit-win-memory-release (owner)
  (ngit-win-free (ngit-win-memory-address owner) (aref owner 2))
  (remhash owner ngit-win--owners)
  (aset owner 1 0) (aset owner 2 0) t)

(defun ngit-win-memory-cstring (bytes)
  (let* ((owner (ngit-win-memory-allocate (1+ (length bytes))))
         (address (ngit-win-memory-address owner)))
    (ptr-write-bytes address bytes)
    (ptr-write-u8 address (length bytes) 0)
    owner))

(when (ngit-windows-p)
  ;; The Linux-only package cannot be loaded on the PE runtime.  These
  ;; compatibility names retain the exact owner contract at the app boundary.
  (defalias 'nl-ffi-memory-allocate #'ngit-win-memory-allocate)
  (defalias 'nl-ffi-memory-address #'ngit-win-memory-address)
  (defalias 'nl-ffi-memory-release #'ngit-win-memory-release)
  (defalias 'nl-ffi-memory-cstring #'ngit-win-memory-cstring))

(defun ngit-win-open (path flags mode)
  "Open PATH with UCRT FLAGS and MODE; always use binary byte semantics."
  (let ((wide (ngit-win-wide path)))
    (unwind-protect
        (ngit-win-signed32 (ngit-win-call "ucrtbase.dll" "_wopen"
                                         wide (logior flags #x8080) mode))
      (ngit-win-free wide))))

(defun ngit-win-pread (fd address count offset)
  "Read at OFFSET on an exclusively owned FD; calls are serialized."
  (let ((position (ngit-win-call "ucrtbase.dll" "_lseeki64" fd offset 0)))
    (if (< position 0) -1
      (ngit-win-signed32 (ngit-win-call "ucrtbase.dll" "_read" fd address count)))))

(defun ngit-win-errno ()
  (ptr-read-u32 (ngit-win-call "ucrtbase.dll" "_errno") 0))

(defun ngit-win-read-wide (address count)
  "Decode COUNT UTF-16 code units from ADDRESS without a codec dependency."
  (let ((index 0) characters)
    (while (< index count)
      (let ((code (logior (ptr-read-u8 address (* index 2))
                         (ash (ptr-read-u8 address (1+ (* index 2))) 8))))
        (setq index (1+ index))
        (when (and (>= code #xd800) (<= code #xdbff) (< index count))
          (let ((low (logior (ptr-read-u8 address (* index 2))
                             (ash (ptr-read-u8 address (1+ (* index 2))) 8))))
            (unless (and (>= low #xdc00) (<= low #xdfff))
              (error "ngit: invalid UTF-16 surrogate"))
            (setq code (+ #x10000 (ash (- code #xd800) 10) (- low #xdc00))
                  index (1+ index))))
        (push code characters)))
    (apply #'string (nreverse characters))))

(defun ngit-win-current-directory ()
  (let ((owner (ngit-win-memory-allocate 65536)))
    (unwind-protect
        (let* ((address (ngit-win-memory-address owner))
               (count (ngit-win-call "kernel32.dll" "GetCurrentDirectoryW" 32768 address)))
          (unless (and (> count 0) (< count 32768))
            (error "ngit: cannot read Windows current directory"))
          (let ((path (ngit-win-read-wide address count)))
            (dotimes (i (length path))
              (when (= (aref path i) ?\\) (aset path i ?/)))
            path))
      (ngit-win-memory-release owner))))

(defun ngit-win-write-file (path data excl append)
  "Write unibyte DATA to PATH with atomic exclusive creation when EXCL."
  (let ((fd (ngit-win-open path (logior 1 #x100 (if excl #x400 0)
                                        (if append #x8 #x200)) #o666)))
    (when (< fd 0)
      (if (= (ngit-win-errno) 17)
          (signal 'file-already-exists (list "File exists" path))
        (error "ngit: cannot write %s (errno %d)" path (ngit-win-errno))))
    (unwind-protect
        (let ((owner (ngit-win-memory-allocate (length data))) (done 0))
          (unwind-protect
              (let ((address (ngit-win-memory-address owner)))
                (ptr-write-bytes address data)
                (while (< done (length data))
                  (let ((n (ngit-win-signed32
                            (ngit-win-call "ucrtbase.dll" "_write" fd
                                           (+ address done) (- (length data) done)))))
                    (when (<= n 0) (error "ngit: Windows write failed"))
                    (setq done (+ done n)))))
            (ngit-win-memory-release owner)))
      (ngit-win-call "ucrtbase.dll" "_close" fd))))

(defun ngit-win-stat (path normalized)
  "Return normalized metadata for PATH, populating NORMALIZED stat bytes.
The compatibility buffer uses the existing Linux field offsets, never a
Windows struct cast: dev0, ino8, mode24, uid28, gid32, size48,
mtime88/nsec96, ctime104/nsec112.  Reparse points require explicit support."
  (let ((wide (ngit-win-wide path)) handle)
    (unwind-protect
        (setq handle (nl-ffi-call "CreateFileW" wide #x80 7 0 3 #x2200000 0))
      (ngit-win-free wide))
    (if (= handle -1)
        (let ((code (nl-ffi-call "GetLastError")))
          (unless (memq code '(2 3))
            (error "ngit: cannot stat %s (Windows error %d)" path code)))
      (let ((info (ngit-win-memory-allocate 64)))
        (unwind-protect
            (let ((address (ngit-win-memory-address info)))
              (when (= 0 (nl-ffi-call "GetFileInformationByHandle" handle address))
                (error "ngit: cannot read Windows file information"))
              (let* ((attrs (ptr-read-u32 address 0))
                     (mode (if (/= 0 (logand attrs #x10)) #o040755 #o100644))
                     (ino (logior (ash (ptr-read-u32 address 44) 32)
                                  (ptr-read-u32 address 48)))
                     (size (logior (ash (ptr-read-u32 address 32) 32)
                                   (ptr-read-u32 address 36)))
                     (mtime (- (ptr-read-u64 address 20) 116444736000000000))
                     (ctime (- (ptr-read-u64 address 4) 116444736000000000)))
                (when (/= 0 (logand attrs #x400))
                  (error "ngit: Windows reparse points are unsupported: %s" path))
                (dotimes (i 144) (ptr-write-u8 normalized i 0))
                (ptr-write-u64 normalized 0 (ptr-read-u32 address 28))
                (ptr-write-u64 normalized 8 ino)
                (ptr-write-u32 normalized 24 mode)
                (ptr-write-u64 normalized 48 size)
                (ptr-write-u64 normalized 88 (/ mtime 10000000))
                (ptr-write-u64 normalized 96 (* (% mtime 10000000) 100))
                (ptr-write-u64 normalized 104 (/ ctime 10000000))
                (ptr-write-u64 normalized 112 (* (% ctime 10000000) 100))
                (vector mode ino size (/ mtime 10000000)
                        (* (% mtime 10000000) 100) (/ ctime 10000000))))
          (ngit-win-memory-release info)
          (nl-ffi-call "CloseHandle" handle))))))

(defun ngit-win-read-nofollow (path)
  "Read PATH through one handle, rejecting reparse points on that handle."
  (let ((wide (ngit-win-wide path)) handle)
    (unwind-protect
        (setq handle (nl-ffi-call "CreateFileW" wide #x80000000 7 0 3 #x200000 0))
      (ngit-win-free wide))
    (if (= handle -1)
        (let ((code (nl-ffi-call "GetLastError")))
          (unless (memq code '(2 3))
            (error "ngit: cannot read %s (Windows error %d)" path code)))
      (let ((owner (ngit-win-memory-allocate 65600)))
        (unwind-protect
            (let* ((address (ngit-win-memory-address owner))
                   (count-address (+ address 65536)) parts done)
              (when (= 0 (nl-ffi-call "GetFileInformationByHandle" handle address))
                (error "ngit: cannot inspect opened file %s" path))
              (when (/= 0 (logand (ptr-read-u32 address 0) #x400))
                (error "ngit: Windows reparse points are unsupported: %s" path))
              (while (not done)
                (when (= 0 (nl-ffi-call "ReadFile" handle address 65536 count-address 0))
                  (error "ngit: cannot read opened file %s" path))
                (let ((count (ptr-read-u32 count-address 0)))
                  (if (= count 0) (setq done t)
                    (push (ptr-read-bytes address count) parts))))
              (apply #'concat (nreverse parts)))
          (ngit-win-memory-release owner)
          (nl-ffi-call "CloseHandle" handle))))))

(defun ngit-rename-file (old new &optional replace)
  "Rename OLD to NEW, optionally replacing the destination atomically."
  (if (not (ngit-windows-p)) (rename-file old new replace)
    (ngit-win-close-cached-file old)
    (ngit-win-close-cached-file new)
    (let ((source (ngit-win-wide (expand-file-name old)))
          (target (ngit-win-wide (expand-file-name new))))
      (unwind-protect
          (when (= 0 (nl-ffi-call "MoveFileExW" source target (if replace 9 8)))
            (let ((code (nl-ffi-call "GetLastError")))
              (if (memq code '(80 183))
                  (signal 'file-already-exists (list "File exists" new))
                (signal 'file-error (list "Windows rename failed" code old new)))))
        (ngit-win-free source) (ngit-win-free target)))))

(defun ngit-delete-file (path &optional trash)
  "Delete PATH through the platform's native file API."
  (if (not (ngit-windows-p)) (delete-file path trash)
    (when trash (error "ngit: Windows trash deletion is unsupported"))
    (ngit-win-close-cached-file path)
    (let ((wide (ngit-win-wide (expand-file-name path))))
      (unwind-protect
          (when (= 0 (ngit-win-call "kernel32.dll" "DeleteFileW" wide))
            (signal 'file-error (list "Windows delete failed" (nl-ffi-call "GetLastError") path)))
        (ngit-win-free wide)))))

(defun ngit-create-directory (path &optional parents)
  "Create PATH, recursively creating its parent directories with PARENTS."
  (if (not (ngit-windows-p)) (make-directory path parents)
    (let* ((absolute (directory-file-name (expand-file-name path)))
           (parent (file-name-directory absolute)))
      (when (and parents parent (not (file-directory-p parent)))
        (when (equal absolute (directory-file-name parent))
          (error "ngit: cannot create Windows volume root %s" absolute))
        (ngit-create-directory parent t))
      (let ((wide (ngit-win-wide absolute)))
        (unwind-protect
            (when (= 0 (nl-ffi-call "CreateDirectoryW" wide 0))
              (let ((code (nl-ffi-call "GetLastError")))
                (unless (and parents (= code 183) (file-directory-p absolute))
                  (signal 'file-error (list "Windows directory creation failed" code path)))))
          (ngit-win-free wide))))))

(defun ngit-delete-directory (path &optional recursive trash)
  "Delete directory PATH; recursively remove entries with RECURSIVE."
  (if (not (ngit-windows-p)) (delete-directory path recursive trash)
    (when trash (error "ngit: Windows trash deletion is unsupported"))
    (when recursive
      (dolist (entry (directory-files path t nil t))
        (unless (member (file-name-nondirectory entry) '("." ".."))
          (let ((owner (ngit-win-memory-allocate 256)))
            (unwind-protect
                (let ((st (ngit-win-stat entry (ngit-win-memory-address owner))))
                  (when st
                    (if (= (logand (aref st 0) #o170000) #o040000)
                        (ngit-delete-directory entry t)
                      (ngit-delete-file entry))))
              (ngit-win-memory-release owner))))))
    (let ((wide (ngit-win-wide (expand-file-name path))))
      (unwind-protect
          (when (= 0 (ngit-win-call "kernel32.dll" "RemoveDirectoryW" wide))
            (signal 'file-error (list "Windows directory deletion failed" (nl-ffi-call "GetLastError") path)))
        (ngit-win-free wide)))))

(defun ngit-win-close-cached-file (path)
  "Release ngit's read cache before a Windows mutation of PATH."
  (when (and (boundp 'ngit-native--fds) (fboundp 'ngit-native-close))
    (ngit-native-close path)
    (let ((absolute (expand-file-name path)))
      (unless (equal path absolute) (ngit-native-close absolute)))))

(defvar ngit-win--stdio-initialized nil)

(defun ngit-win-initialize-stdio ()
  "Put available CRT standard descriptors in binary mode exactly once.
An absent OS standard handle has no descriptor to configure.  A failure
for an available handle is an error; protocol bytes must never use text mode."
  (unless ngit-win--stdio-initialized
    (dotimes (fd 3)
      (let ((handle (ngit-win-call "kernel32.dll" "GetStdHandle" (- -10 fd))))
        (unless (or (= handle 0) (= handle -1))
          (when (< (ngit-win-signed32
                    (ngit-win-call "ucrtbase.dll" "_setmode" fd #x8000)) 0)
            (error "ngit: cannot set binary standard descriptor %d (errno %d)"
                   fd (ngit-win-errno))))))
    (setq ngit-win--stdio-initialized t)))

(when (ngit-windows-p) (ngit-win-initialize-stdio))

(provide 'ngit-windows)
;;; ngit-windows.el ends here
