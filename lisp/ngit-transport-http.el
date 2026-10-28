;;; ngit-transport-http.el --- native smart HTTP -*- lexical-binding: t; -*-

;;; Commentary:
;; libcurl owns HTTP framing and verified TLS.  Its default fread/fwrite
;; callbacks stream binary data through libc temporary FILEs.  No pack is
;; accumulated in Lisp.  A v0 connection has one discovery and at most one
;; RPC: the first read after writes commits the complete request, including
;; raw PACK bytes after the command flush.  A v2 connection sends each
;; command as a separate RPC.  Cleanup never sends a request.

;;; Code:
(require 'ngit-native)
(require 'ngit-config)
(unless (ngit-windows-p) (ffi:library "libcurl.so.4"))
(defvar ngit-http--functions (make-hash-table :test 'equal))

(defun ngit-http--call (name &rest args)
  "Call an external-library NAME with integer or pointer ARGS."
  (let ((fn (or (gethash name ngit-http--functions)
                (let ((p (if (ngit-windows-p)
                             (ngit-http--windows-symbol name)
                           (nl-ffi--resolve-via-dlsym name))))
                  (when (zerop p) (ngit-transport--fail "fatal: missing HTTP library function %s" name))
                  (puthash name p ngit-http--functions)))))
    (apply #'ptr-call fn (append args (make-list (- 6 (length args)) 0)))))

(defun ngit-http--windows-symbol (name)
  "Resolve external HTTP functions with explicit CRT ownership."
  (let* ((curl (string-prefix-p "curl_" name))
         (zlib (string-prefix-p "gz" name))
         (dll (cond (curl "libcurl-4.dll") (zlib "zlib1.dll") (t "ucrtbase.dll")))
         (directory (getenv "NGIT_WINDOWS_DLL_DIR"))
         (symbol (or (cdr (assoc name '(("fileno" . "_fileno") ("dup" . "_dup")
                                      ("close" . "_close") ("lseek" . "_lseeki64")
                                      ("fseek" . "_fseeki64") ("ftell" . "_ftelli64"))))
                     name)))
    (ngit-win-symbol (if (and (or curl zlib) directory (not (string= directory "")))
                         (expand-file-name dll directory) dll) symbol)))

(defun ngit-http--check (code operation)
  (unless (= code 0)
    ;; Never include libcurl's error buffer: it can contain a credential URL.
    (ngit-transport--fail "fatal: HTTP %s failed (code %d)" operation code)))

(defun ngit-http--set (handle option value)
  (ngit-http--check (ngit-http--call "curl_easy_setopt" handle option value) "option"))

(defun ngit-http--string (text)
  (let* ((bytes (encode-coding-string text 'utf-8))
         (owner (nl-ffi-memory-allocate (1+ (length bytes))))
         (p (nl-ffi-memory-address owner)))
    (ptr-write-bytes p bytes)
    (ptr-write-u8 p (length bytes) 0)
    owner))

(defun ngit-http--file ()
  (let ((p (ngit-http--call "tmpfile")))
    (when (zerop p) (ngit-transport--fail "fatal: cannot create HTTP spool"))
    p))

(defun ngit-http--fd (file)
  (let ((fd (ngit-http--call "fileno" file)))
    (when (> fd #x7fffffff) (ngit-transport--fail "fatal: invalid HTTP spool descriptor"))
    fd))

(defun ngit-http--seek (file offset)
  (ngit-http--check (ngit-http--call "fseek" file offset 0) "spool seek"))

(defun ngit-http--url (url)
  "Return (CLEAN-URL ORIGIN USERPWD SCHEME AUTHORITY PATH) for URL."
  (when (or (string-match-p "[\r\n\0]" url) (string-search "#" url))
    (ngit-transport--fail "fatal: invalid HTTP URL"))
  (unless (string-match "\\`\\(https?\\)://\\([^/]+\\)\\(/.*\\)?\\'" url)
    (ngit-transport--fail "fatal: unsupported HTTP redirect URL"))
  (let* ((scheme (match-string 1 url)) (authority (match-string 2 url))
         (path (or (match-string 3 url) "/"))
         (at (string-search "@" authority)) credentials)
    (when at
      (setq credentials (substring authority 0 at)
            authority (substring authority (1+ at))))
    (unless (and (not (string-search "@" authority))
                 (string-match-p "\\`\\(?:\\[[0-9A-Fa-f:]+\\]\\|[A-Za-z0-9.-]+\\)\\(?::[0-9]+\\)?\\'" authority))
      (ngit-transport--fail "fatal: invalid HTTP authority"))
    (let* ((lower (downcase authority))
           (origin (concat scheme "://" lower
                           (if (string-match-p "\\(?::[0-9]+\\)\\'" lower) ""
                             (if (equal scheme "https") ":443" ":80")))))
      (list (concat scheme "://" authority path) origin credentials scheme authority path))))

(defun ngit-http--redirect (url location)
  (let ((parts (ngit-http--url url)))
    (cond ((string-match-p "\\`https?://" location) location)
          ((string-prefix-p "//" location) (concat (nth 3 parts) ":" location))
          ((string-prefix-p "/" location) (concat (nth 3 parts) "://" (nth 4 parts) location))
          (t (concat (nth 3 parts) "://" (nth 4 parts)
                     (expand-file-name location (file-name-directory (nth 5 parts))))))))

(defun ngit-http--headers (repo)
  "Return ordered extraHeader values, respecting an empty reset."
  (let (headers)
    (dolist (value (ngit-config-get-all repo "http.extraHeader"))
      (unless (and (stringp value) (not (string-match-p "[\r\n\0]" value)))
        (ngit-transport--fail "fatal: invalid http.extraHeader"))
      (if (equal value "") (setq headers nil) (push value headers)))
    (nreverse headers)))

(defun ngit-http--header-values (file)
  "Parse bounded response headers; return lowercase field/value pairs."
  (ngit-http--check (ngit-http--call "fflush" file) "header flush")
  (let ((size (ngit-http--call "ftell" file)))
    (unless (<= 0 size 65536) (ngit-transport--fail "fatal: excessive HTTP response headers"))
    (ngit-http--seek file 0)
    (let* ((owner (nl-ffi-memory-allocate (max size 1)))
           (p (nl-ffi-memory-address owner)) values)
      (unwind-protect
          (progn
            (unless (= size (ngit-http--call "fread" p 1 size file))
              (ngit-transport--fail "fatal: cannot read HTTP headers"))
            (dolist (line (split-string (ptr-read-bytes p size) "\n"))
              (setq line (string-trim-right line "\r"))
              (cond ((string-prefix-p "HTTP/" line) (setq values nil))
                    ((string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
                     (push (cons (downcase (match-string 1 line)) (match-string 2 line)) values))))
            (nreverse values))
        (nl-ffi-memory-release owner)))))

(defun ngit-http--gzip (file)
  "Return (FILE . SIZE) for a streamed gzip copy of request FILE."
  (let ((output (ngit-http--file)) (fd -1) stream buffer mode success)
    (unwind-protect
        (progn
          (setq fd (ngit-http--call "dup" (ngit-http--fd output)))
          (when (> fd #x7fffffff) (ngit-transport--fail "fatal: cannot duplicate HTTP spool"))
          (setq mode (ngit-http--string "wb9")
                stream (ngit-http--call "gzdopen" fd (nl-ffi-memory-address mode)))
          (when (zerop stream) (ngit-transport--fail "fatal: cannot compress HTTP request"))
          (setq fd -1 buffer (nl-ffi-memory-allocate 65536))
          (ngit-http--seek file 0)
          (let ((p (nl-ffi-memory-address buffer)) (got 0))
            (while (> (setq got (ngit-http--call "fread" p 1 65536 file)) 0)
              (unless (= got (ngit-http--call "gzwrite" stream p got))
                (ngit-transport--fail "fatal: cannot compress HTTP request"))))
          (unless (= 0 (ngit-http--call "ferror" file))
            (ngit-transport--fail "fatal: cannot read HTTP request"))
          (let ((z stream))
            (setq stream nil)
            (ngit-http--check (ngit-http--call "gzclose" z) "gzip close"))
          (let ((size (ngit-http--call "ftell" output)))
            (ngit-http--seek output 0)
            (setq success t)
            (cons output size)))
      (when (and stream (not (zerop stream))) (ngit-http--call "gzclose" stream))
      (when (>= fd 0) (ngit-http--call "close" fd))
      (when buffer (nl-ffi-memory-release buffer))
      (when mode (nl-ffi-memory-release mode))
      (unless success (ngit-http--call "fclose" output)))))

(defun ngit-http--payload (state)
  "Return (FILE SIZE CHUNKED GZIP) using remote-curl's buffer rules."
  (let* ((file (aref state 3)) (size (aref state 8))
         (limit (max 65520 (ngit-config-int (aref state 0) "http.postBuffer" (* 16 65520))))
         ;; v0 wraps the whole request in outer packets and needs room to
         ;; read their final flush even after all payload bytes are buffered.
         (chunked (> size (if (= (aref state 9) 2) limit (- limit 65520)))))
    ;; remote-curl reserves a complete maximum packet before each read,
    ;; including the final flush; a nearly full buffer also streams.
    (when (and (= (aref state 9) 2) (not chunked) (> size (- limit 65524)))
      (let ((owner (nl-ffi-memory-allocate 4)) (offset 0) done)
        (unwind-protect
            (while (not done)
              (if (< (- limit offset 4) 65520)
                  (setq chunked t done t)
                (ngit-http--seek file offset)
                (unless (= 4 (ngit-http--call "fread" (nl-ffi-memory-address owner) 1 4 file))
                  (ngit-transport--fail "fatal: malformed HTTP request packet"))
                (let* ((header (ptr-read-bytes (nl-ffi-memory-address owner) 4))
                       (n (string-to-number header 16)))
                  (unless (and (ngit-hex-p header 4) (or (memq n '(0 1)) (<= 4 n 65520)))
                    (ngit-transport--fail "fatal: malformed HTTP request packet"))
                  (setq offset (+ offset (max n 4)) done (= n 0)))))
          (nl-ffi-memory-release owner))))
    ;; Keep zlib's private CRT away from the owned UCRT descriptors.
    (if (and (not (ngit-windows-p)) (not chunked) (> size 1024))
        (let ((compressed (ngit-http--gzip file)))
          (list (car compressed) (cdr compressed) nil t))
      (list file size chunked nil))))

(defun ngit-http--request (state url post &optional payload)
  "Perform one request, with bounded manual redirects only for discovery."
  (let ((redirects 0) (done nil) (challenged nil) result)
    (while (not done)
      (let ((context (aref state 7))
            (custom-auth (seq-some (lambda (header) (string-prefix-p "authorization:" (downcase header))) (aref state 6)))
            handle body head info owners slist used)
        (unwind-protect
            (progn
              (setq handle (ngit-http--call "curl_easy_init"))
              (when (zerop handle) (ngit-transport--fail "fatal: cannot initialize HTTP"))
              (setq body (ngit-http--file) head (ngit-http--file)
                    info (nl-ffi-memory-allocate 8))
              (dolist (option '((99 . 1) (64 . 1) (81 . 2) (52 . 0) (78 . 10) (13 . 120) (181 . 3)))
                (ngit-http--set handle (car option) (cdr option)))
              (dolist (entry (list (cons 10002 url)
                                  (cons 10018 "ngit/0 smart-http")
                                  (cons 10065 (or (getenv "GIT_SSL_CAINFO")
                                                 (ngit-config-get (aref state 0) "http.sslCAInfo")))
                                  (cons 10173 (and (not custom-auth) (ngit-credential--complete context)
                                                   (cdr (assoc "username" (aref context 1)))))
                                  (cons 10174 (and (not custom-auth) (ngit-credential--complete context)
                                                   (cdr (assoc "password" (aref context 1)))))))
                (when (cdr entry)
                  (let ((owner (ngit-http--string (cdr entry))))
                    (push owner owners)
                    (ngit-http--set handle (car entry) (nl-ffi-memory-address owner)))))
              (setq used (and (not custom-auth) (ngit-credential--complete context)))
              (ngit-http--set handle 107 1) ; CURLAUTH_BASIC; separate decoded fields.
              (ngit-http--set handle 10001 body)
              (ngit-http--set handle 10029 head)
              (when (ngit-windows-p)
                ;; Both callbacks match fread/fwrite's four-argument ABI.
                ;; FILE pointers and callbacks must belong to the same CRT.
                (ngit-http--set handle 20011 (ngit-win-symbol "ucrtbase.dll" "fwrite"))
                (ngit-http--set handle 20079 (ngit-win-symbol "ucrtbase.dll" "fwrite"))
                (ngit-http--set handle 20012 (ngit-win-symbol "ucrtbase.dll" "fread")))
              (dolist (value (append (aref state 6)
                                    (if post
                                        (list (concat "Content-Type: application/x-" (aref state 2) "-request")
                                              (concat "Accept: application/x-" (aref state 2) "-result")
                                              "Expect:" "Pragma:")
                                      (list "Accept: */*" "Pragma: no-cache"))
                                    (when (and (not (eq post 'probe)) (> (aref state 9) 0))
                                      (list (format "Git-Protocol: version=%d" (aref state 9))))
                                    (when (nth 3 payload) (list "Content-Encoding: gzip"))))
                (let* ((owner (ngit-http--string value))
                       (next (ngit-http--call "curl_slist_append" (or slist 0) (nl-ffi-memory-address owner))))
                  (push owner owners)
                  (when (zerop next) (ngit-transport--fail "fatal: cannot allocate HTTP header"))
                  (setq slist next)))
              (ngit-http--set handle 10023 slist)
              (when post
                (ngit-http--seek (or (car payload) (aref state 3)) 0)
                (ngit-http--set handle 47 1)
                (ngit-http--set handle 10015 0)
                (ngit-http--set handle 10009 (or (car payload) (aref state 3)))
                (ngit-http--set handle 30120 (if (nth 2 payload) -1 (or (nth 1 payload) (aref state 8)))))
              (ngit-http--check (ngit-http--call "curl_easy_perform" handle) "transfer")
              (ngit-http--check (ngit-http--call "fflush" body) "body flush")
              (unless (= 0 (ngit-http--call "ferror" body))
                (ngit-transport--fail "fatal: HTTP spool write failed"))
              (ngit-http--check (ngit-http--call "curl_easy_getinfo" handle #x200002 (nl-ffi-memory-address info)) "status")
              (let* ((status (ptr-read-u64 (nl-ffi-memory-address info) 0))
                     (headers (ngit-http--header-values head))
                     (location (cdr (assoc "location" headers))))
                (cond
                 ((memq status '(301 302 303 307 308))
                  (when post (ngit-transport--fail "fatal: HTTP RPC redirect refused"))
                  (unless (and location (< redirects 5))
                    (ngit-transport--fail "fatal: HTTP redirect limit or missing location"))
                  (let* ((next (ngit-http--url (ngit-http--redirect url location)))
                         (old (ngit-http--url url))
                         (suffix (concat "/info/refs?service=" (aref state 2))))
                    (when (nth 2 next) (ngit-transport--fail "fatal: HTTP redirect userinfo refused"))
                    (unless (equal (nth 1 next) (nth 1 old))
                      (aset state 6 nil))
                    (unless (string-suffix-p suffix (car next))
                      (ngit-transport--fail "fatal: unsupported HTTP discovery redirect"))
                    (when (or (not (equal (nth 1 next) (nth 1 old)))
                              (and (aref context 3) (not (equal (nth 5 next) (nth 5 old)))))
                      (aset state 7 (ngit-credential-create (aref state 0)
                                     (substring (car next) 0 (- (length (car next)) (length suffix)))))
                      (setq challenged nil))
                    (setq url (car next) redirects (1+ redirects))))
                 ((= status 401)
                  (when used (ngit-credential-reject context))
                  (when (or used custom-auth post challenged)
                    (ngit-transport--fail "fatal: HTTP server returned status 401"))
                  (setq challenged t)
                  (ngit-credential-fill context))
                 ((/= status 200) (ngit-transport--fail "fatal: HTTP server returned status %d" status))
                 (t
                  (when used (ngit-credential-approve context))
                  (unless (equal (downcase (car (split-string (or (cdr (assoc "content-type" headers)) "") ";")))
                                 (concat "application/x-" (aref state 2) (if post "-result" "-advertisement")))
                    (ngit-transport--fail "fatal: unsupported smart HTTP response type"))
                  (ngit-http--seek body 0)
                  (setq done t result (cons body url) body nil)))))
          (when (and handle (/= handle 0)) (ngit-http--call "curl_easy_cleanup" handle))
          (when slist (ngit-http--call "curl_slist_free_all" slist))
          (dolist (owner owners) (nl-ffi-memory-release owner))
          (when info (nl-ffi-memory-release info))
          (when head (ngit-http--check (ngit-http--call "fclose" head) "header close"))
          (when body (ngit-http--check (ngit-http--call "fclose" body) "body close")))))
    result))

(defun ngit-http--packet (conn)
  (let ((header (ngit-transport-read conn 4)))
    (unless (and header (= (length header) 4) (ngit-hex-p header 4))
      (ngit-transport--fail "fatal: malformed smart HTTP advertisement"))
    (let ((size (string-to-number header 16)))
      (cond ((= size 0) 'flush)
            ((or (< size 4) (> size 65520)) (ngit-transport--fail "fatal: unsupported HTTP packet"))
            (t (or (ngit-transport-read conn (- size 4))
                   (ngit-transport--fail "fatal: truncated HTTP packet")))))))

(defun ngit-http-open (repo url service)
  "Open smart HTTP SERVICE, validating its discovery advertisement."
  (require 'ngit-credential)
  (let* ((parts (ngit-http--url url))
         (base (replace-regexp-in-string "/+\\'" "" (car parts)))
         ;; State: repo, base, service, request FILE, response FILE, posted,
         ;; ordered custom headers, credentials, request bytes, version
         ;; (requested during discovery, negotiated thereafter).
         (state (vector repo base service nil nil nil (ngit-http--headers repo) (ngit-credential-create repo url) 0
                        (ngit-transport-protocol-version repo service)))
         (conn (vector ngit-transport--tag 0 -1 -1 nil nil state)) success)
    (unwind-protect
        (progn
          (let* ((suffix (concat "/info/refs?service=" service))
                 (response (ngit-http--request state (concat base suffix) nil))
                 (effective (cdr response)))
            (aset state 4 (car response))
            (unless (string-suffix-p suffix effective)
              (ngit-transport--fail "fatal: unsupported HTTP discovery redirect"))
            (aset state 1 (substring effective 0 (- (length effective) (length suffix))))
            (aset conn 3 (ngit-http--fd (car response))))
          (let ((preamble (ngit-http--packet conn)) (offset 0) (version 0))
            (cond
             ((member preamble (list (concat "# service=" service)
                                     (concat "# service=" service "\n")))
              (unless (eq (ngit-http--packet conn) 'flush)
                (ngit-transport--fail "fatal: invalid smart HTTP service preamble"))
              (setq offset (+ 8 (length preamble))))
             ((member preamble '("version 2" "version 2\n")) (setq version 2))
             (t (ngit-transport--fail "fatal: invalid smart HTTP service preamble")))
            (let ((packet (ngit-http--packet conn)) (first t)
                  (seen (make-hash-table :test 'equal)))
              (when (and (= version 0) (member packet '("version 1" "version 1\n")))
                (setq version 1 packet (ngit-http--packet conn)))
              (while (stringp packet)
                (unless (= version 2)
                  (unless (and (>= (length packet) 42) (ngit-hex-p (substring packet 0 40) 40)
                               (= (aref packet 40) ?\s)
                               (string-suffix-p "\n" packet)
                               (or first (not (string-search "\0" packet))))
                    (ngit-transport--fail "fatal: invalid HTTP advertised ref"))
                  (when (string-search "object-format=sha256" packet)
                    (ngit-transport--fail "fatal: unsupported HTTP object format"))
                  (let* ((nul (string-search "\0" packet))
                         (name (substring packet 41 (or nul (1- (length packet)))))
                         (plain (if (string-suffix-p "^{}" name) (substring name 0 -3) name))
                         (zero (equal (substring packet 0 40) (make-string 40 ?0))))
                    (unless (and (not (gethash name seen))
                                 (if zero (and first (equal name "capabilities^{}"))
                                   (or (equal name "HEAD")
                                       (and (string-prefix-p "refs/" plain)
                                            (ngit--valid-branch-name-p plain)))))
                      (ngit-transport--fail "fatal: invalid HTTP advertised ref name"))
                    (puthash name t seen)))
                (setq first nil packet (ngit-http--packet conn)))
              (unless (and (eq packet 'flush) (null (ngit-transport-read conn 1)))
                (ngit-transport--fail "fatal: invalid HTTP advertisement termination")))
            (aset state 9 version)
            (ngit-http--seek (aref state 4) offset)
            (aset conn 5 nil))
          (aset state 3 (ngit-http--file))
          (aset conn 2 (ngit-http--fd (aref state 3)))
          (setq success t) conn)
      (unless success (ngit-http-close conn)))))

(defun ngit-http-p (conn)
  (and (> (length conn) 6) (vectorp (aref conn 6)) (= (length (aref conn 6)) 10)))

(defun ngit-http-before-write (conn bytes)
  "Account for BYTES and start the next upload-pack RPC after a drained response."
  (let ((state (aref conn 6)))
    (when (aref state 5)
      (unless (equal (aref state 2) "git-upload-pack")
        (ngit-transport--fail "fatal: HTTP RPC already sent"))
      ;; Packet readers normally stop at the final flush, without an EOF
      ;; read.  Check the spool position instead of relying on CONN's EOF.
      (let* ((fd (aref conn 3))
             (position (ngit-http--call "lseek" fd 0 1))
             (end (ngit-http--call "lseek" fd 0 2)))
        (unless (= position (ngit-http--call "lseek" fd position 0))
          (ngit-transport--fail "fatal: cannot seek HTTP response"))
        (unless (and (>= position 0) (= position end))
          (ngit-transport--fail "fatal: HTTP response not fully read")))
      (let ((file (aref state 3)))
        (aset state 3 nil)
        (aset conn 2 -1)
        (ngit-http--check (ngit-http--call "fclose" file) "request close"))
      (aset state 3 (ngit-http--file))
      (aset conn 2 (ngit-http--fd (aref state 3)))
      (aset state 8 0)
      (aset state 5 nil))
    (aset state 8 (+ (aref state 8) (string-bytes bytes)))))

(defun ngit-http-response-end (conn command)
  "Require the end of a stateless COMMAND response after its final flush."
  (let ((header (ngit-transport-read conn 4)))
    (when header
      (unless (and (= (length header) 4) (ngit-hex-p header 4))
        (ngit--die "fatal: protocol error: bad line length character: %.4s" header))
      (let ((size (string-to-number header 16)))
        (when (or (= size 3) (> size 65520))
          (ngit--die "fatal: protocol error: bad line length %d" size))
        (when (> size 4)
          (let ((line (ngit-transport-read conn (- size 4))))
            (when (and line (string-prefix-p "ERR " line))
              (ngit--die "fatal: remote error: %s" (string-trim-right (substring line 4)))))))
      (ngit-transport--fail
       (if (eq command 'ls-refs)
           "fatal: expected response end packet after ref listing"
         "fatal: git fetch-pack: expected response end packet")))))

(defun ngit-http--check-v2-response (file request)
  "Check every RPC packet, including bytes after a flush, as remote-curl does."
  (let ((owner (nl-ffi-memory-allocate 24)) (offset 0) size lsrefs)
    (unwind-protect
        (progn
          (ngit-http--seek request 0)
          (let ((count (ngit-http--call "fread" (nl-ffi-memory-address owner) 1 24 request)))
            (setq lsrefs (string-search "command=ls-refs"
                                        (ptr-read-bytes (nl-ffi-memory-address owner) count))))
          (ngit-http--seek request 0)
          (ngit-http--check (ngit-http--call "fseek" file 0 2) "response seek")
          (setq size (ngit-http--call "ftell" file))
          (while (< offset size)
            (let ((available (- size offset)))
              (when (< available 4)
                (ngit-native-write-stderr
                 (format "error: %d bytes of length header were received\n" available))
                (ngit-transport--fail
                 (if lsrefs "fatal: expected response end packet after ref listing"
                   "fatal: the remote end hung up unexpectedly")))
              (ngit-http--seek file offset)
              (unless (= 4 (ngit-http--call "fread" (nl-ffi-memory-address owner) 1 4 file))
                (ngit-transport--fail "fatal: cannot read HTTP response"))
              (let ((header (ptr-read-bytes (nl-ffi-memory-address owner) 4)))
                (unless (ngit-hex-p header 4)
                  (ngit-native-write-stderr
                   (format "fatal: remote-curl: bad line length character: %.4s\n" header))
                  (ngit-transport--fail
                   (if lsrefs "fatal: expected flush after ref listing"
                     "fatal: the remote end hung up unexpectedly")))
                (let ((length (string-to-number header 16)))
                  (when (= length 2)
                    (ngit-native-write-stderr "fatal: remote-curl: unexpected response end packet\n")
                    (ngit-transport--fail "fatal: the remote end hung up unexpectedly"))
                  (when (> length available)
                    (ngit-native-write-stderr
                     (format "error: %d bytes of body are still expected\n" (- length available)))
                    (ngit-transport--fail
                     (if lsrefs "fatal: expected response end packet after ref listing"
                       "fatal: git fetch-pack: expected response end packet")))
                  (setq offset (+ offset (max length 4))))))))
      (ngit-http--seek file 0)
      ;; fread may retain lookahead; transport reads use the raw descriptor.
      (unless (= 0 (ngit-http--call "lseek" (ngit-http--fd file) 0 0))
        (ngit-transport--fail "fatal: cannot seek HTTP response"))
      (nl-ffi-memory-release owner))))

(defun ngit-http-ready (conn)
  "Send a pending complete RPC once, before its first response read."
  (let ((state (aref conn 6)))
    (when (and (> (aref state 8) 0) (not (aref state 5)))
      ;; Mark before performing: errors and unwind cleanup must never retry push.
      (aset state 5 t)
      (let* ((url (concat (aref state 1) "/" (aref state 2)))
             (payload (when (equal (aref state 2) "git-upload-pack")
                        (ngit-http--payload state)))
             response)
        (unwind-protect
            (progn
              (when (nth 2 payload)
                ;; Git probes large RPCs with a four-byte flush before
                ;; committing to a non-replayable chunked upload.
                (let ((file (ngit-http--file)) (owner (ngit-http--string "0000")))
                  (unwind-protect
                      (progn
                        (unless (= 4 (ngit-http--call "fwrite" (nl-ffi-memory-address owner) 1 4 file))
                          (ngit-transport--fail "fatal: cannot write HTTP probe"))
                        (let ((probe (ngit-http--request state url 'probe (list file 4))))
                          (ngit-http--check (ngit-http--call "fclose" (car probe)) "probe close")))
                    (ngit-http--call "fclose" file)
                    (nl-ffi-memory-release owner))))
              (setq response (ngit-http--request state url t payload)))
          (when (and payload (not (equal (car payload) (aref state 3))))
            (ngit-http--check (ngit-http--call "fclose" (car payload)) "gzip spool close")))
        (when (aref state 4)
          (ngit-http--check (ngit-http--call "fclose" (aref state 4)) "discovery close"))
        (aset state 4 (car response))
        (aset conn 3 (ngit-http--fd (car response)))
        (aset conn 5 nil)
        (when (= (aref state 9) 2)
          (ngit-http--check-v2-response (car response) (aref state 3)))))))

(defun ngit-http-close (conn)
  "Release FILE spools idempotently without ever sending an RPC."
  (let ((state (aref conn 6)) failed)
    (dolist (slot '(3 4))
      (when (aref state slot)
        (let ((file (aref state slot)))
          (aset state slot nil)
          (unless (= 0 (ngit-http--call "fclose" file)) (setq failed t)))))
    (aset conn 2 -1) (aset conn 3 -1) (aset conn 4 0)
    (when failed (ngit-transport--fail "fatal: cannot close HTTP spool"))
    0))

(provide 'ngit-transport-http)
;;; ngit-transport-http.el ends here
