;;; braid-http.el --- Braid-HTTP client -*- lexical-binding: t -*-

;; Author: Michael Toomim <toomim@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/braid-org/braid-emacs
;; Keywords: comm, tools

;;; Commentary:

;; Low-level Braid-HTTP networking library for Emacs.
;; Handles raw TCP/TLS connections, HTTP request formatting, and
;; 209 Multiresponse stream parsing with automatic reconnection.
;;
;; Public API:
;;   (braid-http-subscribe HOST PORT PATH ON-MESSAGE &key ...)  → braid-http-sub
;;   (braid-http-unsubscribe SUB)
;;   (braid-http-put HOST PORT PATH VERSION PARENTS PATCH &key ...)

;;; Code:

(require 'cl-lib)


;;;; ======================================================================
;;;; Configuration
;;;; ======================================================================

(defcustom braid-http-cookies nil
  "Alist mapping hostnames to cookie strings.
Each entry is (HOST . COOKIE-STRING) where HOST is matched against
the request's Host header value (e.g. \"example.com\" or \"localhost:8888\").
COOKIE-STRING is sent as the Cookie header value."
  :type '(alist :key-type string :value-type string)
  :group 'braid)

(defun braid-http--cookie-header (host port)
  "Return a Cookie header alist entry for HOST:PORT, or nil.
Looks up the formatted host (via `braid-http--format-host') in
`braid-http-cookies'."
  (when-let ((cookie (cdr (assoc (braid-http--format-host host port)
                                 braid-http-cookies))))
    `(("Cookie" . ,cookie))))


;;;; ======================================================================
;;;; Structured Headers: Version / Parents
;;;; ======================================================================
;; Format per draft-toomim-httpbis-versions:
;;   single:    "server-0"
;;   multiple:  "v1", "v2"

(defun braid-http--format-version (versions)
  "Format list of version strings as a Structured Headers value.
Example: (\"v1\" \"v2\") → \"\\\"v1\\\", \\\"v2\\\"\""
  (mapconcat (lambda (v) (format "%S" v)) versions ", "))

(defun braid-http--parse-version (str)
  "Parse a Structured Headers version value into a list of strings.
Example: \"\\\"v1\\\", \\\"v2\\\"\" → (\"v1\" \"v2\").
Returns nil for nil or empty input."
  (when (and str (not (string-empty-p (string-trim str))))
    (let (result)
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (while (re-search-forward "\"\\([^\"]*\\)\"" nil t)
          (push (match-string 1) result)))
      (nreverse result))))


;;;; ======================================================================
;;;; Content-Range header
;;;; ======================================================================

(defun braid-http--parse-content-range (str)
  "Parse a Content-Range header value.
For text ranges: \"text [7:12]\" → (\"text\" 7 12).
For json ranges: \"json [\\\"key\\\"]\" → (\"json\" \"key\").
Returns nil if STR is nil or doesn't match."
  (when str
    (cond
     ;; text [N:N]
     ((string-match
       "\\([A-Za-z][A-Za-z0-9]*\\)\\s-*\\[\\([0-9]+\\):\\([0-9]+\\)\\]"
       str)
      (list (match-string 1 str)
            (string-to-number (match-string 2 str))
            (string-to-number (match-string 3 str))))
     ;; json ["key"]
     ((string-match
       "json\\s-*\\[\"\\([^\"]*\\)\"\\]"
       str)
      (list "json" (match-string 1 str))))))


;;;; ======================================================================
;;;; Header block parsing
;;;; ======================================================================

(defun braid-http--parse-header-block (str)
  "Parse a block of header lines (no status line, no trailing blank line).
Returns an alist of (lowercase-name . value) pairs."
  (let (headers)
    (dolist (line (split-string str "\r\n") (nreverse headers))
      (when (string-match "^\\([^:\r\n]+\\):\\s-*\\(.*?\\)\\s-*$" line)
        (push (cons (downcase (match-string 1 line))
                    (match-string 2 line))
              headers)))))


;;;; ======================================================================
;;;; Request formatting
;;;; ======================================================================

(defun braid-http--format-host (host port)
  "Format HOST:PORT for an HTTP Host header, omitting default ports."
  (if (or (= port 80) (= port 443))
      host
    (format "%s:%d" host port)))

(defun braid-http--format-get (host port path headers)
  "Format an HTTP GET request string.
HEADERS is an alist of (name . value) pairs."
  (let ((all-headers (append headers (braid-http--cookie-header host port))))
    (concat (format "GET %s HTTP/1.1\r\n" path)
            (format "Host: %s\r\n" (braid-http--format-host host port))
            (mapconcat (lambda (h) (format "%s: %s\r\n" (car h) (cdr h)))
                       all-headers "")
            "\r\n")))

(defun braid-http--format-put (host port path version parents body-headers body)
  "Format an HTTP PUT request string.
VERSION and PARENTS are lists of version strings (Structured Headers format).
BODY-HEADERS is an alist of additional headers (Content-Type, etc.).
BODY is a unibyte string of the request body."
  (let* ((version-headers
          (append `(("Host"    . ,(braid-http--format-host host port))
                    ("Version" . ,(braid-http--format-version version))
                    ("Parents" . ,(braid-http--format-version parents)))
                  body-headers
                  (braid-http--cookie-header host port))))
    (concat (format "PUT %s HTTP/1.1\r\n" path)
            (mapconcat (lambda (h) (format "%s: %s\r\n" (car h) (cdr h)))
                       version-headers "")
            "\r\n"
            body)))


;;;; ======================================================================
;;;; Chunked transfer-encoding decoder
;;;; ======================================================================
;; Wire format per RFC 7230 §4.1:
;;   chunk = chunk-size CRLF chunk-data CRLF
;;   chunk-size = 1*HEXDIG [ ";" chunk-ext ]
;;   last-chunk = 1*("0") CRLF CRLF

(defun braid-http--dechunk (raw body)
  "Strip chunked transfer-encoding framing from RAW, appending to BODY.
Returns (new-raw . new-body).  Partial chunks stay in new-raw."
  (catch 'need-more
    (while t
      (let ((crlf (string-match "\r\n" raw)))
        (unless crlf (throw 'need-more (cons raw body)))
        (let* ((size-hex (car (split-string (substring raw 0 crlf) ";")))
               (size     (string-to-number (string-trim size-hex) 16)))
          (if (= size 0)
              ;; Terminal chunk — consume it and stop
              (throw 'need-more (cons "" body))
            (let ((data-start (+ crlf 2))
                  (data-end   (+ crlf 2 size)))
              (unless (>= (length raw) (+ data-end 2))
                (throw 'need-more (cons raw body)))
              (setq body (concat body (substring raw data-start data-end)))
              (setq raw  (substring raw (+ data-end 2))))))))))


;;;; ======================================================================
;;;; Subscription struct
;;;; ======================================================================

(cl-defstruct braid-http-sub
  "State for a long-lived Braid-HTTP 209 subscription."
  host port path peer
  (tls nil)               ; t to use TLS (HTTPS)
  process
  ;; Receive buffers
  (raw-buf  "")           ; raw bytes from TCP, not yet dechunked
  (body-buf "")           ; dechunked body bytes, not yet parsed
  ;; Parse state
  ;;   :outer-headers  — reading the HTTP/1.1 209 response line + headers
  ;;   :sub-headers    — reading the next "200 OK\r\n<headers>\r\n\r\n"
  ;;   :body           — reading Content-Length bytes (snapshot or single patch)
  ;;   :patch-n-headers — reading headers of the next patch in a Patches: N block
  ;;   :patch-n-body    — reading the body of the current patch in a Patches: N block
  (stage :outer-headers)
  (chunked nil)           ; t if Transfer-Encoding: chunked
  ;; Current sub-response being assembled
  (cur-headers nil)       ; alist of the current "200 OK" response's headers
  (cur-cl      0)         ; Content-Length of the current body unit
  ;; Patches: N block state
  (cur-patches-n    0)    ; total patches expected in this Patches: N block
  (cur-patches-i    0)    ; patches read so far
  (cur-patches-list nil)  ; accumulated patches (reversed), each a plist
  (cur-patch-cr     nil)  ; content-range of the patch currently being read
  ;; Reconnection
  (status           :connecting)  ; :connecting | :open | :connected | :disconnected | :closed
  (reconnect-timer  nil)
  (reconnect-delay  1.0)   ; seconds; doubles on each failure
  (reconnect-max-delay 30.0) ; upper bound for reconnect-delay
  (extra-headers    nil)   ; alist: extra GET headers (preserved across reconnects)
  (last-parents     nil)   ; list of strings: sent as Parents on reconnect
  (parents-fn       nil)   ; function returning parents list; overrides last-parents on reconnect
  ;; Heartbeat-based dead-connection detection
  (heartbeat-interval nil)   ; requested interval in seconds, or nil to disable
  (heartbeat-timer    nil)   ; recurring check timer
  (last-data-time     nil)   ; (float-time) of last data in filter
  ;; Callbacks
  on-message    ; (lambda (msg-plist))
  on-connect    ; (lambda ())
  on-disconnect) ; (lambda ())


;;;; ======================================================================
;;;; Parse pump
;;;; ======================================================================

(defun braid-http--pump (sub)
  "Drive SUB's parse state machine until no more progress can be made."
  ;; Phase 1: outer HTTP response headers (read from raw-buf)
  (when (eq (braid-http-sub-stage sub) :outer-headers)
    (unless (braid-http--try-outer-headers sub)
      (cl-return-from braid-http--pump nil)))

  ;; Phase 2: dechunk raw-buf → body-buf
  (let ((result (if (braid-http-sub-chunked sub)
                    (braid-http--dechunk (braid-http-sub-raw-buf sub)
                                        (braid-http-sub-body-buf sub))
                  ;; Not chunked: pass through directly
                  (cons "" (concat (braid-http-sub-body-buf sub)
                                   (braid-http-sub-raw-buf sub))))))
    (setf (braid-http-sub-raw-buf  sub) (car result))
    (setf (braid-http-sub-body-buf sub) (cdr result)))

  ;; Phase 3: parse sub-responses from body-buf
  (let ((keep-going t))
    (while keep-going
      (setq keep-going
            (pcase (braid-http-sub-stage sub)
              (:sub-headers    (braid-http--try-sub-headers sub))
              (:body           (braid-http--try-body sub))
              (:patch-n-headers (braid-http--try-patch-n-headers sub))
              (:patch-n-body    (braid-http--try-patch-n-body sub))
              (_               nil))))))

(defun braid-http--try-outer-headers (sub)
  "Try to parse the outer HTTP/1.1 response line and headers.
On success, advances stage to :sub-headers (or :error) and returns t.
Returns nil if more data is needed."
  (let ((raw (braid-http-sub-raw-buf sub)))
    (when-let ((end (string-match "\r\n\r\n" raw)))
      (let* ((block  (substring raw 0 end))
             (lines  (split-string block "\r\n"))
             (sl     (car lines))  ; status line
             (status (when (string-match "HTTP/[^ ]+ \\([0-9]+\\)" sl)
                       (string-to-number (match-string 1 sl))))
             (hdrs   (braid-http--parse-header-block
                      (mapconcat #'identity (cdr lines) "\r\n")))
             (te     (cdr (assoc "transfer-encoding" hdrs))))
        (setf (braid-http-sub-raw-buf sub) (substring raw (+ end 4)))
        (setf (braid-http-sub-chunked sub)
              (and te (string-match-p "chunked" (downcase te))))
        (if (eql status 209)
            (progn
              (setf (braid-http-sub-stage  sub) :sub-headers)
              (setf (braid-http-sub-status sub) :connected)
              (setf (braid-http-sub-reconnect-delay sub) 1.0) ; reset backoff
              ;; Start heartbeat watchdog timer
              (when-let ((hb (braid-http-sub-heartbeat-interval sub)))
                (setf (braid-http-sub-last-data-time sub) (float-time))
                (let ((timeout (+ (* 1.2 hb) 3)))
                  (setf (braid-http-sub-heartbeat-timer sub)
                        (run-with-timer
                         15 15
                         (lambda ()
                           (when (and (eq (braid-http-sub-status sub) :connected)
                                      (braid-http-sub-last-data-time sub)
                                      (> (- (float-time) (braid-http-sub-last-data-time sub))
                                         timeout))
                             (message "Braid: heartbeat timeout (no data for %.0fs) — reconnecting" timeout)
                             (delete-process (braid-http-sub-process sub))))))))
              (when (braid-http-sub-on-connect sub)
                (funcall (braid-http-sub-on-connect sub))))
          ;; Non-209 (e.g. 200 snapshot): parse body via the existing :body stage
          (let* ((cl-str (cdr (assoc "content-length" hdrs)))
                 (cl     (if cl-str (string-to-number cl-str) 0)))
            (setf (braid-http-sub-cur-headers sub) hdrs)
            (setf (braid-http-sub-cur-cl      sub) cl)
            (setf (braid-http-sub-stage       sub) :body)))
        t))))

(defun braid-http--try-sub-headers (sub)
  "Try to parse the next sub-response status line and headers from body-buf.
Returns t if a complete header block was parsed, nil if more data needed."
  (let ((buf (braid-http-sub-body-buf sub)))
    ;; Skip leading bare CRLFs (heartbeats)
    (while (and (>= (length buf) 2) (string-prefix-p "\r\n" buf))
      (setq buf (substring buf 2)))
    (setf (braid-http-sub-body-buf sub) buf)
    ;; Now look for end of header block
    (when-let ((end (string-match "\r\n\r\n" buf)))
      (let* ((block   (substring buf 0 end))
             (lines   (split-string block "\r\n"))
             ;; First line is the sub-response status ("200 OK"), rest are headers
             (headers (braid-http--parse-header-block
                       (mapconcat #'identity (cdr lines) "\r\n")))
             (cl-str  (cdr (assoc "content-length" headers)))
             (cl      (if cl-str (string-to-number cl-str) 0)))
        (setf (braid-http-sub-cur-headers sub) headers)
        (setf (braid-http-sub-cur-cl      sub) cl)
        (setf (braid-http-sub-body-buf    sub) (substring buf (+ end 4)))
        ;; Branch: Patches: N block, or plain body
        (let ((patches-str (cdr (assoc "patches" headers))))
          (if patches-str
              (let ((n (string-to-number patches-str)))
                (setf (braid-http-sub-cur-patches-n    sub) n)
                (setf (braid-http-sub-cur-patches-i    sub) 0)
                (setf (braid-http-sub-cur-patches-list sub) nil)
                (setf (braid-http-sub-stage            sub) :patch-n-headers))
            (setf (braid-http-sub-stage sub) :body)))
        t))))

(defun braid-http--try-body (sub)
  "Try to read the current sub-response's body (snapshot or single patch).
Returns t if fully read, nil if more data needed."
  (let* ((buf (braid-http-sub-body-buf sub))
         (cl  (braid-http-sub-cur-cl sub)))
    (when (>= (length buf) cl)
      (let* ((raw-body  (substring buf 0 cl))
             (body-text (decode-coding-string raw-body 'utf-8))
             (headers   (braid-http-sub-cur-headers sub))
             (ver-str   (cdr (assoc "version"       headers)))
             (par-str   (cdr (assoc "parents"       headers)))
             (cr-str    (cdr (assoc "content-range" headers)))
             (version   (braid-http--parse-version ver-str))
             (parents   (braid-http--parse-version par-str))
             (msg       (list :version        version
                              :parents        parents
                              :content-range  (braid-http--parse-content-range cr-str)
                              :content-length cl
                              :body           body-text
                              :patches        nil
                              :headers        headers)))
        (setf (braid-http-sub-body-buf sub) (substring buf cl))
        (setf (braid-http-sub-stage    sub) :sub-headers)
        (when version
          (setf (braid-http-sub-last-parents sub) version))
        (when (braid-http-sub-on-message sub)
          (funcall (braid-http-sub-on-message sub) msg))
        t))))

(defun braid-http--try-patch-n-headers (sub)
  "Try to read one patch's headers within a Patches: N block.
Returns t if parsed, nil if more data needed."
  (let* ((buf (braid-http-sub-body-buf sub))
         (end (string-match "\r\n\r\n" buf)))
    (when end
      (let* ((block   (substring buf 0 end))
             (headers (braid-http--parse-header-block block))
             (cl-str  (cdr (assoc "content-length" headers)))
             (cr-str  (cdr (assoc "content-range"  headers)))
             (cl      (if cl-str (string-to-number cl-str) 0)))
        (setf (braid-http-sub-cur-cl       sub) cl)
        (setf (braid-http-sub-cur-patch-cr sub) (braid-http--parse-content-range cr-str))
        (setf (braid-http-sub-body-buf     sub) (substring buf (+ end 4)))
        (setf (braid-http-sub-stage        sub) :patch-n-body)
        t))))

(defun braid-http--try-patch-n-body (sub)
  "Try to read one patch's body within a Patches: N block.
Returns t if read, nil if more data needed."
  (let* ((buf (braid-http-sub-body-buf sub))
         (cl  (braid-http-sub-cur-cl sub)))
    (when (>= (length buf) cl)
      (let ((body-text (decode-coding-string (substring buf 0 cl) 'utf-8)))
        (setf (braid-http-sub-body-buf sub) (substring buf cl))
        ;; Accumulate this patch
        (push (list :content-range (braid-http-sub-cur-patch-cr sub)
                    :content-length cl
                    :body           body-text)
              (braid-http-sub-cur-patches-list sub))
        (cl-incf (braid-http-sub-cur-patches-i sub))
        (if (< (braid-http-sub-cur-patches-i sub) (braid-http-sub-cur-patches-n sub))
            ;; More patches to read
            (setf (braid-http-sub-stage sub) :patch-n-headers)
          ;; All patches done — emit the update
          (let* ((headers  (braid-http-sub-cur-headers sub))
                 (ver-str  (cdr (assoc "version" headers)))
                 (par-str  (cdr (assoc "parents" headers)))
                 (version  (braid-http--parse-version ver-str))
                 (parents  (braid-http--parse-version par-str))
                 (patches  (nreverse (braid-http-sub-cur-patches-list sub)))
                 (msg      (list :version  version
                                 :parents  parents
                                 :patches  patches
                                 :body     nil
                                 :content-range nil
                                 :headers  headers)))
            (setf (braid-http-sub-stage sub) :sub-headers)
            (when version
              (setf (braid-http-sub-last-parents sub) version))
            (when (braid-http-sub-on-message sub)
              (funcall (braid-http-sub-on-message sub) msg))))
        t))))


;;;; ======================================================================
;;;; Connection management
;;;; ======================================================================

(defun braid-http--make-process (name host port tls filter sentinel &optional nowait)
  "Open a TCP (or TLS) connection to HOST:PORT and return the process.
If TLS is non-nil, negotiate TLS using `open-network-stream'.
If NOWAIT is non-nil, return immediately before the connection is established;
the SENTINEL will be called with an \"open\" event when ready."
  (if tls
      (let ((proc (open-network-stream name nil host port
                                       :type 'tls :nowait nowait)))
        (set-process-coding-system proc 'binary 'binary)
        (set-process-query-on-exit-flag proc nil)
        (set-process-filter   proc filter)
        (set-process-sentinel proc sentinel)
        proc)
    (make-network-process
     :name     name
     :host     host
     :service  port
     :coding   '(binary . binary)
     :filter   filter
     :sentinel sentinel
     :nowait   nowait
     :noquery  t)))

(defun braid-http--open (sub parents)
  "Open a fresh TCP/TLS connection for SUB, sending a GET with PARENTS."
  ;; Cancel any old heartbeat timer from a previous connection
  (when (braid-http-sub-heartbeat-timer sub)
    (cancel-timer (braid-http-sub-heartbeat-timer sub))
    (setf (braid-http-sub-heartbeat-timer sub) nil))
  (let* ((hb-interval (braid-http-sub-heartbeat-interval sub))
         (headers (append `(("Subscribe" . "true")
                             ("Peer"      . ,(braid-http-sub-peer sub)))
                           (when hb-interval
                             `(("Heartbeats" . ,(format "%ds" hb-interval))))
                           (when parents
                             `(("Parents" . ,(braid-http--format-version parents))))
                           (braid-http-sub-extra-headers sub)))
         (request (braid-http--format-get (braid-http-sub-host sub)
                                          (braid-http-sub-port sub)
                                          (braid-http-sub-path sub)
                                          headers))
         (proc
          (braid-http--make-process
           (format "braid:%s:%d%s"
                   (braid-http-sub-host sub)
                   (braid-http-sub-port sub)
                   (braid-http-sub-path sub))
           (braid-http-sub-host sub)
           (braid-http-sub-port sub)
           (braid-http-sub-tls  sub)
           (lambda (proc data) (braid-http--filter   sub proc data))
           (lambda (proc event) (braid-http--sentinel sub proc event request))
           'nowait)))
    ;; Reset parse state for a fresh connection
    (setf (braid-http-sub-raw-buf  sub) "")
    (setf (braid-http-sub-body-buf sub) "")
    (setf (braid-http-sub-stage    sub) :outer-headers)
    (setf (braid-http-sub-chunked  sub) nil)
    (setf (braid-http-sub-status   sub) :connecting)
    (setf (braid-http-sub-process  sub) proc)))

(defun braid-http--filter (sub _proc data)
  "Process filter: accumulate DATA and pump the parser."
  (setf (braid-http-sub-last-data-time sub) (float-time))
  (setf (braid-http-sub-raw-buf sub) (concat (braid-http-sub-raw-buf sub) data))
  (condition-case err
      (braid-http--pump sub)
    (error
     (message "braid-http parse error: %S" err))))

(defun braid-http--sentinel (sub proc event &optional request)
  "Process sentinel: handle connect/disconnect events."
  (cond
   ((string-prefix-p "open" event)
    (setf (braid-http-sub-status sub) :open)
    (when request (process-send-string proc request)))

   ((not (eq (braid-http-sub-status sub) :closed))
    ;; Unexpected disconnect — call on-disconnect BEFORE updating status
    ;; so the callback can inspect the previous status (e.g. :connecting
    ;; means TCP never opened, :open means TCP opened but no 209).
    (when (braid-http-sub-on-disconnect sub)
      (funcall (braid-http-sub-on-disconnect sub)))
    (setf (braid-http-sub-status sub) :disconnected)
    (braid-http--schedule-reconnect sub))))

(defun braid-http--schedule-reconnect (sub)
  "Schedule a reconnect for SUB with exponential backoff."
  (when (braid-http-sub-reconnect-timer sub)
    (cancel-timer (braid-http-sub-reconnect-timer sub)))
  (let* ((max-delay (braid-http-sub-reconnect-max-delay sub))
         (delay     (min max-delay (braid-http-sub-reconnect-delay sub))))
    (setf (braid-http-sub-reconnect-delay sub) (min max-delay (* delay 2.0)))
    (setf (braid-http-sub-reconnect-timer sub)
          (run-with-timer
           delay nil
           (lambda ()
             (when (not (eq (braid-http-sub-status sub) :closed))
               (braid-http--open sub (if (braid-http-sub-parents-fn sub)
                                         (funcall (braid-http-sub-parents-fn sub))
                                       (braid-http-sub-last-parents sub)))))))))

(defun braid-http-expedite-reconnect (sub)
  "If SUB is disconnected, cancel its pending timer and reconnect immediately.
Resets the backoff delay to 1.0.  No-op if SUB is not disconnected."
  (when (eq (braid-http-sub-status sub) :disconnected)
    (when (braid-http-sub-reconnect-timer sub)
      (cancel-timer (braid-http-sub-reconnect-timer sub))
      (setf (braid-http-sub-reconnect-timer sub) nil))
    (setf (braid-http-sub-reconnect-delay sub) 1.0)
    (braid-http--open sub (if (braid-http-sub-parents-fn sub)
                             (funcall (braid-http-sub-parents-fn sub))
                           (braid-http-sub-last-parents sub)))))


;;;; ======================================================================
;;;; Public API
;;;; ======================================================================

(cl-defun braid-http-subscribe (host port path on-message
                          &key on-connect on-disconnect peer extra-headers tls
                          heartbeat-interval parents-fn)
  "Open a Braid-HTTP GET subscription to HOST:PORT/PATH.

ON-MESSAGE is called for each sub-response in the 209 stream with a plist:
  :version        list of strings
  :parents        list of strings
  :content-range  (unit start end) or nil
  :content-length integer
  :body           string

ON-CONNECT is called (no args) when a 209 is successfully received.
ON-DISCONNECT is called (no args) on any unexpected disconnect.
  The subscription will automatically reconnect.

PEER is a peer identifier string (random if omitted).
EXTRA-HEADERS is an alist of additional GET headers sent on every connect.
HEARTBEAT-INTERVAL, when non-nil, requests heartbeats from the server
  every N seconds and kills the connection if no data arrives within
  1.2*N+3 seconds.
PARENTS-FN, when non-nil, is a function called on each reconnect to get
  the parents list.  Overrides the default `last-parents' tracking.

Returns a braid-http-sub struct.  Pass to braid-http-unsubscribe to close."
  (let ((sub (make-braid-http-sub
              :host               host
              :port               port
              :path               path
              :peer               (or peer (format "%04x%04x"
                                                    (random #xffff)
                                                    (random #xffff)))
              :tls                tls
              :on-message         on-message
              :on-connect         on-connect
              :on-disconnect      on-disconnect
              :extra-headers      extra-headers
              :heartbeat-interval heartbeat-interval
              :parents-fn         parents-fn)))
    (braid-http--open sub nil)
    sub))

(defun braid-http-unsubscribe (sub)
  "Close subscription SUB and cancel any pending reconnect."
  (setf (braid-http-sub-status sub) :closed)
  (when (braid-http-sub-reconnect-timer sub)
    (cancel-timer (braid-http-sub-reconnect-timer sub))
    (setf (braid-http-sub-reconnect-timer sub) nil))
  (when (braid-http-sub-heartbeat-timer sub)
    (cancel-timer (braid-http-sub-heartbeat-timer sub))
    (setf (braid-http-sub-heartbeat-timer sub) nil))
  (when-let ((proc (braid-http-sub-process sub)))
    (when (process-live-p proc)
      (delete-process proc))))

(cl-defun braid-http-put (host port path version parents patch
                    &key peer content-type merge-type tls on-done on-error)
  "Send a Braid-HTTP PUT to HOST:PORT/PATH.

VERSION and PARENTS are lists of version strings.
PATCH is a plist with:
  :range    a plist with :unit  (string, only \"text\" supported currently),
                         :start (integer, code-point index inclusive),
                         :end   (integer, code-point index exclusive)
  :content  string (replacement text; empty string for deletion)

ON-DONE is called with the HTTP response status code (integer).
ON-ERROR is called with an error message string.

Returns the (short-lived) network process."
  (let* ((content       (or (plist-get patch :content) ""))
         (content-bytes (encode-coding-string content 'utf-8))
         (content-len   (length content-bytes))
         (range         (plist-get patch :range))
         (start         (or (plist-get range :start) 0))
         (end           (or (plist-get range :end)   0))
         (unit          (or (plist-get range :unit)  "text"))
         (body-headers  (append
                         `(("Content-Type"   . ,(or content-type "text/plain"))
                           ("Merge-Type"     . ,(or merge-type "simpleton"))
                           ("Content-Length" . ,(number-to-string content-len))
                           ("Content-Range"  . ,(format "%s [%d:%d]" unit start end)))
                         (when peer `(("Peer" . ,peer)))))
         (request       (braid-http--format-put host port path version parents
                                                body-headers content-bytes))
         (resp-buf "")
         (done nil)
         (proc
          (braid-http--make-process
           (format "braid-http-put:%s:%d%s" host port path)
           host port tls
           (lambda (_proc data)
             (unless done
               (setq resp-buf (concat resp-buf data))
               (when (string-match "HTTP/[^ ]+ \\([0-9]+\\)" resp-buf)
                 (setq done t)
                 (when on-done
                   (funcall on-done
                            (string-to-number (match-string 1 resp-buf)))))))
           (lambda (proc event)
             (cond
              ((string-prefix-p "open" event)
               ;; Connection (and TLS handshake) established — send now.
               (process-send-string proc request))
              ((and (not done)
                    (not (string-match-p "finished" event))
                    (not (string-match-p "deleted" event)))
               (message "braid PUT error: %s" (string-trim event))
               (when on-error (funcall on-error (string-trim event))))
              ((not (process-live-p proc))
               (setq resp-buf ""))))
           'nowait)))
    proc))


(provide 'braid-http)
;;; braid-http.el ends here
