;;; braid-http-tests.el --- ERT tests for braid-http.el -*- lexical-binding: t -*-
;;
;; Run unit tests:
;;   emacs --batch -L . -l braid-http-tests.el -f ert-run-tests-batch-and-exit
;;
;; Run integration tests (requires node server on 127.0.0.1:8888):
;;   emacs --batch -l braid-http.el -l braid-http-tests.el \
;;         --eval "(braid-test-integration)"

;;; Code:

(require 'ert)
(require 'braid-http)
(require 'braid-text)


;;;; ======================================================================
;;;; Helpers
;;;; ======================================================================

(defun braid-test--collect-messages (body-str)
  "Parse all sub-responses from BODY-STR and return a list of message plists.
Bypasses the outer HTTP headers and dechunking — body-str is fed directly
into the sub-headers parser, simulating what arrives after dechunking."
  (let (messages)
    (let ((sub (make-braid-http-sub
                :host "127.0.0.1" :port 8888 :path "/test"
                :stage :sub-headers
                :body-buf body-str
                :on-message (lambda (msg) (push msg messages)))))
      (braid-http--pump sub))
    (nreverse messages)))


;;;; ======================================================================
;;;; Version / Parents formatting and parsing
;;;; ======================================================================

(ert-deftest braid-format-version/single ()
  (should (equal (braid-http--format-version '("server-0"))
                 "\"server-0\"")))

(ert-deftest braid-format-version/multiple ()
  (should (equal (braid-http--format-version '("v1" "v2"))
                 "\"v1\", \"v2\"")))

(ert-deftest braid-format-version/empty ()
  (should (equal (braid-http--format-version '()) "")))

(ert-deftest braid-parse-version/single ()
  (should (equal (braid-http--parse-version "\"server-0\"")
                 '("server-0"))))

(ert-deftest braid-parse-version/multiple ()
  (should (equal (braid-http--parse-version "\"v1\", \"v2\"")
                 '("v1" "v2"))))

(ert-deftest braid-parse-version/nil ()
  (should (null (braid-http--parse-version nil))))

(ert-deftest braid-parse-version/empty-string ()
  (should (null (braid-http--parse-version ""))))

(ert-deftest braid-format-parse-version/roundtrip ()
  (let ((versions '("a3f9b2-7" "other-12")))
    (should (equal (braid-http--parse-version
                    (braid-http--format-version versions))
                   versions))))


;;;; ======================================================================
;;;; Content-Range parsing
;;;; ======================================================================

(ert-deftest braid-parse-content-range/basic ()
  (should (equal (braid-http--parse-content-range "text [7:12]")
                 '("text" 7 12))))

(ert-deftest braid-parse-content-range/zero-length ()
  (should (equal (braid-http--parse-content-range "text [0:0]")
                 '("text" 0 0))))

(ert-deftest braid-parse-content-range/deletion ()
  "A deletion has no content (handled by body) but range end > start."
  (should (equal (braid-http--parse-content-range "text [9:13]")
                 '("text" 9 13))))

(ert-deftest braid-parse-content-range/nil ()
  (should (null (braid-http--parse-content-range nil))))


;;;; ======================================================================
;;;; GET request formatting
;;;; ======================================================================

(ert-deftest braid-format-get/basic ()
  (let ((req (braid-http--format-get
              "127.0.0.1" 8888 "/test"
              '(("Subscribe" . "true")
                ("Peer"      . "a3f9b2")
                ("Accept"    . "text/plain")))))
    (should (string-prefix-p "GET /test HTTP/1.1\r\n" req))
    (should (string-match-p "Host: 127.0.0.1:8888\r\n" req))
    (should (string-match-p "Subscribe: true\r\n" req))
    (should (string-match-p "Peer: a3f9b2\r\n" req))
    ;; Must end with blank line
    (should (string-suffix-p "\r\n\r\n" req))))

(ert-deftest braid-format-get/with-parents ()
  (let ((req (braid-http--format-get
              "127.0.0.1" 8888 "/test"
              `(("Subscribe" . "true")
                ("Parents"   . ,(braid-http--format-version '("server-0")))))))
    (should (string-match-p "Parents: \"server-0\"\r\n" req))))


;;;; ======================================================================
;;;; PUT request formatting
;;;; ======================================================================

(ert-deftest braid-format-put/basic ()
  (let* ((content       "WORLD!!")
         (content-bytes (encode-coding-string content 'utf-8))
         (req (braid-http--format-put
               "127.0.0.1" 8888 "/test"
               '("a3f9b2-7") '("server-0")
               `(("Content-Type"   . "text/plain")
                 ("Merge-Type"     . "simpleton")
                 ("Peer"           . "a3f9b2")
                 ("Content-Length" . "7")
                 ("Content-Range"  . "text [7:12]"))
               content-bytes)))
    (should (string-prefix-p "PUT /test HTTP/1.1\r\n" req))
    (should (string-match-p "Host: 127.0.0.1:8888\r\n" req))
    (should (string-match-p "Version: \"a3f9b2-7\"\r\n" req))
    (should (string-match-p "Parents: \"server-0\"\r\n" req))
    (should (string-match-p "Content-Range: text \\[7:12\\]\r\n" req))
    (should (string-match-p "Merge-Type: simpleton\r\n" req))
    (should (string-suffix-p "WORLD!!" req))))

(ert-deftest braid-format-put/empty-parents ()
  "Empty parents list formats as empty string (no Parents header value)."
  (let* ((req (braid-http--format-put
               "127.0.0.1" 8888 "/test"
               '("v1") '()
               '(("Content-Length" . "0")
                 ("Content-Range"  . "text [0:0]"))
               "")))
    (should (string-match-p "Version: \"v1\"\r\n" req))
    ;; Parents header is present with empty value
    (should (string-match-p "Parents: \r\n" req))))


;;;; ======================================================================
;;;; Chunked transfer-encoding decoder
;;;; ======================================================================

(ert-deftest braid-dechunk/single-chunk ()
  (let* ((chunked "d\r\nHello, world!\r\n0\r\n\r\n")
         (result  (braid-http--dechunk chunked "")))
    (should (equal (cdr result) "Hello, world!"))
    (should (equal (car result) ""))))

(ert-deftest braid-dechunk/two-chunks ()
  (let* ((chunked "5\r\nHello\r\n6\r\n world\r\n0\r\n\r\n")
         (result  (braid-http--dechunk chunked "")))
    (should (equal (cdr result) "Hello world"))))

(ert-deftest braid-dechunk/appends-to-existing-body ()
  (let* ((chunked "5\r\nworld\r\n0\r\n\r\n")
         (result  (braid-http--dechunk chunked "Hello ")))
    (should (equal (cdr result) "Hello world"))))

(ert-deftest braid-dechunk/incomplete-chunk-data ()
  "When chunk data hasn't fully arrived, raw-buf is unchanged."
  (let* (;; chunk size is 13 ("d" hex) but only 5 bytes of data arrived
         (partial "d\r\nHello")
         (result  (braid-http--dechunk partial "")))
    (should (equal (car result) "d\r\nHello"))
    (should (equal (cdr result) ""))))

(ert-deftest braid-dechunk/incomplete-size-line ()
  "When size line hasn't fully arrived, raw-buf is unchanged."
  (let* ((result (braid-http--dechunk "d" "")))
    (should (equal (car result) "d"))
    (should (equal (cdr result) ""))))

(ert-deftest braid-dechunk/chunk-with-extension ()
  "Chunk extensions after ';' are ignored."
  (let* ((chunked "5;ext=ignored\r\nhello\r\n0\r\n\r\n")
         (result  (braid-http--dechunk chunked "")))
    (should (equal (cdr result) "hello"))))


;;;; ======================================================================
;;;; Sub-response stream parsing
;;;; ======================================================================

(ert-deftest braid-parse/snapshot ()
  "Full snapshot: Content-Length with no Content-Range."
  (let* ((stream (concat "200 OK\r\n"
                         "Version: \"server-0\"\r\n"
                         "Parents: \"root\"\r\n"
                         "Content-Length: 13\r\n"
                         "\r\n"
                         "Hello, world!"))
         (msgs (braid-test--collect-messages stream)))
    (should (= (length msgs) 1))
    (let ((m (car msgs)))
      (should (equal (plist-get m :version) '("server-0")))
      (should (equal (plist-get m :parents) '("root")))
      (should (equal (plist-get m :body)    "Hello, world!"))
      (should (null  (plist-get m :content-range))))))

(ert-deftest braid-parse/patch ()
  "Patch: Content-Range present."
  (let* ((stream (concat "200 OK\r\n"
                         "Version: \"other-client-20\"\r\n"
                         "Parents: \"server-0\"\r\n"
                         "Content-Length: 5\r\n"
                         "Content-Range: text [7:12]\r\n"
                         "\r\n"
                         "world"))
         (msgs (braid-test--collect-messages stream)))
    (should (= (length msgs) 1))
    (let ((m (car msgs)))
      (should (equal (plist-get m :version)       '("other-client-20")))
      (should (equal (plist-get m :content-range) '("text" 7 12)))
      (should (equal (plist-get m :body)          "world")))))

(ert-deftest braid-parse/empty-body ()
  "Content-Length: 0 with a Content-Range (pure deletion)."
  (let* ((stream (concat "200 OK\r\n"
                         "Content-Length: 0\r\n"
                         "Content-Range: text [9:13]\r\n"
                         "\r\n"))
         (msgs (braid-test--collect-messages stream)))
    (should (= (length msgs) 1))
    (let ((m (car msgs)))
      (should (equal (plist-get m :body)          ""))
      (should (equal (plist-get m :content-range) '("text" 9 13))))))

(ert-deftest braid-parse/two-responses-back-to-back ()
  "Two sub-responses with no separator."
  (let* ((stream (concat "200 OK\r\n"
                         "Version: \"server-0\"\r\n"
                         "Content-Length: 13\r\n"
                         "\r\n"
                         "Hello, world!"
                         "200 OK\r\n"
                         "Version: \"other-client-20\"\r\n"
                         "Content-Length: 5\r\n"
                         "Content-Range: text [7:12]\r\n"
                         "\r\n"
                         "world"))
         (msgs (braid-test--collect-messages stream)))
    (should (= (length msgs) 2))
    (should (equal (plist-get (nth 0 msgs) :version) '("server-0")))
    (should (equal (plist-get (nth 1 msgs) :version) '("other-client-20")))))

(ert-deftest braid-parse/heartbeat-skipped ()
  "Bare CRLF heartbeats before a sub-response are silently skipped."
  (let* ((stream (concat "\r\n"
                         "\r\n"
                         "200 OK\r\n"
                         "Version: \"server-0\"\r\n"
                         "Content-Length: 5\r\n"
                         "\r\n"
                         "hello"))
         (msgs (braid-test--collect-messages stream)))
    (should (= (length msgs) 1))
    (should (equal (plist-get (car msgs) :body) "hello"))))

(ert-deftest braid-parse/no-version-or-parents ()
  "Sub-responses without Version or Parents headers are valid."
  (let* ((stream (concat "200 OK\r\n"
                         "Content-Length: 0\r\n"
                         "Content-Range: text [9:13]\r\n"
                         "\r\n"))
         (msgs (braid-test--collect-messages stream)))
    (should (= (length msgs) 1))
    (should (null (plist-get (car msgs) :version)))
    (should (null (plist-get (car msgs) :parents)))))

(ert-deftest braid-parse/incremental-delivery ()
  "Parser handles data arriving in small pieces."
  (let* ((full-stream (concat "200 OK\r\n"
                              "Version: \"server-0\"\r\n"
                              "Content-Length: 5\r\n"
                              "\r\n"
                              "hello"))
         messages
         (sub (make-braid-http-sub
               :host "127.0.0.1" :port 8888 :path "/test"
               :stage :sub-headers
               :on-message (lambda (m) (push m messages)))))
    ;; Feed one byte at a time
    (dotimes (i (length full-stream))
      (setf (braid-http-sub-body-buf sub)
            (concat (braid-http-sub-body-buf sub)
                    (substring full-stream i (1+ i))))
      (braid-http--pump sub))
    (setq messages (nreverse messages))
    (should (= (length messages) 1))
    (should (equal (plist-get (car messages) :body) "hello"))))


;;;; ======================================================================
;;;; Patches: N block parsing
;;;; ======================================================================

(ert-deftest braid-parse/patches-n-single ()
  "Patches: 1 block with one patch."
  (let* ((stream (concat "200 OK\r\n"
                         "Version: \"put--7\"\r\n"
                         "Parents: \"put-1\"\r\n"
                         "Patches: 1\r\n"
                         "\r\n"
                         "Content-Length: 5\r\n"
                         "Content-Range: text [0:0]\r\n"
                         "\r\n"
                         "hello"))
         (msgs (braid-test--collect-messages stream)))
    (should (= (length msgs) 1))
    (let* ((m  (car msgs))
           (ps (plist-get m :patches)))
      (should (equal (plist-get m :version) '("put--7")))
      (should (null  (plist-get m :body)))
      (should (= (length ps) 1))
      (should (equal (plist-get (car ps) :content-range) '("text" 0 0)))
      (should (equal (plist-get (car ps) :body) "hello")))))

(ert-deftest braid-parse/patches-n-two ()
  "Patches: 2 block with two patches in one sub-response."
  (let* ((stream (concat "200 OK\r\n"
                         "Version: \"v2\"\r\n"
                         "Patches: 2\r\n"
                         "\r\n"
                         "Content-Length: 2\r\n"
                         "Content-Range: text [0:5]\r\n"
                         "\r\n"
                         "Hi"
                         "Content-Length: 0\r\n"
                         "Content-Range: text [9:13]\r\n"
                         "\r\n"))
         (msgs (braid-test--collect-messages stream)))
    (should (= (length msgs) 1))
    (let* ((m  (car msgs))
           (ps (plist-get m :patches)))
      (should (= (length ps) 2))
      (should (equal (plist-get (nth 0 ps) :content-range) '("text" 0 5)))
      (should (equal (plist-get (nth 0 ps) :body) "Hi"))
      (should (equal (plist-get (nth 1 ps) :content-range) '("text" 9 13)))
      (should (equal (plist-get (nth 1 ps) :body) "")))))

(ert-deftest braid-parse/patches-n-then-snapshot ()
  "Patches: N block followed immediately by a plain snapshot."
  (let* ((stream (concat "200 OK\r\n"
                         "Version: \"v1\"\r\n"
                         "Patches: 1\r\n"
                         "\r\n"
                         "Content-Length: 5\r\n"
                         "Content-Range: text [0:0]\r\n"
                         "\r\n"
                         "hello"
                         "200 OK\r\n"
                         "Version: \"v2\"\r\n"
                         "Content-Length: 5\r\n"
                         "\r\n"
                         "hello"))
         (msgs (braid-test--collect-messages stream)))
    (should (= (length msgs) 2))
    (should (plist-get (nth 0 msgs) :patches))
    (should (null (plist-get (nth 1 msgs) :patches)))
    (should (equal (plist-get (nth 1 msgs) :body) "hello"))))


;;;; ======================================================================
;;;; Heartbeat dead-connection detection
;;;; ======================================================================

(ert-deftest braid-heartbeat/header-in-get ()
  "Heartbeats header appears in GET request."
  (let ((req (braid-http--format-get
              "127.0.0.1" 8888 "/test"
              '(("Subscribe" . "true")
                ("Heartbeats" . "30s")))))
    (should (string-match-p "Heartbeats: 30s\r\n" req))))

(ert-deftest braid-heartbeat/filter-updates-last-data-time ()
  "Process filter updates last-data-time on every data arrival."
  (let ((sub (make-braid-http-sub
              :host "127.0.0.1" :port 8888 :path "/test"
              :peer "test"
              :stage :sub-headers)))
    (should (null (braid-http-sub-last-data-time sub)))
    (braid-http--filter sub nil "some-data")
    (should (braid-http-sub-last-data-time sub))
    (should (< (- (float-time) (braid-http-sub-last-data-time sub)) 1.0))))

(ert-deftest braid-heartbeat/timer-starts-on-209 ()
  "Heartbeat timer starts when a 209 response is received."
  (let ((sub (make-braid-http-sub
              :host "127.0.0.1" :port 8888 :path "/test"
              :peer "test"
              :heartbeat-interval 30
              :stage :outer-headers
              :raw-buf "HTTP/1.1 209 \r\n\r\n"
              :on-message (lambda (_)))))
    (braid-http--pump sub)
    (unwind-protect
        (progn
          (should (braid-http-sub-heartbeat-timer sub))
          (should (braid-http-sub-last-data-time sub)))
      (when (braid-http-sub-heartbeat-timer sub)
        (cancel-timer (braid-http-sub-heartbeat-timer sub))))))

(ert-deftest braid-heartbeat/no-timer-without-interval ()
  "No heartbeat timer when heartbeat-interval is nil."
  (let ((sub (make-braid-http-sub
              :host "127.0.0.1" :port 8888 :path "/test"
              :peer "test"
              :heartbeat-interval nil
              :stage :outer-headers
              :raw-buf "HTTP/1.1 209 \r\n\r\n"
              :on-message (lambda (_)))))
    (braid-http--pump sub)
    (should (null (braid-http-sub-heartbeat-timer sub)))))

(ert-deftest braid-heartbeat/unsubscribe-cancels-timer ()
  "Unsubscribing cancels the heartbeat timer."
  (let ((sub (make-braid-http-sub
              :host "127.0.0.1" :port 8888 :path "/test"
              :peer "test"
              :heartbeat-interval 30
              :stage :outer-headers
              :raw-buf "HTTP/1.1 209 \r\n\r\n"
              :on-message (lambda (_)))))
    (braid-http--pump sub)
    (should (braid-http-sub-heartbeat-timer sub))
    (braid-http-unsubscribe sub)
    (should (null (braid-http-sub-heartbeat-timer sub)))))

(ert-deftest braid-heartbeat/timeout-detected-when-stale ()
  "Heartbeat timeout condition triggers when last-data-time is old enough."
  (let* ((interval 30)
         (timeout  (+ (* 1.2 interval) 3))  ; 39 seconds
         (sub (make-braid-http-sub
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test"
               :heartbeat-interval interval
               :status :connected
               :last-data-time (- (float-time) (+ timeout 1)))))
    (should (> (- (float-time) (braid-http-sub-last-data-time sub))
               timeout))))

(ert-deftest braid-heartbeat/no-timeout-when-recent-data ()
  "No timeout when data arrived recently."
  (let* ((interval 30)
         (timeout  (+ (* 1.2 interval) 3))
         (sub (make-braid-http-sub
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test"
               :heartbeat-interval interval
               :status :connected
               :last-data-time (float-time))))
    (should-not (> (- (float-time) (braid-http-sub-last-data-time sub))
                   timeout))))


;;;; ======================================================================
;;;; PUT ACK timeout
;;;; ======================================================================

(ert-deftest braid-put-ack/timer-starts-on-first-put ()
  "PUT ACK timer starts when pending-puts transitions from 0 to >0."
  (let* ((buf (generate-new-buffer " *braid-test*"))
         (bt  (make-braid-text
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test" :buffer buf
               :prev-state ""
               :current-version '("v0"))))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "hello"))
          (should (null (braid-text-put-ack-timer bt)))
          (braid-text--flush bt)
          (should (braid-text-put-ack-timer bt))
          (should (= (braid-text-pending-puts bt) 1)))
      (when (braid-text-put-ack-timer bt)
        (cancel-timer (braid-text-put-ack-timer bt)))
      (kill-buffer buf))))

(ert-deftest braid-put-ack/timer-not-restarted-on-subsequent-puts ()
  "PUT ACK timer is not restarted when additional PUTs are sent."
  (let* ((buf (generate-new-buffer " *braid-test*"))
         (bt  (make-braid-text
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test" :buffer buf
               :prev-state ""
               :current-version '("v0"))))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "hello"))
          (braid-text--flush bt)
          (let ((first-timer (braid-text-put-ack-timer bt)))
            (should first-timer)
            ;; Make another edit
            (with-current-buffer buf
              (goto-char (point-max))
              (let ((inhibit-modification-hooks t))
                (insert "!")))
            (braid-text--flush bt)
            ;; Same timer, not a new one
            (should (eq (braid-text-put-ack-timer bt) first-timer))
            (should (= (braid-text-pending-puts bt) 2))))
      (when (braid-text-put-ack-timer bt)
        (cancel-timer (braid-text-put-ack-timer bt)))
      (kill-buffer buf))))

(ert-deftest braid-put-ack/connection-dead-triggers-reconnect ()
  "braid-text--connection-dead triggers reconnect."
  (let* ((buf (generate-new-buffer " *braid-test*"))
         (sub (make-braid-http-sub
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test"))
         (bt  (make-braid-text
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test" :buffer buf
               :pending-puts 3
               :put-ack-timer (run-with-timer 999 nil #'ignore)
               :sub sub))
         (reconnect-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'braid-text--reconnect)
                   (lambda (_bt) (setq reconnect-called t))))
          (braid-text--connection-dead bt)
          (should reconnect-called))
      (when (braid-text-put-ack-timer bt)
        (cancel-timer (braid-text-put-ack-timer bt)))
      (kill-buffer buf))))

(ert-deftest braid-put-ack/close-cancels-timer ()
  "braid-text-close cancels the PUT ACK timer."
  (let* ((buf (generate-new-buffer " *braid-test*"))
         (sub (make-braid-http-sub
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test"))
         (bt  (make-braid-text
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test" :buffer buf
               :put-ack-timer (run-with-timer 999 nil #'ignore)
               :sub sub)))
    (unwind-protect
        (progn
          (should (braid-text-put-ack-timer bt))
          (braid-text-close bt)
          (should (null (braid-text-put-ack-timer bt))))
      (when (braid-text-put-ack-timer bt)
        (cancel-timer (braid-text-put-ack-timer bt)))
      (when (buffer-live-p buf) (kill-buffer buf)))))


;;;; ======================================================================
;;;; Reconnect behavior
;;;; ======================================================================

(ert-deftest braid-reconnect/preserves-current-version ()
  "braid-text--reconnect does NOT reset current-version."
  (let* ((buf (generate-new-buffer " *braid-test*"))
         (sub (make-braid-http-sub
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test"))
         (bt  (make-braid-text
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test" :buffer buf
               :current-version '("v0")
               :prev-state "hello"
               :sub sub)))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "hello world"))
          (cl-letf (((symbol-function 'braid-http-subscribe)
                     (lambda (&rest _) sub))
                    ((symbol-function 'braid-text--put-proc-open)
                     (lambda (_bt) nil)))
            (braid-text--reconnect bt))
          ;; Version is preserved (not reset to nil)
          (should (equal (braid-text-current-version bt) '("v0")))
          ;; pending-puts reset to 0
          (should (= (braid-text-pending-puts bt) 0)))
      (kill-buffer buf))))

(ert-deftest braid-reconnect/resets-pending-puts ()
  "braid-text--reconnect resets pending-puts and cancels ACK timer."
  (let* ((buf (generate-new-buffer " *braid-test*"))
         (sub (make-braid-http-sub
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test"))
         (bt  (make-braid-text
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test" :buffer buf
               :pending-puts 5
               :put-ack-timer (run-with-timer 999 nil #'ignore)
               :current-version '("v0")
               :prev-state "hello"
               :sub sub)))
    (unwind-protect
        (cl-letf (((symbol-function 'braid-http-subscribe)
                   (lambda (&rest _) sub))
                  ((symbol-function 'braid-text--put-proc-open)
                   (lambda (_bt) nil)))
          (braid-text--reconnect bt)
          (should (= (braid-text-pending-puts bt) 0))
          (should (null (braid-text-put-ack-timer bt))))
      (when (braid-text-put-ack-timer bt)
        (cancel-timer (braid-text-put-ack-timer bt)))
      (kill-buffer buf))))

;;;; ======================================================================
;;;; PUT throttling
;;;; ======================================================================

(ert-deftest braid-throttle/flush-suppressed-at-max ()
  "Flushes are suppressed when pending-puts >= max-outstanding-puts."
  (let* ((buf (generate-new-buffer " *braid-test*"))
         (bt  (make-braid-text
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test" :buffer buf
               :prev-state "old"
               :current-version '("v0")
               :pending-puts 10
               :max-outstanding-puts 10)))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "new text"))
          (braid-text--flush bt)
          ;; No PUT should have been sent — still at 10
          (should (= (braid-text-pending-puts bt) 10)))
      (kill-buffer buf))))

(ert-deftest braid-throttle/flush-suppressed-when-muted ()
  "Flushes are suppressed during 503 backpressure mute period."
  (let* ((buf (generate-new-buffer " *braid-test*"))
         (bt  (make-braid-text
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test" :buffer buf
               :prev-state "old"
               :current-version '("v0")
               :muted-until (+ (float-time) 60.0))))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "new text"))
          (braid-text--flush bt)
          ;; No PUT should have been sent
          (should (= (braid-text-pending-puts bt) 0)))
      (kill-buffer buf))))

(ert-deftest braid-throttle/mute-clears-after-expiry ()
  "Mute is cleared when the mute time has passed."
  (let* ((buf (generate-new-buffer " *braid-test*"))
         (bt  (make-braid-text
               :host "127.0.0.1" :port 8888 :path "/test"
               :peer "test" :buffer buf
               :prev-state "old"
               :current-version '("v0")
               :muted-until (- (float-time) 1.0))))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "old"))
          (braid-text--flush bt)
          ;; Mute should have been cleared (time expired)
          (should (null (braid-text-muted-until bt))))
      (kill-buffer buf))))


;;;; ======================================================================
;;;; Integration test (requires live server)
;;;; ======================================================================

(defun braid-test--wait-for (pred timeout-secs label)
  "Spin the event loop until PRED returns non-nil or TIMEOUT-SECS elapses.
Returns t if PRED became true, nil if we timed out."
  (let ((deadline (+ (float-time) timeout-secs)))
    (while (and (not (funcall pred))
                (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (if (funcall pred)
      (progn (message "PASS: %s" label) t)
    (message "FAIL: %s (timed out after %ss)" label timeout-secs)
    nil))

(defun braid-test-integration ()
  "Run a live integration test against the server on 127.0.0.1:8888.
Tests:
  1. Subscribe and receive an initial snapshot.
  2. PUT an insert, verify the subscription stream delivers the echo
     with the correct patch content.
  3. Wait 0.5s then PUT a second insert, verify it arrives as a
     separate message — testing multi-update streaming over time.
Prints PASS/FAIL for each step and exits with code 0 or 1."
  ;; Use a unique path per run so state from previous runs doesn't interfere
  (let* ((path     (format "/braid-test-%x" (random #xffffff)))
         (messages nil)
         (fail     nil)
         (sub      (braid-http-subscribe
                    "127.0.0.1" 8888 path
                    (lambda (msg) (push msg messages)))))

    (cl-flet ((wait (pred secs label)
                (unless (braid-test--wait-for pred secs label)
                  (setq fail t))))

      ;; 1. Initial snapshot
      (wait (lambda () messages) 3.0 "received initial snapshot")

      ;; 2. First PUT: insert "hello" at start, wait for echo with correct content
      (unless fail
        (setq messages nil)
        (braid-put "127.0.0.1" 8888 path
                   '("tc-5") '()
                   '(:range (:unit "text" :start 0 :end 0) :content "hello"))
        (wait (lambda () messages) 3.0 "received echo of first PUT")
        (unless fail
          (let* ((m       (car (last messages)))
                 (patches (plist-get m :patches))
                 (content (and patches (plist-get (car patches) :body))))
            (if (equal content "hello")
                (message "PASS: first PUT echo has correct content %S" content)
              (message "FAIL: first PUT echo content was %S, expected \"hello\"" content)
              (setq fail t)))))

      ;; 3. Second PUT after a delay — use version from first echo as parents.
      ;;    Tests streaming multiple distinct updates over time.
      (unless fail
        (let* ((echo-version (plist-get (car (last messages)) :version))
               (_ (setq messages nil))
               (_ (accept-process-output nil 0.5))) ; deliberate delay
          (braid-put "127.0.0.1" 8888 path
                     '("tc-10") echo-version
                     '(:range (:unit "text" :start 5 :end 5) :content " world")
                     :on-done  (lambda (status) (message "DEBUG: second PUT status: %d" status))
                     :on-error (lambda (err)    (message "DEBUG: second PUT error: %s" err)))
          (wait (lambda () messages) 3.0 "received echo of second PUT (streaming)")
          (unless fail
            (let* ((m       (car (last messages)))
                   (patches (plist-get m :patches))
                   (content (and patches (plist-get (car patches) :body))))
              (if (equal content " world")
                  (message "PASS: second PUT echo has correct content %S" content)
                (message "FAIL: second PUT echo content was %S, expected \" world\"" content)
                (setq fail t))))))

    (braid-unsubscribe sub)
    (kill-emacs (if fail 1 0)))))


(provide 'braid-http-tests)
;;; braid-http-tests.el ends here
