;;; braid-stress.el --- Stress test for concurrent simpleton edits -*- lexical-binding: t -*-
;;
;; Reproduces the scenario where two clients edit simultaneously, one faster
;; than 1 round-trip, forcing the server to rebase.  Uses braid-text (the
;; real simpleton client) against the /text/* routes of test-server.js.
;;
;; Usage:
;;   emacs --batch -l braid-text.el -l braid-stress.el \
;;         --eval "(braid-stress-test)"

;;; Code:

(require 'cl-lib)
(require 'braid-text)

(defconst braid-stress--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing this file and test/.")

(defvar braid-stress--errors nil
  "List of error messages captured during the test.")

(defun braid-stress--start-server ()
  "Start the local braid-text test server."
  (let* ((script (expand-file-name "test/test-server.js" braid-stress--dir))
         (proc (start-process "braid-test-server" nil "node" script)))
    (set-process-query-on-exit-flag proc nil)
    proc))

(defun braid-stress--add-latency (bt delay)
  "Add DELAY seconds of latency to BT's subscription receive path."
  (let* ((sub  (braid-text-sub bt))
         (proc (braid-http-sub-process sub))
         (orig (process-filter proc)))
    (set-process-filter
     proc
     (lambda (_proc data)
       (run-with-timer delay nil
                       (lambda ()
                         (when (process-live-p proc)
                           (funcall orig proc data))))))))

(defun braid-stress--edit (bt buf text pos)
  "Insert TEXT at POS in BUF and flush BT via fast path."
  (with-current-buffer buf
    (let ((inhibit-modification-hooks t))
      (goto-char (+ (point-min) pos))
      (let ((beg (point)))
        (insert text)
        (braid-text-buffer-changed bt beg (point) 0)))))

(defun braid-stress--wait (seconds)
  "Yield for SECONDS, processing timers and network I/O."
  (let ((deadline (+ (float-time) seconds)))
    (while (< (float-time) deadline)
      (accept-process-output nil 0.05))))

(defun braid-stress--buf-text (buf)
  "Return BUF's content as a string."
  (with-current-buffer buf
    (buffer-substring-no-properties (point-min) (point-max))))

(defun braid-stress--get-server-text (path)
  "Fetch the current text at PATH from the test server via plain GET.
Returns the body as a string, or nil on failure."
  (let* ((response-buf (generate-new-buffer " *stress-get*"))
         (proc (make-network-process
                :name "stress-get"
                :buffer response-buf
                :host "127.0.0.1"
                :service 8888
                :coding 'binary))
         (request (format "GET %s HTTP/1.1\r\nHost: 127.0.0.1:8888\r\nAccept: text/plain\r\nConnection: close\r\n\r\n" path)))
    (set-process-query-on-exit-flag proc nil)
    (process-send-string proc request)
    ;; Wait for response
    (let ((deadline (+ (float-time) 3.0)))
      (while (and (process-live-p proc) (< (float-time) deadline))
        (accept-process-output proc 0.1)))
    (prog1
        (with-current-buffer response-buf
          (goto-char (point-min))
          (when (search-forward "\r\n\r\n" nil t)
            ;; Parse Content-Length to avoid trailing HTTP data
            (let ((body-start (point))
                  (cl nil))
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "Content-Length: \\([0-9]+\\)" body-start t)
                  (setq cl (string-to-number (match-string 1)))))
              (if cl
                  (buffer-substring-no-properties body-start (min (+ body-start cl) (point-max)))
                (buffer-substring-no-properties body-start (point-max))))))
      (kill-buffer response-buf))))

(defun braid-stress-test ()
  "Stress test: two simpleton clients, rapid concurrent edits, forced rebase."
  (let* ((server (braid-stress--start-server))
         (path   (format "/text/stress-%04x" (random #xffff)))
         (buf-a  (get-buffer-create "*stress-A*"))
         (buf-b  (get-buffer-create "*stress-B*"))
         (latency 0.15)  ; 150ms one-way → 300ms RTT
         (errors '())
         (braid-text-debug t)
         bt-a bt-b)

    ;; Capture Braid error/mismatch messages
    (let ((orig-message (symbol-function 'message)))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (let ((msg (apply #'format fmt args)))
                     (when (string-match-p "\\`Braid:" msg)
                       (push msg errors))
                     (apply orig-message fmt args)))))

        (dolist (b (list buf-a buf-b))
          (with-current-buffer b (erase-buffer)))

        (braid-stress--wait 0.5)    ; let server start

        ;; Connect both clients to the same resource
        (setq bt-a (braid-text-open "127.0.0.1" 8888 path buf-a))
        (setq bt-b (braid-text-open "127.0.0.1" 8888 path buf-b))
        (braid-stress--wait 0.5)    ; initial snapshot

        ;; Add latency to subscription receive path
        (braid-stress--add-latency bt-a latency)
        (braid-stress--add-latency bt-b latency)

        ;; ── Rounds 1-3: build up document content ──────────────────────
        (message "--- Round 1: A burst (5), B single ---")
        (dotimes (i 5)
          (braid-stress--edit bt-a buf-a (format "a%d " i) 0))
        (braid-stress--edit bt-b buf-b "BBB " 0)
        (braid-stress--wait 3.0)

        (message "--- Round 2: both burst (5 each) ---")
        (dotimes (i 5)
          (braid-stress--edit bt-a buf-a (format "X%d " i) 0)
          (braid-stress--edit bt-b buf-b (format "Y%d " i) 0))
        (braid-stress--wait 3.0)

        (message "--- Round 3: A burst (10), B burst (10) ---")
        (dotimes (i 10)
          (braid-stress--edit bt-a buf-a (format "P%d " i) 0))
        (braid-stress--wait 0.05)
        (dotimes (i 10)
          (braid-stress--edit bt-b buf-b (format "Q%d " i) 0))
        (braid-stress--wait 5.0)

        (message "  synced: %s" (equal (braid-stress--buf-text buf-a)
                                        (braid-stress--buf-text buf-b)))

        ;; ── Round 4: random positions + deletions (triggers multi-patch) ─
        (message "--- Round 4: A random (20), B random (20) ---")
        (dotimes (i 20)
          (let ((len-a (with-current-buffer buf-a (- (point-max) (point-min)))))
            (if (and (> len-a 3) (zerop (random 3)))
                (let* ((s (random (max 1 (- len-a 2))))
                       (e (min len-a (+ s 1 (random 2)))))
                  (with-current-buffer buf-a
                    (let ((inhibit-modification-hooks t)
                          (beg (+ (point-min) s))
                          (end (+ (point-min) e)))
                      (delete-region beg end)
                      (braid-text-buffer-changed bt-a beg beg (- e s)))))
              (braid-stress--edit bt-a buf-a (format "r%d " i)
                                  (random (1+ len-a))))))
        (dotimes (i 20)
          (let ((len-b (with-current-buffer buf-b (- (point-max) (point-min)))))
            (braid-stress--edit bt-b buf-b (format "s%d " i)
                                (random (1+ len-b)))))

        (message "--- settling ---")
        (braid-stress--wait 5.0)

        (message "  A: %S" (braid-stress--buf-text buf-a))
        (message "  B: %S" (braid-stress--buf-text buf-b))

        ;; Fetch the server's ground truth via plain GET
        (let ((server-text (braid-stress--get-server-text path)))
          (message "  S: %S" server-text)

          ;; ── Results ────────────────────────────────────────────────────
          (let* ((text-a (braid-stress--buf-text buf-a))
                 (text-b (braid-stress--buf-text buf-b))
                 (converged (equal text-a text-b)))
            (message "")
            (if converged
                (message "PASS: buffers converged")
              (message "FAIL: buffers diverged"))
            (when server-text
              (message "  A == server: %s" (equal text-a server-text))
              (message "  B == server: %s" (equal text-b server-text)))
            (if errors
                (progn
                  (message "Braid errors captured (%d):" (length errors))
                  (dolist (e (nreverse errors))
                    (message "  %s" e)))
              (message "No Braid errors."))))

        ;; Cleanup
        (braid-text-close bt-a)
        (braid-text-close bt-b)
        (when (process-live-p server)
          (delete-process server))))))

(provide 'braid-stress)
;;; braid-stress.el ends here
