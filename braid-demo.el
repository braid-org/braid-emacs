;;; braid-demo.el --- Two-buffer Braid-HTTP live sync demo -*- lexical-binding: t -*-
;;
;; Shows two Emacs buffers staying in sync through a Braid-HTTP echo server.
;; Each buffer takes turns making a random edit; the other receives and
;; applies the echo.  No versioning is needed because edits are serialised.
;;
;; The server is started automatically as a child process with output
;; suppressed, so it never corrupts the terminal display.
;;
;; Interactive demo:
;;
;;   emacs -Q -nw -l braid-http.el -l braid-demo.el \
;;         --eval "(braid-demo-run \"/demo\" 20)"
;;
;; Automated batch test:
;;
;;   emacs --batch -l braid-http.el -l braid-demo.el \
;;         --eval "(braid-demo-test)"

;;; Code:

(require 'cl-lib)
(require 'braid-http)

;; Capture the directory of this file at load time so we can find the server.
(defconst braid-demo--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing braid-demo.el and test-server/.")

(defvar braid-demo--server-proc nil
  "The test-server Node process started by the demo, if any.")

(defvar braid-demo--words
  '("hello" "world" "foo" "bar" "baz" "hi" "cat" "dog" "one" "two")
  "Word pool for random insertions.")

(cl-defstruct braid-demo-buf
  "State for one synced buffer."
  buffer   ; Emacs buffer object
  sub      ; braid-http-sub (also holds the peer ID)
  name)    ; display label ("A" or "B")


;;; ── Server management ────────────────────────────────────────────────────

(defun braid-demo--start-server ()
  "Start the local test server as a child process with output suppressed.
If the server is already running (port 8888 in use), the new process
will exit immediately and silently — the existing server handles traffic."
  (let ((script (expand-file-name "test-server/test-server.js" braid-demo--dir)))
    (when (file-exists-p script)
      (setq braid-demo--server-proc
            (start-process "braid-test-server" nil "node" script))
      (set-process-query-on-exit-flag braid-demo--server-proc nil))))


;;; ── Patch application ────────────────────────────────────────────────────

(defun braid-demo--apply-patch (dbuf content-range body)
  "Apply CONTENT-RANGE / BODY patch to DBUF's buffer."
  (let ((start (nth 1 content-range))
        (end   (nth 2 content-range)))
    (with-current-buffer (braid-demo-buf-buffer dbuf)
      (let ((inhibit-modification-hooks t))
        (delete-region (+ (point-min) start)
                       (+ (point-min) end))
        (goto-char (+ (point-min) start))
        (insert body)))))

(defun braid-demo--on-message (dbuf msg)
  "Subscription callback: apply the first patch in MSG to DBUF's buffer."
  (when-let* ((patches (plist-get msg :patches))
              (p       (car patches))
              (cr      (plist-get p :content-range))
              (body    (plist-get p :body)))
    (braid-demo--apply-patch dbuf cr body)))


;;; ── Sync assertion ───────────────────────────────────────────────────────

(defun braid-demo--check-sync (da db label)
  "Signal an error if DA and DB buffers have diverged.
Raises a Lisp error (stopping the demo) on any mismatch."
  (let ((ca (with-current-buffer (braid-demo-buf-buffer da)
              (buffer-substring-no-properties (point-min) (point-max))))
        (cb (with-current-buffer (braid-demo-buf-buffer db)
              (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (equal ca cb)
      (error "SYNC ERROR at %s\n  A: %S\n  B: %S" label ca cb))))


;;; ── Random edit generation ───────────────────────────────────────────────

(defun braid-demo--random-edit (dbuf path)
  "Apply a random insert or delete to DBUF's buffer and PUT it to PATH.
Uses the same peer ID as the subscription so the server never echoes
it back to this client."
  (with-current-buffer (braid-demo-buf-buffer dbuf)
    (let* ((len  (- (point-max) (point-min)))
           (edit (if (or (zerop len) (zerop (random 2)))
                     (let* ((pos  (random (1+ len)))
                            (word (nth (random (length braid-demo--words))
                                       braid-demo--words)))
                       (goto-char (+ (point-min) pos))
                       (insert word)
                       (list pos pos word))
                   (let* ((s (random len))
                          (e (min len (+ s 1 (random 3)))))
                     (delete-region (+ (point-min) s) (+ (point-min) e))
                     (list s e "")))))
      (message "[%s] %s at [%d:%d]"
               (braid-demo-buf-name dbuf)
               (if (equal (nth 2 edit) "")
                   "delete"
                 (format "insert %S" (nth 2 edit)))
               (nth 0 edit) (nth 1 edit))
      (braid-http-put "127.0.0.1" 8888 path
                 '() '()
                 (list :range   (list :unit  "text"
                                      :start (nth 0 edit)
                                      :end   (nth 1 edit))
                       :content (nth 2 edit))
                 :peer (braid-http-sub-peer (braid-demo-buf-sub dbuf))))))


;;; ── Shared edit loop ─────────────────────────────────────────────────────

(defun braid-demo--run-edits (da db path n-edits wait-fn)
  "Make N-EDITS alternating between DA and DB, calling WAIT-FN for each delay.
Checks sync before every edit (after the first) and after the last.
Signals a Lisp error immediately on any sync divergence."
  (dotimes (i n-edits)
    (when (> i 0)
      (braid-demo--check-sync da db (format "before edit %d" (1+ i))))
    (braid-demo--random-edit (if (zerop (mod i 2)) da db) path)
    (funcall wait-fn))
  (braid-demo--check-sync da db "final"))


;;; ── Public entry points ──────────────────────────────────────────────────

(defun braid-demo-run (path n-edits &optional delay)
  "Subscribe two side-by-side buffers to PATH; make N-EDITS; verify sync.
DELAY is seconds between edits (default 0.2).  The test server is started
automatically as a child process with output suppressed.
Leaves Emacs open when done — quit with C-x C-c."
  (let* ((delay (or delay 0.2))
         (buf-a (get-buffer-create "*braid-A*"))
         (buf-b (get-buffer-create "*braid-B*"))
         (da    (make-braid-demo-buf :buffer buf-a :name "A"))
         (db    (make-braid-demo-buf :buffer buf-b :name "B")))

    (dolist (b (list buf-a buf-b))
      (with-current-buffer b (erase-buffer)))

    ;; Start server silently before touching the terminal
    (braid-demo--start-server)

    ;; Side-by-side windows
    (delete-other-windows)
    (switch-to-buffer buf-a)
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buf-b)
    (other-window 1)
    (sit-for 0)

    ;; Discard any keyboard input that arrived before we took over
    ;; (e.g. from server startup noise on the same terminal)
    (discard-input)

    ;; Open subscriptions
    (setf (braid-demo-buf-sub da)
          (braid-http-subscribe "127.0.0.1" 8888 path
                           (lambda (msg) (braid-demo--on-message da msg))))
    (setf (braid-demo-buf-sub db)
          (braid-http-subscribe "127.0.0.1" 8888 path
                           (lambda (msg) (braid-demo--on-message db msg))))
    (sit-for 0.3)

    (condition-case err
        (progn
          (braid-demo--run-edits da db path n-edits (lambda () (sit-for delay)))
          (message "✓  SYNC OK after %d edits.  Content: %S"
                   n-edits
                   (with-current-buffer buf-a
                     (buffer-substring-no-properties (point-min) (point-max)))))
      (error
       (message "✗  %s" (error-message-string err))))

    ;; Discard any input that accumulated during sit-for calls
    (discard-input)))

(defun braid-demo-test ()
  "Batch-mode sync test.  Exits with 0 on success, 1 on failure."
  (let* ((path  "/demo")
         (buf-a (get-buffer-create "*braid-A*"))
         (buf-b (get-buffer-create "*braid-B*"))
         (da    (make-braid-demo-buf :buffer buf-a :name "A"))
         (db    (make-braid-demo-buf :buffer buf-b :name "B")))
    (dolist (b (list buf-a buf-b))
      (with-current-buffer b (erase-buffer)))

    (braid-demo--start-server)

    (setf (braid-demo-buf-sub da)
          (braid-http-subscribe "127.0.0.1" 8888 path
                           (lambda (msg) (braid-demo--on-message da msg))))
    (setf (braid-demo-buf-sub db)
          (braid-http-subscribe "127.0.0.1" 8888 path
                           (lambda (msg) (braid-demo--on-message db msg))))
    (accept-process-output nil 0.3)

    (condition-case err
        (progn
          (braid-demo--run-edits da db path 10
                                 (lambda () (accept-process-output nil 0.2)))
          (message "PASS: both buffers in sync after 10 edits: %S"
                   (with-current-buffer buf-a
                     (buffer-substring-no-properties (point-min) (point-max))))
          (braid-http-unsubscribe (braid-demo-buf-sub da))
          (braid-http-unsubscribe (braid-demo-buf-sub db))
          (kill-emacs 0))
      (error
       (message "FAIL: %s" (error-message-string err))
       (braid-http-unsubscribe (braid-demo-buf-sub da))
       (braid-http-unsubscribe (braid-demo-buf-sub db))
       (kill-emacs 1)))))

(provide 'braid-demo)
;;; braid-demo.el ends here
