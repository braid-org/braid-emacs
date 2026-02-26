;;; fuzz-emacs-agent.el --- Emacs agent for fuzz testing -*- lexical-binding: t -*-
;;
;; Controlled via stdin/stdout line protocol by the JS fuzz driver.
;; Each line from stdin is a JSON command; each response is a JSON line on stdout.
;;
;; Commands:
;;   {"cmd":"connect","host":"...","port":N,"path":"..."}
;;   {"cmd":"insert","pos":N,"text":"..."}
;;   {"cmd":"delete","start":N,"end":N}
;;   {"cmd":"changed"}  (or "flush" for compat)
;;   {"cmd":"wait-ack","timeout":N}
;;   {"cmd":"state"}
;;   {"cmd":"client-state"}
;;   {"cmd":"version"}
;;   {"cmd":"pending"}
;;   {"cmd":"kill-sub"}
;;   {"cmd":"kill-put"}
;;   {"cmd":"wait-connected","timeout":N}
;;   {"cmd":"quit"}

(require 'json)
(require 'braid-http)
(require 'braid-text)
(require 'braid-cursors)

(defvar fuzz-bt nil "The braid-text instance.")
(defvar fuzz-bc nil "The braid-cursor instance.")
(defvar fuzz-buf nil "The buffer being synced.")
(defvar fuzz-connected nil)
(defvar fuzz-disconnected nil)

(defun fuzz-respond (obj)
  "Write OBJ as JSON to stdout, followed by newline."
  (princ (json-encode obj))
  (princ "\n"))

(defun fuzz-read-line ()
  "Read one line from stdin.  Returns nil on EOF."
  (condition-case nil
      (read-string "")
    (error nil)))

(defun fuzz-handle (cmd-json)
  "Parse and execute a JSON command, return a response plist."
  (let* ((cmd-obj (json-read-from-string cmd-json))
         (cmd (cdr (assq 'cmd cmd-obj))))
    (cond
     ((equal cmd "connect")
      (let ((host (cdr (assq 'host cmd-obj)))
            (port (cdr (assq 'port cmd-obj)))
            (path (cdr (assq 'path cmd-obj))))
        (setq fuzz-buf (generate-new-buffer " *fuzz*"))
        (setq fuzz-connected nil)
        (setq fuzz-disconnected nil)
        (setq braid-text-debug t)
        (setq fuzz-bt
              (braid-text-open host port path fuzz-buf
                               :heartbeat-interval nil
                               :on-connect (lambda ()
                                             (setq fuzz-connected t)
                                             (setq fuzz-disconnected nil))
                               :on-disconnect (lambda ()
                                                (setq fuzz-disconnected t)
                                                (setq fuzz-connected nil))))
        '((ok . t))))

     ((equal cmd "insert")
      (let ((pos (cdr (assq 'pos cmd-obj)))
            (text (cdr (assq 'text cmd-obj))))
        (with-current-buffer fuzz-buf
          (let ((inhibit-modification-hooks t))
            ;; pos is 0-indexed code-point offset; convert to Emacs 1-indexed
            (goto-char (+ (point-min) pos))
            (insert text)))
        '((ok . t))))

     ((equal cmd "delete")
      (let ((start (cdr (assq 'start cmd-obj)))
            (end (cdr (assq 'end cmd-obj))))
        (with-current-buffer fuzz-buf
          (let ((inhibit-modification-hooks t))
            (delete-region (+ (point-min) start) (+ (point-min) end))))
        '((ok . t))))

     ((or (equal cmd "changed") (equal cmd "flush"))
      (braid-text--changed fuzz-bt)
      '((ok . t)))

     ((equal cmd "wait-ack")
      (let* ((timeout (or (cdr (assq 'timeout cmd-obj)) 10))
             (deadline (+ (float-time) timeout)))
        (while (and (> (braid-text-outstanding-changes fuzz-bt) 0)
                    (< (float-time) deadline))
          (accept-process-output nil 0.05))
        (if (<= (braid-text-outstanding-changes fuzz-bt) 0)
            '((ok . t))
          `((ok . :json-false) (error . "timeout waiting for acks")))))

     ((equal cmd "state")
      (let ((text (with-current-buffer fuzz-buf
                    (buffer-substring-no-properties (point-min) (point-max)))))
        `((ok . t) (state . ,text) (length . ,(length text)))))

     ((equal cmd "client-state")
      (let ((cs (braid-text-client-state fuzz-bt)))
        `((ok . t) (state . ,cs) (length . ,(length cs)))))

     ((equal cmd "version")
      `((ok . t) (version . ,(braid-text-client-version fuzz-bt))))

     ((equal cmd "pending")
      `((ok . t) (pending . ,(braid-text-outstanding-changes fuzz-bt))))

     ((equal cmd "kill-sub")
      (let ((proc (braid-http-sub-process (braid-text-sub fuzz-bt))))
        (when (and proc (process-live-p proc))
          (delete-process proc)))
      '((ok . t)))

     ((equal cmd "kill-put")
      (let ((proc (braid-text-put-proc fuzz-bt)))
        (when (and proc (process-live-p proc))
          (delete-process proc)))
      ;; delete-process fires "deleted" sentinel (treated as intentional).
      ;; Simulate real network failure by triggering reconnect + reset.
      (braid-text--put-proc-reconnect fuzz-bt)
      '((ok . t)))

     ((equal cmd "wait-connected")
      (let* ((timeout (or (cdr (assq 'timeout cmd-obj)) 10))
             (deadline (+ (float-time) timeout)))
        (while (and (not fuzz-connected)
                    (< (float-time) deadline))
          (accept-process-output nil 0.05))
        (if fuzz-connected
            '((ok . t))
          `((ok . :json-false) (error . "timeout waiting for connect")))))

     ((equal cmd "wait-text")
      (let* ((expected (cdr (assq 'text cmd-obj)))
             (timeout (or (cdr (assq 'timeout cmd-obj)) 10))
             (deadline (+ (float-time) timeout)))
        (while (and (not (equal expected
                                (with-current-buffer fuzz-buf
                                  (buffer-substring-no-properties (point-min) (point-max)))))
                    (< (float-time) deadline))
          (accept-process-output nil 0.05))
        (let ((actual (with-current-buffer fuzz-buf
                        (buffer-substring-no-properties (point-min) (point-max)))))
          (if (equal expected actual)
              '((ok . t))
            `((ok . :json-false) (error . "text mismatch") (actual . ,actual))))))

     ((equal cmd "spin")
      ;; Just spin the event loop for a given time
      (let ((duration (or (cdr (assq 'duration cmd-obj)) 0.5)))
        (accept-process-output nil duration))
      '((ok . t)))

     ((equal cmd "open-cursors")
      (setq fuzz-bc (braid-cursors-open fuzz-bt))
      (if fuzz-bc
          '((ok . t))
        '((ok . :json-false) (error . "server does not support cursors"))))

     ((equal cmd "set-cursor")
      ;; pos is 0-indexed code-point offset
      (let ((pos (cdr (assq 'pos cmd-obj))))
        (with-current-buffer fuzz-buf
          (goto-char (+ (point-min) pos)))
        ;; Send cursor directly (idle timers don't fire reliably in batch mode)
        (when fuzz-bc
          (let ((pair (cons pos pos)))
            (braid-cursors--do-send fuzz-bc pair)))
        '((ok . t))))

     ((equal cmd "cursor-pos")
      ;; Return current cursor position (0-indexed)
      (let ((pos (with-current-buffer fuzz-buf
                   (- (point) (point-min)))))
        `((ok . t) (pos . ,pos))))

     ((equal cmd "remote-cursors")
      ;; Return hash of peer-id → selections
      (let ((result nil))
        (when (and fuzz-bc (braid-cursor-remote fuzz-bc))
          (maphash
           (lambda (peer-id entry)
             (let ((sels (mapcar (lambda (sel)
                                   `((from . ,(car sel)) (to . ,(cdr sel))))
                                 (plist-get entry :selections))))
               (push (cons peer-id (vconcat sels)) result)))
           (braid-cursor-remote fuzz-bc)))
        `((ok . t) (cursors . ,(or result ())))))

     ((equal cmd "quit")
      (when fuzz-bc (braid-cursors-close fuzz-bc))
      (when fuzz-bt (braid-text-close fuzz-bt))
      (when (buffer-live-p fuzz-buf) (kill-buffer fuzz-buf))
      '((ok . t) (quit . t)))

     (t
      `((ok . :json-false) (error . ,(format "unknown command: %s" cmd)))))))

;; Main loop
(let ((running t))
  (while running
    (let ((line (fuzz-read-line)))
      (if (null line)
          (setq running nil)
        (condition-case err
            (let ((resp (fuzz-handle line)))
              (fuzz-respond resp)
              (when (cdr (assq 'quit resp))
                (setq running nil)))
          (error
           (fuzz-respond `((ok . :json-false)
                           (error . ,(error-message-string err))))))))))

(kill-emacs 0)
