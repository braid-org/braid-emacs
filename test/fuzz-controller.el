;;; fuzz-controller.el --- Braid-fuzz controller for Emacs -*- lexical-binding: t -*-
;;
;; Implements the braid-fuzz JSON-RPC controller protocol.
;; Reads JSON commands from stdin, writes JSON responses to stdout.
;; Also pushes async events (update, ack, error) from callbacks.
;;
;; Launch via:
;;   emacs --batch -L /path/to/braid-emacs -l test/fuzz-controller.el
;;
;; Commands:
;;   hello, results, open-http, close-http, sync-text, edit,
;;   send-text, ack, end-sync, kill-put,
;;   connect-cursors, set-cursor, get-cursors

(message "fc: loading controller...")

(require 'json)
(require 'braid-http)
(require 'braid-text)
(require 'braid-cursors)

(message "fc: requires loaded, setting up...")

;;; ── State ──────────────────────────────────────────────────────────

(defvar fc-bt nil "Active braid-text instance.")
(defvar fc-bc nil "Active braid-cursor instance.")
(defvar fc-buf nil "Buffer being synced.")
(defvar fc-connected nil)
(defvar fc-disconnected nil)

;; Raw HTTP state (for open-http / close-http)
(defvar fc-http-sub nil "Active braid-http subscription (for open-http GET).")
(defvar fc-http-put-proc nil "Active PUT process (for open-http PUT).")

;; TCP client connection (set when client connects to our TCP server)
(defvar fc-client-proc nil "The TCP client connection process.")

;;; ── I/O ────────────────────────────────────────────────────────────

(defun fc-emit (obj)
  "Write OBJ as JSON to the TCP client, followed by newline."
  (when (and fc-client-proc (process-live-p fc-client-proc))
    (process-send-string fc-client-proc
                         (concat (json-encode obj) "\n"))))

(defun fc-reply (id &optional data)
  "Reply to command ID with ok and optional DATA alist."
  (fc-emit (append `((id . ,id) (ok . t)) data)))

(defun fc-reply-error (id msg)
  "Reply to command ID with error MSG."
  (fc-emit `((id . ,id) (error . ,msg))))

(defun fc-push-event (event data)
  "Push an unsolicited event (no id)."
  (fc-emit `((event . ,event) (data . ,data))))

(defun fc-read-line ()
  "Read one line from stdin. Returns nil on EOF."
  (condition-case nil
      (read-string "")
    (error nil)))

;;; ── URL parsing helper ─────────────────────────────────────────────

(defun fc-parse-url (url)
  "Parse URL into (host port path tls).
Supports http:// and https:// URLs."
  (when (string-match "^\\(https?\\)://\\([^/:]+\\)\\(?::\\([0-9]+\\)\\)?\\(/.*\\)" url)
    (let* ((scheme (match-string 1 url))
           (host (match-string 2 url))
           (port-str (match-string 3 url))
           (path (match-string 4 url))
           (tls (equal scheme "https"))
           (port (if port-str (string-to-number port-str) (if tls 443 80))))
      (list host port path tls))))

;;; ── Command handlers ───────────────────────────────────────────────

(defun fc-handle (msg)
  "Handle a parsed JSON command MSG."
  (let* ((id  (cdr (assq 'id msg)))
         (cmd (cdr (assq 'cmd msg))))
    (condition-case err
        (cond

         ;; ── Basics ──────────────────────────────────────────
         ((equal cmd "hello")
          (fc-reply id))

         ((equal cmd "results")
          (fc-reply id)
          ;; Clean up
          (when fc-bc (ignore-errors (braid-cursors-close fc-bc)))
          (when fc-bt (ignore-errors (braid-text-close fc-bt)))
          (when fc-http-sub (ignore-errors (braid-http-unsubscribe fc-http-sub)))
          (when (buffer-live-p fc-buf) (kill-buffer fc-buf))
          ;; Signal main loop to stop. The braid-fuzz harness will SIGTERM
          ;; us after 500ms (kill-emacs can't terminate while the reader
          ;; thread is blocked on stdin).
          (setq fc-running nil))

         ;; ── Raw HTTP (open-http / close-http) ──────────────
         ((equal cmd "open-http")
          (let* ((url (cdr (assq 'url msg)))
                 (parsed (fc-parse-url url))
                 (host (nth 0 parsed))
                 (port (nth 1 parsed))
                 (path (nth 2 parsed))
                 (tls  (nth 3 parsed))
                 (method (upcase (or (cdr (assq 'method msg)) "GET")))
                 (subscribe (cdr (assq 'subscribe msg)))
                 (heartbeats-val (cdr (assq 'heartbeats msg)))
                 (extra-headers (cdr (assq 'headers msg)))
                 (peer (cdr (assq 'peer msg)))
                 (version (cdr (assq 'version msg)))
                 (parents (cdr (assq 'parents msg)))
                 (patches (cdr (assq 'patches msg))))

            ;; Convert extra-headers from alist to alist of (string . string)
            (let ((hdrs nil))
              (when extra-headers
                (dolist (pair (if (listp extra-headers) extra-headers nil))
                  (push (cons (symbol-name (car pair)) (cdr pair)) hdrs)))

              (if (equal method "PUT")
                  ;; ── PUT with retry ──
                  (let* ((patch-obj (and patches (> (length patches) 0) (aref patches 0)))
                         (unit (or (and patch-obj (cdr (assq 'unit patch-obj))) "text"))
                         (range-str (and patch-obj (cdr (assq 'range patch-obj))))
                         (content (or (and patch-obj (cdr (assq 'content patch-obj))) ""))
                         ;; Parse range "[N:M]" → start, end
                         (start 0) (end 0))
                    (when (and range-str (string-match "\\[\\([0-9]+\\):\\([0-9]+\\)\\]" range-str))
                      (setq start (string-to-number (match-string 1 range-str)))
                      (setq end (string-to-number (match-string 2 range-str))))
                    ;; Convert version/parents from vectors to lists
                    (when (vectorp version) (setq version (append version nil)))
                    (when (vectorp parents) (setq parents (append parents nil)))
                    (let* ((merge-type (and hdrs (cdr (assoc "Merge-Type" hdrs))))
                           (patch-plist `(:range (:unit ,unit :start ,start :end ,end)
                                          :content ,content))
                           (put-fn nil))
                      ;; Define a retry-capable PUT sender
                      (setq put-fn
                            (lambda ()
                              (setq fc-http-put-proc
                                    (braid-http-put host port path
                                                    version parents patch-plist
                                                    :peer peer
                                                    :merge-type merge-type
                                                    :tls tls
                                                    :on-done (lambda (status)
                                                               (if (and (>= status 500) (/= status 550))
                                                                   ;; 5xx (except 550) → retry after 1s
                                                                   (run-at-time 1.0 nil put-fn)
                                                                 (fc-push-event "ack" `((status . ,status)))))
                                                    :on-error (lambda (_errmsg)
                                                                ;; Connection died → retry after 1s
                                                                (run-at-time 1.0 nil put-fn))))))
                      (funcall put-fn))
                    (fc-reply id))

                ;; ── GET (subscription) ──
                (progn
                  (setq fc-http-sub
                        (braid-http-subscribe
                         host port path
                         ;; on-message callback → push update events
                         (lambda (update)
                           (let* ((ver (plist-get update :version))
                                  (par (plist-get update :parents))
                                  (body (plist-get update :body))
                                  (cr (plist-get update :content-range))
                                  (patches-list (plist-get update :patches))
                                  (item `((version . ,(and ver (vconcat ver)))
                                          (parents . ,(and par (vconcat par))))))
                             ;; Build patches array from content-range or patches list
                             (cond
                              (patches-list
                               ;; Patches:N format (N > 0)
                               (let ((parr (mapcar
                                            (lambda (p)
                                              (let ((pcr (plist-get p :content-range))
                                                    (pbody (plist-get p :body)))
                                                `((range . ,(and pcr (vector (nth 1 pcr) (nth 2 pcr))))
                                                  (content . ,pbody)
                                                  (unit . ,(and pcr (nth 0 pcr))))))
                                            patches-list)))
                                 (push (cons 'patches (vconcat parr)) item)))
                              (cr
                               ;; Single patch via Content-Range
                               (let ((patch `((range . ,(vector (nth 1 cr) (nth 2 cr)))
                                              (content . ,body)
                                              (unit . ,(nth 0 cr)))))
                                 (push (cons 'patches (vector patch)) item)))
                              (body
                               ;; Snapshot (full body)
                               (push (cons 'body body) item))
                              (t
                               ;; Patches: 0 — no body, no patches, no content-range
                               (push (cons 'patches (vector)) item)))
                             (fc-push-event "update" item)))
                         :peer peer
                         :headers hdrs
                         :tls tls
                         :heartbeats heartbeats-val
                         :on-connect (lambda () nil)
                         :on-disconnect (lambda () nil)))
                  (fc-reply id))))))

         ((equal cmd "close-http")
          (when fc-http-sub
            (braid-http-unsubscribe fc-http-sub)
            (setq fc-http-sub nil))
          (fc-reply id))

         ;; ── Simpleton (sync-text / edit / send-text / ack / end-sync) ──

         ((equal cmd "sync-text")
          (let* ((url (cdr (assq 'url msg)))
                 (parsed (fc-parse-url url))
                 (host (nth 0 parsed))
                 (port (nth 1 parsed))
                 (path (nth 2 parsed))
                 (tls  (nth 3 parsed)))
            (setq fc-buf (generate-new-buffer " *fuzz-ctrl*"))
            (setq fc-connected nil)
            (setq fc-disconnected nil)
            (setq fc-bt
                  (braid-text-open host port path fc-buf
                                   :tls tls
                                   :heartbeats nil
                                   :on-connect (lambda ()
                                                 (setq fc-connected t)
                                                 (setq fc-disconnected nil))
                                   :on-disconnect (lambda ()
                                                    (setq fc-disconnected t)
                                                    (setq fc-connected nil))))
            (fc-reply id)))

         ((equal cmd "edit")
          (let ((pos  (or (cdr (assq 'pos msg)) 0))
                (len  (or (cdr (assq 'len msg)) 0))
                (text (or (cdr (assq 'text msg)) "")))
            (with-current-buffer fc-buf
              (let ((inhibit-modification-hooks t))
                ;; Delete len chars at pos, then insert text
                (goto-char (+ (point-min) pos))
                (when (> len 0)
                  (delete-char (min len (- (point-max) (point)))))
                (insert text)))
            ;; Flush the edit through simpleton
            (when fc-bt
              (braid-text--changed fc-bt))
            (fc-reply id)))

         ((equal cmd "send-text")
          (let ((text (if (and fc-buf (buffer-live-p fc-buf))
                          (with-current-buffer fc-buf
                            (buffer-substring-no-properties (point-min) (point-max)))
                        "")))
            (fc-reply id `((state . ,text)))))

         ((equal cmd "ack")
          (if (or (null fc-bt) (<= (braid-text-outstanding-changes fc-bt) 0))
              (fc-reply id)
            ;; Wait for outstanding changes to drain
            (let ((deadline (+ (float-time) 30)))
              (while (and (> (braid-text-outstanding-changes fc-bt) 0)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05))
              (if (<= (braid-text-outstanding-changes fc-bt) 0)
                  (fc-reply id)
                (fc-reply-error id "timeout waiting for acks")))))

         ((equal cmd "end-sync")
          (when fc-bt
            (braid-text-close fc-bt)
            (setq fc-bt nil))
          (fc-reply id))

         ((equal cmd "kill-put")
          (when fc-bt
            (let ((proc (braid-text-put-proc fc-bt)))
              (when (and proc (process-live-p proc))
                (delete-process proc)))
            (braid-text--put-proc-reconnect fc-bt))
          (fc-reply id))

         ;; ── Cursors ────────────────────────────────────────

         ((equal cmd "connect-cursors")
          (if (null fc-bt)
              (fc-reply-error id "no active sync-text connection")
            (let ((result nil)
                  (deadline (+ (float-time) 10)))
              (braid-cursors-start-sharing fc-bt
                (lambda (bc) (setq fc-bc bc) (setq result t)))
              (while (and (null result) (< (float-time) deadline))
                (accept-process-output nil 0.05))
              (if fc-bc
                  (fc-reply id)
                (fc-reply-error id "failed to connect cursors")))))

         ((equal cmd "set-cursor")
          (let ((pos (or (cdr (assq 'pos msg)) 0))
                (end-pos (cdr (assq 'end msg))))
            (when (and fc-buf (buffer-live-p fc-buf))
              (with-current-buffer fc-buf
                (goto-char (+ (point-min) pos))))
            (when fc-bc
              (let ((pair (cons pos (or end-pos pos))))
                (braid-cursors--do-send fc-bc pair)))
            (fc-reply id)))

         ((equal cmd "get-cursors")
          (let ((result nil))
            (when (and fc-bc (braid-cursor-remote fc-bc))
              (maphash
               (lambda (peer-id entry)
                 (let ((sels (mapcar (lambda (sel)
                                       `((from . ,(car sel)) (to . ,(cdr sel))))
                                     (plist-get entry :selections))))
                   (push (cons peer-id (vconcat sels)) result)))
               (braid-cursor-remote fc-bc)))
            (fc-reply id `((cursors . ,(or result ()))))))

         ;; ── Unknown command ────────────────────────────────
         (t
          (fc-reply-error id (format "unknown command: %s" cmd))))

      ;; Error handler
      (error
       (fc-reply-error id (error-message-string err))))))

;;; ── Main loop ──────────────────────────────────────────────────────
;;
;; In --batch mode, (read-string "") and threads both block the whole
;; process.  We use a local TCP server instead: the launcher script runs
;; a tiny forwarder that reads stdin and sends to our TCP port.  Emacs
;; reads from the TCP connection via process filters (non-blocking).

(defvar fc-running t)
(defvar fc-stdin-buf "")
(defvar fc-tcp-port nil)

(defun fc-process-line (line)
  "Process a single JSON command line."
  (let ((trimmed (string-trim line)))
    (unless (string-empty-p trimmed)
      (condition-case err
          (let ((msg (json-read-from-string trimmed)))
            (fc-handle msg))
        (error
         (fc-emit `((error . ,(format "parse error: %s"
                                       (error-message-string err))))))))))

;; Create TCP server on a random port
(let* ((server (make-network-process
                :name "fc-server"
                :server t
                :host "127.0.0.1"
                :service 0  ;; random port
                :family 'ipv4
                :noquery t
                ;; For server processes, filter/sentinel are inherited by
                ;; child connection processes.
                :filter (lambda (proc output)
                          ;; Track the client connection
                          (unless fc-client-proc
                            (setq fc-client-proc proc))
                          (setq fc-stdin-buf (concat fc-stdin-buf output))
                          (while (string-match "\n" fc-stdin-buf)
                            (let ((line (substring fc-stdin-buf 0 (match-beginning 0))))
                              (setq fc-stdin-buf (substring fc-stdin-buf (match-end 0)))
                              (fc-process-line line))))
                :sentinel (lambda (proc event)
                            ;; Only stop on client disconnection, not server events
                            (when (and (string-match-p "\\(deleted\\|connection broken\\)" event)
                                       (not (eq (process-contact proc :server) t)))
                              (setq fc-running nil)))))
       (addr (process-contact server :local))
       (port (aref addr (1- (length addr)))))
  (setq fc-tcp-port port)
  ;; Print port on stderr for the launcher script to read
  (message "FC_PORT=%d" port)

  ;; Main event loop
  (while fc-running
    (accept-process-output nil 0.05)))

(kill-emacs 0)
