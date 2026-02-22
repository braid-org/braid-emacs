;;; braid-text.el --- Braid-HTTP simpleton client (buffer-based) -*- lexical-binding: t -*-

;; Author: Michael Toomim <toomim@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/braid-org/braid-emacs
;; Keywords: comm, tools

;;; Commentary:

;; Implements the simpleton sync algorithm on top of braid-http.el.
;; See https://braid.org/protocol/simpleton for the protocol spec.
;; Connects an Emacs buffer to a braid-text server resource and handles
;; diffing, patching, version tracking, and digest verification.

;;; Code:

(require 'cl-lib)
(require 'braid-http)

(defvar braid-text-debug nil
  "When non-nil, log buffer state before/after each applied update.")

(defvar braid-text-put-ack-timeout 20.0
  "Seconds to wait for a PUT ACK before declaring the connection dead.")


;;;; ======================================================================
;;;; Struct
;;;; ======================================================================

(cl-defstruct braid-text
  "State for one simpleton-synced buffer."
  host port path peer buffer
  (tls             nil)    ; t to use TLS
  (client-version nil)    ; sorted list of version strings
  (client-state   "")     ; text content as of client-version
                          ;   may differ from buffer text if local edits are pending
  (char-counter    -1)     ; cumulative char-delta; used to form version IDs
  (put-proc        nil)    ; persistent network process for pipelining PUTs
  (put-queue       "")     ; raw PUT request bytes queued while put-proc is connecting
  (pending-puts    0)      ; number of PUTs sent but not yet acked by server
  (put-ack-timer   nil)    ; one-shot timer: fires if PUTs go unacked too long
  (max-outstanding-puts 10)  ; throttle: stop sending when this many PUTs unacked
  (muted-until     nil)    ; float-time until which PUTs are suppressed (503 backpressure)
  (content-type    "text/plain") ; content-type from server, used in PUTs
  sub)                     ; braid-http-sub handle


;;;; ======================================================================
;;;; Public API
;;;; ======================================================================

(cl-defun braid-text-open (host port path buffer
                          &key tls on-connect on-disconnect
                          (heartbeat-interval 30))
  "Subscribe BUFFER to the braid-text resource at HOST:PORT/PATH.
Uses the simpleton merge type.  Returns a braid-text struct.
Pass the struct to `braid-text-buffer-changed' whenever the buffer
is locally edited, and to `braid-text-close' to disconnect.

ON-CONNECT and ON-DISCONNECT are optional callbacks forwarded to
`braid-http-subscribe'.
HEARTBEAT-INTERVAL (default 30) requests server heartbeats every N seconds.
Set to nil to disable heartbeat-based dead connection detection."
  (let* ((peer (format "%04x%04x" (random #xffff) (random #xffff)))
         (bt   (make-braid-text :host   host
                                :port   port
                                :path   path
                                :peer   peer
                                :buffer buffer
                                :tls    tls)))
    (setf (braid-text-sub bt)
          (braid-http-subscribe host port path
                           (lambda (msg) (braid-text--on-message bt msg))
                           :peer          peer
                           :tls           tls
                           :on-connect    (lambda ()
                                          ;; Flush offline edits before server data arrives.
                                          ;; This advances client-version so the server's
                                          ;; patches (parented at our old version) are rejected
                                          ;; by the parent check.  The server will then rebase.
                                          (braid-text--flush bt)
                                          (when on-connect (funcall on-connect)))
                           :on-disconnect on-disconnect
                           :heartbeat-interval heartbeat-interval
                           :parents-fn    (lambda () (braid-text-client-version bt))
                           :extra-headers '(("Merge-Type" . "simpleton")
                                             ("Accept"     . "text/plain, text/markdown, text/html, application/json"))))
    (setf (braid-text-put-proc bt) (braid-text--put-proc-open bt))
    bt))

(defun braid-text-buffer-changed (bt)
  "Diff BT's buffer against its last-known state and PUT a patch if changed.
Call this from `after-change-functions' whenever the buffer has been locally edited."
  (braid-text--flush bt))

(defun braid-text--flush (bt)
  "Diff BT's buffer against its last-known state and PUT a patch if changed.
The PUT is sent immediately on the persistent put-proc connection (pipelined)."
  (unless (or
           ;; Throttle: don't send if too many PUTs outstanding
           (>= (braid-text-pending-puts bt) (braid-text-max-outstanding-puts bt))
           ;; Backpressure: don't send if muted (503)
           (and (braid-text-muted-until bt)
                (< (float-time) (braid-text-muted-until bt))))
    (when (braid-text-muted-until bt)
      (setf (braid-text-muted-until bt) nil))
    (let* ((new-state (braid-text--buffer-text bt))
           (prev      (braid-text-client-state bt)))
    (unless (equal new-state prev)
      (let* ((diff          (braid-text--simple-diff prev new-state))
             (start         (plist-get diff :start))
             (end           (plist-get diff :end))
             (content       (plist-get diff :content))
             (delta         (+ (- end start) (length content)))
             (parents       (braid-text-client-version bt)))
        (cl-incf (braid-text-char-counter bt) delta)
        (let* ((version       (list (format "%s-%d"
                                            (braid-text-peer bt)
                                            (braid-text-char-counter bt))))
               (content-bytes (encode-coding-string content 'utf-8))
               (body-headers  `(("Content-Type"   . ,(braid-text-content-type bt))
                                 ("Merge-Type"     . "simpleton")
                                 ("Content-Length" . ,(number-to-string (length content-bytes)))
                                 ("Content-Range"  . ,(format "text [%d:%d]" start end))
                                 ("Peer"           . ,(braid-text-peer bt))))
               (request       (braid-http--format-put
                                (braid-text-host bt) (braid-text-port bt) (braid-text-path bt)
                                version parents body-headers content-bytes))
               (proc          (braid-text-put-proc bt)))
          (setf (braid-text-client-version bt) version)
          (setf (braid-text-client-state bt) new-state)
          ;; Start PUT ACK timer when transitioning 0 → >0
          (when (and (= (braid-text-pending-puts bt) 0)
                     (not (braid-text-put-ack-timer bt)))
            (setf (braid-text-put-ack-timer bt)
                  (run-with-timer
                   braid-text-put-ack-timeout nil
                   (lambda ()
                     (when (> (braid-text-pending-puts bt) 0)
                       (braid-text--connection-dead bt))))))
          (cl-incf (braid-text-pending-puts bt))
          (if (and proc (process-live-p proc))
              (process-send-string proc request)
            ;; put-proc is dead — queue the request and reconnect if needed
            (setf (braid-text-put-queue bt)
                  (concat (braid-text-put-queue bt) request))
            (when proc (braid-text--put-proc-reconnect bt)))))))))

(defun braid-text-close (bt)
  "Close BT's PUT connection and subscription, stopping all syncing."
  (when (braid-text-put-ack-timer bt)
    (cancel-timer (braid-text-put-ack-timer bt))
    (setf (braid-text-put-ack-timer bt) nil))
  (when-let ((p (braid-text-put-proc bt)))
    (when (process-live-p p) (delete-process p)))
  (setf (braid-text-put-proc  bt) nil)
  (setf (braid-text-put-queue bt) "")
  (braid-http-unsubscribe (braid-text-sub bt)))

(defun braid-text--reconnect (bt)
  "Reconnect BT's subscription without resetting state.
Closes the old subscription and opens a fresh one with Parents set to
`client-version' (via parents-fn).  Local buffer edits are preserved
because the server will send patches from our version, not a full snapshot."
  (message "Braid: reconnecting")
  ;; Reset pending-puts counter for throttling — retried PUTs will re-increment it
  (setf (braid-text-pending-puts bt) 0)
  (when (braid-text-put-ack-timer bt)
    (cancel-timer (braid-text-put-ack-timer bt))
    (setf (braid-text-put-ack-timer bt) nil))
  ;; Preserve callbacks and max-delay from the old subscription.
  (let* ((old-sub       (braid-text-sub bt))
         (on-connect    (braid-http-sub-on-connect old-sub))
         (on-disconnect (braid-http-sub-on-disconnect old-sub))
         (max-delay     (braid-http-sub-reconnect-max-delay old-sub))
         (hb-interval   (braid-http-sub-heartbeat-interval old-sub)))
    ;; Close the old subscription
    (braid-http-unsubscribe old-sub)
    ;; Reconnect put-proc
    (when-let ((p (braid-text-put-proc bt)))
      (when (process-live-p p) (delete-process p)))
    (braid-text--put-proc-reconnect bt)
    ;; Open a new subscription — on-connect will flush offline edits
    (let ((new-sub (braid-http-subscribe (braid-text-host bt)
                                    (braid-text-port bt)
                                    (braid-text-path bt)
                                    (lambda (msg) (braid-text--on-message bt msg))
                                    :peer          (braid-text-peer bt)
                                    :tls           (braid-text-tls bt)
                                    :on-connect    on-connect
                                    :on-disconnect on-disconnect
                                    :heartbeat-interval hb-interval
                                    :parents-fn    (lambda () (braid-text-client-version bt))
                                    :extra-headers '(("Merge-Type" . "simpleton")
                                                      ("Accept"     . "text/plain, text/markdown, text/html, application/json")))))
      (setf (braid-http-sub-reconnect-max-delay new-sub) max-delay)
      (setf (braid-text-sub bt) new-sub))))


;;;; ======================================================================
;;;; Persistent PUT connection (private)
;;;; ======================================================================

(defun braid-text--put-proc-open (bt)
  "Open a persistent TCP/TLS connection to BT's host:port for pipelining PUTs.
Returns the process.  No HTTP request is sent on this connection — it is a raw
socket that we write PUT request bytes onto directly."
  (braid-http--make-process
   (format "braid-put:%s:%d%s"
           (braid-text-host bt) (braid-text-port bt) (braid-text-path bt))
   (braid-text-host bt)
   (braid-text-port bt)
   (braid-text-tls  bt)
   ;; Filter: count HTTP responses to decrement pending-puts.
   ;; Handle 503 (backpressure) specially; log other non-2xx errors.
   (lambda (_proc data)
     (let ((start 0)
           (matched nil)
           (should-flush nil))
       (while (string-match "HTTP/1\\.[01] \\([0-9]+\\)" data start)
         (setq start (match-end 0))
         (setq matched t)
         (let ((code (string-to-number (match-string 1 data))))
           (cl-decf (braid-text-pending-puts bt))
           (cond
            ;; 503: server overloaded — enter mute mode
            ((= code 503)
             (message "Braid: server busy (503) — backing off for 3s")
             (setf (braid-text-muted-until bt) (+ (float-time) 3.0))
             (run-with-timer 3.0 nil (lambda () (braid-text--flush bt))))
            ;; Success — may need to flush throttled edits
            ((and (>= code 200) (< code 300))
             (setq should-flush t))
            ;; Other errors — log but don't reconnect (subscription handles that)
            (t
             (message "Braid: PUT rejected (HTTP %d)" code)))))
       (when matched
         (when (< (braid-text-pending-puts bt) 0)
           (setf (braid-text-pending-puts bt) 0))
         ;; Cancel PUT ACK timer when all PUTs are acked
         (when (and (<= (braid-text-pending-puts bt) 0)
                    (braid-text-put-ack-timer bt))
           (cancel-timer (braid-text-put-ack-timer bt))
           (setf (braid-text-put-ack-timer bt) nil))
         (force-mode-line-update)
         ;; Flush throttled edits now that we have capacity
         (when should-flush
           (braid-text--flush bt)))))
   ;; Sentinel: flush queued requests on open; reconnect on unexpected close.
   (lambda (proc event)
     (cond
      ((string-prefix-p "open" event)
       (let ((queued (braid-text-put-queue bt)))
         (setf (braid-text-put-queue bt) "")
         (unless (string-empty-p queued)
           (process-send-string proc queued))))
      ((or (string-prefix-p "finished" event)
           (string-prefix-p "deleted"  event))
       nil)  ; closed intentionally
      (t
       ;; Unexpected disconnect — reconnect after a brief pause.
       (run-with-timer 1.0 nil #'braid-text--put-proc-reconnect bt))))
   'nowait))

(defun braid-text--put-proc-reconnect (bt)
  "Reconnect BT's put-proc after a disconnect.
Queued PUTs are preserved — their version chains are valid because
client-version is never reset on reconnect."
  (setf (braid-text-put-proc bt) (braid-text--put-proc-open bt)))

(defun braid-text--connection-dead (bt)
  "Handle a detected dead connection for BT.
Reconnects the subscription (which will use parents-fn to send
client-version as Parents).  The server will either send patches
from that version, or wait for a retry PUT to establish the version."
  (message "Braid: connection dead (PUT ACK timeout) — reconnecting")
  (braid-text--reconnect bt))


;;;; ======================================================================
;;;; Subscription callback (private)
;;;; ======================================================================

(defun braid-text--repr-digest (text)
  "Compute the repr-digest of TEXT (UTF-8 SHA-256, base64, braid wire format)."
  (let* ((bytes (encode-coding-string text 'utf-8))
         (hash  (secure-hash 'sha256 bytes nil nil t)))
    (format "sha-256=:%s:" (base64-encode-string hash t))))

(defun braid-text--on-message (bt msg)
  "Handle an incoming subscription message for BT."
  (if (not (buffer-live-p (braid-text-buffer bt)))
      ;; Buffer was killed — close the connection.
      (braid-text-close bt)
    (let* ((version (plist-get msg :version))
           (parents (sort (copy-sequence (plist-get msg :parents)) #'string<))
           (patches (plist-get msg :patches))
           (body    (plist-get msg :body))
           (headers (plist-get msg :headers)))
      (when braid-text-debug
        (message "Braid: recv version=%s parents=%s cur=%s match=%s patches=%d body=%s"
                 version parents (braid-text-client-version bt)
                 (equal parents (braid-text-client-version bt))
                 (length (or patches '()))
                 (if body (format "%dchars" (length body)) "nil")))
      ;; Simpleton: only accept if parents == our client-version
      (when (equal parents (braid-text-client-version bt))
        (condition-case err
            (progn
              ;; Apply patches incrementally or set full body (initial connect)
              (cond
               (patches
                (braid-text--apply-patches bt patches))
               (t
                (braid-text--set-buffer-text bt (or body ""))))
              ;; Advance version only after the apply succeeds, so a crash here
              ;; does not silently advance the version and cause divergence.
              (setf (braid-text-client-version bt)
                    (sort (copy-sequence version) #'string<))
              ;; Capture content-type from server for use in PUTs.
              (when-let ((ct (cdr (assoc "content-type" headers))))
                (setf (braid-text-content-type bt) ct))
              ;; Verify integrity if the server sent a repr-digest.
              ;; Compare against client-state (= buffer after applying patches).
              (when-let ((expected (cdr (assoc "repr-digest" headers))))
                (let ((actual (braid-text--repr-digest (braid-text-client-state bt))))
                  (unless (equal actual expected)
                    (message "Braid: DIGEST MISMATCH after version %s — reconnecting"
                             version)
                    (run-with-timer 0.1 nil #'braid-text--reconnect bt))))
              ;; After receiving an ACK-like update, flush any accumulated edits
              ;; (important when throttling has been holding back sends).
              (braid-text--flush bt))
          (error
           (message "Braid: failed to apply update (version %s): %S" version err)))))))



;;;; ======================================================================
;;;; Buffer helpers (private)
;;;; ======================================================================

(defun braid-text--buffer-text (bt)
  "Return the full text of BT's buffer as a string."
  (with-current-buffer (braid-text-buffer bt)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun braid-text--adjust-point (pos start end new-len)
  "Adjust 0-indexed char offset POS for a patch replacing [START,END) with NEW-LEN chars.
If POS is after the region it shifts by the net delta.
If POS is inside the region it is clamped to START.
If POS is at or before START it is unchanged."
  (cond
   ((>= pos end)  (+ pos (- new-len (- end start))))
   ((> pos start) start)
   (t             pos)))

(defun braid-text--with-saved-points (buf thunk)
  "Call THUNK with BUF as current buffer, then restore each window's point.
Before calling THUNK, saves the 0-indexed point offset for every window
displaying BUF.  After THUNK returns the saved offsets (already adjusted
by THUNK via `braid-text--adjust-point') are written back."
  ;; Capture offsets before any edits.
  (let* ((base    (with-current-buffer buf (point-min)))
         (windows (get-buffer-window-list buf nil t))
         (saved   (mapcar (lambda (w) (cons w (- (window-point w) base)))
                          windows))
         ;; THUNK may update `saved' in place; return the final offsets.
         (result  (funcall thunk saved)))
    ;; Recompute base in case erase-buffer changed it (narrowing is rare but safe).
    (let ((new-base (with-current-buffer buf (point-min))))
      (dolist (ws result)
        (set-window-point (car ws) (+ new-base (cdr ws)))))))

(defun braid-text--set-buffer-text (bt text)
  "Replace BT's buffer content with TEXT, suppressing change hooks."
  (let ((buf (braid-text-buffer bt)))
    (braid-text--with-saved-points
     buf
     (lambda (saved)
       (with-current-buffer buf
         (let* ((new-len             (length text))
                (inhibit-modification-hooks t))
           (erase-buffer)
           (insert text)
           (set-buffer-modified-p nil)
           (when buffer-file-name (set-visited-file-modtime))
           ;; Clamp each saved point to the new buffer length.
           (mapcar (lambda (ws)
                     (cons (car ws) (min (cdr ws) new-len)))
                   saved)))))
    (setf (braid-text-client-state bt) text)))

(defun braid-text--adjust-point-absolute (pos patches)
  "Adjust 0-indexed char offset POS for absolute-coordinate PATCHES.
PATCHES must be sorted ascending by start and non-overlapping.
Each patch replaces [start,end) in the original text with its body."
  (let ((cum-delta 0))
    (catch 'done
      (dolist (p patches)
        (let* ((cr    (plist-get p :content-range))
               (start (nth 1 cr))
               (end   (nth 2 cr))
               (new-len (length (plist-get p :body)))
               (delta (- new-len (- end start))))
          (when (< pos start)
            (throw 'done nil))
          (when (< pos end)
            (setq pos start)
            (throw 'done nil))
          (cl-incf cum-delta delta))))
    (+ pos cum-delta)))

(defun braid-text--apply-patches (bt patches)
  "Apply subscription PATCHES to BT's buffer, suppressing change hooks.
Patches use absolute coordinates (all relative to the original state
before any patches in this block).  They are sorted ascending by start
position and applied left-to-right with a cumulative offset to account
for shifts from preceding patches."
  ;; Filter out no-op patches (nil content-range, empty body) that the server
  ;; sends when a concurrent edit is fully subsumed during rebase.
  (setq patches (cl-remove-if
                 (lambda (p)
                   (and (null (plist-get p :content-range))
                        (let ((body (plist-get p :body)))
                          (or (null body) (string-empty-p body)))))
                 patches))
  ;; Sort ascending by start position (protocol does not guarantee order).
  (setq patches (sort patches
                      (lambda (a b)
                        (< (nth 1 (plist-get a :content-range))
                           (nth 1 (plist-get b :content-range))))))
  (let ((buf (braid-text-buffer bt)))
    (braid-text--with-saved-points
     buf
     (lambda (saved)
       (with-current-buffer buf
         (let ((inhibit-modification-hooks t)
               (offset 0))
           ;; Apply left-to-right, tracking cumulative offset from prior patches.
           (dolist (p patches)
             (let* ((cr      (plist-get p :content-range))
                    (start   (+ (nth 1 cr) offset))
                    (end     (+ (nth 2 cr) offset))
                    (content (plist-get p :body)))
               (delete-region (+ (point-min) start)
                              (+ (point-min) end))
               (goto-char (+ (point-min) start))
               (insert content)
               (cl-incf offset (- (length content) (- end start)))))))
       ;; Adjust cursor positions using absolute coordinates.
       ;; Patches are sorted ascending; walk them once per cursor.
       (let ((result
              (mapcar (lambda (ws)
                        (cons (car ws)
                              (braid-text--adjust-point-absolute
                               (cdr ws) patches)))
                      saved)))
         (with-current-buffer buf
           (set-buffer-modified-p nil)
           (when buffer-file-name (set-visited-file-modtime)))
         result)))
    (setf (braid-text-client-state bt)
          (braid-text--buffer-text bt))))


;;;; ======================================================================
;;;; Diff helper (private)
;;;; ======================================================================

(defun braid-text--simple-diff (old new)
  "Return a single-patch plist (:start S :end E :content C) diffing OLD → NEW.
Finds the longest common prefix and suffix and returns the minimal middle edit."
  (let* ((olen   (length old))
         (nlen   (length new))
         (minlen (min olen nlen))
         (p 0)
         (s 0))
    ;; Common prefix
    (while (and (< p minlen) (eq (aref old p) (aref new p)))
      (cl-incf p))
    ;; Common suffix (only within the non-prefix portion)
    (let ((maxs (- minlen p)))
      (while (and (< s maxs)
                  (eq (aref old (- olen s 1)) (aref new (- nlen s 1))))
        (cl-incf s)))
    (list :start   p
          :end     (- olen s)
          :content (substring new p (- nlen s)))))


(provide 'braid-text)
;;; braid-text.el ends here
