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
(require 'myers-diff)

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
  (unacked-puts    nil)    ; list of raw PUT request byte strings awaiting ACK
  (outstanding-changes    0)      ; number of PUTs sent but not yet acked by server
  (put-ack-timer   nil)    ; one-shot timer: fires if PUTs go unacked too long
  (max-outstanding-changes 10)  ; throttle: stop sending when this many PUTs unacked
  (muted-until     nil)    ; float-time until which PUTs are suppressed (503 backpressure)
  (content-type    "text/plain") ; content-type from server, used in PUTs
  (throttled       nil)    ; t when buffer is ahead of client-state and we can't send
  (throttled-update nil)   ; buffered server update to apply when throttle clears
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
  (let* ((peer (format "emacs%04x%04x" (random #xffff) (random #xffff)))
         (bt   (make-braid-text :host   host
                                :port   port
                                :path   path
                                :peer   peer
                                :buffer buffer
                                :tls    tls)))
    (setf (braid-text-sub bt)
          (braid-http-subscribe
           host port path
           (lambda (msg) (braid-text--on-message bt msg))
           :peer          peer
           :tls           tls
           :on-connect    (lambda ()
                            ;; Cancel stale PUT ACK timer — reconnect resets it
                            (when (braid-text-put-ack-timer bt)
                              (cancel-timer (braid-text-put-ack-timer bt))
                              (setf (braid-text-put-ack-timer bt) nil))
                            ;; Ensure put-proc is alive for retrying queued PUTs
                            (unless (and (braid-text-put-proc bt)
                                         (process-live-p (braid-text-put-proc bt)))
                              (braid-text--put-proc-reconnect bt))
                            ;; Send offline edits ONLY if buffer has diverged
                            ;; from client-state (D1+D4).  This advances
                            ;; client-version so the server's patches (parented
                            ;; at our old version) are rejected by the parent
                            ;; check.  The server will then rebase.
                            ;; The JS doesn't need this because changed() runs
                            ;; on every keystroke keeping prev_state current.
                            ;; In Elisp, offline edits accumulate in the buffer
                            ;; without updating client-state.
                            (when (buffer-live-p (braid-text-buffer bt))
                              (unless (equal (braid-text-client-state bt)
                                             (braid-text--buffer-text bt))
                                (braid-text--changed bt)))
                            (when on-connect (funcall on-connect)))
           :on-disconnect on-disconnect
           :heartbeat-interval heartbeat-interval
           :parents-fn    (lambda () (braid-text-client-version bt))
           :extra-headers '(("Merge-Type" . "simpleton")
                            ("Accept"     . "text/plain, text/markdown, text/html, application/json"))))
    
    (setf (braid-text-put-proc bt) (braid-text--put-proc-open bt))
    bt))

(defun braid-text-buffer-changed (bt &optional beg end old-len)
  "Diff BT's buffer against its last-known state and PUT a patch if changed.
Call this from `after-change-functions' whenever the buffer has been locally edited.
When BEG, END, OLD-LEN are provided (from `after-change-functions'), they are
converted to 0-indexed change-info and passed to the fast path."
  (if (and beg end old-len)
      (let* ((base    (with-current-buffer (braid-text-buffer bt) (point-min)))
             (start   (- beg base))
             (end-old (+ start old-len))
             (content (with-current-buffer (braid-text-buffer bt)
                        (buffer-substring-no-properties beg end))))
        (braid-text--changed bt (list start end-old content)))
    (braid-text--changed bt nil)))

(defun braid-text--changed (bt &optional change-info)
  "Diff BT's buffer against its last-known state and PUT a patch if changed.
The PUT is sent immediately on the persistent put-proc connection (pipelined).
When at max outstanding PUTs and the buffer has changed, sets `throttled' to
prevent server updates from being applied to a shifted buffer.  When called
with no pending changes and throttled, clears the throttle and applies any
buffered server update.

CHANGE-INFO, when non-nil, is a list (START END-IN-OLD CONTENT) providing
the exact edit from `after-change-functions' (0-indexed).  This avoids the
O(n) buffer copy and diff on the hot path."
  ;; Clear expired mute
  (when (and (braid-text-muted-until bt)
             (>= (float-time) (braid-text-muted-until bt)))
    (setf (braid-text-muted-until bt) nil))
  (let* ((at-max (>= (braid-text-outstanding-changes bt)
                     (braid-text-max-outstanding-changes bt)))
         (muted (braid-text-muted-until bt)))  ; non-nil means still muted
    ;; Fast path: hook gave us the exact edit, and we can send immediately
    (if (and change-info (not at-max) (not muted)
             (not (braid-text-throttled bt)))
        (let ((start      (nth 0 change-info))
              (end-in-old (nth 1 change-info))
              (content    (nth 2 change-info)))
          ;; Skip no-op edits (e.g. empty insert at a position)
          (unless (and (= start end-in-old) (string-empty-p content))
            (let ((new-state (concat (substring (braid-text-client-state bt) 0 start)
                                     content
                                     (substring (braid-text-client-state bt) end-in-old))))
              (braid-text--send-patch bt new-state start end-in-old content))))
      ;; Slow path: full buffer copy + diff
      (let* ((new-state (braid-text--buffer-text bt))
             (has-change (not (equal new-state (braid-text-client-state bt)))))
        (cond
         ;; No local change — if throttled, clear throttle and apply buffered update
         ((not has-change)
          (when (braid-text-throttled bt)
            (setf (braid-text-throttled bt) nil)
            (let ((update (braid-text-throttled-update bt)))
              (setf (braid-text-throttled-update bt) nil)
              (when update
                (braid-text--on-message bt update)))))

         ;; At max outstanding or muted — mark throttled, don't send
         ((or at-max muted)
          (setf (braid-text-throttled bt) t))

         ;; Normal: send the diff (Myers for multi-patch)
         (t
          (let ((patches (myers-diff-patches
                          (braid-text-client-state bt) new-state)))
            (when patches
              (braid-text--send-patches bt new-state patches)))))))))

(defun braid-text--send-patches (bt new-state patches)
  "Build and send a PUT with one or more PATCHES for BT.
NEW-STATE is the post-edit buffer text (becomes new client-state).
PATCHES is a list of (:start S :end E :content C).
Single-patch PUTs use inline Content-Range; multi-patch use Patches: N."
  (let* ((delta (cl-reduce #'+ patches
                           :key (lambda (p)
                                  (+ (- (plist-get p :end) (plist-get p :start))
                                     (length (plist-get p :content))))))
         (parents (braid-text-client-version bt)))
    (cl-incf (braid-text-char-counter bt) delta)
    (let* ((version (list (format "%s-%d"
                                  (braid-text-peer bt)
                                  (braid-text-char-counter bt))))
           (request (braid-http--format-put-patches
                     (braid-text-host bt) (braid-text-port bt) (braid-text-path bt)
                     version parents patches
                     (braid-text-content-type bt) (braid-text-peer bt)))
           (proc    (braid-text-put-proc bt)))
      (setf (braid-text-client-version bt) version)
      (setf (braid-text-client-state bt) new-state)
      ;; Start PUT ACK timer when transitioning 0 → >0
      (when (and (= (braid-text-outstanding-changes bt) 0)
                 (not (braid-text-put-ack-timer bt)))
        (setf (braid-text-put-ack-timer bt)
              (run-with-timer
               braid-text-put-ack-timeout nil
               (lambda ()
                 (when (> (braid-text-outstanding-changes bt) 0)
                   (braid-text--connection-dead bt))))))
      (cl-incf (braid-text-outstanding-changes bt))
      (setf (braid-text-throttled bt) nil)
      ;; Always append to unacked queue; pop only on ACK
      (setf (braid-text-unacked-puts bt)
            (append (braid-text-unacked-puts bt) (list request)))
      ;; Send immediately if socket is fully connected
      (when (and proc (eq (process-status proc) 'open))
        (process-send-string proc request))
      ;; If proc is dead, reconnect (sentinel will re-send queue)
      (when (and proc (not (process-live-p proc)))
        (braid-text--put-proc-reconnect bt)))))

(defun braid-text--send-patch (bt new-state start end content)
  "Build and send a single-patch PUT for BT.
Thin wrapper around `braid-text--send-patches'."
  (braid-text--send-patches bt new-state
                            (list (list :start start :end end :content content))))

(defun braid-text-close (bt)
  "Close BT's PUT connection and subscription, stopping all syncing."
  (when (braid-text-put-ack-timer bt)
    (cancel-timer (braid-text-put-ack-timer bt))
    (setf (braid-text-put-ack-timer bt) nil))
  (when-let ((p (braid-text-put-proc bt)))
    (when (process-live-p p) (delete-process p)))
  (setf (braid-text-put-proc    bt) nil)
  (setf (braid-text-unacked-puts bt) nil)
  (braid-http-unsubscribe (braid-text-sub bt)))

(defun braid-text--reconnect (bt)
  "Reconnect BT's subscription without resetting state.
Closes the old subscription and opens a fresh one with Parents set to
`client-version' (via parents-fn).  Local buffer edits are preserved
because the server will send patches from our version, not a full snapshot."
  (message "Braid: reconnecting")
  ;; Cancel stale PUT ACK timer — reconnect resets it
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
    ;; Open a new subscription
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

(defun braid-text--fatal-error (bt message)
  "Handle a fatal sync error for BT.
Closes the sync connection, logs MESSAGE, and signals the user.
Matches JS behavior of `throw new Error(...)` which halts the
subscription handler.  The buffer is left intact for manual recovery."
  (message "Braid: FATAL ERROR — %s" message)
  (braid-text-close bt)
  ;; Display prominently so the user notices
  (display-warning 'braid (format "Sync stopped: %s\nBuffer left intact for manual recovery." message)
                   :error))

(defun braid-text--not-authorized (bt code)
  "Handle a 401/403 response for BT.
Reconnects to reload the server's state (discarding the rejected
local edit), then marks the buffer read-only.  The subscription
stays open so the user can still see remote changes."
  (message "Braid: cannot edit — %d not authorized" code)
  (let ((buf (braid-text-buffer bt)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq buffer-read-only t))))
  (braid-text--reconnect bt))


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
   ;; Filter: count HTTP responses to decrement outstanding-changes.
   ;; Status code handling (D7):
   ;;   - 2xx: success, send accumulated edits via changed()
   ;;   - 309: "Version Unknown Here" — server doesn't have our parents yet;
   ;;     retry after Retry-After delay (default 1s).  This happens when PUTs
   ;;     arrive out of order or the subscription hasn't caught up.
   ;;   - 503: server overloaded — mute for 3s then call changed() (retry-equivalent)
   ;;   - 550: permanent rejection (e.g., digest mismatch) — fatal, stop syncing
   ;;   - Other transient errors (408, 429, 500, 502, 504): retry via delayed changed()
   (lambda (_proc data)
     (let ((start 0)
           (matched nil)
           (should-send nil))
       (while (string-match "HTTP/1\\.[01] \\([0-9]+\\)" data start)
         (setq start (match-end 0))
         (setq matched t)
         (let ((code (string-to-number (match-string 1 data))))
           (cl-decf (braid-text-outstanding-changes bt))
           (let ((popped (pop (braid-text-unacked-puts bt))))
             (cond
              ;; 550: permanent rejection — fatal, stop syncing
              ((= code 550)
               (braid-text--fatal-error bt (format "PUT permanently rejected (HTTP 550)")))
              ;; 309: Version Unknown — re-queue and retry the same PUT bytes
              ((= code 309)
               (cl-incf (braid-text-outstanding-changes bt))
               (setf (braid-text-unacked-puts bt)
                     (append (braid-text-unacked-puts bt) (list popped)))
               (message "Braid: version unknown (309) — retrying in 1s")
               (run-with-timer 1.0 nil
                 (lambda ()
                   (let ((proc (braid-text-put-proc bt)))
                     (when (and proc (eq (process-status proc) 'open))
                       (process-send-string proc popped))))))
              ;; 503: server overloaded — enter mute mode
              ((= code 503)
               (message "Braid: server busy (503) — backing off for 3s")
               (setf (braid-text-muted-until bt) (+ (float-time) 3.0))
               (run-with-timer 3.0 nil (lambda () (braid-text--changed bt))))
              ;; Success — send throttled edits
              ((and (>= code 200) (< code 300))
               (setq should-send t))
              ;; 401/403: not authorized — undo the local edit and go read-only
              ((memq code '(401 403))
               (braid-text--not-authorized bt code))
              ;; 408: idle connection timeout (Node.js closes idle keep-alive
              ;; connections).  Not an error — just means we need to reopen the
              ;; pipelining connection on next PUT.  No retry needed.
              ((= code 408) nil)
              ;; Transient errors — retry via delayed changed()
              ((memq code '(429 500 502 504))
               (message "Braid: PUT error (HTTP %d) — retrying in 1s" code)
               (run-with-timer 1.0 nil (lambda () (braid-text--changed bt))))
              ;; Other errors — log and call changed() (don't silently lose the edit)
              (t
               (message "Braid: PUT rejected (HTTP %d)" code)
               (setq should-send t))))))
       (when matched
         (when (< (braid-text-outstanding-changes bt) 0)
           (setf (braid-text-outstanding-changes bt) 0))
         ;; Cancel PUT ACK timer when all PUTs are acked
         (when (and (<= (braid-text-outstanding-changes bt) 0)
                    (braid-text-put-ack-timer bt))
           (cancel-timer (braid-text-put-ack-timer bt))
           (setf (braid-text-put-ack-timer bt) nil))
         (force-mode-line-update)
         ;; Send throttled edits now that we have capacity
         (when should-send
           (braid-text--changed bt)))))
   ;; Sentinel: send queued requests on open; reconnect on unexpected close.
   (lambda (proc event)
     (cond
      ((string-prefix-p "open" event)
       (dolist (req (braid-text-unacked-puts bt))
         (process-send-string proc req)))
      ((or (string-prefix-p "finished" event)
           (string-prefix-p "deleted"  event))
       nil)  ; closed intentionally
      (t
       ;; Unexpected disconnect — restart PUT ACK timer for re-sent PUTs
       (when (braid-text-put-ack-timer bt)
         (cancel-timer (braid-text-put-ack-timer bt)))
       (when (> (braid-text-outstanding-changes bt) 0)
         (setf (braid-text-put-ack-timer bt)
               (run-with-timer braid-text-put-ack-timeout nil
                 (lambda () (when (> (braid-text-outstanding-changes bt) 0)
                              (braid-text--connection-dead bt))))))
       ;; Reconnect after a brief pause
       (run-with-timer 1.0 nil #'braid-text--put-proc-reconnect bt))))
   'nowait))

(defun braid-text--put-proc-reconnect (bt)
  "Reconnect BT's put-proc after a disconnect.
Unacked PUTs are preserved in the queue — the sentinel will re-send them
when the new socket opens.  Outstanding-changes count is NOT reset because
the unacked PUTs are still in flight (just on a new socket)."
  ;; Cancel stale PUT ACK timer; restart if unacked PUTs remain
  (when (braid-text-put-ack-timer bt)
    (cancel-timer (braid-text-put-ack-timer bt))
    (setf (braid-text-put-ack-timer bt) nil))
  (when (> (braid-text-outstanding-changes bt) 0)
    (setf (braid-text-put-ack-timer bt)
          (run-with-timer braid-text-put-ack-timeout nil
            (lambda () (when (> (braid-text-outstanding-changes bt) 0)
                         (braid-text--connection-dead bt))))))
  (condition-case err
      (setf (braid-text-put-proc bt) (braid-text--put-proc-open bt))
    (error
     (message "Braid: put-proc reconnect failed (%s) — retrying in 3s"
              (error-message-string err))
     (run-with-timer 3.0 nil #'braid-text--put-proc-reconnect bt))))

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
    (let* ((version       (plist-get msg :version))
           (parents       (sort (copy-sequence (plist-get msg :parents)) #'string<))
           (patches       (plist-get msg :patches))
           (body          (plist-get msg :body))
           (content-range (plist-get msg :content-range))
           (headers       (plist-get msg :headers)))
      (when braid-text-debug
        (message "Braid: recv version=%s parents=%s cur=%s match=%s patches=%d body=%s throttled=%s"
                 version parents (braid-text-client-version bt)
                 (equal parents (braid-text-client-version bt))
                 (length (or patches '()))
                 (if body (format "%dchars" (length body)) "nil")
                 (braid-text-throttled bt))
        (when patches
          (message "Braid: raw-patches-in-msg type=%s val=%S"
                   (type-of patches)
                   (mapcar (lambda (p)
                             (list (plist-get p :content-range)
                                   (let ((b (plist-get p :body)))
                                     (if (and b (> (length b) 30))
                                         (substring b 0 30) b))))
                           patches))))
      ;; Simpleton: only accept if parents == our client-version.
      ;; Check parents FIRST, then throttle (matches JS: the parent check
      ;; runs before the throttle check, so only matching updates are buffered).
      (when (equal parents (braid-text-client-version bt))
        ;; When throttled, the buffer is ahead of client-state and we can't
        ;; send (too many outstanding PUTs).  Applying server patches now
        ;; would corrupt the buffer because positions are relative to
        ;; client-state, not the actual buffer.  Buffer the update for later.
        (if (braid-text-throttled bt)
            (setf (braid-text-throttled-update bt) msg)
        (condition-case err
            (progn
              ;; Apply patches incrementally or set full body (initial connect).
              ;; The server may send patches in two wire formats:
              ;;   1. Patches: N block → :patches is a list, :content-range nil
              ;;   2. Inline single patch (no Patches: header) → :patches nil,
              ;;      :content-range set, :body is the patch content
              ;; When neither patches nor content-range is set, it's a snapshot.
              (cond
               (patches
                (braid-text--apply-patches bt patches))
               (content-range
                (braid-text--apply-patches
                 bt (list (list :content-range content-range :body body))))
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
              ;; On mismatch: CRASH HARD (D6).  A digest mismatch means the
              ;; document is already corrupted.  Continuing would compound it.
              ;; Matches JS: throw new Error('repr-digest mismatch')
              (when-let ((expected (cdr (assoc "repr-digest" headers))))
                (let ((actual (braid-text--repr-digest (braid-text-client-state bt))))
                  (unless (equal actual expected)
                    (braid-text--fatal-error
                     bt (format "DIGEST MISMATCH after version %s\n  expected: %s\n  actual:   %s"
                                version expected actual)))))
              ;; No changed() call here (D2): JS does NOT call changed()
              ;; after receiving a server update.  The PUT ACK handler
              ;; (in the put-proc filter) calls changed() for accumulated
              ;; edits.
              )
          (error
           (message "Braid: failed to apply update (version %s): %S" version err))))))))  ; condition-case, if, when, let*, if, defun



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
  "Replace BT's buffer content with TEXT, suppressing change hooks.
On first load (client-version nil), clears the undo list and makes the
buffer editable (it starts read-only until the first snapshot arrives)."
  (let ((buf (braid-text-buffer bt))
        (first-load (null (braid-text-client-version bt))))
    (braid-text--with-saved-points
     buf
     (lambda (saved)
       (with-current-buffer buf
         (let* ((new-len             (length text))
                (buffer-undo-list    t) ; suppress undo recording
                (inhibit-modification-hooks t)
                (inhibit-read-only t))
           (erase-buffer)
           (insert text)
           (set-buffer-modified-p nil)
           (when buffer-file-name (set-visited-file-modtime))
           ;; Clamp each saved point to the new buffer length.
           (mapcar (lambda (ws)
                     (cons (car ws) (min (cdr ws) new-len)))
                   saved)))))
    (setf (braid-text-client-state bt) text)
    ;; On first load: clear undo list and make buffer editable
    (when first-load
      (with-current-buffer buf
        (setq buffer-undo-list nil)
        (setq buffer-read-only nil)))))

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
  (when braid-text-debug
    (message "Braid: apply-patches count=%d client-state=%S"
             (length patches)
             (let ((s (braid-text-client-state bt)))
               (if (> (length s) 60) (substring s 0 60) s)))
    (dolist (p patches)
      (message "Braid:   raw-patch cr=%S body=%S"
               (plist-get p :content-range)
               (let ((b (plist-get p :body)))
                 (if (and b (> (length b) 40)) (substring b 0 40) b)))))
  (let ((buf (braid-text-buffer bt)))
    (braid-text--with-saved-points
     buf
     (lambda (saved)
       (with-current-buffer buf
         (let ((inhibit-modification-hooks t)
               (inhibit-read-only t)
               (offset 0))
           ;; Apply left-to-right, tracking cumulative offset from prior patches.
           (dolist (p patches)
             (let* ((cr      (plist-get p :content-range))
                    (start   (+ (nth 1 cr) offset))
                    (end     (+ (nth 2 cr) offset))
                    (content (plist-get p :body)))
               (when braid-text-debug
                 (message "Braid: apply-patch abs=[%d:%d] adj=[%d:%d] offset=%d content=%S buflen=%d"
                          (nth 1 cr) (nth 2 cr) start end offset
                          (if (> (length content) 40) (substring content 0 40) content)
                          (- (point-max) (point-min))))
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
