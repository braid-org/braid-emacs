;;; braid-cursors.el --- Cursor/selection sharing for Braid -*- lexical-binding: t -*-

;; Author: Michael Toomim <toomim@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/braid-org/braid-emacs
;; Keywords: comm, tools

;;; Commentary:

;; Shares cursor positions and text selections with remote peers over
;; Braid-HTTP using the `application/text-cursors+json' content type.
;; Remote cursors appear as colored overlays in the buffer.

;;; Code:

(require 'cl-lib)
(require 'braid-http)
(require 'braid-text)
(require 'json)


;;;; ======================================================================
;;;; Customisation
;;;; ======================================================================

(defcustom braid-cursor-colors
  ["#e06c75" "#61afef" "#98c379" "#c678dd" "#e5c07b" "#56b6c2"]
  "Color palette for remote peer cursors and selections."
  :type '(vector string)
  :group 'braid)


;;;; ======================================================================
;;;; Struct
;;;; ======================================================================

(cl-defstruct braid-cursor
  "State for cursor/selection sharing on a braid-text connection."
  bt              ; parent braid-text struct (for host/port/peer/buffer)
  sub             ; braid-http-sub for cursor subscription
  put-proc        ; persistent TCP/TLS connection for cursor PUTs
  put-queue       ; queued PUT bytes while put-proc is connecting
  last-sent       ; last sent (from . to) to avoid redundant PUTs
  send-timer      ; idle timer for throttled sends
  hook-fn         ; stored lambda for post-command-hook removal
  (remote nil))   ; hash table: peer-id → plist (:selections :cursor-ovs :sel-ovs)


;;;; ======================================================================
;;;; Feature detection
;;;; ======================================================================

(defun braid-cursors--check-support (host port path tls)
  "Check if server supports cursors for PATH via HEAD request.
Returns non-nil if the server responds 200 with Content-Type
containing application/text-cursors+json."
  (condition-case nil
      (let* ((proc (open-network-stream
                    "braid-cursor-head" " *braid-cursor-head*"
                    host port
                    :type (if tls 'tls 'plain)))
             (request (concat (format "HEAD %s HTTP/1.1\r\n" path)
                              (format "Host: %s\r\n"
                                      (braid-http--format-host host port))
                              "Accept: application/text-cursors+json\r\n"
                              "Connection: close\r\n"
                              "\r\n"))
             (response ""))
        (process-send-string proc request)
        (while (accept-process-output proc 5))
        (setq response (with-current-buffer (process-buffer proc)
                         (buffer-string)))
        (delete-process proc)
        (kill-buffer (process-buffer proc))
        (and (string-match-p "HTTP/[0-9.]+ 200" response)
             (let ((case-fold-search t))
               (string-match-p "content-type:.*application/text-cursors\\+json"
                               response))))
    (error nil)))


;;;; ======================================================================
;;;; Public API
;;;; ======================================================================

(defun braid-cursors-open (bt)
  "Start cursor sharing for a braid-text connection BT.
Returns a `braid-cursor' struct, or nil if the server does not support cursors."
  (if (not (braid-cursors--check-support
            (braid-text-host bt) (braid-text-port bt)
            (braid-text-path bt) (braid-text-tls bt)))
      (progn
        (message "Braid cursors: server does not support cursors for %s"
                 (braid-text-path bt))
        nil)
  (let* ((bc (make-braid-cursor
              :bt bt
              :remote (make-hash-table :test 'equal)
              :put-queue "")))
    ;; Open persistent PUT connection
    (setf (braid-cursor-put-proc bc) (braid-cursors--put-proc-open bc))
    ;; Subscribe for cursor updates
    (setf (braid-cursor-sub bc)
          (braid-http-subscribe
           (braid-text-host bt) (braid-text-port bt) (braid-text-path bt)
           (lambda (msg) (braid-cursors--on-update bc msg))
           :peer (braid-text-peer bt)
           :tls (braid-text-tls bt)
           :extra-headers '(("Accept" . "application/text-cursors+json")
                             ("Heartbeats" . "10"))
           :on-connect (lambda ()
                         (let ((buf (braid-text-buffer (braid-cursor-bt bc))))
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (braid-cursors--force-send bc)))))
           :on-disconnect (lambda () (braid-cursors--clear-all bc))))
    ;; Install post-command-hook for sending local cursor
    (let ((fn (lambda () (braid-cursors--maybe-send bc))))
      (setf (braid-cursor-hook-fn bc) fn)
      (with-current-buffer (braid-text-buffer bt)
        (add-hook 'post-command-hook fn nil t)))
    bc)))

(defun braid-cursors--clear-all (bc)
  "Remove all remote cursor/selection overlays and clear state."
  (let ((buf (braid-text-buffer (braid-cursor-bt bc))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (maphash (lambda (_peer-id entry)
                   (dolist (ov (plist-get entry :cursor-ovs))
                     (when (overlayp ov) (delete-overlay ov)))
                   (dolist (ov (plist-get entry :sel-ovs))
                     (when (overlayp ov) (delete-overlay ov))))
                 (braid-cursor-remote bc)))
      (clrhash (braid-cursor-remote bc)))))

(defun braid-cursors-close (bc)
  "Stop cursor sharing for BC."
  (when bc
    (let ((buf (braid-text-buffer (braid-cursor-bt bc))))
      ;; Remove post-command-hook
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (braid-cursor-hook-fn bc)
            (remove-hook 'post-command-hook (braid-cursor-hook-fn bc) t))))
      ;; Cancel send timer
      (when (braid-cursor-send-timer bc)
        (cancel-timer (braid-cursor-send-timer bc))
        (setf (braid-cursor-send-timer bc) nil))
      ;; Unsubscribe cursor subscription
      (when (braid-cursor-sub bc)
        (braid-http-unsubscribe (braid-cursor-sub bc))
        (setf (braid-cursor-sub bc) nil))
      ;; Kill PUT connection
      (when-let ((p (braid-cursor-put-proc bc)))
        (when (process-live-p p) (delete-process p))
        (setf (braid-cursor-put-proc bc) nil))
      ;; Remove all remote cursor/selection overlays
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (maphash (lambda (_peer-id entry)
                     (dolist (ov (plist-get entry :cursor-ovs))
                       (when (overlayp ov) (delete-overlay ov)))
                     (dolist (ov (plist-get entry :sel-ovs))
                       (when (overlayp ov) (delete-overlay ov))))
                   (braid-cursor-remote bc)))))))


;;;; ======================================================================
;;;; PUT connection (private)
;;;; ======================================================================

(defun braid-cursors--put-proc-open (bc)
  "Open a persistent TCP/TLS connection for cursor PUTs."
  (let ((bt (braid-cursor-bt bc)))
    (braid-http--make-process
     (format "braid-cursor-put:%s:%d%s"
             (braid-text-host bt) (braid-text-port bt) (braid-text-path bt))
     (braid-text-host bt)
     (braid-text-port bt)
     (braid-text-tls bt)
     ;; Filter: ignore responses (fire-and-forget)
     (lambda (_proc _data) nil)
     ;; Sentinel: flush queue on open, reconnect on unexpected close
     (lambda (proc event)
       (cond
        ((string-prefix-p "open" event)
         (let ((queued (braid-cursor-put-queue bc)))
           (setf (braid-cursor-put-queue bc) "")
           (unless (string-empty-p queued)
             (process-send-string proc queued))))
        ((or (string-prefix-p "finished" event)
             (string-prefix-p "deleted" event))
         nil)
        (t
         (run-with-timer 1.0 nil #'braid-cursors--put-proc-reconnect bc))))
     'nowait)))

(defun braid-cursors--put-proc-reconnect (bc)
  "Reconnect BC's put-proc."
  (setf (braid-cursor-put-proc bc) (braid-cursors--put-proc-open bc)))


;;;; ======================================================================
;;;; Local cursor transform (public)
;;;; ======================================================================

(defun braid-cursors--transform-pos (pos del-start del-len ins-len)
  "Transform position POS through an edit at DEL-START."
  (cond
   ((= del-len 0)
    (if (< pos del-start) pos (+ pos ins-len)))
   ((<= pos del-start) pos)
   ((<= pos (+ del-start del-len)) (+ del-start ins-len))
   (t (+ pos (- ins-len del-len)))))

(defun braid-cursors-changed (bc beg end old-len)
  "Transform all stored remote cursor positions through a buffer edit.
BEG and END are buffer positions (1-indexed) after the change.
OLD-LEN is the length of the deleted text.
Call this from `after-change-functions'."
  (when (and bc (braid-cursor-remote bc))
    (let ((del-start (1- beg))
          (del-len old-len)
          (ins-len (- end beg))
          (buf (braid-text-buffer (braid-cursor-bt bc))))
      (when (buffer-live-p buf)
        (maphash
         (lambda (_peer-id entry)
           (plist-put entry :selections
                      (mapcar (lambda (sel)
                                (cons (braid-cursors--transform-pos
                                       (car sel) del-start del-len ins-len)
                                      (braid-cursors--transform-pos
                                       (cdr sel) del-start del-len ins-len)))
                              (plist-get entry :selections))))
         (braid-cursor-remote bc))
        (braid-cursors--render bc)))))


;;;; ======================================================================
;;;; Receiving remote cursors (private)
;;;; ======================================================================

(defun braid-cursors--on-update (bc msg)
  "Handle an incoming cursor subscription message."
  (let* ((bt (braid-cursor-bt bc))
         (buf (braid-text-buffer bt))
         (body-text (plist-get msg :body))
         (patches (plist-get msg :patches)))
    (when (buffer-live-p buf)
      (condition-case err
          (let ((peers (make-hash-table :test 'equal)))
            ;; Body = full snapshot (initial update)
            (when body-text
              (let ((json (json-parse-string body-text :object-type 'hash-table)))
                (maphash (lambda (k v) (puthash k v peers)) json)))
            ;; Patches = per-peer updates
            (dolist (p patches)
              (let* ((cr (plist-get p :content-range))
                     (peer-id (and cr (equal (car cr) "json") (cadr cr)))
                     (p-body (plist-get p :body)))
                (when peer-id
                  (if (or (null p-body) (string-empty-p p-body))
                      ;; Empty content = peer disconnected
                      (puthash peer-id :disconnected peers)
                    (puthash peer-id
                             (json-parse-string p-body :object-type 'hash-table)
                             peers)))))
            ;; Apply changes
            (maphash
             (lambda (peer-id value)
               (unless (equal peer-id (braid-text-peer bt))
                 (let ((entry (or (gethash peer-id (braid-cursor-remote bc))
                                  (list :selections nil :cursor-ovs nil :sel-ovs nil))))
                   (if (eq value :disconnected)
                       (progn
                         (dolist (ov (plist-get entry :cursor-ovs))
                           (when (overlayp ov) (delete-overlay ov)))
                         (dolist (ov (plist-get entry :sel-ovs))
                           (when (overlayp ov) (delete-overlay ov)))
                         (remhash peer-id (braid-cursor-remote bc)))
                     (let ((sel-list (braid-cursors--parse-selections value buf)))
                       (plist-put entry :selections sel-list)
                       (puthash peer-id entry (braid-cursor-remote bc)))))))
             peers)
            (braid-cursors--render bc))
        (error
         (message "Braid cursors: parse error: %S" err))))))

(defun braid-cursors--parse-selections (selections buf)
  "Parse SELECTIONS (a vector of {from, to} objects) into a list of (from . to) conses.
Clamp positions to buffer length."
  (let ((max-pos (with-current-buffer buf
                   (- (point-max) (point-min))))
        result)
    (if (vectorp selections)
        (dotimes (i (length selections))
          (let* ((sel (aref selections i))
                 (from (min (gethash "from" sel 0) max-pos))
                 (to (min (gethash "to" sel 0) max-pos)))
            (push (cons from to) result)))
      ;; Also handle a single object (not wrapped in array)
      (when (hash-table-p selections)
        (let ((from (min (gethash "from" selections 0) max-pos))
              (to (min (gethash "to" selections 0) max-pos)))
          (push (cons from to) result))))
    (nreverse result)))


;;;; ======================================================================
;;;; Overlay rendering (private)
;;;; ======================================================================

(defun braid-cursors--render (bc)
  "Render all remote cursors/selections as overlays in the buffer."
  (let* ((bt (braid-cursor-bt bc))
         (buf (braid-text-buffer bt)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (maphash
         (lambda (peer-id entry)
           ;; Delete existing overlays
           (dolist (ov (plist-get entry :cursor-ovs))
             (when (overlayp ov) (delete-overlay ov)))
           (dolist (ov (plist-get entry :sel-ovs))
             (when (overlayp ov) (delete-overlay ov)))
           (let ((color (braid-cursors--color-for-peer peer-id))
                 (bg-color (braid-cursors--bg-color-for-peer peer-id))
                 (cursor-ovs nil)
                 (sel-ovs nil)
                 (base (point-min)))
             ;; Create new overlays for each selection
             (dolist (sel (plist-get entry :selections))
               (let ((from (car sel))
                     (to (cdr sel)))
                 (if (= from to)
                     ;; Cursor: block cursor (colored background on one char)
                     (let* ((pos (+ base from)))
                       (if (< pos (point-max))
                           (let ((ov (make-overlay pos (1+ pos) buf t nil)))
                             (overlay-put ov 'face
                                          `(:background ,color :foreground "white"))
                             (overlay-put ov 'braid-cursor-peer peer-id)
                             (push ov cursor-ovs))
                         ;; End of buffer: after-string block
                         (let ((ov (make-overlay pos pos buf t nil)))
                           (overlay-put ov 'after-string
                                        (propertize " "
                                                    'face `(:background ,color)
                                                    'cursor 0))
                           (overlay-put ov 'braid-cursor-peer peer-id)
                           (push ov cursor-ovs))))
                   ;; Selection (range)
                   (let* ((f (+ base (min from to)))
                          (t_ (+ base (max from to)))
                          (ov (make-overlay f t_ buf t nil)))
                     (overlay-put ov 'face `(:background ,bg-color))
                     (overlay-put ov 'braid-cursor-peer peer-id)
                     (push ov sel-ovs)))))
             (plist-put entry :cursor-ovs cursor-ovs)
             (plist-put entry :sel-ovs sel-ovs)))
         (braid-cursor-remote bc))))))

(defun braid-cursors--color-for-peer (peer-id)
  "Return a foreground color for PEER-ID from the palette."
  (aref braid-cursor-colors
        (mod (sxhash peer-id) (length braid-cursor-colors))))

(defun braid-cursors--bg-color-for-peer (peer-id)
  "Return a background color (lighter) for PEER-ID from the palette."
  (let ((color (braid-cursors--color-for-peer peer-id)))
    ;; Add transparency by mixing with white
    (braid-cursors--lighten-color color 0.3)))

(defun braid-cursors--lighten-color (hex-color alpha)
  "Lighten HEX-COLOR by mixing with white at ALPHA opacity.
ALPHA 0.0 = white, 1.0 = original color."
  (let* ((r (string-to-number (substring hex-color 1 3) 16))
         (g (string-to-number (substring hex-color 3 5) 16))
         (b (string-to-number (substring hex-color 5 7) 16))
         (lr (round (+ (* r alpha) (* 255 (- 1 alpha)))))
         (lg (round (+ (* g alpha) (* 255 (- 1 alpha)))))
         (lb (round (+ (* b alpha) (* 255 (- 1 alpha))))))
    (format "#%02x%02x%02x" lr lg lb)))


;;;; ======================================================================
;;;; Sending local cursor (private)
;;;; ======================================================================

(defun braid-cursors--maybe-send (bc)
  "Called from `post-command-hook'.  Schedule a cursor PUT after idle delay."
  (when (and bc (braid-cursor-bt bc))
    (let* ((from (1- (point)))
           (to (if (use-region-p) (1- (mark)) from))
           (pair (cons (min from to) (max from to))))
      (unless (equal pair (braid-cursor-last-sent bc))
        (braid-cursors--schedule-send bc pair)))))

(defun braid-cursors--force-send (bc)
  "Force re-send of current cursor position (e.g. after incoming edits)."
  (when (and bc (braid-cursor-bt bc))
    (let* ((from (1- (point)))
           (to (if (use-region-p) (1- (mark)) from))
           (pair (cons (min from to) (max from to))))
      (setf (braid-cursor-last-sent bc) nil)  ; clear dedup
      (braid-cursors--schedule-send bc pair))))

(defun braid-cursors--schedule-send (bc pair)
  "Schedule a cursor PUT of PAIR after idle delay."
  (when (braid-cursor-send-timer bc)
    (cancel-timer (braid-cursor-send-timer bc)))
  (setf (braid-cursor-send-timer bc)
        (run-with-idle-timer
         0.05 nil
         (lambda ()
           (when (braid-cursor-bt bc)
             (braid-cursors--do-send bc pair))))))

(defun braid-cursors--do-send (bc pair)
  "Send cursor position PAIR as (from . to) via PUT."
  (let* ((bt (braid-cursor-bt bc))
         (from (car pair))
         (to (cdr pair))
         (body (format "[{\"from\":%d,\"to\":%d}]" from to))
         (body-bytes (encode-coding-string body 'utf-8))
         (peer (braid-text-peer bt))
         (request (concat (format "PUT %s HTTP/1.1\r\n" (braid-text-path bt))
                          (format "Host: %s\r\n"
                                  (braid-http--format-host
                                   (braid-text-host bt) (braid-text-port bt)))
                          (format "Content-Type: application/text-cursors+json\r\n")
                          (format "Content-Range: json [\"%s\"]\r\n" peer)
                          (format "Peer: %s\r\n" peer)
                          (format "Content-Length: %d\r\n" (length body-bytes))
                          "\r\n"
                          body))
         (proc (braid-cursor-put-proc bc)))
    (if (and proc (process-live-p proc))
        (process-send-string proc request)
      ;; Queue and reconnect
      (setf (braid-cursor-put-queue bc)
            (concat (braid-cursor-put-queue bc) request))
      (when proc (braid-cursors--put-proc-reconnect bc)))
    (setf (braid-cursor-last-sent bc) pair)))


(provide 'braid-cursors)
;;; braid-cursors.el ends here
