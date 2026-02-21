;;; braid-mode.el --- Minor mode for Braid-HTTP text sync -*- lexical-binding: t -*-

;; Author: Michael Toomim <toomim@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/braid-org/braid-emacs
;; Keywords: comm, tools

;;; Commentary:

;; A minor mode that keeps an Emacs buffer in sync with a braid-text
;; server resource using the simpleton merge algorithm.
;;
;; Use `braid-connect' to connect any buffer, or open files under
;; `~/http/' for automatic braidfs integration via `braid-live'.

;;; Code:

(require 'braid-text)


;;;; ======================================================================
;;;; Customisation
;;;; ======================================================================

(defgroup braid nil
  "Braid-HTTP collaborative editing."
  :group 'communication)

(defcustom braid-default-host "127.0.0.1"
  "Default host for `braid-connect'."
  :type 'string
  :group 'braid)

(defcustom braid-default-port 8888
  "Default port for `braid-connect'."
  :type 'integer
  :group 'braid)

(defcustom braid-reconnect-max-delay-foreground 3.0
  "Max reconnect backoff (seconds) for braid buffers displayed in the selected window."
  :type 'number
  :group 'braid)

(defcustom braid-reconnect-max-delay-background 120.0
  "Max reconnect backoff (seconds) for braid buffers not displayed in the selected window."
  :type 'number
  :group 'braid)


;;;; ======================================================================
;;;; Buffer-local state
;;;; ======================================================================

(defvar-local braid-mode--bt nil
  "The `braid-text' struct syncing this buffer, or nil if not connected.")

(defvar-local braid-mode--ever-connected nil
  "Non-nil once the subscription has received a successful 209 response.")

(defvar-local braid-mode--tls-fallback-tried nil
  "Non-nil if we already tried flipping TLS/plain to prevent infinite loops.")

(defvar-local braid-mode--saved-auto-save-name nil
  "Saved `buffer-auto-save-file-name' restored when `braid-mode' is disabled.")

(defvar-local braid-mode--saved-backup nil
  "Saved `make-backup-files' value restored when `braid-mode' is disabled.")

(defvar-local braid-mode--saved-create-lockfiles nil
  "Saved `create-lockfiles' value restored when `braid-mode' is disabled.")

(defvar-local braid-mode--saved-auto-revert nil
  "Non-nil if `auto-revert-mode' was active before `braid-mode' was enabled.")


;;;; ======================================================================
;;;; Minor mode
;;;; ======================================================================

(defvar-local braid-mode--saved-mode-line-modified nil
  "Saved `mode-line-modified' value restored when `braid-mode' is disabled.")

(defun braid-mode--on-window-selection-change (_frame)
  "Adjust reconnect-max-delay for braid buffers based on window focus.
Foreground buffers get a short cap; background buffers get a long one.
If a disconnected buffer is newly focused, expedite its reconnect."
  (dolist (buf (buffer-list))
    (when (buffer-local-value 'braid-mode--bt buf)
      (let* ((bt  (buffer-local-value 'braid-mode--bt buf))
             (sub (braid-text-sub bt))
             (focused (eq buf (window-buffer (selected-window))))
             (max-d (if focused
                        braid-reconnect-max-delay-foreground
                      braid-reconnect-max-delay-background)))
        (setf (braid-http-sub-reconnect-max-delay sub) max-d)
        (when focused
          (braid-http-expedite-reconnect sub))))))

(defun braid-mode--install-indicator ()
  "Replace `mode-line-modified' with the braid status indicator.
Shows ●● when connected and synced, ○○ when edits are pending ack,
and ** when disconnected."
  (setq braid-mode--saved-mode-line-modified mode-line-modified)
  (setq-local mode-line-modified
              '(:eval (cond
                       ((null braid-mode--bt) "○○")
                       ((not (eq (braid-http-sub-status (braid-text-sub braid-mode--bt))
                                 :connected))
                        "**")
                       ((> (braid-text-pending-puts braid-mode--bt) 0)
                        "○○")
                       (t "●●")))))

(defun braid-mode--remove-indicator ()
  "Restore the original `mode-line-modified'."
  (if braid-mode--saved-mode-line-modified
      (setq-local mode-line-modified braid-mode--saved-mode-line-modified)
    (kill-local-variable 'mode-line-modified)))

(defun braid-mode--suppress-supersession (orig-fn &rest args)
  "Suppress the supersession warning for braid-mode buffers.
braidfs writes synced content to disk asynchronously, so the file's
modtime may advance after braid-text has already applied the update
to the buffer.  This is expected and not a real conflict."
  (unless braid-mode
    (apply orig-fn args)))

(defun braid-mode--save-noop ()
  "No-op replacement for `save-buffer' in live braid-mode buffers."
  (interactive)
  (message "(No no; already saved to network!)"))

(defun braid-mode--after-change (_beg _end _old-len)
  "Push local buffer edits to the server."
  (when braid-mode--bt
    (braid-text-buffer-changed braid-mode--bt))
  ;; Always keep the buffer appearing unmodified — whether we sent a change
  ;; or not (handles cases like capitalize-word on already-capitalized text).
  (set-buffer-modified-p nil)
  ;; Update modtime so Emacs doesn't warn about the file changing on disk
  ;; (e.g. when braidfs writes the synced content to the underlying file).
  (when buffer-file-name (set-visited-file-modtime)))

(defun braid-mode--on-kill ()
  "Clean up braid connection when the buffer is killed."
  (when braid-mode--bt
    (braid-text-close braid-mode--bt)
    (setq braid-mode--bt nil))
  ;; Remove global hooks/advice if no other braid-mode buffers remain.
  (unless (cl-some (lambda (b) (and (not (eq b (current-buffer)))
                                    (buffer-local-value 'braid-mode b)))
                   (buffer-list))
    (advice-remove 'ask-user-about-supersession-threat
                   #'braid-mode--suppress-supersession)
    (remove-hook 'window-selection-change-functions
                 #'braid-mode--on-window-selection-change)))

;;;###autoload
(define-minor-mode braid-mode
  "Minor mode to sync the current buffer with a Braid-HTTP server.
Enable with `braid-connect'; disable to close the connection."
  :lighter nil
  :group 'braid
  (if braid-mode
      (progn
        (add-hook 'after-change-functions #'braid-mode--after-change nil t)
        (add-hook 'kill-buffer-hook #'braid-mode--on-kill nil t)
        ;; Install focus-change hook (global, shared by all braid buffers).
        (add-hook 'window-selection-change-functions
                  #'braid-mode--on-window-selection-change)
        ;; Insert indicator near the left of the mode line, after mode-line-modified.
        (braid-mode--install-indicator)
        ;; Suppress "file changed on disk" warnings — braidfs updates the
        ;; underlying file asynchronously, which is expected, not a conflict.
        (advice-add 'ask-user-about-supersession-threat
                    :around #'braid-mode--suppress-supersession)
        ;; Disable auto-revert — braidfs file writes would cause it to
        ;; revert the buffer, fighting with braid-text's live sync.
        (setq braid-mode--saved-auto-revert
              (and (bound-and-true-p auto-revert-mode) t))
        (when (bound-and-true-p auto-revert-mode)
          (auto-revert-mode -1))
        ;; Disable auto-save, backups, and manual saves for this buffer.
        (setq braid-mode--saved-auto-save-name buffer-auto-save-file-name)
        (setq buffer-auto-save-file-name nil)
        (make-local-variable 'make-backup-files)
        (setq braid-mode--saved-backup make-backup-files)
        (setq make-backup-files nil)
        (make-local-variable 'create-lockfiles)
        (setq braid-mode--saved-create-lockfiles create-lockfiles)
        (setq create-lockfiles nil)
        ;; Make C-x C-s a no-op — edits are synced live, not saved to disk.
        (local-set-key (kbd "C-x C-s") #'braid-mode--save-noop))
    (remove-hook 'after-change-functions #'braid-mode--after-change t)
    (remove-hook 'kill-buffer-hook #'braid-mode--on-kill t)
    ;; Remove indicator from mode line.
    (braid-mode--remove-indicator)
    ;; Remove global hooks/advice if no other braid-mode buffers remain.
    (unless (cl-some (lambda (b) (and (not (eq b (current-buffer)))
                                      (buffer-local-value 'braid-mode b)))
                     (buffer-list))
      (advice-remove 'ask-user-about-supersession-threat
                     #'braid-mode--suppress-supersession)
      (remove-hook 'window-selection-change-functions
                   #'braid-mode--on-window-selection-change))
    ;; Restore auto-revert if it was active before.
    (when braid-mode--saved-auto-revert
      (auto-revert-mode 1))
    ;; Restore auto-save, backup, lock-file, and save-buffer settings.
    (setq buffer-auto-save-file-name braid-mode--saved-auto-save-name)
    (setq make-backup-files braid-mode--saved-backup)
    (kill-local-variable 'make-backup-files)
    (setq create-lockfiles braid-mode--saved-create-lockfiles)
    (kill-local-variable 'create-lockfiles)
    (local-unset-key (kbd "C-x C-s"))
    (when braid-mode--bt
      (braid-text-close braid-mode--bt)
      (setq braid-mode--bt nil)
      (message "Braid: disconnected."))))


;;;; ======================================================================
;;;; braidfs path → URL parsing
;;;; ======================================================================

(defcustom braid-http-dir (expand-file-name "~/http/")
  "Root directory that braidfs maps to HTTP URLs.
Files under this directory are reachable as
  http://<first-component>/<rest-of-path>
where <first-component> may include a port, e.g. \"localhost:8888\"."
  :type 'directory
  :group 'braid)

(defun braid-live--parse-path (file)
  "Parse FILE (under `braid-http-dir') into (host port path).
Returns nil if FILE is not under `braid-http-dir', or if the file
is directly inside `braid-http-dir' (not in a subdirectory), or if
the first path component is a dotfile (e.g. .braidfs)."
  (let ((root (file-name-as-directory (expand-file-name braid-http-dir))))
    (when (string-prefix-p root file)
      (let* ((rel        (substring file (length root)))
             (slash      (string-match "/" rel)))
        (when (and slash (> slash 0) (not (string-prefix-p "." rel)))
          (let* ((domain     (substring rel 0 slash))
                 (path-rest  (substring rel slash))
                 (colon      (string-match ":" domain))
                 (host       (if colon (substring domain 0 colon) domain))
                 (tls        (not (member host '("localhost" "127.0.0.1" "::1"))))
                 (port       (cond (colon (string-to-number (substring domain (1+ colon))))
                                   (tls   443)
                                   (t     80))))
            (list host port path-rest tls)))))))

;;;###autoload
(defun braid-live ()
  "Toggle live sync for a buffer whose file lives under `braid-http-dir'.
Derives host, port, and path from the file path following the braidfs
convention: ~/http/<host>:<port>/<path>  →  http://<host>:<port>/<path>.
Calls `braid-connect' to start syncing, or disables `braid-mode' if
already connected."
  (interactive)
  (if braid-mode
      (braid-mode -1)
    (let* ((file   (buffer-file-name))
           (parsed (and file (braid-live--parse-path file))))
      (if parsed
          (apply #'braid-connect parsed)
        (user-error "Buffer file is not under %s" braid-http-dir)))))


;;;; ======================================================================
;;;; Interactive entry point
;;;; ======================================================================

(defun braid-mode--on-connect ()
  "Called when the subscription receives a successful 209 response."
  (setq braid-mode--ever-connected t))

(defun braid-mode--on-disconnect ()
  "Called on unexpected disconnect.  Tries TLS fallback if never connected."
  (when (and (not braid-mode--ever-connected)
             (not braid-mode--tls-fallback-tried)
             braid-mode--bt)
    (setq braid-mode--tls-fallback-tried t)
    (let* ((bt      braid-mode--bt)
           (host    (braid-text-host bt))
           (port    (braid-text-port bt))
           (old-tls (braid-text-tls bt))
           (new-tls (not old-tls))
           (new-port (cond
                      ;; Flip default ports: 80 ↔ 443
                      ((and old-tls (= port 443)) 80)
                      ((and (not old-tls) (= port 80)) 443)
                      (t port)))
           (path    (braid-text-path bt))
           (buf     (current-buffer)))
      (message "Braid: %s connection failed, trying %s on port %d"
               (if old-tls "TLS" "plain")
               (if new-tls "TLS" "plain")
               new-port)
      ;; Can't create processes inside a sentinel, so schedule for next event loop.
      (run-with-timer 0.1 nil
                      (lambda ()
                        (when (buffer-live-p buf)
                          (with-current-buffer buf
                            (braid-text-close bt)
                            (setq braid-mode--bt nil)
                            (setq braid-mode--ever-connected nil)
                            (setq braid-mode--bt
                                  (braid-text-open host new-port path buf
                                                   :tls new-tls
                                                   :on-connect
                                                   (lambda ()
                                                     (with-current-buffer buf
                                                       (braid-mode--on-connect)))
                                                   :on-disconnect
                                                   (lambda ()
                                                     (with-current-buffer buf
                                                       (braid-mode--on-disconnect))))))))))))

;;;###autoload
(defun braid-connect (host port path &optional tls)
  "Connect the current buffer to a braid-text resource at HOST:PORT/PATH.
If TLS is non-nil, use a TLS connection.
Enables `braid-mode' and begins syncing.  The server's current content
is applied to the buffer once the subscription is established."
  (interactive
   (list (read-string (format "Host (default %s): " braid-default-host)
                      nil nil braid-default-host)
         (read-number "Port: " braid-default-port)
         (read-string "Path: " (concat "/text/" (buffer-name)))
         nil))
  ;; Close any existing connection first
  (when braid-mode--bt
    (braid-text-close braid-mode--bt)
    (setq braid-mode--bt nil))
  ;; Reset fallback state
  (setq braid-mode--ever-connected nil)
  (setq braid-mode--tls-fallback-tried nil)
  (braid-mode 1)
  (let ((buf (current-buffer)))
    (setq braid-mode--bt (braid-text-open host port path buf
                                          :tls tls
                                          :on-connect
                                          (lambda ()
                                            (with-current-buffer buf
                                              (braid-mode--on-connect)))
                                          :on-disconnect
                                          (lambda ()
                                            (with-current-buffer buf
                                              (braid-mode--on-disconnect)))))
    ;; Set initial reconnect cap based on whether this buffer is focused.
    (let* ((focused (eq buf (window-buffer (selected-window))))
           (max-d   (if focused
                        braid-reconnect-max-delay-foreground
                      braid-reconnect-max-delay-background)))
      (setf (braid-http-sub-reconnect-max-delay (braid-text-sub braid-mode--bt))
            max-d)))
  (message "Braid: connecting to %s://%s%s%s" (if tls "https" "http") host
           (if (or (and tls (= port 443)) (and (not tls) (= port 80)))
               ""
             (format ":%d" port))
           path))


;;;; ======================================================================
;;;; Auto-enable for ~/http/ files
;;;; ======================================================================

(defun braid-mode--maybe-auto-live ()
  "Enable `braid-live' automatically for files under `braid-http-dir'."
  (when (and buffer-file-name
             (braid-live--parse-path buffer-file-name))
    (braid-live)))

;;;###autoload
(defun braid-mode-auto-live-setup ()
  "Enable automatic `braid-live' for files opened under `braid-http-dir'.
Call this in your init file, or use `braid-mode-auto-live' custom variable."
  (add-hook 'find-file-hook #'braid-mode--maybe-auto-live))

(defcustom braid-mode-auto-live t
  "If non-nil, automatically enable `braid-live' for files under `braid-http-dir'."
  :type 'boolean
  :group 'braid
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (add-hook 'find-file-hook #'braid-mode--maybe-auto-live)
           (remove-hook 'find-file-hook #'braid-mode--maybe-auto-live))))

;; Activate immediately when this file is loaded, if the option is set.
(when braid-mode-auto-live
  (add-hook 'find-file-hook #'braid-mode--maybe-auto-live))


(provide 'braid-mode)
;;; braid-mode.el ends here
