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
;; Open URLs directly with find-file (C-x C-f http://host/path).

;;; Code:

(require 'braid-text)
(require 'braid-cursors)
(eval-when-compile (require 'url-parse))

;; Forward-declare the minor-mode variable (defined by define-minor-mode below)
(defvar braid-mode)


;;;; ======================================================================
;;;; Customisation
;;;; ======================================================================

(defgroup braid nil
  "Braid-HTTP collaborative editing."
  :group 'communication)

(defcustom braid-reconnect-max-delay-foreground 3.0
  "Max reconnect backoff for foreground braid buffers, in seconds."
  :type 'number
  :group 'braid)

(defcustom braid-reconnect-max-delay-background 120.0
  "Max reconnect backoff for background braid buffers, in seconds."
  :type 'number
  :group 'braid)


;;;; ======================================================================
;;;; Buffer-local state
;;;; ======================================================================

(defvar-local braid-mode--bt nil
  "The `braid-text' struct syncing this buffer, or nil if not connected.")
(put 'braid-mode--bt 'permanent-local t)

(defvar-local braid-mode--bc nil
  "The `braid-cursor' struct for cursor sharing, or nil.")
(put 'braid-mode--bc 'permanent-local t)

(defvar-local braid-mode--ever-connected nil
  "Non-nil once the subscription has received a successful 209 response.")
(put 'braid-mode--ever-connected 'permanent-local t)

(defvar-local braid-mode--tls-fallback-tried nil
  "Non-nil if we already tried flipping TLS/plain to prevent infinite loops.")
(put 'braid-mode--tls-fallback-tried 'permanent-local t)

(defvar-local braid-mode--saved-auto-save-name nil
  "Saved `buffer-auto-save-file-name' restored when `braid-mode' is disabled.")
(put 'braid-mode--saved-auto-save-name 'permanent-local t)

(defvar-local braid-mode--saved-backup nil
  "Saved `make-backup-files' value restored when `braid-mode' is disabled.")
(put 'braid-mode--saved-backup 'permanent-local t)

(defvar-local braid-mode--saved-create-lockfiles nil
  "Saved `create-lockfiles' value restored when `braid-mode' is disabled.")
(put 'braid-mode--saved-create-lockfiles 'permanent-local t)

(defvar-local braid-mode--saved-auto-revert nil
  "Non-nil if `auto-revert-mode' was active before `braid-mode' was enabled.")
(put 'braid-mode--saved-auto-revert 'permanent-local t)


;;;; ======================================================================
;;;; Minor mode
;;;; ======================================================================

(defvar-local braid-mode--saved-mode-line-modified nil
  "Saved `mode-line-modified' value restored when `braid-mode' is disabled.")
(put 'braid-mode--saved-mode-line-modified 'permanent-local t)

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
                       ((> (braid-text-outstanding-changes braid-mode--bt) 0)
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

(defun braid-mode--after-change (beg end old-len)
  "Push local buffer edits to the server."
  (when braid-mode--bt
    (braid-text-buffer-changed braid-mode--bt beg end old-len))
  ;; Cursor transforms are handled by the :on-edit hook in braid-text,
  ;; which fires for both local and remote edits.
  ;; Always keep the buffer appearing unmodified — whether we sent a change
  ;; or not (handles cases like capitalize-word on already-capitalized text).
  (set-buffer-modified-p nil)
  ;; Update modtime so Emacs doesn't warn about the file changing on disk
  ;; (e.g. when braidfs writes the synced content to the underlying file).
  (when (and buffer-file-name
             (not (string-match-p "\\`https?://" buffer-file-name)))
    (set-visited-file-modtime)))

(defun braid-mode--on-kill ()
  "Clean up braid connection when the buffer is killed."
  (when braid-mode--bc
    (braid-cursors-close braid-mode--bc)
    (setq braid-mode--bc nil))
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

(defun braid-mode--install-hooks ()
  "Install buffer-local hooks needed by braid-mode.
Called on enable and after major-mode changes (which kill buffer-local hooks)."
  (add-hook 'after-change-functions #'braid-mode--after-change nil t)
  (add-hook 'kill-buffer-hook #'braid-mode--on-kill nil t))

(defun braid-mode--after-major-mode-change ()
  "Re-install braid-mode setup after a major-mode change.
Major modes call `kill-all-local-variables' which strips buffer-local hooks.
Our state variables survive (they are permanent-local), but the hooks and
mode-line indicator need to be re-installed."
  (when braid-mode
    (braid-mode--install-hooks)
    (braid-mode--install-indicator)
    ;; Re-suppress auto-save, backups, lockfiles
    (setq buffer-auto-save-file-name nil)
    (make-local-variable 'make-backup-files)
    (setq make-backup-files nil)
    (make-local-variable 'create-lockfiles)
    (setq create-lockfiles nil)
    (local-set-key (kbd "C-x C-s") #'braid-mode--save-noop)))

;; This hook runs after every major-mode change in every buffer.
;; The body checks braid-mode first, so it's a no-op for non-braid buffers.
(add-hook 'after-change-major-mode-hook #'braid-mode--after-major-mode-change)

;;;###autoload
(define-minor-mode braid-mode
  "Minor mode to sync the current buffer with a Braid-HTTP server.
Enable with `braid-connect'; disable to close the connection."
  :lighter nil
  :group 'braid
  (if braid-mode
      (progn
        (braid-mode--install-hooks)
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
    (when braid-mode--bc
      (braid-cursors-close braid-mode--bc)
      (setq braid-mode--bc nil))
    (when braid-mode--bt
      (braid-text-close braid-mode--bt)
      (setq braid-mode--bt nil)
      (message "Braid: disconnected."))))

;; Survive major-mode changes (kill-all-local-variables).
(put 'braid-mode 'permanent-local t)


;;;; ======================================================================
;;;; URL parsing
;;;; ======================================================================

(defun braid-mode--parse-url (url)
  "Parse URL into (host port path tls)."
  (require 'url-parse)
  (let* ((obj (url-generic-parse-url url))
         (tls (equal (url-type obj) "https"))
         (host (url-host obj))
         (port (url-port obj))
         (path (let ((f (url-filename obj)))
                 (if (or (null f) (string-empty-p f)) "/" f))))
    (list host port path tls)))


;;;; ======================================================================
;;;; Interactive entry point
;;;; ======================================================================

(defun braid-mode--on-connect ()
  "Called when the subscription receives a successful 209 response."
  (setq braid-mode--ever-connected t)
  ;; Retry cursor setup if it failed earlier (e.g. server was not running).
  (when (and (not braid-mode--bc) braid-mode--bt)
    (setq braid-mode--bc (braid-cursors-open braid-mode--bt))))

(defun braid-mode--on-disconnect ()
  "Called on unexpected disconnect.  Tries TLS fallback if never connected.
Only attempts TLS fallback if the TCP connection was actually established
\(status progressed past :connecting), indicating a protocol mismatch
rather than a server that is simply not running."
  (when (and (not braid-mode--ever-connected)
             (not braid-mode--tls-fallback-tried)
             braid-mode--bt
             ;; Only try TLS fallback if TCP connected but HTTP failed.
             ;; If status is still :connecting, the server is just unreachable
             ;; and the normal reconnect loop will handle it.
             (not (eq (braid-http-sub-status (braid-text-sub braid-mode--bt))
                      :connecting)))
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
                            (when braid-mode--bc
                              (braid-cursors-close braid-mode--bc)
                              (setq braid-mode--bc nil))
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
                                                       (braid-mode--on-disconnect)))
                                                   :on-edit
                                                   (lambda (patches)
                                                     (when (buffer-live-p buf)
                                                       (with-current-buffer buf
                                                         (when braid-mode--bc
                                                           (braid-cursors-on-edit braid-mode--bc patches)))))))
                            (setq braid-mode--bc (braid-cursors-open braid-mode--bt)))))))))

(defun braid-connect (url)
  "Connect the current buffer to the braid-text resource at URL.
URL is an http:// or https:// URL string."
  (pcase-let ((`(,host ,port ,path ,tls) (braid-mode--parse-url url)))
    ;; Close any existing connection first
    (when braid-mode--bc
      (braid-cursors-close braid-mode--bc)
      (setq braid-mode--bc nil))
    (when braid-mode--bt
      (braid-text-close braid-mode--bt)
      (setq braid-mode--bt nil))
    ;; Reset fallback state
    (setq braid-mode--ever-connected nil)
    (setq braid-mode--tls-fallback-tried nil)
    (braid-mode 1)
    ;; Start read-only until first snapshot arrives (braid-text--set-buffer-text clears it)
    (setq buffer-read-only t)
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
                                                (braid-mode--on-disconnect)))
                                            :on-edit
                                            (lambda (patches)
                                              (when (buffer-live-p buf)
                                                (with-current-buffer buf
                                                  (when braid-mode--bc
                                                    (braid-cursors-on-edit braid-mode--bc patches)))))))
      ;; Start cursor sharing
      (setq braid-mode--bc (braid-cursors-open braid-mode--bt))
      ;; Set initial reconnect cap based on whether this buffer is focused.
      (let* ((focused (eq buf (window-buffer (selected-window))))
             (max-d   (if focused
                          braid-reconnect-max-delay-foreground
                        braid-reconnect-max-delay-background)))
        (setf (braid-http-sub-reconnect-max-delay (braid-text-sub braid-mode--bt))
              max-d)))
    (message "Braid: connecting to %s" url)))


;;;; ======================================================================
;;;; URL file-name-handler
;;;; ======================================================================

(defconst braid-mode--url-regexp "\\`https?://"
  "Regexp matching HTTP/HTTPS URLs for file-name-handler-alist.")

(defun braid-mode--file-handler (operation &rest args)
  "Handle file operations on http:// and https:// URLs."
  (pcase operation
    ('expand-file-name
     (let ((name (car args))
           (dir (nth 1 args)))
       (cond
        ;; URL name — return as-is
        ((string-match-p "\\`https?://" name) name)
        ;; Absolute local path with URL dir — expand normally, ignore URL
        ((and dir (string-match-p "\\`https?://" dir)
              (or (string-prefix-p "/" name) (string-prefix-p "~" name)))
         (let ((inhibit-file-name-handlers
                (cons 'braid-mode--file-handler inhibit-file-name-handlers))
               (inhibit-file-name-operation 'expand-file-name))
           (expand-file-name name)))
        ;; Relative path with URL dir — expand against URL directory
        ((and dir (string-match-p "\\`https?://" dir))
         (concat (file-name-as-directory dir) name))
        ;; Non-URL dir — normal expansion
        (t
         (let ((inhibit-file-name-handlers
                (cons 'braid-mode--file-handler inhibit-file-name-handlers))
               (inhibit-file-name-operation 'expand-file-name))
           (expand-file-name name dir))))))
    ('file-truename (car args))
    ('file-name-directory
     (let ((url (car args)))
       (if (string-match "\\(https?://[^/]+/\\).*/" url)
           (match-string 0 url)
         (and (string-match "\\(https?://[^/]+\\)" url)
              (concat (match-string 1 url) "/")))))
    ('file-name-nondirectory
     (if (string-match "/\\([^/]*\\)\\'" (car args))
         (match-string 1 (car args))
       ""))
    ('insert-file-contents
     (when (nth 1 args)  ; visit flag
       (setq buffer-file-name (car args)))
     (list (car args) 0))
    ('write-region (list (nth 2 args) 0))
    ('file-exists-p t)
    ('file-readable-p t)
    ('file-writable-p t)
    ('file-directory-p nil)
    ('file-remote-p nil)
    ('file-regular-p t)
    ('file-modes #o644)
    ('file-attributes nil)
    ('verify-visited-file-modtime t)
    ('set-visited-file-modtime nil)
    ('make-auto-save-file-name "")
    ('file-newer-than-file-p nil)
    ('file-name-sans-versions (car args))
    ('vc-registered nil)
    ('abbreviate-file-name (car args))
    ('substitute-in-file-name (car args))
    ('directory-file-name
     (let ((url (car args)))
       (if (and (> (length url) 0) (string-suffix-p "/" url))
           (substring url 0 -1)
         url)))
    ('file-name-as-directory
     (let ((url (car args)))
       (if (string-suffix-p "/" url) url (concat url "/"))))
    ('unhandled-file-name-directory nil)
    (_
     (let ((inhibit-file-name-handlers
            (cons 'braid-mode--file-handler inhibit-file-name-handlers))
           (inhibit-file-name-operation operation))
       (apply operation args)))))

(add-to-list 'file-name-handler-alist
             (cons braid-mode--url-regexp #'braid-mode--file-handler))


;;;; ======================================================================
;;;; Auto-connect for URL buffers
;;;; ======================================================================

(defun braid-mode--maybe-auto-connect ()
  "Auto-connect braid-text for URL buffers opened with find-file."
  (when (and buffer-file-name
             (string-match-p braid-mode--url-regexp buffer-file-name)
             (not braid-mode))
    (braid-connect buffer-file-name)))

(add-hook 'find-file-hook #'braid-mode--maybe-auto-connect)


(provide 'braid-mode)
;;; braid-mode.el ends here
