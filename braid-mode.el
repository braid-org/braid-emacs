;;; braid-mode.el --- Minor mode for Braid-HTTP text sync -*- lexical-binding: t -*-

;; Author: Michael Toomim <toomim@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/braid-org/braid-emacs
;; Keywords: comm, tools

;;
;; A minor mode that keeps an Emacs buffer in sync with a braid-text
;; server resource using the simpleton merge algorithm.
;;
;; Usage:
;;
;;   M-x braid-connect   — prompts for host, port, path; enables sync
;;   M-x braid-mode      — toggle (disabling closes the connection)
;;
;; Or from Lisp:
;;
;;   (braid-connect "127.0.0.1" 8888 "/text/my-doc")

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


;;;; ======================================================================
;;;; Buffer-local state
;;;; ======================================================================

(defvar-local braid-mode--bt nil
  "The `braid-text' struct syncing this buffer, or nil if not connected.")

(defvar-local braid-mode--saved-auto-save-name nil
  "Saved `buffer-auto-save-file-name' restored when `braid-mode' is disabled.")

(defvar-local braid-mode--saved-backup nil
  "Saved `make-backup-files' value restored when `braid-mode' is disabled.")

(defvar-local braid-mode--saved-create-lockfiles nil
  "Saved `create-lockfiles' value restored when `braid-mode' is disabled.")


;;;; ======================================================================
;;;; Minor mode
;;;; ======================================================================

(defun braid-mode--lighter ()
  "Return the mode-line string for `braid-mode'."
  (if (null braid-mode--bt)
      " ○"
    (pcase (braid-sub-status (braid-text-sub braid-mode--bt))
      (:connected "●")
      (_          "○"))))

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

;;;###autoload
(define-minor-mode braid-mode
  "Minor mode to sync the current buffer with a Braid-HTTP server.
Enable with `braid-connect'; disable to close the connection."
  :lighter (:eval (braid-mode--lighter))
  :group 'braid
  (if braid-mode
      (progn
        (add-hook 'after-change-functions #'braid-mode--after-change nil t)
        ;; Disable auto-save and backups for this buffer.
        (setq braid-mode--saved-auto-save-name buffer-auto-save-file-name)
        (setq buffer-auto-save-file-name nil)
        (make-local-variable 'make-backup-files)
        (setq braid-mode--saved-backup make-backup-files)
        (setq make-backup-files nil)
        (make-local-variable 'create-lockfiles)
        (setq braid-mode--saved-create-lockfiles create-lockfiles)
        (setq create-lockfiles nil))
    (remove-hook 'after-change-functions #'braid-mode--after-change t)
    ;; Restore auto-save, backup, and lock-file settings.
    (setq buffer-auto-save-file-name braid-mode--saved-auto-save-name)
    (setq make-backup-files braid-mode--saved-backup)
    (kill-local-variable 'make-backup-files)
    (setq create-lockfiles braid-mode--saved-create-lockfiles)
    (kill-local-variable 'create-lockfiles)
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
Returns nil if FILE is not under `braid-http-dir'."
  (let ((root (file-name-as-directory (expand-file-name braid-http-dir))))
    (when (string-prefix-p root file)
      (let* ((rel        (substring file (length root)))
             (slash      (string-match "/" rel))
             (domain     (if slash (substring rel 0 slash) rel))
             (path-rest  (if slash (substring rel slash) "/"))
             (colon      (string-match ":" domain))
             (host       (if colon (substring domain 0 colon) domain))
             (tls        (not colon))
             (port       (if colon
                             (string-to-number (substring domain (1+ colon)))
                           443)))
        (list host port path-rest tls)))))

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
  (braid-mode 1)
  (setq braid-mode--bt (braid-text-open host port path (current-buffer) :tls tls))
  (message "Braid: connecting to %s://%s%s" (if tls "https" "http") host
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
