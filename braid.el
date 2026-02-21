;;; braid.el --- Collaborative editing via Braid-HTTP -*- lexical-binding: t -*-

;; Author: Michael Toomim <toomim@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/braid-org/braid-emacs
;; Keywords: comm, tools

;;; Commentary:

;; Sync Emacs buffers with HTTP resources using the Braid protocol.
;;
;; Use `braid-connect' to connect a buffer to any Braid server, or
;; open files under `~/http/' for automatic braidfs integration.
;;
;; This package includes:
;;   braid-http  — low-level Braid-HTTP client library
;;   braid-text  — simpleton sync algorithm
;;   braid-mode  — minor mode with modeline indicator

;;; Code:

(require 'braid-mode)

(provide 'braid)
;;; braid.el ends here
