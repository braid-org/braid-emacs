;;; profile-large-file.el --- Profile braid-emacs on large files -*- lexical-binding: t -*-
;;
;; Usage:
;;   emacs --batch -L . -l test/profile-large-file.el
;;
;; Measures per-keystroke latency of the braid-text fast path, slow path,
;; PUT formatting, and patch application on a 10KB buffer.
;; Uses a mock process to isolate computation from network I/O.

;;; Code:

(require 'braid-http)
(require 'braid-text)
(require 'myers-diff)

(defvar profile--iterations 100
  "Number of iterations for each benchmark.")

(defvar profile--text-size 10000
  "Target size of the test buffer in characters.")

(defun profile--make-text (n)
  "Generate N characters of test text (repeated 'abcdefghij\\n')."
  (let ((line "abcdefghij\n")
        (parts nil))
    (while (< (* (length line) (length parts)) n)
      (push line parts))
    (substring (apply #'concat (nreverse parts)) 0 n)))

(defun profile--make-mock-bt (text)
  "Create a braid-text struct with TEXT as client-state and a mock process."
  (let* ((buf (generate-new-buffer " *profile-test*"))
         (bt (make-braid-text
              :host "127.0.0.1" :port 8888 :path "/test"
              :peer "profile0001"
              :buffer buf
              :client-version '("profile-0")
              :client-state text
              :char-counter 0
              :outstanding-changes 0
              :max-outstanding-changes 1000)))
    ;; Insert text into buffer
    (with-current-buffer buf
      (insert text))
    ;; Create a mock process (pipe to /dev/null) so process-send-string works
    (let ((proc (start-process "profile-mock" nil "cat")))
      (set-process-query-on-exit-flag proc nil)
      (setf (braid-text-put-proc bt) proc))
    bt))

(defun profile--reset-bt (bt text)
  "Reset BT's state for the next iteration."
  (setf (braid-text-client-state bt) text)
  (setf (braid-text-client-version bt) '("profile-0"))
  (setf (braid-text-char-counter bt) 0)
  (setf (braid-text-outstanding-changes bt) 0)
  (setf (braid-text-unacked-puts bt) nil)
  (let ((buf (braid-text-buffer bt)))
    (with-current-buffer buf
      (let ((inhibit-modification-hooks t))
        (erase-buffer)
        (insert text)))))

(defun profile--bench (name n thunk)
  "Run THUNK N times, report timing for NAME.
Returns (name mean-us max-us min-us)."
  (let ((times nil))
    (dotimes (_ n)
      (let ((t0 (float-time)))
        (funcall thunk)
        (push (* 1e6 (- (float-time) t0)) times)))
    (let* ((sorted (sort times #'<))
           (total (apply #'+ sorted))
           (mean (/ total (float n)))
           (mn (car sorted))
           (mx (car (last sorted))))
      (list name mean mx mn))))

(defun profile--run ()
  "Run all benchmarks and print results."
  (let* ((text (profile--make-text profile--text-size))
         (bt (profile--make-mock-bt text))
         (buf (braid-text-buffer bt))
         (mid (/ (length text) 2))
         (results nil))

    (message "")
    (message "=== braid-emacs profiling ===")
    (message "Buffer size: %d chars (%d KB)" (length text) (/ (length text) 1024))
    (message "Iterations:  %d" profile--iterations)
    (message "Byte-compiled: %s" (if (byte-code-function-p
                                       (symbol-function 'braid-text--changed))
                                     "yes" "NO (interpreted)"))
    (message "")

    ;; --- Benchmark 1: Fast path — single char insert in middle ---
    (push
     (profile--bench
      "fast-path: insert 1 char"
      profile--iterations
      (lambda ()
        (profile--reset-bt bt text)
        ;; Simulate inserting "x" at position mid
        (with-current-buffer buf
          (let ((inhibit-modification-hooks t))
            (goto-char (+ (point-min) mid))
            (insert "x")))
        (braid-text-buffer-changed bt (+ (point-min) mid)
                                   (+ (point-min) mid 1) 0)))
     results)

    ;; --- Benchmark 2: Fast path — single char delete ---
    (push
     (profile--bench
      "fast-path: delete 1 char"
      profile--iterations
      (lambda ()
        (profile--reset-bt bt text)
        ;; Simulate deleting 1 char at position mid
        (with-current-buffer buf
          (let ((inhibit-modification-hooks t))
            (goto-char (+ (point-min) mid))
            (delete-char 1)))
        (braid-text-buffer-changed bt (+ (point-min) mid)
                                   (+ (point-min) mid) 1)))
     results)

    ;; --- Benchmark 3: Fast path — paste 100 chars ---
    (let ((paste-text (make-string 100 ?z)))
      (push
       (profile--bench
        "fast-path: paste 100 chars"
        profile--iterations
        (lambda ()
          (profile--reset-bt bt text)
          (with-current-buffer buf
            (let ((inhibit-modification-hooks t))
              (goto-char (+ (point-min) mid))
              (insert paste-text)))
          (braid-text-buffer-changed bt (+ (point-min) mid)
                                     (+ (point-min) mid 100) 0)))
       results))

    ;; --- Benchmark 4: Slow path (full buffer diff via Myers) ---
    (push
     (profile--bench
      "slow-path: Myers diff"
      profile--iterations
      (lambda ()
        (profile--reset-bt bt text)
        ;; Insert "x" in middle without change-info → forces slow path
        (with-current-buffer buf
          (let ((inhibit-modification-hooks t))
            (goto-char (+ (point-min) mid))
            (insert "x")))
        (braid-text--changed bt)))
     results)

    ;; --- Benchmark 5: concat to rebuild client-state (isolated) ---
    (let ((state (braid-text-client-state bt))
          (content "x"))
      (push
       (profile--bench
        "concat: rebuild state"
        profile--iterations
        (lambda ()
          (concat (substring state 0 mid) content (substring state mid))))
       results))

    ;; --- Benchmark 6: PUT formatting (isolated) ---
    (push
     (profile--bench
      "format-put-patches"
      profile--iterations
      (lambda ()
        (braid-http--format-put-patches
         "127.0.0.1" 8888 "/test"
         '("v1") '("v0")
         (list (list :start mid :end mid :content "x"))
         "text/plain" "profile0001")))
     results)

    ;; --- Benchmark 7: Apply patches (receive path) ---
    (push
     (profile--bench
      "apply-patches: 1 patch"
      profile--iterations
      (lambda ()
        (profile--reset-bt bt text)
        ;; Synthetic incoming patch: insert "hello" at position mid
        (braid-text--apply-patches
         bt
         (list (list :content-range (list "text" mid mid) :body "hello")))))
     results)

    ;; --- Benchmark 8: Apply patches — 5 patches ---
    (let ((patches (cl-loop for i from 0 below 5
                            for pos = (* i (/ (length text) 5))
                            collect (list :content-range (list "text" pos pos)
                                          :body "hi"))))
      (push
       (profile--bench
        "apply-patches: 5 patches"
        profile--iterations
        (lambda ()
          (profile--reset-bt bt text)
          (braid-text--apply-patches bt patches)))
       results))

    ;; --- Benchmark 9: simple-diff (prefix/suffix scan) ---
    (let ((old text)
          (new (concat (substring text 0 mid) "x" (substring text mid))))
      (push
       (profile--bench
        "simple-diff (1 char)"
        profile--iterations
        (lambda ()
          (braid-text--simple-diff old new)))
       results))

    ;; --- Benchmark 10: Myers diff (multi-edit) ---
    (let ((old text)
          (new (concat "NEW:" (substring text 0 mid) "x" (substring text mid) ":END")))
      (push
       (profile--bench
        "myers-diff (multi-edit)"
        profile--iterations
        (lambda ()
          (myers-diff-patches old new)))
       results))

    ;; --- Print results table ---
    (setq results (nreverse results))
    (message "%-30s %10s %10s %10s" "Operation" "Mean (us)" "Max (us)" "Min (us)")
    (message "%-30s %10s %10s %10s" "------------------------------"
             "----------" "----------" "----------")
    (dolist (r results)
      (message "%-30s %10.1f %10.1f %10.1f"
               (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r)))

    ;; Flag anything over 1ms
    (message "")
    (let ((slow (cl-remove-if (lambda (r) (< (nth 1 r) 1000)) results)))
      (if slow
          (progn
            (message "WARNING: %d operations exceed 1ms mean:" (length slow))
            (dolist (r slow)
              (message "  - %s: %.1f us (%.2f ms)"
                       (nth 0 r) (nth 1 r) (/ (nth 1 r) 1000.0))))
        (message "All operations under 1ms. No optimization needed.")))

    ;; Cleanup
    (let ((proc (braid-text-put-proc bt)))
      (when (and proc (process-live-p proc))
        (delete-process proc)))
    (when (buffer-live-p buf) (kill-buffer buf))
    (message "")))

;; Run when loaded in batch mode
(when noninteractive
  (profile--run))

(provide 'profile-large-file)
;;; profile-large-file.el ends here
