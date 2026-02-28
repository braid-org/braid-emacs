;;; hook-patch-test.el --- Test patch correctness for complex Emacs operations -*- lexical-binding: t -*-
;;
;; Tests the hybrid before-change + after-change hook approach used by
;; braid-mode to generate patches from buffer changes.
;;
;; Theory:
;;   - after-change-functions' (beg, end, old-len) gives accurate change info
;;     for each individual change → fast path (substring splice).
;;   - combine-after-change-calls normally produces a single combined
;;     after-change call with potentially incorrect args.  BUT: merely
;;     installing a before-change-functions hook causes Emacs to fire
;;     individual before+after pairs instead of combining.
;;   - The bc-count > 1 check is a safety net: if combining ever does
;;     occur (future Emacs version, edge case), fall back to Myers diff.
;;
;; What we test:
;;   Part A: Every complex Emacs operation produces correct patches via
;;           the hybrid approach (fast path + slow path fallback).
;;   Part B: combine-after-change-calls fires individual calls when
;;           before-change hook is installed (bc-count never exceeds 1).
;;   Part C: The slow path (Myers diff) produces correct results when
;;           bc-count > 1 is forced.
;;
;; Run:
;;   emacs --batch -L . -l test/hook-patch-test.el -f run-hook-patch-tests

;;; Code:

(require 'cl-lib)
(require 'myers-diff)

;;;; ======================================================================
;;;; Hybrid hook simulation (matches braid-mode implementation)
;;;; ======================================================================

(defvar hpt--client-state nil "Simulated client-state string.")
(defvar hpt--bc-count 0 "Before-change call count (reset by after-change).")
(defvar hpt--patches nil "List of applied patches for inspection.")
(defvar hpt--error nil "Non-nil if a patch application failed.")
(defvar hpt--used-slow-path nil "Non-nil if the slow path (Myers diff) was used.")
(defvar hpt--max-bc-count 0 "Max bc-count observed across all after-change calls.")

(defun hpt--before-change (_beg _end)
  "Count before-change calls to detect combine-after-change-calls."
  (cl-incf hpt--bc-count))

(defun hpt--after-change (beg end old-len)
  "Apply patch using the hybrid approach.
If bc-count > 1 (combined change), use Myers diff (slow path).
Otherwise, use after-change args (fast path)."
  (when (> hpt--bc-count hpt--max-bc-count)
    (setq hpt--max-bc-count hpt--bc-count))
  (if (> hpt--bc-count 1)
      ;; Slow path: full diff
      (let* ((new-text (buffer-substring-no-properties (point-min) (point-max)))
             (diff-patches (myers-diff-patches hpt--client-state new-text)))
        (setq hpt--used-slow-path t)
        (push (list :slow-path diff-patches) hpt--patches)
        (setq hpt--client-state new-text))
    ;; Fast path: use after-change args
    (let* ((base (point-min))
           (start (- beg base))
           (end-old (+ start old-len))
           (content (buffer-substring-no-properties beg end)))
      (push (list start end-old content) hpt--patches)
      (condition-case err
          (setq hpt--client-state
                (concat (substring hpt--client-state 0 start)
                        content
                        (substring hpt--client-state end-old)))
        (error (setq hpt--error
                     (format "patch failed: start=%d end-old=%d cs-len=%d content=%S err=%S"
                             start end-old (length hpt--client-state) content err))))))
  (setq hpt--bc-count 0))

(defun hpt--reset ()
  "Reset all test state."
  (setq hpt--client-state nil
        hpt--patches nil
        hpt--error nil
        hpt--bc-count 0
        hpt--used-slow-path nil
        hpt--max-bc-count 0))

(defun hpt--run-test (name old-text setup-fn op-fn)
  "Run one patch correctness test using the hybrid approach.
Returns a plist with :name, :ok, :error, :slow, :max-bc, :patches, etc."
  (with-temp-buffer
    (insert old-text)
    (when setup-fn (funcall setup-fn))
    (hpt--reset)
    (setq hpt--client-state (buffer-substring-no-properties (point-min) (point-max)))
    (add-hook 'before-change-functions #'hpt--before-change nil t)
    (add-hook 'after-change-functions #'hpt--after-change nil t)
    (unwind-protect
        (funcall op-fn)
      (remove-hook 'before-change-functions #'hpt--before-change t)
      (remove-hook 'after-change-functions #'hpt--after-change t))
    (let* ((new-text (buffer-substring-no-properties (point-min) (point-max)))
           (ok (and (null hpt--error)
                    (equal hpt--client-state new-text))))
      (list :name name
            :ok ok
            :error hpt--error
            :slow hpt--used-slow-path
            :max-bc hpt--max-bc-count
            :patches (nreverse hpt--patches)
            :cs hpt--client-state
            :old old-text
            :new new-text))))

(defvar hpt--fail-count 0 "Total failures.")

(defun hpt--report (result)
  "Print one test result and return it."
  (let ((name (plist-get result :name))
        (ok (plist-get result :ok))
        (slow (plist-get result :slow))
        (max-bc (plist-get result :max-bc)))
    (message "  %-35s  %s%s"
             name
             (if ok "OK" "FAIL")
             (cond (slow " (slow path)")
                   ((> max-bc 1) (format " (max-bc=%d)" max-bc))
                   (t "")))
    (unless ok
      (cl-incf hpt--fail-count)
      (when (plist-get result :error)
        (message "    ERROR: %s" (plist-get result :error)))
      (message "    old:     %S" (plist-get result :old))
      (message "    new:     %S" (plist-get result :new))
      (message "    cs:      %S" (plist-get result :cs))
      (message "    patches: %S" (plist-get result :patches)))
    result))


;;;; ======================================================================
;;;; Part A: Patch correctness for complex operations
;;;; ======================================================================

(defun hpt--run-part-a ()
  "Test that every complex operation produces correct patches."
  (message "\n--- Part A: Patch correctness for complex operations ---")

  ;; Case changes
  (hpt--report (hpt--run-test "capitalize-word"
    "hello world foo"
    (lambda () (goto-char (point-min)))
    (lambda () (capitalize-word 1))))

  (hpt--report (hpt--run-test "capitalize-word-already-caps"
    "Hello world"
    (lambda () (goto-char (point-min)))
    (lambda () (capitalize-word 1))))

  (hpt--report (hpt--run-test "capitalize-word-mixed"
    "hELLO world"
    (lambda () (goto-char (point-min)))
    (lambda () (capitalize-word 1))))

  (hpt--report (hpt--run-test "capitalize-word-multi"
    "hello world foo bar"
    (lambda () (goto-char (point-min)))
    (lambda () (capitalize-word 3))))

  (hpt--report (hpt--run-test "upcase-word"
    "hello world"
    (lambda () (goto-char (point-min)))
    (lambda () (upcase-word 1))))

  (hpt--report (hpt--run-test "downcase-word"
    "HELLO WORLD"
    (lambda () (goto-char (point-min)))
    (lambda () (downcase-word 1))))

  (hpt--report (hpt--run-test "upcase-region"
    "hello world"
    nil
    (lambda () (upcase-region (point-min) (point-max)))))

  (hpt--report (hpt--run-test "downcase-region"
    "HELLO WORLD"
    nil
    (lambda () (downcase-region (point-min) (point-max)))))

  ;; Replace
  (hpt--report (hpt--run-test "replace-string-grow"
    "foo bar foo baz foo"
    nil
    (lambda ()
      (goto-char (point-min))
      (while (search-forward "foo" nil t)
        (replace-match "quux" t t)))))

  (hpt--report (hpt--run-test "replace-string-shrink"
    "foobar foobar foobar"
    nil
    (lambda ()
      (goto-char (point-min))
      (while (search-forward "foobar" nil t)
        (replace-match "x" t t)))))

  (hpt--report (hpt--run-test "replace-regexp"
    "cat sat on the mat"
    nil
    (lambda ()
      (goto-char (point-min))
      (while (re-search-forward "\\bat\\b" nil t)
        (replace-match "AT" t t)))))

  ;; Fill / indent
  (hpt--report (hpt--run-test "fill-paragraph"
    "This is a very long line of text that should be wrapped when fill-paragraph is called because it exceeds the fill column which defaults to seventy characters but we want even more text here."
    (lambda () (goto-char (point-min)) (setq-local fill-column 40))
    (lambda () (fill-paragraph))))

  (hpt--report (hpt--run-test "fill-region"
    "This is a long line that needs wrapping to test fill-region properly and see what hooks fire.\nAnother long line that also needs wrapping to really exercise the fill algorithm properly."
    (lambda () (setq-local fill-column 30))
    (lambda () (fill-region (point-min) (point-max)))))

  (hpt--report (hpt--run-test "indent-rigidly"
    "line one\nline two\nline three"
    nil
    (lambda () (indent-rigidly (point-min) (point-max) 4))))

  (hpt--report (hpt--run-test "center-line"
    "hello"
    (lambda () (goto-char (point-min)) (setq-local fill-column 40))
    (lambda () (center-line))))

  ;; Transpose
  (hpt--report (hpt--run-test "transpose-words"
    "hello world"
    (lambda () (goto-char 7))
    (lambda () (transpose-words 1))))

  (hpt--report (hpt--run-test "transpose-lines"
    "line one\nline two\n"
    (lambda () (goto-char (point-min)) (forward-line 1))
    (lambda () (transpose-lines 1))))

  (hpt--report (hpt--run-test "transpose-chars"
    "ab"
    (lambda () (goto-char 3))
    (lambda () (transpose-chars 1))))

  ;; Whitespace
  (hpt--report (hpt--run-test "delete-trailing-whitespace"
    "hello   \nworld  \nfoo"
    nil
    (lambda () (delete-trailing-whitespace))))

  (hpt--report (hpt--run-test "just-one-space"
    "hello     world"
    (lambda () (goto-char 8))
    (lambda () (just-one-space))))

  (hpt--report (hpt--run-test "tabify"
    "    hello\n        world"
    nil
    (lambda () (tabify (point-min) (point-max)))))

  (hpt--report (hpt--run-test "untabify"
    "\thello\n\t\tworld"
    nil
    (lambda () (untabify (point-min) (point-max)))))

  ;; Dynamic completion
  (hpt--report (hpt--run-test "dabbrev-expand"
    "expansion\nexp"
    (lambda () (goto-char (point-max)))
    (lambda () (dabbrev-expand nil))))

  ;; Sort
  (hpt--report (hpt--run-test "sort-lines"
    "cherry\napple\nbanana"
    nil
    (lambda () (sort-lines nil (point-min) (point-max)))))

  ;; Kill/yank
  (hpt--report (hpt--run-test "kill-word"
    "hello world"
    (lambda () (goto-char (point-min)))
    (lambda () (kill-word 1))))

  (hpt--report (hpt--run-test "kill-and-yank-elsewhere"
    "hello world"
    (lambda ()
      (goto-char (point-min))
      (let ((inhibit-modification-hooks t))
        (kill-word 1)
        (goto-char (point-min))
        (insert "hello"))
      (goto-char (point-min))
      (kill-word 1))
    (lambda ()
      (goto-char (point-max))
      (yank))))

  ;; combine-after-change-calls (should be un-combined by bc hook presence)
  (hpt--report (hpt--run-test "combine-replace+insert"
    "hello world"
    nil
    (lambda ()
      (combine-after-change-calls
        (goto-char (point-min))
        (delete-char 5)
        (insert "HELLO")
        (goto-char (point-max))
        (insert "!")))))

  (hpt--report (hpt--run-test "combine-multi-replace"
    "aaa bbb ccc"
    nil
    (lambda ()
      (combine-after-change-calls
        (goto-char (point-min))
        (when (search-forward "aaa" nil t) (replace-match "XXX" t t))
        (when (search-forward "ccc" nil t) (replace-match "ZZZ" t t)))))))


;;;; ======================================================================
;;;; Part B: Verify combine-after-change-calls doesn't combine with bc hook
;;;; ======================================================================

(defun hpt--run-part-b ()
  "Verify that bc hook presence prevents combining."
  (message "\n--- Part B: combine-after-change-calls un-combined by bc hook ---")

  ;; With before-change hook: should get individual calls
  (let ((bc-count 0) (ac-count 0) (ac-args nil))
    (with-temp-buffer
      (insert "aaa bbb ccc")
      (add-hook 'before-change-functions
                (lambda (_b _e) (setq bc-count (1+ bc-count))) nil t)
      (add-hook 'after-change-functions
                (lambda (b e ol) (setq ac-count (1+ ac-count))
                  (push (list (- b (point-min)) (- e (point-min)) ol) ac-args))
                nil t)
      (combine-after-change-calls
        (goto-char (point-min))
        (when (search-forward "aaa" nil t) (replace-match "XXX" t t))
        (when (search-forward "ccc" nil t) (replace-match "ZZZ" t t))))
    (if (and (= bc-count 2) (= ac-count 2))
        (message "  %-35s  OK (bc=%d ac=%d)" "with-bc-hook-individual-calls" bc-count ac-count)
      (message "  %-35s  FAIL (bc=%d ac=%d args=%S)" "with-bc-hook-individual-calls"
               bc-count ac-count (nreverse ac-args))
      (cl-incf hpt--fail-count)))

  ;; Without before-change hook: should combine (this is the bug we're protecting against)
  (let ((ac-count 0) (ac-args nil))
    (with-temp-buffer
      (insert "aaa bbb ccc")
      (add-hook 'after-change-functions
                (lambda (b e ol) (setq ac-count (1+ ac-count))
                  (push (list (- b (point-min)) (- e (point-min)) ol) ac-args))
                nil t)
      (combine-after-change-calls
        (goto-char (point-min))
        (when (search-forward "aaa" nil t) (replace-match "XXX" t t))
        (when (search-forward "ccc" nil t) (replace-match "ZZZ" t t))))
    (if (= ac-count 1)
        (message "  %-35s  OK (ac=%d — combined, buggy)" "without-bc-hook-combined" ac-count)
      (message "  %-35s  UNEXPECTED (ac=%d — not combined)" "without-bc-hook-combined" ac-count))))


;;;; ======================================================================
;;;; Part C: Slow path produces correct results when forced
;;;; ======================================================================

(defun hpt--run-part-c ()
  "Force bc-count > 1 and verify slow path correctness."
  (message "\n--- Part C: Forced slow path (Myers diff) correctness ---")

  ;; Simulate a hypothetical scenario where bc-count > 1
  ;; by manually setting it before running an operation.
  (dolist (test-case
           '(("forced-slow: replace"
              "hello world"
              nil
              (lambda ()
                (goto-char (point-min))
                (delete-char 5)
                (insert "HELLO")))
             ("forced-slow: multi-edit"
              "aaa bbb ccc"
              nil
              (lambda ()
                (goto-char (point-min))
                (when (search-forward "aaa" nil t) (replace-match "XXX" t t))
                (when (search-forward "ccc" nil t) (replace-match "ZZZ" t t))))
             ("forced-slow: fill-paragraph"
              "This is a very long line that should be wrapped because it exceeds the fill column."
              (lambda () (goto-char (point-min)) (setq-local fill-column 30))
              (lambda () (fill-paragraph)))
             ("forced-slow: sort-lines"
              "cherry\napple\nbanana"
              nil
              (lambda () (sort-lines nil (point-min) (point-max))))
             ("forced-slow: transpose-words"
              "hello world"
              (lambda () (goto-char 7))
              (lambda () (transpose-words 1)))))
    (let ((name (nth 0 test-case))
          (old-text (nth 1 test-case))
          (setup-fn (nth 2 test-case))
          (op-fn (nth 3 test-case)))
      (with-temp-buffer
        (insert old-text)
        (when setup-fn (funcall setup-fn))
        (hpt--reset)
        (setq hpt--client-state (buffer-substring-no-properties (point-min) (point-max)))
        ;; Force bc-count > 1 so every after-change call takes slow path
        (add-hook 'before-change-functions
                  (lambda (_b _e) (setq hpt--bc-count 99)) nil t)
        (add-hook 'after-change-functions #'hpt--after-change nil t)
        (unwind-protect
            (funcall op-fn)
          (remove-hook 'before-change-functions
                       (car (buffer-local-value 'before-change-functions (current-buffer))) t)
          (remove-hook 'after-change-functions #'hpt--after-change t))
        (let* ((new-text (buffer-substring-no-properties (point-min) (point-max)))
               (ok (and (null hpt--error)
                        hpt--used-slow-path
                        (equal hpt--client-state new-text))))
          (if ok
              (message "  %-35s  OK (slow path)" name)
            (cl-incf hpt--fail-count)
            (message "  %-35s  FAIL slow=%s err=%s" name hpt--used-slow-path hpt--error)
            (unless (equal hpt--client-state new-text)
              (message "    cs:  %S" hpt--client-state)
              (message "    new: %S" new-text))))))))


;;;; ======================================================================
;;;; Runner
;;;; ======================================================================

(defun run-hook-patch-tests ()
  "Run all hook-patch tests and exit with appropriate status."
  (setq hpt--fail-count 0)
  (message "========================================")
  (message "Hook-patch correctness tests")
  (message "========================================")
  (hpt--run-part-a)
  (hpt--run-part-b)
  (hpt--run-part-c)
  (message "\n========================================")
  (if (= hpt--fail-count 0)
      (message "ALL PASS")
    (message "%d FAILURE(S)" hpt--fail-count))
  (message "========================================")
  (kill-emacs (if (> hpt--fail-count 0) 1 0)))

(provide 'hook-patch-test)
;;; hook-patch-test.el ends here
