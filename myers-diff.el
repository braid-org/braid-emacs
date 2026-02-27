;;; myers-diff.el --- Character-level Myers diff -*- lexical-binding: t -*-

;; Based on "An O(ND) Difference Algorithm" by Eugene Myers.
;; Operates directly on string characters via `aref'.
;;
;; Primary entry point:
;;   (myers-diff-patches OLD NEW) → list of (:start S :end E :content C)
;;
;; Each patch replaces OLD[start..end) with content from NEW.
;; Optimized: strips common prefix/suffix first, uses vector-based trace.

;;; Code:

(require 'cl-lib)


;;;; ================================================================
;;;; Core Myers algorithm (vector-based trace)
;;;; ================================================================

(defun myers-diff--core (old new)
  "Compute a character-level Myers diff of strings OLD and NEW.
Returns a list of edit ops: (:equal ...), (:delete ...), (:insert ...).
Uses vector-based trace for O(1) snapshot lookup during backtrack."
  (let* ((n (length old))
         (m (length new))
         (max-d (+ n m))
         (v-size (1+ (* 2 max-d)))
         (v (make-vector v-size 0))
         (trace (make-vector (1+ max-d) nil))
         (found-d nil))

    (if (and (= n 0) (= m 0))
        nil

      ;; Initialize: V[1] = 0
      (aset v (+ 1 max-d) 0)

      ;; Forward pass
      (cl-block outer
        (cl-loop for d from 0 to max-d do
          (aset trace d (copy-sequence v))
          (cl-loop for k from (- d) to d by 2 do
            (let* ((k-idx (+ k max-d))
                   (x (if (or (= k (- d))
                              (and (/= k d)
                                   (< (aref v (+ k -1 max-d))
                                      (aref v (+ k  1 max-d)))))
                          (aref v (+ k 1 max-d))
                        (1+ (aref v (+ k -1 max-d)))))
                   (y (- x k)))
              ;; Follow diagonal (matching characters)
              (while (and (< x n) (< y m)
                          (eq (aref old x) (aref new y)))
                (setq x (1+ x))
                (setq y (1+ y)))
              (aset v k-idx x)
              (when (and (>= x n) (>= y m))
                (setq found-d d)
                (cl-return-from outer))))))

      ;; Backtrack to recover edit script
      (let ((edits nil)
            (x n)
            (y m))
        (cl-loop for d from found-d downto 0 do
          (let* ((v-snap (aref trace d))
                 (k (- x y))
                 (prev-k
                  (cond
                   ((or (= k (- d))
                        (and (/= k d)
                             (< (aref v-snap (+ (1- k) max-d))
                                (aref v-snap (+ (1+ k) max-d)))))
                    (1+ k))
                   (t (1- k))))
                 (prev-x (aref v-snap (+ prev-k max-d)))
                 (prev-y (- prev-x prev-k)))
            (let ((snake-start-x (if (= prev-k (1+ k)) prev-x (1+ prev-x)))
                  (snake-start-y (if (= prev-k (1+ k)) (1+ prev-y) prev-y)))
              ;; Record diagonal (equal) portion
              (when (> x snake-start-x)
                (push (list :equal
                            :start-a snake-start-x
                            :start-b snake-start-y
                            :length (- x snake-start-x))
                      edits))
              ;; Record edit move
              (cond
               ((= prev-k (1+ k))
                (when (> d 0)
                  (push (list :insert :start-b prev-y :length 1) edits)))
               (t
                (when (> d 0)
                  (push (list :delete :start-a prev-x :length 1) edits)))))
            (setq x prev-x
                  y prev-y)))

        ;; Merge adjacent operations
        (myers-diff--merge-ops edits)))))

(defun myers-diff--merge-ops (ops)
  "Merge adjacent OPS of the same type into larger spans."
  (when ops
    (let ((merged (list (car ops))))
      (dolist (op (cdr ops))
        (let ((prev (car merged)))
          (cond
           ;; Merge consecutive :equal
           ((and (eq (car op) :equal) (eq (car prev) :equal)
                 (= (+ (plist-get (cdr prev) :start-a)
                        (plist-get (cdr prev) :length))
                     (plist-get (cdr op) :start-a)))
            (setcar merged
                    (list :equal
                          :start-a (plist-get (cdr prev) :start-a)
                          :start-b (plist-get (cdr prev) :start-b)
                          :length (+ (plist-get (cdr prev) :length)
                                     (plist-get (cdr op) :length)))))
           ;; Merge consecutive :delete
           ((and (eq (car op) :delete) (eq (car prev) :delete)
                 (= (+ (plist-get (cdr prev) :start-a)
                        (plist-get (cdr prev) :length))
                     (plist-get (cdr op) :start-a)))
            (setcar merged
                    (list :delete
                          :start-a (plist-get (cdr prev) :start-a)
                          :length (+ (plist-get (cdr prev) :length)
                                     (plist-get (cdr op) :length)))))
           ;; Merge consecutive :insert
           ((and (eq (car op) :insert) (eq (car prev) :insert)
                 (= (+ (plist-get (cdr prev) :start-b)
                        (plist-get (cdr prev) :length))
                     (plist-get (cdr op) :start-b)))
            (setcar merged
                    (list :insert
                          :start-b (plist-get (cdr prev) :start-b)
                          :length (+ (plist-get (cdr prev) :length)
                                     (plist-get (cdr op) :length)))))
           (t
            (push op merged)))))
      (nreverse merged))))


;;;; ================================================================
;;;; Ops → Patches conversion
;;;; ================================================================

(defun myers-diff--ops-to-patches (ops new)
  "Convert merged Myers OPS to a list of (:start S :end E :content C) patches.
Adjacent delete+insert pairs are coalesced into a single replace patch.
NEW is the target string (used to extract insert content)."
  (let ((patches nil)
        (ops-remaining ops))
    (while ops-remaining
      (let ((op (pop ops-remaining)))
        (pcase (car op)
          (:equal nil)
          (:delete
           (let ((start (plist-get (cdr op) :start-a))
                 (len   (plist-get (cdr op) :length)))
             ;; Check if next op is insert — coalesce into replace
             (if (and ops-remaining (eq (car (car ops-remaining)) :insert))
                 (let* ((ins (pop ops-remaining))
                        (ins-start (plist-get (cdr ins) :start-b))
                        (ins-len   (plist-get (cdr ins) :length)))
                   (push (list :start start
                               :end (+ start len)
                               :content (substring new ins-start
                                                   (+ ins-start ins-len)))
                         patches))
               (push (list :start start
                           :end (+ start len)
                           :content "")
                     patches))))
          (:insert
           (let* ((ins-start (plist-get (cdr op) :start-b))
                  (ins-len   (plist-get (cdr op) :length))
                  ;; Position in old: look at what follows
                  ;; If next op is :equal, insert goes before its start-a
                  ;; If next op is :delete, insert goes before its start-a
                  ;; If no next op, insert goes at end of old
                  (old-pos (if ops-remaining
                               (let ((next (car ops-remaining)))
                                 (pcase (car next)
                                   (:equal  (plist-get (cdr next) :start-a))
                                   (:delete (plist-get (cdr next) :start-a))
                                   ;; insert after insert shouldn't happen after merge
                                   (_ 0)))
                             ;; No more ops — must be end of old string.
                             ;; We can compute: start-b + anything after = end of new
                             ;; old-pos = start-a of previous equal + its length
                             ;; Actually for trailing insert, old-pos is just
                             ;; wherever we are in old, which is end of old.
                             ;; We use a sentinel — caller must set this.
                             nil)))
             ;; For trailing inserts, old-pos will be set by caller
             ;; But we can compute it: the insert's position in old is
             ;; determined by the edit graph. For a standalone insert
             ;; preceded by :equal at (A,B) with length L, the insert
             ;; position in old is A+L. For leading insert, it's 0.
             (unless old-pos
               ;; Trailing insert: position is at end of old.
               ;; We can get this from the insert's position in the edit graph:
               ;; if start-b = B, and prior ops consumed (B - net_inserts) from old,
               ;; but simpler: end of old = sum of all :equal lengths + :delete lengths
               (let ((pos 0))
                 (dolist (o ops)
                   (pcase (car o)
                     (:equal  (cl-incf pos (plist-get (cdr o) :length)))
                     (:delete (cl-incf pos (plist-get (cdr o) :length)))))
                 (setq old-pos pos)))
             (push (list :start old-pos
                         :end old-pos
                         :content (substring new ins-start
                                             (+ ins-start ins-len)))
                   patches))))))
    (nreverse patches)))


;;;; ================================================================
;;;; Primary entry point
;;;; ================================================================

(defun myers-diff-patches (old new)
  "Diff strings OLD → NEW. Returns list of (:start S :end E :content C).
Each patch replaces OLD[start..end) with content from NEW.
Strips common prefix/suffix first, then runs Myers on the remainder.
Returns nil if OLD and NEW are equal."
  (let* ((olen (length old))
         (nlen (length new))
         (minlen (min olen nlen))
         (prefix 0)
         (suffix 0))
    ;; Strip common prefix
    (while (and (< prefix minlen) (eq (aref old prefix) (aref new prefix)))
      (cl-incf prefix))
    ;; Strip common suffix (only within the non-prefix portion)
    (let ((max-suffix (- minlen prefix)))
      (while (and (< suffix max-suffix)
                  (eq (aref old (- olen suffix 1))
                      (aref new (- nlen suffix 1))))
        (cl-incf suffix)))
    ;; Extract the differing middle portions
    (let ((mid-old (substring old prefix (- olen suffix)))
          (mid-new (substring new prefix (- nlen suffix))))
      (cond
       ;; No difference
       ((and (string-empty-p mid-old) (string-empty-p mid-new))
        nil)
       ;; Trivial: pure insert or pure delete — single patch
       ((or (string-empty-p mid-old) (string-empty-p mid-new))
        (list (list :start prefix
                    :end (+ prefix (length mid-old))
                    :content mid-new)))
       ;; General case: run Myers on trimmed middle, convert to patches
       (t
        (let* ((ops (myers-diff--core mid-old mid-new))
               (raw-patches (myers-diff--ops-to-patches ops mid-new)))
          ;; Offset patches back to original coordinates
          (mapcar (lambda (p)
                    (list :start (+ prefix (plist-get p :start))
                          :end   (+ prefix (plist-get p :end))
                          :content (plist-get p :content)))
                  raw-patches)))))))


;;;; ================================================================
;;;; Legacy API (kept for backwards compatibility and testing)
;;;; ================================================================

(defun myers-diff (old new)
  "Compute a character-level Myers diff of strings OLD and NEW.
Returns a list of edit ops: (:equal ...), (:delete ...), (:insert ...).
Consider using `myers-diff-patches' instead for braid-text integration."
  (myers-diff--core old new))

(defun myers-diff-apply (old new ops)
  "Reconstruct NEW from OLD by applying OPS. Returns the rebuilt string."
  (let ((parts nil))
    (dolist (op ops)
      (pcase (car op)
        (:equal
         (push (substring old
                          (plist-get (cdr op) :start-a)
                          (+ (plist-get (cdr op) :start-a)
                             (plist-get (cdr op) :length)))
               parts))
        (:insert
         (push (substring new
                          (plist-get (cdr op) :start-b)
                          (+ (plist-get (cdr op) :start-b)
                             (plist-get (cdr op) :length)))
               parts))
        (:delete nil)))
    (apply #'concat (nreverse parts))))


(provide 'myers-diff)
;;; myers-diff.el ends here
