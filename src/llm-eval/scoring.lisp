;;;; scoring.lisp -- scores that need no model. Each returns a number
;;;; from 0 to 1.

(in-package #:llm-eval)

(defun tokens (text)
  "Lowercase word and number tokens from TEXT. Commas inside numbers are
dropped and periods kept, so \"$1,100\" gives \"1100\" and \"1.1°C\" gives
\"1.1\" and \"c\"."
  (let ((result '())
        (current (make-string-output-stream)))
    (flet ((finish-token ()
             (let ((token (string-trim "." (get-output-stream-string current))))
               (when (plusp (length token))
                 (push token result)))))
      (loop for previous = nil then char
            for char across (string-downcase text)
            for index from 0
            do (cond ((alphanumericp char) (write-char char current))
                     ((char= char #\.) (write-char char current))
                     ((and (char= char #\,)
                           previous (digit-char-p previous)
                           (< (1+ index) (length text))
                           (digit-char-p (char text (1+ index))))
                      nil)
                     (t (finish-token))))
      (finish-token))
    (nreverse result)))

(defun exact-match (answer reference)
  "1 if ANSWER and REFERENCE have the same tokens, else 0."
  (if (equal (tokens answer) (tokens reference)) 1 0))

(defun token-f1 (answer reference)
  "F1 score of the token overlap between ANSWER and REFERENCE, the measure
used by the SQuAD question answering benchmark. Each token counts as many
times as it appears in both texts."
  (let ((answer-tokens (tokens answer))
        (reference-tokens (tokens reference)))
    (if (or (null answer-tokens) (null reference-tokens))
        0
        (let ((counts (make-hash-table :test #'equal))
              (common 0))
          (dolist (token reference-tokens)
            (incf (gethash token counts 0)))
          (dolist (token answer-tokens)
            (when (plusp (gethash token counts 0))
              (decf (gethash token counts))
              (incf common)))
          (if (zerop common)
              0
              (let ((precision (/ common (length answer-tokens)))
                    (recall (/ common (length reference-tokens))))
                (/ (* 2 precision recall) (+ precision recall))))))))

(defun keyword-recall (answer keywords)
  "Fraction of KEYWORDS found in ANSWER as whole tokens. With no keywords
the score is 1."
  (if (null keywords)
      1
      (let ((text (format nil " ~{~A~^ ~} " (tokens answer))))
        (/ (count-if (lambda (keyword)
                       (search (format nil " ~{~A~^ ~} " (tokens keyword)) text))
                     keywords)
           (length keywords)))))
