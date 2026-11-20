;;;; judge.lisp -- LLM-as-judge: a second model grades each answer
;;;; against the reference answer.

(in-package #:llm-eval)

(defvar *judge-model* "ollama/qwen3.5:4b")

(defvar *judge-extra-body* '(("reasoning_effort" . "none"))
  "Extra request fields for the judge. qwen3.5 thinks before it answers,
which takes about 50 seconds per call on my Mac; turning thinking off
brings that under one second.")

(defun judge-prompt (question reference answer)
  (format nil "You are grading an answer from a question answering system.

Question: ~A

Reference answer: ~A

Candidate answer: ~A

Does the candidate answer agree with the reference answer on the facts the
question asks for? Extra correct detail is fine. Wrong numbers, wrong names,
or a missing answer are not.

Reply with exactly two lines:
SCORE: 1 if the candidate agrees with the reference, or SCORE: 0 if not
REASON: one sentence"
          question reference answer))

(defun parse-judge-reply (reply)
  "Return the score (0 or 1) and the reason from a judge REPLY. The score
is NIL when the reply has no SCORE line with a 0 or 1."
  (let* ((score-start (search "SCORE:" reply :test #'char-equal))
         (digit (and score-start
                     (find-if-not (lambda (char) (member char '(#\Space #\Tab)))
                                  reply :start (+ score-start 6))))
         (reason-start (search "REASON:" reply :test #'char-equal))
         (reason (if reason-start
                     (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  (subseq reply (+ reason-start 7)))
                     "")))
    (values (case digit (#\1 1) (#\0 0) (t nil))
            reason)))

(defun litelm-judge (prompt)
  (litelm:response-content
   (litelm:completion *judge-model* :messages prompt
                                    :temperature 0
                                    :extra-body *judge-extra-body*)))

(defvar *judge-function* 'litelm-judge
  "Function of one argument, the prompt, that returns the judge's reply.
Tests bind this to a fake judge.")

(defun judge (question reference answer)
  "Ask the judge model to grade ANSWER. Returns the score (0, 1, or NIL
if the reply could not be parsed) and the judge's reason."
  (let ((reply (funcall *judge-function* (judge-prompt question reference answer))))
    (multiple-value-bind (score reason) (parse-judge-reply (or reply ""))
      (values score (if score reason (format nil "Unparsable judge reply: ~S" reply))))))
