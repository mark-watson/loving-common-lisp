;;;; package.lisp

(defpackage #:llm-eval
  (:use #:cl)
  (:export
   ;; dataset.lisp
   #:eval-case
   #:make-eval-case
   #:eval-case-id
   #:eval-case-question
   #:eval-case-reference
   #:eval-case-keywords
   #:load-cases
   ;; scoring.lisp
   #:tokens
   #:exact-match
   #:token-f1
   #:keyword-recall
   ;; judge.lisp
   #:*judge-model*
   #:*judge-function*
   #:judge-prompt
   #:parse-judge-reply
   #:judge
   ;; harness.lisp
   #:eval-result
   #:eval-result-case-id
   #:eval-result-answer
   #:eval-result-exact
   #:eval-result-f1
   #:eval-result-keywords
   #:eval-result-judge
   #:eval-result-judge-reason
   #:eval-result-seconds
   #:eval-result-error
   #:run-eval
   #:summarize
   #:print-report
   ;; store.lisp
   #:open-store
   #:save-run
   #:run-summaries
   #:print-run-summaries
   #:compare-runs
   #:print-comparison))
