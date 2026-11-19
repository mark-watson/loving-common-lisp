;;;; package.lisp

(defpackage #:resilient-llm
  (:use #:cl)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:export
   ;; basics.lisp
   #:bad-age
   #:bad-age-text
   #:parse-age
   #:parse-ages
   #:skip-entry
   #:parse-ages-skipping
   #:parse-ages-with-default
   #:parse-ages-logging
   ;; resilient-llm.lisp
   #:*default-model*
   #:*completion-function*
   #:*sleep-function*
   #:complete
   #:retry
   #:use-model
   #:transient-error-p
   #:retrying
   #:retrying-attempt
   #:retrying-delay
   #:retrying-cause
   #:backoff-delay
   #:call-with-retries
   #:with-retries
   #:call-with-fallback-models
   #:with-fallback-models
   #:malformed-json
   #:malformed-json-text
   #:strip-code-fences
   #:parse-json-reply
   #:complete-json
   #:repair-json
   #:with-json-repair))
