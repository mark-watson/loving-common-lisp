;;;; packages.lisp --- Package definition for the NSK engine.

(in-package :cl-user)

(defpackage :nsk
  (:use :cl)
  (:documentation "NSK: Neural-Symbolic Knowledge Graph engine.")
  (:export
   ;; logic variables and neural predicates
   #:logic-var #:logic-var-p #:logic-var-name
   #:neural-predicate #:neural-predicate-p #:neural-predicate-name
   ;; unification
   #:unify #:resolve #:+fail+
   ;; storage
   #:graph #:graph-p #:make-graph #:*graph* #:*log-path*
   #:open-store #:close-store #:add-triple #:remove-triple
   #:all-triples #:triple-count #:candidate-triples
   ;; query engine
   #:match-triple #:match-triple-pattern #:prove #:run-query
   #:ask #:solutions #:query-result #:query-result-p #:query-result-solutions
   ;; reader syntax
   #:*nsk-readtable* #:install-nsk-syntax #:enable-nsk-syntax
   #:nsk-read-from-string
   ;; json helpers
   #:json-encode #:json-parse #:json-get
   ;; neural layer
   #:*ollama-url* #:*ollama-model* #:query-neural-fallback
   #:sanitize-to-keyword #:text->triples #:ingest-text
   ;; server
   #:start-server #:stop-server
   ;; entry points
   #:repl #:main #:version))
