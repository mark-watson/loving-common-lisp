;;;; resilient-llm.asd -- conditions and restarts for robust LLM calls.

(asdf:defsystem #:resilient-llm
  :description "Condition system tutorial, plus retry, fallback, and JSON repair policies for LLM calls."
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:litelm #:usocket #:com.inuoe.jzon)
  :serial t
  :components ((:file "package")
               (:file "basics")
               (:file "resilient-llm")))

(asdf:defsystem #:resilient-llm/tests
  :description "Offline tests for resilient-llm. They use fake models, so no LLM server is needed."
  :depends-on (#:resilient-llm)
  :components ((:file "tests")))
