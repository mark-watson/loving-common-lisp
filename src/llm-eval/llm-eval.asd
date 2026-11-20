;;;; llm-eval.asd -- testing and evaluating LLM-based code.

(asdf:defsystem #:llm-eval
  :description "Evaluation harness for LLM applications: golden datasets, scoring, LLM-as-judge, and run history in SQLite."
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:litelm #:sqlite)
  :serial t
  :components ((:file "package")
               (:file "dataset")
               (:file "scoring")
               (:file "judge")
               (:file "harness")
               (:file "store"))
  :in-order-to ((asdf:test-op (asdf:test-op #:llm-eval/tests))))

(asdf:defsystem #:llm-eval/rag
  :description "Evaluate the agentic RAG example from the RAG chapter."
  :depends-on (#:llm-eval #:rag)
  :components ((:file "rag-eval")))

(asdf:defsystem #:llm-eval/tests
  :description "FiveAM unit tests. They run offline: every model call is faked."
  :depends-on (#:llm-eval #:rag #:fiveam)
  :components ((:file "tests"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :all-tests :llm-eval-tests))))
