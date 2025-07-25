(defpackage :cl-llm-agent
  (:use :cl)
  (:export #:define-agent
           #:make-agent
           #:agent-converse
           #:agent-llm-call
           #:make-context
           #:context-data
           #:context-set
           #:context-get
           #:context-remove
           #:display-context
           #:register-tool
           #:list-tools
           #:execute-tool
           #:agent-tools
           #:agent-context
           #:*agent-verbose*))
