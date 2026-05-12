;;; package.lisp -- Package definition for cl-ai-coding-agent
(defpackage :cl-ai-coding-agent
  (:use :cl)
  (:export #:coding-agent-query
           #:coding-agent-query-file
           #:coding-agent-repl
           #:*verbose*))
