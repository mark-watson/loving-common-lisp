(in-package :cl-user)

(defpackage :cl-llm-agent-tests
  (:use :cl :fiveam :cl-llm-agent))

(in-package :cl-llm-agent-tests)

(defsuite cl-llm-agent-tests)
(in-suite cl-llm-agent-tests)

(def-test context-set-get-remove
  (let ((ctx (make-context)))
    (context-set ctx "foo" 42)
    (is (= (context-get ctx "foo") 42))
    (context-remove ctx "foo")
    (is (null (context-get ctx "foo")))))

(def-test tool-registration-and-execution
  (register-tool "dummy"
                 :description "dummy"
                 :parameters '(x)
                 :parameter-example "x: number"
                 :function (lambda (x) (* 2 x)))
  (is (= (execute-tool "dummy" 21) 42))
  (remhash "dummy" *tool-registry*))

(run! 'cl-llm-agent-tests)