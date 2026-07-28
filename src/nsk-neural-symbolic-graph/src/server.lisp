;;;; server.lisp --- Optional REST server (Hunchentoot) for the --serve flag.
;;;;
;;;; Hunchentoot is referenced only through FIND-SYMBOL at call time, so this
;;;; file compiles and loads under a bare image. START-SERVER loads the library
;;;; on demand through Quicklisp.

(in-package :nsk)

(defvar *acceptor* nil "The running Hunchentoot acceptor, if any.")

(defun ensure-hunchentoot ()
  "Make sure the HUNCHENTOOT package is loaded; load it via Quicklisp if not."
  (unless (find-package :hunchentoot)
    (let ((quickload (and (find-package :ql) (find-symbol "QUICKLOAD" :ql))))
      (unless quickload
        (error "Quicklisp is not available to load hunchentoot."))
      (funcall quickload :hunchentoot :silent t)))
  (or (find-package :hunchentoot)
      (error "Could not load hunchentoot.")))

(defun hsym (name)
  "Resolve an exported HUNCHENTOOT symbol by NAME at call time."
  (or (find-symbol (string name) :hunchentoot)
      (error "hunchentoot symbol ~a not found" name)))

(defun set-content-type-json ()
  (funcall (fdefinition (list 'setf (hsym "CONTENT-TYPE*"))) "application/json"))

;;; Request fields map to graph terms: "?x" is a variable, null means "any",
;;; and any other string becomes a keyword.

(defun field->term (value role)
  (cond ((or (null value) (eq value :null)) (logic-var role))
        ((and (stringp value) (plusp (length value)) (char= (char value 0) #\?))
         (logic-var (intern (string-upcase (subseq value 1)) :keyword)))
        ((stringp value) (sanitize-to-keyword value))
        (t value)))

(defun term->json (term)
  (cond ((keywordp term) (string-downcase (symbol-name term)))
        ((stringp term) term)
        ((logic-var-p term) (format nil "?~a" (logic-var-name term)))
        (t (princ-to-string term))))

(defun solution->json (solution)
  (cons :object
        (mapcar (lambda (binding)
                  (cons (string-downcase (symbol-name (car binding)))
                        (term->json (cdr binding))))
                solution)))

(defun handle-query ()
  "POST /query with a JSON body {\"subject\":..,\"predicate\":..,\"object\":..}.
   Null or \"?name\" fields are variables. Returns the matching solutions."
  (set-content-type-json)
  (let* ((raw (funcall (hsym "RAW-POST-DATA") :force-text t))
         (request (and raw (ignore-errors (json-parse raw))))
         (s (field->term (json-get request "subject") :subject))
         (p (field->term (json-get request "predicate") :predicate))
         (o (field->term (json-get request "object") :object))
         (sols (solutions (match-triple s p o))))
    (json-encode
     (list :object
           (cons "count" (length sols))
           (cons "results" (cons :array (mapcar #'solution->json sols)))))))

(defun handle-health ()
  "GET /health returns basic engine status."
  (set-content-type-json)
  (json-encode (list :object
                     (cons "status" "ok")
                     (cons "triples" (triple-count))
                     (cons "model" *ollama-model*))))

(defun start-server (&optional (port 8800))
  "Load Hunchentoot if needed and start serving /query and /health on PORT."
  (ensure-hunchentoot)
  (when *acceptor* (stop-server))
  (let ((table (find-symbol "*DISPATCH-TABLE*" :hunchentoot))
        (prefix (hsym "CREATE-PREFIX-DISPATCHER")))
    (set table (list (funcall prefix "/query" 'handle-query)
                     (funcall prefix "/health" 'handle-health))))
  (setf *acceptor* (make-instance (hsym "EASY-ACCEPTOR") :port port))
  (funcall (hsym "START") *acceptor*)
  *acceptor*)

(defun stop-server ()
  "Stop the running acceptor, if any."
  (when *acceptor*
    (funcall (hsym "STOP") *acceptor*)
    (setf *acceptor* nil))
  t)
