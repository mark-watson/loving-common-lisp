;;;; mcp-server.lisp -- a minimal Model Context Protocol server.
;;;;
;;;; MCP stdio transport: the client starts this process and sends one
;;;; JSON-RPC 2.0 message per line on stdin. We write one JSON message per
;;;; line on stdout. Stdout must carry nothing else, so all logging goes
;;;; to stderr.

(in-package #:mcp-server)

(defparameter *server-name* "common-lisp-macos")
(defparameter *server-version* "0.1.0")

;; Newest first. We echo the client's version if we support it,
;; otherwise we answer with our newest version.
(defparameter *supported-protocol-versions*
  '("2025-11-25" "2025-06-18" "2025-03-26" "2024-11-05"))

;;; JSON helpers. jzon maps JSON objects to EQUAL hash tables, arrays to
;;; vectors, true to T, false to NIL, and null to the symbol NULL.

(defun obj (&rest keys-and-values)
  "Build a JSON object from alternating string keys and values."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on keys-and-values by #'cddr
          do (setf (gethash key table) value))
    table))

(defun json-get (object key &optional default)
  (if (hash-table-p object)
      (gethash key object default)
      default))

(defun log-message (format-control &rest args)
  (format *error-output* "~&[mcp] ~?~%" format-control args)
  (finish-output *error-output*))

;;; Tool registry

(defstruct tool
  name          ; string sent to the client, e.g. "add_reminder"
  description
  input-schema  ; JSON Schema object (hash table)
  function)     ; function of one argument: the arguments hash table

(defvar *tools* (make-hash-table :test #'equal)
  "Map from tool name to TOOL struct.")

(defun lisp-name->json-name (symbol)
  (substitute #\_ #\- (string-downcase (symbol-name symbol))))

(defun json-type-p (value json-type)
  (cond ((string= json-type "string") (stringp value))
        ((string= json-type "integer") (integerp value))
        ((string= json-type "number") (realp value))
        ((string= json-type "boolean") (or (eq value t) (eq value nil)))
        (t t)))

(defun parameter-value (arguments json-name json-type requiredp default)
  "Fetch and type check one tool argument. An LLM writes these values, so
never trust them: a string where we expect an integer can turn into SQL
or AppleScript injection further down."
  (multiple-value-bind (value presentp) (gethash json-name arguments)
    (cond ((not presentp)
           (if requiredp
               (error "Missing required argument: ~A" json-name)
               default))
          ((not (json-type-p value json-type))
           (error "Argument ~A must be of type ~A" json-name json-type))
          (t value))))

(defun make-input-schema (parameters)
  (let ((properties (make-hash-table :test #'equal))
        (required-names '()))
    (dolist (parameter parameters)
      (destructuring-bind (var json-type description &key required default)
          parameter
        (declare (ignore default))
        (let ((json-name (lisp-name->json-name var)))
          (setf (gethash json-name properties)
                (obj "type" json-type "description" description))
          (when required (push json-name required-names)))))
    (obj "type" "object"
         "properties" properties
         "required" (coerce (nreverse required-names) 'vector))))

(defmacro deftool (name (&rest parameters) description &body body)
  "Define an MCP tool. Each parameter is
  (VAR JSON-TYPE DESCRIPTION &key REQUIRED DEFAULT)
VAR is bound to the argument value inside BODY. BODY returns the tool's
text result; a non-string result is converted with PRINC-TO-STRING."
  (let ((arguments (gensym "ARGUMENTS")))
    `(setf (gethash ,(lisp-name->json-name name) *tools*)
           (make-tool
            :name ,(lisp-name->json-name name)
            :description ,description
            :input-schema (make-input-schema ',parameters)
            :function
            (lambda (,arguments)
              (declare (ignorable ,arguments))
              (let* ,(loop for (var json-type nil . options) in parameters
                           collect `(,var (parameter-value
                                           ,arguments
                                           ,(lisp-name->json-name var)
                                           ,json-type
                                           ,(getf options :required)
                                           ,(getf options :default))))
                ,@body))))))

;;; JSON-RPC

(define-condition json-rpc-error (error)
  ((code :initarg :code :reader json-rpc-error-code)
   (message :initarg :message :reader json-rpc-error-message))
  (:report (lambda (condition stream)
             (format stream "JSON-RPC error ~D: ~A"
                     (json-rpc-error-code condition)
                     (json-rpc-error-message condition)))))

(defun rpc-error (code format-control &rest args)
  (error 'json-rpc-error :code code
                         :message (apply #'format nil format-control args)))

(defun success-response (id result)
  (obj "jsonrpc" "2.0" "id" id "result" result))

(defun error-response (id code message)
  (obj "jsonrpc" "2.0" "id" id
       "error" (obj "code" code "message" message)))

;;; MCP methods

(defun handle-initialize (params)
  (let* ((requested (json-get params "protocolVersion"))
         (version (if (member requested *supported-protocol-versions*
                              :test #'equal)
                      requested
                      (first *supported-protocol-versions*))))
    (log-message "initialize: client asked for ~A, using ~A" requested version)
    (obj "protocolVersion" version
         "capabilities" (obj "tools" (obj "listChanged" nil))
         "serverInfo" (obj "name" *server-name* "version" *server-version*))))

(defun handle-tools-list ()
  (let ((tools (loop for tool being the hash-values of *tools*
                     collect (obj "name" (tool-name tool)
                                  "description" (tool-description tool)
                                  "inputSchema" (tool-input-schema tool)))))
    (obj "tools" (coerce (sort tools #'string< :key (lambda (tool) (gethash "name" tool)))
                         'vector))))

(defun tool-result (text &optional errorp)
  (obj "content" (vector (obj "type" "text" "text" text))
       "isError" errorp))

(defun handle-tools-call (params)
  (let* ((name (json-get params "name"))
         (arguments (json-get params "arguments"))
         (tool (and (stringp name) (gethash name *tools*))))
    (unless tool
      (rpc-error -32602 "Unknown tool: ~A" name))
    (unless (hash-table-p arguments)
      (setf arguments (make-hash-table :test #'equal)))
    (log-message "tools/call ~A" name)
    ;; MCP reports failures inside a tool as a normal result with isError
    ;; true, so the model can read the message and try again.
    (handler-case
        (let ((result (funcall (tool-function tool) arguments)))
          (tool-result (if (stringp result) result (princ-to-string result))))
      (error (condition)
        (log-message "tool ~A failed: ~A" name condition)
        (tool-result (princ-to-string condition) t)))))

(defun dispatch (method params)
  (cond ((equal method "initialize") (handle-initialize params))
        ((equal method "ping") (obj))
        ((equal method "tools/list") (handle-tools-list))
        ((equal method "tools/call") (handle-tools-call params))
        (t (rpc-error -32601 "Method not found: ~A" method))))

(defun handle-message (message)
  "Process one parsed JSON-RPC MESSAGE. Return the response object, or NIL
for notifications, which never get a response."
  (multiple-value-bind (id requestp)
      (if (hash-table-p message) (gethash "id" message) (values nil nil))
    (let ((method (json-get message "method"))
          (params (json-get message "params")))
      (cond ((not (stringp method))
             (and requestp (error-response id -32600 "Invalid request")))
            ((not requestp)
             (log-message "notification ~A" method)
             nil)
            (t
             (handler-case (success-response id (dispatch method params))
               (json-rpc-error (condition)
                 (error-response id (json-rpc-error-code condition)
                                 (json-rpc-error-message condition)))
               (error (condition)
                 (error-response id -32603 (princ-to-string condition)))))))))

(defun handle-line (line)
  (let ((message (handler-case (jzon:parse line)
                   (error () (return-from handle-line
                               (error-response 'null -32700 "Parse error"))))))
    (handle-message message)))

(defun start (&key (input *standard-input*) (output *standard-output*))
  "Read JSON-RPC messages from INPUT, one per line, until end of file."
  (log-message "~A ~A started with ~D tools"
               *server-name* *server-version* (hash-table-count *tools*))
  ;; Anything printed to *standard-output* would corrupt the protocol
  ;; stream, so point it at stderr while tools run.
  (let ((*standard-output* *error-output*))
    (loop for line = (read-line input nil)
          while line
          unless (zerop (length (string-trim '(#\Space #\Tab #\Return) line)))
            do (let ((response (handle-line line)))
                 (when response
                   ;; jzon escapes newlines inside strings, so each
                   ;; message fits on one line as the transport requires.
                   (write-line (jzon:stringify response) output)
                   (finish-output output)))))
  (log-message "stdin closed, exiting"))
