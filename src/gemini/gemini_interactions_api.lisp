(in-package #:gemini)

;;; ====================================================================
;;; Gemini Interactions API - Multi-turn conversations with tool use
;;; ====================================================================
;;;
;;; Supports multi-turn conversations combining Google Search (built-in)
;;; and custom function declarations (client-side tool use).
;;;
;;; Typical workflow:
;;;   1. Build function declarations with MAKE-FUNCTION-DECLARATION
;;;   2. Call GENERATE-WITH-TOOLS for Turn 1 -- returns TEXT, FUNCTION-CALLS, MODEL-CONTENT-HT
;;;   3. Invoke the functions yourself and collect results
;;;   4. Call CONTINUE-WITH-FUNCTION-RESPONSES for Turn 2 -- returns final TEXT
;;; ====================================================================


;;; ---- Internal utilities ----

(defun %symbol-name-to-camel-case (sym-name)
  "Converts a Lisp symbol name string (e.g. \"FUNCTION-CALL\") to camelCase (\"functionCall\").
   Consecutive hyphens (produced when cl-json decodes mixed snake_CamelCase keys) are collapsed."
  (let* ((raw-words (loop for start = 0 then (1+ pos)
                          for pos = (position #\- sym-name :start start)
                          collect (subseq sym-name start pos)
                          while pos))
         ;; Drop empty segments arising from consecutive hyphens (e.g. "SEARCH--SUGGESTIONS")
         (words (remove-if (lambda (w) (zerop (length w))) raw-words)))
    (if (null words)
        (string-downcase sym-name)
        (with-output-to-string (s)
          (write-string (string-downcase (first words)) s)
          (dolist (word (rest words))
            (write-char (char-upcase (char word 0)) s)
            (write-string (string-downcase (subseq word 1)) s))))))

(defun %is-decoded-alist-p (x)
  "Returns T if X looks like a cl-json decoded JSON object (alist with symbol keys)."
  (and (listp x) x (consp (first x)) (symbolp (caar x))))

(defun %decoded-to-ht (decoded)
  "Recursively converts a cl-json decoded value back to hash-tables for re-encoding.
   cl-json decodes JSON objects as alists with keyword keys (e.g. :FUNCTION-CALL).
   This inverts that, producing hash-tables with camelCase string keys so the
   value can be re-serialised faithfully with cl-json:encode-json-to-string."
  (cond
    ((%is-decoded-alist-p decoded)
     (let ((ht (make-hash-table :test 'equal)))
       (dolist (pair decoded ht)
         (let* ((key (%symbol-name-to-camel-case (symbol-name (car pair))))
                (val (%decoded-to-ht (cdr pair))))
           (setf (gethash key ht) val)))))
    ((listp decoded)
     (mapcar #'%decoded-to-ht decoded))
    (t decoded)))

(defun %make-content-ht (role parts)
  "Creates a content hash-table with role and parts list."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "role" ht) role
          (gethash "parts" ht) parts)
    ht))

(defun %make-text-part (text)
  "Creates a text part hash-table."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "text" ht) text)
    ht))

(defun %make-function-response-part (name id response-data)
  "Creates a functionResponse part hash-table.
   NAME: function name string
   ID: the function call ID returned by the model in Turn 1
   RESPONSE-DATA: the value to return as the function result (string, number, etc.)"
  (let ((resp-ht (make-hash-table :test 'equal))
        (fr-ht   (make-hash-table :test 'equal))
        (part-ht (make-hash-table :test 'equal)))
    ;; API expects: {"response": {"response": <data>}}
    (setf (gethash "response" resp-ht) response-data)
    (setf (gethash "name"     fr-ht) name
          (gethash "id"       fr-ht) id
          (gethash "response" fr-ht) resp-ht)
    (setf (gethash "functionResponse" part-ht) fr-ht)
    part-ht))

(defun %make-tools-list (function-declarations &optional google-search-p)
  "Builds the tools array for the API payload.
   FUNCTION-DECLARATIONS: list of hash-tables from MAKE-FUNCTION-DECLARATION, or NIL.
   GOOGLE-SEARCH-P: when T, prepends the built-in Google Search tool."
  (let ((tools '()))
    (when function-declarations
      (let ((fn-tool (make-hash-table :test 'equal)))
        (setf (gethash "functionDeclarations" fn-tool) function-declarations)
        (push fn-tool tools)))
    (when google-search-p
      (let ((gs-tool (make-hash-table :test 'equal)))
        (setf (gethash "googleSearch" gs-tool) (make-hash-table :test 'equal))
        (push gs-tool tools)))
    (nreverse tools)))

(defun %make-tool-config ()
  "Creates the toolConfig hash-table that asks the server to include its own tool invocations in the response."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "includeServerSideToolInvocations" ht) t)
    ht))

(defun %extract-function-calls (candidate)
  "Returns a list of plists (:NAME :ID :ARGS) for every functionCall part in CANDIDATE.
   ARGS is a cl-json decoded alist, e.g. ((:LOCATION . \"Barrow, AK\"))."
  (let* ((content (cdr (assoc :CONTENT candidate)))
         (parts   (cdr (assoc :PARTS content))))
    (loop for part in parts
          for fc-pair = (assoc :FUNCTION-CALL part)
          when fc-pair
          collect (let ((fc (cdr fc-pair)))
                    (list :name (cdr (assoc :NAME fc))
                          :id   (cdr (assoc :ID   fc))
                          :args (cdr (assoc :ARGS fc)))))))

(defun %get-text-from-candidate (candidate)
  "Returns the first text string found in CANDIDATE's parts, or NIL."
  (let* ((content (cdr (assoc :CONTENT candidate)))
         (parts   (cdr (assoc :PARTS content))))
    (loop for part in parts
          for text-pair = (assoc :TEXT part)
          when text-pair return (cdr text-pair))))


;;; ---- Public API ----

(defun make-function-declaration (name description parameters &optional required-params)
  "Creates a functionDeclaration hash-table suitable for GENERATE-WITH-TOOLS.

   NAME: string -- the function name the model will invoke
   DESCRIPTION: string -- natural-language description of what the function does
   PARAMETERS: list of (param-name type description) triples, e.g.:
     '((\"location\" \"STRING\" \"The city and state, e.g. San Francisco, CA\"))
   REQUIRED-PARAMS: optional list of required parameter name strings, e.g.:
     '(\"location\")

   Example:
     (make-function-declaration
       \"getWeather\"
       \"Get the weather in a given location\"
       '((\"location\" \"STRING\" \"The city and state, e.g. San Francisco, CA\"))
       '(\"location\"))"
  (let ((decl-ht  (make-hash-table :test 'equal))
        (params-ht (make-hash-table :test 'equal))
        (props-ht  (make-hash-table :test 'equal)))
    (dolist (param parameters)
      (destructuring-bind (pname ptype pdesc) param
        (let ((prop-ht (make-hash-table :test 'equal)))
          (setf (gethash "type"        prop-ht) ptype
                (gethash "description" prop-ht) pdesc)
          (setf (gethash pname props-ht) prop-ht))))
    (setf (gethash "type"       params-ht) "OBJECT"
          (gethash "properties" params-ht) props-ht)
    (when required-params
      (setf (gethash "required" params-ht) required-params))
    (setf (gethash "name"        decl-ht) name
          (gethash "description" decl-ht) description
          (gethash "parameters"  decl-ht) params-ht)
    decl-ht))

(defun generate-with-tools (prompt function-declarations
                            &key (model-id *model*) google-search-p)
  "Turn 1: Send PROMPT to the model with optional tool support.

   PROMPT: the user's text question.
   FUNCTION-DECLARATIONS: list of hash-tables from MAKE-FUNCTION-DECLARATION, or NIL.
   :GOOGLE-SEARCH-P: when T, enables the built-in Google Search tool.
   :MODEL-ID: model to use (defaults to *model*).

   Returns three values:
     TEXT             - model's text reply, or NIL when it chose to call functions.
     FUNCTION-CALLS   - list of plists (:NAME :ID :ARGS) for each function call made.
                        ARGS is a cl-json alist, e.g. ((:LOCATION . \"Barrow, AK\")).
     MODEL-CONTENT-HT - the model's content as a hash-table; pass this unchanged to
                        CONTINUE-WITH-FUNCTION-RESPONSES as the conversation history."
  (let* ((payload      (make-hash-table :test 'equal))
         (user-content (%make-content-ht "user" (list (%make-text-part prompt)))))
    (setf (gethash "contents"   payload) (list user-content)
          (gethash "tools"      payload) (%make-tools-list function-declarations google-search-p)
          (gethash "toolConfig" payload) (%make-tool-config))
    (let* ((api-url        (concatenate 'string *gemini-api-base-url* model-id ":generateContent"))
           (data           (cl-json:encode-json-to-string payload))
           (escaped-json   (escape-json data))
           (curl-cmd       (format nil
                             "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
                             api-url *google-api-key* escaped-json))
           (response-string  (run-curl-command curl-cmd))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (candidates       (cdr (assoc :CANDIDATES decoded-response)))
           (candidate        (first candidates))
           (text             (%get-text-from-candidate candidate))
           (function-calls   (%extract-function-calls candidate))
           (model-content-ht (%decoded-to-ht (cdr (assoc :CONTENT candidate)))))
      (values text function-calls model-content-ht))))

(defun continue-with-function-responses (original-prompt model-content-ht
                                         function-responses function-declarations
                                         &key (model-id *model*) google-search-p)
  "Turn 2+: Continue the conversation by supplying function call results.

   ORIGINAL-PROMPT: the same user text string passed to GENERATE-WITH-TOOLS in Turn 1.
   MODEL-CONTENT-HT: the third return value from GENERATE-WITH-TOOLS (the model's content).
   FUNCTION-RESPONSES: list of plists with keys :NAME :ID :RESPONSE, one per function call, e.g.:
     (list (list :name \"getWeather\" :id \"call_123\" :response \"Very cold. 22F.\"))
     The :ID must match the :ID from the corresponding FUNCTION-CALLS entry returned by Turn 1.
   FUNCTION-DECLARATIONS: same list used in GENERATE-WITH-TOOLS.
   :GOOGLE-SEARCH-P: whether to include the Google Search tool (match Turn 1 setting).
   :MODEL-ID: model to use (defaults to *model*).

   Returns the model's final text response string."
  (let* ((payload         (make-hash-table :test 'equal))
         ;; Rebuild Turn-1 user message
         (user-content-1  (%make-content-ht "user" (list (%make-text-part original-prompt))))
         ;; Turn-1 model response (already a correctly shaped hash-table)
         ;; Turn-2 user message carrying the function results
         (fn-parts        (mapcar (lambda (fr)
                                    (%make-function-response-part
                                     (getf fr :name)
                                     (getf fr :id)
                                     (getf fr :response)))
                                  function-responses))
         (user-content-2  (%make-content-ht "user" fn-parts)))
    (setf (gethash "contents"   payload) (list user-content-1 model-content-ht user-content-2)
          (gethash "tools"      payload) (%make-tools-list function-declarations google-search-p)
          (gethash "toolConfig" payload) (%make-tool-config))
    (let* ((api-url        (concatenate 'string *gemini-api-base-url* model-id ":generateContent"))
           (data           (cl-json:encode-json-to-string payload))
           (escaped-json   (escape-json data))
           (curl-cmd       (format nil
                             "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
                             api-url *google-api-key* escaped-json))
           (response-string  (run-curl-command curl-cmd))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (candidates       (cdr (assoc :CANDIDATES decoded-response)))
           (candidate        (first candidates)))
      (%get-text-from-candidate candidate))))

#|
;;; ---- Usage example ----

;; 1. Define a custom function the model can request
(defparameter *get-weather-fn*
  (gemini:make-function-declaration
   "getWeather"
   "Get the weather in a given location"
   '(("location" "STRING" "The city and state, e.g. San Francisco, CA"))
   '("location")))

;; 2. Turn 1 -- send the question with tools enabled
(multiple-value-bind (text function-calls model-content)
    (gemini:generate-with-tools
     "What is the northernmost city in the United States? What's the weather like there today?"
     (list *get-weather-fn*)
     :google-search-p t)
  (format t "Text: ~A~%" text)
  (format t "Function calls: ~A~%" function-calls)

  ;; 3. If the model requested function calls, handle them and continue
  (when function-calls
    (let* ((fc (first function-calls))
           ;; In a real application you would call the actual weather API here.
           ;; The :ARGS plist contains ((:LOCATION . "Barrow, AK")) or similar.
           (weather-result "Very cold. 22 degrees Fahrenheit.")
           (fn-responses
            (list (list :name (getf fc :name)
                        :id   (getf fc :id)
                        :response weather-result))))

      ;; 4. Turn 2 -- provide the function results, get the final answer
      (let ((final-answer
             (gemini:continue-with-function-responses
              "What is the northernmost city in the United States? What's the weather like there today?"
              model-content
              fn-responses
              (list *get-weather-fn*)
              :google-search-p t)))
        (format t "~%Final answer: ~A~%" final-answer)))))
|#
