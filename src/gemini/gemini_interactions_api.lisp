(in-package #:gemini)

;;; ====================================================================
;;; Gemini Interactions API - Multi-turn conversations with tool use
;;; ====================================================================
;;;
;;; Uses the v1beta Interactions API with the new steps schema.
;;; Supports multi-turn conversations combining Google Search (built-in)
;;; and custom function declarations (client-side tool use).
;;;
;;; Typical workflow:
;;;   1. Build function declarations with MAKE-FUNCTION-DECLARATION
;;;   2. Call GENERATE-WITH-TOOLS for Turn 1 -- returns TEXT, FUNCTION-CALLS, INTERACTION-ID
;;;   3. Invoke the functions yourself and collect results
;;;   4. Call CONTINUE-WITH-FUNCTION-RESPONSES for Turn 2 -- returns final TEXT
;;; ====================================================================


;;; ---- Internal utilities ----

(defun %make-tools-list (function-declarations &optional google-search-p)
  "Builds the tools array for the Interactions API payload.
   FUNCTION-DECLARATIONS: list of hash-tables from MAKE-FUNCTION-DECLARATION, or NIL.
   GOOGLE-SEARCH-P: when T, includes the built-in Google Search tool."
  (let ((tools '()))
    (when function-declarations
      (loop for fn-decl in function-declarations
            do (let ((fn-tool (make-hash-table :test 'equal)))
                 (setf (gethash "type" fn-tool) "function"
                       (gethash "name" fn-tool) (gethash "name" fn-decl)
                       (gethash "description" fn-tool) (gethash "description" fn-decl)
                       (gethash "parameters" fn-tool) (gethash "parameters" fn-decl))
                 (push fn-tool tools))))
    (when google-search-p
      (let ((gs-tool (make-hash-table :test 'equal)))
        (setf (gethash "type" gs-tool) "google_search")
        (push gs-tool tools)))
    (nreverse tools)))

(defun %extract-function-calls-from-steps (steps)
  "Returns a list of plists (:NAME :ID :ARGS) for every function_call step.
   ARGS is a cl-json decoded alist, e.g. ((:LOCATION . \"Barrow, AK\"))."
  (loop for step in steps
        when (string-equal (cdr (assoc :TYPE step)) "function_call")
        collect (list :name (cdr (assoc :NAME step))
                      :id   (cdr (assoc :ID   step))
                      :args (cdr (assoc :ARGUMENTS step)))))

(defun %get-text-from-steps (steps)
  "Returns the text from the last model_output step, or NIL."
  (loop for step in (reverse steps)
        when (string-equal (cdr (assoc :TYPE step)) "model_output")
        return (let* ((content (cdr (assoc :CONTENT step)))
                      (first-content (first content)))
                 (cdr (assoc :TEXT first-content)))))


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
  "Turn 1: Send PROMPT to the model with optional tool support via the Interactions API.

   PROMPT: the user's text question.
   FUNCTION-DECLARATIONS: list of hash-tables from MAKE-FUNCTION-DECLARATION, or NIL.
   :GOOGLE-SEARCH-P: when T, enables the built-in Google Search tool.
   :MODEL-ID: model to use (defaults to *model*).

   Returns three values:
     TEXT             - model's text reply, or NIL when it chose to call functions.
     FUNCTION-CALLS   - list of plists (:NAME :ID :ARGS) for each function call made.
                        ARGS is a cl-json alist, e.g. ((:LOCATION . \"Barrow, AK\")).
     INTERACTION-ID   - the interaction ID for use in follow-up turns."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "model" payload) model-id
          (gethash "input" payload) prompt
          (gethash "tools" payload) (%make-tools-list function-declarations google-search-p))
    (let* ((curl-cmd       (%interactions-curl-cmd payload))
           (response-string  (run-curl-command curl-cmd))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (steps            (cdr (assoc :STEPS decoded-response)))
           (interaction-id   (cdr (assoc :ID decoded-response)))
           (text             (%get-text-from-steps steps))
           (function-calls   (%extract-function-calls-from-steps steps)))
      (values text function-calls interaction-id))))

(defun continue-with-function-responses (interaction-id
                                         function-responses function-declarations
                                         &key (model-id *model*) google-search-p)
  "Turn 2+: Continue an interaction by supplying function call results.

   INTERACTION-ID: the interaction ID from GENERATE-WITH-TOOLS.
   FUNCTION-RESPONSES: list of plists with keys :NAME :ID :RESPONSE, one per function call, e.g.:
     (list (list :name \"getWeather\" :id \"fc_123\" :response \"Very cold. 22F.\"))
     The :ID must match the :ID from the corresponding FUNCTION-CALLS entry returned by Turn 1.
   FUNCTION-DECLARATIONS: same list used in GENERATE-WITH-TOOLS.
   :GOOGLE-SEARCH-P: whether to include the Google Search tool (match Turn 1 setting).
   :MODEL-ID: model to use (defaults to *model*).

   Returns the model's final text response string."
  (let* ((payload (make-hash-table :test 'equal))
         (fn-results (mapcar (lambda (fr)
                               (let ((result-ht (make-hash-table :test 'equal)))
                                 (setf (gethash "type" result-ht) "function_result"
                                       (gethash "call_id" result-ht) (getf fr :id)
                                       (gethash "result" result-ht) (getf fr :response))
                                 result-ht))
                             function-responses)))
    (setf (gethash "model" payload) model-id
          (gethash "interaction_id" payload) interaction-id
          (gethash "input" payload) fn-results
          (gethash "tools" payload) (%make-tools-list function-declarations google-search-p))
    (let* ((curl-cmd       (%interactions-curl-cmd payload))
           (response-string  (run-curl-command curl-cmd))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (steps            (cdr (assoc :STEPS decoded-response))))
      (%get-text-from-steps steps))))

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
(multiple-value-bind (text function-calls interaction-id)
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
              interaction-id
              fn-responses
              (list *get-weather-fn*)
              :google-search-p t)))
        (format t "~%Final answer: ~A~%" final-answer)))))
|#
