;;;; neural.lisp --- The neural integration layer (local Ollama daemon).
;;;;
;;;; When a symbolic query fails on a ~ predicate, NSK asks the model to infer
;;;; the missing object. The same layer turns free text into triples.
;;;;
;;;; Model access goes through litelm, the book's provider-neutral client, so
;;;; this file carries no HTTP code and no request envelope of its own. The
;;;; litelm symbols are resolved at call time, which keeps NSK loadable on an
;;;; image where litelm is absent: a neural query then reports why it cannot
;;;; run, and the symbolic engine is unaffected.

(in-package :nsk)

(defparameter *ollama-url* "http://localhost:11434"
  "Base URL of the local Ollama daemon. litelm reaches the daemon through its
   OpenAI-compatible prefix, so NSK appends /v1 to this when calling out.")

(defparameter *ollama-model* "qwen3.5:4b"
  "Model used for inference and text extraction.")

(defparameter *inference-system*
  "You are a graph database inference node. Given a Subject and a Predicate, infer the single most likely Object. Reply ONLY as JSON: {\"result\": \"value\"}."
  "System prompt that constrains inference output to strict JSON.")

(defparameter *extraction-system*
  "You extract knowledge-graph triples from text. Reply ONLY as JSON of the form {\"triples\": [{\"subject\": \"..\", \"predicate\": \"..\", \"object\": \"..\"}]}. Use short lower-case tokens."
  "System prompt that constrains extraction output to strict JSON.")

;;; Term helpers

(defun term-label (term)
  "Readable label for a subject or predicate term."
  (cond ((neural-predicate-p term) (term-label (neural-predicate-name term)))
        ((keywordp term) (string-downcase (symbol-name term)))
        ((symbolp term) (string-downcase (symbol-name term)))
        ((stringp term) term)
        (t (princ-to-string term))))

(defun sanitize-to-keyword (string)
  "Convert an LLM string such as \"Common Lisp\" into the keyword :COMMON-LISP."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return #\. #\,) string))
         (clean (substitute #\- #\Space (string-upcase trimmed))))
    (intern clean :keyword)))

;;; The litelm bridge

(defun litelm-function (name)
  "Return the exported LITELM function NAME, or signal if litelm is absent.

   The lookup happens at call time rather than at read time, so this file still
   compiles and loads on an image where litelm is not installed -- the same
   trick the server uses for hunchentoot."
  (let ((symbol (and (find-package :litelm) (find-symbol name :litelm))))
    (unless (and symbol (fboundp symbol))
      (error "litelm is not loaded, so NSK cannot reach the local model; ~
              load it with (ql:quickload :litelm)."))
    (fdefinition symbol)))

(defun ollama-generate (prompt system)
  "Send PROMPT, under SYSTEM, to the local Ollama model through litelm.
   Returns the model's reply text."
  (let ((response (funcall (litelm-function "COMPLETION")
                           (format nil "ollama/~a" *ollama-model*)
                           :messages (list (list :system system)
                                           (list :user prompt))
                           :api-base (format nil "~a/v1" *ollama-url*))))
    (funcall (litelm-function "RESPONSE-CONTENT") response)))

;;; Reading the model's JSON
;;;
;;; The chat endpoint has no response-format flag that insists on bare JSON the
;;; way Ollama's native /api/generate does, so a reply can arrive wrapped in a
;;; Markdown fence or a sentence. JSON-OBJECT-IN walks the text from the first
;;; { to its matching }, ignoring braces inside strings, and parses that slice,
;;; which tolerates all three shapes.

(defun json-object-in (text)
  "Return the first complete JSON object in TEXT as an alist, or NIL."
  (when (stringp text)
    (let ((start (position #\{ text)))
      (when start
        (let ((depth 0) (in-string nil) (end nil) (i start) (n (length text)))
          (loop while (and (< i n) (null end)) do
            (let ((ch (char text i)))
              (cond (in-string
                     (cond ((char= ch #\\) (incf i))
                           ((char= ch #\") (setf in-string nil))))
                    ((char= ch #\") (setf in-string t))
                    ((char= ch #\{) (incf depth))
                    ((char= ch #\})
                     (decf depth)
                     (when (zerop depth) (setf end (1+ i))))))
            (incf i))
          (when end
            (ignore-errors (json-parse (subseq text start end)))))))))

;;; Ollama calls

(defun query-neural-fallback (subject predicate)
  "Ask the model to infer the object for (SUBJECT PREDICATE). Return a string,
   or NIL if litelm is unavailable, the daemon is unreachable, or the model
   gives nothing."
  (handler-case
      (let* ((prompt (format nil "Subject: ~a. Predicate: ~a. What is the Object?"
                             (term-label subject) (term-label predicate)))
             (reply (ollama-generate prompt *inference-system*))
             (result (let ((object (json-object-in reply)))
                       (and object (json-get object "result")))))
        (cond ((and result (stringp result) (plusp (length result))) result)
              ((and reply (stringp reply) (plusp (length reply))) reply)
              (t nil)))
    (error (e)
      (format *error-output* "~&; neural fallback unavailable: ~a~%" e)
      nil)))

(defun text->triples (text)
  "Use the model to parse TEXT into a list of (S P O) keyword triples."
  (handler-case
      (let* ((reply (ollama-generate text *extraction-system*))
             (object (json-object-in reply))
             (rows (and object (json-get object "triples"))))
        (loop for row in rows
              for s = (json-get row "subject")
              for p = (json-get row "predicate")
              for o = (json-get row "object")
              when (and (stringp s) (stringp p) (stringp o))
                collect (list (sanitize-to-keyword s)
                              (sanitize-to-keyword p)
                              (sanitize-to-keyword o))))
    (error (e)
      (format *error-output* "~&; extraction unavailable: ~a~%" e)
      nil)))

(defun ingest-text (text &optional (graph *graph*))
  "Extract triples from TEXT and add them to GRAPH. Return the triples added."
  (let ((triples (text->triples text)))
    (dolist (tr triples triples)
      (add-triple (first tr) (second tr) (third tr) graph))))
