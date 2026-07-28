;;;; neural.lisp --- The neural integration layer (local Ollama daemon).
;;;;
;;;; When a symbolic query fails on a ~ predicate, NSK asks the model to infer
;;;; the missing object. The same layer turns free text into triples. HTTP goes
;;;; through dexador when it is loaded, otherwise through a native LispWorks
;;;; socket, so the core keeps no hard dependency on an HTTP library.

(in-package :nsk)

(defparameter *ollama-url* "http://localhost:11434"
  "Base URL of the local Ollama daemon.")

(defparameter *ollama-model* "qwen3.5:4b"
  "Model used for inference and text extraction.")

(defparameter *ollama-timeout* 60
  "Socket timeout, in seconds, for Ollama requests.")

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

;;; HTTP transport

(defun parse-url (url)
  "Return (values host port path) for a simple http URL."
  (let* ((mark (search "://" url))
         (rest (if mark (subseq url (+ mark 3)) url))
         (slash (position #\/ rest))
         (authority (if slash (subseq rest 0 slash) rest))
         (path (if slash (subseq rest slash) "/"))
         (colon (position #\: authority))
         (host (if colon (subseq authority 0 colon) authority))
         (port (if colon (parse-integer authority :start (1+ colon)) 80)))
    (values host port path)))

(defun http-post-json (url body)
  "POST BODY (a JSON string) to URL and return the response body string."
  (let ((dex-post (and (find-package :dexador)
                       (find-symbol "POST" :dexador))))
    (cond
      (dex-post
       (funcall dex-post url :content body
                :headers '(("Content-Type" . "application/json"))))
      ((and (find-package :comm) (find-symbol "OPEN-TCP-STREAM" :comm))
       (native-http-post-json url body))
      (t (error "No HTTP client available; load dexador or run on LispWorks.")))))

(defun native-http-post-json (url body)
  "POST using a raw LispWorks TCP socket. Resolved dynamically so this file
   compiles without the COMM package present."
  (let ((open-fn (find-symbol "OPEN-TCP-STREAM" :comm))
        (crlf (coerce (list #\Return #\Linefeed) 'string)))
    (multiple-value-bind (host port path) (parse-url url)
      (let ((stream (funcall open-fn host port
                             :read-timeout *ollama-timeout*
                             :element-type 'base-char)))
        (unless stream (error "Cannot connect to ~a:~a" host port))
        (unwind-protect
             (progn
               (write-string (format nil "POST ~a HTTP/1.1~a" path crlf) stream)
               (write-string (format nil "Host: ~a:~a~a" host port crlf) stream)
               (write-string (format nil "Content-Type: application/json~a" crlf) stream)
               (write-string (format nil "Content-Length: ~a~a" (length body) crlf) stream)
               (write-string (format nil "Connection: close~a~a" crlf crlf) stream)
               (write-string body stream)
               (force-output stream)
               (read-http-body stream))
          (close stream))))))

(defun read-http-body (stream)
  "Read an HTTP response from STREAM and return only the body."
  (read-line stream nil "")             ; status line
  (let ((chunked nil) (length nil))
    (loop for line = (read-line stream nil nil)
          while line
          for trimmed = (string-right-trim '(#\Return) line)
          until (string= trimmed "")
          do (let ((low (string-downcase trimmed)))
               (cond ((and (>= (length low) 18)
                           (string= "transfer-encoding:" low :end2 18)
                           (search "chunked" low))
                      (setf chunked t))
                     ((and (>= (length low) 15)
                           (string= "content-length:" low :end2 15))
                      (setf length (parse-integer low :start 15 :junk-allowed t))))))
    (cond (chunked (read-chunked-body stream))
          (length (read-n-chars stream length))
          (t (read-to-eof stream)))))

(defun read-n-chars (stream n)
  (let* ((buf (make-string n))
         (got (read-sequence buf stream)))
    (subseq buf 0 got)))

(defun read-to-eof (stream)
  (with-output-to-string (out)
    (loop for ch = (read-char stream nil nil)
          while ch do (write-char ch out))))

(defun read-chunked-body (stream)
  (with-output-to-string (out)
    (loop
      (let* ((line (string-right-trim '(#\Return) (read-line stream nil "")))
             (semi (position #\; line))
             (size (parse-integer line :radix 16
                                       :end (or semi (length line))
                                       :junk-allowed t)))
        (when (or (null size) (zerop size)) (return))
        (write-string (read-n-chars stream size) out)
        (read-line stream nil "")))))    ; trailing CRLF after each chunk

;;; Ollama calls

(defun ollama-generate (prompt system)
  "Send a /api/generate request and return the model's raw response string."
  (let* ((payload (json-encode
                   (list :object
                         (cons "model" *ollama-model*)
                         (cons "system" system)
                         (cons "prompt" prompt)
                         (cons "format" "json")
                         (cons "stream" :false))))
         (raw (http-post-json (format nil "~a/api/generate" *ollama-url*) payload))
         (outer (json-parse raw)))
    (json-get outer "response")))

(defun query-neural-fallback (subject predicate)
  "Ask the model to infer the object for (SUBJECT PREDICATE). Return a string,
   or NIL if the daemon is unreachable or gives nothing."
  (handler-case
      (let* ((prompt (format nil "Subject: ~a. Predicate: ~a. What is the Object?"
                             (term-label subject) (term-label predicate)))
             (response (ollama-generate prompt *inference-system*)))
        (when (and response (stringp response))
          (let* ((inner (ignore-errors (json-parse response)))
                 (result (and (consp inner) (json-get inner "result"))))
            (cond ((and result (stringp result) (plusp (length result))) result)
                  ((plusp (length response)) response)
                  (t nil)))))
    (error (e)
      (format *error-output* "~&; neural fallback unavailable: ~a~%" e)
      nil)))

(defun text->triples (text)
  "Use the model to parse TEXT into a list of (S P O) keyword triples."
  (handler-case
      (let* ((response (ollama-generate text *extraction-system*))
             (inner (and response (stringp response) (json-parse response)))
             (rows (and (consp inner) (json-get inner "triples"))))
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
