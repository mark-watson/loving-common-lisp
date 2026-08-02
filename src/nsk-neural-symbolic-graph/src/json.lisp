;;;; json.lisp --- A small, self-contained JSON reader and writer.
;;;;
;;;; NSK keeps its own JSON code so the core has no external dependencies and
;;;; can load and run under a bare LispWorks image. The writer takes a tagged
;;;; Lisp form; the reader returns alists for objects and lists for arrays.

(in-package :nsk)

;;; Writer

(defun json-write-string (string stream)
  (write-char #\" stream)
  (loop for ch across string do
    (case ch
      (#\" (write-string "\\\"" stream))
      (#\\ (write-string "\\\\" stream))
      (#\Newline (write-string "\\n" stream))
      (#\Return (write-string "\\r" stream))
      (#\Tab (write-string "\\t" stream))
      (#\Backspace (write-string "\\b" stream))
      (#\Page (write-string "\\f" stream))
      (t (if (< (char-code ch) #x20)
             (format stream "\\u~4,'0x" (char-code ch))
             (write-char ch stream)))))
  (write-char #\" stream))

(defun json-encode (value &optional stream)
  "Encode VALUE as JSON. Objects are (:object (key . val) ...); arrays are
   (:array val ...); literals are :true, :false, :null. With no STREAM,
   return a string."
  (if stream
      (%json-encode value stream)
      (with-output-to-string (s) (%json-encode value s))))

(defun %json-encode (value stream)
  (cond
    ((stringp value) (json-write-string value stream))
    ((integerp value) (princ value stream))
    ((floatp value) (format stream "~f" value))
    ((eq value :true) (write-string "true" stream))
    ((eq value :false) (write-string "false" stream))
    ((eq value :null) (write-string "null" stream))
    ((and (consp value) (eq (car value) :object))
     (write-char #\{ stream)
     (loop for (pair . more) on (cdr value) do
       (json-write-string (string (car pair)) stream)
       (write-char #\: stream)
       (%json-encode (cdr pair) stream)
       (when more (write-char #\, stream)))
     (write-char #\} stream))
    ((and (consp value) (eq (car value) :array))
     (write-char #\[ stream)
     (loop for (v . more) on (cdr value) do
       (%json-encode v stream)
       (when more (write-char #\, stream)))
     (write-char #\] stream))
    (t (json-write-string (princ-to-string value) stream))))

;;; Reader

(define-condition json-error (error)
  ((message :initarg :message :reader json-error-message))
  (:report (lambda (c s) (format s "JSON parse error: ~a" (json-error-message c)))))

(defstruct (json-cursor (:constructor make-json-cursor (string)))
  (string "" :type string)
  (pos 0 :type fixnum))

(defun jc-peek (c)
  (let ((s (json-cursor-string c)) (p (json-cursor-pos c)))
    (when (< p (length s)) (char s p))))

(defun jc-next (c)
  (prog1 (jc-peek c) (incf (json-cursor-pos c))))

(defun jc-skip-ws (c)
  (loop for ch = (jc-peek c)
        while (and ch (member ch '(#\Space #\Tab #\Newline #\Return)))
        do (jc-next c)))

(defun json-parse (string)
  "Parse a JSON document into Lisp data. Objects become alists of
   (string-key . value); arrays become lists; strings stay strings; numbers
   parse to numbers; true/false/null become :true/:false/:null."
  (let ((c (make-json-cursor string)))
    (prog1 (json-parse-value c) (jc-skip-ws c))))

(defun json-parse-value (c)
  (jc-skip-ws c)
  (let ((ch (jc-peek c)))
    (cond ((null ch) (error 'json-error :message "unexpected end of input"))
          ((char= ch #\{) (json-parse-object c))
          ((char= ch #\[) (json-parse-array c))
          ((char= ch #\") (json-parse-string c))
          ((or (digit-char-p ch) (char= ch #\-)) (json-parse-number c))
          ((char= ch #\t) (json-parse-literal c "true" :true))
          ((char= ch #\f) (json-parse-literal c "false" :false))
          ((char= ch #\n) (json-parse-literal c "null" :null))
          (t (error 'json-error :message (format nil "unexpected char ~a" ch))))))

(defun json-parse-literal (c text value)
  (loop for expected across text
        for got = (jc-next c)
        unless (and got (char= got expected))
          do (error 'json-error :message (format nil "bad literal, expected ~a" text)))
  value)

(defun json-parse-object (c)
  (jc-next c)                           ; consume {
  (jc-skip-ws c)
  (if (eql (jc-peek c) #\})
      (progn (jc-next c) '())
      (let ((pairs '()))
        (loop
          (jc-skip-ws c)
          (let ((key (json-parse-string c)))
            (jc-skip-ws c)
            (unless (eql (jc-next c) #\:)
              (error 'json-error :message "expected : after object key"))
            (push (cons key (json-parse-value c)) pairs))
          (jc-skip-ws c)
          (let ((ch (jc-next c)))
            (cond ((eql ch #\,) nil)
                  ((eql ch #\}) (return (nreverse pairs)))
                  (t (error 'json-error :message "expected , or } in object"))))))))

(defun json-parse-array (c)
  (jc-next c)                           ; consume [
  (jc-skip-ws c)
  (if (eql (jc-peek c) #\])
      (progn (jc-next c) '())
      (let ((items '()))
        (loop
          (push (json-parse-value c) items)
          (jc-skip-ws c)
          (let ((ch (jc-next c)))
            (cond ((eql ch #\,) nil)
                  ((eql ch #\]) (return (nreverse items)))
                  (t (error 'json-error :message "expected , or ] in array"))))))))

(defun json-parse-string (c)
  (unless (eql (jc-next c) #\")
    (error 'json-error :message "expected a string"))
  (let ((out (make-string-output-stream)))
    (loop for ch = (jc-next c) do
      (cond ((null ch) (error 'json-error :message "unterminated string"))
            ((char= ch #\") (return))
            ((char= ch #\\)
             (let ((esc (jc-next c)))
               (case esc
                 (#\" (write-char #\" out))
                 (#\\ (write-char #\\ out))
                 (#\/ (write-char #\/ out))
                 (#\b (write-char #\Backspace out))
                 (#\f (write-char #\Page out))
                 (#\n (write-char #\Newline out))
                 (#\r (write-char #\Return out))
                 (#\t (write-char #\Tab out))
                 (#\u (write-char (code-char (json-parse-hex c 4)) out))
                 (t (error 'json-error :message "bad string escape")))))
            (t (write-char ch out))))
    (get-output-stream-string out)))

(defun json-parse-hex (c n)
  (let ((val 0))
    (dotimes (i n val)
      (let ((d (digit-char-p (jc-next c) 16)))
        (unless d (error 'json-error :message "bad \\u escape"))
        (setf val (+ (* val 16) d))))))

(defun json-parse-number (c)
  (let ((start (json-cursor-pos c)))
    (when (eql (jc-peek c) #\-) (jc-next c))
    (loop for ch = (jc-peek c)
          while (and ch (or (digit-char-p ch) (member ch '(#\. #\e #\E #\+ #\-))))
          do (jc-next c))
    (let ((token (subseq (json-cursor-string c) start (json-cursor-pos c))))
      (if (find-if (lambda (ch) (member ch '(#\. #\e #\E))) token)
          (let ((*read-default-float-format* 'double-float)
                (*read-eval* nil))
            (read-from-string token))
          (parse-integer token)))))

(defun json-get (object key &optional default)
  "Look up KEY (a string) in an object alist produced by JSON-PARSE."
  (let ((pair (and (listp object) (assoc key object :test #'string=))))
    (if pair (cdr pair) default)))
