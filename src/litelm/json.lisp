;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License
;;;
;;; Minimal self-contained JSON encoder/decoder.
;;;
;;; Encoding rules (Lisp -> JSON):
;;;   string                              -> JSON string
;;;   integer / float                     -> JSON number
;;;   t  :false  :null                    -> true / false / null
;;;   alist with string or keyword keys   -> JSON object
;;;                                          (:max-tokens -> "max_tokens")
;;;   any other list                      -> JSON array
;;;   any other symbol                    -> JSON string (downcased)
;;;
;;; Decoding rules (JSON -> Lisp):
;;;   object -> alist with string keys, array -> list, string -> string,
;;;   number -> number, true -> t, false/null -> nil

(in-package #:litelm)

;;; ---- encoder ----

(defun json-key (key)
  "Convert a string or keyword to a JSON object key (downcased, - -> _)."
  (etypecase key
    (string key)
    (symbol (substitute #\_ #\- (string-downcase (symbol-name key))))))

(defun %write-json-string (s out)
  (write-char #\" out)
  (loop for ch across s do
    (case ch
      (#\" (write-string "\\\"" out))
      (#\\ (write-string "\\\\" out))
      (#\Newline (write-string "\\n" out))
      (#\Return (write-string "\\r" out))
      (#\Tab (write-string "\\t" out))
      (t (if (< (char-code ch) 32)
             (format out "\\u~4,'0x" (char-code ch))
             (write-char ch out)))))
  (write-char #\" out))

(defun %write-json-float (f out)
  (if (and (<= -1.0d15 f 1.0d15) (= f (truncate f)))
      (format out "~D.0" (truncate f))
      ;; ~G prints the shortest round-trip representation (Burger-Dybvig)
      (let ((s (string-right-trim " " (format nil "~G" f))))
        (write-string s out)
        (when (char= (char s (1- (length s))) #\.)
          (write-char #\0 out)))))

(defun %json-alist-p (x)
  (and (consp x)
       (every (lambda (e) (and (consp e) (or (stringp (car e)) (symbolp (car e)))))
              x)))

(defun write-json (x out)
  (cond
    ((stringp x) (%write-json-string x out))
    ((eq x t) (write-string "true" out))
    ((eq x :false) (write-string "false" out))
    ((eq x :null) (write-string "null" out))
    ((integerp x) (princ x out))
    ((floatp x) (%write-json-float x out))
    ((realp x) (%write-json-float (coerce x 'double-float) out))
    ((%json-alist-p x)
     (write-char #\{ out)
     (loop for (k . v) in x for first = t then nil do
       (unless first (write-char #\, out))
       (%write-json-string (json-key k) out)
       (write-char #\: out)
       (write-json v out))
     (write-char #\} out))
    ((listp x)
     (write-char #\[ out)
     (loop for e in x for first = t then nil do
       (unless first (write-char #\, out))
       (write-json e out))
     (write-char #\] out))
    ((symbolp x) (%write-json-string (string-downcase (symbol-name x)) out))
    (t (error "Cannot JSON-encode ~S" x))))

(defun json-encode (x)
  "Encode the nested list structure X as a JSON string."
  (with-output-to-string (out) (write-json x out)))

;;; ---- decoder ----

(defun json-decode (string)
  "Decode a JSON string into nested alists (string keys), lists, strings,
numbers, t (true) and nil (false/null)."
  (let ((pos 0) (len (length string)))
    (labels ((peek () (and (< pos len) (char string pos)))
             (advance () (incf pos))
             (skip-ws ()
               (loop while (and (peek) (member (peek) '(#\Space #\Tab #\Newline #\Return)))
                     do (advance)))
             (expect (ch)
               (unless (eql (peek) ch)
                 (error "JSON parse error at ~D: expected ~S in ~S" pos ch string))
               (advance))
             (parse-string ()
               (expect #\")
               (with-output-to-string (out)
                 (loop for ch = (peek) do
                   (cond ((null ch) (error "Unterminated JSON string"))
                         ((char= ch #\") (advance) (return))
                         ((char= ch #\\)
                          (advance)
                          (let ((esc (peek)))
                            (advance)
                            (case esc
                              (#\n (write-char #\Newline out))
                              (#\t (write-char #\Tab out))
                              (#\r (write-char #\Return out))
                              (#\b (write-char #\Backspace out))
                              (#\f (write-char #\Page out))
                              (#\u (write-char (code-char (parse-integer string
                                                                          :start pos
                                                                          :end (+ pos 4)
                                                                          :radix 16))
                                               out)
                                   (incf pos 4))
                              (t (write-char esc out)))))
                         (t (write-char ch out) (advance))))))
             (parse-number ()
               (let ((start pos))
                 (when (eql (peek) #\-) (advance))
                 (loop while (and (peek) (digit-char-p (peek))) do (advance))
                 (let ((is-float nil))
                   (when (eql (peek) #\.)
                     (setf is-float t) (advance)
                     (loop while (and (peek) (digit-char-p (peek))) do (advance)))
                   (when (and (peek) (member (peek) '(#\e #\E)))
                     (setf is-float t) (advance)
                     (when (and (peek) (member (peek) '(#\+ #\-))) (advance))
                     (loop while (and (peek) (digit-char-p (peek))) do (advance)))
                   (let ((token (string-trim " " (subseq string start pos))))
                     (if is-float
                         (let ((*read-default-float-format* 'double-float))
                           (read-from-string token))
                         (parse-integer token))))))
             (parse-array ()
               (expect #\[) (skip-ws)
               (if (eql (peek) #\])
                   (progn (advance) nil)
                   (loop collect (parse-value) into items
                         do (skip-ws)
                            (cond ((eql (peek) #\,) (advance) (skip-ws))
                                  ((eql (peek) #\]) (advance) (return items))
                                  (t (error "JSON array parse error at ~D" pos))))))
             (parse-object ()
               (expect #\{) (skip-ws)
               (if (eql (peek) #\})
                   (progn (advance) nil)
                   (loop collect (let ((k (parse-string)))
                                   (skip-ws) (expect #\:) (skip-ws)
                                   (cons k (parse-value)))
                           into pairs
                         do (skip-ws)
                            (cond ((eql (peek) #\,) (advance) (skip-ws))
                                  ((eql (peek) #\}) (advance) (return pairs))
                                  (t (error "JSON object parse error at ~D" pos))))))
             (parse-literal (word value)
               (unless (and (<= (+ pos (length word)) len)
                            (string= word string :start2 pos :end2 (+ pos (length word))))
                 (error "JSON literal parse error at ~D" pos))
               (incf pos (length word))
               value)
             (parse-value ()
               (skip-ws)
               (let ((ch (peek)))
                 (cond
                   ((null ch) (error "Unexpected end of JSON input"))
                   ((char= ch #\") (parse-string))
                   ((char= ch #\{) (parse-object))
                   ((char= ch #\[) (parse-array))
                   ((char= ch #\t) (parse-literal "true" t))
                   ((char= ch #\f) (parse-literal "false" nil))
                   ((char= ch #\n) (parse-literal "null" nil))
                   ((or (digit-char-p ch) (char= ch #\-)) (parse-number))
                   (t (error "JSON parse error at ~D: ~S" pos ch))))))
      (parse-value))))

(defun aget (alist key)
  "Fetch KEY (string) from a decoded JSON alist."
  (cdr (assoc key alist :test #'equal)))
