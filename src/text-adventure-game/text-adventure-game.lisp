;;;; text-adventure-game.lisp
;;;; Text adventure game using Ollama for AI-driven storytelling.
;;;;
;;;; Usage (LispWorks):
;;;;   (load "text-adventure-game.lisp")
;;;;   (text-adventure:play)
;;;;
;;;; Usage (SBCL):
;;;;   sbcl --load text-adventure-game.lisp --eval '(text-adventure:play)'

(ql:quickload :cl-json)

(defpackage #:text-adventure
  (:use #:cl #:cl-json)
  (:export #:play))

(in-package #:text-adventure)

(defvar *model-host* "http://localhost:11434/api/chat")
(defvar *model-name* "mistral")

(defun lisp-to-json-string (data)
  (with-output-to-string (s)
    (json:encode-json data s)))

(defun substitute-subseq (string old new &key (test #'eql))
  (let ((pos (search old string :test test)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (subseq string (+ pos (length old))))
        string)))

(defun load-story (filepath)
  (handler-case
      (with-open-file (f filepath :direction :input)
        (let ((content (make-string (file-length f))))
          (read-sequence content f)
          content))
    (file-error (e)
      (declare (ignore e))
      (format t "Error: ~a not found.~%" filepath)
      nil)))

(defun call-ollama (messages)
  "Send messages to Ollama chat API and return the assistant's response."
  (let* ((data (list (cons :model *model-name*)
                     (cons :stream nil)
                     (cons :messages messages)))
         (json-data (lisp-to-json-string data))
         ;; cl-json encodes nil as null, but stream needs false
         (fixed-json-data (substitute-subseq json-data ":null" ":false"
                                             :test #'string=))
         (curl-command
          (format nil "curl -s ~a -d ~s"
                  *model-host*
                  fixed-json-data)))
    (handler-case
        (let* ((proc (system:open-pipe curl-command :direction :input))
               (response (with-output-to-string (out)
                           (loop for line = (read-line proc nil nil)
                                 while line
                                 do (write-line line out)))))
          (close proc)
          (with-input-from-string (s response)
            (let* ((json-as-list (json:decode-json s))
                   (message (cdr (assoc :message json-as-list)))
                   (content (cdr (assoc :content message))))
              (or content "No response content"))))
      (error (e)
        (format t "Error calling Ollama: ~a~%" e)
        nil))))

(defun play (&key (story-file "story.txt") (model "mistral"))
  "Start the text adventure game. Reads story-file as the initial prompt
   and uses Ollama to generate responses to player actions."
  (setf *model-name* model)
  (let ((story (load-story story-file)))
    (unless story
      (return-from play))
    (let ((messages (list (list (cons :role "system")
                                (cons :content story)))))
      (format t "~a~%~%" story)
      (format t "Welcome to the Text Adventure!~%")
      (format t "Describe what you want to do, or type 'quit' to exit.~%~%")
      (loop
        (format t "> ")
        (force-output)
        (let ((user-input (string-trim '(#\Space #\Tab #\Newline) (read-line))))
          (when (member user-input '("quit" "exit" "QUIT" "EXIT") :test #'string=)
            (format t "Goodbye!~%")
            (return))
          (when (string= user-input "")
            (go :continue))
          (setf messages (append messages
                                 (list (list (cons :role "user")
                                             (cons :content user-input)))))
          (let ((response (call-ollama messages)))
            (when response
              (format t "~a~%" response)
              (setf messages (append messages
                                     (list (list (cons :role "assistant")
                                                 (cons :content response))))))))))))
