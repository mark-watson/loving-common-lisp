;;;; text-adventure-game.lisp
;;;; Text adventure game using Ollama for AI-driven storytelling.
;;;; Self-contained: a local chat function posts to Ollama's /api/chat endpoint.
;;;;
;;;; Usage (LispWorks):
;;;;   (load "text-adventure-game_ollama.lisp")
;;;;   (text-adventure:play)
;;;;
;;;; Usage (SBCL):
;;;;   sbcl --load text-adventure-game_ollama.lisp --eval '(text-adventure:play)'

(ql:quickload '(:uiop :cl-json))

(defpackage #:text-adventure
  (:use #:cl)
  (:export #:play))

(in-package #:text-adventure)

(defvar *ollama-endpoint* "http://localhost:11434/api/chat")
(defvar *ollama-model* "qwen3.5:0.8b")

(defun substitute-subseq (string old new &key (test #'eql))
  "Single-pass string substitution used to repair cl-json's NIL -> null."
  (let ((pos (search old string :test test)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (subseq string (+ pos (length old))))
        string)))

(defun chat (messages &key (model-id *ollama-model*))
  "Send the multi-turn MESSAGES (list of (:|role| . ...) (:|content| . ...)
alists) to the local Ollama server and return the assistant's text."
  (let* ((data (list (cons :|model| model-id)
                      (cons :|stream| nil)
                      (cons :|messages| messages)))
         (json-data (cl-json:encode-json-to-string data))
         (fixed-json-data
          (substitute-subseq json-data ":null" ":false" :test #'string=))
         (process (uiop:launch-program
                   (format nil "curl -s ~a -d ~s" *ollama-endpoint* fixed-json-data)
                   :output :stream
                   :error-output :stream))
         (response (with-output-to-string (out)
                     (loop for line = (read-line (uiop:process-info-output process) nil nil)
                           while line
                           do (write-line line out)))))
    (with-input-from-string (s response)
      (let* ((json-as-list (cl-json:decode-json s))
             (message-resp (cdr (assoc :message json-as-list)))
             (content (cdr (assoc :content message-resp))))
        (or content "No response content")))))

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

(defun play (&key (story-file "story.txt") (model *ollama-model*))
  "Start the text adventure game. Reads story-file as the initial prompt
   and uses the local chat function to generate responses to player actions."
  (let ((story (load-story story-file)))
    (unless story
      (return-from play))
    (let ((messages (list (list (cons :|role| "system")
                                (cons :|content| story)))))
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
                                 (list (list (cons :|role| "user")
                                             (cons :|content| user-input)))))
          (let ((response (chat messages :model-id model)))
            (when response
              (format t "~a~%" response)
              (setf messages (append messages
                                     (list (list (cons :|role| "assistant")
                                                 (cons :|content| response))))))))))))
