;;;; text-adventure-game.lisp
;;;; Text adventure game using Ollama for AI-driven storytelling.
;;;;
;;;; Usage (LispWorks):
;;;;   (load "text-adventure-game.lisp")
;;;;   (text-adventure:play)
;;;;
;;;; Usage (SBCL):
;;;;   sbcl --load text-adventure-game.lisp --eval '(text-adventure:play)'

(ql:quickload :llm)

(defpackage #:text-adventure
  (:use #:cl)
  (:export #:play))

(in-package #:text-adventure)

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

(defun play (&key (story-file "story.txt") (model ollama:*ollama-model*))
  "Start the text adventure game. Reads story-file as the initial prompt
   and uses Ollama to generate responses to player actions."
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
          (let ((response (ollama:chat messages :model-id model)))
            (when response
              (format t "~a~%" response)
              (setf messages (append messages
                                     (list (list (cons :|role| "assistant")
                                                 (cons :|content| response))))))))))))
