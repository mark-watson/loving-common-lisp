;;;; text-adventure-game_fireworks.lisp
;;;; Text adventure game using Fireworks AI for AI-driven storytelling.
;;;;
;;;; Usage (LispWorks):
;;;;   (load "text-adventure-game_fireworks.lisp")
;;;;   (text-adventure:play)
;;;;
;;;; Usage (SBCL):
;;;;   sbcl --load text-adventure-game_fireworks.lisp --eval '(text-adventure:play)'
;;;;
;;;; Requires FIREWORKS_API_KEY environment variable to be set.

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

(defun play (&key (story-file "story.txt") (model fireworks-ai:*fireworks-model*))
  "Start the text adventure game. Reads story-file as the initial prompt
   and uses Fireworks AI to generate responses to player actions."
  (let ((story (load-story story-file)))
    (unless story
      (return-from play))
    (let ((messages (list `((:role . "system")
                            (:content . ,story)))))
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
                                 (list `((:role . "user")
                                         (:content . ,user-input)))))
          (let ((response (fireworks-ai:chat messages :model-id model)))
            (when response
              (format t "~a~%" response)
              (setf messages (append messages
                                     (list `((:role . "assistant")
                                             (:content . ,response))))))))))))
