;;;; knowledge-base-navigator.lisp
;;;;
;;;; A Knowledge Base Navigator utilizing the Gemini 3 Flash LLM API.
;;;; This replaces the legacy SPARQL/DBPedia backend of KGN with an immersive AI-driven pipeline.

(ql:quickload '("cl-json" "uiop" "alexandria"))

(in-package #:knowledge-base-navigator)

;; Tokenizer utility for parsing user selection space-separated integers
(defun tokenize-string (string &key (separators '(#\Space #\Tab #\Newline #\Return #\Comma)))
  (let ((tokens '())
        (current-word (make-string-output-stream)))
    (loop for char across string
          if (member char separators)
            do (let ((word (get-output-stream-string current-word)))
                 (when (plusp (length word))
                   (push word tokens)))
          else
            do (write-char char current-word))
    (let ((word (get-output-stream-string current-word)))
      (when (plusp (length word))
        (push word tokens)))
    (nreverse tokens)))

;; safely execute curl by passing a temp json file rather than quotes in bash string
(defun run-curl-command-with-json (url json-payload)
  (let ((temp-file (uiop:run-program "mktemp" :output :string)))
    (setf temp-file (string-trim #(#\Space #\Tab #\Newline #\Return) temp-file))
    (with-open-file (stream temp-file :direction :output :if-exists :supersede)
      (write-string json-payload stream))
    (let* ((curl-cmd
            (format nil "curl -s -X POST \"~A\" -H \"Content-Type: application/json\" -d @~A"
                    url temp-file))
           (response-body
            (multiple-value-bind (output error-output exit-code)
                (uiop:run-program curl-cmd :output :string :error-output :string :ignore-error-status t)
              (declare (ignore exit-code error-output))
              output)))
      (uiop:run-program (format nil "rm -f ~A" temp-file) :ignore-error-status t)
      response-body)))

(defun get-gemini-chat-completion (user-prompt)
  "Sends a prompt to the Gemini Interactions API and returns the content of the response."
  (let* ((api-key (uiop:getenv "GEMINI_API_KEY"))
         (model-name "gemini-3-flash-preview")
         (base-url "https://generativelanguage.googleapis.com/v1beta/interactions")
         (payload
           `((:model . ,model-name)
             (:input . ,user-prompt)))
         (json-payload (json:encode-json-to-string payload)))
    
    (unless (and api-key (not (string= api-key "")))
      (error "GEMINI_API_KEY environment variable is not set."))

    (handler-case
        (let* ((temp-file (uiop:run-program "mktemp" :output :string)))
          (setf temp-file (string-trim #(#\Space #\Tab #\Newline #\Return) temp-file))
          (with-open-file (stream temp-file :direction :output :if-exists :supersede)
            (write-string json-payload stream))
          (let* ((curl-cmd
                  (format nil "curl -s -X POST \"~A?key=~A\" -H \"Content-Type: application/json\" -H \"Api-Revision: 2026-05-20\" -d @~A"
                          base-url api-key temp-file))
                 (response-body
                  (multiple-value-bind (output error-output exit-code)
                      (uiop:run-program curl-cmd :output :string :error-output :string :ignore-error-status t)
                    (declare (ignore exit-code error-output))
                    output)))
            (uiop:run-program (format nil "rm -f ~A" temp-file) :ignore-error-status t)
            (let* ((parsed-response (json:decode-json-from-string response-body))
                   (steps (cdr (assoc :steps parsed-response))))
              (if steps
                  ;; Find the last model_output step and extract its text
                  (loop for step in (reverse steps)
                        when (string-equal (cdr (assoc :type step)) "model_output")
                        return (let* ((content (cdr (assoc :content step)))
                                      (first-content (first content)))
                                 (cdr (assoc :text first-content))))
                  (progn
                    (format *error-output* "~%[Error] Gemini Interactions API response did not contain steps: ~A~%" parsed-response)
                    nil)))))
      (error (e)
        (format *error-output* "An unexpected error occurred: ~A~%" e)
        nil))))


(defun kbn-ui ()
  "Main user interface loop for Knowledge Base Navigator powered by Gemini."
  (let ((prompt ""))
    (loop
      (format t "~%============= GEMINI KNOWLEDGE BASE NAVIGATOR =============~%")
      (format t "~%Enter entity names separated by space or a descriptive sentence (or type 'quit' to exit):~%> ")
      (finish-output)
      (setf prompt (read-line))
      
      (when (or (string-equal prompt "quit") (string-equal prompt "q"))
        (format t "Goodbye!~%")
        (return))
        
      (when (> (length prompt) 0)
        (format t "~%[Extracting entities using Gemini 3 Flash...]~%")
        (let* ((extract-prompt
                (format nil "Analyze the following user text: \"~A\".~%~
Identify potential encyclopedic entities (people, companies, countries, cities, products, concepts, etc.) mentioned.~%~
Categorize them if necessary. Return them as a neatly formatted numbered list (1., 2., 3., etc.) with a short 1-sentence description for each.~%~
DO NOT return any other conversational text, ONLY the numbered list so the user can see their options." prompt))
               (entity-list-text (get-gemini-chat-completion extract-prompt)))
               
          (if (null entity-list-text)
              (format t "~%[Error getting entity list from Gemini. Please try again.]~%")
              (progn
                (format t "~%--- IDENTIFIED ENTITIES ---~%~A~%---------------------------~%" entity-list-text)
                (format t "~%Enter the numbers of the entities you want detailed information for (space separated):~%> ")
                (finish-output)
                (let* ((selection-line (read-line))
                       (tokens (tokenize-string selection-line))
                       (valid-tokens (remove-if-not #'(lambda (s) (every #'digit-char-p s)) tokens))
                       (indices (mapcar #'parse-integer valid-tokens)))
                  (if (null indices)
                      (format t "~%[No valid selections made. Skipping to next prompt.]~%")
                      (progn
                        (format t "~%[Fetching detailed facts and relationships for selected entities...]~%")
                        (let* ((detail-prompt
                                (format nil "Review this numbered list of entities:~%~A~%~%~
The user has specifically selected the following entity numbers from the list: ~{~A~^, ~}.~%~
Task 1: For each selected entity, generate detailed, factual, encyclopedic information (like birth place, description, and relationships for people; industry, net income, description, relationships for companies; and similar DBpedia-level details for countries, cities, or products).~%~
Task 2: Evaluate ALL the selected entities collectively and explicitly summarize any known relationships, associations, or historical connections among them.~%~
Format the output carefully with clean section headers and bullet points." 
                                        entity-list-text indices))
                               (details-text (get-gemini-chat-completion detail-prompt)))
                          (format t "~%~A~%" details-text))))))))))))
