# Image Processing With Local Ollama Models

Here we use a very small model **qwen3.5:0.8b** to support the use case of natural language queries to read image files and answer questions on the image content. This can easily be customized for specific data processing pipelines.

The code for this example can be found in the directory **loving-common-lisp/src/ollama_images**.

## Design Notes for the Example code

Briefly dear reader, here are my design notes for this project:

- Network Delegation: Rather than importing a heavy Lisp HTTP client (like `Dexador` or `Drakma`), the library delegates network I/O to curl via `uiop:run-program`. This keeps the dependency footprint small.
- Security & Injection Prevention: By passing curl arguments as a strict list and piping the JSON payload through an in-memory string stream (`make-string-input-stream`) to curl's stdin (--data-binary @-), the design completely bypasses the system shell. This eliminates command injection risks and avoids temporary file overhead.
- Asymmetric JSON Handling: It relies on the external `cl-json` library for decoding responses but uses a lightweight, custom recursive function (`lisp-to-json-string`) for encoding. This ensures strict control over how Lisp keywords, symbols, and lists map to the specific JSON schema required by the Ollama API without needing complex CLOS (Common Lisp Object System) serializations.
- Functional Wrappers: The public API (`image-to-text`, `describe-image-simple`) hides the encoding and transport complexity, presenting a clean, functional interface with sensible dynamic variables (*model-name*, *ollama-host*) for environment overrides.

## Code to Process Images

This Common Lisp module acts as a lightweight client for the Ollama vision API by base64-encoding local images and serializing them into a customized JSON payload. To minimize dependencies and avoid shell injection risks, it securely streams this data to a spawned curl process via standard input and parses the returned JSON to extract the model's text description.

```lisp
;;;; describe-image.lisp — Send images to Ollama vision models for description
;;;;
;;;; Usage (from SBCL REPL):
;;;;   (load "describe-image.lisp")
;;;;   (describe-image:image-to-text "ticket.png" "Print out the plain text in this image")
;;;;   (describe-image:image-to-text '("a.png" "b.png") "Compare these two images.")
;;;;   (describe-image:describe-image-simple "photo.jpg")
;;;;
;;;; Environment:
;;;;   OLLAMA_MODEL — optional model override (default: qwen3.5:0.8b)
;;;;   OLLAMA_HOST  — optional API host override (default: http://localhost:11434/api/chat)

(ql:quickload '(:cl-base64 :cl-json :uiop) :silent t)

(defpackage #:describe-image
  (:use #:cl)
  (:export #:image-to-text
           #:describe-image-simple
           #:*model-name*
           #:*ollama-host*))

(in-package #:describe-image)

(defvar *model-name* "qwen3.5:0.8b"
  "Default vision-capable model for image queries.")

(defvar *ollama-host* "http://localhost:11434/api/chat"
  "Ollama API endpoint for chat completions.")

(defun encode-image (image-path)
  "Read IMAGE-PATH file and return its contents as a base64-encoded string.
   Signals an error immediately if the file does not exist."
  (unless (probe-file image-path)
    (error "Image file not found: ~a" image-path))
  (with-open-file (in image-path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      (cl-base64:usb8-array-to-base64-string bytes))))

(defun lisp-to-json-string (data)
  "Convert a Lisp nested association list to a JSON string.
   Keyword keys become JSON object keys; :true/:false become JSON booleans;
   lists of cons cells become JSON objects; plain lists become JSON arrays."
  (with-output-to-string (s)
    (labels ((alist-object-p (lst)
               "Return T if LST is a non-empty alist whose every car is a keyword."
               (and (consp lst)
                    (every #'(lambda (elem)
                               (and (consp elem)
                                    (keywordp (car elem))))
                           lst)))
             (write-json-value (value)
               (cond
                 ((null value)   (write-string "null"  s))
                 ((eq value :true)  (write-string "true"  s))
                 ((eq value :false) (write-string "false" s))
                 ((stringp value)
                  (write-char #\" s)
                  (loop for char across value
                        do (case char
                             (#\"      (write-string "\\\"" s))
                             (#\\      (write-string "\\\\" s))
                             (#\Newline (write-string "\\n"  s))
                             (#\Return  (write-string "\\r"  s))
                             (#\Tab     (write-string "\\t"  s))
                             (t         (write-char char s))))
                  (write-char #\" s))
                 ((numberp value)
                  (format s "~a" value))
                 ((symbolp value)
                  (write-json-value (string-downcase (symbol-name value))))
                 ((listp value)
                  (if (alist-object-p value)
                      ;; Encode as JSON object
                      (progn
                        (write-char #\{ s)
                        (loop for pair in value
                              for i from 0
                              when (> i 0) do (write-string ", " s)
                              do (let ((key (car pair))
                                       (val (cdr pair)))
                                   (write-json-value (if (keywordp key)
                                                         (string-downcase (symbol-name key))
                                                         key))
                                   (write-char #\: s)
                                   (write-json-value val)))
                        (write-char #\} s))
                      ;; Encode as JSON array
                      (progn
                        (write-char #\[ s)
                        (loop for elem in value
                              for i from 0
                              when (> i 0) do (write-string ", " s)
                              do (write-json-value elem))
                        (write-char #\] s))))
                 (t (error "Unsupported JSON type: ~a" (type-of value))))))
      (write-json-value data))))

(defun parse-ollama-response (json-string)
  "Parse the Ollama JSON response string.
   Signals a descriptive error if the response is empty or contains an API error field.
   Returns the message content string on success."
  (when (or (null json-string) (string= json-string ""))
    (error "Empty response from Ollama — is the server running at ~a?" *ollama-host*))
  (let ((parsed (cl-json:decode-json-from-string json-string)))
    ;; Propagate server-side error messages rather than swallowing them
    (let ((err (cdr (assoc :error parsed))))
      (when err
        (error "Ollama API error: ~a" err)))
    (let ((message (cdr (assoc :message parsed))))
      (when message
        (cdr (assoc :content message))))))

(defun call-ollama-vision (image-base64-list prompt &key (model *model-name*) (host *ollama-host*))
  "Call the Ollama vision API with a list of base64-encoded images and PROMPT.
   JSON is streamed directly to curl's stdin — no temp file, no shell injection.
   curl stderr is captured and reported on failure.
   Returns the response content string."
  (let* ((message (list (cons :|role|   "user")
                        (cons :|content| prompt)
                        (cons :|images|  image-base64-list)))
         (data    (list (cons :|model|    model)
                        (cons :|stream|   :false)
                        (cons :|messages| (list message))))
         (json-data (lisp-to-json-string data)))
    (multiple-value-bind (response-string stderr-string exit-code)
        ;; Pass args as a list — uiop bypasses the shell, preventing injection
        (uiop:run-program (list "curl" "-s" "-X" "POST" host
                                "-H" "Content-Type: application/json"
                                "--data-binary" "@-")
                          :input        (make-string-input-stream json-data)
                          :output       '(:string :stripped t)
                          :error-output '(:string :stripped t)
                          :ignore-error-status t)
      (unless (zerop exit-code)
        (error "curl failed (exit code ~a): ~a" exit-code stderr-string))
      (parse-ollama-response response-string))))

(defun image-to-text (image-paths prompt &key (model nil) (host nil))
  "Send one or more images to an Ollama vision model with PROMPT and return text.

   IMAGE-PATHS may be a single path string or a list of path strings.
   Optional keyword arguments:
     :model — override *model-name*
     :host  — override *ollama-host*

   Examples:
     (describe-image:image-to-text \"test.jpg\" \"What is in this image?\")
     (describe-image:image-to-text \"ticket.png\" \"Print out the text\" :model \"llava\")
     (describe-image:image-to-text '(\"before.png\" \"after.png\") \"Compare these images.\")"
  (let ((paths (if (listp image-paths) image-paths (list image-paths))))
    ;; Validate all paths upfront before encoding any of them
    (dolist (p paths)
      (unless (probe-file p)
        (error "Image file not found: ~a" p)))
    (let ((encoded-list (mapcar #'encode-image paths))
          (use-model    (or model *model-name*))
          (use-host     (or host  *ollama-host*)))
      (call-ollama-vision encoded-list prompt :model use-model :host use-host))))

(defun describe-image-simple (image-path)
  "Convenience wrapper — describe a single image using the default model and prompt.
   Equivalent to: (image-to-text IMAGE-PATH \"What is in this image?\")"
  (image-to-text image-path "What is in this image?"))
```

This lightweight module serves as an excellent foundation for embedding local, privacy-preserving vision capabilities into larger architectures. You could use it to automate the annotation and curation of proprietary training datasets by running bulk inference on unlabelled image directories, integrate it into a CI/CD pipeline for intelligent visual regression testing to compare "before and after" graphical states, or deploy it as the ingestion layer for an expert system that extracts structured semantic data from scanned technical diagrams and legacy documents without relying on external cloud APIs.


The following diagram shows the high-level architecture of the Ollama image processing module developed in this chapter:

{width: "80%"}
![Architecture diagram](images/ollama_images_architecture.png)

## Example Program Output

Here is a sample run:

```
$ sbcl
* (load "describe-image.lisp")
* (describe-image:image-to-text "ticket.png" "Print out the text in this image")
"Fanfares and Fireworks
Flagstaff Symphony Orchestra
Ardrey Memorial Auditorium
Friday, September 26, 2025
7:30 PM (AZ)

Level Section Row Seat
Main Main Level M 31

WJJNBY.1.2406.1498
Friday, September 26, 2025 @ 7:30 PM

Price Service Fee Ticket Type
$53.00 $0.00 Early Bird Tickets
New Subscriber C3

The unique barcodes on this ticket allow only one entry to the event. If multiple copies of an ETTicket are made, the first copy of the ETTicket to arrive at the event will gain entry after scanning and validation. Other copies of this ticket will be denied entry."
* 
```

