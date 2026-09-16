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
;;;;   OLLAMA_HOST  — optional OpenAI-compatible base URL override
;;;;                  (default: http://localhost:11434/v1)
;;;;
;;;; Transport, JSON encoding/decoding, and error mapping are delegated to the
;;;; litelm library (src/litelm), the same way the ollama package does it.

(ql:quickload '(:litelm :cl-base64 :uiop) :silent t)

(defpackage #:describe-image
  (:use #:cl)
  (:export #:image-to-text
           #:describe-image-simple
           #:*model-name*
           #:*ollama-host*))

(in-package #:describe-image)

(defvar *model-name* (or (uiop:getenv "OLLAMA_MODEL") "qwen3.5:0.8b")
  "Default vision-capable model for image queries.")

(defvar *ollama-host* (or (uiop:getenv "OLLAMA_HOST") "http://localhost:11434/v1")
  "OpenAI-compatible Ollama API base URL; litelm appends /chat/completions.")

(defun encode-image (image-path)
  "Read IMAGE-PATH file and return its contents as a base64-encoded string.
   Signals an error immediately if the file does not exist."
  (unless (probe-file image-path)
    (error "Image file not found: ~a" image-path))
  (with-open-file (in image-path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      (cl-base64:usb8-array-to-base64-string bytes))))

(defun image-media-type (image-path)
  "Return the media type for IMAGE-PATH, derived from its file extension."
  (let ((type (string-downcase (or (pathname-type image-path) ""))))
    (cond ((member type '("jpg" "jpeg") :test #'string=) "image/jpeg")
          ((string= type "gif")  "image/gif")
          ((string= type "webp") "image/webp")
          ((string= type "bmp")  "image/bmp")
          (t "image/png"))))

(defun text-part (prompt)
  "Build the OpenAI-compatible text content part for PROMPT."
  (list (cons "type" "text")
        (cons "text" prompt)))

(defun image-part (image-path base64)
  "Build the OpenAI-compatible image content part for one base64-encoded image.
   IMAGE-PATH supplies the media type; BASE64 is the encoded image data."
  (list (cons "type" "image_url")
        (cons "image_url"
              (list (cons "url"
                          (concatenate 'string
                                       "data:" (image-media-type image-path)
                                       ";base64," base64))))))

(defun ensure-model-name (model)
  "Ensure MODEL has a provider prefix for litelm routing (defaults to ollama/)."
  (if (find #\/ model)
      model
      (concatenate 'string "ollama/" model)))

(defun call-ollama-vision (image-parts prompt &key (model *model-name*) (host *ollama-host*))
  "Send IMAGE-PARTS (content parts built by IMAGE-PART) and PROMPT to Ollama.
   litelm builds the request, performs the HTTP POST, and decodes the response.
   Returns the model's text response string."
  (let ((response (litelm:completion (ensure-model-name model)
                                     :messages (list (list :user (cons (text-part prompt)
                                                                       image-parts)))
                                     :api-base host)))
    (or (litelm:response-content response) "")))

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
    (let ((parts      (loop for p in paths
                            collect (image-part p (encode-image p))))
          (use-model  (or model *model-name*))
          (use-host   (or host  *ollama-host*)))
      (call-ollama-vision parts prompt :model use-model :host use-host))))

(defun describe-image-simple (image-path)
  "Convenience wrapper — describe a single image using the default model and prompt.
   Equivalent to: (image-to-text IMAGE-PATH \"What is in this image?\")"
  (image-to-text image-path "What is in this image?"))
