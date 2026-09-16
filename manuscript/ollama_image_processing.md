# Image Processing With Local Ollama Models

Here we use a very small model **qwen3.5:0.8b** to support the use case of natural language queries to read image files and answer questions on the image content. This can easily be customized for specific data processing pipelines.

The code for this example can be found in the directory **loving-common-lisp/src/ollama_images**.

## Design Notes for the Example code

Briefly dear reader, here are my design notes for this project:

- Network Delegation to litelm: The module performs no HTTP of its own. Every request goes through the **litelm** library (src/litelm), which routes a `"provider/model"` string to an OpenAI-compatible endpoint using `dexador`. That removes the `curl` subprocess, the hand-rolled JSON encoder, and the external JSON decoder that earlier drafts of this example required.
- Multimodal Messages as Lisp Data: The prompt and the images travel as ordinary Lisp structures in litelm's message format -- a `:user` message whose content is the OpenAI-compatible content-parts list, one `text` part plus one `image_url` part per image. litelm encodes that to JSON internally, so there is no wire format to maintain by hand and no shell is involved, which eliminates the command-injection surface entirely.
- Structured Error Handling: Because litelm maps HTTP failures onto a condition hierarchy (`litelm:not-found-error`, `litelm:api-error`, `litelm:rate-limit-error`, and friends), an unknown model name or an unreachable server surfaces as a catchable Lisp condition instead of a string that has to be parsed.
- Functional Wrappers: The public API (`image-to-text`, `describe-image-simple`) hides the encoding and transport complexity, presenting a clean, functional interface with sensible dynamic variables (*model-name*, *ollama-host*) for environment overrides.

## Code to Process Images

This Common Lisp module acts as a lightweight client for the Ollama vision API. It base64-encodes local images, wraps each one in an OpenAI-compatible `image_url` content part, and hands the resulting message to `litelm:completion`, which builds the request, performs the HTTP POST, and decodes the reply. The only work left to this file is turning image files into base64 strings and shaping them as litelm content parts.

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

## Optional Practice Problems

1. **Batch Directory Processor:** Write a function `describe-images-in-directory` that takes a directory path and an optional prompt string (defaulting to "What is in this image?"), finds all `.png`, `.jpg`, and `.jpeg` files in the directory using `uiop:directory-files`, calls `describe-image:image-to-text` on each one, and returns an alist of `(filename . description)` pairs. Print a progress line for each image processed. This exercise practices combining UIOP file utilities with the vision API for bulk processing.

2. **Model Comparison:** The `*model-name*` variable defaults to `qwen3.5:0.8b`, but Ollama supports several vision models (e.g., `llava`, `llava:13b`, `moondream`). Write a function `compare-models` that takes an image path, a prompt, and a list of model name strings. For each model, call `image-to-text` with the `:model` keyword, measure the wall-clock time using `get-internal-real-time`, and print the model name, response time in seconds, and the first 200 characters of the response. Discuss how response quality and speed vary across model sizes.

3. **Structured Data Extraction:** The `image-to-text` function returns free-form text. Write a function `extract-ticket-info` that takes an image path of a ticket or receipt, crafts a prompt that asks the model to return the data as a JSON object with specific keys (e.g., `"event"`, `"date"`, `"time"`, `"price"`, `"seat"`), calls `image-to-text` with that prompt, and then parses the returned JSON string with `litelm:json-decode`. Return the parsed alist. Handle the case where the model returns text that isn't valid JSON by wrapping the parse in `handler-case` and falling back to the raw text.

4. **Image Diff Reporter:** The `image-to-text` function already supports multiple images. Write a function `visual-diff-report` that takes two image paths (e.g., a "before" and "after" screenshot), sends them both with a prompt like "Describe all visual differences between these two images in a numbered list", and returns the model's comparison. Then write a wrapper `visual-regression-test` that takes a directory of "expected" images and a directory of "actual" images, pairs them by filename, and generates a diff report for each pair. This simulates a visual regression testing pipeline.

5. **Fetch and Describe URL Images:** The current `encode-image` function only reads local files. Write a function `encode-image-from-url` that uses `uiop:run-program` to call `curl` to download an image from a URL into a temporary file (via `uiop:with-temporary-file`), then base64-encodes it. Write a public wrapper `url-image-to-text` that accepts a URL string and a prompt, downloads the image, and passes the local path to the existing `image-to-text` function. Test it with a publicly accessible image URL. Be sure to clean up the temporary file afterwards.

6. **Conversational Image REPL:** The current API is single-shot — each call to `image-to-text` is independent. Write a function `image-chat` that loads one or more images, displays an initial description, then enters a REPL loop where the user can ask follow-up questions about the same images. Maintain a `chat-history` list of `(role content)` messages in litelm's message format and modify `call-ollama-vision` (or write a variant) to pass that full list to `litelm:completion` rather than the single `:user` message it builds today. This lets the model reference its previous answers when the user asks "What color is the text?" after an initial "Describe this image" turn.
