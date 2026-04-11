# Knowledge Base Navigator: Building an AI-Powered Information System

Earlier we looked the my old Knowledge Graph Navigator (KGN) project that combined symbolic Natural language Processing (NLP) with access to public knowledge graphs like Wikidata and DBPedia. Here I take a much simpler approach using an inexpensive Gemini model.

The code can be found in the directory: **loving-common-lisp/src/knowledge-base-navigator**.

In this chapter, we explore a practical application that combines modern AI APIs with Common Lisp to create an interactive knowledge exploration tool. The Knowledge Base Navigator demonstrates how to integrate external services, handle JSON data, and supply a user-friendly text interfaces.

This example differs from the KGN project since no *knowledge base* is constructed and stored. Here we use live web searches via Google’s `web_search` tool that is available with the Gemini APIs to dynamically construct user content. Dear reader, this is a different approach!

## Project Overview

The Knowledge Base Navigator is a modern evolution of my classic Knowledge Graph Navigator (KGN). This new version uses Google's Gemini Flash LLM API to extract entities from natural language, disambiguate them, discover semantic links between entities, and retrieve detailed encyclopedic information. This represents a paradigm shift from traditional database-backed systems to an AI-driven pipeline.

The system follows a two-stage process:

1. **Entity Extraction**: User provides text, Gemini identifies potential entities (people, companies, countries, etc.) and returns them as a numbered list
2. **Deep Retrieval**: User selects entities by number, Gemini provides detailed facts and analyzes relationships between entities

## Project Structure

The New Knowledge Navigator consists of three files, one source file and two small configuration files:

```
knowledge-base-navigator/
├── knowledge-base-navigator.asd     # ASDF system definition
├── project.lisp                     # Package definition
└── knowledge-base-navigator.lisp    # Core application
```

### System Definition (ASDF)

The `.asd` file defines the system, its metadata, dependencies, and compilation order:

```lisp
;;;; knowledge-base-navigator.asd

(asdf:defsystem #:knowledge-base-navigator
  :description "Knowledge Base Navigator using Gemini 3 Flash LLM"
  :author "Mark Watson <markw@markwatson.com>"
  :license "Apache 2"
  :depends-on (#:cl-json #:uiop #:alexandria)
  :serial t
  :components ((:file "project")
               (:file "knowledge-base-navigator")))
```

Key elements:

- **`#:prefix`** notation creates uninterned symbols, avoiding package conflicts
- **`:depends-on`** specifies required libraries: `cl-json` for JSON encoding/decoding, `uiop` for system utilities, `alexandria` for common utilities
- **`:serial t`** ensures files compile in order (project.lisp before knowledge-base-navigator.lisp)

### Package Definition

The package file establishes the namespace and exports:

```lisp
;;;; project.lisp
;;;; Package definition and global variables

(defpackage #:knowledge-base-navigator
  (:use #:cl)
  (:export #:kbn-ui
           #:get-gemini-chat-completion))

(in-package #:knowledge-base-navigator)
```

- **`defpackage`** creates a new namespace, isolating symbols from other packages
- **`:use #:cl`** imports standard Common Lisp symbols
- **`:export`** makes `kbn-ui` and `get-gemini-chat-completion` accessible externally

## Core Implementation

The following code snippets are part of the file **knowledge-base-navigator.lisp**.

### Tokenizer Utility

The tokenizer parses user input into individual tokens:

```lisp
(defun tokenize-string (string &key (separators '(#\Space #\Tab #\Newline #\Return)))
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
```

This function uses a character-by-character approach:

1. Iterates across each character in the input string
2. Accumulates characters into a stream until hitting a separator
3. Pushes completed words onto the token list
4. Returns tokens in original order via `nreverse`

**Why implement a custom tokenizer?** The standard `cl-ppcre:split` or `uiop:split-string` would work here, but this implementation demonstrates manual string processing and is dependency-free.

### HTTP Communication via curl

Rather than using Common Lisp HTTP libraries (which can have SSL/Certificate issues), this project invokes `curl` directly:

```lisp
(defun run-curl-command-with-json (url json-payload)
  (let ((temp-file (uiop:run-program "mktemp" :output :string)))
    (setf temp-file (string-trim '(#\Space #\Tab #\Newline #\Return) temp-file))
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
```

**Key technique**: JSON payload is written to a temporary file, then passed to curl with `-d @filename`. This avoids complex shell escaping issues with embedded quotes and special characters. 

The flow:
1. Create temp file with `mktemp`
2. Write JSON payload to file
3. Execute curl with `@filename` syntax
4. Clean up temp file
5. Return response body

### Gemini API Integration

The main API function constructs requests and parses responses:

```lisp
(defun get-gemini-chat-completion (user-prompt)
  "Sends a prompt to the Gemini API and returns the content of the response."
  (let* ((api-key (uiop:getenv "GEMINI_API_KEY"))
         (model-name "models/gemini-3-flash-preview")
         (base-url (format nil "https://generativelanguage.googleapis.com/v1beta/~A:generateContent?key=~A" 
                           model-name api-key))
         (payload
           `((:contents . ,(vector
                            `((:parts . ,(vector `((:text . ,user-prompt)))))))))
         (json-payload (json:encode-json-to-string payload)))
    
    (unless (and api-key (not (string= api-key "")))
      (error "GEMINI_API_KEY environment variable is not set."))

    (handler-case
        (let* ((response-body (run-curl-command-with-json base-url json-payload))
               (parsed-response (json:decode-json-from-string response-body))
               (candidates (cdr (assoc :candidates parsed-response))))
          (if candidates
              (let* ((first-candidate (first candidates))
                     (content (cdr (assoc :content first-candidate)))
                     (parts (cdr (assoc :parts content)))
                     (first-part (first parts))
                     (text (cdr (assoc :text first-part))))
                text)
              (progn
                (format *error-output* "~%[Error] Gemini API response did not contain candidates: ~A~%" 
                        parsed-response)
                nil)))
      (error (e)
        (format *error-output* "An unexpected error occurred: ~A~%" e)
        nil))))
```

**API Structure Notes**:

- Backquote (`) is used for template construction, with comma (,) for evaluation
- Gemini expects a nested JSON structure: contents -> parts -> text
- `json:encode-json-to-string` converts Lisp data to JSON
- `json:decode-json-from-string` parses the response
- `handler-case` provides error handling, returning nil on failure

**Response parsing pattern**: The Gemini response has nested associative lists (alists). Navigate with:

```lisp
(cdr (assoc :key alist))  ; Get value for :key
(first list)              ; Get first element
(cdr ...)                 ; Continue navigating
```

### Interactive UI Loop

The `kbn-ui` function implements an interactive text user interface:

```lisp
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
        ;; ... entity extraction and detail retrieval
        ))))
```

**Key loop patterns**:

- `loop` with `(return)` for controlled exit
- `read-line` for user input
- `finish-output` ensures prompts display immediately
- `when` guards for conditional execution

The entity extraction creates a prompt that instructs Gemini to return ONLY a numbered list:

```lisp
(format nil "Analyze the following user text: \"~A\".~
Identify potential encyclopedic entities (people, companies, countries, cities, products, concepts, etc.) mentioned.~
Categorize them if necessary. Return them as a neatly formatted numbered list (1., 2., 3., etc.) with a short 1-sentence description for each.~
DO NOT return any other conversational text, ONLY the numbered list so the user can see their options." prompt)
```

**Prompt engineering principle**: Be explicit about output format. The instruction to return ONLY the numbered list prevents verbose conversational responses.

User selection handling:

```lisp
(let* ((tokens (tokenize-string selection-line))
       (valid-tokens (remove-if-not #'(lambda (s) (every #'digit-char-p s)) tokens))
       (indices (mapcar #'parse-integer valid-tokens)))
  ;; Process indices...
)
```

This filters non-numeric input and converts valid selections to integers.

## Running the Application

Load the system via Quicklisp:

```lisp
;; Ensure project directory is in Quicklisp's path
;; Usually via symlink:
;;  ln -s /path/to/knowledge-base-navigator ~/quicklisp/local-projects/
;; or by adding the current directory (or a parent directory)
;; to ql:*local-project-directories*

;; Load system
(ql:quickload "knowledge-base-navigator")

;; Start UI
(knowledge-base-navigator:kbn-ui)
```

### Example Session

```
============= GEMINI KNOWLEDGE BASE NAVIGATOR =============

Enter entity names separated by space or a descriptive sentence (or type 'quit' to exit):
> Bill Gates and Microsoft

[Extracting entities using Gemini 3 Flash...]

--- IDENTIFIED ENTITIES ---
1. Bill Gates: An American business magnate, software developer, and philanthropist who co-founded Microsoft Corporation.
2. Microsoft: A multinational technology corporation that develops, manufactures, and licenses computer software.
---------------------------

Enter the numbers of the entities you want detailed information for (space separated):
> 1 2

[Fetching detailed facts and relationships for selected entities...]

=== BILL GATES ===
* Born: October 28, 1955, Seattle, Washington
* Occupation: Business magnate, investor, philanthropist
* Net Worth: ~$120 billion (as of 2024)
* Founded Microsoft in 1975 with Paul Allen

=== MICROSOFT ===
* Founded: April 4, 1975
* Headquarters: Redmond, Washington
* Industry: Technology, Software, Cloud Computing
* Revenue: $211 billion (2023)

=== RELATIONSHIP ===
Bill Gates co-founded Microsoft with Paul Allen in 1975. He served as CEO until 2000 and remained Chairman until 2014. Microsoft was the primary source of Gates' wealth.
```

## Key Takeaways

1. **Package Organization**: Use ASDF systems with separate files for package definitions
2. **External Process Integration**: `uiop:run-program` provides robust external command execution
3. **JSON Handling**: `cl-json` encodes Lisp data structures to JSON and decodes responses
4. **Error Handling**: Use `handler-case` for robust API error handling
5. **Prompt Engineering**: Construct clear prompts with explicit format instructions
6. **REPL Interfaces**: `loop` + `read-line` + `format` create simple but effective CLIs

## Dependencies

| Library | Purpose |
|---------|---------|
| `cl-json` | JSON encoding/decoding |
| `uiop` | System utilities, process execution |
| `alexandria` | Common Lisp utilities |

## Environment Setup

```bash
# Set API key before starting Lisp
export GEMINI_API_KEY="your_api_key_here"

# Start SBCL
sbcl

# Load Quicklisp
(quicklisp:setup)

# Load project
(ql:quickload "knowledge-base-navigator")
```

---

This example demonstrates that Common Lisp remains highly relevant for modern API-driven applications. The combination of powerful REPL development, mature libraries, and straightforward process integration makes it an excellent choice for exploratory programming and rapid prototyping.
