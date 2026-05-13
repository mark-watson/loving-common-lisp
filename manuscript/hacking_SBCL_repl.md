# Hacking the SBCL REPL

> **Note to readers:** This chapter is a work in progress. Additional topics will be added in future updates.

The SBCL REPL is already a powerful environment for interactive development, but Common Lisp's extensibility lets us reshape it into something more. In this chapter we explore how to customize the SBCL REPL by modifying the `~/.sbclrc` startup file, adding shell command execution, custom reader macros, and quality-of-life improvements that blur the line between a Lisp REPL and a Unix shell. The two customizations we implement are:

- Using `#!` to execute shell commands.
- Adding an AI coding agent that is always available in the REPL and instantiated with `#?`.

## Shell Access via a Reader Macro

To achieve seamless, shell-like command execution in the SBCL REPL without requiring s-expression wrappers, the most idiomatic and robust mechanism is Common Lisp's **reader macros**.

Instead of hacking the REPL's top-level evaluation loop directly, we modify the readtable so that the reader itself intercepts special characters. Here is how the pieces fit together:

1. **Interception** — By binding `#!` as a dispatch macro character, the Lisp reader hands control to a custom function the moment it encounters the `#!` sequence.
2. **Consumption** — The macro function consumes the rest of the input line as a raw string, completely bypassing the standard Lisp parser for that line.
3. **Evaluation** — The macro returns a valid Lisp form (a function call) containing the parsed string. The REPL naturally evaluates this form.
4. **Process Management** — Shell commands like `ls` or `grep` are passed directly to the underlying OS shell using `uiop:run-program`.
5. **State Mutation** — Process-level state changes, specifically `cd`, cannot be delegated to a subshell because the subshell's environment terminates immediately. The macro must trap `cd`, execute it internally using `uiop:chdir`, and update Lisp's `*default-pathname-defaults*` to keep the Lisp environment in sync with the OS-level working directory.

> **Why `#!` instead of bare `!`?** The exclamation mark is a valid constituent character in Common Lisp symbols. Libraries like `cl-json` use symbols such as `run!` internally. A bare `!` reader macro hijacks this character globally — when SBCL loads any `.asd` file or source file containing `!` in a symbol name, the reader macro fires, consumes the rest of the line as a shell command, and leaves the reader with unbalanced parentheses. Using `#!` (a dispatch macro on `#`) avoids this entirely because `#` is already reserved as a dispatching macro character and never appears as part of a symbol name.

### Adding the Code to ~/.sbclrc

Add the following code to the end of your `~/.sbclrc` file. It requires ASDF (which ships with SBCL) for access to UIOP's process and filesystem utilities:

```lisp
(require :asdf)

(defun run-shell-command (line)
  "Executes a raw shell command string and forcefully prints captured output."
  (if (string= line "")
      (values)
      (let* ((space-pos (position #\Space line))
             (cmd (if space-pos (subseq line 0 space-pos) line))
             (args (if space-pos
                       (string-trim " " (subseq line space-pos))
                       "")))
        (cond
          ;; Handle cd internally
          ((string-equal cmd "cd")
           (let ((target-dir (if (string= args "")
                                 (namestring (user-homedir-pathname))
                                 args)))
             (uiop:chdir target-dir)
             (setf *default-pathname-defaults* (uiop:getcwd))
             (format t "~A~%" (uiop:getcwd))
             (force-output)
             (values)))

          ;; Pass to system shell, capture output as strings
          (t
           (multiple-value-bind (stdout stderr exit-code)
               (uiop:run-program line
                                 :output :string
                                 :error-output :string
                                 :ignore-error-status t)
             (declare (ignore exit-code))
             ;; Print captured strings explicitly to Lisp's stdout
             (when (plusp (length stdout))
               (princ stdout))
             (when (plusp (length stderr))
               (princ stderr))
             (force-output)
             (values)))))))

(defun bang-reader (stream disp-char sub-char)
  "Dispatch reader macro for #! — executes a shell command."
  (declare (ignore disp-char sub-char))
  (let ((line (with-output-to-string (out)
                ;; Peek at the next character. If it's not a newline
                ;; or EOF, read it.
                (loop for c = (peek-char nil stream nil nil)
                      while (and c
                                 (char/= c #\Newline)
                                 (char/= c #\Return))
                      do (write-char (read-char stream) out)))))
    `(run-shell-command ,(string-trim " " line))))

;; Bind #! as a dispatch macro character
(set-dispatch-macro-character #\# #\! #'bang-reader)
```

### How It Works

#### The `run-shell-command` Function

The `run-shell-command` function is the workhorse. It takes a single string argument — the raw text after the `#!` sequence — and dispatches it:

- **Empty input** — returns immediately with no values.
- **`cd` command** — handled internally. A `cd` executed in a subshell would change that subshell's directory, then immediately exit — leaving the parent Lisp process unchanged. Instead, we call `uiop:chdir` to change the OS-level working directory and update `*default-pathname-defaults*` so that subsequent Lisp file operations (like `(load "foo.lisp")`) resolve relative to the new directory.
- **Everything else** — delegated to the OS shell via `uiop:run-program`. The `:output :string` and `:error-output :string` keyword arguments capture both stdout and stderr as strings. We then print them explicitly with `princ` (which omits quotation marks, unlike `print` or `format ~S`) and call `force-output` to flush the stream immediately.

The `:ignore-error-status t` argument prevents `uiop:run-program` from signaling a condition on non-zero exit codes — important for commands like `grep` that return exit code 1 when no matches are found.

#### The `bang-reader` Dispatch Macro Function

The `bang-reader` function is installed as a dispatch reader macro for the `#!` character sequence. When the Lisp reader encounters `#!`, it calls this function with three arguments: the input stream, the dispatch character (`#`), and the sub-character (`!`). We declare both character arguments as ignored since we only need the stream.

Rather than using `read-line` (which would consume the newline and potentially confuse the REPL's line tracking), the function uses `peek-char` in a loop to read characters one at a time until it hits a newline or end-of-file. This leaves the newline in the stream for the REPL to consume normally.

The function returns a backquoted form:

```lisp
`(run-shell-command ,(string-trim " " line))
```

This is a valid Lisp form that the REPL's evaluator processes normally — calling `run-shell-command` with the captured string as its argument.

#### The `set-dispatch-macro-character` Binding

The final line installs the dispatch macro:

```lisp
(set-dispatch-macro-character #\# #\! #'bang-reader)
```

After this executes (at SBCL startup, via `~/.sbclrc`), any `#!` sequence at the REPL is intercepted before the standard Lisp reader ever sees the rest of the line. Unlike a bare `!` reader macro, this approach is safe — it cannot interfere with symbol names containing exclamation marks, because `#` is already reserved as a non-constituent dispatching prefix.

### Example Session

After adding this code to `~/.sbclrc` and restarting SBCL:

```text
$ sbcl
* #! ls -la *.lisp
-rw-r--r--  1 mark  staff  1234 May 11 16:30 example.lisp
-rw-r--r--  1 mark  staff   567 May 11 15:00 utils.lisp

* #! pwd
/Users/mark/projects

* #! cd /tmp

/private/tmp/

* #! pwd
/private/tmp/

* #! cd

/Users/mark/

* (+ 1 2)
3
```

Notice that normal Lisp expressions still work exactly as before. The `#!` macro only activates when `#!` appears in the input. You can freely alternate between shell commands and Lisp expressions within the same REPL session.

## AI Coding Agent Integration

The `#!` reader macro from the previous section gave us shell access from the REPL. In this section we go further by integrating an AI coding agent directly into the REPL so we can ask questions, diagnose errors, and generate code without leaving SBCL.

The `cl-ai-coding-agent` package (developed in the "Building an AI Coding Assistant for Common Lisp") provides a function `coding-agent-query` that takes a string prompt and returns the agent's response. The agent can autonomously list directories, read files, write new files, and diagnose stacktraces using Gemini's function-calling API. But calling it directly requires quoting strings:

```lisp
(cl-ai-coding-agent:coding-agent-query
  "Write a function that sorts a list of strings")
```

This syntactic friction slows down the interactive workflow. We want three levels of integration:

1. **An `ai` macro** — eliminates string quoting by stringifying unevaluated symbols, so we can type natural language as Lisp forms.
2. **A `#?` reader macro** — captures an entire line as a prompt, matching the `#!` pattern from the shell integration.
3. **Automatic error interception** — hooks into SBCL's debugger to automatically feed unhandled errors to the agent.

### The Code

Add the following code to your `~/.sbclrc` file, after the shell integration code from the previous section. It assumes `cl-ai-coding-agent` is installed in your Quicklisp local-projects directory:

```lisp
;;; ---- AI Coding Agent REPL Integration ----

;; Load the agent on startup
(ql:quickload :cl-ai-coding-agent :silent t)

;; 1. The AI macro -- type natural language without quotes
(defmacro ai (&rest words)
  "Ask the AI coding agent a question using natural
   language without string quotes.
   Usage: (ai write a function that sorts strings)"
  (let ((prompt (format nil "~{~A~^ ~}" words)))
    `(progn
       (format t "~&~A~%"
               (cl-ai-coding-agent:coding-agent-query
                ,prompt))
       (values))))

;; 2. The #? reader macro -- one-line AI queries
(defun ai-query-reader (stream disp-char sub-char)
  "Dispatch reader macro for #? -- sends the rest
   of the line to the AI coding agent."
  (declare (ignore disp-char sub-char))
  (let ((line (with-output-to-string (out)
                (loop for c = (peek-char nil stream
                                         nil nil)
                      while (and c
                                 (char/= c #\Newline)
                                 (char/= c #\Return))
                      do (write-char
                          (read-char stream) out)))))
    `(progn
       (format t "~&~A~%"
               (cl-ai-coding-agent:coding-agent-query
                ,(string-trim " " line)))
       (values))))

(set-dispatch-macro-character #\# #\? #'ai-query-reader)

;; 3. Automatic error interception
(defun ai-diagnose-error (condition)
  "Format a condition into a diagnostic prompt and
   send it to the AI coding agent."
  (let* ((text (format nil "~A" condition))
         (response
          (cl-ai-coding-agent:coding-agent-query
           text)))
    (format t "~&~%--- AI Diagnosis ---~%~A~%~
               --- End Diagnosis ---~%"
            response)))
```

### How It Works

#### The `ai` Macro

The `ai` macro accepts unquoted natural language tokens and concatenates them into a prompt string at macro-expansion time:

```lisp
(ai write a quicksort function)
```

expands to:

```lisp
(progn
  (format t "~&~A~%"
    (cl-ai-coding-agent:coding-agent-query
      "write a quicksort function"))
  (values))
```

The `(values)` suppresses the return value to keep the REPL output clean — we only want to see the agent's printed response, not a redundant return string. Because the macro stringifies symbols, any valid Lisp identifier characters work: letters, digits, hyphens. However, characters that the Lisp reader treats specially — parentheses, commas, quotes, semicolons — will cause read errors. For prompts that need those characters, use `coding-agent-query` with an explicit string, or the `#?` reader macro.

#### The `#?` Reader Macro

The `#?` dispatch macro works identically to `#!` but routes to the AI agent instead of the shell:

```lisp
* #? write the common list file test.lisp that prints numbers form 1 to 10

I have created the file `test.lisp` with the following Common Lisp code:

``lisp
(loop for i from 1 to 10
      do (format t "~D~%" i))
``
```

Like `#!`, it consumes the rest of the input line as a raw string, bypassing the Lisp reader entirely. This means any characters — including parentheses, quotes, and semicolons — are treated as plain text. This makes `#?` ideal for pasting error messages or asking questions that contain Lisp syntax.

#### Diagnosing Stacktraces from Files

Stacktraces often contain double-quote characters and span multiple lines, making them awkward to paste into a Lisp string literal. The `coding-agent-query-file` function reads the contents of a file and sends them as the prompt:

```lisp
;; Save a stacktrace to a file (e.g., from terminal):
;;   pbpaste > /tmp/error.txt
;;
;; Then in the REPL:
(cl-ai-coding-agent:coding-agent-query-file
  "/tmp/error.txt")

"### Error Analysis: DIVISION-BY-ZERO

**1. Root Cause**
The error was caused by the expression `(/ 10 0)`, which attempts to perform an integer division where the divisor is zero.

**2. What Went Wrong**
In Common Lisp (and most programming languages), division by zero is an undefined operation. When the SBCL kernel's `INTEGER-/-INTEGER` function encountered `0` as the denominator, it signaled a `DIVISION-BY-ZERO` condition. This halted execution and invoked the debugger.

**3. Concrete Fix**
To prevent this error, you should validate that the divisor is not zero before performing the division. Depending on your use case, you can use a conditional check or handle the condition.

#### Option A: Conditional Check (Recommended)
Before dividing, check if the denominator is zero using `zerop`.

``lisp
(let ((numerator 10)
      (denominator 0))
  (if (zerop denominator)
      (format t \"Error: Cannot divide by zero.\")
      (/ numerator denominator)))
``

#### Option B: Using `handler-case`
If the division is part of a larger computation and you want to catch the error gracefully:

``lisp
(handler-case (/ 10 0)
  (division-by-zero ()
    (format t \"Caught division by zero! Returning NIL.\")
    nil))
``

#### Option C: Using `ignore-errors`
If you simply want the expression to return `NIL` instead of crashing:

``lisp
(ignore-errors (/ 10 0)) ; Returns NIL and the condition object as a second value
``"

```

### Example Session

After adding the integration code to `~/.sbclrc` and restarting SBCL:

```text
$ sbcl
* (ai what is a hash table in common lisp)
A hash table in Common Lisp is a data structure that maps
keys to values using a hash function for fast lookups.
You create one with MAKE-HASH-TABLE and access entries
with GETHASH:

  (defvar *ht* (make-hash-table))
  (setf (gethash :name *ht*) "Alice")
  (gethash :name *ht*)  ; => "Alice"

* #? list the .lisp files in this directory and describe them
The current directory contains:
- agent.lisp: Core agent logic with the tool-use loop
- package.lisp: Package definition and exports
- tools.lisp: File-system tool implementations
- cl-ai-coding-agent.asd: ASDF system definition

* #! ls *.lisp
agent.lisp    package.lisp  tools.lisp

* (+ 1 2)
3

* (/ 1 0)
--- AI Diagnosis ---
This error occurs because Common Lisp does not allow
division by zero. The expression (/ 1 0) attempts to
divide the integer 1 by 0, which is mathematically
undefined.

Fix: Add a guard before dividing:

  (let ((divisor 0))
    (if (zerop divisor)
        (error "Cannot divide by zero")
        (/ 1 divisor)))
--- End Diagnosis ---

debugger invoked on DIVISION-BY-ZERO ...


* #? write a file test.lisp to run a simple test of groq.lisp
I have created a file named `test.lisp` in the current directory to test the `groq` library. This script loads the system using ASDF and performs a simple completion request.

The contents of `test.lisp` are as follows:

``lisp
;;;; test.lisp
;;;; Simple test for the groq library

(require :asdf)
(asdf:load-asd (truename "groq.asd"))
(asdf:load-system :groq)

(defun run-groq-test ()
  (format t "Starting Groq API test...~%")
  (unless (uiop:getenv "GROQ_API_KEY")
    (format t "Warning: GROQ_API_KEY environment variable is not set.~%")
    (return-from run-groq-test))

  (let* ((prompt "Say 'Hello, World!' in Common Lisp.")
         (response (groq:groq-completion prompt))
         (content (groq:groq-extract-content response)))
    (format t "Prompt: ~A~%" prompt)
    (format t "Extracted Content:~%~A~%" content)))

(run-groq-test)
```

To run this test, ensure you have your `GROQ_API_KEY` environment variable set, then execute:
```bash
sbcl --load test.lisp --quit
``
* 

```

Notice how all three integration levels coexist. The `ai` macro handles simple natural-language queries, `#?` handles anything with special characters, `#!` still works for shell commands, and the error hook provides automatic diagnosis before dropping into the debugger. Standard Lisp expressions continue to work normally.
