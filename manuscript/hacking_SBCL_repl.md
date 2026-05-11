# Hacking the SBCL REPL

> **Note to readers:** This chapter is a work in progress. Additional topics will be added in future updates.

The SBCL REPL is already a powerful environment for interactive development, but Common Lisp's extensibility lets us reshape it into something more. In this chapter we explore how to customize the SBCL REPL by modifying the `~/.sbclrc` startup file — adding shell command execution, custom reader macros, and quality-of-life improvements that blur the line between a Lisp REPL and a Unix shell.

## Shell Access via a Reader Macro

To achieve seamless, shell-like command execution in the SBCL REPL without requiring s-expression wrappers, the most idiomatic and robust mechanism is Common Lisp's **reader macros**.

Instead of hacking the REPL's top-level evaluation loop directly, we modify the readtable so that the reader itself intercepts special characters. Here is how the pieces fit together:

1. **Interception** — By binding `!` as a macro character, the Lisp reader hands control to a custom function the moment it encounters `!`.
2. **Consumption** — The macro function consumes the rest of the input line as a raw string, completely bypassing the standard Lisp parser for that line.
3. **Evaluation** — The macro returns a valid Lisp form (a function call) containing the parsed string. The REPL naturally evaluates this form.
4. **Process Management** — Shell commands like `ls` or `grep` are passed directly to the underlying OS shell using `uiop:run-program`.
5. **State Mutation** — Process-level state changes, specifically `cd`, cannot be delegated to a subshell because the subshell's environment terminates immediately. The macro must trap `cd`, execute it internally using `uiop:chdir`, and update Lisp's `*default-pathname-defaults*` to keep the Lisp environment in sync with the OS-level working directory.

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

(defun bang-reader (stream char)
  "Reader macro for ! that leaves the newline in the stream."
  (declare (ignore char))
  (let ((line (with-output-to-string (out)
                ;; Peek at the next character. If it's not a newline
                ;; or EOF, read it.
                (loop for c = (peek-char nil stream nil nil)
                      while (and c
                                 (char/= c #\Newline)
                                 (char/= c #\Return))
                      do (write-char (read-char stream) out)))))
    `(run-shell-command ,(string-trim " " line))))

;; Bind the '!' character
(set-macro-character #\! #'bang-reader)
```

### How It Works

#### The `run-shell-command` Function

The `run-shell-command` function is the workhorse. It takes a single string argument — the raw text after the `!` character — and dispatches it:

- **Empty input** — returns immediately with no values.
- **`cd` command** — handled internally. A `cd` executed in a subshell would change that subshell's directory, then immediately exit — leaving the parent Lisp process unchanged. Instead, we call `uiop:chdir` to change the OS-level working directory and update `*default-pathname-defaults*` so that subsequent Lisp file operations (like `(load "foo.lisp")`) resolve relative to the new directory.
- **Everything else** — delegated to the OS shell via `uiop:run-program`. The `:output :string` and `:error-output :string` keyword arguments capture both stdout and stderr as strings. We then print them explicitly with `princ` (which omits quotation marks, unlike `print` or `format ~S`) and call `force-output` to flush the stream immediately.

The `:ignore-error-status t` argument prevents `uiop:run-program` from signaling a condition on non-zero exit codes — important for commands like `grep` that return exit code 1 when no matches are found.

#### The `bang-reader` Macro Function

The `bang-reader` function is installed as a reader macro for the `!` character. When the Lisp reader encounters `!`, it calls this function with the input stream and the triggering character.

Rather than using `read-line` (which would consume the newline and potentially confuse the REPL's line tracking), the function uses `peek-char` in a loop to read characters one at a time until it hits a newline or end-of-file. This leaves the newline in the stream for the REPL to consume normally.

The function returns a backquoted form:

```lisp
`(run-shell-command ,(string-trim " " line))
```

This is a valid Lisp form that the REPL's evaluator processes normally — calling `run-shell-command` with the captured string as its argument.

#### The `set-macro-character` Binding

The final line installs the reader macro:

```lisp
(set-macro-character #\! #'bang-reader)
```

After this executes (at SBCL startup, via `~/.sbclrc`), any line beginning with `!` at the REPL is intercepted before the standard Lisp reader ever sees it.

### Example Session

After adding this code to `~/.sbclrc` and restarting SBCL:

```text
$ sbcl
* !ls -la *.lisp
-rw-r--r--  1 mark  staff  1234 May 11 16:30 example.lisp
-rw-r--r--  1 mark  staff   567 May 11 15:00 utils.lisp

* !pwd
/Users/mark/projects

* !cd /tmp

/private/tmp/

* !pwd
/private/tmp/

* !cd

/Users/mark/

* (+ 1 2)
3
```

Notice that normal Lisp expressions still work exactly as before. The `!` macro only activates when `!` is the first character on the line. You can freely alternate between shell commands and Lisp expressions within the same REPL session.
