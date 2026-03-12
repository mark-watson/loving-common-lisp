# Common Lisp LLM Tools: a work in progress

A collection of small, self-contained Common Lisp command-line tools designed
for use as LLM tool-calls in **LM Studio**, **gemini-clj**, and **Claude Code**.

Each tool is compiled with SBCL into a compressed, standalone executable
(no runtime dependencies) and placed in `./bin/`.

## Building

```bash
make          # build all tools
make clean    # remove compiled binaries and core files
```

Individual targets: `make current-time`, `make files-in-current-directory`.

---

## Tools

### `current-time`

**Source:** `current-time.lisp`  
**Binary:** `./bin/current-time`

Returns the current local date and time in ISO 8601 format.

```
$ ./bin/current-time
2026-03-12T11:17:14-07:00
```

Useful as an LLM tool when the model needs to know the current wall-clock time.

---

### `files-in-current-directory`

**Source:** `files-in-current-directory.lisp`  
**Binary:** `./bin/files-in-current-directory`

Lists "important" files (source code, Makefiles, docs, data files, PDFs, etc.)
in the current directory and all subdirectories, one per line, sorted.
Hidden directories (e.g. `.git`) are skipped automatically.

```
$ ./bin/files-in-current-directory
/path/to/project/Makefile
/path/to/project/README.md
/path/to/project/current-time.lisp
/path/to/project/files-in-current-directory.lisp
```

An optional directory argument can be passed:

```
$ ./bin/files-in-current-directory /some/other/path
```

Useful as an LLM tool to give the model a quick view of a project's source files.

---

## Integrating `current-time` with LLM Platforms

Make sure the binary is built and on your PATH (or use the absolute path):

```bash
make current-time
export PATH="$PWD/bin:$PATH"
```

### LM Studio

LM Studio supports tool-calling via its local OpenAI-compatible API. Define
`current-time` as a tool in your request payload:

```json
{
  "tools": [
    {
      "type": "function",
      "function": {
        "name": "current_time",
        "description": "Returns the current local date and time in ISO 8601 format.",
        "parameters": { "type": "object", "properties": {} }
      }
    }
  ]
}
```

When the model calls `current_time`, execute `./bin/current-time` in your client
code, capture stdout, and return the result as a `tool` role message.

### gemini-clj

In your Gemini Lisp client, register the tool as a function declaration and
provide a handler that shells out to the binary:

```lisp
;; Tool declaration (passed to the Gemini API)
(defparameter *current-time-tool*
  '(:name "current_time"
    :description "Returns the current local date and time in ISO 8601 format."
    :parameters (:type "object" :properties ())))

;; Handler — called when the model requests the tool
(defun handle-current-time ()
  (string-trim '(#\Newline #\Space)
               (with-output-to-string (s)
                 (sb-ext:run-program "/path/to/bin/current-time" '()
                                     :output s))))
```

Pass `*current-time-tool*` in the `tools` field of your API request and route
`functionCall` responses to `handle-current-time`.

### Claude Code

Add the tool to your `claude_tools` configuration (e.g. in `.claude/tools.json`
or inline in your API call):

```json
{
  "name": "current_time",
  "description": "Returns the current local date and time in ISO 8601 format.",
  "input_schema": { "type": "object", "properties": {} }
}
```

When Claude emits a `tool_use` block with `"name": "current_time"`, run:

```bash
./bin/current-time
```

and return the captured output in a `tool_result` content block. Claude Code's
`/tool` slash-command can also point directly at the binary by setting the
`command` field to the absolute path of `./bin/current-time`.
