# cl-llm-agent

cl-llm-agent is a Common Lisp framework for building LLM-based agents that can use tools,
maintain context, and interface with different LLM back-ends.

## Features
- Context management (get, set, remove)
- Tool registration and execution
- Generic agent abstraction with pluggable back-ends
- Example Gemini-based agent
- Testing suite with FiveAM

## Installation
1. Clone the repository.
2. In your Lisp REPL:
   ```lisp
   (ql:quickload :cl-llm-agent)
   ```

## Usage
Define tools and agents, then interact:
```lisp
  (defparameter *ctx* (cl-llm-agent:make-context))
  (cl-llm-agent:context-set *ctx* "task" "summarize text")
  (defparameter *agent*
    (cl-llm-agent:make-agent 'cl-llm-agent::gemini-agent :context *ctx*))
  (cl-llm-agent:agent-converse *agent* "Summarize the project README.")
```

## Testing
Run in your Lisp REPL:
```lisp
  (ql:quickload :cl-llm-agent)
  (asdf:test-system :cl-llm-agent)
```
