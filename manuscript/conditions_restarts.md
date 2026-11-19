# Conditions, Restarts, and Resilient LLM Clients

Most languages handle errors with exceptions. When code throws an exception, the stack unwinds to the nearest matching `catch`, and the code that failed is gone. Common Lisp has the condition system, which can do that and more. It splits error handling into three parts:

1. **Signaling**: low-level code reports a problem by signaling a *condition*.
2. **Restarts**: low-level code also lists ways to recover, called *restarts*.
3. **Handlers**: high-level code decides which restart to use.

The key point: a handler runs *before* the stack unwinds. It can look at the problem and then choose a restart, and the program continues from the point the low-level code chose, with all its state intact. If no handler takes charge, the debugger shows the restarts and you choose one by hand.

The first half of this chapter covers the condition system with a small example that has nothing to do with AI. The second half applies it to a daily problem: calls to LLM services fail. Servers rate-limit you, local servers aren't running, a model name has a typo, and small models return broken JSON. We build a small library, `resilient-llm`, that offers restarts for each failure, plus three handler policies: retry with backoff, fall back to another model, and ask the model to repair its JSON.

The LLM code builds on the `litelm` library from the previous chapter. The live examples need a local Ollama server with the model `llama3.2:3b`. The tests need no server.

**Note: The source code is in the directory `src/resilient-llm`.**

## Project Structure

| File | Purpose |
|------|---------|
| `package.lisp` | Package definition and exports |
| `basics.lisp` | Condition system fundamentals |
| `resilient-llm.lisp` | Restarts for LLM calls, and retry, fallback, and JSON repair policies |
| `tests.lisp` | Offline tests that use fake models |
| `examples.lisp` | Live examples that use Ollama |
| `resilient-llm.asd` | ASDF system definitions |

```lisp
;;;; resilient-llm.asd -- conditions and restarts for robust LLM calls.

(asdf:defsystem #:resilient-llm
  :description "Condition system tutorial, plus retry, fallback, and JSON repair policies for LLM calls."
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:litelm #:usocket #:com.inuoe.jzon)
  :serial t
  :components ((:file "package")
               (:file "basics")
               (:file "resilient-llm")))

(asdf:defsystem #:resilient-llm/tests
  :description "Offline tests for resilient-llm. They use fake models, so no LLM server is needed."
  :depends-on (#:resilient-llm)
  :components ((:file "tests")))
```

We depend on `usocket` because the HTTP client `dexador` reports network failures with `usocket` conditions, and we need to name those types. The `jzon` library parses JSON replies from models.

## Condition System Fundamentals

### Defining a Condition

A condition is an object that describes a situation. You define a condition type with `define-condition`, which looks like `defclass`:

```lisp
(define-condition bad-age (error)
  ((text :initarg :text :reader bad-age-text))
  (:report (lambda (condition stream)
             (format stream "Not a valid age: ~S" (bad-age-text condition)))))
```

`bad-age` inherits from `error`. The `:report` function writes the message that the debugger shows and that `princ` prints.

The standard condition types form a hierarchy. The ones you will use most:

| Type | Use |
|------|-----|
| `condition` | Root of the hierarchy |
| `warning` | Something looks wrong, but the program can go on |
| `serious-condition` | Parent of `error` and of conditions such as storage exhaustion |
| `error` | The program cannot go on without help |

### Three Ways to Signal

| Function | If no handler takes charge |
|----------|----------------------------|
| `signal` | Returns `nil`, and the program continues |
| `warn` | Prints the warning to `*error-output*` and returns `nil` |
| `error` | Enters the debugger |

`signal` is useful for conditions that are not errors at all. Code can announce an event, and any caller who cares can listen. Later in this chapter we signal a `retrying` condition before each retry so callers can log it.

### handler-case: Unwind, Then Handle

`handler-case` works like `try`/`catch` in other languages. When a matching condition is signaled, the stack unwinds to the `handler-case` form, and the clause's value becomes the value of the form:

```lisp
(handler-case (parse-integer "abc")
  (parse-error () 0))
;; => 0
```

The code that signaled is gone by the time the clause runs, so it can't continue. `ignore-errors` is a shorthand for a `handler-case` that returns `nil` and the condition on any `error`.

### handler-bind: Handle Without Unwinding

`handler-bind` runs its handler function at the point of the signal, *on top of* the stack. The handler can do one of two things:

1. Transfer control somewhere else, for example by invoking a restart. This is how it takes charge.
2. Return normally. This *declines* the condition, and the search continues with the next handler out.

Handlers run innermost first. A handler that logs and returns lets an outer handler make the real decision.

### restart-case: Offer Ways to Recover

`restart-case` runs a form and names ways to recover if something goes wrong inside it. Here is `parse-age`, which offers two restarts:

```lisp
(defun parse-age (text)
  "Return TEXT as an integer age from 0 to 150. On bad input, signal BAD-AGE
with two restarts: USE-VALUE supplies a replacement age, and SKIP-ENTRY
returns NIL."
  (restart-case
      (let ((age (ignore-errors (parse-integer text))))
        (if (and age (<= 0 age 150))
            age
            (error 'bad-age :text text)))
    (use-value (value)
      :report "Supply an age to use instead."
      :interactive (lambda ()
                     (format *query-io* "Age to use: ")
                     (finish-output *query-io*)
                     (list (read *query-io*)))
      value)
    (skip-entry ()
      :report "Skip this entry."
      nil)))

(defun parse-ages (texts)
  "Parse each string in TEXTS. Entries skipped with SKIP-ENTRY are dropped."
  (loop for text in texts
        for age = (parse-age text)
        when age collect age))
```

Each restart clause has a name, a lambda list, options, and a body. When a handler invokes a restart, the stack unwinds to the `restart-case` form, and the restart's body supplies the value of the form. `:report` describes the restart in the debugger. `:interactive` is a function that prompts for the restart's arguments when you pick it in the debugger.

`use-value` is one of the standard restart names in Common Lisp. Each has a function of the same name that finds and invokes it: `use-value`, `store-value`, `continue`, `muffle-warning`, and `abort`. `skip-entry` is our own name; you invoke it with `(invoke-restart 'skip-entry)`.

### Choosing a Policy

`parse-age` never decides what to do about bad input. Callers decide by binding a handler that picks a restart. Here are three policies for the same low-level code:

```lisp
(defun parse-ages-skipping (texts)
  (handler-bind ((bad-age (lambda (condition)
                            (declare (ignore condition))
                            (invoke-restart 'skip-entry))))
    (parse-ages texts)))

(defun parse-ages-with-default (texts default)
  (handler-bind ((bad-age (lambda (condition)
                            (use-value default condition))))
    (parse-ages texts)))

(defun parse-ages-logging (texts)
  "Log each bad entry, then skip it. The first handler returns normally,
which declines the condition, so the next handler out gets its turn."
  (handler-bind ((bad-age (lambda (condition)
                            (declare (ignore condition))
                            (invoke-restart 'skip-entry))))
    (handler-bind ((bad-age (lambda (condition)
                              (format t "~&Log: ~A~%" condition))))
      (parse-ages texts))))
```

```lisp
(parse-ages-skipping '("42" "abc" "7" "200"))      ;; => (42 7)
(parse-ages-with-default '("42" "abc" "7" "200") 0) ;; => (42 0 7 0)
(parse-ages-logging '("1" "x"))
;; Log: Not a valid age: "x"
;; => (1)
```

Notice that `parse-ages` keeps looping after a bad entry. With `handler-case`, the first bad entry would unwind out of the loop and lose the ages already parsed. With `handler-bind` and restarts, the loop continues from the bad entry.

`parse-ages-logging` nests two handlers. The inner one prints and returns, which declines the condition, so the outer one runs and invokes `skip-entry`.

### Restarts in the Debugger

When no handler takes charge, the debugger lists the restarts, and you pick one by number or by name. Here is a session in the SBCL REPL:

```
* (parse-ages (list "42" "abc" "7"))

debugger invoked on a BAD-AGE in thread
#<THREAD tid=259 "main thread" RUNNING {80052904A3}>:
  Not a valid age: "abc"

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [USE-VALUE ] Supply an age to use instead.
  1: [SKIP-ENTRY] Skip this entry.
  2: [ABORT     ] Exit debugger, returning to top level.

(PARSE-AGE "abc")
   source: (ERROR 'BAD-AGE :TEXT TEXT)
0] use-value
Age to use: 30
(42 30 7)
```

The program asked for a value, used it, and finished the loop. In other languages you would fix the data and run the program again from the start. In Lisp you fix it inside the running program. This matters most for long jobs, such as a batch of a thousand LLM calls that fails on item 950.

## Resilient LLM Calls

Now we apply the same pattern to LLM calls. The low-level function `complete` offers restarts. Separate policy functions choose among them.

### Which Failures Can We Expect?

I ran three calls through `litelm` to see which conditions they signal:

| Situation | Condition signaled |
|-----------|--------------------|
| Unknown model name (HTTP 404) | `litelm:not-found-error` |
| Too many requests (HTTP 429) | `litelm:rate-limit-error` |
| Server not running | `usocket:invalid-argument-error`, a subtype of `usocket:socket-error` |

`litelm` maps HTTP status codes to its own conditions, all subtypes of `litelm:api-error`, which holds the status code and the response body. Network failures come from the socket library and never pass through `litelm`.

The refused connection surprised me. I expected `usocket:connection-refused-error`, but SBCL on macOS reported `usocket:invalid-argument-error`. I don't know why; the socket library maps the operating system error that way. This is a good reason to test against `usocket:socket-error`, the common parent, rather than a narrow subtype.

### The Low-Level Call

`complete` sends a prompt and returns the reply text. It offers three restarts:

```lisp
(defvar *default-model* "ollama/llama3.2:3b")

(defun litelm-completion (model prompt)
  (litelm:response-content (litelm:completion model :messages prompt)))

(defvar *completion-function* 'litelm-completion
  "Function of (MODEL PROMPT) that returns the reply text. Tests bind this
to a fake model.")

(defvar *sleep-function* 'sleep
  "Function of one argument, the number of seconds to wait. Tests bind
this to a function that only records the delay.")

(defun complete (prompt &key (model *default-model*))
  "Send PROMPT to MODEL and return the reply text. If the call fails, these
restarts are available: RETRY, USE-MODEL, and USE-VALUE."
  (loop
    (restart-case (return (funcall *completion-function* model prompt))
      (retry ()
        :report (lambda (stream) (format stream "Call ~A again." model)))
      (use-model (new-model)
        :report "Call a different model."
        :interactive (lambda ()
                       (format *query-io* "Model (for example \"ollama/qwen3.5:2b\"): ")
                       (finish-output *query-io*)
                       (list (read *query-io*)))
        (setf model new-model))
      (use-value (reply)
        :report "Return a reply of your choice instead."
        :interactive (lambda ()
                       (format *query-io* "Reply string: ")
                       (finish-output *query-io*)
                       (list (read *query-io*)))
        (return reply)))))
```

The `loop` and `restart-case` work together. On success, `return` exits the loop with the reply. When a handler invokes `retry`, the empty restart body returns, and the loop calls the model again. `use-model` changes the `model` variable and loops. `use-value` returns its argument from the loop.

The two special variables, `*completion-function*` and `*sleep-function*`, exist for testing. Tests bind them to fake functions so they run fast and need no server.

Before we write any policy, the restarts already help at the REPL:

```
* (complete "Reply with only the word yes." :model "ollama/no-such-model")

debugger invoked on a LITELM:NOT-FOUND-ERROR in thread
#<THREAD tid=259 "main thread" RUNNING {80052904A3}>:
  LLM API error 404: {"error":{"message":"model 'no-such-model' not found","type":"not_found_error","param":null,"code":null}}

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [RETRY    ] Call ollama/no-such-model again.
  1: [USE-MODEL] Call a different model.
  2: [USE-VALUE] Return a reply of your choice instead.
  3: [ABORT    ] Exit debugger, returning to top level.

(LITELM::%MAP-HTTP-ERROR 404 "{\"error\":{\"message\":\"model 'no-such-model' not found\",\"type\":\"not_found_error\",\"param\":null,\"code\":null}}
")
   source: (ERROR CONDITION :STATUS STATUS :BODY BODY)
0] use-model
Model (for example "ollama/qwen3.5:2b"): "ollama/llama3.2:3b"
"yes"
```

If you forget to start Ollama, you can start it in another terminal and pick `retry`. The call continues as if it had worked the first time.

### Which Errors Are Worth Retrying?

Some failures go away if you wait, and some never will. Retrying a bad API key wastes time. `transient-error-p` lists the ones worth retrying:

```lisp
(defun transient-error-p (condition)
  "True for errors that may go away if we wait: rate limits, server errors
\(HTTP 5xx), and network failures such as a refused connection."
  (or (typep condition 'litelm:rate-limit-error)
      (and (typep condition 'litelm:api-error)
           (integerp (litelm:api-error-status condition))
           (>= (litelm:api-error-status condition) 500))
      (typep condition 'usocket:socket-error)))
```

I did not test timeouts. `litelm` sets a 60 second connect timeout and a 120 second read timeout. If a timeout in your setup signals a condition type that this function does not list, add that type here.

### Exponential Backoff with Jitter

When a server says "too many requests", retrying at once makes things worse. The standard fix is exponential backoff: wait 1 second, then 2, then 4, up to a cap. Jitter adds randomness so that many clients that failed at the same moment don't all retry at the same moment:

```lisp
;; SBCL starts every process with the same *RANDOM-STATE*, so without
;; this seed every client would pick the same "random" delays.
(defvar *backoff-random-state* (make-random-state t))

(defun backoff-delay (attempt &key (base-delay 1.0) (max-delay 30.0))
  "Exponential backoff with jitter. The ceiling doubles with each ATTEMPT
\(1, 2, 3, ...) up to MAX-DELAY, and the delay is a random value from half
the ceiling to the full ceiling. The randomness keeps many clients that
failed at the same moment from retrying at the same moment."
  (let ((ceiling (min max-delay (* base-delay (expt 2 (1- attempt))))))
    (+ (/ ceiling 2) (random (/ ceiling 2.0) *backoff-random-state*))))
```

The comment about `*random-state*` comes from a bug I found while testing. My first version called `(random n)` with the default random state, and two separate runs printed the same three delays: 0.46, 0.72, and 1.26 seconds. Every SBCL process starts with the same random state, so the jitter did nothing to spread out separate clients. `(make-random-state t)` seeds a new state from a source of randomness.

### The Retry Policy

The policy is a `handler-bind` that invokes the `retry` restart for transient errors:

```lisp
(define-condition retrying (condition)
  ((attempt :initarg :attempt :reader retrying-attempt)
   (delay :initarg :delay :reader retrying-delay)
   (cause :initarg :cause :reader retrying-cause))
  (:report (lambda (condition stream)
             (format stream "Retry ~D in ~,2F seconds after: ~A"
                     (retrying-attempt condition)
                     (retrying-delay condition)
                     (retrying-cause condition)))))

(defun call-with-retries (thunk &key (max-retries 3) (base-delay 1.0) (max-delay 30.0))
  "Call THUNK. When a transient error reaches us and a RETRY restart is
available, wait and invoke RETRY, at most MAX-RETRIES times in total for
the whole call. Other errors pass through untouched."
  (let ((retries 0))
    (handler-bind
        ((error (lambda (condition)
                  (let ((restart (find-restart 'retry condition)))
                    (when (and restart
                               (transient-error-p condition)
                               (< retries max-retries))
                      (incf retries)
                      (let ((delay (backoff-delay retries :base-delay base-delay
                                                          :max-delay max-delay)))
                        ;; SIGNAL returns NIL when no one handles RETRYING,
                        ;; so this costs nothing unless a caller listens.
                        (signal 'retrying :attempt retries :delay delay
                                          :cause condition)
                        (funcall *sleep-function* delay)
                        (invoke-restart restart)))))))
      (funcall thunk))))

(defmacro with-retries ((&rest options &key max-retries base-delay max-delay)
                        &body body)
  "Run BODY, retrying transient LLM errors with exponential backoff."
  (declare (ignore max-retries base-delay max-delay))
  `(call-with-retries (lambda () ,@body) ,@options))
```

Three details matter:

1. The handler calls `find-restart` before it acts. If the error came from code that offers no `retry` restart, the handler declines.
2. When the retry budget runs out, the handler returns normally. That declines the condition, and an outer handler or the debugger gets it.
3. Before each retry, the handler signals `retrying`. `signal` returns `nil` when no one listens, so callers who don't care pay nothing. Callers who want a log bind a handler for `retrying`.

The pair of a `call-with-` function and a `with-` macro is a common Lisp idiom. The function holds the logic, so you can redefine it at the REPL without recompiling every caller. The macro only wraps the body in a `lambda`.

The retry budget covers everything inside `with-retries`. If the body makes several calls, they share the budget.

### The Fallback Policy

When a model fails for good, try another one. This policy invokes `use-model` with the next model from a list:

```lisp
(defun call-with-fallback-models (models thunk)
  "Call THUNK. When an LLM error reaches us, switch to the next model in
MODELS by invoking the USE-MODEL restart."
  (let ((remaining models))
    (handler-bind
        ((error (lambda (condition)
                  (let ((restart (find-restart 'use-model condition)))
                    (when (and restart
                               remaining
                               (or (typep condition 'litelm:litelm-error)
                                   (transient-error-p condition)))
                      (invoke-restart restart (pop remaining)))))))
      (funcall thunk))))

(defmacro with-fallback-models ((&rest models) &body body)
  "Run BODY. If a model call fails, try the next of MODELS."
  `(call-with-fallback-models (list ,@models) (lambda () ,@body)))
```

Policies combine by nesting, and the order matters. Put `with-retries` inside `with-fallback-models`:

```lisp
(with-fallback-models ("ollama/llama3.2:3b")
  (with-retries ()
    (complete "..." :model "ollama/no-such-model")))
```

The inner handler sees each error first. It retries transient errors until its budget runs out, then declines. The outer handler then switches models. A `not-found-error` is not transient, so the retry handler declines at once and the fallback handler switches models without waiting.

Since the retry budget is shared, a transient error on the fallback model gets no retries if the first model used up the budget. For a separate budget per model, write a policy that resets its counter when it invokes `use-model`.

### JSON Replies and Repair

We often ask a model for JSON. Small models wrap it in Markdown code fences, add a sentence before it, or write invalid JSON such as `{a: 1}` with an unquoted key. We handle the common case, code fences, in plain code, and we use a restart for the rest:

```lisp
(define-condition malformed-json (error)
  ((text :initarg :text :reader malformed-json-text)
   (cause :initarg :cause :reader malformed-json-cause))
  (:report (lambda (condition stream)
             (format stream "Model reply is not valid JSON (~A): ~S"
                     (malformed-json-cause condition)
                     (malformed-json-text condition)))))

(defun strip-code-fences (text)
  "Remove a Markdown code fence such as ```json ... ``` around TEXT. Small
models add these even when told not to."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         (start (and (>= (length trimmed) 3)
                     (string= "```" trimmed :end2 3)
                     (position #\Newline trimmed)))
         (end (and start (search "```" trimmed :from-end t))))
    (if (and start end (> end start))
        (string-trim '(#\Space #\Tab #\Newline #\Return)
                     (subseq trimmed (1+ start) end))
        trimmed)))

(defun parse-json-reply (text)
  "Parse TEXT as JSON, or signal MALFORMED-JSON."
  (handler-case (jzon:parse (strip-code-fences text))
    (error (condition)
      (error 'malformed-json :text text :cause condition))))
```

`parse-json-reply` uses `handler-case` to turn any parse error from `jzon` into our own `malformed-json` condition, which carries the bad text. Callers can then handle one condition type no matter which JSON library we use.

`complete-json` asks for JSON and offers a `repair-json` restart that sends the bad text and the parse error back to the model:

```lisp
(defun repair-prompt (text condition)
  (format nil "This text should be valid JSON, but parsing failed with: ~A~%~%~A~%~%~
Reply with only the corrected JSON, no explanation and no Markdown."
          (malformed-json-cause condition) text))

(defun complete-json (prompt &key (model *default-model*))
  "Ask MODEL for JSON and return it parsed with jzon: objects become hash
tables and arrays become vectors. If the reply is not valid JSON, these
restarts are available: REPAIR-JSON and USE-VALUE."
  (let ((text (complete prompt :model model)))
    (loop
      (restart-case (return (parse-json-reply text))
        (repair-json (condition)
          :report "Ask the model to fix its JSON."
          (setf text (complete (repair-prompt text condition) :model model)))
        (use-value (value)
          :report "Return a value of your choice instead."
          :interactive (lambda ()
                         (format *query-io* "Value: ")
                         (finish-output *query-io*)
                         (list (read *query-io*)))
          (return value))))))

(defmacro with-json-repair ((&key (max-repairs 2)) &body body)
  "Run BODY. When a model reply is not valid JSON, ask the model to repair
it, at most MAX-REPAIRS times in total."
  (let ((repairs (gensym "REPAIRS")))
    `(let ((,repairs 0))
       (handler-bind
           ((malformed-json
              (lambda (condition)
                (let ((restart (find-restart 'repair-json condition)))
                  (when (and restart (< ,repairs ,max-repairs))
                    (incf ,repairs)
                    (invoke-restart restart condition))))))
         ,@body))))
```

The `repair-json` restart takes the condition as its argument, so the repair prompt can quote the parse error. The model then knows what to fix.

The repair call goes through `complete`, so it has its own `retry` and `use-model` restarts. If the server rate-limits the repair call, an outer `with-retries` handles it like any other call.

Since `with-json-repair` uses a `gensym` for its counter, the macro's variable can't clash with a variable in the body.

## Testing with Fake Models

Since `complete` calls the model through `*completion-function*`, tests can bind a fake model. This helper returns a fake that fails a set number of times with a rate-limit error, then succeeds:

```lisp
(defun rate-limit ()
  (error 'litelm:rate-limit-error :status 429 :body "slow down"))

(defun make-flaky-model (failures reply)
  "A fake model that signals a rate limit FAILURES times, then returns REPLY.
The second value of the result counts calls."
  (let ((calls 0))
    (values (lambda (model prompt)
              (declare (ignore model prompt))
              (incf calls)
              (if (<= calls failures) (rate-limit) reply))
            (lambda () calls))))

(defmacro with-fake-model ((function) &body body)
  `(let ((*completion-function* ,function)
         (*sleep-function* (lambda (seconds) (declare (ignore seconds)))))
     ,@body))
```

`with-fake-model` also binds `*sleep-function*` to a function that does nothing, so the retry tests take no time. Here are two of the tests from `tests.lisp`:

```lisp
  (multiple-value-bind (model calls) (make-flaky-model 2 "ok")
    (let ((delays '()))
      (check "retry twice, then succeed"
             (with-fake-model (model)
               (handler-bind ((retrying (lambda (condition)
                                          (push (retrying-attempt condition) delays))))
                 (with-retries () (complete "hi"))))
             "ok")
      (check "three calls in total" (funcall calls) 3)
      (check "RETRYING signaled for attempts 1 and 2" (reverse delays) '(1 2))))
```

```lisp
  (let ((prompts '()))
    (flet ((model (model prompt)
             (declare (ignore model))
             (push prompt prompts)
             (if (= (length prompts) 1) "{a: 1}" "{\"a\": 1}")))
      (check "no repair policy: MALFORMED-JSON escapes"
             (with-fake-model (#'model) (complete-json "give me json"))
             '(:error malformed-json))
      (setf prompts '())
      (check "repair malformed JSON"
             (with-fake-model (#'model)
               (with-json-repair ()
                 (gethash "a" (complete-json "give me json"))))
             1)
      (check "the repair prompt quotes the bad reply"
             (and (search "{a: 1}" (first prompts)) t)
             t)))
```

Run all 23 tests with:

```lisp
(ql:quickload :resilient-llm/tests)
(resilient-llm::run-tests)
```

```
pass  skip bad ages
pass  replace bad ages with a default
pass  log, then skip
pass  no handler: the error escapes
pass  retry twice, then succeed
pass  three calls in total
pass  RETRYING signaled for attempts 1 and 2
pass  give up after max-retries
pass  one call plus three retries
pass  no retry for errors that are not transient
pass  called once
pass  a refused connection is transient
pass  HTTP 503 is transient
pass  HTTP 401 is not transient
pass  backoff stays between half the ceiling and the ceiling
pass  fall back to the next model
pass  models tried in order
pass  a handler can supply a canned reply with USE-VALUE
pass  strip a ```json fence
pass  parse a fenced reply
pass  no repair policy: MALFORMED-JSON escapes
pass  repair malformed JSON
pass  the repair prompt quotes the bad reply
All tests passed.
```

## Live Examples with Ollama

The file `examples.lisp` runs three examples against a local Ollama server. To simulate a server that is down, it registers a `litelm` provider on a port where nothing listens:

```lisp
(litelm:define-provider :ollama-down "http://localhost:11999/v1" :requires-key nil)

(defun log-retries (condition)
  (format t "~&~A~%" condition))

(defun example-fallback ()
  (format t "~&--- Missing model, fall back to llama3.2:3b~%")
  (with-fallback-models ("ollama/llama3.2:3b")
    (with-retries ()
      (complete "In one sentence, what is Common Lisp?"
                :model "ollama/no-such-model"))))

(defun example-retries-then-fallback ()
  (format t "~&--- Server down: retry with backoff, then fall back~%")
  (handler-bind ((retrying #'log-retries))
    (with-fallback-models ("ollama/llama3.2:3b")
      (with-retries (:max-retries 3 :base-delay 0.5)
        (complete "Name one Common Lisp implementation. Reply with only its name."
                  :model "ollama-down/llama3.2:3b")))))

(defun example-json ()
  (format t "~&--- JSON reply~%")
  (let ((result
          (with-json-repair ()
            ;; This handler runs first and returns normally, so it only
            ;; logs; WITH-JSON-REPAIR then picks the REPAIR-JSON restart.
            (handler-bind ((malformed-json
                             (lambda (condition)
                               (format t "~&Repairing: ~A~%" condition))))
              (complete-json
               "Return a JSON object with keys \"name\" and \"year\" for the
programming language Common Lisp, where year is the year ANSI approved
the standard. Reply with only the JSON.")))))
    (format t "name: ~A, year: ~A~%"
            (gethash "name" result) (gethash "year" result))
    result))
```

```lisp
(ql:quickload :resilient-llm)
(load "examples.lisp")
```

```
--- Missing model, fall back to llama3.2:3b
Common Lisp is a general-purpose, high-level programming language based on the ANSI standard for Lisp, known for its macro system, strong type inference, and extensive standard library, making it a powerful and flexible language for both rapid prototyping and long-term development.
--- Server down: retry with backoff, then fall back
Retry 1 in 0.31 seconds after: Condition USOCKET:INVALID-ARGUMENT-ERROR was signalled.
Retry 2 in 0.94 seconds after: Condition USOCKET:INVALID-ARGUMENT-ERROR was signalled.
Retry 3 in 1.36 seconds after: Condition USOCKET:INVALID-ARGUMENT-ERROR was signalled.
SBCL
--- JSON reply
name: Common Lisp, year: 1994
```

In the second example the refused connection is transient, so the retry policy waited three times, with delays that grow, then declined. The fallback policy took over and switched to the working model. Your delays will differ because of the jitter.

In the third example `llama3.2:3b` returned valid JSON on the first try, so no "Repairing" line appeared. The repair path runs in the offline tests, where a fake model returns `{a: 1}` first.

## Wrap Up

The condition system separates three jobs that most languages tie together: reporting a problem, offering ways to recover, and choosing one. In this chapter:

- `complete` and `complete-json` report problems and offer the `retry`, `use-model`, `use-value`, and `repair-json` restarts, but decide nothing.
- `with-retries`, `with-fallback-models`, and `with-json-repair` each pick restarts for one kind of problem, and they combine by nesting.
- With no policy in place, the debugger offers the same restarts, so you can fix a failed call by hand and keep the program's state.

The same pattern applies to other code in this book. A RAG pipeline that embeds thousands of documents could offer a `skip-document` restart, and a web scraper could offer `retry` and `skip-url` restarts.
