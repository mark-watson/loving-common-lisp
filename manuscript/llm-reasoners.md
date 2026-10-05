# Advanced Reasoning with World Models and LLMs: The llm-reasoning Library

**Dear reader, in the litelm chapter, we gave our Lisp programs a way to talk to a language model. This chapter asks a different question: what do we do when a single call to the model is not enough? We will build a small library, llm-reasoning, that turns a reasoning method into a search over states and actions, driving it with a local model to solve arithmetic word problems.
The design is inspired by the Python [llm-reasoners](https://github.com/maitrix-org/llm-reasoners) library. Its central idea is worth stating plainly because it helps to explain everything else in this chapter: a reasoning method is not a prompt. It is a search. By framing the problem with states, transitional actions, and scoring heuristics, we can systematically steer LLMs. Traditional techniques like chain of thought, beam search, best-first search, and Monte Carlo tree search all map to this architecture. They differ primarily in their exploration width and evaluation logic.

By adding reasoning we can help steer LLMs. In some sense LLMs can perform some reasoning but by monitoring the outputs of an LLM and using complex reasoning, we can improve a LLM’s native (innate) reasoning ability.

The source code lives in the directory **loving-common-lisp/src/llm-reasoners**. It has two files and no `ASD` system definition:

| File | What it is |
|---|---|
| `llm-reasoning-lib.lisp` | The library: protocol, litelm backend, beam search, chain of thought, answer handling, self-tests |
| `CoT-gsm8k-example.lisp` | A worked example: chain of thought over GSM8K, using a local oMLX model |

The only dependency is **litelm**, from the previous chapter. Both files load by path, and the library bootstraps Quicklisp and finds `litelm.asd` on its own, so `sbcl --script` works as well as an interactive session.

## Three Pieces

The Python library splits a reasoning method into three parts, and so do we.

| Piece | Responsibility | Protocol |
|---|---|---|
| **LanguageModel** | Talk to the model | `generate` |
| **WorldModel** | State, dynamics, terminal test | `init-state`, `step`, `is-terminal` |
| **SearchConfig** | Which actions to try, and how good each is | `get-actions`, `fast-reward`, `reward` |

A **search-algorithm** explores a world model using a search config. A **reasoner** holds one of each. Chain of thought needs no search at all, so it lives apart as **cot-reasoner**. That mirrors the Python repo, where CoT is a plain class rather than a `Reasoner`.

Both world models and search configs need to know which problem they are solving, so they share a base class that carries it:

```lisp
(defclass reasoning-component ()
  ((example :initarg :example :accessor component-example :initform nil)
   (prompt  :initarg :prompt  :accessor component-prompt  :initform nil))
  (:documentation "Shared per-problem state: the example and its prompt."))

(defgeneric update-example (component example prompt)
  (:documentation "Bind EXAMPLE and PROMPT, then return COMPONENT."))
```

This is the only mutable per-problem state in the library. A **reasoner** calls **update-example** on both components before each search, so one reasoner object can serve a stream of different problems without rebuilding anything.

## The World Model

Three generics describe a world:

```lisp
(defgeneric init-state (world-model)
  (:documentation "Return the initial state."))

(defgeneric step (world-model state action)
  (:documentation "Apply ACTION in STATE.  Returns (values next-state aux-plist),
where AUX may carry :CONFIDENCE for the default REWARD method."))

(defgeneric is-terminal (world-model state)
  (:documentation "True when STATE ends the search."))
```

Note the two values from **step**. The first is the next state, and the second is a plist of whatever else the step learned. The library only looks for one key, `:confidence`, but your own world model can carry anything it likes down that channel.

The **search-config** side is just as small:

```lisp
(defgeneric get-actions (search-config state))
(defgeneric fast-reward (search-config state action))
(defgeneric reward (search-config state action &key r-useful confidence))
```

Why two reward functions? Because search asks two different questions. **fast-reward** answers "is this action worth expanding at all", and it runs once per candidate, so it must be cheap. **reward** answers "how good is the state this action produced", and it may call the model. Separating them lets you keep the beam wide without paying for a model call on every candidate you discard.

## Scoring a Move

The default **reward** method combines two numbers into one. It takes the usefulness of the action and the confidence in the resulting state, and returns their weighted geometric mean:

```$
r(s,a) \;=\; r_{\mathrm{useful}}^{\,\alpha} \;\cdot\; c^{\,1-\alpha}
```

Here `\alpha`$ is the slot **search-config-reward-alpha**, which defaults to `0.5`, and `c`$ is the confidence from **step**, which defaults to `0.8`. The code is three lines:

```lisp
(defmethod reward ((config search-config) state action &key r-useful (confidence 0.8))
  (declare (ignore state action))
  (let ((alpha (search-config-reward-alpha config)))
    (* (expt (float (or r-useful 0.0)) alpha)
       (expt (float (or confidence 0.8)) (- 1 alpha)))))
```

A geometric mean is the right choice here, and it is worth pausing on why. An arithmetic mean would let a very confident move compensate for a useless one. A geometric mean will not: if either factor is zero, the score is zero. With `\alpha = 0.5`$ the two factors count equally, and raising `\alpha`$ makes the search trust its own action scoring over the model's stated confidence.

## Nodes and Beam Search

A node in the search tree records three things:

```lisp
(defstruct (search-node (:constructor make-search-node (&key state reward trace)))
  (state nil)
  (reward 0.0)
  (trace nil))
```

The **trace** is the list of actions that reached this state. Keeping it on every node costs a little memory and buys you an explainable answer: when the search finishes you can print the path it took, not just where it ended.

Beam search keeps a fixed number of live nodes. At each depth it expands every non-terminal node in the beam, scores each child, keeps the best **beam-size** of them, and stops as soon as any survivor is terminal:

```lisp
(defclass beam-search (search-algorithm)
  ((beam-size :initarg :beam-size :accessor beam-search-beam-size :initform 3)
   (max-depth :initarg :max-depth :accessor beam-search-max-depth :initform 4)
   (verbose   :initarg :verbose   :accessor beam-search-verbose   :initform nil)))
```

The heart of the **search** method is one nested loop:

```lisp
(dolist (node beams)
  (unless (is-terminal wm (search-node-state node))
    (dolist (action (get-actions config (search-node-state node)))
      (multiple-value-bind (next aux)
          (step wm (search-node-state node) action)
        (let* ((useful (fast-reward config (search-node-state node) action))
               (r (reward config (search-node-state node) action
                          :r-useful useful
                          :confidence (getf aux :confidence 0.8)))
               (child (make-search-node
                       :state next
                       :reward (+ (search-node-reward node) r)
                       :trace (append (search-node-trace node) (list action)))))
          (push child candidates))))))
```

Rewards accumulate down the path, so a node's **reward** is the sum of every step that produced it. Pruning is a sort and a **subseq**:

```lisp
(defun %best-nodes (nodes n)
  "The N NODES with the highest accumulated reward."
  (let ((sorted (sort (copy-list nodes) #'> :key #'search-node-reward)))
    (if (> (length sorted) n) (subseq sorted 0 n) sorted)))
```

The **copy-list** matters. **sort** destroys its argument, and the caller still needs the original list.

Pass `:verbose t` to **make-beam-search** and it prints the winning path when the search ends, which is the fastest way to see whether your reward function is doing what you meant.

## The Reasoner

With the three pieces in place, a reasoner is short:

```lisp
(defmethod solve ((r reasoner) example &key prompt)
  (update-example (reasoner-world-model r) example prompt)
  (update-example (reasoner-search-config r) example prompt)
  (search (reasoner-search-algorithm r)
          (reasoner-world-model r)
          (reasoner-search-config r)))
```

Bind the problem onto both components, then search. **solve** returns the best **search-node**, so you get the final state, its score, and the action trace that produced it.

Here is a complete reasoning method for testing purposes with no model in it at all. Add 1, 2, or 3 until you reach 10:

```lisp
(defclass count-to-five-world-model (llm-reasoning:world-model) ())

(defmethod llm-reasoning:init-state ((wm count-to-five-world-model)) 0)

(defmethod llm-reasoning:is-terminal ((wm count-to-five-world-model) state)
  (>= state 5))

(defmethod llm-reasoning:step ((wm count-to-five-world-model) state action)
  (values (+ state action) (list :confidence 1.0)))

(defclass count-to-five-config (llm-reasoning:search-config) ())

(defmethod llm-reasoning:get-actions ((sc count-to-five-config) state)
  (if (>= state 5) '() '(1 2)))

(defmethod llm-reasoning:fast-reward ((sc count-to-five-config) state action)
  (declare (ignore state action))
  1.0)

(defmethod llm-reasoning:reward ((sc count-to-five-config) state action
                                 &key r-useful confidence)
  (declare (ignore r-useful confidence))
  (let ((next (+ state action)))
    (cond ((= next 5) 1.0)
          ((< next 5) (* 0.9 (/ next 5.0)))
          (t 0.0))))
```

Five generics you must implement: **init-state**, **step**, and **is-terminal** on the world model, **get-actions** and **fast-reward** on the search config. None has a default method, so omitting one fails at search time with a `no-applicable-method` error rather than a warning when you define the class. **reward** is optional: the default method on **search-config** applies unless you override it, and **update-example** and **model-name** have defaults too.

Run it and you get the shortest path it could find:

```lisp
(let ((node (llm-reasoning:solve
             (llm-reasoning:make-reasoner
              :world-model (make-instance 'count-to-five-world-model)
              :search-config (make-instance 'count-to-five-config)
              :search-algorithm (llm-reasoning:make-beam-search
                                 :beam-size 3 :max-depth 5))
             "count to five")))
  (llm-reasoning:search-node-state node)   ; => 5
  (llm-reasoning:search-node-trace node))  ; => (2 2 1)
```

**solve** takes an example argument, here the string `"count to five"`. This world model ignores it, since the target is baked into the methods, but a real one would read it in **init-state** through **component-example**.

That reward function deserves a comment, because it looks odd. Landing exactly on the target scores `1.0`. Overshooting scores `0.0`. Undershooting scores `0.9` times the fraction of the way there. The `0.9` factor is what makes hitting the target strictly better than getting close, and the monotone fraction is what keeps the beam moving forward instead of stalling. If overshooting scored anything at all, the search would happily run past the goal.

## Making the World Model Call the Model

Everything above is pure Lisp. To get the Reasoning via Planning (RAP) pattern, where the model itself proposes the next state, you put a **generate** call inside **step**:

```lisp
(defmethod llm-reasoning:step ((wm my-world-model) state action)
  (let ((text (llm-reasoning:generate (my-world-model-lm wm)
                                      (build-prompt state action)
                                      :temperature 0.0)))
    (values (parse-next-state text)
            (list :confidence 1.0))))
```

Nothing else changes. The search, the beam, and the reasoner do not know or care whether **step** is arithmetic or a model call. That is the payoff of keeping the world model behind a generic function.

## The Language Model

The library ships one backend, and it is four slots deep:

```lisp
(defclass litelm-model (language-model)
  ((model :initarg :model :accessor litelm-model-model :initform nil)
   (max-tokens :initarg :max-tokens :accessor litelm-model-max-tokens :initform 512)
   (temperature :initarg :temperature :accessor litelm-model-temperature :initform 0.0)
   (retries :initarg :retries :accessor litelm-model-retries :initform 3)
   (pause :initarg :pause :accessor litelm-model-pause :initform 1.0)))
```

**generate** sends the prompt as a single user message. litelm accepts a bare string for `:messages` and turns it into one user turn, which keeps this code short:

```lisp
(defmethod generate ((model litelm-model) prompt &key max-tokens temperature)
  (let ((attempt 0))
    (loop
      (incf attempt)
      (handler-case
          (let ((response (litelm:completion
                           (litelm-model-model model)
                           :messages prompt
                           :max-tokens (or max-tokens (litelm-model-max-tokens model))
                           :temperature (or temperature
                                            (litelm-model-temperature model)))))
            (return (or (litelm:response-content response) "")))
        (error (e)
          (when (>= attempt (litelm-model-retries model))
            (error "litelm completion failed after ~D attempt~:P: ~A" attempt e))
          (format *error-output* "~&litelm error (attempt ~D): ~A~%" attempt e)
          (sleep (* attempt (litelm-model-pause model))))))))
```

The retry loop is not decoration. A local oMLX server loads the model on first request, and that load can take longer than the HTTP timeout. Backing off linearly, `1` second then `2` then `3`, turns a cold start into a slow success instead of a crash. Note also that **generate** never returns nil. An empty completion becomes `""`, so callers do not need a nil check on every result.

Because the model is just a litelm string, swapping providers is swapping a string:

```lisp
(llm-reasoning:make-litelm-model :model "omlx/Laguna-XS-2.1-6bit")   ; the default
(llm-reasoning:make-litelm-model :model "openai/gpt-4o")              ; needs OPENAI_API_KEY
```

## Answer Handling

Getting a number out of a paragraph of model prose is the unglamorous half of any benchmark, and it is where most reported accuracies quietly go wrong. The library has six functions for it.

**normalize-answer** strips the characters that vary between a model's phrasing and a dataset's gold answer:

```lisp
(normalize-answer "$1,000.")  ; => "1000"
```

It accepts a string, symbol, character, or number. Numbers matter more than they look: a gold answer read from JSON arrives as an integer, and comparing an integer to a string answer would otherwise fail on a type error rather than report a mismatch.

**retrieve-answer** finds the answer in a chain of thought. It searches for the phrase "the answer is", case-insensitively, takes the *last* occurrence so a model that reconsiders still gives you its final word, then reads the first numeric run after it:

```lisp
(retrieve-answer "She sold 48 + 24 = 72 clips. The answer is 72.")  ; => "72"
(retrieve-answer "just some text")                                  ; => NIL
```

Returning nil when the phrase is absent is a deliberate choice. An unparseable completion counts as a wrong answer rather than raising an error, so a benchmark run finishes and reports a number instead of stopping on the first rambling model.

**answer-equal** compares two answers. It tries them as numbers first and falls back to a case-insensitive string compare, so `"18"`, `"18."`, `"18.0"`, and `18` all agree. **answer-from-dataset** takes whatever follows `####` in a GSM8K gold field. **majority-vote** returns the most frequent non-nil answer, breaking ties by first appearance. **accuracy** scores two parallel lists:

```lisp
(accuracy '("18" "3") '(18 4))  ; => 0.5
```

One security note. **%as-number** parses model output with **read-from-string**, so it binds `*read-eval*` to nil first. Without that, a completion containing `#.` could run arbitrary code at parse time. Model output is untrusted input.

## Chain of Thought

CoT needs none of the search machinery. It builds one prompt and calls the model:

```lisp
(defun cot-prompt (few-shot question &key instruction)
  (concatenate 'string
               (or instruction "")
               (apply #'concatenate 'string few-shot)
               "Q: " question (string #\Newline) "A:"))
```

**few-shot** is a list of `"Q: ... A: ..."` strings, each ending in a blank line. **cot-solve** returns two values, the answer and the full list of completions:

```lisp
(defmethod cot-solve ((r cot-reasoner) question)
  (let* ((prompt (cot-prompt (cot-reasoner-few-shot r) question
                             :instruction (cot-reasoner-instruction r)))
         (n (max 1 (cot-reasoner-n-samples r)))
         (completions '())
         (answers '()))
    (dotimes (i n)
      (let* ((text (generate (cot-reasoner-model r) prompt
                             :temperature (cot-reasoner-temperature r)))
             (answer (retrieve-answer text)))
        (push text completions)
        (push answer answers)))
    (setf completions (nreverse completions)
          answers (nreverse answers))
    (values (if (= n 1)
                (first answers)
                (majority-vote answers))
            completions)))
```

Set **n-samples** above `1` with a positive temperature and this becomes self-consistency: sample several chains, extract each answer, take the majority. The prompt is built once and reused, which is the whole trick. Diverse reasoning paths to the same question, one prompt.

Returning the completions alongside the answer costs nothing and saves you re-running an expensive benchmark when you want to read what the model actually said.

## Testing Without a Model

A library that needs a live model to test itself is a library nobody tests. So **llm-reasoning-lib.lisp** carries a toy world model, add 1, 2, or 3 until you reach 10, and a **run-self-tests** function that exercises the answer handling and the search against it:

```bash
$ sbcl --non-interactive --load llm-reasoning-lib.lisp \
       --eval '(llm-reasoning:run-self-tests)'
llm-reasoning self-tests
  ok    normalize-answer strips $ , and trailing period
  ok    normalize-answer accepts a number
  ...
  ok    a REASONER drives the same search
20/20 checks passed
```

Twenty checks, no network, no server, no API key. **run-self-tests** returns `T` on success and signals an error on failure, so it works as a CI gate:

```bash
sbcl --non-interactive --load llm-reasoning-lib.lisp \
     --eval '(llm-reasoning:run-self-tests)' || echo "BROKEN"
```

The checks split into two groups. Sixteen cover answer handling: normalization, numeric and string comparison, nil handling, answer retrieval with and without the trigger phrase, `####` extraction, majority voting, and accuracy. Four cover the search: that beam search reaches the toy target, that it reports a reward of at least `1.0`, that it records the actions it took, and that a **reasoner** drives the same search to the same state.

## The GSM8K Example

**CoT-gsm8k-example.lisp** puts all of it to work. It embeds four few-shot chains, the same ones the upstream Python example reads from `examples/CoT/gsm8k/prompts/cot.json`, and six questions from the GSM8K test split as `(question . gold)` pairs. Both live in top-level parameters, `*cot-few-shot*` and `*gsm8k-examples*`, so extending the benchmark means extending a list.

Two settings in that file matter more than they look.

The first is the instruction prepended to every prompt:

```lisp
(defparameter *answer-instruction*
  "Your response need to be ended with \"So the answer is\"\n\n")
```

Yes, "need to be ended" is ungrammatical. It is quoted verbatim from the upstream example, and matching it exactly is what makes the accuracy figures comparable. Without some instruction like this, small local models ramble past the point where they would have stated an answer, and **retrieve-answer** finds nothing.

The second is the token budget:

```lisp
(defparameter *max-tokens* 2048)
```

At `1024` the wordiest question in the set, the one about a 150 percent profit, gets cut off mid-deliberation. The model reconsiders the percentage, talks past the budget, and you lose an answer the reasoning had already earned. At `2048` it finishes.

Run it from the source directory:

```bash
$ sbcl --script CoT-gsm8k-example.lisp            # all six questions
$ sbcl --script CoT-gsm8k-example.lisp 2          # the first two
$ sbcl --script CoT-gsm8k-example.lisp 6 5        # six questions, five samples each
```

Here is a real run, against the local oMLX server, in 57 seconds for all six:

```
model:     omlx/Laguna-XS-2.1-6bit
questions: 6
samples:   1

[1/6] Janet's ducks lay 16 eggs per day. She eats three for breakfast every
morning and bakes muffins for her friends every day with four. She sells the
remainder at the farmers' market daily for $2 per fresh duck egg. How much in
dollars does she make every day at the farmers' market?
  chain:  Janet's ducks lay 16 eggs per day. She eats 3 eggs and uses 4 eggs
  for baking muffins daily. The remaining eggs are 16 - 3 - 4 = 9. She sells
  these 9 eggs at $2 each, so she earns 9 * $2 = $18 daily. The answer is 18.
  answer: 18   gold: 18   correct
...
accuracy: 1.0000 (6/6)
```

Six out of six. The **run** function returns `(values accuracy correct-count question-count)`, so you can call it from Lisp and collect results across models without parsing the printed output.

Each chain prints shortened, first 300 characters and last 200, by **%summarize-chain**. Keeping the tail is the point: "The answer is ..." appears at the end, so when a parse fails you can see immediately whether the model never said it or the extractor missed it.

Loading the file runs **main** once. To load it for inspection without contacting a model, pre-create the switch. It lives in a package the file itself defines, so it cannot simply be bound beforehand:

```lisp
(setf (symbol-value (intern "*RUN-ON-LOAD*"
                            (or (find-package :cot-gsm8k-example)
                                (make-package :cot-gsm8k-example))))
      nil)
(load "CoT-gsm8k-example.lisp")
```

## Choosing a Local Model

The examples run against **oMLX**, an OpenAI-compatible server for Apple Silicon at `http://127.0.0.1:8000/v1`. It needs no API key, so you can try everything in this chapter for free:

```bash
$ curl -s http://127.0.0.1:8000/v1/models
```

Prefer `omlx/Laguna-XS-2.1-6bit`. In an eight question GSM8K comparison it was both the fastest and the most accurate of the three local candidates tested while writing this chapter: 78 seconds at 77.6 tokens per second, against 56 seconds at 62.6 for gemma-4 and 1420 seconds at 13.7 for Qwen3.8.

Do not route gemma-4 through litelm. That MLX repo ships no `chat_template`, so oMLX returns an empty assistant message, and raw non-chat completions just echo your prompt back. The upstream Python repo works around this inside its own oMLX backend by applying a Gemma turn template before sending. litelm has no equivalent hook, so there is nothing to configure. The blank reply is easy to mistake for a prompt bug or a token budget problem.

oMLX also exposes no token log-probabilities. It accepts `logprobs` and `top_logprobs` in the request and drops them. That rules out the score-candidate family of reasoning algorithms, including self-evaluation, against this server. Use a sampling based reward instead, as the upstream RAP example does.

## Gotchas

### STEP and SEARCH are shadowed

The protocol wants the names llm-reasoners uses, and two of them are already taken. **step** is a Common Lisp macro and **search** is a Common Lisp function, so the package shadows both:

```lisp
(defpackage #:llm-reasoning
  (:use #:cl)
  (:shadow #:step #:search)
  ...)
```

A package that does `(:use #:cl #:llm-reasoning)` now has a conflict and must **shadowing-import** those two symbols. The example dodges the problem by importing only what it needs:

```lisp
(defpackage #:cot-gsm8k-example
  (:use #:cl)
  (:import-from #:llm-reasoning
                #:make-litelm-model #:make-cot-reasoner #:cot-solve
                #:model-name #:answer-equal #:accuracy))
```

Inside the library, the Common Lisp **search** is written `cl:search`. Miss that qualification and the answer extractor silently calls your generic function instead of the string search.

### dexador and `*PRINT-CASE*`

This one is genuinely nasty, and it has nothing to do with reasoning.

litelm POSTs through dexador. dexador's `DEFINE-ALIST-CACHE` macro builds function names with `(FORMAT NIL "LOOKUP-IN-~A" ...)`, and **format** obeys `*print-case*`. If your `~/.sbclrc` contains `(setf *print-case* :downcase)`, as many do for prettier REPL output, dexador compiles a definition named `|LOOKUP-IN-content-encoding-cache|` while its own source references the symbol `LOOKUP-IN-CONTENT-ENCODING-CACHE`. Every POST that carries a body then dies:

```
The function dexador.body::lookup-in-content-encoding-cache is undefined.
```

The part that, dear reader, took me a while to figure out is that `dex:get` keeps working, because the bug is only in the POST path. Nothing about that error message suggests a symbol case problem.

The library binds `*print-case*` to `:upcase` around its own litelm load, so a fresh compile comes out correct. If a bad fasl is already cached, force one rebuild:

```lisp
(let ((*print-case* :upcase)) (asdf:load-system :dexador :force t))
```

### --script skips your init file

`sbcl --script file.lisp` does not load `~/.sbclrc`. Quicklisp is missing and your project directories are unregistered. The library handles both itself: it loads `~/quicklisp/setup.lisp` if present, then searches for `litelm.asd` in the usual places and honours a `LITELM_ASD` environment variable if litelm lives somewhere else. A useful side effect is that `--script` also skips a `*print-case*` setting in your init file, which sidesteps the dexador bug above.

## What Is Not Here

The port is deliberately narrow, and it helps to know the edges.

There is one search algorithm. No MCTS, no depth first, no best first. Adding one means subclassing **search-algorithm** and writing a **search** method, which is a small job, but nobody has done it yet.

There is no dataset loading. The example embeds six GSM8K rows and four prompts as literal lists. Extend `*gsm8k-examples*` or read a file yourself.

There is no visualization and no run logging beyond the `:verbose` flag on beam search.

## Known Limitations

A few rough edges are worth knowing before they cost you an afternoon.

**retrieve-answer** takes the *first* numeric run after "the answer is", not the last number in the sentence. A chain that hedges, "the answer is unclear, but 7 apples were left", yields `7`. A non-numeric answer yields nil.

**make-reasoner** does not check its arguments. Omit `:world-model` and the failure surfaces later as a `no-applicable-method` error on **update-example**, which does not point at the real mistake.

**litelm-model-pause**, the retry backoff, has an accessor but is not a keyword on **make-litelm-model**. Set it with **setf** after construction.

The **batch-size** slot on **search-config** is defined and never read.

## Chapter Wrap Up

Dear reader, we started with a claim: a reasoning method is a search, not a prompt. This chapter made that claim concrete in about 650 lines of Common Lisp. Three generic function protocols, **language-model**, **world-model**, and **search-config**, separate what the model does from what the problem does from how we score a move. A **search-node** struct carries state, accumulated reward, and the action trace. Beam search prunes with a sort and a **subseq**. And **solve** is three lines, because by the time you reach it every interesting decision lives behind a generic function.

The reward formula, `r_{\mathrm{useful}}^{\alpha} \cdot c^{1-\alpha}`$, is the one place where a small mathematical choice carries real weight. A geometric mean refuses to let confidence excuse a useless action, and that single decision is what keeps the beam honest.

Along the way we met the practical side. Answer extraction from prose is where benchmark numbers are won or lost, so six small functions do it carefully and `*read-eval*` stays off. Twenty offline self-tests mean you can change the search without a model running. And two environment bugs, symbol case in dexador and a missing init file under `--script`, will otherwise eat an afternoon each.

The next step is yours: pick a problem with a state and a set of moves, write a world model, and let the search find a path.

## Optional Practice Problems

1. **A Different Search:** Subclass **search-algorithm** and implement depth first search with an explicit stack instead of a beam. Run it against the toy `sum-to-ten` world model from **run-self-tests** and compare the action trace it finds with the one beam search returns. Explain the difference in terms of your reward function.

2. **Countdown:** Write a world model for the numbers round of a countdown game. The state is a list of available numbers, the actions are `(a op b)` pairs, and the target is a three digit number. Reuse **beam-search** unchanged. What **max-depth** do you need, and how does the beam size affect how often you hit the target exactly?

3. **Model-Driven Step:** Build a RAP-style world model where **step** calls **generate** to propose the next state. Use a math word problem, prompt for one line of reasoning per step, and make **is-terminal** true when the model emits a final answer. Compare the trace against plain chain of thought on the same question.

4. **Self-Consistency Curve:** Run **CoT-gsm8k-example.lisp** with 1, 3, 5, and 9 samples at temperature `0.7`. Record accuracy at each point and plot it. Does accuracy still improve at 9 samples, and does the cost justify it? Repeat at temperature `0.3` and explain the difference.

5. **A Harder Extractor:** Replace **retrieve-answer** with a version that handles answers in a `\boxed{}` macro, "the final answer is", and a trailing number with no trigger phrase at all. Keep the existing sixteen answer-handling self-tests passing and add checks for each new form. Report how the GSM8K accuracy changes.
