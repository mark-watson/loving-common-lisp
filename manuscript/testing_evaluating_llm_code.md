# Testing and Evaluating LLM-Based Code

Code that calls an LLM needs two kinds of tests.

The first kind is the usual unit test. Most of an LLM application is plain code: text chunking, prompt templates, reply parsing, retrieval math, and storage. This code is deterministic, so a unit test can check that a given input gives an exact output. Unit tests should run in seconds with no network, so we replace each model call with a fake.

The second kind is the *evaluation*, or *eval*. Here we run the real system with real models over a fixed set of questions and score the answers. The output changes from run to run, a correct answer can be worded a hundred ways, and no single score tells the whole story. An eval answers a question that unit tests can't: did my last change make the answers better or worse?

In this chapter we build `llm-eval`, a small library for both kinds of tests, and use it on the agentic RAG system from the earlier RAG chapter. We cover:

- Unit tests with the FiveAM framework, including a test that runs the whole RAG pipeline with fake models.
- A golden dataset of questions and reference answers, stored as Lisp data.
- Three scores that need no model: exact match, token F1, and keyword recall.
- LLM-as-judge, where a second model grades each answer.
- A run history in SQLite, so we can find regressions between runs.

Everything runs on local models through Ollama. You need three models:

```bash
ollama pull nomic-embed-text   # embeddings for RAG retrieval
ollama pull llama3.2:3b        # the RAG system's chat model
ollama pull qwen3.5:4b         # the judge
```

**Note: The source code is in the directory `src/llm-eval`.**

## Project Structure

| File | Purpose |
|------|---------|
| `package.lisp` | Package definition and exports |
| `dataset.lisp` | Loading golden datasets |
| `scoring.lisp` | Exact match, token F1, and keyword recall |
| `judge.lisp` | LLM-as-judge |
| `harness.lisp` | Runs a system over a dataset and scores it |
| `store.lisp` | Run history in SQLite |
| `rag-eval.lisp` | Evaluates the RAG example |
| `tests.lisp` | FiveAM unit tests |
| `examples.lisp` | Runs three evals and compares them |
| `data/rag-golden.lisp` | Golden dataset for the RAG example |
| `llm-eval.asd` | ASDF system definitions |

```lisp
;;;; llm-eval.asd -- testing and evaluating LLM-based code.

(asdf:defsystem #:llm-eval
  :description "Evaluation harness for LLM applications: golden datasets, scoring, LLM-as-judge, and run history in SQLite."
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:litelm #:sqlite)
  :serial t
  :components ((:file "package")
               (:file "dataset")
               (:file "scoring")
               (:file "judge")
               (:file "harness")
               (:file "store"))
  :in-order-to ((asdf:test-op (asdf:test-op #:llm-eval/tests))))

(asdf:defsystem #:llm-eval/rag
  :description "Evaluate the agentic RAG example from the RAG chapter."
  :depends-on (#:llm-eval #:rag)
  :components ((:file "rag-eval")))

(asdf:defsystem #:llm-eval/tests
  :description "FiveAM unit tests. They run offline: every model call is faked."
  :depends-on (#:llm-eval #:rag #:fiveam)
  :components ((:file "tests"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :all-tests :llm-eval-tests))))
```

We split the code into three systems. The core system `llm-eval` knows nothing about RAG, so you can use it to evaluate any LLM application. The `:in-order-to` and `:perform` options connect the tests to ASDF, so `(asdf:test-system :llm-eval)` runs them. The `:perform` method uses `uiop:symbol-call` and `uiop:find-symbol*` because the test package does not exist when ASDF reads the `.asd` file.

## The Golden Dataset

A golden dataset is a list of questions, each with a correct answer written by a person. I wrote twelve cases from the three text files in `src/RAG/data`. Each case is a plist in a plain Lisp file:

```lisp
(:id "co2-preindustrial"
 :question "What was the pre-industrial concentration of atmospheric CO2?"
 :reference "About 280 parts per million (ppm)."
 :keywords ("280"))

(:id "solid-state-companies"
 :question "Which companies are leading the development of solid-state batteries?"
 :reference "Toyota, Samsung SDI, and QuantumScape."
 :keywords ("toyota" "samsung" "quantumscape"))

(:id "storage-roles"
 :question "What is the most common form of grid-scale energy storage, and what role does it play next to batteries in a hybrid storage system?"
 :reference "Pumped-storage hydroelectricity. It provides bulk, long-duration storage over hours or days, while batteries handle short, fast, frequent charge cycles."
 :keywords ("pumped"))
```

The `:id` stays the same across runs, so we can match a case in one run with the same case in another. `:keywords` lists the facts a correct answer must contain.

Lisp data files are easy to write and need no parser, but the Lisp reader can run code: `#.(form)` evaluates `form` at read time. `load-cases` turns that off:

```lisp
(defstruct eval-case
  id          ; short stable name, used to match results across runs
  question
  reference   ; a correct answer, written by a person
  keywords)   ; facts a correct answer must mention

(defun load-cases (pathname)
  "Read eval cases from PATHNAME, a file of plists. *READ-EVAL* is NIL so
a #. form in a data file can't run code."
  (with-open-file (in pathname)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (loop for form = (read in nil in)
              until (eq form in)
              collect (destructuring-bind (&key id question reference keywords)
                          form
                        (unless (and (stringp id) (stringp question) (stringp reference))
                          (error "Bad eval case, needs string :id, :question, and :reference: ~S"
                                 form))
                        (make-eval-case :id id :question question
                                        :reference reference :keywords keywords)))))))
```

`with-standard-io-syntax` resets the reader to its standard settings, so a change to `*read-base*` or `*package*` elsewhere in your image can't change how the file reads. Passing the stream itself as the end-of-file value is a common idiom: no form in the file can be `eq` to the stream, so `until (eq form in)` can't stop early.

Twelve cases is enough to show the method. For a real application, aim for at least fifty, and add a case each time a user reports a bad answer.

## Scores That Need No Model

`scoring.lisp` has three scores. Each returns a number from 0 to 1. All three start from the same tokenizer:

```lisp
(defun tokens (text)
  "Lowercase word and number tokens from TEXT. Commas inside numbers are
dropped and periods kept, so \"$1,100\" gives \"1100\" and \"1.1°C\" gives
\"1.1\" and \"c\"."
  (let ((result '())
        (current (make-string-output-stream)))
    (flet ((finish-token ()
             (let ((token (string-trim "." (get-output-stream-string current))))
               (when (plusp (length token))
                 (push token result)))))
      (loop for previous = nil then char
            for char across (string-downcase text)
            for index from 0
            do (cond ((alphanumericp char) (write-char char current))
                     ((char= char #\.) (write-char char current))
                     ((and (char= char #\,)
                           previous (digit-char-p previous)
                           (< (1+ index) (length text))
                           (digit-char-p (char text (1+ index))))
                      nil)
                     (t (finish-token))))
      (finish-token))
    (nreverse result)))
```

Numbers matter in this dataset, so the tokenizer keeps decimal points and drops thousands separators. `string-trim` removes the period at the end of a sentence, so `"1.1."` becomes `"1.1"`. In the `loop`, `previous` holds the character before `char`: the clauses step in order, so `previous` takes the old value of `char` before `char` moves on.

### Exact Match and Token F1

```lisp
(defun exact-match (answer reference)
  "1 if ANSWER and REFERENCE have the same tokens, else 0."
  (if (equal (tokens answer) (tokens reference)) 1 0))

(defun token-f1 (answer reference)
  "F1 score of the token overlap between ANSWER and REFERENCE, the measure
used by the SQuAD question answering benchmark. Each token counts as many
times as it appears in both texts."
  (let ((answer-tokens (tokens answer))
        (reference-tokens (tokens reference)))
    (if (or (null answer-tokens) (null reference-tokens))
        0
        (let ((counts (make-hash-table :test #'equal))
              (common 0))
          (dolist (token reference-tokens)
            (incf (gethash token counts 0)))
          (dolist (token answer-tokens)
            (when (plusp (gethash token counts 0))
              (decf (gethash token counts))
              (incf common)))
          (if (zerop common)
              0
              (let ((precision (/ common (length answer-tokens)))
                    (recall (/ common (length reference-tokens))))
                (/ (* 2 precision recall) (+ precision recall))))))))
```

Precision is the share of answer tokens found in the reference. Recall is the share of reference tokens found in the answer. F1 is their harmonic mean. Common Lisp's rational numbers keep the result exact, so `(token-f1 "about 280 ppm" "About 280 parts per million (ppm).")` returns `2/3`, not `0.6666667`, which makes unit tests simple.

### Keyword Recall

```lisp
(defun keyword-recall (answer keywords)
  "Fraction of KEYWORDS found in ANSWER as whole tokens. With no keywords
the score is 1."
  (if (null keywords)
      1
      (let ((text (format nil " ~{~A~^ ~} " (tokens answer))))
        (/ (count-if (lambda (keyword)
                       (search (format nil " ~{~A~^ ~} " (tokens keyword)) text))
                     keywords)
           (length keywords)))))
```

Both strings are joined with spaces and padded with a space at each end, so `"280"` matches the token `280` but not `1280`.

## LLM-as-Judge

A judge is a second model that reads the question, the reference answer, and the system's answer, and decides whether the answer is correct. It handles what the other scores can't: different wording, extra detail, and answers that contain the right number but say something false about it.

```lisp
(defvar *judge-model* "ollama/qwen3.5:4b")

(defvar *judge-extra-body* '(("reasoning_effort" . "none"))
  "Extra request fields for the judge. qwen3.5 thinks before it answers,
which takes about 50 seconds per call on my Mac; turning thinking off
brings that under one second.")

(defun judge-prompt (question reference answer)
  (format nil "You are grading an answer from a question answering system.

Question: ~A

Reference answer: ~A

Candidate answer: ~A

Does the candidate answer agree with the reference answer on the facts the
question asks for? Extra correct detail is fine. Wrong numbers, wrong names,
or a missing answer are not.

Reply with exactly two lines:
SCORE: 1 if the candidate agrees with the reference, or SCORE: 0 if not
REASON: one sentence"
          question reference answer))
```

A pass/fail score works better than a 1 to 10 scale with small judge models. A small model has no steady sense of the gap between a 6 and a 7, but it can tell whether two numbers match. The `REASON` line costs little and helps a great deal when you read the failures.

### Choosing the Judge Model

I tried three local models on one test: the reference says 280 ppm and the candidate says "roughly 300 parts per million". A good judge must fail this answer.

| Model | Verdict | Time |
|-------|---------|------|
| `llama3.2:3b` | SCORE: 1, "300 ppm is approximately equal to 280 ppm" | 0.5 s |
| `qwen3.5:4b` | SCORE: 0, "a significant numerical discrepancy" | 52 s |
| `qwen3.5:4b`, thinking off | SCORE: 0, "a factual disagreement" | 0.8 s |

`llama3.2:3b` is too lenient to judge. That is also the model our RAG system uses, and a model grading its own output is a poor check in any case. `qwen3.5:4b` gave the right verdict, but it is a reasoning model that writes a long hidden chain of thought before it answers. Ollama's OpenAI-style endpoint accepts `"reasoning_effort": "none"` to turn that off, and the verdict stayed correct.

The `litelm` library had no way to send a field like that, so I added an `:extra-body` keyword to `litelm:completion`: an alist that `completion` appends to the JSON request. The litelm chapter describes it.

Test your judge before you trust it. A few known-bad answers like the one above make a good unit test for a judge, run by hand whenever you change the judge model or prompt.

### Parsing the Verdict

```lisp
(defun parse-judge-reply (reply)
  "Return the score (0 or 1) and the reason from a judge REPLY. The score
is NIL when the reply has no SCORE line with a 0 or 1."
  (let* ((score-start (search "SCORE:" reply :test #'char-equal))
         (digit (and score-start
                     (find-if-not (lambda (char) (member char '(#\Space #\Tab)))
                                  reply :start (+ score-start 6))))
         (reason-start (search "REASON:" reply :test #'char-equal))
         (reason (if reason-start
                     (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  (subseq reply (+ reason-start 7)))
                     "")))
    (values (case digit (#\1 1) (#\0 0) (t nil))
            reason)))

(defun litelm-judge (prompt)
  (litelm:response-content
   (litelm:completion *judge-model* :messages prompt
                                    :temperature 0
                                    :extra-body *judge-extra-body*)))

(defvar *judge-function* 'litelm-judge
  "Function of one argument, the prompt, that returns the judge's reply.
Tests bind this to a fake judge.")

(defun judge (question reference answer)
  "Ask the judge model to grade ANSWER. Returns the score (0, 1, or NIL
if the reply could not be parsed) and the judge's reason."
  (let ((reply (funcall *judge-function* (judge-prompt question reference answer))))
    (multiple-value-bind (score reason) (parse-judge-reply (or reply ""))
      (values score (if score reason (format nil "Unparsable judge reply: ~S" reply))))))
```

An unparsable reply gets the score `nil`, not 0. Counting a judge failure as a wrong answer would mix two separate problems. `summarize` skips `nil` scores when it computes the mean, and the reason column shows the raw reply so you can fix the prompt.

The judge runs at temperature 0 so that it gives the same verdict for the same input as often as the model allows.

## The Harness

`run-eval` takes the system under test as a function of one question string that returns an answer string. This keeps the harness separate from any one application:

```lisp
(defstruct eval-result
  case-id
  answer
  exact
  f1
  keywords
  judge          ; 0, 1, or NIL when not judged or the judge failed
  judge-reason
  seconds
  error)         ; error message string when the system failed

(defun elapsed-seconds (start)
  (float (/ (- (get-internal-real-time) start) internal-time-units-per-second)))

(defun run-eval (system cases &key (use-judge t) (verbose t))
  "Call SYSTEM, a function of one question string that returns an answer
string, on each of CASES and score each answer. An error in one case is
recorded in its result, and the run goes on to the next case."
  (loop for eval-case in cases
        for index from 1
        collect
        (let* ((start (get-internal-real-time))
               (error-message nil)
               (answer (handler-case (funcall system (eval-case-question eval-case))
                         (error (condition)
                           (setf error-message (princ-to-string condition))
                           "")))
               (seconds (elapsed-seconds start))
               (reference (eval-case-reference eval-case)))
          (multiple-value-bind (judge-score judge-reason)
              (cond (error-message (values 0 "System error"))
                    (use-judge (judge (eval-case-question eval-case) reference answer))
                    (t (values nil "")))
            (when verbose
              (format t "~&[~D/~D] ~A: judge ~A, ~,1F s~%"
                      index (length cases) (eval-case-id eval-case)
                      (or judge-score "-") seconds))
            (make-eval-result
             :case-id (eval-case-id eval-case)
             :answer answer
             :exact (exact-match answer reference)
             :f1 (float (token-f1 answer reference))
             :keywords (float (keyword-recall answer (eval-case-keywords eval-case)))
             :judge judge-score
             :judge-reason judge-reason
             :seconds seconds
             :error error-message)))))
```

The `handler-case` around the system call matters. An eval runs many slow model calls, and one crash on case 40 should not throw away 39 finished results. A system error scores 0, since from the user's view a crash is a wrong answer.

`summarize` and `print-report` compute and print the means:

```lisp
(defun mean (numbers)
  (let ((numbers (remove nil numbers)))
    (if numbers
        (float (/ (reduce #'+ numbers) (length numbers)))
        nil)))

(defun summarize (results)
  "Return a plist of mean scores over RESULTS."
  (list :cases (length results)
        :errors (count-if #'eval-result-error results)
        :exact (mean (mapcar #'eval-result-exact results))
        :f1 (mean (mapcar #'eval-result-f1 results))
        :keywords (mean (mapcar #'eval-result-keywords results))
        :judge (mean (mapcar #'eval-result-judge results))
        :seconds (mean (mapcar #'eval-result-seconds results))))
```

## Storing Runs in SQLite

An eval score means little alone. It means a lot next to the score from yesterday's run. `store.lisp` saves every run in SQLite with the `sqlite` library, the same one the Knowledge Graph Navigator chapter uses:

```lisp
(defun open-store (path)
  "Open (and create if needed) the SQLite database at PATH. Use
\":memory:\" for a throwaway database."
  (let ((db (sqlite:connect path)))
    (sqlite:execute-non-query db
      "CREATE TABLE IF NOT EXISTS runs (
         id INTEGER PRIMARY KEY,
         created TEXT NOT NULL,
         label TEXT NOT NULL,
         config TEXT)")
    (sqlite:execute-non-query db
      "CREATE TABLE IF NOT EXISTS results (
         run_id INTEGER NOT NULL REFERENCES runs(id),
         case_id TEXT NOT NULL,
         answer TEXT,
         exact REAL, f1 REAL, keywords REAL, judge REAL,
         judge_reason TEXT,
         seconds REAL,
         error TEXT)")
    db))

(defun timestamp ()
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour minute second)))

(defun save-run (db label config results)
  "Store RESULTS under a new run and return the run id. LABEL names the run
and CONFIG describes the settings, for example the model name."
  (sqlite:with-transaction db
    (sqlite:execute-non-query db
      "INSERT INTO runs (created, label, config) VALUES (?, ?, ?)"
      (timestamp) label (prin1-to-string config))
    (let ((run-id (sqlite:last-insert-rowid db)))
      (dolist (result results)
        (sqlite:execute-non-query db
          "INSERT INTO results (run_id, case_id, answer, exact, f1, keywords,
                                judge, judge_reason, seconds, error)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
          run-id
          (eval-result-case-id result)
          (eval-result-answer result)
          (eval-result-exact result)
          (eval-result-f1 result)
          (eval-result-keywords result)
          (eval-result-judge result)
          (eval-result-judge-reason result)
          (eval-result-seconds result)
          (eval-result-error result)))
      run-id)))
```

`with-transaction` writes the run and its results as one unit: either all rows land or none do. It also makes the inserts much faster, since SQLite then syncs to disk once instead of once per row. A `nil` value binds as SQL `NULL`, which is what we want for a missing judge score.

The run's `config` column holds the settings as a printed plist. Always record what you changed between runs; a score history without the settings is hard to use a month later.

### Comparing Runs

The mean hides the details. Two runs can have the same mean judge score while one case got fixed and another broke. `compare-runs` lines up the cases of two runs by id:

```lisp
(defun compare-runs (db old-run new-run &key (metric "judge") (threshold 0))
  "Compare METRIC, a column name of the results table, between two runs.
Returns two lists of (case-id old-score new-score): regressions, where the
score dropped by more than THRESHOLD, and improvements, where it rose by
more than THRESHOLD."
  (unless (member metric '("exact" "f1" "keywords" "judge") :test #'string=)
    ;; The column name goes into the SQL text, so only allow known names.
    (error "Unknown metric ~S" metric))
  (let ((rows (sqlite:execute-to-list db
                (format nil "SELECT a.case_id, a.~A, b.~A
                             FROM results a JOIN results b ON a.case_id = b.case_id
                             WHERE a.run_id = ? AND b.run_id = ?
                             ORDER BY a.case_id" metric metric)
                old-run new-run)))
    (flet ((change (row) (- (or (third row) 0) (or (second row) 0))))
      (values (remove-if-not (lambda (row) (< (change row) (- threshold))) rows)
              (remove-if-not (lambda (row) (> (change row) threshold)) rows)))))
```

SQL placeholders (`?`) work for values but not for column names, so the metric name goes into the SQL text with `format`. The check against a fixed list of names keeps that safe. The self-join on `case_id` pairs each case in the old run with the same case in the new run.

The `threshold` exists because of what I saw in the real runs below: F1 moves a little on every run, and a list of tiny changes hides the real ones.

## Unit Tests with FiveAM

FiveAM is a widely used Common Lisp test framework. You group tests into suites with `def-suite`, write each test with `test`, and check values with `is`. When an `is` check fails, FiveAM reports the form and the values it saw. Here are the scoring tests:

```lisp
(defpackage #:llm-eval-tests
  (:use #:cl #:llm-eval #:fiveam)
  (:export #:all-tests))

(in-package #:llm-eval-tests)

(def-suite all-tests :description "All llm-eval tests.")
(def-suite scoring :in all-tests)
(def-suite judging :in all-tests)
(def-suite datasets :in all-tests)
(def-suite harness :in all-tests)

;;; Scoring

(in-suite scoring)

(test tokens
  (is (equal '("hello" "world") (tokens "Hello, world!")))
  (is (equal '("1100" "per" "kwh") (tokens "$1,100 per kWh")))
  (is (equal '("1.1" "c") (tokens "1.1°C.")))
  (is (null (tokens "  ...  "))))

(test exact-match
  (is (= 1 (exact-match "About 73 miles." "about 73 miles")))
  (is (= 0 (exact-match "73 miles" "About 73 miles."))))

(test token-f1
  ;; 3 answer tokens, 6 reference tokens, 3 in common:
  ;; precision 1, recall 1/2, F1 2/3.
  (is (= 2/3 (token-f1 "about 280 ppm" "About 280 parts per million (ppm).")))
  (is (= 1 (token-f1 "Over 50%." "over 50")))
  (is (= 0 (token-f1 "no idea" "About 16%.")))
  (is (= 0 (token-f1 "" "About 16%."))))

(test keyword-recall
  (is (= 2/3 (keyword-recall "Toyota and QuantumScape lead." '("toyota" "samsung" "quantumscape"))))
  (is (= 1 (keyword-recall "It costs $250 to $600." '("250" "600"))))
  (is (= 0 (keyword-recall "1280 ppm" '("280"))) "Keywords match whole tokens only")
  (is (= 1 (keyword-recall "anything" '()))))
```

`signals` checks that a form signals a condition. This test writes a data file with a `#.` form and checks that loading it fails in the reader instead of running the code:

```lisp
(test read-eval-is-off
  (uiop:with-temporary-file (:stream out :pathname path :direction :output)
    (write-string "(:id #.(error \"code ran\") :question \"q\" :reference \"r\")" out)
    :close-stream
    (signals reader-error (load-cases path))))
```

If `*read-eval*` were true, the reader would run `(error "code ran")`, which signals a `simple-error`, not a `reader-error`, and the test would fail. In `uiop:with-temporary-file`, the `:close-stream` marker separates the forms that write the file from the forms that use it.

### Faking Model Calls

Every model call in `llm-eval` goes through a special variable: `*judge-function*` for the judge. The RAG code from the RAG chapter does the same with `rag::*embedding-fn*` for embeddings and `rag::*generate-fn*` for chat. A test binds these variables to fakes with `let`, and the binding ends when the `let` ends, even on error.

This style of fake is simple in Lisp. There is no mocking library and no HTTP server to start; the test replaces the function that would make the HTTP call. The cost is a small design rule for your own code: route each model call through a variable or a parameter.

The most useful fake-model test runs the whole RAG pipeline: chunking, embedding, vector search, the query rewriter, the sufficiency check, and the synthesis agent. Only the two model calls are fake:

```lisp
(defun fake-embedding (text)
  "A tiny bag-of-words embedding over a fixed vocabulary."
  (let ((words (tokens text)))
    (map 'simple-vector
         (lambda (word) (if (member word words :test #'string=) 1.0 0.0))
         '("co2" "ppm" "wind" "denmark" "battery"))))

(defun fake-chat (prompt &key model)
  (declare (ignore model))
  (cond ((search "Sufficient Context Agent" prompt)
         (format nil "VERDICT: SUFFICIENT~%REASON: ok~%MISSING: NONE"))
        ((search "Synthesis Agent" prompt)
         (if (search "280 ppm" prompt)
             "About 280 ppm (facts.txt)."
             "I don't know."))
        (t "pre-industrial co2 ppm")))

(test rag-pipeline-offline
  (uiop:with-temporary-file (:stream out :pathname path :direction :output :type "txt")
    (write-string "Pre-industrial CO2 was 280 ppm. Denmark gets half its power from wind." out)
    :close-stream
    (let ((rag:*rag-verbose* nil)
          (rag::*embedding-fn* #'fake-embedding)
          (rag::*embedding-cache* (make-hash-table :test 'equal))
          (rag::*generate-fn* #'fake-chat)
          (*judge-function* #'exact-judge))
      (let ((corpus (rag:make-corpus :name "test" :description "test")))
        (rag:add-document corpus path)
        (let ((results (run-eval (lambda (question) (rag:query corpus question))
                                 (list (make-eval-case :id "co2"
                                                       :question "Pre-industrial CO2?"
                                                       :reference "About 280 ppm (facts.txt)."
                                                       :keywords '("280")))
                                 :verbose nil)))
          (is (= 1 (eval-result-keywords (first results))))
          (is (eql 1 (eval-result-judge (first results)))))))))
```

The fake chat model answers each RAG agent by looking for words from that agent's prompt. The synthesis agent's fake answers correctly only when the retrieved passage that reached its prompt contains "280 ppm", so the test fails if retrieval breaks. The test also binds a fresh `rag::*embedding-cache*`; without that, cached embeddings from an earlier run could hide a bug.

`exact-judge` is a fake judge that passes an answer only if it repeats the reference:

```lisp
(defun exact-judge (prompt)
  "A fake judge that passes an answer only if it repeats the reference."
  (let* ((reference (subseq prompt (+ (search "Reference answer: " prompt) 18)))
         (reference (subseq reference 0 (position #\Newline reference)))
         (candidate (subseq prompt (+ (search "Candidate answer: " prompt) 18)))
         (candidate (subseq candidate 0 (position #\Newline candidate))))
    (format nil "SCORE: ~D~%REASON: fake" (if (string= reference candidate) 1 0))))
```

The store tests use an in-memory SQLite database, `":memory:"`, so they leave no files behind:

```lisp
(test store-finds-regressions
  (let ((*judge-function* #'exact-judge)
        (db (open-store ":memory:")))
    (unwind-protect
         (let* ((old (save-run db "old" '(:version 1)
                               (run-eval (lambda (q) (cond ((string= q "qa") "alpha")
                                                           ((string= q "qb") "beta")
                                                           (t "wrong")))
                                         (fake-cases) :verbose nil)))
                (new (save-run db "new" '(:version 2)
                               (run-eval (lambda (q) (cond ((string= q "qa") "alpha")
                                                           ((string= q "qc") "gamma")
                                                           (t "wrong")))
                                         (fake-cases) :verbose nil))))
           (multiple-value-bind (regressions improvements) (compare-runs db old new)
             (is (equal '(("b" 1.0d0 0.0d0)) regressions))
             (is (equal '(("c" 0.0d0 1.0d0)) improvements)))
           (is (equal '(nil nil)
                      (multiple-value-list (compare-runs db old new :threshold 1)))
               "Changes no larger than the threshold are ignored")
           (is (= 2 (length (run-summaries db)))))
      (sqlite:disconnect db))))
```

SQLite returns `REAL` columns as double floats, hence `1.0d0` in the expected values.

Run the tests with:

```lisp
(asdf:test-system :llm-eval)
```

```
Running test suite ALL-TESTS
 Running test suite SCORING
  Running test TOKENS ....
  Running test EXACT-MATCH ..
  Running test TOKEN-F1 ....
  Running test KEYWORD-RECALL ....
 Running test suite JUDGING
  Running test PARSE-JUDGE-REPLY ....
  Running test JUDGE-PROMPT-HAS-ALL-PARTS ...
  Running test JUDGE-WITH-FAKE-MODEL ....
 Running test suite DATASETS
  Running test GOLDEN-DATASET-LOADS ..
  Running test READ-EVAL-IS-OFF .
 Running test suite HARNESS
  Running test RUN-EVAL-SURVIVES-A-FAILING-CASE ...
  Running test STORE-FINDS-REGRESSIONS ....
  Running test RAG-PIPELINE-OFFLINE ..
 Did 37 checks.
    Pass: 37 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)
```

## Evaluating the RAG System

`rag-eval.lisp` connects the harness to the RAG system. It points RAG's embeddings at the local `nomic-embed-text` model, loads the three sample documents, and wraps `rag:query` as a function of one question:

```lisp
(defparameter *rag-cases-file*
  (asdf:system-relative-pathname :llm-eval "data/rag-golden.lisp"))

(defparameter *store-file*
  (asdf:system-relative-pathname :llm-eval "eval-runs.db"))

(defun build-rag-corpus ()
  (setf rag:*embedding-model* "ollama/nomic-embed-text"
        rag:*rag-verbose* nil)
  (let ((corpus (rag:make-corpus :name "sample-docs"
                                 :description "Energy, EVs, and climate")))
    (dolist (file '("renewable-energy.txt" "electric-vehicles.txt" "climate-science.txt"))
      (rag:add-document corpus (asdf:system-relative-pathname :rag (format nil "data/~A" file))))
    corpus))

(defun make-rag-system (corpus &key (model "ollama/llama3.2:3b") (top-k 3) (max-iterations 3))
  "Return a function of one question that answers it with agentic RAG."
  (lambda (question)
    (rag:query corpus question :model model :top-k top-k :max-iterations max-iterations)))

(defun run-rag-eval (label &key (model "ollama/llama3.2:3b") (top-k 3) (max-iterations 3)
                                (corpus (build-rag-corpus)))
  "Run the RAG golden dataset with the given settings, print a report, and
store the run. Returns the run id."
  (let* ((cases (load-cases *rag-cases-file*))
         (results (run-eval (make-rag-system corpus :model model :top-k top-k
                                                    :max-iterations max-iterations)
                            cases))
         (db (open-store (namestring *store-file*))))
    (unwind-protect
         (progn
           (print-report results)
           (save-run db label
                     (list :model model :top-k top-k :max-iterations max-iterations
                           :judge *judge-model*)
                     results))
      (sqlite:disconnect db))))
```

`examples.lisp` runs three evals and compares them. The first two use the same settings, which shows how much the scores move from noise alone. The third gives the RAG system less to work with: one retrieved chunk per query and one round of retrieval.

```lisp
(let* ((corpus (build-rag-corpus))
       (baseline (run-rag-eval "baseline" :corpus corpus))
       (baseline-again (run-rag-eval "baseline again" :corpus corpus))
       (less-context (run-rag-eval "top-k 1, 1 iteration" :corpus corpus
                                                          :top-k 1 :max-iterations 1))
       (db (open-store (namestring *store-file*))))
  (unwind-protect
       (progn
         (terpri)
         (print-run-summaries db)
         (terpri)
         (print-comparison db baseline baseline-again)
         (terpri)
         (print-comparison db baseline less-context))
    (sqlite:disconnect db)))
```

```lisp
(ql:quickload :llm-eval/rag)
(load "examples.lisp")
```

All three runs took under two minutes on my Mac. Here is the report for the third run:

```
case                         exact f1    keys  judge
co2-preindustrial                0  0.13  1.00 1    
co2-2024                         0  0.30  1.00 0    
warming-so-far                   0  0.23  1.00 1    
greenland                        0  0.06  1.00 1    
dac-cost                         0  0.18  1.00 1    
leaf-range                       0  0.08  1.00 1    
battery-manufacturing-co2        0  0.17  1.00 1    
solid-state-companies            0  0.02  0.00 0    
storage-cost-drop                0  0.53  1.00 1    
denmark-wind                     0  0.06  1.00 1    
hydro-share                      0  0.09  1.00 1    
storage-roles                    0  0.13  1.00 1    
mean                          0.00  0.16  0.92  0.83
12 cases, 0 errors, 1.2 seconds per case
```

And the summary and comparisons:

```
id  label                  created             cases f1    keys  judge
  1 baseline               2026-11-19 17:08:10    12  0.13  1.00  1.00
  2 baseline again         2026-11-19 17:08:55    12  0.12  1.00  1.00
  3 top-k 1, 1 iteration   2026-11-19 17:09:18    12  0.16  0.92  0.83

Run 1 -> run 2, metric judge
Regressions: none
Improvements: none

Run 1 -> run 3, metric judge
Regressions:
  co2-2024: 1.00 -> 0.00
  solid-state-companies: 1.00 -> 0.00
Improvements: none
```

## What the Scores Tell Us

**Exact match is useless for generated answers.** It scored 0 on all 36 answers, including the correct ones. The RAG system writes full sentences and cites its sources, so its answers never match a short reference word for word. Exact match only suits tasks with short, fixed answers, such as classification labels.

**Token F1 does not track correctness.** The baseline's answers were all correct, yet their mean F1 was 0.13. Worse, F1 *rose* to 0.16 in the third run while two answers broke. Shorter answers simply share a larger share of their tokens with a short reference. Here is the F1 comparison between runs 1 and 3, with a threshold of 0.1 to hide the smallest changes:

```lisp
(print-comparison db 1 3 :metric "f1" :threshold 0.1)
```

```
Run 1 -> run 3, metric f1
Regressions:
  co2-preindustrial: 0.26 -> 0.13
Improvements:
  storage-cost-drop: 0.20 -> 0.53
  warming-so-far: 0.05 -> 0.23
```

F1 flagged a case the judge passed and missed both real failures. Without the threshold, the list had twelve entries, most of them noise.

**Keyword recall caught one failure but not the other.** With one chunk per query, retrieval never found the passage that names the solid-state battery companies, and the answer said so. Keyword recall dropped to 0 for that case. The `co2-2024` failure is different. You can see why by reading the answer in SQLite:

```bash
sqlite3 eval-runs.db "SELECT run_id, answer, judge_reason FROM results WHERE case_id = 'co2-2024'"
```

In run 3 the answer says CO2 "has reached over 420 parts per million", then ends with "However, I couldn't find any information on the specific level of atmospheric CO2 as of 2024." The number is there, so keyword recall gave it full marks. The judge failed it: "The candidate answer contains a direct contradiction."

**The judge is the most useful score, and it is not consistent.** The run 1 answer for the same case had almost the same contradiction ("the passage does not provide information on the exact level of atmospheric CO2 as of 2024"), and the judge passed it. The judge is a model too, so it makes mistakes. Read the answers and reasons behind every regression before you act on it. Don't treat a one-case change as proof.

**Two runs with the same settings agreed on the judge score.** Runs 1 and 2 both scored 12 of 12, and their F1 means differed by 0.01. Before you trust a difference between two configurations, run the same configuration twice to see how much the scores move on their own.

**The cheap setting was faster and worse.** The third run took 1.2 seconds per case against about 3 seconds for the baseline, and it lost two answers. That is the kind of trade-off an eval should show you before your users find it.

## Applying the Harness to Other Code

`run-eval` takes any function from a question string to an answer string, so it works with any question answering code in this book. For the document QA chapter it would be:

```lisp
(run-eval (lambda (question) (docs-qa:qa question t))
          (load-cases "my-docs-qa-cases.lisp"))
```

I have not run this one: `docs-qa` uses OpenAI embeddings by default, and you would first need to write a golden dataset for its sample documents.

## Wrap Up

LLM applications need both unit tests and evals:

- Unit tests check the deterministic code. Route each model call through a special variable or parameter, and tests can replace it with a fake and run offline in seconds.
- Evals check the answers. Keep a golden dataset with stable case ids, store every run with its settings, and compare runs case by case rather than by the mean alone.
- Of the scores we tried, keyword recall is a cheap first check and an LLM judge is the best single score. Exact match and token F1 did not track correctness for this RAG system.
- Test the judge before you trust it, keep it a different model from the system under test, and read the answers behind each regression.

A good next step is to grow the golden dataset with questions the documents can't answer. A good RAG system should say it doesn't know, and a case with the reference answer "The documents do not say." tests that.
