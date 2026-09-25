# Overview of Probability

A screening test for a rare disease is 99 % accurate. You test positive. What
is the chance you are actually sick?

Most people say "about 99 %." The correct answer is about **2 %**. That gap is
the reason this chapter exists: probability is not one idea but two traditions
that answer different questions, and the difference matters the moment you put
a number in front of a person.

This is a short tour of both traditions, built around the medical-screening
example and a small Common Lisp library in **src/Probability**. It is
deliberately brief. I have written a full book on the subject, *Common Lisp and
Probability* ([leanpub.com/probability-common-lisp](https://leanpub.com/probability-common-lisp)),
and this chapter is a doorway to it, not a substitute.

## The result that surprises everyone

A disease affects 0.1 % of the population. A test catches 99 % of true cases
(sensitivity) and flags 5 % of healthy people (false-positive rate).

Walk through 100,000 people:

- 100 have the disease; the test catches 99 of them.
- 99,900 are healthy; 5 % of them — about 4,995 people — test positive anyway.
- So roughly 5,094 people test positive, and only 99 of them are sick.

The probability of disease given a positive test — the **positive predictive
value** — is therefore 99 / 5,094 ≈ **1.94 %**. A 99 %-accurate test is wrong
about almost everyone it flags, because the false positives come from a
population a thousand times larger than the true cases. The library computes
the same number through Bayes' Theorem:

```text
P(disease | positive) = 0.0194  (1.94 %)
```

The base rate, not the accuracy, dominates. This is the most useful lesson in
applied probability, and it generalises: the rarer the condition, the more a
positive result means "probably not."

The same example also computes a Pearson correlation between test result and
diagnosis — about **0.14** here. The association is real, but it is no help at
all for a single patient. Association and prediction are different things, and
neither is causation.

## Fact, prediction, or wishful thinking?

Before trusting any percentage, ask what kind of number it is. In *Prophecy*,
Carissa Véliz suggests sorting every percentage into two piles: **facts** and
**predictions**. A measured error rate is a fact. A modelled probability is a
prediction wearing the costume of precision: it depends on the priors, the
training data, and the structural assumptions baked into it. Tag predictions
"not a fact," and instead of absorbing the number you start interrogating it —
what is the model blind to? Is this correlation or causation? How fragile is it
outside the data?

## Bayes in one line

Bayes' Theorem is the arithmetic of changing your mind:

```text
P(H | E) = P(E | H) · P(H) / P(E)
```

| Piece | Name | Question it answers |
|---|---|---|
| P(H) | prior | How plausible was the hypothesis before the evidence? |
| P(E \| H) | likelihood | If the hypothesis were true, how likely is this evidence? |
| P(E) | marginal likelihood | How likely is the evidence under *all* hypotheses? |
| P(H \| E) | posterior | How plausible is the hypothesis now? |

The library represents a model as a normalised alist of
`(hypothesis . probability)` pairs, so `update` is a two-step map: multiply
each prior by its likelihood, then divide by the total.

{lang="lisp",linenos=off}
~~~~~~~~
(defun update (model evidence likelihood-fn)
  "Return a new model with posteriors computed via Bayes' Theorem.
EVIDENCE is an arbitrary datum passed to LIKELIHOOD-FN.
LIKELIHOOD-FN is a function of two arguments (HYPOTHESIS EVIDENCE)
that returns P(evidence | hypothesis)."
  ;; Compute unnormalised posteriors.
  (let* ((unnormalised
           (mapcar (lambda (pair)
                     (cons (car pair)
                           (* (funcall likelihood-fn (car pair) evidence)
                              (cdr pair))))
                   model))
         (marginal (reduce #'+ unnormalised :key #'cdr)))
    (when (zerop marginal)
      (error "Marginal likelihood is zero — evidence is impossible under all hypotheses."))
    ;; Normalise.
    (mapcar (lambda (pair)
              (cons (car pair) (/ (cdr pair) marginal)))
            unnormalised)))
~~~~~~~~

The `marginal` binding is the denominator of Bayes' Theorem; dividing by it
makes the posteriors sum to one.

## The library

`probability` is a single ASDF system with four modules:

| File | Contents |
|---|---|
| `bayes.lisp` | `make-bayes-model`, `update`, `posterior`, `posteriors`, `maximum-a-posteriori` |
| `correlation.lisp` | `pearson-r`, `spearman-rho`, `correlation-matrix` |
| `frequentist.lisp` | `z-score`, `z-test-proportion`, `chi-squared-test`, `chi-squared-independence`, `confidence-interval-proportion` |
| `examples/` | `medical.lisp` (Bayesian), `frequentist-demo.lisp` (frequentist) |

Everything is plain Common Lisp with no dependency beyond ASDF. From the
`Probability/` directory:

~~~~~~~~
sbcl --eval '(require :asdf)' \
     --load probability.asd \
     --eval '(asdf:load-system :probability)' \
     --eval '(probability:run-medical-example)' \
     --quit
~~~~~~~~

Or, with Quicklisp, put the directory on your local-projects path and call
`(ql:quickload :probability)`. The Bayesian half prints the result from the
previous section; the correlation half simulates 100,000 people and reports
`pearson-r ≈ 0.14`. Because the population is simulated, the last digits move
between runs; the conclusion does not.

## Frequentists vs. Bayesians

The deepest fault line in probability is not mathematical but philosophical:
what *is* a probability?

| | Frequentist | Bayesian |
|---|---|---|
| **Definition** | Long-run frequency over repeated trials | Degree of belief, updated as evidence arrives |
| **Parameters** | Fixed but unknown constants | Random variables with distributions |
| **Core question** | P(Data \| H): how likely is this data? | P(H \| Data): how likely is this hypothesis? |
| **Signature tool** | p-value, confidence interval | posterior, credible interval |

A p-value answers only the first question, and it is *not* the probability that
the hypothesis is true — arguably the most common error in applied statistics.
A 95 % confidence interval likewise does not mean "a 95 % chance the true value
is in here"; that is what a Bayesian credible interval means.

In practice the two converge. Given plenty of data and a vague prior, a
Bayesian posterior and a frequentist estimate are nearly the same number. The
choice is mostly about what you need to say: frequentists dominate regulatory
testing, Bayesians dominate sequential decision-making, and pragmatists use
both.

## The frequentist check

`examples/frequentist-demo.lisp` re-runs the screening scenario with
frequentist tools:

1. simulate 100,000 patients and count true/false positives and negatives;
2. test whether test result and disease status are independent (a 2×2
   chi-squared test, `chi-squared-independence`, with df = 1);
3. put a Wilson score confidence interval on the PPV;
4. print the Bayesian and frequentist answers side by side.

A representative run:

| Quantity | Value |
|---|---|
| Simulated trial (N = 100,000) | ≈100 true positives, ≈5,000 false positives |
| Chi-squared (df = 1) | ≈1,800, p < 10⁻¹⁵ |
| PPV (95 % Wilson CI) | ≈1.9 % (≈1.5 %–2.4 %) |
| Bayesian posterior P(disease \| positive) | 1.94 % |

The exact counts depend on the random draw; the conclusions do not.

The chi-squared test is screaming that the association is real, and it is. The
PPV is still about 2 %. **Statistical significance is not practical
significance:** a tiny p-value says an association exists, not that it is large
or useful. That is the opening puzzle reached from the other direction.

Note the two different tests. `chi-squared-test` is a goodness-of-fit test for
a single list of counts; `chi-squared-independence` takes a contingency table
and uses the correct (rows − 1) × (cols − 1) degrees of freedom.

## The vocabulary you need

| Term | Meaning |
|---|---|
| Prior | P(H) — belief before evidence |
| Likelihood | P(E \| H) — how well a hypothesis explains the data |
| Posterior | P(H \| E) — belief after evidence |
| Marginal likelihood | P(E) = Σ P(E \| H)·P(H) — the normaliser |
| MAP | The hypothesis with the highest posterior |
| Sensitivity | P(test+ \| disease) — true-positive rate |
| False-positive rate | P(test+ \| healthy) |
| PPV | TP / (TP + FP) — chance of disease given a positive test |
| p-value | P(data this extreme \| H₀ true) — not P(H₀) |
| Confidence interval | Covers the truth in 95 % of repeated experiments |
| Credible interval | Contains the parameter with 95 % probability, given data and prior |

## Practice problems

1. **Continuous evidence.** `update` takes a discrete evidence token. Write a
   likelihood function that returns a normal PDF for a continuous observation,
   then update the model with a numeric measurement. Start in `bayes.lisp`.
2. **Credible interval.** Given ordered hypotheses, write a function that
   returns the narrowest set of hypotheses holding at least 95 % of the
   posterior mass, and compare it with the Wilson interval in `frequentist.lisp`.
3. **Simpson's paradox.** `pearson-r` can reverse when groups are pooled. Build
   a small causal model with a confounder Z, compute both P(Y | X) and
   P(Y | do(X)), and show when adjusting for Z changes the answer.
4. **Fisher's exact test.** The chi-squared test is asymptotic, so it is shaky
   with rare events and small counts. Implement Fisher's exact test for a 2×2
   table and compare its p-value with `chi-squared-independence` on the
   screening data.
5. **Sequential updating.** Write `sequential-update`, which folds a list of
   evidence items through a model one at a time and prints the entropy of the
   posterior after each step, showing uncertainty collapse as data arrives.

## Wrap up

- **Mind the base rate.** A 99 %-accurate test for a 0.1 % condition is wrong
  about almost every positive it reports.
- **Facts and predictions are different objects.** A number from a model
  deserves a different kind of trust from a measured one.
- **Significance is not usefulness, and correlation is not causation.** The
  frequentist and Bayesian toolkits agree on the medical example; they simply
  narrate it differently.

For the mathematics behind these results — conjugate priors, MCMC, decision
theory, and much more — see *Common Lisp and Probability*.
