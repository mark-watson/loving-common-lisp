# Overview of Probability

Probability theory provides the mathematical foundation for quantifying uncertainty in stochastic environments. While classical frequentist approaches treat probability as the long-run frequency of repeatable events, Bayesian probability reframes it as a dynamic measure of belief or system state. Through Bayes' Theorem, initial prior assumptions are systematically updated with incoming evidence to compute a posterior distribution, enabling rigorous, continuous inference even when dealing with sparse or evolving data sets.

Standard probabilistic models, however, fundamentally map correlations rather than causality. While observational probability can determine the likelihood of variables co-occurring, causal inference — often formalized through structural causal models or do-calculus — is required to understand directional influence and counterfactuals. This distinction is critical; calculating the likelihood of observing a specific system state requires entirely different mathematical machinery than predicting the outcome of an active intervention upon that system.

In modern tech stacks, particularly within local-first AI orchestration and agentic frameworks, these mathematical principles are the engine of predictive capabilities. Probabilistic inference drives the fundamental token prediction and weight distribution in small language models (SLMs), while Bayesian methods inform uncertainty quantification, active learning, and dynamic state tracking. The current architectural frontier lies in bridging the remaining gap: pushing these deployment stacks from purely probabilistic pattern-matching toward integrated causal reasoning to enable reliable, autonomous decision-making.

The source code for this chapter is in the directory **src/Probability**.


## Words of Warning

Professor Carissa Véliz says in her excellent book "Prophecy" that when you read a percentage you should first ask yourself if you are being told a fact or a prediction. If a percentage is a prediction, consciously tag it as "not a fact."

The danger of conflating the two lies in the illusion of precision that numbers naturally provide. A percentage representing a historical measurement — such as a quantified error rate in a static dataset — is a grounded, verifiable reality. A predictive percentage, however, is fundamentally an artifact of a specific model. It is a mathematical expression of uncertainty, heavily dependent on the chosen priors, the limits of the training data, and the structural assumptions baked into the algorithm. When we fail to recognize this distinction, we grant probabilistic forecasts an unearned epistemological weight, treating calculated inferences as though they were empirical truths.

Consciously tagging a predictive percentage as "not a fact" serves as a vital cognitive circuit breaker. It forces a shift from passive acceptance to active, structural critique. Instead of absorbing the number, this tagging prompts you to interrogate the mechanics behind it: What variables is the model blind to? Is it projecting forward based on mere correlation, or does it account for causal mechanics? How fragile is this prediction to out-of-distribution events? By actively demoting these percentages from facts to hypotheses, we maintain agency over our decision-making and avoid becoming captive to the misplaced certainty of an output.

## Glossary of Terms

Before diving into the library and the worked examples, here is a reference for the statistical vocabulary used throughout this chapter. Many of these terms are used casually in data-science writing but have precise technical meanings that matter when interpreting results.

**Prior (prior probability)** — Your initial belief about how likely a hypothesis is *before* you observe any new evidence. In the medical example below, the prior probability of disease is the prevalence rate (0.1 %). Priors can be informative (based on domain knowledge) or uninformative (deliberately vague, to let the data speak).

**Posterior (posterior probability)** — Your updated belief about a hypothesis *after* incorporating observed evidence via Bayes' Theorem. The posterior is the central output of Bayesian inference: P(Hypothesis | Data). In the medical example, the posterior probability of disease given a positive test is approximately 1.9 % — far lower than intuition suggests.

**Likelihood** — The probability of observing the evidence *assuming a specific hypothesis is true*: P(Evidence | Hypothesis). The likelihood is not itself a probability distribution over hypotheses; it is a function that tells you how well each hypothesis explains the data you actually observed. In the medical example, the likelihood of a positive test result given disease is 0.99 (the sensitivity).

**Marginal likelihood (evidence)** — The total probability of the observed evidence across all hypotheses: Σ P(Evidence | H) · P(H). It acts as the normalising constant in Bayes' Theorem, ensuring the posteriors sum to one.

**Bayes' Theorem** — The mathematical rule connecting prior, likelihood, and posterior: P(H | E) = P(E | H) · P(H) / P(E). It is the engine of Bayesian inference and the foundation of the `update` function in this library.

**Maximum a posteriori (MAP)** — The hypothesis with the highest posterior probability. It is the Bayesian analogue of a "best guess" and is returned by the `maximum-a-posteriori` function.

**Prevalence (base rate)** — The proportion of a population that has a particular condition. A critical input to Bayesian reasoning: when the base rate is very low, even a highly accurate test produces many false positives relative to true positives. Ignoring the base rate is one of the most common reasoning errors in applied statistics.

**Sensitivity (true-positive rate)** — The probability that a test correctly identifies a positive case: P(Test+ | Condition+). A sensitivity of 99 % means the test catches 99 out of every 100 truly positive cases.

**Specificity (true-negative rate)** — The probability that a test correctly identifies a negative case: P(Test− | Condition−). Specificity = 1 − false-positive rate.

**False-positive rate** — The probability that a test incorrectly flags a healthy individual as positive: P(Test+ | Condition−). In our medical example this is 5 %.

**Positive predictive value (PPV)** — Among everyone who tested positive, the fraction who actually have the condition: TP / (TP + FP). The PPV depends heavily on prevalence; for rare conditions, PPV can be very low even when sensitivity and specificity are high.

**Z-score (standard score)** — The number of standard deviations a data point lies from the mean: z = (x − μ) / σ. A z-score of 0 means the value equals the mean; ±1.96 corresponds to the outer 5 % of a standard normal distribution. Used in hypothesis testing to determine how extreme an observation is.

**P-value** — The probability of observing data *at least as extreme* as what was measured, *assuming the null hypothesis is true*. A small p-value (e.g. < 0.05) is conventionally taken as evidence against the null hypothesis. Crucially, the p-value is **not** the probability that the hypothesis is true or false — a subtlety that is routinely misunderstood. A p-value of 0.03 means "if nothing interesting were happening, we would see data this extreme only 3 % of the time."

**Null hypothesis (H₀)** — The default assumption of "no effect" or "no difference" that a frequentist test tries to reject. For example, "the positive-test rate equals the disease prevalence."

**Chi-squared test** — A test that compares observed counts against expected counts to determine whether the discrepancy is larger than chance alone would predict. Pearson's chi-squared statistic is Σ (O − E)² / E, summed over all categories. Used here to test whether test results and disease status are independent.

**Degrees of freedom (df)** — The number of independent values that are free to vary in a statistical calculation. For a chi-squared test on a table with k categories, df = k − 1.

**Confidence interval (CI)** — A frequentist range estimate. A 95 % CI means: if you repeated the experiment many times, 95 % of the computed intervals would contain the true parameter. It does *not* mean there is a 95 % probability the parameter is in this particular interval — that is the Bayesian credible interval.

**Wilson score interval** — A method for computing a confidence interval for a binomial proportion that is more accurate than the simple normal-approximation (Wald) interval, especially for small samples or proportions near 0 or 1. Used in this library's `confidence-interval-proportion` function.

**Credible interval (Bayesian)** — The Bayesian counterpart of a confidence interval. A 95 % credible interval means there is a 95 % probability that the parameter lies within this range, given the observed data and priors. This is what most people intuitively think a confidence interval means.

**Pearson correlation coefficient (r)** — A measure of the linear association between two variables, ranging from −1 (perfect negative) to +1 (perfect positive). A value near 0 means no linear relationship. It measures *association*, not causation.

**Spearman rank correlation (ρ)** — A non-parametric measure of monotonic association. It works by ranking the data and then computing Pearson-r on the ranks. More robust to outliers and non-linear but monotonic relationships than Pearson-r.

**Correlation matrix** — A table showing the pairwise Pearson-r values for every combination of variables in a dataset. Useful for a quick overview of which variables move together.

**Confounding variable (confounder)** — A hidden variable that influences both X and Y, creating a spurious correlation between them. Correlation analysis cannot detect confounders; only causal reasoning or controlled experiments can.

**Stochastic** — Involving randomness or uncertainty. A stochastic process is one whose outcomes are not fully determined by its inputs.

**Causal inference** — The process of determining whether and how one variable directly influences another, as opposed to merely being correlated with it. Requires structural causal models, do-calculus, or controlled experiments — standard probability alone is insufficient.

## A Common Lisp Library to Explore Probability

### Design

The library is a single ASDF system (`probability`) providing four modules that span both major schools of statistical reasoning — Bayesian and Frequentist. (For a detailed comparison of the two paradigms, see [Frequentists vs. Bayesians](#frequentists-vs-bayesians) below.)

1. **Bayesian Inference (`bayes.lisp`)**
   - `make-bayes-model (prior-alist)` — create a model from an alist of `(hypothesis . prior-probability)` pairs. Priors are normalised automatically.
   - `update (model evidence likelihood-fn)` — apply Bayes' Theorem. `likelihood-fn` is a function `(hypothesis) → P(evidence | hypothesis)`. Returns a new model with posterior probabilities.
   - `posterior (model hypothesis)` — look up the posterior for a single hypothesis.
   - `posteriors (model)` — return the full posterior alist.
   - `maximum-a-posteriori (model)` — return the hypothesis with the highest posterior.

2. **Correlation helpers (`correlation.lisp`)**
   - `pearson-r (xs ys)` — Pearson correlation coefficient for two equal-length lists of numbers.
   - `spearman-rho (xs ys)` — Spearman rank-order correlation.
   - `correlation-matrix (data-alist)` — given an alist of `(name . values)` pairs, return a matrix of pairwise Pearson-r values.
   - These functions explicitly measure *association*, not causation. Docstrings include warnings about confounding variables.

3. **Frequentist Statistics (`frequentist.lisp`)**
   - `z-score`, `z-test-proportion`, `chi-squared-test`, `confidence-interval-proportion` — the classical hypothesis-testing toolkit. Covered in detail in [Experimenting with Frequentist Methods](#experimenting-with-frequentist-methods) below.

4. **Worked examples (`examples/`)**
   - `medical.lisp` — Bayesian screening-test scenario: prior disease prevalence = 0.1 %, test sensitivity = 99 %, false-positive rate = 5 %. Walks through posterior computation after a positive result and shows the posterior probability of disease is only ~1.9 % — a famous counter-intuitive result. Also computes Pearson-r between test results and actual diagnoses to illustrate that correlation can be strong yet misleading about individual risk.
   - `frequentist-demo.lisp` — revisits the same scenario from the frequentist standpoint (chi-squared test, Wilson CI for PPV), then prints both results side by side to highlight that the two frameworks converge on the same practical conclusion.

### File layout

~~~~~~~~
Probability/
├── probability.asd
├── package.lisp
├── bayes.lisp                   ;; Bayesian toolkit
├── correlation.lisp             ;; Correlation toolkit
├── frequentist.lisp             ;; Frequentist toolkit
├── examples/
│   ├── medical.lisp             ;; Bayesian worked example
│   └── frequentist-demo.lisp    ;; Frequentist worked example
└── README.md
~~~~~~~~

### Running the example

~~~~~~~~
# from the Probability/ directory
sbcl --load probability.asd \
     --eval '(asdf:load-system :probability)' \
     --eval '(probability:run-medical-example)' \
     --quit
~~~~~~~~

## Walking Through the Bayesian Code

### The Bayes Model

The core data structure is simply a normalised association list of `(hypothesis . probability)` pairs. The constructor ensures priors sum to one:

{lang="lisp",linenos=off}
~~~~~~~~
(defun make-bayes-model (prior-alist)
  "Create a Bayes model from PRIOR-ALIST, an alist of (HYPOTHESIS . PRIOR).
Priors are automatically normalised so they sum to 1."
  (let ((total (reduce #'+ prior-alist :key #'cdr)))
    (when (zerop total)
      (error "All priors are zero — cannot normalise."))
    (mapcar (lambda (pair)
              (cons (car pair) (/ (cdr pair) total)))
            prior-alist)))
~~~~~~~~

### Updating with Evidence

The `update` function applies Bayes' Theorem. For each hypothesis it multiplies the prior by the likelihood of the observed evidence, then normalises:

{lang="lisp",linenos=off}
~~~~~~~~
(defun update (model evidence likelihood-fn)
  "Return a new model with posteriors computed via Bayes' Theorem.
LIKELIHOOD-FN is a function of two arguments (HYPOTHESIS EVIDENCE)
that returns P(evidence | hypothesis)."
  (declare (ignore evidence))
  (let* ((unnormalised
           (mapcar (lambda (pair)
                     (cons (car pair)
                           (* (funcall likelihood-fn (car pair))
                              (cdr pair))))
                   model))
         (marginal (reduce #'+ unnormalised :key #'cdr)))
    (when (zerop marginal)
      (error "Marginal likelihood is zero — evidence is impossible under all hypotheses."))
    (mapcar (lambda (pair)
              (cons (car pair) (/ (cdr pair) marginal)))
            unnormalised)))
~~~~~~~~

The `marginal` variable is the denominator in Bayes' Theorem — the total probability of the evidence across all hypotheses. Dividing by it gives us proper posterior probabilities that sum to one.

### The Medical Screening Example

The worked example in `examples/medical.lisp` makes Bayes' Theorem concrete. A rare disease affects 0.1 % of the population. A screening test has 99 % sensitivity and a 5 % false-positive rate. A patient tests positive — what is the probability they are actually sick?

{lang="lisp",linenos=off}
~~~~~~~~
(defun medical-likelihood (hypothesis)
  "Return P(positive-test | HYPOTHESIS)."
  (ecase hypothesis
    (:disease *sensitivity*)
    (:healthy *false-positive-rate*)))

(defun run-bayesian-analysis ()
  "Compute posterior probability of disease given a positive test."
  (let* ((prior (make-bayes-model `((:disease . ,*prevalence*)
                                    (:healthy . ,(- 1.0d0 *prevalence*)))))
         (updated (update prior :positive-test #'medical-likelihood)))
    (format t "~%=== Bayesian Analysis: Medical Screening Test ===~%")
    (format t "Prior probabilities:~%")
    (dolist (p (posteriors prior))
      (format t "  P(~A) = ~,4F~%" (car p) (cdr p)))
    (format t "~%After a POSITIVE test result:~%")
    (dolist (p (posteriors updated))
      (format t "  P(~A | positive) = ~,4F  (~,2F %)~%"
              (car p) (cdr p) (* 100.0d0 (cdr p))))
    updated))
~~~~~~~~

The answer is approximately **1.9 %**. Despite 99 % sensitivity, the disease is so rare that the vast majority of positive results come from the 5 % false-positive rate applied to the enormous healthy population. This is exactly the kind of counter-intuitive result that Bayes' Theorem reveals — and that ignoring the base rate obscures.

### Correlation Analysis

The example also generates a synthetic population of 100,000 individuals and computes the Pearson correlation between test results and actual disease status:

{lang="lisp",linenos=off}
~~~~~~~~
(defun run-correlation-analysis ()
  "Generate a synthetic population and compute Pearson-r between test
results and actual disease status."
  (let* ((pop (generate-synthetic-population 100000))
         (tests     (car pop))
         (diagnoses (cdr pop))
         (r (pearson-r tests diagnoses)))
    (format t "~%=== Correlation Analysis (N = ~D) ===~%" (length tests))
    (format t "Pearson r(test-result, disease) = ~,4F~%" r)
    r))
~~~~~~~~

The correlation is positive but modest. This illustrates a crucial point: a statistically real association does not translate into reliable individual prediction. You need Bayesian reasoning with the base rate for that.

## Frequentists vs. Bayesians

The deepest fault-line in probability runs between two camps that disagree on what a probability *is*.

### What probability means

| | Frequentist | Bayesian |
|---|---|---|
| **Definition** | Long-run frequency of an event over (hypothetically) infinite repeated trials. | Degree of belief or confidence in a proposition, updated as evidence arrives. |
| **Parameters** | Fixed but unknown constants. | Random variables described by probability distributions. |
| **Data** | A random sample from an infinite population. | Fixed once observed. |
| **Core question** | "How likely is this data, assuming the hypothesis is true?" — P(Data \| H) | "How likely is the hypothesis, given the data I observed?" — P(H \| Data) |

### Key tools and how they differ

**Frequentist toolkit:** p-values, confidence intervals, maximum-likelihood estimation.  A p-value answers a narrow question: *if the null hypothesis were true, how extreme would this data be?*  It does **not** tell you the probability that the hypothesis is correct — a subtlety that is routinely misunderstood in published research.

**Bayesian toolkit:** prior distributions, likelihood functions, posterior distributions (via Bayes' Theorem), credible intervals.  A 95 % Bayesian credible interval means exactly what most people *think* a confidence interval means: there is a 95 % probability that the parameter lies in this range, given the data and priors.

### Strengths and weaknesses

**Frequentist strengths:**
- No subjective prior required — results depend only on the data at hand.
- Standardised, widely accepted in regulatory contexts (e.g., clinical trials, FDA submissions).
- Computationally cheap for large datasets.

**Frequentist weaknesses:**
- p-values are chronically misinterpreted; "statistically significant" ≠ "practically important."
- Cannot directly state the probability that a hypothesis is true.
- Struggles with small-sample or rare-event problems where prior information could help.

**Bayesian strengths:**
- Directly answers the question practitioners usually care about: "How probable is my hypothesis?"
- Naturally incorporates prior knowledge — invaluable with small samples or sequential data.
- Produces a full posterior distribution, giving richer uncertainty quantification than a single point estimate.

**Bayesian weaknesses:**
- Choice of prior is subjective; a bad prior can bias results, especially with little data.
- Posterior computation can be expensive (MCMC, variational inference) for complex models.
- Less standardised — harder to compare across studies when priors differ.

### The modern pragmatic view

In practice, the two frameworks often converge: with large datasets and uninformative (flat) priors, Bayesian posteriors and frequentist confidence intervals yield nearly identical results.  Most working statisticians and machine-learning engineers are pragmatists — they reach for whichever tool fits the problem.  Frequentist methods dominate formal hypothesis testing and regulatory work; Bayesian methods dominate sequential decision-making, reinforcement learning, and any setting where prior information is too valuable to ignore.

The library in this repository leans Bayesian because Bayes' Theorem is the clearest lens for the medical-screening example: it forces you to confront the base rate, which pure frequentist significance testing can obscure.

## Experimenting with Frequentist Methods

To give the frequentist perspective equal hands-on treatment, the library includes a `frequentist.lisp` module and a companion worked example.

### Frequentist module API (`frequentist.lisp`)

- `z-score (observed expected std-dev)` — compute the standard z-score: (observed − expected) / σ.
- `z-test-proportion (successes n hypothesised-p)` — one-sample z-test for a proportion. Returns `(z-score p-value)` as multiple values. Tests whether the observed proportion differs significantly from `hypothesised-p`.
- `chi-squared-test (observed expected)` — Pearson's chi-squared goodness-of-fit test. `observed` and `expected` are equal-length lists of counts. Returns `(chi-squared df p-value)` as multiple values.
- `confidence-interval-proportion (successes n &key (confidence 0.95))` — Wilson score interval for a binomial proportion. Returns `(lower upper)` as multiple values.

The p-value computation uses a rational approximation to the standard normal CDF (Abramowitz & Stegun 26.2.17), accurate to ~1.5 × 10⁻⁷ — more than sufficient for exploratory work.

### Walking Through the Frequentist Code

The normal CDF approximation is the most mathematically dense piece of the library. It uses a polynomial fit from the classic *Handbook of Mathematical Functions*:

{lang="lisp",linenos=off}
~~~~~~~~
(defun phi-approx (z)
  "Approximate the standard normal CDF Φ(z) using the
Abramowitz & Stegun 26.2.17 rational approximation."
  (let* ((p  0.2316419d0)
         (b1 0.319381530d0)
         (b2 -0.356563782d0)
         (b3 1.781477937d0)
         (b4 -1.821255978d0)
         (b5 1.330274429d0)
         (az (abs (float z 1.0d0)))
         (t-val (/ 1.0d0 (+ 1.0d0 (* p az))))
         (pdf (/ (exp (* -0.5d0 az az))
                 (sqrt (* 2.0d0 pi))))
         (cdf (- 1.0d0
                 (* pdf
                    (+ (* b1 t-val)
                       (* b2 (expt t-val 2))
                       (* b3 (expt t-val 3))
                       (* b4 (expt t-val 4))
                       (* b5 (expt t-val 5)))))))
    (if (>= z 0.0d0) cdf (- 1.0d0 cdf))))
~~~~~~~~

The one-sample z-test for a proportion builds on this to answer "is the observed success rate significantly different from a hypothesised value?":

{lang="lisp",linenos=off}
~~~~~~~~
(defun z-test-proportion (successes n hypothesised-p)
  "One-sample z-test for a binomial proportion.
Tests H₀: p = HYPOTHESISED-P against H₁: p ≠ HYPOTHESISED-P (two-tailed).
Returns two values: Z-SCORE and P-VALUE."
  (let* ((p0    (float hypothesised-p 1.0d0))
         (n     (float n 1.0d0))
         (p-hat (/ (float successes 1.0d0) n))
         (se    (sqrt (/ (* p0 (- 1.0d0 p0)) n)))
         (z     (/ (- p-hat p0) se))
         (p-val (* 2.0d0 (- 1.0d0 (phi-approx (abs z))))))
    (values z (min p-val 1.0d0))))
~~~~~~~~

The Wilson score confidence interval is more accurate than the simple Wald interval for extreme proportions:

{lang="lisp",linenos=off}
~~~~~~~~
(defun confidence-interval-proportion (successes n &key (confidence 0.95d0))
  "Wilson score confidence interval for a binomial proportion.
Returns two values: LOWER and UPPER bounds."
  (let* ((n    (float n 1.0d0))
         (p    (/ (float successes 1.0d0) n))
         (z    (z-critical confidence))
         (z2   (* z z))
         (denom (+ 1.0d0 (/ z2 n)))
         (centre (/ (+ p (/ z2 (* 2.0d0 n))) denom))
         (margin (/ (* z (sqrt (+ (/ (* p (- 1.0d0 p)) n)
                                   (/ z2 (* 4.0d0 n n)))))
                    denom)))
    (values (max 0.0d0 (- centre margin))
            (min 1.0d0 (+ centre margin)))))
~~~~~~~~

### Worked example — Frequentist Medical Screening (`examples/frequentist-demo.lisp`)

Revisits the same medical-screening scenario from a purely frequentist standpoint:

1. **Simulates a clinical trial** — 100,000 individuals screened; counts true positives, false positives, true negatives, false negatives.
2. **Chi-squared test of independence** — tests whether the test result and disease status are statistically independent. With N = 100,000 and 5 % false-positive rate, the test overwhelmingly rejects independence (p ≈ 0), but this tells you nothing about the *magnitude* of risk for an individual patient.
3. **Confidence interval for positive predictive value (PPV)** — among everyone who tested positive, what fraction actually has the disease? The 95 % Wilson interval is computed, showing the PPV is dismally low (~1–3 %) despite "statistical significance."
4. **Side-by-side comparison** — prints the Bayesian posterior (from `run-medical-example`) alongside the frequentist CI, demonstrating that the two frameworks reach the same conclusion via different reasoning.

The example drives home the point from the "Words of Warning" section: a statistically significant association (tiny p-value) does not imply a practically useful prediction for any individual.

### Running the frequentist example

~~~~~~~~
# from the Probability/ directory
sbcl --load probability.asd \
     --eval '(asdf:load-system :probability)' \
     --eval '(probability:run-frequentist-demo)' \
     --quit
~~~~~~~~

### Key output from the frequentist demo

- Chi-squared test rejects independence (p < 10⁻¹⁵) — the association is "highly significant."
- But PPV is only **1.80 %** (95 % Wilson CI: 1.46 %–2.20 %).
- The Bayesian posterior agrees: **1.94 %**.
- **Lesson:** statistical significance (small p-value) ≠ practical significance (high PPV).

## Wrap Up

This chapter explored probability from both the Bayesian and frequentist perspectives, using a medical screening scenario to illustrate a result that surprises almost everyone: a highly accurate test applied to a rare condition produces a dismally low positive predictive value. The Bayesian framework makes this transparent by forcing you to account for the base rate; the frequentist framework confirms it through confidence intervals on the PPV, even as its chi-squared test screams "significant!"

The key takeaways are:

- **Always consider the base rate.** A 99 %-accurate test means little when the condition is rare.
- **Statistical significance ≠ practical significance.** A tiny p-value tells you an association exists; it does not tell you the association is large or useful.
- **Correlation ≠ causation, and even correlation ≠ reliable individual prediction.** The Pearson-r between test results and disease is real but insufficient for clinical decision-making.
- **Both frameworks have their place.** Bayesian methods shine when prior information matters; frequentist methods dominate regulatory and large-sample settings. Pragmatic practitioners use both.
