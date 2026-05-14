# Overview of Probability

Probability theory provides the mathematical foundation for quantifying uncertainty in stochastic environments. While classical frequentist approaches treat probability as the long-run frequency of repeatable events, Bayesian probability reframes it as a dynamic measure of belief or system state. Through Bayes' Theorem, initial prior assumptions are systematically updated with incoming evidence to compute a posterior distribution, enabling rigorous, continuous inference even when dealing with sparse or evolving data sets.

Standard probabilistic models, however, fundamentally map correlations rather than causality. While observational probability can determine the likelihood of variables co-occurring, causal inference—often formalized through structural causal models or do-calculus—is required to understand directional influence and counterfactuals. This distinction is critical; calculating the likelihood of observing a specific system state requires entirely different mathematical machinery than predicting the outcome of an active intervention upon that system.

In modern tech stacks, particularly within local-first AI orchestration and agentic frameworks, these mathematical principles are the engine of predictive capabilities. Probabilistic inference drives the fundamental token prediction and weight distribution in small language models (SLMs), while Bayesian methods inform uncertainty quantification, active learning, and dynamic state tracking. The current architectural frontier lies in bridging the remaining gap: pushing these deployment stacks from purely probabilistic pattern-matching toward integrated causal reasoning to enable reliable, autonomous decision-making.

## Words of Warning

Professor Carissa Véliz says in her excellent book "Prophecy" that when you read a percentage you should first ask yourself if you are being told a fact or a prediction. If a percentage is a prediction, consciously tag it as "not a fact."

The danger of conflating the two lies in the illusion of precision that numbers naturally provide. A percentage representing a historical measurement—such as a quantified error rate in a static dataset—is a grounded, verifiable reality. A predictive percentage, however, is fundamentally an artifact of a specific model. It is a mathematical expression of uncertainty, heavily dependent on the chosen priors, the limits of the training data, and the structural assumptions baked into the algorithm. When we fail to recognize this distinction, we grant probabilistic forecasts an unearned epistemological weight, treating calculated inferences as though they were empirical truths.

Consciously tagging a predictive percentage as "not a fact" serves as a vital cognitive circuit breaker. It forces a shift from passive acceptance to active, structural critique. Instead of absorbing the number, this tagging prompts you to interrogate the mechanics behind it: What variables is the model blind to? Is it projecting forward based on mere correlation, or does it account for causal mechanics? How fragile is this prediction to out-of-distribution events? By actively demoting these percentages from facts to hypotheses, we maintain agency over our decision-making and avoid becoming captive to the misplaced certainty of an output.

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

```
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
```

### Running the example

```bash
# from the Probability/ directory
sbcl --load probability.asd \
     --eval '(asdf:load-system :probability)' \
     --eval '(probability:run-medical-example)' \
     --quit
```

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

### Worked example — Frequentist Medical Screening (`examples/frequentist-demo.lisp`)

Revisits the same medical-screening scenario from a purely frequentist standpoint:

1. **Simulates a clinical trial** — 100 000 individuals screened; counts true positives, false positives, true negatives, false negatives.
2. **Chi-squared test of independence** — tests whether the test result and disease status are statistically independent. With N = 100 000 and 5 % false-positive rate, the test overwhelmingly rejects independence (p ≈ 0), but this tells you nothing about the *magnitude* of risk for an individual patient.
3. **Confidence interval for positive predictive value (PPV)** — among everyone who tested positive, what fraction actually has the disease? The 95 % Wilson interval is computed, showing the PPV is dismally low (~1–3 %) despite "statistical significance."
4. **Side-by-side comparison** — prints the Bayesian posterior (from `run-medical-example`) alongside the frequentist CI, demonstrating that the two frameworks reach the same conclusion via different reasoning.

The example drives home the point from the "Words of Warning" section: a statistically significant association (tiny p-value) does not imply a practically useful prediction for any individual.


### Running the frequentist example

```bash
# from the Probability/ directory
sbcl --load probability.asd \
     --eval '(asdf:load-system :probability)' \
     --eval '(probability:run-frequentist-demo)' \
     --quit
```

### Key output from the frequentist demo

- Chi-squared test rejects independence (p < 10⁻¹⁵) — the association is "highly significant."
- But PPV is only **1.80 %** (95 % Wilson CI: 1.46 %–2.20 %).
- The Bayesian posterior agrees: **1.94 %**.
- **Lesson:** statistical significance (small p-value) ≠ practical significance (high PPV).
