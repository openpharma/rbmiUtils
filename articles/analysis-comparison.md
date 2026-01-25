# Comparing Analysis Methods: ANCOVA vs G-computation

## Introduction

This vignette provides guidance on choosing between ANCOVA and
G-computation analysis methods in rbmiUtils, explaining when to use each
approach and their respective advantages.

## Overview of Methods

### ANCOVA (Analysis of Covariance)

ANCOVA is a linear regression-based method that estimates treatment
effects while adjusting for baseline covariates. It is the default
analysis method in rbmiUtils.

**Key characteristics:**

- Assumes a linear relationship between outcome and covariates
- Estimates conditional treatment effects (effect at mean covariate
  values)
- Well-established in regulatory settings for continuous outcomes
- Provides least squares means (LSM) for each treatment group

**When to use ANCOVA:**

- Continuous endpoints (e.g., change from baseline in a biomarker)
- Linear relationship between covariates and outcome is reasonable
- Interest in conditional treatment effects
- Standard regulatory submissions

### G-computation

G-computation (also known as standardization or marginal
standardization) is a method that estimates marginal
(population-averaged) treatment effects. In rbmiUtils, it is implemented
via logistic regression for binary outcomes.

**Key characteristics:**

- Estimates marginal treatment effects (averaged over covariate
  distribution)
- Appropriate for binary outcomes (responder analyses)
- Uses logistic regression with robust variance estimation
- Results are on the probability (risk) scale

**When to use G-computation:**

- Binary endpoints (e.g., responder rates, clinical response)
- Interest in marginal (population-level) treatment effects
- When the target estimand is a risk difference

## Quick Comparison Table

| Feature          | ANCOVA                                                                               | G-computation                                                                                          |
|------------------|--------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------|
| Outcome type     | Continuous                                                                           | Binary                                                                                                 |
| Effect type      | Conditional                                                                          | Marginal                                                                                               |
| Typical estimand | Difference in means                                                                  | Risk difference                                                                                        |
| Model            | Linear regression                                                                    | Logistic regression                                                                                    |
| Interpretation   | Effect at mean covariates                                                            | Population-averaged effect                                                                             |
| Function         | [`rbmi::ancova`](https://openpharma.github.io/rbmi/latest-tag/reference/ancova.html) | [`gcomp_responder_multi()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_responder_multi.md) |

## Example: Continuous Outcome with ANCOVA

``` r
library(rbmi)
library(rbmiUtils)
library(dplyr)

# Load example data
data("ADMI")

# Prepare data
ADMI <- ADMI |>
  mutate(
    TRT = factor(TRT, levels = c("Placebo", "Drug A")),
    USUBJID = factor(USUBJID),
    AVISIT = factor(AVISIT),
    STRATA = factor(STRATA),
    REGION = factor(REGION)
  )

# Define variables
vars <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CHG",  # Continuous outcome
  covariates = c("BASE", "STRATA", "REGION")
)

# Define method
method <- method_bayes(
  n_samples = 100,
  control = control_bayes(warmup = 200, thin = 5)
)

# ANCOVA analysis
ana_ancova <- analyse_mi_data(
  data = ADMI,
  vars = vars,
  method = method,
  fun = ancova  # ANCOVA for continuous outcome
)

# Pool and tidy results
pool_ancova <- pool(ana_ancova)
tidy_ancova <- tidy_pool_obj(pool_ancova)

# View treatment effects
extract_trt_effects(tidy_ancova)
```

## Example: Binary Outcome with G-computation

``` r
# Create binary responder outcome
ADMI_binary <- ADMI |>
  mutate(RESP = as.integer(CHG < -5))  # Responder if CHG improved by >5 units

# Update vars for binary outcome
vars_binary <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "RESP",  # Binary outcome
  covariates = c("BASE", "STRATA", "REGION")
)

# G-computation analysis for binary outcome
ana_gcomp <- analyse_mi_data(
  data = ADMI_binary,
  vars = vars_binary,
  method = method,
  fun = gcomp_responder_multi,  # G-computation for binary
  reference_levels = "Placebo",
  contrast = "diff",  # Risk difference
  var_method = "Ge",
  type = "HC0"
)

# Pool results
pool_gcomp <- pool(ana_gcomp)

# View results (note: different parameter naming)
print(pool_gcomp)
```

## Interpreting Results

### ANCOVA Results

ANCOVA provides:

- **Treatment comparison (`trt_*`)**: Difference in adjusted means
  between treatment and control
- **Least squares means (`lsm_ref_*`, `lsm_alt_*`)**: Adjusted mean
  outcome for reference and alternative treatment groups at each visit

Example interpretation: “The adjusted mean change from baseline was -5.2
units (95% CI: -7.1, -3.3) greater in the Drug A group compared to
Placebo at Week 24.”

### G-computation Results

G-computation provides:

- **Treatment comparison (`trt_*`)**: Risk difference between treatments
- **Marginal risks (`lsm_*`)**: Response probability for each treatment
  group

Example interpretation: “The response rate was 15.3 percentage points
(95% CI: 8.2, 22.4) higher in the Drug A group compared to Placebo at
Week 24.”

## Statistical Considerations

### Conditional vs. Marginal Effects

**Conditional effects** (ANCOVA):

- Answer: “What is the treatment effect for a patient with average
  covariates?”
- Can vary depending on the covariate values
- More efficient when covariates are predictive

**Marginal effects** (G-computation):

- Answer: “What is the average treatment effect across the population?”
- Averaged over the covariate distribution in the data
- More directly interpretable for policy decisions

### Model Assumptions

**ANCOVA assumptions:**

1.  Linear relationship between outcome and covariates
2.  Homogeneity of regression slopes (same relationship in both groups)
3.  Normality of residuals (for inference)

**G-computation assumptions:**

1.  Correct specification of the outcome model
2.  No unmeasured confounding (satisfied by randomization)
3.  Positivity (all covariate patterns can occur in both groups)

### Variance Estimation

**ANCOVA:**

- Uses standard regression-based variance
- Rubin’s rules for pooling across imputations

**G-computation:**

- Uses robust (sandwich) variance estimation via `beeca` package
- Methods available: “Ge” (default), “Ye”, etc.
- HC0, HC1, HC3 variance types available

## Recommendations

### Use ANCOVA when:

1.  Your primary endpoint is continuous
2.  Linear covariate adjustment is appropriate
3.  You need LSM estimates for each group
4.  Regulatory precedent uses ANCOVA

### Use G-computation when:

1.  Your endpoint is binary (responder analysis)
2.  You want marginal treatment effects
3.  You need risk differences
4.  Population-level interpretation is preferred

### Both methods can be used when:

You have both continuous and binary endpoints in your trial. Apply
ANCOVA to continuous endpoints and G-computation to binary endpoints.

## Additional Options

### Custom Analysis Functions

You can also create custom analysis functions for special cases:

``` r
# Custom function returning treatment means
custom_means <- function(data, vars, ...) {
  visits <- unique(data[[vars$visit]])
  result <- list()

  for (v in visits) {
    visit_data <- data[data[[vars$visit]] == v, ]
    for (grp in levels(data[[vars$group]])) {
      grp_data <- visit_data[visit_data[[vars$group]] == grp, ]
      key <- paste0("mean_", gsub(" ", "", grp), "_", gsub(" ", "", v))
      n <- nrow(grp_data)
      result[[key]] <- list(
        est = mean(grp_data[[vars$outcome]], na.rm = TRUE),
        se = sd(grp_data[[vars$outcome]], na.rm = TRUE) / sqrt(n),
        df = n - 1
      )
    }
  }
  result
}

# Use custom function
ana_custom <- analyse_mi_data(
  data = ADMI,
  vars = vars,
  method = method,
  fun = custom_means
)
```

## Conclusion

Choose your analysis method based on:

1.  **Outcome type**: Continuous -\> ANCOVA, Binary -\> G-computation
2.  **Estimand type**: Conditional -\> ANCOVA, Marginal -\>
    G-computation
3.  **Regulatory context**: Check precedent in your therapeutic area

Both methods are well-supported in rbmiUtils and can be easily applied
to imputed datasets using
[`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md).
