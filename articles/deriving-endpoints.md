# Deriving Endpoints from Imputed Data

## Introduction

Once the rbmi pipeline has been run and imputed datasets are available,
the same imputed data can be reused to analyse derived endpoints without
re-running the computationally expensive
[`draws()`](https://openpharma.github.io/rbmi/latest-tag/reference/draws.html)
and
[`impute()`](https://openpharma.github.io/rbmi/latest-tag/reference/impute.html)
steps. This is particularly useful for binary responder endpoints, where
the response is defined by whether a subject’s outcome crosses a
pre-specified threshold.

This vignette demonstrates how to define binary responder endpoints from
imputed continuous data and analyse them using rbmi’s
[`analyse()`](https://openpharma.github.io/rbmi/latest-tag/reference/analyse.html)
/
[`pool()`](https://openpharma.github.io/rbmi/latest-tag/reference/pool.html)
machinery via **rbmiUtils** helper functions. Two types of responder
definitions are covered:

1.  **Threshold-based responder** – using the pre-derived `CRIT1FLN`
    column (CHG \> 3) already present in the imputed data.
2.  **Clinical cutoff responder** – deriving a new binary variable from
    the continuous `CHG` column using a higher threshold (CHG \> 5),
    demonstrating the flexibility of imputed data reuse.

For the full rbmi workflow including
[`draws()`](https://openpharma.github.io/rbmi/latest-tag/reference/draws.html),
[`impute()`](https://openpharma.github.io/rbmi/latest-tag/reference/impute.html),
and continuous ANCOVA analysis, see the [pipeline
vignette](https://openpharma.github.io/rbmiUtils/articles/pipeline.md).
This vignette assumes familiarity with rbmi core concepts (draws,
impute, analyse, pool).

## Prerequisites and Setup

We need **rbmi** for the analysis/pool infrastructure, **rbmiUtils** for
the analysis helpers and reporting functions, and **dplyr** for data
manipulation.

``` r
library(rbmi)
library(rbmiUtils)
library(dplyr)
```

Next, we load the pre-built `ADMI` dataset bundled with rbmiUtils. This
dataset contains 100 imputed copies of a simulated two-arm clinical
trial, with continuous change-from-baseline outcomes (`CHG`) and a
pre-derived binary responder variable (`CRIT1FLN`, defined as
`CHG > 3`).

``` r
data("ADMI", package = "rbmiUtils")
```

Before analysis, we convert the key grouping columns to factors. Factor
levels control the ordering of treatment arms and visits, so it is
important to set them explicitly.

``` r
ADMI <- ADMI |>
  mutate(
    TRT = factor(TRT, levels = c("Placebo", "Drug A")),
    USUBJID = factor(USUBJID),
    AVISIT = factor(AVISIT),
    STRATA = factor(STRATA),
    REGION = factor(REGION)
  )
```

We also define the analysis variables for the binary responder endpoint.
The outcome is `CRIT1FLN` (the numeric 0/1 responder flag), and we
adjust for baseline, stratification, and region.

``` r
vars_binary <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CRIT1FLN",
  covariates = c("BASE", "STRATA", "REGION")
)
```

Finally, we specify the method object. This must match the imputation
method originally used to create the imputed datasets – here, Bayesian
MI with 100 samples.

``` r
method <- method_bayes(
  n_samples = 100,
  control = control_bayes(warmup = 200, thin = 2)
)
```

## Threshold-Based Responder (CHG \> 3)

The `ADMI` dataset already contains the `CRIT1FLN` column, which flags
subjects as responders (`1`) if their change from baseline exceeds 3,
and non-responders (`0`) otherwise. We can verify this:

> **Interpreting the results:** In this simulated dataset, positive CHG
> values represent worsening (an increase in symptom score). Therefore
> CHG \> 3 defines “worsening responders” – subjects whose symptoms
> deteriorated by more than 3 points. A lower responder rate for Drug A
> compared to Placebo indicates that fewer patients on the active
> treatment experienced clinically meaningful worsening, which is
> evidence of drug efficacy.

``` r
# The responder criterion is pre-derived
ADMI |>
  distinct(CRIT) |>
  pull(CRIT)
#> [1] "CHG > 3"
#> attr(,"label")
#> [1] "Responder criteria (definition)"
```

### Analyse

We use
[`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md)
with
[`gcomp_responder_multi()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_responder_multi.md)
as the analysis function. This applies g-computation via logistic
regression at each visit, estimating covariate-adjusted marginal
treatment effects using the method of Ge et al. (implemented in the
beeca package).

``` r
ana_obj <- analyse_mi_data(
  data = ADMI,
  vars = vars_binary,
  method = method,
  fun = gcomp_responder_multi,
  reference_levels = "Placebo"
)
```

### Pool

Pool the per-imputation results using Rubin’s rules:

``` r
pool_obj <- pool(ana_obj)
```

### Results

The
[`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
function converts the pool object into a tidy tibble with clearly
labelled columns for estimates, standard errors, confidence intervals,
and p-values.

``` r
tidy_pool_obj(pool_obj)
#> # A tibble: 6 × 10
#>   parameter  description visit parameter_type lsm_type      est      se      lci
#>   <chr>      <chr>       <chr> <chr>          <chr>       <dbl>   <dbl>    <dbl>
#> 1 trt_Drug … Treatment … Week… trt            NA       -3.08e-2 1.15e-2 -0.0534 
#> 2 lsm_Drug … Least Squa… Week… lsm            Drug A    7.79e-5 7.85e-4 -0.00146
#> 3 lsm_Place… Least Squa… Week… lsm            Placebo   3.09e-2 1.15e-2  0.00843
#> 4 trt_Drug … Treatment … Week… trt            NA       -9.57e-2 2.10e-2 -0.137  
#> 5 lsm_Drug … Least Squa… Week… lsm            Drug A    7.27e-3 5.04e-3 -0.00262
#> 6 lsm_Place… Least Squa… Week… lsm            Placebo   1.03e-1 2.06e-2  0.0627 
#> # ℹ 2 more variables: uci <dbl>, pval <dbl>
```

The efficacy table presents the results in a regulatory-style format:

``` r
efficacy_table(
  pool_obj,
  title = "Responder Analysis: CHG > 3",
  subtitle = "G-computation with Marginal Effects (Ge et al.)",
  arm_labels = c(ref = "Placebo", alt = "Drug A")
)
```

| Responder Analysis: CHG \> 3                    |          |            |                |          |
|-------------------------------------------------|----------|------------|----------------|----------|
| G-computation with Marginal Effects (Ge et al.) |          |            |                |          |
|                                                 | Estimate | Std. Error | 95% CI         | P-value  |
| Week 24                                         |          |            |                |          |
| LS Mean (Drug A)                                | 0.00     | 0.00       | (-0.00, 0.00)  | 0.921    |
| LS Mean (Placebo)                               | 0.03     | 0.01       | (0.01, 0.05)   | 0.007    |
| Treatment Difference                            | −0.03    | 0.01       | (-0.05, -0.01) | 0.007    |
| Week 48                                         |          |            |                |          |
| LS Mean (Drug A)                                | 0.01     | 0.01       | (-0.00, 0.02)  | 0.150    |
| LS Mean (Placebo)                               | 0.10     | 0.02       | (0.06, 0.14)   | \< 0.001 |
| Treatment Difference                            | −0.10    | 0.02       | (-0.14, -0.05) | \< 0.001 |
| Pooling method: rubin                           |          |            |                |          |
| Number of imputations: 100                      |          |            |                |          |
| Confidence level: 95%                           |          |            |                |          |

## Clinical Cutoff Responder (CHG \> 5)

The key advantage of working with imputed continuous data is the
flexibility to derive new binary endpoints without re-running the
imputation model. Here, we define a more stringent “clinically
meaningful improvement” threshold of CHG \> 5.

### Derive the New Endpoint

We create a new binary column `RESP5` directly from the continuous `CHG`
values in the imputed data:

``` r
ADMI_cutoff <- ADMI |>
  mutate(RESP5 = as.numeric(CHG > 5))
```

We can inspect the responder rates by treatment arm and visit:

``` r
ADMI_cutoff |>
  group_by(TRT, AVISIT) |>
  summarise(
    n = n(),
    responders = sum(RESP5),
    rate = mean(RESP5),
    .groups = "drop"
  )
#> # A tibble: 4 × 5
#>   TRT     AVISIT      n responders    rate
#>   <fct>   <fct>   <int>      <dbl>   <dbl>
#> 1 Placebo Week 24 23900        105 0.00439
#> 2 Placebo Week 48 23900        941 0.0394 
#> 3 Drug A  Week 24 26100          0 0      
#> 4 Drug A  Week 48 26100          0 0
```

### Analyse

We define new analysis variables pointing to the `RESP5` outcome and
repeat the analysis:

``` r
vars_cutoff <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "RESP5",
  covariates = c("BASE", "STRATA", "REGION")
)
```

``` r
ana_obj_cutoff <- analyse_mi_data(
  data = ADMI_cutoff,
  vars = vars_cutoff,
  method = method,
  fun = gcomp_responder_multi,
  reference_levels = "Placebo"
)
```

### Pool and Display

``` r
pool_obj_cutoff <- pool(ana_obj_cutoff)
```

``` r
tidy_pool_obj(pool_obj_cutoff)
#> # A tibble: 6 × 10
#>   parameter         description visit parameter_type lsm_type       est       se
#>   <chr>             <chr>       <chr> <chr>          <chr>        <dbl>    <dbl>
#> 1 trt_Drug A-Place… Treatment … Week… trt            NA       -4.39e- 3 4.28e- 3
#> 2 lsm_Drug A_Week … Least Squa… Week… lsm            Drug A    1.58e-11 9.51e-12
#> 3 lsm_Placebo_Week… Least Squa… Week… lsm            Placebo   4.39e- 3 4.28e- 3
#> 4 trt_Drug A-Place… Treatment … Week… trt            NA       -4.30e- 2 1.39e- 2
#> 5 lsm_Drug A_Week … Least Squa… Week… lsm            Drug A    4.35e-10 3.45e-11
#> 6 lsm_Placebo_Week… Least Squa… Week… lsm            Placebo   4.30e- 2 1.39e- 2
#> # ℹ 3 more variables: lci <dbl>, uci <dbl>, pval <dbl>
```

``` r
efficacy_table(
  pool_obj_cutoff,
  title = "Responder Analysis: CHG > 5",
  subtitle = "G-computation with Marginal Effects (Ge et al.)",
  arm_labels = c(ref = "Placebo", alt = "Drug A")
)
```

| Responder Analysis: CHG \> 5                    |          |            |                |          |
|-------------------------------------------------|----------|------------|----------------|----------|
| G-computation with Marginal Effects (Ge et al.) |          |            |                |          |
|                                                 | Estimate | Std. Error | 95% CI         | P-value  |
| Week 24                                         |          |            |                |          |
| LS Mean (Drug A)                                | 0.00     | 0.00       | (-0.00, 0.00)  | 0.098    |
| LS Mean (Placebo)                               | 0.00     | 0.00       | (-0.00, 0.01)  | 0.304    |
| Treatment Difference                            | 0.00     | 0.00       | (-0.01, 0.00)  | 0.304    |
| Week 48                                         |          |            |                |          |
| LS Mean (Drug A)                                | 0.00     | 0.00       | (0.00, 0.00)   | \< 0.001 |
| LS Mean (Placebo)                               | 0.04     | 0.01       | (0.02, 0.07)   | 0.002    |
| Treatment Difference                            | −0.04    | 0.01       | (-0.07, -0.02) | 0.002    |
| Pooling method: rubin                           |          |            |                |          |
| Number of imputations: 100                      |          |            |                |          |
| Confidence level: 95%                           |          |            |                |          |

## Storing Results as ARD

The Analysis Results Dataset (ARD) format from the pharmaverse provides
a standardised long-format representation suitable for downstream use
with tools like gtsummary. The
[`pool_to_ard()`](https://openpharma.github.io/rbmiUtils/reference/pool_to_ard.md)
function converts a pool object into this format:

``` r
ard <- pool_to_ard(pool_obj)
print(ard)
#>    group1 group1_level         group2 group2_level   group3 group3_level
#> 1   visit      Week 24 parameter_type          trt lsm_type           NA
#> 2   visit      Week 24 parameter_type          trt lsm_type           NA
#> 3   visit      Week 24 parameter_type          trt lsm_type           NA
#> 4   visit      Week 24 parameter_type          trt lsm_type           NA
#> 5   visit      Week 24 parameter_type          trt lsm_type           NA
#> 6   visit      Week 24 parameter_type          trt lsm_type           NA
#> 7   visit      Week 24 parameter_type          lsm lsm_type       Drug A
#> 8   visit      Week 24 parameter_type          lsm lsm_type       Drug A
#> 9   visit      Week 24 parameter_type          lsm lsm_type       Drug A
#> 10  visit      Week 24 parameter_type          lsm lsm_type       Drug A
#>                      variable variable_level stat_name stat_label   stat
#> 1  trt_Drug A-Placebo_Week 24             NA  estimate   Estimate -0.031
#> 2  trt_Drug A-Placebo_Week 24             NA std.error  Std. Err…  0.012
#> 3  trt_Drug A-Placebo_Week 24             NA  conf.low  95% CI L… -0.053
#> 4  trt_Drug A-Placebo_Week 24             NA conf.high  95% CI U… -0.008
#> 5  trt_Drug A-Placebo_Week 24             NA   p.value    p-value  0.007
#> 6  trt_Drug A-Placebo_Week 24             NA    method     Method  rubin
#> 7          lsm_Drug A_Week 24             NA  estimate   Estimate      0
#> 8          lsm_Drug A_Week 24             NA std.error  Std. Err…  0.001
#> 9          lsm_Drug A_Week 24             NA  conf.low  95% CI L… -0.001
#> 10         lsm_Drug A_Week 24             NA conf.high  95% CI U…  0.002
```

Each row in the ARD represents a single statistic (estimate, standard
error, confidence interval bound, or p-value) for a given parameter,
with grouping columns for visit, parameter type, and least-squares-mean
type. This format integrates directly into pharmaverse reporting
workflows.

## Caveats

When deriving binary responder endpoints from multiply imputed
continuous data, keep the following considerations in mind:

- **Imputation model assumptions carry forward.** The binary endpoints
  are derived from continuous values that were imputed under a specific
  model (e.g., Bayesian MMRM with jump-to-reference). The validity of
  the responder analysis depends on the appropriateness of that
  continuous imputation model.

- **Pre-specify responder thresholds.** Responder definitions and their
  thresholds should be documented in the statistical analysis plan
  before unblinding. Post-hoc threshold selection risks inflating type I
  error.

- **Results are conditional on reference-based assumptions.** The
  imputed values – and therefore the derived responder status – reflect
  the chosen reference-based assumption (e.g., jump-to-reference,
  copy-reference). Different assumptions will produce different
  responder rates.
