# MI Diagnostics and Pipeline Inspection

## Introduction

The rbmi multiple imputation pipeline produces several intermediate
objects – draws, imputation, analysis, and pool – each containing useful
metadata about the imputation model and results. **rbmiUtils** v0.3.0
adds tools to inspect these objects and extract diagnostic statistics,
making it easier to verify that the MI pipeline behaved as expected.

This vignette covers three features:

- [`describe_draws()`](https://openpharma.github.io/rbmiUtils/reference/describe_draws.md):
  structured metadata from draws objects (method, formula, samples, MCMC
  convergence)
- [`describe_imputation()`](https://openpharma.github.io/rbmiUtils/reference/describe_imputation.md):
  structured metadata from imputation objects (method, M, references,
  missingness breakdown)
- [`pool_to_ard()`](https://openpharma.github.io/rbmiUtils/reference/pool_to_ard.md)
  MI diagnostic enrichment: fraction of missing information (FMI),
  lambda, relative increase in variance (RIV), and other Rubin’s rules
  diagnostics embedded in the ARD output

## Setup

We load the required packages and prepare data for the
[`pool_to_ard()`](https://openpharma.github.io/rbmiUtils/reference/pool_to_ard.md)
diagnostic enrichment examples in Section 5. This setup uses
[`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md)
which works directly with the pre-imputed `ADMI` dataset (containing an
`IMPID` column), so there is no need for
[`draws()`](https://openpharma.github.io/rbmi/latest-tag/reference/draws.html)
or
[`impute()`](https://openpharma.github.io/rbmi/latest-tag/reference/impute.html).

``` r
library(rbmiUtils)
library(rbmi)
library(dplyr)

data("ADMI", package = "rbmiUtils")
ADMI <- ADMI |>
  mutate(
    TRT = factor(TRT, levels = c("Placebo", "Drug A")),
    USUBJID = factor(USUBJID),
    AVISIT = factor(AVISIT)
  )

vars <- set_vars(
  subjid = "USUBJID", visit = "AVISIT", group = "TRT",
  outcome = "CHG", covariates = c("BASE", "STRATA", "REGION")
)
method <- method_bayes(
  n_samples = 100,
  control = control_bayes(warmup = 200, thin = 5)
)

ana_obj <- analyse_mi_data(ADMI, vars, method, fun = ancova)
pool_obj <- pool(ana_obj)
```

## Inspecting Draws with `describe_draws()`

The
[`describe_draws()`](https://openpharma.github.io/rbmiUtils/reference/describe_draws.md)
function extracts structured metadata from an rbmi `draws` object,
providing a quick summary of the imputation model configuration and, for
Bayesian methods, MCMC convergence diagnostics.

The code below uses `ADEFF` data and defines its own `vars` and `method`
objects. We use `eval = FALSE` because
[`draws()`](https://openpharma.github.io/rbmi/latest-tag/reference/draws.html)
runs MCMC sampling, which is too slow for vignette builds.

``` r
data("ADEFF", package = "rbmiUtils")
ADEFF <- ADEFF |>
  mutate(
    TRT = factor(TRT01P, levels = c("Placebo", "Drug A")),
    USUBJID = factor(USUBJID),
    AVISIT = factor(AVISIT, levels = c("Week 24", "Week 48"))
  )

vars <- set_vars(
  subjid = "USUBJID", visit = "AVISIT", group = "TRT",
  outcome = "CHG", covariates = c("BASE", "STRATA", "REGION")
)
method <- method_bayes(
  n_samples = 100,
  control = control_bayes(warmup = 200, thin = 2)
)

dat <- ADEFF |> select(USUBJID, STRATA, REGION, TRT, BASE, CHG, AVISIT)
draws_obj <- draws(data = dat, vars = vars, method = method)
desc <- describe_draws(draws_obj)
print(desc)
```

Example output from
[`describe_draws()`](https://openpharma.github.io/rbmiUtils/reference/describe_draws.md):

    -- Draws Summary --
    Method: Bayesian (MCMC via Stan)
    Formula: CHG ~ 1 + BASE + STRATA + REGION + TRT + AVISIT + TRT:AVISIT
    Samples: 100
    Failures: 0
    Covariance: us
    Same covariance across groups: Yes
    --
    -- MCMC Convergence --
    v All Rhat < 1.1 (42 parameters)
    Max Rhat: 1.003
    Min ESS: 245.2

The returned object is a list with programmatic access to all fields:

- `$method` – human-readable method name (e.g., “Bayesian (MCMC via
  Stan)”)
- `$method_class` – raw class: `"bayes"`, `"approxbayes"`, or
  `"condmean"`
- `$formula` – the deparsed model formula string
- `$n_samples` – total number of samples drawn
- `$n_failures` – number of failed samples
- `$mcmc` – (Bayesian only) list with `rhat`, `ess`, `max_rhat`,
  `min_ess`, `n_params`, `converged`

## Inspecting Imputations with `describe_imputation()`

The
[`describe_imputation()`](https://openpharma.github.io/rbmiUtils/reference/describe_imputation.md)
function extracts metadata from an rbmi `imputation` object, including
the method, number of imputations (M), reference arm mappings, and a
missingness breakdown by visit and treatment arm.

This section continues from the `draws_obj` created in the code above
(Section 3). Again, we use `eval = FALSE` because the pipeline requires
MCMC.

``` r
impute_obj <- impute(
  draws_obj,
  references = c("Placebo" = "Placebo", "Drug A" = "Placebo")
)
desc <- describe_imputation(impute_obj)
print(desc)
```

Example output from
[`describe_imputation()`](https://openpharma.github.io/rbmiUtils/reference/describe_imputation.md):

    -- Imputation Summary --
    Method: Bayesian (MCMC via Stan)
    Imputations (M): 100
    Subjects: 200
    --
    -- References --
    Placebo -> Placebo
    Drug A -> Placebo
    -- Missingness by Visit and Arm --
        visit   group n_total n_miss pct_miss
      Week 24 Placebo     100      8      8.0
      Week 24  Drug A     100     10     10.0
      Week 48 Placebo     100     15     15.0
      Week 48  Drug A     100     18     18.0

The returned object provides programmatic access to:

- `$method` – human-readable method name
- `$n_imputations` – number of imputations (M)
- `$n_subjects` – total number of unique subjects
- `$references` – named character vector of reference arm mappings (or
  `NULL`)
- `$missingness` – a `data.frame` with columns `visit`, `group`,
  `n_total`, `n_miss`, `pct_miss`

## MI Diagnostic Statistics in ARD

The
[`pool_to_ard()`](https://openpharma.github.io/rbmiUtils/reference/pool_to_ard.md)
function converts a pool object to the pharmaverse Analysis Results
Dataset (ARD) format. When you also pass the `analysis_obj`, it enriches
the ARD with MI diagnostic statistics computed from Rubin’s rules.

``` r
# Base ARD (no diagnostics)
ard <- pool_to_ard(pool_obj)

# Enriched ARD with MI diagnostics
ard_enriched <- pool_to_ard(pool_obj, analysis_obj = ana_obj)
```

The enriched ARD includes additional rows for each parameter with
diagnostic statistics. We can filter and display them:

``` r
ard_enriched |>
  dplyr::filter(stat_name %in% c("fmi", "lambda", "riv", "df.adjusted", "re")) |>
  dplyr::select(group1_level, variable_level, stat_name, stat)
#> {cards} data frame: 30 x 4
#>    group1_level variable_level   stat_name    stat
#> 1       Week 24             NA         fmi   0.023
#> 2       Week 24             NA      lambda   0.019
#> 3       Week 24             NA         riv   0.019
#> 4       Week 24             NA df.adjusted 480.772
#> 5       Week 24             NA          re       1
#> 6       Week 24             NA         fmi   0.021
#> 7       Week 24             NA      lambda   0.017
#> 8       Week 24             NA         riv   0.018
#> 9       Week 24             NA df.adjusted 481.809
#> 10      Week 24             NA          re       1
#> ℹ 20 more rows
#> ℹ Use `print(n = ...)` to see more rows
```

Each diagnostic statistic has a specific interpretation:

- **FMI** (fraction of missing information) – the adjusted proportion of
  total sampling variance attributable to missing data, following the
  mice convention: `(riv + 2/(df + 3)) / (1 + riv)`
- **lambda** – the proportion of total variance due to
  between-imputation variance (missingness)
- **RIV** (relative increase in variance) – the ratio of
  between-imputation variance to within-imputation variance, scaled by
  `(1 + 1/M)`
- **df.adjusted** – Barnard-Rubin adjusted degrees of freedom, which
  accounts for finite complete-data degrees of freedom
- **re** (relative efficiency) – `1 / (1 + fmi/M)`, the efficiency of
  the MI estimator relative to an estimator with infinite imputations

## When Diagnostics Are Not Available

Non-Rubin pooling methods (e.g., conditional mean with jackknife) do not
produce MI diagnostic statistics because the variance decomposition does
not apply. When
[`pool_to_ard()`](https://openpharma.github.io/rbmiUtils/reference/pool_to_ard.md)
is called with an `analysis_obj` from a non-Rubin method, it emits an
informative message and omits diagnostic rows from the ARD.

The
[`describe_draws()`](https://openpharma.github.io/rbmiUtils/reference/describe_draws.md)
and
[`describe_imputation()`](https://openpharma.github.io/rbmiUtils/reference/describe_imputation.md)
functions work with all method types (Bayesian, approximate Bayesian,
and conditional mean).

## Learn More

- [From rbmi Analysis to Regulatory
  Tables](https://openpharma.github.io/rbmiUtils/articles/pipeline.md) –
  the full end-to-end pipeline vignette
- [`pool_to_ard()`](https://openpharma.github.io/rbmiUtils/reference/pool_to_ard.md)
  – function documentation with ARD format details
- [`describe_draws()`](https://openpharma.github.io/rbmiUtils/reference/describe_draws.md)
  and
  [`describe_imputation()`](https://openpharma.github.io/rbmiUtils/reference/describe_imputation.md)
  – function documentation with full field descriptions
