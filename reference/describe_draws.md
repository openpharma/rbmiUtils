# Describe an rbmi Draws Object

Extracts structured metadata from an rbmi draws object, including
method, formula, sample count, failures, covariance structure, and (for
Bayesian methods) MCMC convergence diagnostics. Returns an S3 object
with an informative [`print()`](https://rdrr.io/r/base/print.html)
method.

## Usage

``` r
describe_draws(draws_obj)
```

## Arguments

- draws_obj:

  A `draws` object returned by
  [`rbmi::draws()`](https://openpharma.github.io/rbmi/latest-tag/reference/draws.html).

## Value

An S3 object of class `c("describe_draws", "list")` containing:

- method:

  Human-readable method name (e.g., "Bayesian (MCMC via Stan)")

- method_class:

  Raw class name: "bayes", "approxbayes", or "condmean"

- n_samples:

  Total number of samples

- n_failures:

  Number of failed samples

- formula:

  Deparsed model formula string

- covariance:

  Covariance structure (e.g., "us")

- same_cov:

  Logical; whether same covariance is used across groups

- condmean_type:

  (condmean only) "jackknife" or "bootstrap"

- n_primary:

  (condmean only) Always 1

- n_resampled:

  (condmean only) Number of resampled draws

- bayes_control:

  (bayes only) List with warmup, thin, chains, seed

- mcmc:

  (bayes with stanfit only) List with rhat, ess, max_rhat, min_ess,
  n_params, converged

## Details

For conditional mean methods, the sample count is displayed as "1 + N"
matching the rbmi convention where the first sample is the primary
(full-data) fit and the remaining N are jackknife or bootstrap
resamples.

For Bayesian methods, MCMC convergence diagnostics (ESS, Rhat) are
extracted from the `stanfit` object when `rstan` is available. The
`converged` flag uses the Rhat \< 1.1 threshold matching rbmi's own
convention.

## See also

- [`rbmi::draws()`](https://openpharma.github.io/rbmi/latest-tag/reference/draws.html)
  to create draws objects

- [`rbmi::method_condmean()`](https://openpharma.github.io/rbmi/latest-tag/reference/method.html),
  [`rbmi::method_bayes()`](https://openpharma.github.io/rbmi/latest-tag/reference/method.html),
  [`rbmi::method_approxbayes()`](https://openpharma.github.io/rbmi/latest-tag/reference/method.html)
  for method specification

## Examples

``` r
if (FALSE) { # \dontrun{
library(rbmi)
library(dplyr)
data("ADEFF", package = "rbmiUtils")

# Prepare ADEFF data for rbmi pipeline
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
dat <- ADEFF |> select(USUBJID, STRATA, REGION, TRT, BASE, CHG, AVISIT)
draws_obj <- draws(
  data = dat, vars = vars,
  method = method_bayes(n_samples = 100)
)

# Inspect the draws object
desc <- describe_draws(draws_obj)
print(desc)

# Programmatic access to metadata
desc$method
desc$n_samples
desc$formula
} # }
```
