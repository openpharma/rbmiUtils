# Describe an rbmi Imputation Object

Extracts structured metadata from an rbmi imputation object, including
method, number of imputations (M), reference arm mappings, subject
count, and a missingness breakdown by visit and treatment arm. Returns
an S3 object with an informative
[`print()`](https://rdrr.io/r/base/print.html) method.

## Usage

``` r
describe_imputation(impute_obj)
```

## Arguments

- impute_obj:

  An `imputation` object returned by
  [`rbmi::impute()`](https://openpharma.github.io/rbmi/latest-tag/reference/impute.html).

## Value

An S3 object of class `c("describe_imputation", "list")` containing:

- method:

  Human-readable method name (e.g., "Bayesian (MCMC via Stan)")

- method_class:

  Raw class name: "bayes", "approxbayes", or "condmean"

- n_imputations:

  Number of imputations (M)

- references:

  Named character vector of reference arm mappings, or NULL

- n_subjects:

  Total number of unique subjects

- visits:

  Character vector of visit names

- missingness:

  A data.frame with columns: visit, group, n_total, n_miss, pct_miss

## Details

The missingness table is built by cross-tabulating
`impute_obj$data$is_missing` by visit and treatment group. Each row
shows the total number of subjects in that group, how many had missing
data at that visit, and the percentage missing.

## See also

- [`rbmi::impute()`](https://openpharma.github.io/rbmi/latest-tag/reference/impute.html)
  to create imputation objects

- [`describe_draws()`](https://openpharma.github.io/rbmiUtils/reference/describe_draws.md)
  for inspecting draws objects

## Examples

``` r
if (FALSE) { # \dontrun{
library(rbmi)
library(dplyr)
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
dat <- ADEFF |> select(USUBJID, STRATA, REGION, TRT, BASE, CHG, AVISIT)
draws_obj <- draws(
  data = dat, vars = vars,
  method = method_bayes(n_samples = 100)
)
impute_obj <- impute(
  draws_obj,
  references = c("Placebo" = "Placebo", "Drug A" = "Placebo")
)

# Inspect the imputation
desc <- describe_imputation(impute_obj)
print(desc)

# Programmatic access
desc$n_imputations
desc$missingness
desc$references
} # }
```
