# Summary Method for Pool Objects

Provides a detailed visit-level breakdown of pooled analysis results
with significance flags. Shows treatment comparisons and least squares
means grouped by visit.

## Usage

``` r
# S3 method for class 'pool'
summary(object, alpha = 0.05, ...)
```

## Arguments

- object:

  An object of class `pool`, typically obtained from
  [`rbmi::pool()`](https://openpharma.github.io/rbmi/latest-tag/reference/pool.html).

- alpha:

  Numeric. Significance threshold for flagging p-values. Default is
  0.05. Flags are: `*` for p \< alpha, `**` for p \< 0.01, `***` for p
  \< 0.001.

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns a list with:

- n_parameters:

  Number of parameters in the pool object

- visits:

  Character vector of unique visit names

- method:

  Pooling method used

- n_imputations:

  Number of imputations combined

- conf.level:

  Confidence level

- tidy_df:

  The full tidy tibble from
  [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)

## Details

The summary output groups results by visit, showing treatment
comparisons with significance flags and least squares means. This
provides a quick overview of which visits have statistically significant
treatment effects.

Significance flags:

- `*` p \< alpha (default 0.05)

- `**` p \< 0.01

- `***` p \< 0.001

## See also

- [`print.pool()`](https://openpharma.github.io/rbmiUtils/reference/print.pool.md)
  for compact tabular output

- [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
  for full tidy tibble output

- [`rbmi::pool()`](https://openpharma.github.io/rbmi/latest-tag/reference/pool.html)
  to create pool objects

## Examples

``` r
# \donttest{
library(rbmi)
library(rbmiUtils)
data("ADMI")

ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
ADMI$USUBJID <- factor(ADMI$USUBJID)
ADMI$AVISIT <- factor(ADMI$AVISIT)

vars <- set_vars(
  subjid = "USUBJID", visit = "AVISIT", group = "TRT",
  outcome = "CHG", covariates = c("BASE", "STRATA", "REGION")
)
method <- method_bayes(n_samples = 20, control = control_bayes(warmup = 20))

ana_obj <- analyse_mi_data(ADMI, vars, method, fun = ancova)
#> Warning: Data contains 100 imputations but method expects 20. Using first 20
#> imputations.
pool_obj <- pool(ana_obj)
summary(pool_obj)
#> 
#> ── Pool Object Summary ─────────────────────────────────────────────────────────
#> Method: rubin
#> N imputations: 20
#> Confidence: 95%
#> Alternative: two.sided
#> 6 parameters across 2 visits
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Week 24 ──
#> 
#> Treatment Comparisons
#> Treatment Comparison: -2.18 (-2.54, -1.82) p=< 0.001 ***
#> Least Squares Means
#> Least Squares Mean for Reference at Week 24: 0.09 (-0.17, 0.34)
#> Least Squares Mean for Alternative at Week 24: -2.1 (-2.34, -1.85)
#> 
#> ── Week 48 ──
#> 
#> Treatment Comparisons
#> Treatment Comparison: -3.79 (-4.3, -3.29) p=< 0.001 ***
#> Least Squares Means
#> Least Squares Mean for Reference at Week 48: 0.04 (-0.33, 0.4)
#> Least Squares Mean for Alternative at Week 48: -3.76 (-4.11, -3.41)
# }
```
