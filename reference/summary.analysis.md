# Summary Method for Analysis Objects

Provides a detailed summary of an analysis object from
[`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md).

## Usage

``` r
# S3 method for class 'analysis'
summary(object, n_preview = 5, ...)
```

## Arguments

- object:

  An object of class `analysis`.

- n_preview:

  Maximum number of parameters to show in the preview table. Defaults to
  5.

- ...:

  Additional arguments (currently unused).

## Value

A list containing summary information (invisibly).

## Examples

``` r
# \donttest{
library(rbmi)
library(rbmiUtils)
data("ADMI")

# Create analysis object
vars <- set_vars(
  subjid = "USUBJID", visit = "AVISIT", group = "TRT",
  outcome = "CHG", covariates = c("BASE", "STRATA")
)
method <- method_bayes(n_samples = 10, control = control_bayes(warmup = 10))

ana_obj <- analyse_mi_data(ADMI, vars, method, fun = function(d, v, ...) 1)
#> Warning: Data contains 100 imputations but method expects 10. Using first 10
#> imputations.
summary(ana_obj)
#> 
#> ── Analysis Object Summary ─────────────────────────────────────────────────────
#> 
#> ── Imputations ──
#> 
#> Count: 10
#> 
#> ── Analysis ──
#> 
#> Function: `<Anonymous Function>()`
#> Delta: None
#> 
#> ── Method ──
#> 
#> Type: bayes
#> Samples: 10
#> 
#> ── Pooling ──
#> 
#> Method: rubin
#> 
#> Next steps:
#> 1. `pool_obj <- rbmi::pool(analysis_obj)`
#> 2. `tidy_df <- tidy_pool_obj(pool_obj)`
# }
```
