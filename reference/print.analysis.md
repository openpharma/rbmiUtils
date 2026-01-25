# Print Method for Analysis Objects

Prints a summary of an analysis object from
[`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md).

## Usage

``` r
# S3 method for class 'analysis'
print(x, ...)
```

## Arguments

- x:

  An object of class `analysis`.

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns the input object.

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
#> Warning: Data contains 100 imputations but method expects 10. Using first 10 imputations.
print(ana_obj)
#> Analysis object from rbmiUtils
#> -------------------------------
#> Number of imputations: 10 
#> Analysis function: <Anonymous Function> 
#> Delta adjustment: No
#> Method type: bayes 
#> Pooling method: rubin 
#> 
#> Use `rbmi::pool()` to obtain pooled estimates.
# }
```
