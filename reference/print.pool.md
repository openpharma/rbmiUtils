# Print Method for Pool Objects

Displays a formatted summary of a pooled analysis object from
[`rbmi::pool()`](https://openpharma.github.io/rbmi/latest-tag/reference/pool.html).
Uses cli formatting to show rounded estimates, confidence intervals,
parameter labels, method information, number of imputations, and
confidence level.

## Usage

``` r
# S3 method for class 'pool'
print(x, digits = 2, ...)
```

## Arguments

- x:

  An object of class `pool`, typically obtained from
  [`rbmi::pool()`](https://openpharma.github.io/rbmi/latest-tag/reference/pool.html).

- digits:

  Integer. Number of decimal places for rounding estimates, standard
  errors, and confidence interval bounds. Default is 2.

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns the original pool object `x` (for pipe chaining).

## Details

This method overrides
[`rbmi::print.pool()`](https://openpharma.github.io/rbmi/latest-tag/reference/pool.html)
to provide enhanced, formatted console output using the cli package. The
override produces a "Registered S3 method overwritten" message at
package load time, which is expected and harmless (same pattern as
[`print.analysis()`](https://openpharma.github.io/rbmiUtils/reference/print.analysis.md)).

The output includes:

- A header with parameter and visit counts

- Metadata: pooling method, number of imputations, confidence level

- A compact results table with key columns: parameter, visit, est, lci,
  uci, pval

## See also

- [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
  for full tidy tibble output

- [`summary.pool()`](https://openpharma.github.io/rbmiUtils/reference/summary.pool.md)
  for visit-level breakdown with significance flags

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
print(pool_obj)
#> 
#> Pool Object
#> -----------
#> Number of Results Combined: 20
#> Method: rubin
#> Confidence Level: 0.95
#> Alternative: two.sided
#> 
#> Results:
#> 
#>   ========================================================
#>       parameter      est     se     lci     uci     pval  
#>   --------------------------------------------------------
#>      trt_Week 24    -2.181  0.182  -2.539  -1.823  <0.001 
#>    lsm_ref_Week 24  0.086   0.132  -0.173  0.344   0.514  
#>    lsm_alt_Week 24  -2.096  0.126  -2.344  -1.847  <0.001 
#>      trt_Week 48    -3.794  0.256  -4.297  -3.29   <0.001 
#>    lsm_ref_Week 48  0.036   0.186  -0.33   0.403   0.846  
#>    lsm_alt_Week 48  -3.758  0.177  -4.106  -3.409  <0.001 
#>   --------------------------------------------------------
#> 
# }
```
