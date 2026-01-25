# Format Results Table for Publication

Adds formatted columns to a tidy results table, creating
publication-ready output with properly formatted estimates, confidence
intervals, and p-values.

## Usage

``` r
format_results_table(
  data,
  est_col = "est",
  lci_col = "lci",
  uci_col = "uci",
  pval_col = "pval",
  est_digits = 2,
  pval_digits = 3,
  pval_threshold = 0.001,
  ci_sep = ", "
)
```

## Arguments

- data:

  A data.frame or tibble, typically output from
  [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md).

- est_col:

  Character. Name of the estimate column. Default is "est".

- lci_col:

  Character. Name of the lower CI column. Default is "lci".

- uci_col:

  Character. Name of the upper CI column. Default is "uci".

- pval_col:

  Character. Name of the p-value column. Default is "pval".

- est_digits:

  Integer. Decimal places for estimates. Default is 2.

- pval_digits:

  Integer. Decimal places for p-values. Default is 3.

- pval_threshold:

  Numeric. Threshold for p-value formatting. Default is 0.001.

- ci_sep:

  Character. Separator for CI bounds. Default is ", ".

## Value

A tibble with additional formatted columns:

- est_ci:

  Formatted estimate with confidence interval

- pval_fmt:

  Formatted p-value

## Details

This function is designed to work with output from
[`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
but can be used with any data.frame containing estimate, CI, and p-value
columns. The original columns are preserved; new formatted columns are
added.

## Examples

``` r
library(dplyr)

# Create example results
results <- tibble::tibble(
  parameter = c("trt_Week24", "lsm_ref_Week24", "lsm_alt_Week24"),
  description = c("Treatment Effect", "LS Mean (Reference)", "LS Mean (Treatment)"),
  est = c(-2.45, 5.20, 2.75),
  se = c(0.89, 0.65, 0.71),
  lci = c(-4.20, 3.93, 1.36),
  uci = c(-0.70, 6.47, 4.14),
  pval = c(0.006, NA, NA)
)

# Format for publication
formatted <- format_results_table(results)
print(formatted[, c("description", "est_ci", "pval_fmt")])
#> # A tibble: 3 × 3
#>   description         est_ci               pval_fmt
#>   <chr>               <chr>                <chr>   
#> 1 Treatment Effect    -2.45 (-4.20, -0.70) 0.006   
#> 2 LS Mean (Reference) 5.20 (3.93, 6.47)    NA      
#> 3 LS Mean (Treatment) 2.75 (1.36, 4.14)    NA      
```
