# Format Results for Reporting

Formats a tidy results tibble for publication-ready reporting, with
options for rounding, confidence interval formatting, and column
selection.

## Usage

``` r
format_results(
  results,
  digits = 2,
  ci_format = c("parens", "brackets", "dash"),
  pval_digits = 3,
  include_se = FALSE
)
```

## Arguments

- results:

  A tibble from
  [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
  or
  [`combine_results()`](https://openpharma.github.io/rbmiUtils/reference/combine_results.md).

- digits:

  Integer specifying the number of decimal places for estimates. Default
  is 2.

- ci_format:

  Character string specifying CI format. Options are: "parens" for
  "(LCI, UCI)", "brackets" for "LCI, UCI", or "dash" for "LCI - UCI".
  Default is "parens".

- pval_digits:

  Integer specifying decimal places for p-values. Default is 3.

- include_se:

  Logical indicating whether to include standard error column. Default
  is FALSE.

## Value

A tibble with formatted columns suitable for reporting.

## See also

- [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
  to create tidy results

- [`combine_results()`](https://openpharma.github.io/rbmiUtils/reference/combine_results.md)
  to combine multiple analyses

## Examples

``` r
# \donttest{
library(rbmi)

# Assuming you have a tidy result
# tidy_result <- tidy_pool_obj(pool_obj)
# formatted <- format_results(tidy_result, digits = 3, ci_format = "brackets")
# }
```
