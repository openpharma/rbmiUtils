# Combine Results Across Multiple Analyses

Combines tidy result tibbles from multiple analyses (e.g., different
endpoints or subgroups) into a single table with an identifying column.

## Usage

``` r
combine_results(..., results_list = NULL, id_col = "analysis")
```

## Arguments

- ...:

  Named arguments where each is a tidy result tibble from
  [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md).

- results_list:

  Alternative to `...`: a named list of tidy result tibbles.

- id_col:

  Character string specifying the name of the identifier column. Default
  is "analysis".

## Value

A tibble with all results combined, with an additional column
identifying the source analysis.

## See also

- [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
  to create tidy results from pooled objects

- [`format_results()`](https://openpharma.github.io/rbmiUtils/reference/format_results.md)
  to format combined results for reporting

## Examples

``` r
# \donttest{
library(rbmi)
library(dplyr)

# Assuming you have multiple pooled results
# results_week24 <- tidy_pool_obj(pool_obj_week24)
# results_week48 <- tidy_pool_obj(pool_obj_week48)

# Combine them
# combined <- combine_results(
#   "Week 24" = results_week24,
#   "Week 48" = results_week48
# )
# }
```
