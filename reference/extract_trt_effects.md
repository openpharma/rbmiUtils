# Extract Treatment Effect Estimates

Convenience function to extract only treatment comparison estimates from
tidy results, filtering out least squares means.

## Usage

``` r
extract_trt_effects(results, visit = NULL)
```

## Arguments

- results:

  A tibble from
  [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md).

- visit:

  Optional character vector of visits to filter. If NULL (default),
  returns results for all visits.

## Value

A tibble containing only treatment effect rows.

## See also

- [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
  to create tidy results

- [`extract_lsm()`](https://openpharma.github.io/rbmiUtils/reference/extract_lsm.md)
  to extract least squares means

## Examples

``` r
# \donttest{
library(rbmi)

# Assuming you have a tidy result
# tidy_result <- tidy_pool_obj(pool_obj)
# trt_effects <- extract_trt_effects(tidy_result)
# trt_week24 <- extract_trt_effects(tidy_result, visit = "Week 24")
# }
```
