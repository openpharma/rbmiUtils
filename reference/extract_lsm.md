# Extract Least Squares Means

Convenience function to extract only least squares mean estimates from
tidy results.

## Usage

``` r
extract_lsm(results, visit = NULL, arm = NULL)
```

## Arguments

- results:

  A tibble from
  [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md).

- visit:

  Optional character vector of visits to filter. If NULL (default),
  returns results for all visits.

- arm:

  Optional character: "ref" for reference arm, "alt" for alternative
  arm, or NULL (default) for both.

## Value

A tibble containing only LSM rows.

## See also

- [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
  to create tidy results

- [`extract_trt_effects()`](https://openpharma.github.io/rbmiUtils/reference/extract_trt_effects.md)
  to extract treatment effects

## Examples

``` r
# \donttest{
library(rbmi)

# Assuming you have a tidy result
# tidy_result <- tidy_pool_obj(pool_obj)
# all_lsm <- extract_lsm(tidy_result)
# ref_lsm <- extract_lsm(tidy_result, arm = "ref")
# }
```
