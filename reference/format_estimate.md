# Format Estimate with Confidence Interval

Formats a point estimate with its confidence interval in standard
publication format: "estimate (lower, upper)".

## Usage

``` r
format_estimate(estimate, lower, upper, digits = 2, sep = ", ")
```

## Arguments

- estimate:

  Numeric vector of point estimates.

- lower:

  Numeric vector of lower confidence interval bounds.

- upper:

  Numeric vector of upper confidence interval bounds.

- digits:

  Integer. Number of decimal places for rounding. Default is 2.

- sep:

  Character. Separator between lower and upper bounds. Default is ", ".

## Value

A character vector of formatted estimates with confidence intervals.

## Details

The function formats estimates as "X.XX (X.XX, X.XX)" by default. All
three input vectors must have the same length. `NA` values in any
position result in `NA_character_` for that element.

## Examples

``` r
# Single estimate
format_estimate(1.5, 0.8, 2.2)
#> [1] "1.50 (0.80, 2.20)"
#> "1.50 (0.80, 2.20)"

# Multiple estimates
format_estimate(
  estimate = c(-2.5, -1.8),
  lower = c(-4.0, -3.2),
  upper = c(-1.0, -0.4)
)
#> [1] "-2.50 (-4.00, -1.00)" "-1.80 (-3.20, -0.40)"
#> "-2.50 (-4.00, -1.00)" "-1.80 (-3.20, -0.40)"

# More decimal places
format_estimate(0.234, 0.123, 0.345, digits = 3)
#> [1] "0.234 (0.123, 0.345)"
#> "0.234 (0.123, 0.345)"

# Different separator
format_estimate(1.5, 0.8, 2.2, sep = " to ")
#> [1] "1.50 (0.80 to 2.20)"
#> "1.50 (0.80 to 2.20)"
```
