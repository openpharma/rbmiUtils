# Format P-values for Publication

Formats p-values according to common publication standards, with
configurable thresholds and decimal places.

## Usage

``` r
format_pvalue(x, digits = 3, threshold = 0.001, html = FALSE)
```

## Arguments

- x:

  A numeric vector of p-values.

- digits:

  Integer. Number of decimal places for rounding. Default is 3.

- threshold:

  Numeric. P-values below this threshold are displayed as "\<
  threshold". Default is 0.001.

- html:

  Logical. If `TRUE`, uses HTML formatting for the less-than symbol.
  Default is `FALSE`.

## Value

A character vector of formatted p-values.

## Details

The function applies the following rules:

- P-values below `threshold` are formatted as "\< 0.001" (or HTML
  equivalent)

- P-values \>= `threshold` are rounded to `digits` decimal places

- `NA` values are preserved as `NA_character_`

- Values \> 1 or \< 0 return `NA_character_` with a warning

## Examples

``` r
# Basic usage
format_pvalue(0.0234)
#> [1] "0.023"
#> "0.023"

format_pvalue(0.00005)
#> [1] "< 0.001"
#> "< 0.001"

# Vector input
pvals <- c(0.5, 0.05, 0.001, 0.0001, NA)
format_pvalue(pvals)
#> [1] "0.500"   "0.050"   "0.001"   "< 0.001" NA       
#> "0.500" "0.050" "0.001" "< 0.001" NA

# Custom threshold
format_pvalue(0.005, threshold = 0.01)
#> [1] "< 0.01"
#> "< 0.01"

# HTML output
format_pvalue(0.0001, html = TRUE)
#> [1] "&lt; 0.001"
#> "&lt; 0.001"
```
