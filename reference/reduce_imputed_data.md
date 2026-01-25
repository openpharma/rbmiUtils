# Reduce Imputed Data for Efficient Storage

Extracts only the imputed records (those that were originally missing)
from a full imputed dataset. This significantly reduces storage
requirements when working with many imputations, as observed values are
identical across all imputations and only need to be stored once in the
original data.

## Usage

``` r
reduce_imputed_data(imputed_data, original_data, vars)
```

## Arguments

- imputed_data:

  A data.frame containing the full imputed dataset with an `IMPID`
  column identifying each imputation. Typically the output from
  [`get_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/get_imputed_data.md).

- original_data:

  A data.frame containing the original dataset before imputation, with
  missing values in the outcome column.

- vars:

  A `vars` object as created by
  [`rbmi::set_vars()`](https://openpharma.github.io/rbmi/latest-tag/reference/set_vars.html).

## Value

A data.frame containing only the rows from `imputed_data` that
correspond to originally missing outcome values. All columns from
`imputed_data` are preserved.

## Details

Storage savings depend on the proportion of missing data. For example:

- Original: 1000 rows, 44 missing values

- Full imputed (1000 imputations): 1,000,000 rows

- Reduced (1000 imputations): 44,000 rows (4.4\\

Use
[`expand_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/expand_imputed_data.md)
to reconstruct the full imputed dataset when needed for analysis.

## See also

[`expand_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/expand_imputed_data.md)
to reconstruct the full dataset,
[`get_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/get_imputed_data.md)
to extract imputed data from an rbmi imputation object.

## Examples

``` r
library(rbmi)
library(dplyr)

# Example with package data
data("ADMI", package = "rbmiUtils")
data("ADEFF", package = "rbmiUtils")

# Prepare original data to match ADMI structure
original <- ADEFF |>
  mutate(
    TRT = TRT01P,
    USUBJID = as.character(USUBJID)
  )

vars <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CHG"
)

# Reduce to only imputed values
reduced <- reduce_imputed_data(ADMI, original, vars)

# Compare sizes
cat("Full imputed rows:", nrow(ADMI), "\n")
#> Full imputed rows: 100000 
cat("Reduced rows:", nrow(reduced), "\n")
#> Reduced rows: 4400 
cat("Compression:", round(100 * nrow(reduced) / nrow(ADMI), 1), "%\n")
#> Compression: 4.4 %
```
