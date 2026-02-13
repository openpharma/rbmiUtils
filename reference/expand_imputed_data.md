# Expand Reduced Imputed Data to Full Dataset

Reconstructs the full imputed dataset from a reduced form by merging
imputed values back with the original observed data. This is the inverse
operation of
[`reduce_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/reduce_imputed_data.md).

## Usage

``` r
expand_imputed_data(reduced_data, original_data, vars)
```

## Arguments

- reduced_data:

  A data.frame containing only the imputed values, as returned by
  [`reduce_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/reduce_imputed_data.md).

- original_data:

  A data.frame containing the original dataset before imputation, with
  missing values in the outcome column.

- vars:

  A `vars` object as created by
  [`rbmi::set_vars()`](https://openpharma.github.io/rbmi/latest-tag/reference/set_vars.html).

## Value

A data.frame containing the full imputed dataset with one complete
dataset per `IMPID` value. The structure matches the output of
[`get_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/get_imputed_data.md).

## Details

For each imputation (identified by `IMPID`), this function:

1.  Starts with the original data (observed values)

2.  Replaces missing outcome values with the corresponding imputed
    values

3.  Stacks all imputations together

## See also

- [`rbmi::impute()`](https://openpharma.github.io/rbmi/latest-tag/reference/impute.html)
  which creates the imputed datasets this function operates on

- [`reduce_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/reduce_imputed_data.md)
  to create the reduced dataset

- [`get_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/get_imputed_data.md)
  to extract imputed data from an rbmi imputation object

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

# Reduce and then expand
reduced <- reduce_imputed_data(ADMI, original, vars)
expanded <- expand_imputed_data(reduced, original, vars)

# Verify expansion
cat("Original ADMI rows:", nrow(ADMI), "\n")
#> Original ADMI rows: 100000 
cat("Expanded rows:", nrow(expanded), "\n")
#> Expanded rows: 100000 
```
