# Create IMPID Column for Imputed Datasets

Adds an `IMPID` column to a list of imputed datasets, converting them to
a single stacked data.frame suitable for use with
[`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md).

## Usage

``` r
create_impid(imputed_list, id_prefix = "")
```

## Arguments

- imputed_list:

  A list of data.frames, where each element represents one imputed
  dataset.

- id_prefix:

  Optional character prefix for IMPID values. Default is empty string.

## Value

A single data.frame with all imputed datasets stacked, with an `IMPID`
column identifying the source imputation.

## Details

This function is useful when you have imputed datasets from a source
other than rbmi (e.g., from mice or another MI package) and want to use
them with rbmiUtils analysis functions.

## See also

- [`get_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/get_imputed_data.md)
  to extract imputed data from rbmi objects

- [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md)
  to analyse stacked imputed data

## Examples

``` r
# Create example imputed datasets
imp1 <- data.frame(USUBJID = c("S1", "S2"), CHG = c(1.5, 2.5))
imp2 <- data.frame(USUBJID = c("S1", "S2"), CHG = c(1.8, 2.2))
imp3 <- data.frame(USUBJID = c("S1", "S2"), CHG = c(1.6, 2.4))

# Stack with IMPID
stacked <- create_impid(list(imp1, imp2, imp3))
print(stacked)
#>   IMPID USUBJID CHG
#> 1     1      S1 1.5
#> 2     1      S2 2.5
#> 3     2      S1 1.8
#> 4     2      S2 2.2
#> 5     3      S1 1.6
#> 6     3      S2 2.4
```
