# Efficient Storage of Imputed Data

## Introduction

When performing multiple imputation with many imputations (e.g.,
100-1000), the full imputed dataset can become very large. However, most
of this data is redundant: observed values are identical across all
imputations.

The [rbmiUtils](https://github.com/openpharma/rbmiUtils) package
provides two functions to address this:

- [`reduce_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/reduce_imputed_data.md):
  Extract only the imputed values (originally missing)
- [`expand_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/expand_imputed_data.md):
  Reconstruct the full dataset when needed

This approach can reduce storage requirements by 90% or more, depending
on the proportion of missing data.

## The Storage Problem

Consider a typical clinical trial dataset:

- 500 subjects
- 5 visits per subject = 2,500 rows
- 5% missing data = 125 missing values
- 1,000 imputations

**Full storage**: 2,500 rows × 1,000 imputations = **2.5 million rows**

**Reduced storage**: 125 missing values × 1,000 imputations = **125,000
rows** (5% of full size)

## Setup

``` r
library(dplyr)
library(rbmi)
library(rbmiUtils)
```

## Example with Package Data

The [rbmiUtils](https://github.com/openpharma/rbmiUtils) package
includes example datasets we can use:

``` r
data("ADMI", package = "rbmiUtils")  # Full imputed dataset
data("ADEFF", package = "rbmiUtils") # Original data with missing values

# Check dimensions
cat("Full imputed dataset (ADMI):", nrow(ADMI), "rows\n")
#> Full imputed dataset (ADMI): 100000 rows
cat("Number of imputations:", length(unique(ADMI$IMPID)), "\n")
#> Number of imputations: 100
```

## Reducing Imputed Data

First, prepare the original data to match the imputed data structure:

``` r
original <- ADEFF |>
 mutate(
   TRT = TRT01P,
   USUBJID = as.character(USUBJID)
 )

# Count missing values
n_missing <- sum(is.na(original$CHG))
cat("Missing values in original data:", n_missing, "\n")
#> Missing values in original data: 44
```

Define the variables specification:

``` r
vars <- set_vars(
 subjid = "USUBJID",
 visit = "AVISIT",
 group = "TRT",
 outcome = "CHG"
)
```

Now reduce the imputed data:

``` r
reduced <- reduce_imputed_data(ADMI, original, vars)

cat("Full imputed rows:", nrow(ADMI), "\n")
#> Full imputed rows: 100000
cat("Reduced rows:", nrow(reduced), "\n")
#> Reduced rows: 4400
cat("Compression ratio:", round(100 * nrow(reduced) / nrow(ADMI), 1), "%\n")
#> Compression ratio: 4.4 %
```

## What’s in the Reduced Data?

The reduced dataset contains only the rows that were originally missing:

``` r
# First few rows
head(reduced)
#> # A tibble: 6 × 12
#>   IMPID STRATA REGION  REGIONC TRT    BASE   CHG AVISIT USUBJID CRIT1FLN CRIT1FL
#>   <dbl> <chr>  <chr>     <dbl> <chr> <dbl> <dbl> <chr>  <chr>      <dbl> <chr>  
#> 1     1 A      North …       1 Plac…    12 -1.96 Week … ID011          0 N      
#> 2     1 A      Europe        3 Drug…     3 -3.71 Week … ID014          0 N      
#> 3     1 B      Europe        3 Drug…     9 -1.96 Week … ID018          0 N      
#> 4     1 A      Asia          4 Drug…    10 -5.55 Week … ID033          0 N      
#> 5     1 A      Asia          4 Drug…     0 -1.28 Week … ID061          0 N      
#> 6     1 A      South …       2 Plac…     5 -2.60 Week … ID071          0 N      
#> # ℹ 1 more variable: CRIT <chr>

# Structure matches original imputed data
cat("\nColumns in reduced data:\n")
#> 
#> Columns in reduced data:
cat(paste(names(reduced), collapse = ", "))
#> IMPID, STRATA, REGION, REGIONC, TRT, BASE, CHG, AVISIT, USUBJID, CRIT1FLN, CRIT1FL, CRIT
```

Each row represents an imputed value for a specific
subject-visit-imputation combination.

## Expanding Back to Full Data

When you need to run analyses, expand the reduced data back to full
form:

``` r
expanded <- expand_imputed_data(reduced, original, vars)

cat("Expanded rows:", nrow(expanded), "\n")
#> Expanded rows: 100000
cat("Original ADMI rows:", nrow(ADMI), "\n")
#> Original ADMI rows: 100000
```

## Verifying Data Integrity

Let’s verify that the round-trip preserves data integrity:

``` r
# Sort both datasets for comparison
admi_sorted <- ADMI |>
 arrange(IMPID, USUBJID, AVISIT)

expanded_sorted <- expanded |>
 arrange(IMPID, USUBJID, AVISIT)

# Compare CHG values
all_equal <- all.equal(
 admi_sorted$CHG,
 expanded_sorted$CHG,
 tolerance = 1e-10
)

cat("Data integrity check:", all_equal, "\n")
#> Data integrity check: Attributes: < Modes: list, NULL > Attributes: < Lengths: 1, 0 > Attributes: < names for target but not for current > Attributes: < current is not list-like >
```

## Practical Workflow

Here’s how to integrate efficient storage into your workflow:

### Save Reduced Data

``` r
# After imputation
impute_obj <- impute(draws_obj, references = c("Placebo" = "Placebo", "Drug A" = "Placebo"))
full_imputed <- get_imputed_data(impute_obj)

# Reduce for storage
reduced <- reduce_imputed_data(full_imputed, original_data, vars)

# Save both (reduced is much smaller)
saveRDS(reduced, "imputed_reduced.rds")
saveRDS(original_data, "original_data.rds")
```

### Load and Analyse

``` r
# Load saved data
reduced <- readRDS("imputed_reduced.rds")
original_data <- readRDS("original_data.rds")

# Expand when needed for analysis
full_imputed <- expand_imputed_data(reduced, original_data, vars)

# Run analysis
ana_obj <- analyse_mi_data(
 data = full_imputed,
 vars = vars,
 method = method,
 fun = ancova
)
```

## Storage Comparison

Here’s a comparison of storage requirements for different scenarios:

| Subjects | Visits | Missing % | Imputations | Full Rows | Reduced Rows | Savings |
|----------|--------|-----------|-------------|-----------|--------------|---------|
| 500      | 5      | 5%        | 100         | 250,000   | 12,500       | 95%     |
| 500      | 5      | 5%        | 1,000       | 2,500,000 | 125,000      | 95%     |
| 1,000    | 8      | 10%       | 500         | 4,000,000 | 400,000      | 90%     |
| 200      | 4      | 20%       | 1,000       | 800,000   | 160,000      | 80%     |

The savings scale with:

- **Lower missing %** = greater savings
- **More imputations** = same relative savings, but more absolute
  reduction

## When to Use This Approach

**Use reduced storage when:**

- Running many imputations (100+)
- Saving imputed data for later analysis
- Sharing data between team members
- Working with memory constraints

**Keep full data when:**

- Working interactively with few imputations
- Performing exploratory analysis
- Storage is not a concern

## Edge Cases

### No Missing Data

If the original data has no missing values,
[`reduce_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/reduce_imputed_data.md)
returns an empty data.frame:

``` r
# If original has no missing values
reduced <- reduce_imputed_data(full_imputed, complete_data, vars)
nrow(reduced)
#> [1] 0

# expand_imputed_data handles this correctly
expanded <- expand_imputed_data(reduced, complete_data, vars)
# Returns original data with IMPID = "1"
```

### Single Imputation

The functions work with any number of imputations, including just one.

## Summary

The
[`reduce_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/reduce_imputed_data.md)
and
[`expand_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/expand_imputed_data.md)
functions provide an efficient way to store imputed datasets:

1.  **Reduce** after imputation to store only what’s necessary
2.  **Expand** before analysis to reconstruct full datasets
3.  **Verify** data integrity is preserved through round-trip

This approach is particularly valuable when working with large numbers
of imputations or when storage and memory are constrained.
