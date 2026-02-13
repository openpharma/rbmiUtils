# Data Preparation and Validation

## Introduction

This vignette demonstrates how to prepare and validate data before
running multiple imputation with
[`{rbmi}`](https://cran.r-project.org/package=rbmi). The
[rbmiUtils](https://github.com/openpharma/rbmiUtils) package provides
three key functions for this workflow:

- [`validate_data()`](https://openpharma.github.io/rbmiUtils/reference/validate_data.md):
  Pre-flight validation to catch common data issues
- [`prepare_data_ice()`](https://openpharma.github.io/rbmiUtils/reference/prepare_data_ice.md):
  Build intercurrent event data from flag columns
- [`summarise_missingness()`](https://openpharma.github.io/rbmiUtils/reference/summarise_missingness.md):
  Understand missing data patterns

Using these functions helps ensure your imputation will run successfully
and gives you insight into the structure of your missing data.

## Setup

``` r
library(dplyr)
library(rbmi)
library(rbmiUtils)
```

## Example Data

We’ll create a small example dataset to demonstrate the functions:

``` r
set.seed(42)

dat <- data.frame(
 USUBJID = factor(rep(paste0("SUBJ-", 1:20), each = 4)),
 AVISIT = factor(
   rep(c("Week 4", "Week 8", "Week 12", "Week 16"), 20),
   levels = c("Week 4", "Week 8", "Week 12", "Week 16")
 ),
 TRT = factor(rep(c("Placebo", "Drug A"), each = 40)),
 BASE = rep(round(rnorm(20, 50, 10), 1), each = 4),
 STRATA = factor(rep(sample(c("Low", "High"), 20, replace = TRUE), each = 4))
)

# Generate CHG with some missing values
dat$CHG <- round(rnorm(80, mean = -2, sd = 3), 1)

# Create missing data patterns:
# - Subjects 3, 8: monotone dropout at Week 12
# - Subject 15: intermittent missing at Week 8
# - Subject 18: monotone dropout at Week 16
dat$CHG[dat$USUBJID == "SUBJ-3" & dat$AVISIT %in% c("Week 12", "Week 16")] <- NA
dat$CHG[dat$USUBJID == "SUBJ-8" & dat$AVISIT %in% c("Week 12", "Week 16")] <- NA
dat$CHG[dat$USUBJID == "SUBJ-15" & dat$AVISIT == "Week 8"] <- NA
dat$CHG[dat$USUBJID == "SUBJ-18" & dat$AVISIT == "Week 16"] <- NA

# Add discontinuation flag
dat$DISCFL <- ifelse(
 dat$USUBJID %in% c("SUBJ-3", "SUBJ-8") & dat$AVISIT == "Week 12",
 "Y",
 ifelse(
   dat$USUBJID == "SUBJ-18" & dat$AVISIT == "Week 16",
   "Y",
   "N"
 )
)

head(dat, 12)
#>    USUBJID  AVISIT     TRT BASE STRATA  CHG DISCFL
#> 1   SUBJ-1  Week 4 Placebo 63.7    Low -0.6      N
#> 2   SUBJ-1  Week 8 Placebo 63.7    Low  0.1      N
#> 3   SUBJ-1 Week 12 Placebo 63.7    Low  1.1      N
#> 4   SUBJ-1 Week 16 Placebo 63.7    Low -3.8      N
#> 5   SUBJ-2  Week 4 Placebo 44.4    Low -0.5      N
#> 6   SUBJ-2  Week 8 Placebo 44.4    Low -7.2      N
#> 7   SUBJ-2 Week 12 Placebo 44.4    Low -4.4      N
#> 8   SUBJ-2 Week 16 Placebo 44.4    Low -4.6      N
#> 9   SUBJ-3  Week 4 Placebo 53.6   High -9.2      N
#> 10  SUBJ-3  Week 8 Placebo 53.6   High -1.9      N
#> 11  SUBJ-3 Week 12 Placebo 53.6   High   NA      Y
#> 12  SUBJ-3 Week 16 Placebo 53.6   High   NA      N
```

## Define Variables

``` r
vars <- set_vars(
 subjid = "USUBJID",
 visit = "AVISIT",
 group = "TRT",
 outcome = "CHG",
 covariates = c("BASE", "STRATA"),
 strategy = "STRATEGY"
)
```

## Validating Data

The
[`validate_data()`](https://openpharma.github.io/rbmiUtils/reference/validate_data.md)
function performs comprehensive checks on your data before imputation:

``` r
# This will pass validation
validate_data(dat, vars)
```

The function checks:

- Data is a data.frame
- All required columns exist (subjid, visit, group, outcome, covariates)
- Factor columns are properly typed
- Outcome column is numeric
- Covariates have no missing values
- No duplicate subject-visit combinations
- If `data_ice` is provided: valid subjects, visits, and strategies

### Catching Validation Errors

Here’s an example of how validation catches issues:

``` r
# Create problematic data
bad_dat <- dat
bad_dat$TRT <- as.character(bad_dat$TRT)  # Should be factor
bad_dat$BASE[1] <- NA  # Covariate with missing value

# This will report all issues at once
tryCatch(
 validate_data(bad_dat, vars),
 error = function(e) cat(e$message)
)
#> Warning: 1 column is character instead of factor.
#> ℹ Column: TRT.
#> ℹ `rbmi::draws()` will auto-coerce, but explicit conversion gives you control
#>   over level ordering.
#> ℹ Example: `data$TRT <- factor(data$TRT)`
#> Data validation failed.
```

## Summarising Missing Data

Before imputation, it’s important to understand your missing data
patterns:

``` r
miss <- summarise_missingness(dat, vars)
```

### Missing by Visit

``` r
print(miss$by_visit)
#> # A tibble: 8 × 5
#>   visit   group       n n_miss pct_miss
#>   <fct>   <fct>   <int>  <int>    <dbl>
#> 1 Week 4  Drug A     10      0        0
#> 2 Week 4  Placebo    10      0        0
#> 3 Week 8  Drug A     10      1       10
#> 4 Week 8  Placebo    10      0        0
#> 5 Week 12 Drug A     10      0        0
#> 6 Week 12 Placebo    10      2       20
#> 7 Week 16 Drug A     10      1       10
#> 8 Week 16 Placebo    10      2       20
```

### Subject Patterns

``` r
print(miss$patterns)
#> # A tibble: 20 × 4
#>    USUBJID TRT     pattern      dropout_visit
#>    <fct>   <chr>   <chr>        <chr>        
#>  1 SUBJ-1  Placebo complete     NA           
#>  2 SUBJ-2  Placebo complete     NA           
#>  3 SUBJ-3  Placebo monotone     Week 12      
#>  4 SUBJ-4  Placebo complete     NA           
#>  5 SUBJ-5  Placebo complete     NA           
#>  6 SUBJ-6  Placebo complete     NA           
#>  7 SUBJ-7  Placebo complete     NA           
#>  8 SUBJ-8  Placebo monotone     Week 12      
#>  9 SUBJ-9  Placebo complete     NA           
#> 10 SUBJ-10 Placebo complete     NA           
#> 11 SUBJ-11 Drug A  complete     NA           
#> 12 SUBJ-12 Drug A  complete     NA           
#> 13 SUBJ-13 Drug A  complete     NA           
#> 14 SUBJ-14 Drug A  complete     NA           
#> 15 SUBJ-15 Drug A  intermittent NA           
#> 16 SUBJ-16 Drug A  complete     NA           
#> 17 SUBJ-17 Drug A  complete     NA           
#> 18 SUBJ-18 Drug A  monotone     Week 16      
#> 19 SUBJ-19 Drug A  complete     NA           
#> 20 SUBJ-20 Drug A  complete     NA
```

### Summary by Treatment Group

``` r
print(miss$summary)
#> # A tibble: 2 × 5
#>   group   n_subjects n_complete n_monotone n_intermittent
#>   <chr>        <int>      <int>      <int>          <int>
#> 1 Drug A          10          8          1              1
#> 2 Placebo         10          8          2              0
```

The three pattern types are:

- **complete**: No missing outcome values
- **monotone**: Once a value is missing, all subsequent visits are also
  missing (dropout)
- **intermittent**: Missing values with observed values both before and
  after

## Preparing ICE Data

When subjects discontinue treatment, you may want to apply
reference-based imputation strategies (see the [`{rbmi}`
documentation](https://cran.r-project.org/web/packages/rbmi/vignettes/quickstart.html)
for details on intercurrent event handling). The
[`prepare_data_ice()`](https://openpharma.github.io/rbmiUtils/reference/prepare_data_ice.md)
function builds the required `data_ice` data.frame from a
discontinuation flag:

``` r
data_ice <- prepare_data_ice(
 data = dat,
 vars = vars,
 ice_col = "DISCFL",
 strategy = "JR"  # Jump to Reference
)

print(data_ice)
#>   USUBJID  AVISIT STRATEGY
#> 1 SUBJ-18 Week 16       JR
#> 2  SUBJ-3 Week 12       JR
#> 3  SUBJ-8 Week 12       JR
```

The function:

- Identifies subjects with ICE flags (`"Y"`, `TRUE`, or `1`)
- Takes the first flagged visit per subject
- Assigns the specified imputation strategy

Available strategies are:

- `"MAR"`: Missing at Random
- `"CR"`: Copy Reference
- `"JR"`: Jump to Reference
- `"CIR"`: Copy Increment from Reference
- `"LMCF"`: Last Mean Carried Forward

## Complete Workflow

Here’s how these functions fit into a typical
[`{rbmi}`](https://cran.r-project.org/package=rbmi) workflow:

``` r
library(rbmi)
library(rbmiUtils)

# 1. Validate data
validate_data(dat, vars)

# 2. Understand missing patterns
miss <- summarise_missingness(dat, vars)
print(miss$summary)

# 3. Prepare ICE data if needed
data_ice <- prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "JR")

# 4. Define method
method <- method_bayes(
 n_samples = 100,
 control = control_bayes(warmup = 200, thin = 2)
)

# 5. Run imputation
draws_obj <- draws(
 data = dat,
 vars = vars,
 data_ice = data_ice,
 method = method
)

# 6. Continue with impute() and analyse()
```

## Summary

The data preparation functions in
[rbmiUtils](https://github.com/openpharma/rbmiUtils) help you:

1.  **Catch issues early** with
    [`validate_data()`](https://openpharma.github.io/rbmiUtils/reference/validate_data.md)
    before running time-consuming imputations
2.  **Understand your data** with
    [`summarise_missingness()`](https://openpharma.github.io/rbmiUtils/reference/summarise_missingness.md)
    to characterize missing data patterns
3.  **Simplify ICE handling** with
    [`prepare_data_ice()`](https://openpharma.github.io/rbmiUtils/reference/prepare_data_ice.md)
    to build `data_ice` from flag columns

These utilities complement the core
[`{rbmi}`](https://cran.r-project.org/package=rbmi) package and support
reproducible, well-documented analysis workflows.

After data preparation, see
[`vignette('pipeline')`](https://openpharma.github.io/rbmiUtils/articles/pipeline.md)
for the complete analysis workflow from imputation through to regulatory
tables.
