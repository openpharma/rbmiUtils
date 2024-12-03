
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbmiUtils

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of rbmiUtils is to provide additional functionality to {rbmi}
for clinical trial analysis and reporting. This package is currently
under development and is not yet available on CRAN.

## Local installation (for beta-testing only)

You can install the latest version of **rbmiUtils** from GitHub:

``` r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("openpharma/rbmiUtils")
```

After the official release, the package installation will differ.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# Example usage:
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(rbmi)
library(rbmiUtils)

data("ADMI")
N_IMPUTATIONS <- 100
BURN_IN <- 200
BURN_BETWEEN <- 5

# Convert key columns to factors
ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
ADMI$USUBJID <- factor(ADMI$USUBJID)
ADMI$AVISIT <- factor(ADMI$AVISIT)

# Define key variables for ANCOVA analysis
 vars <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CHG",
  covariates = c("BASE", "STRATA", "REGION")  # Covariates for adjustment
 )

# Specify the imputation method (Bayesian) - need for pool step
method <- rbmi::method_bayes(
  n_samples = N_IMPUTATIONS,
  burn_in = BURN_IN,
  burn_between = BURN_BETWEEN
)

# Perform ANCOVA Analysis on Each Imputed Dataset
ana_obj_ancova <- analyse_mi_data(
  data = ADMI,
  vars = vars,
  method = method,
  fun = ancova,  # Apply ANCOVA
  delta = NULL   # No sensitivity analysis adjustment
)

pool_obj_ancova <- pool(ana_obj_ancova)
tidy_df <- tidy_pool_obj(pool_obj_ancova)

# Print tidy data frames
print(tidy_df)
#> # A tibble: 6 × 10
#>   parameter       description visit parameter_type lsm_type     est    se    lci
#>   <chr>           <chr>       <chr> <chr>          <chr>      <dbl> <dbl>  <dbl>
#> 1 trt_Week 24     Treatment … Week… trt            <NA>     -2.17   0.182 -2.53 
#> 2 lsm_ref_Week 24 Least Squa… Week… lsm            ref       0.0782 0.131 -0.179
#> 3 lsm_alt_Week 24 Least Squa… Week… lsm            alt      -2.09   0.126 -2.34 
#> 4 trt_Week 48     Treatment … Week… trt            <NA>     -3.81   0.256 -4.31 
#> 5 lsm_ref_Week 48 Least Squa… Week… lsm            ref       0.0481 0.185 -0.316
#> 6 lsm_alt_Week 48 Least Squa… Week… lsm            alt      -3.76   0.176 -4.11 
#> # ℹ 2 more variables: uci <dbl>, pval <dbl>
```
