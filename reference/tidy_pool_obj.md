# Tidy and Annotate a Pooled Object for Publication

This function processes a pooled analysis object of class `pool` into a
tidy tibble format. It adds contextual information, such as whether a
parameter is a treatment comparison or a least squares mean, dynamically
identifies visit names from the `parameter` column, and provides
additional columns for parameter type, least squares mean type, and
visit.

## Usage

``` r
tidy_pool_obj(pool_obj)
```

## Arguments

- pool_obj:

  A pooled analysis object of class `pool`, typically obtained from
  [`rbmi::pool()`](https://openpharma.github.io/rbmi/latest-tag/reference/pool.html)
  after calling
  [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md).

## Value

A tibble containing the processed pooled analysis results with the
following columns:

- parameter:

  Original parameter name from the pooled object

- description:

  Human-readable description of the parameter

- visit:

  Visit name extracted from parameter (if applicable)

- parameter_type:

  Either "trt" (treatment comparison) or "lsm" (least squares mean)

- lsm_type:

  For LSM parameters: "ref" (reference) or "alt" (alternative)

- est:

  Point estimate

- se:

  Standard error

- lci:

  Lower confidence interval

- uci:

  Upper confidence interval

- pval:

  P-value

## Details

The function dynamically processes the `parameter` column by separating
it into components (e.g., type of estimate, reference vs. alternative
arm, and visit), and provides informative descriptions in the output.

**Workflow:**

1.  Prepare data and run imputation with rbmi

2.  Analyse with
    [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md)

3.  Pool with
    [`rbmi::pool()`](https://openpharma.github.io/rbmi/latest-tag/reference/pool.html)

4.  Tidy with `tidy_pool_obj()` for publication-ready output

## See also

- [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md)
  to analyse imputed datasets

- [`format_results()`](https://openpharma.github.io/rbmiUtils/reference/format_results.md)
  for additional formatting options

## Examples

``` r
# Example usage:
library(dplyr)
library(rbmi)

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
  control = rbmi::control_bayes(
    warmup = BURN_IN,
    thin = BURN_BETWEEN
    )
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
#> 1 trt_Week 24     Treatment … Week… trt            NA       -2.17   0.182 -2.53 
#> 2 lsm_ref_Week 24 Least Squa… Week… lsm            ref       0.0782 0.131 -0.179
#> 3 lsm_alt_Week 24 Least Squa… Week… lsm            alt      -2.09   0.126 -2.34 
#> 4 trt_Week 48     Treatment … Week… trt            NA       -3.81   0.256 -4.31 
#> 5 lsm_ref_Week 48 Least Squa… Week… lsm            ref       0.0481 0.185 -0.316
#> 6 lsm_alt_Week 48 Least Squa… Week… lsm            alt      -3.76   0.176 -4.11 
#> # ℹ 2 more variables: uci <dbl>, pval <dbl>
```
