# Apply Analysis Function to Multiple Imputed Datasets

This function applies an analysis function (e.g., ANCOVA) to imputed
datasets and stores the results for later pooling. It is designed to
work with multiple imputed datasets and apply a given analysis function
to each imputation iteration.

## Usage

``` r
analyse_mi_data(
  data = NULL,
  vars = NULL,
  method = NULL,
  fun = rbmi::ancova,
  delta = NULL,
  ...
)
```

## Arguments

- data:

  A data frame containing the imputed datasets. The data frame should
  include a variable (e.g., `IMPID`) that identifies distinct imputation
  iterations. Typically obtained from
  [`get_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/get_imputed_data.md)
  or
  [`expand_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/expand_imputed_data.md).

- vars:

  A list specifying key variables used in the analysis (e.g., `subjid`,
  `visit`, `group`, `outcome`). Created using
  [`rbmi::set_vars()`](https://openpharma.github.io/rbmi/latest-tag/reference/set_vars.html).
  Required.

- method:

  A method object specifying the imputation method used (e.g., Bayesian
  imputation). Created using
  [`rbmi::method_bayes()`](https://openpharma.github.io/rbmi/latest-tag/reference/method.html),
  [`rbmi::method_approxbayes()`](https://openpharma.github.io/rbmi/latest-tag/reference/method.html),
  or
  [`rbmi::method_condmean()`](https://openpharma.github.io/rbmi/latest-tag/reference/method.html).
  Required.

- fun:

  A function that will be applied to each imputed dataset. Defaults to
  [rbmi::ancova](https://openpharma.github.io/rbmi/latest-tag/reference/ancova.html).
  Other options include
  [`gcomp_responder_multi()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_responder_multi.md)
  for binary outcomes. Must be a valid analysis function.

- delta:

  A `data.frame` used for delta adjustments, or `NULL` if no delta
  adjustments are needed. Defaults to `NULL`. Must contain columns
  matching `vars$subjid`, `vars$visit`, `vars$group`, and a `delta`
  column.

- ...:

  Additional arguments passed to the analysis function `fun`.

## Value

An object of class `analysis` containing the results from applying the
analysis function to each imputed dataset. Pass this to
[`rbmi::pool()`](https://openpharma.github.io/rbmi/latest-tag/reference/pool.html)
to obtain pooled estimates.

## Details

The function loops through distinct imputation datasets (identified by
`IMPID`), applies the provided analysis function `fun`, and stores the
results for later pooling. If a `delta` dataset is provided, it will be
merged with the imputed data to apply the specified delta adjustment
before analysis.

**Workflow:**

1.  Prepare imputed data using
    [`get_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/get_imputed_data.md)
    or
    [`expand_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/expand_imputed_data.md)

2.  Define variables using
    [`rbmi::set_vars()`](https://openpharma.github.io/rbmi/latest-tag/reference/set_vars.html)

3.  Call `analyse_mi_data()` to apply analysis to each imputation

4.  Pool results using
    [`rbmi::pool()`](https://openpharma.github.io/rbmi/latest-tag/reference/pool.html)

5.  Tidy results using
    [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)

## See also

- [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
  to format pooled results for publication

- [`get_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/get_imputed_data.md)
  to extract imputed datasets from rbmi objects

- [`expand_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/expand_imputed_data.md)
  to reconstruct full imputed data from reduced form

- [`gcomp_responder_multi()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_responder_multi.md)
  for binary outcome analysis

- [`validate_data()`](https://openpharma.github.io/rbmiUtils/reference/validate_data.md)
  to check data before imputation

## Examples

``` r
# Example usage with an ANCOVA function
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(rbmi)
library(rbmiUtils)
set.seed(123)
data("ADMI")

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
 n_samples = 20,
 control = rbmi::control_bayes(
   warmup = 20,
   thin = 1
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
#> Warning: Data contains 100 imputations but method expects 20. Using first 20 imputations.
```
