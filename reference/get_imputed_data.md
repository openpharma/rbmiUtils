# Get Imputed Data Sets as a data frame

This function takes an imputed dataset and a mapping variable to return
a dataset with the original IDs mapped back and renamed appropriately.

## Usage

``` r
get_imputed_data(impute_obj)
```

## Arguments

- impute_obj:

  The imputation object from which the imputed datasets are extracted.

## Value

A data frame with the original subject IDs mapped and renamed.

## Examples

``` r
# \donttest{
library(dplyr)
library(rbmi)
library(rbmiUtils)

set.seed(1974)
# Load example dataset
data("ADEFF")

# Prepare data
ADEFF <- ADEFF |>
  mutate(
    TRT = factor(TRT01P, levels = c("Placebo", "Drug A")),
    USUBJID = factor(USUBJID),
    AVISIT = factor(AVISIT)
  )

# Define variables for imputation
vars <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CHG",
  covariates = c("BASE", "STRATA", "REGION")
)

# Define Bayesian imputation method
method <- method_bayes(
  n_samples = 100,
  control = control_bayes(warmup = 200, thin = 2)
)

# Generate draws and perform imputation
draws_obj <- draws(data = ADEFF, vars = vars, method = method)
#> 
#> SAMPLING FOR MODEL 'rbmi_MMRM_us_default' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 0.000419 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 4.19 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:   1 / 400 [  0%]  (Warmup)
#> Chain 1: Iteration:  40 / 400 [ 10%]  (Warmup)
#> Chain 1: Iteration:  80 / 400 [ 20%]  (Warmup)
#> Chain 1: Iteration: 120 / 400 [ 30%]  (Warmup)
#> Chain 1: Iteration: 160 / 400 [ 40%]  (Warmup)
#> Chain 1: Iteration: 200 / 400 [ 50%]  (Warmup)
#> Chain 1: Iteration: 201 / 400 [ 50%]  (Sampling)
#> Chain 1: Iteration: 240 / 400 [ 60%]  (Sampling)
#> Chain 1: Iteration: 280 / 400 [ 70%]  (Sampling)
#> Chain 1: Iteration: 320 / 400 [ 80%]  (Sampling)
#> Chain 1: Iteration: 360 / 400 [ 90%]  (Sampling)
#> Chain 1: Iteration: 400 / 400 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.635 seconds (Warm-up)
#> Chain 1:                0.522 seconds (Sampling)
#> Chain 1:                1.157 seconds (Total)
#> Chain 1: 
impute_obj <- impute(draws_obj,
  references = c("Placebo" = "Placebo", "Drug A" = "Placebo"))

# Extract imputed data with original subject IDs
admi <- get_imputed_data(impute_obj)
head(admi)
#>   IMPID internal_id STRATA        REGION REGIONC TRT01P BASE  AVISIT AVAL
#> 1     1    new_pt_1      A North America       1 Drug A   16 Week 24   12
#> 2     1    new_pt_1      A North America       1 Drug A   16 Week 48   13
#> 3     1    new_pt_2      A          Asia       4 Drug A   10 Week 24    7
#> 4     1    new_pt_2      A          Asia       4 Drug A   10 Week 48    5
#> 5     1    new_pt_3      A South America       2 Drug A   12 Week 24    9
#> 6     1    new_pt_3      A South America       2 Drug A   12 Week 48   10
#>          PARAM CHG    TRT USUBJID
#> 1 ESSDAI score  -4 Drug A   ID001
#> 2 ESSDAI score  -3 Drug A   ID001
#> 3 ESSDAI score  -3 Drug A   ID002
#> 4 ESSDAI score  -5 Drug A   ID002
#> 5 ESSDAI score  -3 Drug A   ID003
#> 6 ESSDAI score  -2 Drug A   ID003
# }
```
