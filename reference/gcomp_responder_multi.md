# G-computation for a Binary Outcome at Multiple Visits

Applies
[`gcomp_responder()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_responder.md)
separately for each unique visit in the data.

## Usage

``` r
gcomp_responder_multi(data, vars, reference_levels = NULL, ...)
```

## Arguments

- data:

  A data.frame containing multiple visits.

- vars:

  A list specifying analysis variables.

- reference_levels:

  Optional reference level for the treatment variable.

- ...:

  Additional arguments passed to
  [`gcomp_responder()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_responder.md).

## Value

A named list of estimates for each visit and treatment group.

## Examples

``` r
# \donttest{
library(dplyr)
library(rbmi)
library(rbmiUtils)

data("ADMI")

ADMI <- ADMI |>
  mutate(
    TRT = factor(TRT, levels = c("Placebo", "Drug A")),
    STRATA = factor(STRATA),
    REGION = factor(REGION)
  )

# Note: method must match the original used for imputation
method <- method_bayes(
  n_samples = 100,
  control = control_bayes(warmup = 20, thin = 2)
)

vars_binary <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CRIT1FLN",
  covariates = c("BASE", "STRATA", "REGION")
)

ana_obj_prop <- analyse_mi_data(
  data = ADMI,
  vars = vars_binary,
  method = method,
  fun = gcomp_responder_multi,
  reference_levels = "Placebo",
  contrast = "diff",
  var_method = "Ge",
  type = "HC0"
)

pool(ana_obj_prop)
#> 
#> Pool Object
#> -----------
#> Number of Results Combined: 100
#> Method: rubin
#> Confidence Level: 0.95
#> Alternative: two.sided
#> 
#> Results:
#> 
#>   ===================================================================
#>            parameter            est     se     lci     uci     pval  
#>   -------------------------------------------------------------------
#>    trt_Drug A-Placebo_Week 24  -0.031  0.012  -0.053  -0.008  0.007  
#>        lsm_Drug A_Week 24        0     0.001  -0.001  0.002   0.921  
#>       lsm_Placebo_Week 24      0.031   0.011  0.008   0.053   0.007  
#>    trt_Drug A-Placebo_Week 48  -0.096  0.021  -0.137  -0.054  <0.001 
#>        lsm_Drug A_Week 48      0.007   0.005  -0.003  0.017    0.15  
#>       lsm_Placebo_Week 48      0.103   0.021  0.063   0.143   <0.001 
#>   -------------------------------------------------------------------
#> 
# }
```
