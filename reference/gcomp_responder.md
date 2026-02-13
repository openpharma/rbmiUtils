# G-computation Analysis for a Single Visit

Performs logistic regression and estimates marginal effects for binary
outcomes.

## Usage

``` r
gcomp_responder(
  data,
  vars,
  reference_levels = NULL,
  var_method = "Ge",
  type = "HC0",
  contrast = "diff"
)
```

## Arguments

- data:

  A data.frame with one visit of data.

- vars:

  A list containing `group`, `outcome`, `covariates`, and `visit`.

- reference_levels:

  Optional vector specifying reference level(s) of the treatment factor.

- var_method:

  Marginal variance estimation method (default: "Ge").

- type:

  Type of robust variance estimator (default: "HC0").

- contrast:

  Type of contrast to compute (default: "diff").

## Value

A named list containing estimates and standard errors for treatment
comparisons and within-arm means.

## Examples

``` r
# \donttest{
library(dplyr)
library(rbmi)
library(rbmiUtils)

data("ADMI")

# Prepare data for a single visit
ADMI <- ADMI |>
  mutate(
    TRT = factor(TRT, levels = c("Placebo", "Drug A")),
    STRATA = factor(STRATA),
    REGION = factor(REGION)
  )

dat_single <- ADMI |>
  filter(AVISIT == "Week 24")

vars <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CRIT1FLN",
  covariates = c("BASE", "STRATA", "REGION")
)

result <- gcomp_responder(
  data = dat_single,
  vars = vars,
  reference_levels = "Placebo"
)
#> Warning: `extract_covariates2()` was deprecated in rbmiUtils 0.2.0.
#> ℹ Internal helper will be removed in a future version.
#> ℹ The deprecated feature was likely used in the rbmiUtils package.
#>   Please report the issue at <https://github.com/openpharma/rbmiUtils/issues>.
#> Warning: `as_simple_formula2()` was deprecated in rbmiUtils 0.2.0.
#> ℹ Internal helper will be removed in a future version.
#> ℹ The deprecated feature was likely used in the rbmiUtils package.
#>   Please report the issue at <https://github.com/openpharma/rbmiUtils/issues>.

print(result)
#> $`trt_Drug A-Placebo`
#> $`trt_Drug A-Placebo`$est
#> [1] -0.03081266
#> 
#> $`trt_Drug A-Placebo`$se
#> [1] 0.001122321
#> 
#> $`trt_Drug A-Placebo`$df
#> [1] NA
#> 
#> 
#> $`lsm_Drug A`
#> $`lsm_Drug A`$est
#> [1] 7.701861e-05
#> 
#> $`lsm_Drug A`$se
#> [1] 5.448088e-05
#> 
#> $`lsm_Drug A`$df
#> [1] NA
#> 
#> 
#> $lsm_Placebo
#> $lsm_Placebo$est
#> [1] 0.03088968
#> 
#> $lsm_Placebo$se
#> [1] 0.001120489
#> 
#> $lsm_Placebo$df
#> [1] NA
#> 
#> 
# }
```
