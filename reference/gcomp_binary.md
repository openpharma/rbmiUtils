# Utility function for Generalized G-computation for Binary Outcomes

Wrapper function for targeting a marginal treatment effect using
g-computation using the beeca package. Intended for binary endpoints.

## Usage

``` r
gcomp_binary(
  data,
  outcome = "CRIT1FLN",
  treatment = "TRT",
  covariates = c("BASE", "STRATA", "REGION"),
  reference = "Placebo",
  contrast = "diff",
  method = "Ge",
  type = "HC0",
  ...
)
```

## Arguments

- data:

  A data.frame containing the analysis dataset.

- outcome:

  Name of the binary outcome variable (as string).

- treatment:

  Name of the treatment variable (as string).

- covariates:

  Character vector of covariate names to adjust for.

- reference:

  Reference level for the treatment variable (default: "Placebo").

- contrast:

  Type of contrast to compute (default: "diff").

- method:

  Marginal estimation method for variance (default: "Ge").

- type:

  Variance estimator type (default: "HC0").

- ...:

  Additional arguments passed to
  [`beeca::get_marginal_effect()`](https://openpharma.github.io/beeca/reference/get_marginal_effect.html).

## Value

A named list with treatment effect estimate, standard error, and degrees
of freedom (if applicable).

## Examples

``` r
# Load required packages
library(rbmiUtils)
library(beeca)      # for get_marginal_effect()
library(dplyr)
# Load example data
data("ADMI")
# Ensure correct factor levels
ADMI <- ADMI |>
  mutate(
    TRT = factor(TRT, levels = c("Placebo", "Drug A")),
    STRATA = factor(STRATA),
    REGION = factor(REGION)
  )
# Apply g-computation for binary responder
result <- gcomp_binary(
  data = ADMI,
  outcome = "CRIT1FLN",
  treatment = "TRT",
  covariates = c("BASE", "STRATA", "REGION"),
  reference = "Placebo",
  contrast = "diff",
  method = "Ge",    # from beeca: GEE robust sandwich estimator
  type = "HC0"      # from beeca: heteroskedasticity-consistent SE
)

# Print results
print(result)
#> $trt
#> $trt$est
#> [1] -0.0632916
#> 
#> $trt$se
#> [1] 0.001189759
#> 
#> $trt$df
#> [1] NA
#> 
#> 
```
