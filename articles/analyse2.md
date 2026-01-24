# Storing and Analyzing Imputed Data with rbmiUtils

## Introduction

This vignette demonstrates how to:

- Perform multiple imputation using the
  [rbmi](https://openpharma.github.io/rbmi/) package.

- Store and modify the imputed data using
  [rbmiUtils](https://github.com/openpharma/rbmiUtils).

- Analyze the imputed data using:

  - A standard ANCOVA on a continuous endpoint (`CHG`)
  - A binary responder analysis on `CRIT1FLN` using
    [beeca](https://openpharma.github.io/beeca/)

This pattern enables reproducible workflows where imputation and
analysis can be separated and revisited independently.

## Statistical Context

This approach applies **Rubin’s Rules** for inference after multiple
imputation:

> We fit a model to each imputed dataset, dervive a response variable on
> the CHG score, extract marginal effects or other statistics of
> interest, and combine the results into a single inference using
> Rubin’s combining rules.

------------------------------------------------------------------------

## Step 1: Setup and Data Preparation

``` r
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(rbmi)
library(beeca)
library(rbmiUtils)
```

``` r
set.seed(1974)
```

``` r
data("ADEFF")

ADEFF <- ADEFF %>%
  mutate(
    TRT = factor(TRT01P, levels = c("Placebo", "Drug A")),
    USUBJID = factor(USUBJID),
    AVISIT = factor(AVISIT)
  )
```

## Step 2: Define Imputation Model

``` r
vars <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CHG",
  covariates = c("BASE", "STRATA", "REGION")
)
```

``` r
method <- method_bayes(
  n_samples = 100,
  control = control_bayes(warmup = 200, thin = 2)
)
```

``` r
dat <- ADEFF %>%
  select(USUBJID, STRATA, REGION, REGIONC, TRT, BASE, CHG, AVISIT)

draws_obj <- draws(data = dat, vars = vars, method = method)
#> Trying to compile a simple C file
#> Running /opt/R/4.5.2/lib/R/bin/R CMD SHLIB foo.c
#> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
#> gcc -std=gnu2x -I"/opt/R/4.5.2/lib/R/include" -DNDEBUG   -I"/home/runner/work/_temp/Library/Rcpp/include/"  -I"/home/runner/work/_temp/Library/RcppEigen/include/"  -I"/home/runner/work/_temp/Library/RcppEigen/include/unsupported"  -I"/home/runner/work/_temp/Library/BH/include" -I"/home/runner/work/_temp/Library/StanHeaders/include/src/"  -I"/home/runner/work/_temp/Library/StanHeaders/include/"  -I"/home/runner/work/_temp/Library/RcppParallel/include/"  -I"/home/runner/work/_temp/Library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -D_HAS_AUTO_PTR_ETC=0  -include '/home/runner/work/_temp/Library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/usr/local/include    -fpic  -g -O2  -c foo.c -o foo.o
#> In file included from /home/runner/work/_temp/Library/RcppEigen/include/Eigen/Core:19,
#>                  from /home/runner/work/_temp/Library/RcppEigen/include/Eigen/Dense:1,
#>                  from /home/runner/work/_temp/Library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22,
#>                  from <command-line>:
#> /home/runner/work/_temp/Library/RcppEigen/include/Eigen/src/Core/util/Macros.h:679:10: fatal error: cmath: No such file or directory
#>   679 | #include <cmath>
#>       |          ^~~~~~~
#> compilation terminated.
#> make: *** [/opt/R/4.5.2/lib/R/etc/Makeconf:202: foo.o] Error 1
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
#> Chain 1:  Elapsed Time: 0.633 seconds (Warm-up)
#> Chain 1:                0.524 seconds (Sampling)
#> Chain 1:                1.157 seconds (Total)
#> Chain 1:

impute_obj <- impute(draws_obj, references = c("Placebo" = "Placebo", "Drug A" = "Placebo"))

ADMI <- get_imputed_data(impute_obj)
```

## Step 3: Add Responder Variables

``` r
ADMI <- ADMI %>%
  mutate(
    CRIT1FLN = ifelse(CHG > 3, 1, 0),
    CRIT1FL = ifelse(CRIT1FLN == 1, "Y", "N"),
    CRIT = "CHG > 3"
  )
```

## Step 4: Continuous Endpoint Analysis (CHG)

``` r
ana_obj_ancova <- analyse_mi_data(
  data = ADMI,
  vars = vars,
  method = method,
  fun = ancova
)
```

``` r
pool_obj_ancova <- pool(ana_obj_ancova)
print(pool_obj_ancova)
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
#>   ========================================================
#>       parameter      est     se     lci     uci     pval  
#>   --------------------------------------------------------
#>      trt_Week 24    -2.177  0.182  -2.535  -1.819  <0.001 
#>    lsm_ref_Week 24  0.077   0.131  -0.181  0.334   0.559  
#>    lsm_alt_Week 24   -2.1   0.126  -2.347  -1.854  <0.001 
#>      trt_Week 48    -3.806  0.256  -4.309  -3.303  <0.001 
#>    lsm_ref_Week 48  0.044   0.185  -0.32   0.407   0.812  
#>    lsm_alt_Week 48  -3.762  0.175  -4.107  -3.417  <0.001 
#>   --------------------------------------------------------
```

``` r
tidy_pool_obj(pool_obj_ancova)
#> # A tibble: 6 × 10
#>   parameter       description visit parameter_type lsm_type     est    se    lci
#>   <chr>           <chr>       <chr> <chr>          <chr>      <dbl> <dbl>  <dbl>
#> 1 trt_Week 24     Treatment … Week… trt            NA       -2.18   0.182 -2.54 
#> 2 lsm_ref_Week 24 Least Squa… Week… lsm            ref       0.0765 0.131 -0.181
#> 3 lsm_alt_Week 24 Least Squa… Week… lsm            alt      -2.10   0.126 -2.35 
#> 4 trt_Week 48     Treatment … Week… trt            NA       -3.81   0.256 -4.31 
#> 5 lsm_ref_Week 48 Least Squa… Week… lsm            ref       0.0440 0.185 -0.320
#> 6 lsm_alt_Week 48 Least Squa… Week… lsm            alt      -3.76   0.175 -4.11 
#> # ℹ 2 more variables: uci <dbl>, pval <dbl>
```

## Step 5: Responder Endpoint Analysis (CRIT1FLN)

### Define Analysis Function

``` r
gcomp_responder <- function(data, ...) {
  model <- glm(CRIT1FLN ~ TRT + BASE + STRATA + REGION, data = data, family = binomial)

  marginal_fit <- get_marginal_effect(
    model,
    trt = "TRT",
    method = "Ge",
    type = "HC0",
    contrast = "diff",
    reference = "Placebo"
  )

  res <- marginal_fit$marginal_results
  list(
    trt = list(
      est = res[res$STAT == "diff", "STATVAL"][[1]],
      se = res[res$STAT == "diff_se", "STATVAL"][[1]],
      df = NA
    )
  )
}
```

### Define Variables and Run Analysis

``` r
vars_binary <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CRIT1FLN",
  covariates = c("BASE", "STRATA", "REGION")
)
```

``` r
ana_obj_prop <- analyse_mi_data(
  data = ADMI,
  vars = vars_binary,
  method = method,
  fun = gcomp_responder
)
```

``` r
pool_obj_prop <- pool(ana_obj_prop)
print(pool_obj_prop)
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
#>   ==================================================
#>    parameter   est     se     lci     uci     pval  
#>   --------------------------------------------------
#>       trt     -0.063  0.012  -0.087  -0.039  <0.001 
#>   --------------------------------------------------
```

## Final Notes

- The `ADMI` object can be saved for later reuse.
- Analyses can be modularly applied using custom functions.
- The tidy output from
  [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
  is helpful for reporting and review.
