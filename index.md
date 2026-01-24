# rbmiUtils [![rbmiUtils website](reference/figures/rbmiUtils.png)](https://openpharma.github.io/rbmiUtils/)

`rbmiUtils` extends the functionality of
[`rbmi`](https://github.com/insightsengineering/rbmi) to support more
streamlined workflows for multiple imputation in clinical trials. It is
designed to simplify key tasks such as analysis execution, pooling,
result tidying, and imputed data handling.

## Table of Contents

- [Installation](#installation)
- [Example](#example)
- [Dataset](#dataset)
- [Utilities](#utilities)
- [Development Status](#development-status)

## Installation

You can install the package from cran or the development version of
`rbmiUtils` from GitHub:

| Type        | Source | Command                                           |
|-------------|--------|---------------------------------------------------|
| Release     | CRAN   | `install.packages("rbmiUtils")`                   |
| Development | GitHub | `remotes::install_github("openpharma/rbmiUtils")` |

## Example

This example shows how to run a covariate-adjusted ANCOVA on imputed
datasets using Bayesian multiple imputation:

``` r
library(dplyr)
library(rbmi)
library(rbmiUtils)

data("ADMI")

# Setup
N_IMPUTATIONS <- 100
WARMUP <- 200
THIN <- 5

# Preprocessing
ADMI <- ADMI %>%
  mutate(
    TRT = factor(TRT, levels = c("Placebo", "Drug A")),
    USUBJID = factor(USUBJID),
    AVISIT = factor(AVISIT)
  )

# Define analysis variables
vars <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CHG",
  covariates = c("BASE", "STRATA", "REGION")
)

# Specify imputation method
method <- rbmi::method_bayes(
  n_samples = N_IMPUTATIONS,
  control = rbmi::control_bayes(
    warmup = WARMUP,
    thin = THIN
  )
)

# Run analysis
ana_obj <- analyse_mi_data(
  data = ADMI,
  vars = vars,
  method = method,
  fun = ancova
)

# Pool results and tidy
pool_obj <- pool(ana_obj)
tidy_df <- tidy_pool_obj(pool_obj)

# View results
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

## Datasets

The package includes two example datasets for demonstrating imputation
and analysis:

- `ADEFF`: An example efficacy dataset for with missing data.
- `ADMI`: A large multiple imputation dataset with 100,000 rows and
  multiple visits, treatment arms, and stratification variables.

Use
[`?ADEFF`](https://openpharma.github.io/rbmiUtils/reference/ADEFF.md)
and [`?ADMI`](https://openpharma.github.io/rbmiUtils/reference/ADMI.md)
to view full dataset documentation.

## Utilities

Key exported functions include:

- [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md):
  Applies an analysis function (e.g., ANCOVA) to all imputed datasets.
- [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md):
  Tidies and annotates pooled results for reporting.
- [`get_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/get_imputed_data.md):
  Extracts long-format imputed datasets with original subject IDs
  mapped.

These utilities wrap standard `rbmi` workflows for improved
reproducibility and interpretability.

## Development Status

This package is experimental and under active development. Feedback and
contributions are welcome via [GitHub
issues](https://github.com/openpharma/rbmiUtils/issues) or pull
requests.
