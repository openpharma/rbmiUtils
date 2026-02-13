# rbmiUtils

`rbmiUtils` bridges [rbmi](https://github.com/openpharma/rbmi) analysis
results into publication-ready regulatory tables and forest plots. It
extends rbmi for clinical trial workflows, handling everything from data
validation through to formatted efficacy outputs.

## Installation

You can install the package from CRAN or the development version from
GitHub:

| Type        | Source | Command                                           |
|-------------|--------|---------------------------------------------------|
| Release     | CRAN   | `install.packages("rbmiUtils")`                   |
| Development | GitHub | `remotes::install_github("openpharma/rbmiUtils")` |

## Quick Start

`rbmiUtils` extends the [rbmi](https://openpharma.github.io/rbmi/)
pipeline from raw data to publication-ready outputs. Here is the
complete workflow using the bundled `ADEFF` dataset:

``` r
library(rbmiUtils)
library(rbmi)
library(dplyr)

# Load example efficacy dataset and prepare factors
data("ADEFF", package = "rbmiUtils")
ADEFF <- ADEFF |>
  mutate(
    TRT = factor(TRT01P, levels = c("Placebo", "Drug A")),
    USUBJID = factor(USUBJID),
    AVISIT = factor(AVISIT, levels = c("Week 24", "Week 48"))
  )

# Define analysis variables
vars <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CHG",
  covariates = c("BASE", "STRATA", "REGION")
)

# Configure Bayesian imputation method
method <- method_bayes(
  n_samples = 100,
  control = control_bayes(warmup = 200, thin = 2)
)

# Step 1: Fit imputation model (draws)
dat <- ADEFF |> select(USUBJID, STRATA, REGION, TRT, BASE, CHG, AVISIT)
draws_obj <- draws(data = dat, vars = vars, method = method)

# Step 2: Generate imputed datasets
impute_obj <- impute(
  draws_obj,
  references = c("Placebo" = "Placebo", "Drug A" = "Placebo")
)

# Step 3: Extract stacked imputed data
ADMI <- get_imputed_data(impute_obj)

# Step 4: Analyse each imputed dataset
ana_obj <- analyse_mi_data(data = ADMI, vars = vars, method = method, fun = ancova)

# Step 5: Pool results using Rubin's rules
pool_obj <- pool(ana_obj)

# Publication-ready outputs
efficacy_table(pool_obj, arm_labels = c(ref = "Placebo", alt = "Drug A"))
plot_forest(pool_obj, arm_labels = c(ref = "Placebo", alt = "Drug A"))
```

**Forest Plot**

![Forest Plot](reference/figures/README-forest-plot-1.png)

Forest Plot

**Efficacy Table**

![Efficacy Table](reference/figures/README-efficacy-table-1.png)

Efficacy Table

See the [end-to-end pipeline
vignette](https://openpharma.github.io/rbmiUtils/articles/pipeline.html)
for the complete walkthrough from raw data to these outputs.

## Key Features

- [`validate_data()`](https://openpharma.github.io/rbmiUtils/reference/validate_data.md)
  – pre-flight checks on data structure before imputation
- [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md)
  – run ANCOVA (or custom analysis) across all imputations
- [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
  – tidy pooled results with visit-level annotations
- [`efficacy_table()`](https://openpharma.github.io/rbmiUtils/reference/efficacy_table.md)
  – regulatory-style gt tables (CDISC/ICH Table 14.2.x format)
- [`plot_forest()`](https://openpharma.github.io/rbmiUtils/reference/plot_forest.md)
  – three-panel forest plots with estimates, CIs, and p-values
- [`pool_to_ard()`](https://openpharma.github.io/rbmiUtils/reference/pool_to_ard.md)
  – convert pool objects to pharmaverse ARD format with optional MI
  diagnostic enrichment (FMI, lambda, RIV)
- [`get_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/get_imputed_data.md)
  – extract long-format imputed datasets
- [`describe_draws()`](https://openpharma.github.io/rbmiUtils/reference/describe_draws.md)
  – inspect draws objects (method, samples, convergence diagnostics)
- [`describe_imputation()`](https://openpharma.github.io/rbmiUtils/reference/describe_imputation.md)
  – inspect imputation objects (method, M, missingness breakdown)
- [`format_pvalue()`](https://openpharma.github.io/rbmiUtils/reference/format_pvalue.md)
  /
  [`format_estimate()`](https://openpharma.github.io/rbmiUtils/reference/format_estimate.md)
  – publication-ready formatting

## Learn More

- [From rbmi Analysis to Regulatory
  Tables](https://openpharma.github.io/rbmiUtils/articles/pipeline.html)
  – end-to-end walkthrough from raw data to regulatory outputs
- [Storing and Analyzing Imputed
  Data](https://openpharma.github.io/rbmiUtils/articles/analyse2.html) –
  focused guide on analysis workflows
- [MI Diagnostics and Describe
  Helpers](https://openpharma.github.io/rbmiUtils/articles/diagnostics.html)
  – inspecting draws, imputations, and MI diagnostic statistics
- [Package documentation](https://openpharma.github.io/rbmiUtils/)

## Development Status

This package is experimental and under active development. Feedback and
contributions are welcome via [GitHub
issues](https://github.com/openpharma/rbmiUtils/issues) or pull
requests.
