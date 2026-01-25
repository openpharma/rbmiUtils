# Validate Data Before Imputation

Pre-flight validation of data, variable specification, and intercurrent
event data before calling
[`rbmi::draws()`](https://openpharma.github.io/rbmi/latest-tag/reference/draws.html).
Collects all issues and reports them together in a single error message.

## Usage

``` r
validate_data(data, vars, data_ice = NULL)
```

## Arguments

- data:

  A data.frame containing the analysis dataset.

- vars:

  A `vars` object as created by
  [`rbmi::set_vars()`](https://openpharma.github.io/rbmi/latest-tag/reference/set_vars.html).

- data_ice:

  An optional data.frame of intercurrent events. If provided, must
  contain columns corresponding to `vars$subjid`, `vars$visit`, and
  `vars$strategy`.

## Value

Invisibly returns `TRUE` if all checks pass. Throws an error with
collected messages if any issues are found.

## Details

The following checks are performed:

- `data` is a data.frame

- All columns named in `vars` exist in `data`

- `subjid`, `visit`, and `group` columns are factors

- `outcome` column is numeric

- Covariate columns have no missing values

- Data has one row per subject-visit combination

- If `data_ice` is provided: correct columns, valid subjects, valid
  visits, recognised strategies, and at most one row per subject

## Examples

``` r
library(rbmi)

dat <- data.frame(
  USUBJID = factor(rep(c("S1", "S2", "S3"), each = 3)),
  AVISIT = factor(rep(c("Week 4", "Week 8", "Week 12"), 3),
                  levels = c("Week 4", "Week 8", "Week 12")),
  TRT = factor(rep(c("Placebo", "Drug A", "Drug A"), each = 3)),
  CHG = c(1.1, 2.2, 3.3, 0.5, NA, NA, 1.0, 2.0, NA),
  BASE = rep(c(10, 12, 11), each = 3),
  STRATA = factor(rep(c("A", "B", "A"), each = 3))
)

vars <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CHG",
  covariates = c("BASE", "STRATA")
)

validate_data(dat, vars)
```
