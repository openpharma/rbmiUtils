# Prepare Intercurrent Event Data

Builds a `data_ice` data.frame from a column in the dataset that flags
intercurrent events. For each subject, the first visit (by factor level
order) where the flag is TRUE is used as the ICE visit.

## Usage

``` r
prepare_data_ice(data, vars, ice_col, strategy)
```

## Arguments

- data:

  A data.frame containing the analysis dataset.

- vars:

  A `vars` object as created by
  [`rbmi::set_vars()`](https://openpharma.github.io/rbmi/latest-tag/reference/set_vars.html).

- ice_col:

  Character string naming the column in `data` that indicates ICE
  occurrence. Accepted values are logical (`TRUE`/`FALSE`), character
  (`"Y"`/`"N"`), or numeric (`1`/`0`).

- strategy:

  Character string specifying the imputation strategy to assign. Must be
  one of `"MAR"`, `"CR"`, `"JR"`, `"CIR"`, or `"LMCF"`.

## Value

A data.frame with columns corresponding to `vars$subjid`, `vars$visit`,
and `vars$strategy`, suitable for passing to
[`rbmi::draws()`](https://openpharma.github.io/rbmi/latest-tag/reference/draws.html).

## See also

- [`rbmi::draws()`](https://openpharma.github.io/rbmi/latest-tag/reference/draws.html)
  which accepts the `data_ice` output from this function

- [`validate_data()`](https://openpharma.github.io/rbmiUtils/reference/validate_data.md)
  to check data before imputation

- [`summarise_missingness()`](https://openpharma.github.io/rbmiUtils/reference/summarise_missingness.md)
  to understand missing data patterns

## Examples

``` r
library(rbmi)

dat <- data.frame(
  USUBJID = factor(rep(c("S1", "S2", "S3"), each = 3)),
  AVISIT = factor(rep(c("Week 4", "Week 8", "Week 12"), 3),
                  levels = c("Week 4", "Week 8", "Week 12")),
  TRT = factor(rep(c("Placebo", "Drug A", "Drug A"), each = 3)),
  CHG = rnorm(9),
  DISCFL = c("N","N","N", "N","Y","Y", "N","N","Y")
)

vars <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CHG"
)

ice <- prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "JR")
print(ice)
#>   USUBJID  AVISIT strategy
#> 1      S2  Week 8       JR
#> 2      S3 Week 12       JR
```
