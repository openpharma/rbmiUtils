# Convert Pool Object to ARD Format

Converts an rbmi pool object to the pharmaverse Analysis Results Dataset
(ARD) standard using the cards package. The ARD format is a long-format
data frame where each row represents a single statistic for a given
parameter, with grouping columns for visit, parameter type, and
least-squares-mean type.

## Usage

``` r
pool_to_ard(pool_obj, analysis_obj = NULL, conf.level = NULL)
```

## Arguments

- pool_obj:

  A pooled analysis object of class `"pool"`, typically obtained from
  [`rbmi::pool()`](https://openpharma.github.io/rbmi/latest-tag/reference/pool.html)
  after calling
  [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md).

- analysis_obj:

  An optional analysis object (output of
  [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md)),
  used to compute MI diagnostic statistics. When provided and the
  pooling method is Rubin's rules, the ARD includes additional stat rows
  for FMI, lambda, RIV, Barnard-Rubin adjusted df, complete-data df,
  relative efficiency, and the number of imputations per parameter. When
  `NULL` (the default), only the base ARD is returned.

- conf.level:

  Confidence level used for CI labels (e.g., `0.95` produces "95% CI
  Lower"). If `NULL` (the default), the value is taken from
  `pool_obj$conf.level`. If that is also `NULL`, defaults to `0.95`.

## Value

A data frame of class `"card"` (ARD format) with grouping columns for
visit (`group1`), parameter_type (`group2`), and lsm_type (`group3`).
Each parameter produces rows for five statistics: estimate, std.error,
conf.low, conf.high, and p.value, plus a method row. When `analysis_obj`
is provided and the pooling method is Rubin's rules, additional
diagnostic stat rows are included: fmi, lambda, riv, df.adjusted,
df.complete, re, and m.imputations.

## Details

The function works by:

1.  Tidying the pool object via
    [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)

2.  Reshaping each parameter into long-format ARD rows (one row per
    statistic)

3.  Adding grouping columns (visit, parameter_type, lsm_type)

4.  Optionally enriching with MI diagnostic statistics when
    `analysis_obj` is provided

5.  Applying
    [`cards::as_card()`](https://insightsengineering.github.io/cards/latest-tag/reference/as_card.html)
    and
    [`cards::tidy_ard_column_order()`](https://insightsengineering.github.io/cards/latest-tag/reference/tidy_ard_order.html)
    for standard ARD structure

When `analysis_obj` is provided:

- For Rubin's rules pooling: diagnostic statistics (FMI, lambda, RIV,
  Barnard-Rubin adjusted df, relative efficiency) are computed per
  parameter using the per-imputation estimates, standard errors, and
  degrees of freedom.

- For non-Rubin pooling methods: an informative message is emitted and
  the base ARD is returned without diagnostic rows.

The resulting ARD passes
[`cards::check_ard_structure()`](https://insightsengineering.github.io/cards/latest-tag/reference/check_ard_structure.html)
validation and is suitable for downstream use with gtsummary.

## See also

- [`rbmi::pool()`](https://openpharma.github.io/rbmi/latest-tag/reference/pool.html)
  for creating pool objects

- [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md)
  for creating analysis objects

- [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
  for the underlying data transformation

- [`cards::as_card()`](https://insightsengineering.github.io/cards/latest-tag/reference/as_card.html)
  and
  [`cards::check_ard_structure()`](https://insightsengineering.github.io/cards/latest-tag/reference/check_ard_structure.html)
  for ARD validation

## Examples

``` r
# \donttest{
if (requireNamespace("cards", quietly = TRUE)) {
  library(rbmi)
  data("ADMI", package = "rbmiUtils")
  ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
  ADMI$USUBJID <- factor(ADMI$USUBJID)
  ADMI$AVISIT <- factor(ADMI$AVISIT)

  vars <- set_vars(
    subjid = "USUBJID", visit = "AVISIT", group = "TRT",
    outcome = "CHG", covariates = c("BASE", "STRATA", "REGION")
  )
  method <- method_bayes(
    n_samples = 20,
    control = control_bayes(warmup = 20, thin = 1)
  )

  ana_obj <- analyse_mi_data(ADMI, vars, method, fun = ancova)
  pool_obj <- pool(ana_obj)

  # Base ARD
  ard <- pool_to_ard(pool_obj)

  # Enriched ARD with MI diagnostics (FMI, lambda, RIV, df)
  ard_diag <- pool_to_ard(pool_obj, analysis_obj = ana_obj)
}
#> Warning: Data contains 100 imputations but method expects 20. Using first 20
#> imputations.
# }
```
