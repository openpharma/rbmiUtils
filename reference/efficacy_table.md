# Create Regulatory-Style Efficacy Summary Table

Takes an rbmi pool object and produces a publication-ready gt table in
the style of CDISC/ICH Table 14.2.x. The table displays least squares
means by treatment arm, treatment differences, confidence intervals, and
p-values, organized by visit row groups.

## Usage

``` r
efficacy_table(
  pool_obj,
  title = NULL,
  subtitle = NULL,
  digits = 2,
  ci_level = NULL,
  arm_labels = NULL,
  pval_digits = 3,
  pval_threshold = 0.001,
  font_family = NULL,
  font_size = NULL,
  row_padding = NULL,
  ...
)
```

## Arguments

- pool_obj:

  A pooled analysis object of class `"pool"`, typically obtained from
  [`rbmi::pool()`](https://openpharma.github.io/rbmi/latest-tag/reference/pool.html)
  after calling
  [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md).

- title:

  Optional character string for the table title.

- subtitle:

  Optional character string for the table subtitle.

- digits:

  Integer. Number of decimal places for estimates and standard errors.
  Default is 2.

- ci_level:

  Numeric. Confidence level for CI column labeling. If `NULL` (the
  default), extracted from `pool_obj$conf.level`. Falls back to 0.95 if
  neither is available.

- arm_labels:

  Named character vector with elements `"ref"` and `"alt"` providing
  custom labels for the reference and treatment arms. If `NULL` (the
  default), uses `"Reference"` and `"Treatment"`.

- pval_digits:

  Integer. Number of decimal places for p-values. Default is 3.

- pval_threshold:

  Numeric. P-values below this threshold are displayed as "\<
  threshold". Default is 0.001.

- font_family:

  Optional character string specifying the font family for the table.
  When `NULL` (default), uses gt's default font. Applied via
  [`gt::opt_table_font()`](https://gt.rstudio.com/reference/opt_table_font.html).

- font_size:

  Optional numeric value specifying the table font size in pixels. When
  `NULL` (default), uses gt's default size. Applied via
  [`gt::tab_options()`](https://gt.rstudio.com/reference/tab_options.html).

- row_padding:

  Optional numeric value specifying the vertical padding for data rows
  in pixels. When `NULL` (default), uses gt's default padding. Smaller
  values (e.g., 2-3) create compact regulatory-style tables.

- ...:

  Additional arguments passed to
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html).

## Value

A gt table object of class `gt_tbl`.

## Details

This function assumes a single-parameter-per-visit pool object (the
standard output from an rbmi ANCOVA or MMRM pipeline). It internally
calls
[`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
to parse the pool object, then constructs the gt table.

**Arm labels:** Use the `arm_labels` parameter to customize arm names in
the table. For example,
`arm_labels = c(ref = "Placebo", alt = "Drug A")` will display "LS Mean
(Placebo)" and "LS Mean (Drug A)" instead of the defaults.

**Customization:** The returned gt object can be further customized
using standard gt piping, e.g.,
`efficacy_table(pool_obj) |> gt::tab_options(...)`.

**Example output:**

![](figures/efficacy_table-example.png)

## See also

- [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
  for the underlying data transformation

- [`format_pvalue()`](https://openpharma.github.io/rbmiUtils/reference/format_pvalue.md)
  for p-value formatting rules

- [`rbmi::pool()`](https://openpharma.github.io/rbmi/latest-tag/reference/pool.html)
  to create pool objects

## Examples

``` r
# \donttest{
if (requireNamespace("gt", quietly = TRUE)) {
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

  # Basic table
  tbl <- efficacy_table(pool_obj)

  # Publication-styled table
  efficacy_table(
    pool_obj,
    title = "Table 14.2.1: ANCOVA of Change from Baseline",
    subtitle = "Mixed Model for Repeated Measures",
    arm_labels = c(ref = "Placebo", alt = "Drug A"),
    font_size = 12,
    row_padding = 4
  )
}
#> Warning: Data contains 100 imputations but method expects 20. Using first 20
#> imputations.


  


Table 14.2.1: ANCOVA of Change from Baseline
```

Mixed Model for Repeated Measures

Estimate

Std. Error

95% CI

P-value

Week 24

LS Mean (Placebo)

0.09

0.13

(-0.17, 0.34)

0.514

LS Mean (Drug A)

−2.10

0.13

(-2.34, -1.85)

\< 0.001

Treatment Difference

−2.18

0.18

(-2.54, -1.82)

\< 0.001

Week 48

LS Mean (Placebo)

0.04

0.19

(-0.33, 0.40)

0.846

LS Mean (Drug A)

−3.76

0.18

(-4.11, -3.41)

\< 0.001

Treatment Difference

−3.79

0.26

(-4.30, -3.29)

\< 0.001

Pooling method: rubin

Number of imputations: 20

Confidence level: 95%

\# }
