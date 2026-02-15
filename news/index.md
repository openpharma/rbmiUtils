# Changelog

## rbmiUtils 0.2.2

### New Features

- [`describe_draws()`](https://openpharma.github.io/rbmiUtils/reference/describe_draws.md)
  extracts structured metadata from rbmi draws objects, including method
  type, formula, sample count, and (for Bayesian methods) MCMC
  convergence diagnostics (ESS, Rhat).
- [`describe_imputation()`](https://openpharma.github.io/rbmiUtils/reference/describe_imputation.md)
  extracts imputation metadata including method, number of imputations
  (M), reference arm mappings, and a missingness breakdown by visit and
  treatment arm.
- [`pool_to_ard()`](https://openpharma.github.io/rbmiUtils/reference/pool_to_ard.md)
  gains an `analysis_obj` parameter that enriches the ARD with MI
  diagnostic statistics (FMI, lambda, RIV, Barnard-Rubin adjusted df,
  relative efficiency) when the pooling method is Rubin’s rules.

### Improvements

- [`efficacy_table()`](https://openpharma.github.io/rbmiUtils/reference/efficacy_table.md)
  gains `font_family`, `font_size`, and `row_padding` parameters for
  publication-ready table styling.
- [`plot_forest()`](https://openpharma.github.io/rbmiUtils/reference/plot_forest.md)
  gains `font_family` and `panel_widths` parameters for customizable
  typography and panel layout.
- [`plot_forest()`](https://openpharma.github.io/rbmiUtils/reference/plot_forest.md)
  left panel now uses left-aligned text (hjust=0) for consistent
  positioning regardless of label length.
- Non-Rubin pooling methods now emit an informative
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html)
  message when MI diagnostics are not applicable, rather than returning
  NA rows.

## rbmiUtils 0.2.1

### New Features

- End-to-end pipeline vignette “From rbmi Analysis to Regulatory Tables”
  with continuous and binary analysis walkthroughs.
- README enhanced with rendered efficacy table and forest plot visual
  teasers.
- Rendered example output images in
  [`plot_forest()`](https://openpharma.github.io/rbmiUtils/reference/plot_forest.md)
  and
  [`efficacy_table()`](https://openpharma.github.io/rbmiUtils/reference/efficacy_table.md)
  help pages.
- Inline cross-references to rbmi and beeca documentation in all
  vignettes.

### Improvements

- [`validate_data()`](https://openpharma.github.io/rbmiUtils/reference/validate_data.md)
  now uses cli-formatted error messages with clearer guidance for
  malformed interaction terms, empty data, and type mismatches.
- [`prepare_data_ice()`](https://openpharma.github.io/rbmiUtils/reference/prepare_data_ice.md)
  now errors immediately when `vars$strategy` is NULL instead of
  silently using a default column name.
- [`prepare_data_ice()`](https://openpharma.github.io/rbmiUtils/reference/prepare_data_ice.md)
  warns when visit column is character with guidance to convert to
  factor for correct ordering.
- [`validate_data()`](https://openpharma.github.io/rbmiUtils/reference/validate_data.md)
  batches all type coercion warnings into a single message and warns on
  all-NA covariate columns.
- Data preparation functions handle edge cases (single subject, single
  visit, all-NA outcome, all-complete data) gracefully.

## rbmiUtils 0.2.0

### Breaking Changes

- [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
  now uses regex-based parameter parsing instead of splitting on `_`.
  Output columns (`parameter_type`, `lsm_type`, `visit`) now contain the
  full visit name rather than truncated fragments.

### New Features

- [`efficacy_table()`](https://openpharma.github.io/rbmiUtils/reference/efficacy_table.md)
  creates regulatory-style gt summary tables from pool objects.
- [`plot_forest()`](https://openpharma.github.io/rbmiUtils/reference/plot_forest.md)
  creates publication-quality three-panel forest plots from pool
  objects.
- [`pool_to_ard()`](https://openpharma.github.io/rbmiUtils/reference/pool_to_ard.md)
  converts pool objects to pharmaverse ARD format via the cards package.
- [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) S3 methods for pool
  and analysis objects.
- [`create_impid()`](https://openpharma.github.io/rbmiUtils/reference/create_impid.md)
  converts lists of imputed datasets into stacked data.frames.
- [`combine_results()`](https://openpharma.github.io/rbmiUtils/reference/combine_results.md)
  combines tidy results from multiple analyses.
- [`format_results()`](https://openpharma.github.io/rbmiUtils/reference/format_results.md)
  and
  [`format_results_table()`](https://openpharma.github.io/rbmiUtils/reference/format_results_table.md)
  for publication-ready formatting.
- [`extract_trt_effects()`](https://openpharma.github.io/rbmiUtils/reference/extract_trt_effects.md)
  and
  [`extract_lsm()`](https://openpharma.github.io/rbmiUtils/reference/extract_lsm.md)
  convenience filters for tidy results.

### Dependencies

- Added `cli` (\>= 3.6.0) and `lifecycle` (\>= 1.0.4) to Imports for
  improved error messaging and deprecation support.

### Improvements

- Enhanced input validation in
  [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md)
  with clearer error messages using
  [`inherits()`](https://rdrr.io/r/base/class.html) instead of internal
  class helpers.
- Enhanced input validation in
  [`prepare_data_ice()`](https://openpharma.github.io/rbmiUtils/reference/prepare_data_ice.md)
  to check vars fields.
- Cross-references between related functions in documentation.
- Expanded test coverage and integration tests.

## rbmiUtils 0.1.9

### New Features

- [`reduce_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/reduce_imputed_data.md)
  and
  [`expand_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/expand_imputed_data.md)
  for efficient storage of imputed datasets.
- [`validate_data()`](https://openpharma.github.io/rbmiUtils/reference/validate_data.md)
  for pre-flight validation before
  [`rbmi::draws()`](https://openpharma.github.io/rbmi/latest-tag/reference/draws.html).
- [`prepare_data_ice()`](https://openpharma.github.io/rbmiUtils/reference/prepare_data_ice.md)
  to build `data_ice` from flagged ICE columns.
- [`summarise_missingness()`](https://openpharma.github.io/rbmiUtils/reference/summarise_missingness.md)
  for tabulating missing data patterns.
- [`format_pvalue()`](https://openpharma.github.io/rbmiUtils/reference/format_pvalue.md),
  [`format_estimate()`](https://openpharma.github.io/rbmiUtils/reference/format_estimate.md),
  [`format_results_table()`](https://openpharma.github.io/rbmiUtils/reference/format_results_table.md)
  formatting utilities.
- `ADEFF` and `ADMI` example datasets.

### Improvements

- Initial documentation via pkgdown.

## rbmiUtils 0.1.8

CRAN release: 2026-01-24

### CRAN Release

- Released to CRAN on 2026-01-24.

## rbmiUtils 0.1.7

### Improvements

- Moved tidyr from Suggests to Imports (used in tidy_pool_obj()).
- Added examples to gcomp_responder() documentation.
- Standardized examples to use native pipe operator \|\>.

## rbmiUtils 0.1.6

CRAN release: 2025-09-13

### CRAN Release

- Released to CRAN on 2025-09-13.

### Improvements

- Added additional tests for all utility functions.

## rbmiUtils 0.1.4

CRAN release: 2025-05-23

### CRAN Release

- Released to CRAN on 2025-05-23.

## rbmiUtils 0.1.0

### New Features

- Initial draft package.
- Added a NEWS.md file to track changes to the package.

### Improvements

- Preparation for CRAN submission.
- Added documentation via pkgdown.
