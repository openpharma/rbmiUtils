# Changelog

## rbmiUtils (development version)

### New Features

- Added
  [`create_impid()`](https://openpharma.github.io/rbmiUtils/reference/create_impid.md)
  to convert a list of imputed datasets into a stacked data.frame with
  IMPID column.
- Added
  [`combine_results()`](https://openpharma.github.io/rbmiUtils/reference/combine_results.md)
  to combine tidy results from multiple analyses.
- Added
  [`format_results()`](https://openpharma.github.io/rbmiUtils/reference/format_results.md)
  for publication-ready formatting of results.
- Added
  [`extract_trt_effects()`](https://openpharma.github.io/rbmiUtils/reference/extract_trt_effects.md)
  and
  [`extract_lsm()`](https://openpharma.github.io/rbmiUtils/reference/extract_lsm.md)
  convenience functions to filter tidy results.
- Added [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) S3 methods for
  analysis objects.

### Improvements

- Enhanced input validation in
  [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md)
  with clearer error messages.
- Enhanced input validation in
  [`prepare_data_ice()`](https://openpharma.github.io/rbmiUtils/reference/prepare_data_ice.md)
  to check vars fields.
- Added cross-references between related functions in documentation.
- Expanded test coverage for core functions
  ([`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md),
  [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md),
  [`gcomp_responder_multi()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_responder_multi.md)).
- Added integration tests for complete analysis workflows.

### Previous Changes

- Added
  [`reduce_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/reduce_imputed_data.md)
  and
  [`expand_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/expand_imputed_data.md)
  for efficient storage of imputed datasets when using many imputations.
- Added
  [`validate_data()`](https://openpharma.github.io/rbmiUtils/reference/validate_data.md)
  for pre-flight validation before
  [`rbmi::draws()`](https://openpharma.github.io/rbmi/latest-tag/reference/draws.html).
- Added
  [`prepare_data_ice()`](https://openpharma.github.io/rbmiUtils/reference/prepare_data_ice.md)
  to build `data_ice` from flagged ICE columns.
- Added
  [`summarise_missingness()`](https://openpharma.github.io/rbmiUtils/reference/summarise_missingness.md)
  to tabulate missing data patterns.
- Added formatting utilities for publication-ready output:
  - [`format_pvalue()`](https://openpharma.github.io/rbmiUtils/reference/format_pvalue.md)
    for p-value formatting with configurable thresholds
  - [`format_estimate()`](https://openpharma.github.io/rbmiUtils/reference/format_estimate.md)
    for estimate with confidence interval formatting
  - [`format_results_table()`](https://openpharma.github.io/rbmiUtils/reference/format_results_table.md)
    to add formatted columns to tidy results

## rbmiUtils 0.1.8

CRAN release: 2026-01-24

## rbmiUtils 0.1.7

- Moved `tidyr` from Suggests to Imports (used in
  [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)).
- Added examples to
  [`gcomp_responder()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_responder.md)
  documentation.
- Standardized examples to use native pipe operator `|>`.

## rbmiUtils 0.1.6

CRAN release: 2025-09-13

- Added additional tests for all utility functions.

## rbmiUtils 0.1.4

CRAN release: 2025-05-23

- First release
- Preparation for CRAN submission.
- Initial draft package.
- Added a `NEWS.md` file to track changes to the package.
- Added documentation via pkgdown.
