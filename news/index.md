# Changelog

## rbmiUtils (development version)

### New Features

- Added `create_impid()` to convert a list of imputed datasets into a
  stacked data.frame with IMPID column.
- Added `combine_results()` to combine tidy results from multiple
  analyses.
- Added `format_results()` for publication-ready formatting of results.
- Added `extract_trt_effects()` and `extract_lsm()` convenience
  functions to filter tidy results.
- Added [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) S3 methods for
  analysis objects.
- Added new vignette “Comparing Analysis Methods: ANCOVA vs
  G-computation”.

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
- Added comprehensive integration tests for complete analysis workflows.

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
