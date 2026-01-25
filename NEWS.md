# rbmiUtils (development version)

## New Features

* Added `create_impid()` to convert a list of imputed datasets into a stacked
  data.frame with IMPID column.
* Added `combine_results()` to combine tidy results from multiple analyses.
* Added `format_results()` for publication-ready formatting of results.
* Added `extract_trt_effects()` and `extract_lsm()` convenience functions
  to filter tidy results.
* Added `print()` and `summary()` S3 methods for analysis objects.
* Added new vignette "Comparing Analysis Methods: ANCOVA vs G-computation".

## Improvements

* Enhanced input validation in `analyse_mi_data()` with clearer error messages.
* Enhanced input validation in `prepare_data_ice()` to check vars fields.
* Added cross-references between related functions in documentation.
* Expanded test coverage for core functions (`analyse_mi_data()`,
  `tidy_pool_obj()`, `gcomp_responder_multi()`).
* Added comprehensive integration tests for complete analysis workflows.

## Previous Changes

* Added `reduce_imputed_data()` and `expand_imputed_data()` for efficient
  storage of imputed datasets when using many imputations.
* Added `validate_data()` for pre-flight validation before `rbmi::draws()`.
* Added `prepare_data_ice()` to build `data_ice` from flagged ICE columns.
* Added `summarise_missingness()` to tabulate missing data patterns.
* Added formatting utilities for publication-ready output:
  * `format_pvalue()` for p-value formatting with configurable thresholds
  * `format_estimate()` for estimate with confidence interval formatting
  * `format_results_table()` to add formatted columns to tidy results

# rbmiUtils 0.1.8

# rbmiUtils 0.1.7

* Moved `tidyr` from Suggests to Imports (used in `tidy_pool_obj()`).
* Added examples to `gcomp_responder()` documentation.
* Standardized examples to use native pipe operator `|>`.

# rbmiUtils 0.1.6

* Added additional tests for all utility functions. 

# rbmiUtils 0.1.4

* First release 
* Preparation for CRAN submission.
* Initial draft package.
* Added a `NEWS.md` file to track changes to the package.
* Added documentation via pkgdown.
