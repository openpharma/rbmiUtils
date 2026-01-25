# rbmiUtils (development version)

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
