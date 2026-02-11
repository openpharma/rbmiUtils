# rbmiUtils 0.2.2

## New Features

* `describe_draws()` extracts structured metadata from rbmi draws objects,
  including method type, formula, sample count, and (for Bayesian methods)
  MCMC convergence diagnostics (ESS, Rhat).
* `describe_imputation()` extracts imputation metadata including method,
  number of imputations (M), reference arm mappings, and a missingness
  breakdown by visit and treatment arm.
* `pool_to_ard()` gains an `analysis_obj` parameter that enriches the ARD
  with MI diagnostic statistics (FMI, lambda, RIV, Barnard-Rubin adjusted
  df, relative efficiency) when the pooling method is Rubin's rules.

## Improvements

* `efficacy_table()` gains `font_family`, `font_size`, and `row_padding`
  parameters for publication-ready table styling.
* `plot_forest()` gains `font_family` and `panel_widths` parameters for
  customizable typography and panel layout.
* `plot_forest()` left panel now uses left-aligned text (hjust=0) for
  consistent positioning regardless of label length.
* Non-Rubin pooling methods now emit an informative `cli::cli_inform()`
  message when MI diagnostics are not applicable, rather than returning
  NA rows.

# rbmiUtils 0.2.1

## New Features

* End-to-end pipeline vignette "From rbmi Analysis to Regulatory Tables" with
  continuous and binary analysis walkthroughs.
* README enhanced with rendered efficacy table and forest plot visual teasers.
* Rendered example output images in `plot_forest()` and `efficacy_table()` help pages.
* Inline cross-references to rbmi and beeca documentation in all vignettes.

## Improvements

* `validate_data()` now uses cli-formatted error messages with clearer guidance
  for malformed interaction terms, empty data, and type mismatches.
* `prepare_data_ice()` now errors immediately when `vars$strategy` is NULL
  instead of silently using a default column name.
* `prepare_data_ice()` warns when visit column is character with guidance to
  convert to factor for correct ordering.
* `validate_data()` batches all type coercion warnings into a single message
  and warns on all-NA covariate columns.
* Data preparation functions handle edge cases (single subject, single visit,
  all-NA outcome, all-complete data) gracefully.

# rbmiUtils 0.2.0

## Breaking Changes

* `tidy_pool_obj()` now uses regex-based parameter parsing instead of splitting
  on `_`. Output columns (`parameter_type`, `lsm_type`, `visit`) now contain
  the full visit name rather than truncated fragments.

## New Features

* `efficacy_table()` creates regulatory-style gt summary tables from pool objects.
* `plot_forest()` creates publication-quality three-panel forest plots from pool objects.
* `pool_to_ard()` converts pool objects to pharmaverse ARD format via the cards package.
* `print()` and `summary()` S3 methods for pool and analysis objects.
* `create_impid()` converts lists of imputed datasets into stacked data.frames.
* `combine_results()` combines tidy results from multiple analyses.
* `format_results()` and `format_results_table()` for publication-ready formatting.
* `extract_trt_effects()` and `extract_lsm()` convenience filters for tidy results.

## Dependencies

* Added `cli` (>= 3.6.0) and `lifecycle` (>= 1.0.4) to Imports for improved
  error messaging and deprecation support.

## Improvements

* Enhanced input validation in `analyse_mi_data()` with clearer error messages
  using `inherits()` instead of internal class helpers.
* Enhanced input validation in `prepare_data_ice()` to check vars fields.
* Cross-references between related functions in documentation.
* Expanded test coverage and integration tests.

# Previous Versions

## rbmiUtils 0.1.9

* `reduce_imputed_data()` and `expand_imputed_data()` for efficient storage
  of imputed datasets.
* `validate_data()` for pre-flight validation before `rbmi::draws()`.
* `prepare_data_ice()` to build `data_ice` from flagged ICE columns.
* `summarise_missingness()` for tabulating missing data patterns.
* `format_pvalue()`, `format_estimate()`, `format_results_table()` formatting utilities.
* `ADEFF` and `ADMI` example datasets.
* Initial documentation via pkgdown.

## rbmiUtils 0.1.8
CRAN release: 2026-01-24

## rbmiUtils 0.1.7

* Moved tidyr from Suggests to Imports (used in tidy_pool_obj()).
* Added examples to gcomp_responder() documentation.
* Standardized examples to use native pipe operator |>.

## rbmiUtils 0.1.6
CRAN release: 2025-09-13
Added additional tests for all utility functions.

## rbmiUtils 0.1.4
CRAN release: 2025-05-23

## First release
* Preparation for CRAN submission.
* Initial draft package.
* Added a NEWS.md file to track changes to the package.
* Added documentation via pkgdown.
