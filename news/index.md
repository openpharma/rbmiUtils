# Changelog

## rbmiUtils (development version)

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
