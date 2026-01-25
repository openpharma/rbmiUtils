# Claude.md - rbmiUtils Project Guide

## Project Overview

**rbmiUtils** is an experimental R package that extends the
[rbmi](https://insightsengineering.github.io/rbmi/) package to provide
utilities for multiple imputation workflows in clinical trials. The
package simplifies analysis execution, result pooling, data validation,
and efficient storage of imputed datasets.

**Package Version:** 0.1.8.9000 (development) **License:** GPL (\>= 3)
**Status:** Experimental

## Architecture

### Directory Structure

    rbmiUtils/
    ├── R/                          # Source code (8 files, ~2,000 lines)
    │   ├── analyse_mi_data.R       # Core analysis functions
    │   ├── tidiers.R               # Result tidying (tidy_pool_obj)
    │   ├── imputation_storage.R    # reduce/expand imputed data
    │   ├── data_helpers.R          # validate_data, prepare_data_ice, summarise_missingness
    │   ├── analysis_utils.R        # Binary outcome g-computation
    │   ├── utils.R                 # Data extraction (get_imputed_data)
    │   ├── ADMI.R & ADEFF.R        # Dataset documentation
    │   └── rbmiUtils-package.R     # Package-level documentation
    ├── tests/testthat/             # Test suite (6 files, 84 tests, ~1,100 lines)
    ├── vignettes/                  # User guides (3 vignettes)
    ├── man/                        # Roxygen-generated documentation
    ├── data/                       # Example datasets (ADMI, ADEFF)
    └── .github/workflows/          # CI/CD (R-CMD-check, test-coverage, pkgdown)

### Key Functions by Category

**Analysis Functions:** -
[`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md) -
Apply analysis function to each imputed dataset -
[`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md) -
Convert pooled results to publication-ready tibble -
[`get_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/get_imputed_data.md) -
Extract imputed datasets from rbmi objects

**G-Computation (Binary Endpoints):** -
[`gcomp_responder()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_responder.md) -
G-computation for binary outcomes at single visit -
[`gcomp_responder_multi()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_responder_multi.md) -
Extends to multiple visits -
[`gcomp_binary()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_binary.md) -
Wrapper for binary outcome analysis

**Data Preparation & Validation:** -
[`validate_data()`](https://openpharma.github.io/rbmiUtils/reference/validate_data.md) -
Pre-flight validation of analysis data -
[`prepare_data_ice()`](https://openpharma.github.io/rbmiUtils/reference/prepare_data_ice.md) -
Build ICE (intercurrent events) data frame -
[`summarise_missingness()`](https://openpharma.github.io/rbmiUtils/reference/summarise_missingness.md) -
Tabulate missing data patterns

**Efficient Storage:** -
[`reduce_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/reduce_imputed_data.md) -
Extract only originally missing values (90%+ storage reduction) -
[`expand_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/expand_imputed_data.md) -
Reconstruct full imputed dataset

### Dependencies

- **rbmi** (\>= 1.4) - Core imputation functionality
- **dplyr**, **tidyr**, **purrr** - Data manipulation
- **assertthat**, **rlang** - Input validation and error handling
- **beeca** - Binary endpoint analysis

## Development Workflow

### Running Tests

``` r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-imputation_storage.R")

# Check package
devtools::check()
```

### Building Documentation

``` r
# Regenerate documentation
devtools::document()

# Build vignettes
devtools::build_vignettes()

# Preview pkgdown site
pkgdown::build_site()
```

### Code Style

- Use roxygen2 for documentation (`#'` comments)
- Follow tidyverse style guide
- Use assertthat for input validation
- Export functions via `@export` tag in roxygen

## Planned Improvements

### High Priority

1.  **Expand Test Coverage for Core Functions**
    - [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md)
      currently has minimal tests (7 tests)
    - [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
      only has 2 basic tests
    - [`gcomp_responder_multi()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_responder_multi.md)
      has only 1 test
    - Add edge case testing (empty data, single group, many imputations)
    - Target: 90%+ code coverage for exported functions
2.  **Add Integration Tests**
    - Create end-to-end workflow tests
    - Test complete pipeline: data prep → imputation → analysis →
      pooling
    - Test with ADMI/ADEFF example datasets
3.  **Improve Input Validation**
    - [`prepare_data_ice()`](https://openpharma.github.io/rbmiUtils/reference/prepare_data_ice.md):
      Validate `vars` has `strategy` defined upfront
    - [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md):
      Handle empty groups gracefully
    - Add more informative error messages with actionable guidance
4.  **Workflow Documentation**
    - Add cross-function references in roxygen docs
    - Create “Getting Started” guide showing function dependencies
    - Document which functions to call in what order

### Medium Priority

5.  **Add Helper Functions**
    - `create_impid()` - Helper to create imputation ID column
    - `combine_results()` - Combine results across visits/endpoints
    - `format_results()` - Format results for reporting tables
6.  **Add S3 Methods for Results**
    - [`print()`](https://rdrr.io/r/base/print.html) method for analysis
      results
    - [`summary()`](https://rdrr.io/r/base/summary.html) method with key
      statistics
    - [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method
      for visualizing treatment effects
7.  **Extend Analysis Capabilities**
    - Support for multiple outcome variables
    - More statistical models (mixed models, etc.)
    - Built-in sensitivity analysis utilities
8.  **Comparison/Decision Guide**
    - When to use ANCOVA vs g-computation
    - Trade-offs between different analysis approaches
    - Document statistical assumptions

### Low Priority

9.  **Performance Optimization**
    - Optimize storage functions for very large datasets
    - Consider parallel processing support for
      [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md)
    - Memory profiling and optimization
10. **Enhanced Diagnostics**
    - Imputation model fit diagnostics
    - Convergence checks for Bayesian methods
    - Visual diagnostics for missing data patterns
11. **User Experience**
    - Add progress indicators for long-running operations
    - Interactive diagnostics (Shiny app for exploration)
    - Better handling of warnings/messages

## Testing Guidelines

### Test File Organization

    tests/testthat/
    ├── test-analyse_mi_data.R      # Core analysis tests
    ├── test-tidiers.R              # tidy_pool_obj tests
    ├── test-imputation_storage.R   # reduce/expand tests (good coverage)
    ├── test-data_helpers.R         # validate_data, prepare_data_ice, summarise_missingness
    ├── test-analysis_utils.R       # g-computation tests
    └── test-utils.R                # get_imputed_data tests

### Writing Tests

``` r
test_that("function handles edge case", {
  # Arrange - set up test data
  test_data <- data.frame(...)

  # Act - call the function
  result <- my_function(test_data)


  # Assert - check expectations

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), expected_rows)
})
```

### Test Coverage Targets

| Function                                                                                           | Current       | Target        |
|----------------------------------------------------------------------------------------------------|---------------|---------------|
| [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md)         | Basic         | Comprehensive |
| [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)             | Basic         | Comprehensive |
| [`validate_data()`](https://openpharma.github.io/rbmiUtils/reference/validate_data.md)             | Good          | Comprehensive |
| [`prepare_data_ice()`](https://openpharma.github.io/rbmiUtils/reference/prepare_data_ice.md)       | Good          | Comprehensive |
| [`reduce_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/reduce_imputed_data.md) | Comprehensive | Maintain      |
| [`expand_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/expand_imputed_data.md) | Comprehensive | Maintain      |
| [`gcomp_responder()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_responder.md)         | Good          | Comprehensive |

## Common Tasks

### Adding a New Exported Function

1.  Create function in appropriate R file
2.  Add roxygen documentation with `@export`
3.  Run `devtools::document()` to update NAMESPACE
4.  Add tests in corresponding test file
5.  Update NEWS.md with the addition

### Fixing a Bug

1.  First, write a test that exposes the bug
2.  Fix the bug in the source code
3.  Verify the test passes
4.  Check for similar issues in related functions
5.  Update NEWS.md if user-facing

### Updating Documentation

1.  Edit roxygen comments in R files
2.  Run `devtools::document()`
3.  Preview with `?function_name`
4.  For vignettes, edit .Rmd files in `vignettes/`
5.  Build with `devtools::build_vignettes()`

## Notes for AI Assistants

- This is an R package following standard devtools conventions
- Always run `devtools::check()` before committing changes
- Exported functions should have comprehensive roxygen documentation
- Tests use testthat (edition 3)
- The package is experimental - breaking changes are acceptable with
  documentation
- Focus on clinical trial workflows and rbmi integration
- Validate inputs early and provide helpful error messages
