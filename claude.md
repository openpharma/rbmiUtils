# Claude.md - rbmiUtils Project Guide

## Project Overview

**rbmiUtils** is an experimental R package that extends the [rbmi](https://insightsengineering.github.io/rbmi/) package to provide utilities for multiple imputation workflows in clinical trials. The package simplifies analysis execution, result pooling, data validation, and efficient storage of imputed datasets.

**Package Version:** 0.1.8.9001 (development)
**License:** GPL (>= 3)
**Status:** Experimental

## Architecture

### Directory Structure

```
rbmiUtils/
├── R/                          # Source code (9 files)
│   ├── analyse_mi_data.R       # Core analysis + S3 methods
│   ├── tidiers.R               # Result tidying (tidy_pool_obj)
│   ├── imputation_storage.R    # reduce/expand imputed data
│   ├── data_helpers.R          # validate_data, prepare_data_ice, summarise_missingness
│   ├── analysis_utils.R        # Binary outcome g-computation
│   ├── result_helpers.R        # NEW: create_impid, combine_results, format_results
│   ├── utils.R                 # Data extraction (get_imputed_data)
│   ├── ADMI.R & ADEFF.R        # Dataset documentation
│   └── rbmiUtils-package.R     # Package-level documentation
├── tests/testthat/             # Test suite (8 files)
│   ├── test-analyse_mi_data.R  # Core analysis tests (expanded)
│   ├── test-tidiers.R          # tidy_pool_obj tests (expanded)
│   ├── test-imputation_storage.R
│   ├── test-data_helpers.R
│   ├── test-analysis_utils.R   # g-computation tests (expanded)
│   ├── test-utils.R
│   ├── test-result_helpers.R   # NEW: helper function tests
│   └── test-integration.R      # NEW: end-to-end workflow tests
├── vignettes/                  # User guides (3 vignettes)
│   ├── data-preparation.Rmd
│   ├── efficient-storage.Rmd
│   └── analyse2.Rmd
├── man/                        # Roxygen-generated documentation
├── data/                       # Example datasets (ADMI, ADEFF)
└── .github/workflows/          # CI/CD (R-CMD-check, test-coverage, pkgdown)
```

### Key Functions by Category

**Analysis Functions:**
- `analyse_mi_data()` - Apply analysis function to each imputed dataset
- `tidy_pool_obj()` - Convert pooled results to publication-ready tibble
- `get_imputed_data()` - Extract imputed datasets from rbmi objects
- `print.analysis()` / `summary.analysis()` - S3 methods for analysis objects

**Result Helpers (NEW):**
- `create_impid()` - Create IMPID column from list of imputed datasets
- `combine_results()` - Combine tidy results from multiple analyses
- `format_results()` - Format results for publication-ready tables
- `extract_trt_effects()` - Extract treatment comparison rows
- `extract_lsm()` - Extract least squares mean rows

**G-Computation (Binary Endpoints):**
- `gcomp_responder()` - G-computation for binary outcomes at single visit
- `gcomp_responder_multi()` - Extends to multiple visits
- `gcomp_binary()` - Wrapper for binary outcome analysis

**Data Preparation & Validation:**
- `validate_data()` - Pre-flight validation of analysis data
- `prepare_data_ice()` - Build ICE (intercurrent events) data frame
- `summarise_missingness()` - Tabulate missing data patterns

**Efficient Storage:**
- `reduce_imputed_data()` - Extract only originally missing values (90%+ storage reduction)
- `expand_imputed_data()` - Reconstruct full imputed dataset

### Dependencies

- **rbmi** (>= 1.4) - Core imputation functionality
- **dplyr**, **tidyr**, **purrr** - Data manipulation
- **assertthat**, **rlang** - Input validation and error handling
- **beeca** - Binary endpoint analysis

## Development Workflow

### Running Tests

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-imputation_storage.R")

# Check package
devtools::check()
```

### Building Documentation

```r
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

## Completed Improvements

### High Priority (DONE)

1. **Expanded Test Coverage for Core Functions**
   - `analyse_mi_data()` - Added 10+ new tests for edge cases, delta adjustment, methods
   - `tidy_pool_obj()` - Added 7 new tests for single visit, numeric precision, filtering
   - `gcomp_responder_multi()` - Added 6 new tests for multiple visits, covariates

2. **Added Integration Tests**
   - New `test-integration.R` with end-to-end workflow tests
   - Tests complete pipelines: data prep → analysis → pooling → tidy
   - Tests storage workflow: reduce → expand roundtrip

3. **Improved Input Validation**
   - `prepare_data_ice()` - Now validates vars fields upfront
   - `analyse_mi_data()` - Better error messages, validates method, handles edge cases
   - Clear error messages with actionable guidance

4. **Workflow Documentation**
   - Added `@seealso` cross-references in all major functions
   - Documented recommended workflow in function details
   - Clear indication of function dependencies

### Medium Priority (DONE)

5. **Added Helper Functions**
   - `create_impid()` - Creates IMPID from list of data.frames
   - `combine_results()` - Combines multiple tidy results
   - `format_results()` - Publication-ready formatting
   - `extract_trt_effects()` / `extract_lsm()` - Convenience filters

6. **Added S3 Methods for Results**
   - `print.analysis()` - Shows summary of analysis object
   - `summary.analysis()` - Detailed summary with next steps

## Remaining Improvements

### Low Priority

1. **Performance Optimization**
   - Optimize storage functions for very large datasets
   - Consider parallel processing support for `analyse_mi_data()`
   - Memory profiling and optimization

2. **Enhanced Diagnostics**
   - Imputation model fit diagnostics
   - Convergence checks for Bayesian methods
   - Visual diagnostics for missing data patterns

3. **User Experience**
   - Add progress indicators for long-running operations
   - `plot()` method for visualizing treatment effects
   - Interactive diagnostics (Shiny app for exploration)

4. **Extend Analysis Capabilities**
   - Support for multiple outcome variables
   - More statistical models (mixed models, etc.)
   - Built-in sensitivity analysis utilities

## Testing Guidelines

### Test File Organization

```
tests/testthat/
├── test-analyse_mi_data.R      # Core analysis tests (comprehensive)
├── test-tidiers.R              # tidy_pool_obj tests (comprehensive)
├── test-imputation_storage.R   # reduce/expand tests (comprehensive)
├── test-data_helpers.R         # validation/ICE tests (comprehensive)
├── test-analysis_utils.R       # g-computation tests (comprehensive)
├── test-utils.R                # get_imputed_data tests
├── test-result_helpers.R       # Helper function tests (comprehensive)
└── test-integration.R          # End-to-end workflow tests
```

### Writing Tests

```r
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

### Test Coverage Status

| Function | Coverage Status |
|----------|-----------------|
| `analyse_mi_data()` | Comprehensive |
| `tidy_pool_obj()` | Comprehensive |
| `validate_data()` | Comprehensive |
| `prepare_data_ice()` | Comprehensive |
| `reduce_imputed_data()` | Comprehensive |
| `expand_imputed_data()` | Comprehensive |
| `gcomp_responder()` | Comprehensive |
| `gcomp_responder_multi()` | Comprehensive |
| `create_impid()` | Comprehensive |
| `combine_results()` | Comprehensive |
| `format_results()` | Comprehensive |

## Common Tasks

### Adding a New Exported Function

1. Create function in appropriate R file
2. Add roxygen documentation with `@export`
3. Add `@seealso` references to related functions
4. Run `devtools::document()` to update NAMESPACE
5. Add tests in corresponding test file
6. Update NEWS.md with the addition

### Fixing a Bug

1. First, write a test that exposes the bug
2. Fix the bug in the source code
3. Verify the test passes
4. Check for similar issues in related functions
5. Update NEWS.md if user-facing

### Updating Documentation

1. Edit roxygen comments in R files
2. Run `devtools::document()`
3. Preview with `?function_name`
4. For vignettes, edit .Rmd files in `vignettes/`
5. Build with `devtools::build_vignettes()`

## Notes for AI Assistants

- This is an R package following standard devtools conventions
- Always run `devtools::check()` before committing changes
- Exported functions should have comprehensive roxygen documentation
- Tests use testthat (edition 3)
- The package is experimental - breaking changes are acceptable with documentation
- Focus on clinical trial workflows and rbmi integration
- Validate inputs early and provide helpful error messages
- Use `@seealso` to cross-reference related functions
