# Package index

## Data Preparation

Validate and prepare data before imputation

- [`validate_data()`](https://openpharma.github.io/rbmiUtils/reference/validate_data.md)
  : Validate Data Before Imputation
- [`prepare_data_ice()`](https://openpharma.github.io/rbmiUtils/reference/prepare_data_ice.md)
  : Prepare Intercurrent Event Data
- [`summarise_missingness()`](https://openpharma.github.io/rbmiUtils/reference/summarise_missingness.md)
  : Summarise Missing Data Patterns

## Analysis

Apply analysis functions across imputed datasets

- [`analyse_mi_data()`](https://openpharma.github.io/rbmiUtils/reference/analyse_mi_data.md)
  : Apply Analysis Function to Multiple Imputed Datasets
- [`gcomp_responder()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_responder.md)
  : G-computation Analysis for a Single Visit
- [`gcomp_responder_multi()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_responder_multi.md)
  : G-computation for a Binary Outcome at Multiple Visits
- [`gcomp_binary()`](https://openpharma.github.io/rbmiUtils/reference/gcomp_binary.md)
  : Utility function for Generalized G-computation for Binary Outcomes

## Tidying

Tidy and extract results from pooled objects

- [`tidy_pool_obj()`](https://openpharma.github.io/rbmiUtils/reference/tidy_pool_obj.md)
  : Tidy and Annotate a Pooled Object for Publication
- [`extract_trt_effects()`](https://openpharma.github.io/rbmiUtils/reference/extract_trt_effects.md)
  : Extract Treatment Effect Estimates
- [`extract_lsm()`](https://openpharma.github.io/rbmiUtils/reference/extract_lsm.md)
  : Extract Least Squares Means

## Reporting

Publication-ready tables and plots

- [`efficacy_table()`](https://openpharma.github.io/rbmiUtils/reference/efficacy_table.md)
  : Create Regulatory-Style Efficacy Summary Table
- [`plot_forest()`](https://openpharma.github.io/rbmiUtils/reference/plot_forest.md)
  : Create a Forest Plot from an rbmi Pool Object

## Introspection

Inspect draws, imputations, and MI diagnostics

- [`describe_draws()`](https://openpharma.github.io/rbmiUtils/reference/describe_draws.md)
  : Describe an rbmi Draws Object
- [`describe_imputation()`](https://openpharma.github.io/rbmiUtils/reference/describe_imputation.md)
  : Describe an rbmi Imputation Object

## Formatting

Format values for publication

- [`format_pvalue()`](https://openpharma.github.io/rbmiUtils/reference/format_pvalue.md)
  : Format P-values for Publication
- [`format_estimate()`](https://openpharma.github.io/rbmiUtils/reference/format_estimate.md)
  : Format Estimate with Confidence Interval
- [`format_results_table()`](https://openpharma.github.io/rbmiUtils/reference/format_results_table.md)
  : Format Results Table for Publication
- [`format_results()`](https://openpharma.github.io/rbmiUtils/reference/format_results.md)
  : Format Results for Reporting

## Storage

Efficient storage of imputed datasets

- [`reduce_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/reduce_imputed_data.md)
  : Reduce Imputed Data for Efficient Storage
- [`expand_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/expand_imputed_data.md)
  : Expand Reduced Imputed Data to Full Dataset

## Utilities

Helper functions for imputed data workflows

- [`get_imputed_data()`](https://openpharma.github.io/rbmiUtils/reference/get_imputed_data.md)
  : Get Imputed Data Sets as a data frame
- [`create_impid()`](https://openpharma.github.io/rbmiUtils/reference/create_impid.md)
  : Create IMPID Column for Imputed Datasets
- [`combine_results()`](https://openpharma.github.io/rbmiUtils/reference/combine_results.md)
  : Combine Results Across Multiple Analyses
- [`pool_to_ard()`](https://openpharma.github.io/rbmiUtils/reference/pool_to_ard.md)
  : Convert Pool Object to ARD Format

## Print & Summary Methods

S3 methods for inspecting rbmiUtils objects

- [`print(`*`<analysis>`*`)`](https://openpharma.github.io/rbmiUtils/reference/print.analysis.md)
  : Print Method for Analysis Objects
- [`print(`*`<describe_draws>`*`)`](https://openpharma.github.io/rbmiUtils/reference/print.describe_draws.md)
  : Print Method for describe_draws Objects
- [`print(`*`<describe_imputation>`*`)`](https://openpharma.github.io/rbmiUtils/reference/print.describe_imputation.md)
  : Print Method for describe_imputation Objects
- [`print(`*`<pool>`*`)`](https://openpharma.github.io/rbmiUtils/reference/print.pool.md)
  : Print Method for Pool Objects
- [`summary(`*`<analysis>`*`)`](https://openpharma.github.io/rbmiUtils/reference/summary.analysis.md)
  : Summary Method for Analysis Objects
- [`summary(`*`<pool>`*`)`](https://openpharma.github.io/rbmiUtils/reference/summary.pool.md)
  : Summary Method for Pool Objects

## Datasets

Example datasets for demonstrations

- [`ADEFF`](https://openpharma.github.io/rbmiUtils/reference/ADEFF.md) :
  Example efficacy trial dataset
- [`ADMI`](https://openpharma.github.io/rbmiUtils/reference/ADMI.md) :
  Example multiple imputation trial dataset
