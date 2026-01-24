# Construct an rbmi `analysis` object

This is a helper function to create an analysis object that stores the
results from multiple imputation analyses. It validates the results and
ensures proper class assignment.

This is a modification of the rbmi::as_analysis function.

## Usage

``` r
as_analysis2(results, method, delta = NULL, fun = NULL, fun_name = NULL)
```

## Arguments

- results:

  A list containing the analysis results for each imputation.

- method:

  The method object used for the imputation.

- delta:

  Optional. A delta dataset used for adjustment.

- fun:

  The analysis function that was used.

- fun_name:

  The name of the analysis function (used for printing).

## Value

An object of class `analysis` with the results and associated metadata.
