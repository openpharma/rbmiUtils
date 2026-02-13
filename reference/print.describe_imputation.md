# Print Method for describe_imputation Objects

Displays a formatted summary of an imputation description using cli
formatting, including method, number of imputations, reference arm
mappings, and a missingness breakdown by visit and treatment arm.

## Usage

``` r
# S3 method for class 'describe_imputation'
print(x, ...)
```

## Arguments

- x:

  A `describe_imputation` object from
  [`describe_imputation()`](https://openpharma.github.io/rbmiUtils/reference/describe_imputation.md).

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns `x` (for pipe chaining).

## Examples

``` r
if (FALSE) { # \dontrun{
# After creating impute_obj via the rbmi pipeline (see describe_imputation):
desc <- describe_imputation(impute_obj)
print(desc)  # Formatted cli output with method, M, subjects, references, missingness
} # }
```
