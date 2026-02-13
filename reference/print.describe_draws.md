# Print Method for describe_draws Objects

Displays a formatted summary of a draws description using cli
formatting.

## Usage

``` r
# S3 method for class 'describe_draws'
print(x, ...)
```

## Arguments

- x:

  A `describe_draws` object from
  [`describe_draws()`](https://openpharma.github.io/rbmiUtils/reference/describe_draws.md).

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns `x` (for pipe chaining).

## Examples

``` r
if (FALSE) { # \dontrun{
# After creating draws_obj via the rbmi pipeline (see describe_draws):
desc <- describe_draws(draws_obj)
print(desc)  # Formatted cli output with method, formula, samples, convergence
} # }
```
