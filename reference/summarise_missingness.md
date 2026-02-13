# Summarise Missing Data Patterns

Tabulates missing outcome data by visit and treatment group, and
classifies each subject's missing data pattern as complete, monotone, or
intermittent.

## Usage

``` r
summarise_missingness(data, vars)
```

## Arguments

- data:

  A data.frame containing the analysis dataset with one row per
  subject-visit combination.

- vars:

  A `vars` object as created by
  [`rbmi::set_vars()`](https://openpharma.github.io/rbmi/latest-tag/reference/set_vars.html).

## Value

A list with three components:

- by_visit:

  A tibble with columns: visit, group, n, n_miss, pct_miss

- patterns:

  A tibble with columns: subjid, group, pattern ("complete", "monotone",
  or "intermittent"), dropout_visit (NA if not monotone)

- summary:

  A tibble with columns: group, n_subjects, n_complete, n_monotone,
  n_intermittent

## See also

- [`rbmi::draws()`](https://openpharma.github.io/rbmi/latest-tag/reference/draws.html)
  for imputation after reviewing missingness patterns

- [`validate_data()`](https://openpharma.github.io/rbmiUtils/reference/validate_data.md)
  to check data before imputation

- [`prepare_data_ice()`](https://openpharma.github.io/rbmiUtils/reference/prepare_data_ice.md)
  to create intercurrent event data from flags

## Examples

``` r
library(rbmi)

dat <- data.frame(
  USUBJID = factor(rep(c("S1", "S2", "S3", "S4"), each = 3)),
  AVISIT = factor(rep(c("Week 4", "Week 8", "Week 12"), 4),
                  levels = c("Week 4", "Week 8", "Week 12")),
  TRT = factor(rep(c("Placebo", "Drug A"), each = 6)),
  CHG = c(1, 2, 3, 1, NA, NA, 1, 2, NA, 1, NA, 2)
)

vars <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CHG"
)

result <- summarise_missingness(dat, vars)
print(result$by_visit)
#> # A tibble: 6 × 5
#>   visit   group       n n_miss pct_miss
#>   <fct>   <fct>   <int>  <int>    <dbl>
#> 1 Week 4  Drug A      2      0        0
#> 2 Week 4  Placebo     2      0        0
#> 3 Week 8  Drug A      2      1       50
#> 4 Week 8  Placebo     2      1       50
#> 5 Week 12 Drug A      2      1       50
#> 6 Week 12 Placebo     2      1       50
print(result$patterns)
#> # A tibble: 4 × 4
#>   USUBJID TRT     pattern      dropout_visit
#>   <fct>   <chr>   <chr>        <chr>        
#> 1 S1      Placebo complete     NA           
#> 2 S2      Placebo monotone     Week 8       
#> 3 S3      Drug A  monotone     Week 12      
#> 4 S4      Drug A  intermittent NA           
print(result$summary)
#> # A tibble: 2 × 5
#>   group   n_subjects n_complete n_monotone n_intermittent
#>   <chr>        <int>      <int>      <int>          <int>
#> 1 Drug A           2          0          1              1
#> 2 Placebo          2          1          1              0
```
