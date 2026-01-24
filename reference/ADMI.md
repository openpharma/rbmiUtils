# Example multiple imputation trial dataset

A simplified example of a simulated trial ADMI dataset

## Usage

``` r
ADMI
```

## Format

`ADMI` A data frame with 100,000 rows and 12 columns:

- USUBJID:

  Unique patient identifier

- STRATA:

  Stratification at randomisation

- REGION:

  Stratification by region

- REGIONC:

  Stratification by region, numeric code

- TRT:

  Planned treatment

- BASE:

  Baseline value of primary outcome variable

- CHG:

  Change from baseline

- AVISIT:

  Visit number

- IMPID:

  Imputation number identifier

- CRIT1FLN:

  Responder criteria (binary)

- CRIT1FL:

  Responder criteria (categorical)

- CRIT:

  Responder criteria (definition)
