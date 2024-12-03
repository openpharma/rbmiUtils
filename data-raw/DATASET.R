# Hypothetical Phase 3 trial from simulated data ------------------------

# load data - local version
ADEFF <-
  readr::read_csv("data-raw/extdata/ADEFF.csv",
                  show_col_types = FALSE
  ) |>
  dplyr::as_tibble()

# attach var labels
attr(ADEFF$USUBJID, "label") <- "Unique patient identifier"
attr(ADEFF$AVAL, "label") <- "Primary outcome variable"
attr(ADEFF$STRATA, "label") <- "Stratification at randomisation"
attr(ADEFF$REGION, "label") <- "Stratification by region"
attr(ADEFF$REGIONC, "label") <- "Stratification by region, numeric code"
attr(ADEFF$TRT01P, "label") <- "Planned treatment"
attr(ADEFF$BASE, "label") <- "Baseline value of primary outcome variable"
attr(ADEFF$CHG, "label") <- "Change from baseline"
attr(ADEFF$AVISIT, "label") <- "Visit number"
attr(ADEFF$PARAM, "label") <- "Analysis parameter name"


# Hypothetical Phase 3 trial from simulated ADMI data ------------------------

# load data - local version
ADMI <-
  readr::read_csv("data-raw/extdata/ADMI.csv",
                  show_col_types = FALSE
  ) |>
  dplyr::as_tibble() |>
  dplyr::select(- internal_id)

glimpse(ADMI)

# attach var labels
attr(ADMI$USUBJID, "label") <- "Unique patient identifier"
attr(ADMI$STRATA, "label") <- "Stratification at randomisation"
attr(ADMI$REGION, "label") <- "Stratification by region"
attr(ADMI$REGIONC, "label") <- "Stratification by region, numeric code"
attr(ADMI$TRT, "label") <- "Planned treatment"
attr(ADMI$BASE, "label") <- "Baseline value of primary outcome variable"
attr(ADMI$CHG, "label") <- "Change from baseline"
attr(ADMI$AVISIT, "label") <- "Visit number"
attr(ADMI$IMPID, "label") <- "Imputation number identifier"
attr(ADMI$CRIT1FLN, "label") <- "Responder criteria (binary)"
attr(ADMI$CRIT1FL, "label") <- "Responder criteria (categorical)"
attr(ADMI$CRIT, "label") <- "Responder criteria (definition)"

usethis::use_data(ADEFF, ADMI, overwrite = TRUE)
