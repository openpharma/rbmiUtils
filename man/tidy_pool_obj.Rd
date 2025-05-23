% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidiers.R
\name{tidy_pool_obj}
\alias{tidy_pool_obj}
\title{Tidy and Annotate a Pooled Object for Publication}
\usage{
tidy_pool_obj(pool_obj)
}
\arguments{
\item{pool_obj}{A pooled analysis object of class \code{pool}.}
}
\value{
A tibble containing the processed pooled analysis results. The tibble includes columns for the parameter,
description, estimates, standard errors, confidence intervals, p-values, visit, parameter type, and least squares mean type.
}
\description{
This function processes a pooled analysis object of class \code{pool} into a tidy tibble format.
It adds contextual information, such as whether a parameter is a treatment comparison or a least squares mean,
dynamically identifies visit names from the \code{parameter} column, and provides additional columns for parameter type,
least squares mean type, and visit.
}
\details{
The function rounds numeric columns to three decimal places for presentation. It dynamically processes
the \code{parameter} column by separating it into components (e.g., type of estimate, reference vs. alternative arm, and visit),
and provides informative descriptions in the output.
}
\examples{
# Example usage:
library(dplyr)
library(rbmi)

data("ADMI")
N_IMPUTATIONS <- 100
BURN_IN <- 200
BURN_BETWEEN <- 5

# Convert key columns to factors
ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
ADMI$USUBJID <- factor(ADMI$USUBJID)
ADMI$AVISIT <- factor(ADMI$AVISIT)

# Define key variables for ANCOVA analysis
 vars <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CHG",
  covariates = c("BASE", "STRATA", "REGION")  # Covariates for adjustment
 )

# Specify the imputation method (Bayesian) - need for pool step
method <- rbmi::method_bayes(
  n_samples = N_IMPUTATIONS,
  control = rbmi::control_bayes(
    warmup = BURN_IN,
    thin = BURN_BETWEEN
    )
  )

# Perform ANCOVA Analysis on Each Imputed Dataset
ana_obj_ancova <- analyse_mi_data(
  data = ADMI,
  vars = vars,
  method = method,
  fun = ancova,  # Apply ANCOVA
  delta = NULL   # No sensitivity analysis adjustment
)

pool_obj_ancova <- pool(ana_obj_ancova)
tidy_df <- tidy_pool_obj(pool_obj_ancova)

# Print tidy data frames
print(tidy_df)

}
