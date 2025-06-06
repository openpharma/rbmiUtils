% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis_utils.R
\name{gcomp_responder}
\alias{gcomp_responder}
\title{G-computation Analysis for a Single Visit}
\usage{
gcomp_responder(
  data,
  vars,
  reference_levels = NULL,
  var_method = "Ge",
  type = "HC0",
  contrast = "diff"
)
}
\arguments{
\item{data}{A data.frame with one visit of data.}

\item{vars}{A list containing \code{group}, \code{outcome}, \code{covariates}, and \code{visit}.}

\item{reference_levels}{Optional vector specifying reference level(s) of the treatment factor.}

\item{var_method}{Marginal variance estimation method (default: "Ge").}

\item{type}{Type of robust variance estimator (default: "HC0").}

\item{contrast}{Type of contrast to compute (default: "diff").}
}
\value{
A named list containing estimates and standard errors for treatment comparisons and within-arm means.
}
\description{
Performs logistic regression and estimates marginal effects for binary outcomes.
}
