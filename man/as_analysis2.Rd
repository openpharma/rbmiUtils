% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyse_mi_data.R
\name{as_analysis2}
\alias{as_analysis2}
\title{Construct an rbmi \code{analysis} object}
\usage{
as_analysis2(results, method, delta = NULL, fun = NULL, fun_name = NULL)
}
\arguments{
\item{results}{A list containing the analysis results for each imputation.}

\item{method}{The method object used for the imputation.}

\item{delta}{Optional. A delta dataset used for adjustment.}

\item{fun}{The analysis function that was used.}

\item{fun_name}{The name of the analysis function (used for printing).}
}
\value{
An object of class \code{analysis} with the results and associated metadata.
}
\description{
This is a helper function to create an analysis object that stores the
results from multiple imputation analyses. It validates the results and
ensures proper class assignment.

This is a modification of the rbmi::as_analysis function.
}
