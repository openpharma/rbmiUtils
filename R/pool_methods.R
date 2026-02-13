#' Print Method for Pool Objects
#'
#' Displays a formatted summary of a pooled analysis object from [rbmi::pool()].
#' Uses cli formatting to show rounded estimates, confidence intervals, parameter
#' labels, method information, number of imputations, and confidence level.
#'
#' @param x An object of class `pool`, typically obtained from [rbmi::pool()].
#' @param digits Integer. Number of decimal places for rounding estimates,
#'   standard errors, and confidence interval bounds. Default is 2.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the original pool object `x` (for pipe chaining).
#'
#' @details
#' This method overrides [rbmi::print.pool()] to provide enhanced, formatted
#' console output using the cli package. The override produces a
#' "Registered S3 method overwritten" message at package load time, which is
#' expected and harmless (same pattern as [print.analysis()]).
#'
#' The output includes:
#' \itemize{
#'   \item A header with parameter and visit counts
#'   \item Metadata: pooling method, number of imputations, confidence level
#'   \item A compact results table with key columns: parameter, visit, est, lci, uci, pval
#' }
#'
#' @seealso
#' * [tidy_pool_obj()] for full tidy tibble output
#' * [summary.pool()] for visit-level breakdown with significance flags
#' * [rbmi::pool()] to create pool objects
#'
#' @examples
#' \donttest{
#' library(rbmi)
#' library(rbmiUtils)
#' data("ADMI")
#'
#' ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
#' ADMI$USUBJID <- factor(ADMI$USUBJID)
#' ADMI$AVISIT <- factor(ADMI$AVISIT)
#'
#' vars <- set_vars(
#'   subjid = "USUBJID", visit = "AVISIT", group = "TRT",
#'   outcome = "CHG", covariates = c("BASE", "STRATA", "REGION")
#' )
#' method <- method_bayes(n_samples = 20, control = control_bayes(warmup = 20))
#'
#' ana_obj <- analyse_mi_data(ADMI, vars, method, fun = ancova)
#' pool_obj <- pool(ana_obj)
#' print(pool_obj)
#' }
#'
#' @export
print.pool <- function(x, digits = 2, ...) {
  tidy_df <- tidy_pool_obj(x)
  n_params <- nrow(tidy_df)
  visits <- unique(tidy_df$visit)
  n_visits <- length(visits[!is.na(visits)])

  cli::cli_h1("Pool Object")
  cli::cli_text("{n_params} parameter{?s} across {n_visits} visit{?s}")

  # Metadata
  if (!is.null(x$method)) {
    cli::cli_text("{.field Method}: {x$method}")
  }

  if (!is.null(x$N)) {
    cli::cli_text("{.field N imputations}: {x$N}")
  }

  if (!is.null(x$conf.level)) {
    cli::cli_text("{.field Confidence}: {x$conf.level * 100}%")
  }

  cli::cli_rule()

  # Build compact display data frame
  display_df <- data.frame(
    parameter = tidy_df$parameter,
    visit = tidy_df$visit,
    est = round(tidy_df$est, digits),
    lci = round(tidy_df$lci, digits),
    uci = round(tidy_df$uci, digits),
    pval = format_pvalue(tidy_df$pval),
    stringsAsFactors = FALSE
  )

  print(display_df, row.names = FALSE)

  invisible(x)
}


#' Summary Method for Pool Objects
#'
#' Provides a detailed visit-level breakdown of pooled analysis results with
#' significance flags. Shows treatment comparisons and least squares means
#' grouped by visit.
#'
#' @param object An object of class `pool`, typically obtained from [rbmi::pool()].
#' @param alpha Numeric. Significance threshold for flagging p-values.
#'   Default is 0.05. Flags are: `*` for p < alpha, `**` for p < 0.01,
#'   `***` for p < 0.001.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns a list with:
#' \describe{
#'   \item{n_parameters}{Number of parameters in the pool object}
#'   \item{visits}{Character vector of unique visit names}
#'   \item{method}{Pooling method used}
#'   \item{n_imputations}{Number of imputations combined}
#'   \item{conf.level}{Confidence level}
#'   \item{tidy_df}{The full tidy tibble from [tidy_pool_obj()]}
#' }
#'
#' @details
#' The summary output groups results by visit, showing treatment comparisons
#' with significance flags and least squares means. This provides a quick
#' overview of which visits have statistically significant treatment effects.
#'
#' Significance flags:
#' \itemize{
#'   \item `*` p < alpha (default 0.05)
#'   \item `**` p < 0.01
#'   \item `***` p < 0.001
#' }
#'
#' @seealso
#' * [print.pool()] for compact tabular output
#' * [tidy_pool_obj()] for full tidy tibble output
#' * [rbmi::pool()] to create pool objects
#'
#' @examples
#' \donttest{
#' library(rbmi)
#' library(rbmiUtils)
#' data("ADMI")
#'
#' ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
#' ADMI$USUBJID <- factor(ADMI$USUBJID)
#' ADMI$AVISIT <- factor(ADMI$AVISIT)
#'
#' vars <- set_vars(
#'   subjid = "USUBJID", visit = "AVISIT", group = "TRT",
#'   outcome = "CHG", covariates = c("BASE", "STRATA", "REGION")
#' )
#' method <- method_bayes(n_samples = 20, control = control_bayes(warmup = 20))
#'
#' ana_obj <- analyse_mi_data(ADMI, vars, method, fun = ancova)
#' pool_obj <- pool(ana_obj)
#' summary(pool_obj)
#' }
#'
#' @export
summary.pool <- function(object, alpha = 0.05, ...) {
  tidy_df <- tidy_pool_obj(object)
  n_params <- nrow(tidy_df)
  visits <- unique(tidy_df$visit)
  n_visits <- length(visits[!is.na(visits)])

  cli::cli_h1("Pool Object Summary")

  # Metadata section
  if (!is.null(object$method)) {
    cli::cli_text("{.field Method}: {object$method}")
  }

  if (!is.null(object$N)) {
    cli::cli_text("{.field N imputations}: {object$N}")
  }

  if (!is.null(object$conf.level)) {
    cli::cli_text("{.field Confidence}: {object$conf.level * 100}%")
  }

  if (!is.null(object$alternative)) {
    cli::cli_text("{.field Alternative}: {object$alternative}")
  }

  cli::cli_text("{n_params} parameter{?s} across {n_visits} visit{?s}")
  cli::cli_rule()

  # Visit-level breakdown
  for (v in visits) {
    if (is.na(v)) next
    cli::cli_h2(v)

    visit_df <- tidy_df[tidy_df$visit == v & !is.na(tidy_df$visit), ]

    # Treatment comparison rows
    trt_rows <- visit_df[visit_df$parameter_type == "trt", ]
    if (nrow(trt_rows) > 0) {
      cli::cli_text("{.strong Treatment Comparisons}")
      for (i in seq_len(nrow(trt_rows))) {
        sig_flag <- ""
        if (!is.na(trt_rows$pval[i])) {
          if (trt_rows$pval[i] < 0.001) {
            sig_flag <- " ***"
          } else if (trt_rows$pval[i] < 0.01) {
            sig_flag <- " **"
          } else if (trt_rows$pval[i] < alpha) {
            sig_flag <- " *"
          }
        }
        est_fmt <- round(trt_rows$est[i], 2)
        lci_fmt <- round(trt_rows$lci[i], 2)
        uci_fmt <- round(trt_rows$uci[i], 2)
        pval_fmt <- format_pvalue(trt_rows$pval[i])
        cli::cli_text(
          "  {trt_rows$description[i]}: {est_fmt} ({lci_fmt}, {uci_fmt}) p={pval_fmt}{sig_flag}"
        )
      }
    }

    # LSM rows
    lsm_rows <- visit_df[visit_df$parameter_type == "lsm", ]
    if (nrow(lsm_rows) > 0) {
      cli::cli_text("{.strong Least Squares Means}")
      for (i in seq_len(nrow(lsm_rows))) {
        est_fmt <- round(lsm_rows$est[i], 2)
        lci_fmt <- round(lsm_rows$lci[i], 2)
        uci_fmt <- round(lsm_rows$uci[i], 2)
        cli::cli_text(
          "  {lsm_rows$description[i]}: {est_fmt} ({lci_fmt}, {uci_fmt})"
        )
      }
    }
  }

  invisible(list(
    n_parameters = n_params,
    visits = visits,
    method = object$method,
    n_imputations = object$N,
    conf.level = object$conf.level,
    tidy_df = tidy_df
  ))
}
