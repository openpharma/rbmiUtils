#' Format P-values for Publication
#'
#' Formats p-values according to common publication standards, with configurable
#' thresholds and decimal places.
#'
#' @param x A numeric vector of p-values.
#' @param digits Integer. Number of decimal places for rounding. Default is 3.
#' @param threshold Numeric. P-values below this threshold are displayed as
#'   "< threshold". Default is 0.001.
#' @param html Logical. If `TRUE`, uses HTML formatting for the less-than symbol.
#'   Default is `FALSE`.
#'
#' @return A character vector of formatted p-values.
#'
#' @details
#' The function applies the following rules:
#' \itemize{
#'   \item P-values below `threshold` are formatted as "< 0.001" (or HTML equivalent)
#'   \item P-values >= `threshold` are rounded to `digits` decimal places
#'   \item `NA` values are preserved as `NA_character_`
#'   \item Values > 1 or < 0 return `NA_character_` with a warning
#' }
#'
#' @examples
#' # Basic usage
#' format_pvalue(0.0234)
#' #> "0.023"
#'
#' format_pvalue(0.00005)
#' #> "< 0.001"
#'
#' # Vector input
#' pvals <- c(0.5, 0.05, 0.001, 0.0001, NA)
#' format_pvalue(pvals)
#' #> "0.500" "0.050" "0.001" "< 0.001" NA
#'
#' # Custom threshold
#' format_pvalue(0.005, threshold = 0.01)
#' #> "< 0.01"
#'
#' # HTML output
#' format_pvalue(0.0001, html = TRUE)
#' #> "&lt; 0.001"
#'
#' @export
format_pvalue <- function(x, digits = 3, threshold = 0.001, html = FALSE) {
  assertthat::assert_that(
    is.numeric(x) || all(is.na(x)),
    msg = "`x` must be a numeric vector"
  )
  assertthat::assert_that(
    is.numeric(digits) && length(digits) == 1 && digits >= 1,
    msg = "`digits` must be a positive integer"
  )
  assertthat::assert_that(
    is.numeric(threshold) && length(threshold) == 1 && threshold > 0,
    msg = "`threshold` must be a positive number"

  )

  # Check for invalid p-values

invalid_idx <- which(x < 0 | x > 1)
  if (length(invalid_idx) > 0) {
    warning(
      sprintf("Found %d p-value(s) outside [0, 1]; returning NA for these", length(invalid_idx)),
      call. = FALSE
    )
  }

  lt_symbol <- if (html) "&lt;" else "<"
  threshold_fmt <- format(threshold, nsmall = digits, scientific = FALSE)

  vapply(x, function(p) {
    if (is.na(p)) {
      return(NA_character_)
    }
    if (p < 0 || p > 1) {
      return(NA_character_)
    }
    if (p < threshold) {
      return(paste(lt_symbol, threshold_fmt))
    }
    formatC(p, digits = digits, format = "f")
  }, character(1))
}


#' Format Estimate with Confidence Interval
#'
#' Formats a point estimate with its confidence interval in standard publication
#' format: "estimate (lower, upper)".
#'
#' @param estimate Numeric vector of point estimates.
#' @param lower Numeric vector of lower confidence interval bounds.
#' @param upper Numeric vector of upper confidence interval bounds.
#' @param digits Integer. Number of decimal places for rounding. Default is 2.
#' @param sep Character. Separator between lower and upper bounds. Default is ", ".
#'
#' @return A character vector of formatted estimates with confidence intervals.
#'
#' @details
#' The function formats estimates as "X.XX (X.XX, X.XX)" by default. All three
#' input vectors must have the same length. `NA` values in any position result
#' in `NA_character_` for that element.
#'
#' @examples
#' # Single estimate
#' format_estimate(1.5, 0.8, 2.2)
#' #> "1.50 (0.80, 2.20)"
#'
#' # Multiple estimates
#' format_estimate(
#'   estimate = c(-2.5, -1.8),
#'   lower = c(-4.0, -3.2),
#'   upper = c(-1.0, -0.4)
#' )
#' #> "-2.50 (-4.00, -1.00)" "-1.80 (-3.20, -0.40)"
#'
#' # More decimal places
#' format_estimate(0.234, 0.123, 0.345, digits = 3)
#' #> "0.234 (0.123, 0.345)"
#'
#' # Different separator
#' format_estimate(1.5, 0.8, 2.2, sep = " to ")
#' #> "1.50 (0.80 to 2.20)"
#'
#' @export
format_estimate <- function(estimate, lower, upper, digits = 2, sep = ", ") {
  assertthat::assert_that(
    is.numeric(estimate) || all(is.na(estimate)),
    msg = "`estimate` must be a numeric vector"
  )
  assertthat::assert_that(
    is.numeric(lower) || all(is.na(lower)),
    msg = "`lower` must be a numeric vector"
  )
  assertthat::assert_that(
    is.numeric(upper) || all(is.na(upper)),
    msg = "`upper` must be a numeric vector"
  )
  assertthat::assert_that(
    length(estimate) == length(lower) && length(estimate) == length(upper),
    msg = "`estimate`, `lower`, and `upper` must have the same length"
  )
  assertthat::assert_that(
    is.numeric(digits) && length(digits) == 1 && digits >= 0,
    msg = "`digits` must be a non-negative integer"
  )
  assertthat::assert_that(
    is.character(sep) && length(sep) == 1,
    msg = "`sep` must be a single character string"
  )

  mapply(function(est, lwr, upr) {
    if (is.na(est) || is.na(lwr) || is.na(upr)) {
      return(NA_character_)
    }
    sprintf(
      "%s (%s%s%s)",
      formatC(est, digits = digits, format = "f"),
      formatC(lwr, digits = digits, format = "f"),
      sep,
      formatC(upr, digits = digits, format = "f")
    )
  }, estimate, lower, upper, USE.NAMES = FALSE)
}


#' Format Results Table for Publication
#'
#' Adds formatted columns to a tidy results table, creating publication-ready
#' output with properly formatted estimates, confidence intervals, and p-values.
#'
#' @param data A data.frame or tibble, typically output from [tidy_pool_obj()].
#' @param est_col Character. Name of the estimate column. Default is "est".
#' @param lci_col Character. Name of the lower CI column. Default is "lci".
#' @param uci_col Character. Name of the upper CI column. Default is "uci".
#' @param pval_col Character. Name of the p-value column. Default is "pval".
#' @param est_digits Integer. Decimal places for estimates. Default is 2.
#' @param pval_digits Integer. Decimal places for p-values. Default is 3.
#' @param pval_threshold Numeric. Threshold for p-value formatting. Default is 0.001.
#' @param ci_sep Character. Separator for CI bounds. Default is ", ".
#'
#' @return A tibble with additional formatted columns:
#' \describe{
#'   \item{est_ci}{Formatted estimate with confidence interval}
#'   \item{pval_fmt}{Formatted p-value}
#' }
#'
#' @details
#' This function is designed to work with output from [tidy_pool_obj()] but
#' can be used with any data.frame containing estimate, CI, and p-value columns.
#' The original columns are preserved; new formatted columns are added.
#'
#' @examples
#' library(dplyr)
#'
#' # Create example results
#' results <- tibble::tibble(
#'   parameter = c("trt_Week24", "lsm_ref_Week24", "lsm_alt_Week24"),
#'   description = c("Treatment Effect", "LS Mean (Reference)", "LS Mean (Treatment)"),
#'   est = c(-2.45, 5.20, 2.75),
#'   se = c(0.89, 0.65, 0.71),
#'   lci = c(-4.20, 3.93, 1.36),
#'   uci = c(-0.70, 6.47, 4.14),
#'   pval = c(0.006, NA, NA)
#' )
#'
#' # Format for publication
#' formatted <- format_results_table(results)
#' print(formatted[, c("description", "est_ci", "pval_fmt")])
#'
#' @export
format_results_table <- function(
    data,
    est_col = "est",
    lci_col = "lci",
    uci_col = "uci",
    pval_col = "pval",
    est_digits = 2,
    pval_digits = 3,
    pval_threshold = 0.001,
    ci_sep = ", "
) {
  assertthat::assert_that(
    is.data.frame(data),
    msg = "`data` must be a data.frame"
  )

  # Check required columns exist
  required_cols <- c(est_col, lci_col, uci_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Column(s) not found in `data`: %s",
      paste0("`", missing_cols, "`", collapse = ", ")
    ), call. = FALSE)
  }

  # Format estimate with CI
  data$est_ci <- format_estimate(
    estimate = data[[est_col]],
    lower = data[[lci_col]],
    upper = data[[uci_col]],
    digits = est_digits,
    sep = ci_sep
  )

  # Format p-value if column exists
  if (pval_col %in% names(data)) {
    data$pval_fmt <- format_pvalue(
      data[[pval_col]],
      digits = pval_digits,
      threshold = pval_threshold
    )
  }

  dplyr::as_tibble(data)
}
