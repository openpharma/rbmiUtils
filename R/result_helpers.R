#' Create IMPID Column for Imputed Datasets
#'
#' Adds an `IMPID` column to a list of imputed datasets, converting them to a
#' single stacked data.frame suitable for use with [analyse_mi_data()].
#'
#' @param imputed_list A list of data.frames, where each element represents one
#'   imputed dataset.
#' @param id_prefix Optional character prefix for IMPID values. Default is empty string.
#'
#' @return A single data.frame with all imputed datasets stacked, with an `IMPID`
#'   column identifying the source imputation.
#'
#' @details
#' This function is useful when you have imputed datasets from a source other than
#' rbmi (e.g., from mice or another MI package) and want to use them with rbmiUtils
#' analysis functions.
#'
#' @examples
#' # Create example imputed datasets
#' imp1 <- data.frame(USUBJID = c("S1", "S2"), CHG = c(1.5, 2.5))
#' imp2 <- data.frame(USUBJID = c("S1", "S2"), CHG = c(1.8, 2.2))
#' imp3 <- data.frame(USUBJID = c("S1", "S2"), CHG = c(1.6, 2.4))
#'
#' # Stack with IMPID
#' stacked <- create_impid(list(imp1, imp2, imp3))
#' print(stacked)
#'
#' @seealso
#' * [get_imputed_data()] to extract imputed data from rbmi objects
#' * [analyse_mi_data()] to analyse stacked imputed data
#'
#' @export
create_impid <- function(imputed_list, id_prefix = "") {

  assertthat::assert_that(
    is.list(imputed_list),
    msg = "`imputed_list` must be a list of data.frames"
  )

  if (length(imputed_list) == 0) {
    stop("`imputed_list` cannot be empty", call. = FALSE)
  }

  assertthat::assert_that(
    all(sapply(imputed_list, is.data.frame)),
    msg = "All elements of `imputed_list` must be data.frames"
  )

  assertthat::assert_that(
    is.character(id_prefix) && length(id_prefix) == 1,
    msg = "`id_prefix` must be a single character string"
  )

  # Add IMPID to each dataset
  result_list <- lapply(seq_along(imputed_list), function(i) {
    df <- imputed_list[[i]]
    df$IMPID <- paste0(id_prefix, i)
    df
  })

  # Stack all datasets
  result <- dplyr::bind_rows(result_list)

  # Move IMPID to first column
  result <- result[, c("IMPID", setdiff(names(result), "IMPID")), drop = FALSE]

  result
}


#' Combine Results Across Multiple Analyses
#'
#' Combines tidy result tibbles from multiple analyses (e.g., different endpoints
#' or subgroups) into a single table with an identifying column.
#'
#' @param ... Named arguments where each is a tidy result tibble from [tidy_pool_obj()].
#' @param results_list Alternative to `...`: a named list of tidy result tibbles.
#' @param id_col Character string specifying the name of the identifier column.
#'   Default is "analysis".
#'
#' @return A tibble with all results combined, with an additional column identifying
#'   the source analysis.
#'
#' @examples
#' \donttest{
#' library(rbmi)
#' library(dplyr)
#'
#' # Assuming you have multiple pooled results
#' # results_week24 <- tidy_pool_obj(pool_obj_week24)
#' # results_week48 <- tidy_pool_obj(pool_obj_week48)
#'
#' # Combine them
#' # combined <- combine_results(
#' #   "Week 24" = results_week24,
#' #   "Week 48" = results_week48
#' # )
#' }
#'
#' @seealso
#' * [tidy_pool_obj()] to create tidy results from pooled objects
#' * [format_results()] to format combined results for reporting
#'
#' @export
combine_results <- function(..., results_list = NULL, id_col = "analysis") {

  assertthat::assert_that(
    is.character(id_col) && length(id_col) == 1,
    msg = "`id_col` must be a single character string"
  )

  # Collect results from ... or results_list
  if (!is.null(results_list)) {
    assertthat::assert_that(
      is.list(results_list),
      msg = "`results_list` must be a named list"
    )
    results <- results_list
  } else {
    results <- list(...)
  }

  if (length(results) == 0) {
    stop("No results provided to combine", call. = FALSE)
  }

  # Check all are data.frames/tibbles
  if (!all(sapply(results, function(x) is.data.frame(x)))) {
    stop("All results must be data.frames or tibbles", call. = FALSE)
  }

  # Get names
  result_names <- names(results)
  if (is.null(result_names) || any(result_names == "")) {
    result_names <- paste0("Analysis_", seq_along(results))
    names(results) <- result_names
  }

  # Add identifier and combine
  combined_list <- lapply(names(results), function(nm) {
    df <- results[[nm]]
    df[[id_col]] <- nm
    df
  })

  combined <- dplyr::bind_rows(combined_list)

  # Move id_col to first column
  combined <- combined[, c(id_col, setdiff(names(combined), id_col)), drop = FALSE]

  dplyr::as_tibble(combined)
}


#' Format Results for Reporting
#'
#' Formats a tidy results tibble for publication-ready reporting, with options
#' for rounding, confidence interval formatting, and column selection.
#'
#' @param results A tibble from [tidy_pool_obj()] or [combine_results()].
#' @param digits Integer specifying the number of decimal places for estimates.
#'   Default is 2.
#' @param ci_format Character string specifying CI format. Options are:
#'   "parens" for "(LCI, UCI)", "brackets" for "[LCI, UCI]", or "dash" for
#'   "LCI - UCI". Default is "parens".
#' @param pval_digits Integer specifying decimal places for p-values. Default is 3.
#' @param include_se Logical indicating whether to include standard error column.
#'   Default is FALSE.
#'
#' @return A tibble with formatted columns suitable for reporting.
#'
#' @examples
#' \donttest{
#' library(rbmi)
#'
#' # Assuming you have a tidy result
#' # tidy_result <- tidy_pool_obj(pool_obj)
#' # formatted <- format_results(tidy_result, digits = 3, ci_format = "brackets")
#' }
#'
#' @seealso
#' * [tidy_pool_obj()] to create tidy results
#' * [combine_results()] to combine multiple analyses
#'
#' @export
format_results <- function(
  results,
  digits = 2,
  ci_format = c("parens", "brackets", "dash"),
  pval_digits = 3,
  include_se = FALSE
) {

  assertthat::assert_that(
    is.data.frame(results),
    msg = "`results` must be a data.frame or tibble"
  )

  assertthat::assert_that(
    is.numeric(digits) && length(digits) == 1 && digits >= 0,
    msg = "`digits` must be a non-negative integer"
  )

  assertthat::assert_that(
    is.numeric(pval_digits) && length(pval_digits) == 1 && pval_digits >= 0,
    msg = "`pval_digits` must be a non-negative integer"
  )

  ci_format <- match.arg(ci_format)

  # Check required columns
  required_cols <- c("est", "lci", "uci")
  missing_cols <- setdiff(required_cols, names(results))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Missing required columns: %s",
      paste0("`", missing_cols, "`", collapse = ", ")
    ), call. = FALSE)
  }

  # Format estimate
  results$estimate <- sprintf(paste0("%.", digits, "f"), results$est)

  # Format CI
  lci_fmt <- sprintf(paste0("%.", digits, "f"), results$lci)
  uci_fmt <- sprintf(paste0("%.", digits, "f"), results$uci)

  results$ci <- switch(
    ci_format,
    "parens" = paste0("(", lci_fmt, ", ", uci_fmt, ")"),
    "brackets" = paste0("[", lci_fmt, ", ", uci_fmt, "]"),
    "dash" = paste0(lci_fmt, " - ", uci_fmt)
  )

  # Format p-value
  if ("pval" %in% names(results)) {
    results$p_value <- ifelse(
      results$pval < 10^(-pval_digits),
      paste0("<", format(10^(-pval_digits), scientific = FALSE)),
      sprintf(paste0("%.", pval_digits, "f"), results$pval)
    )
  }

  # Format SE if present and requested
  if (include_se && "se" %in% names(results)) {
    results$std_error <- sprintf(paste0("%.", digits, "f"), results$se)
  }

  # Select and arrange output columns
  out_cols <- c()

  # Include identifier columns if present
  if ("analysis" %in% names(results)) out_cols <- c(out_cols, "analysis")
  if ("description" %in% names(results)) out_cols <- c(out_cols, "description")
  if ("visit" %in% names(results)) out_cols <- c(out_cols, "visit")
  if ("parameter_type" %in% names(results)) out_cols <- c(out_cols, "parameter_type")

  # Add formatted columns
  out_cols <- c(out_cols, "estimate")
  if (include_se && "std_error" %in% names(results)) out_cols <- c(out_cols, "std_error")
  out_cols <- c(out_cols, "ci")
  if ("p_value" %in% names(results)) out_cols <- c(out_cols, "p_value")

  results <- results[, out_cols, drop = FALSE]

  dplyr::as_tibble(results)
}


#' Extract Treatment Effect Estimates
#'
#' Convenience function to extract only treatment comparison estimates from
#' tidy results, filtering out least squares means.
#'
#' @param results A tibble from [tidy_pool_obj()].
#' @param visit Optional character vector of visits to filter. If NULL (default),
#'   returns results for all visits.
#'
#' @return A tibble containing only treatment effect rows.
#'
#' @examples
#' \donttest{
#' library(rbmi)
#'
#' # Assuming you have a tidy result
#' # tidy_result <- tidy_pool_obj(pool_obj)
#' # trt_effects <- extract_trt_effects(tidy_result)
#' # trt_week24 <- extract_trt_effects(tidy_result, visit = "Week 24")
#' }
#'
#' @seealso
#' * [tidy_pool_obj()] to create tidy results
#' * [extract_lsm()] to extract least squares means
#'
#' @export
extract_trt_effects <- function(results, visit = NULL) {

  assertthat::assert_that(
    is.data.frame(results),
    msg = "`results` must be a data.frame or tibble"
  )

  if (!"parameter_type" %in% names(results)) {
    stop("`results` must contain a `parameter_type` column (from `tidy_pool_obj()`)",
         call. = FALSE)
  }

  # Filter to treatment effects
  out <- results[results$parameter_type == "trt", , drop = FALSE]

  # Filter by visit if specified
  if (!is.null(visit)) {
    assertthat::assert_that(
      is.character(visit),
      msg = "`visit` must be a character vector"
    )
    out <- out[out$visit %in% visit, , drop = FALSE]
  }

  dplyr::as_tibble(out)
}


#' Extract Least Squares Means
#'
#' Convenience function to extract only least squares mean estimates from
#' tidy results.
#'
#' @param results A tibble from [tidy_pool_obj()].
#' @param visit Optional character vector of visits to filter. If NULL (default),
#'   returns results for all visits.
#' @param arm Optional character: "ref" for reference arm, "alt" for alternative arm,
#'   or NULL (default) for both.
#'
#' @return A tibble containing only LSM rows.
#'
#' @examples
#' \donttest{
#' library(rbmi)
#'
#' # Assuming you have a tidy result
#' # tidy_result <- tidy_pool_obj(pool_obj)
#' # all_lsm <- extract_lsm(tidy_result)
#' # ref_lsm <- extract_lsm(tidy_result, arm = "ref")
#' }
#'
#' @seealso
#' * [tidy_pool_obj()] to create tidy results
#' * [extract_trt_effects()] to extract treatment effects
#'
#' @export
extract_lsm <- function(results, visit = NULL, arm = NULL) {

  assertthat::assert_that(
    is.data.frame(results),
    msg = "`results` must be a data.frame or tibble"
  )

  if (!"parameter_type" %in% names(results)) {
    stop("`results` must contain a `parameter_type` column (from `tidy_pool_obj()`)",
         call. = FALSE)
  }

  # Filter to LSM
  out <- results[results$parameter_type == "lsm", , drop = FALSE]

  # Filter by visit if specified
  if (!is.null(visit)) {
    assertthat::assert_that(
      is.character(visit),
      msg = "`visit` must be a character vector"
    )
    out <- out[out$visit %in% visit, , drop = FALSE]
  }

  # Filter by arm if specified
  if (!is.null(arm)) {
    assertthat::assert_that(
      is.character(arm) && length(arm) == 1 && arm %in% c("ref", "alt"),
      msg = "`arm` must be 'ref' or 'alt'"
    )
    out <- out[out$lsm_type == arm, , drop = FALSE]
  }

  dplyr::as_tibble(out)
}
