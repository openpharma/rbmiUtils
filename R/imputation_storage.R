#' Reduce Imputed Data for Efficient Storage
#'
#' Extracts only the imputed records (those that were originally missing) from
#' a full imputed dataset. This significantly reduces storage requirements when
#' working with many imputations, as observed values are identical across all
#' imputations and only need to be stored once in the original data.
#'
#' @param imputed_data A data.frame containing the full imputed dataset with an
#'   `IMPID` column identifying each imputation. Typically the output from
#'   [get_imputed_data()].
#' @param original_data A data.frame containing the original dataset before
#'   imputation, with missing values in the outcome column.
#' @param vars A `vars` object as created by [rbmi::set_vars()].
#'
#' @return A data.frame containing only the rows from `imputed_data` that
#'   correspond to originally missing outcome values. All columns from
#'   `imputed_data` are preserved.
#'
#' @details
#' Storage savings depend on the proportion of missing data. For example:
#' \itemize{
#'   \item Original: 1000 rows, 44 missing values
#'   \item Full imputed (1000 imputations): 1,000,000 rows
#'   \item Reduced (1000 imputations): 44,000 rows (4.4\% of full size)
#' }
#'
#' Use [expand_imputed_data()] to reconstruct the full imputed dataset when
#' needed for analysis.
#'
#' @seealso [expand_imputed_data()] to reconstruct the full dataset,
#'   [get_imputed_data()] to extract imputed data from an rbmi imputation object.
#'
#' @examples
#' library(rbmi)
#' library(dplyr)
#'
#' # Example with package data
#' data("ADMI", package = "rbmiUtils")
#' data("ADEFF", package = "rbmiUtils")
#'
#' # Prepare original data to match ADMI structure
#' original <- ADEFF |>
#'   mutate(
#'     TRT = TRT01P,
#'     USUBJID = as.character(USUBJID)
#'   )
#'
#' vars <- set_vars(
#'   subjid = "USUBJID",
#'   visit = "AVISIT",
#'   group = "TRT",
#'   outcome = "CHG"
#' )
#'
#' # Reduce to only imputed values
#' reduced <- reduce_imputed_data(ADMI, original, vars)
#'
#' # Compare sizes
#' cat("Full imputed rows:", nrow(ADMI), "\n")
#' cat("Reduced rows:", nrow(reduced), "\n")
#' cat("Compression:", round(100 * nrow(reduced) / nrow(ADMI), 1), "%\n")
#'
#' @export
reduce_imputed_data <- function(imputed_data, original_data, vars) {


  assertthat::assert_that(
    is.data.frame(imputed_data),
    msg = "`imputed_data` must be a data.frame"
  )
  assertthat::assert_that(
    "IMPID" %in% names(imputed_data),
    msg = "`imputed_data` must contain an `IMPID` column"
  )

  assertthat::assert_that(
    is.data.frame(original_data),
    msg = "`original_data` must be a data.frame"
  )
  assertthat::assert_that(
    is.list(vars),
    msg = "`vars` must be a list as returned by `rbmi::set_vars()`"
  )


  subjid <- vars$subjid
  visit <- vars$visit
  outcome <- vars$outcome

  # Validate required columns exist
  required_orig <- c(subjid, visit, outcome)
  missing_orig <- setdiff(required_orig, names(original_data))
  if (length(missing_orig) > 0) {
    stop(sprintf(
      "`original_data` missing column(s): %s",
      paste0("`", missing_orig, "`", collapse = ", ")
    ), call. = FALSE)
  }

  required_imp <- c("IMPID", subjid, visit, outcome)
  missing_imp <- setdiff(required_imp, names(imputed_data))
  if (length(missing_imp) > 0) {
    stop(sprintf(
      "`imputed_data` missing column(s): %s",
      paste0("`", missing_imp, "`", collapse = ", ")
    ), call. = FALSE)
  }

  # Find which subject-visit combos were originally missing
  is_missing <- is.na(original_data[[outcome]])
  missing_keys <- original_data[is_missing, c(subjid, visit), drop = FALSE]

  if (nrow(missing_keys) == 0) {
    # No missing values - return empty data.frame with correct structure
    return(imputed_data[0, , drop = FALSE])
  }

  # Filter imputed_data to only those keys
  # Use character conversion for robust matching
  missing_keys[[subjid]] <- as.character(missing_keys[[subjid]])
  missing_keys[[visit]] <- as.character(missing_keys[[visit]])

  imputed_keys <- data.frame(
    subjid = as.character(imputed_data[[subjid]]),
    visit = as.character(imputed_data[[visit]]),
    stringsAsFactors = FALSE
  )
  names(imputed_keys) <- c(subjid, visit)

  # Create a key for matching
  missing_key_str <- paste(missing_keys[[subjid]], missing_keys[[visit]], sep = "|||")
  imputed_key_str <- paste(imputed_keys[[subjid]], imputed_keys[[visit]], sep = "|||")

  keep_rows <- imputed_key_str %in% missing_key_str
  reduced <- imputed_data[keep_rows, , drop = FALSE]

  reduced
}


#' Expand Reduced Imputed Data to Full Dataset
#'
#' Reconstructs the full imputed dataset from a reduced form by merging imputed
#' values back with the original observed data. This is the inverse operation of
#' [reduce_imputed_data()].
#'
#' @param reduced_data A data.frame containing only the imputed values, as
#'   returned by [reduce_imputed_data()].
#' @param original_data A data.frame containing the original dataset before
#'   imputation, with missing values in the outcome column.
#' @param vars A `vars` object as created by [rbmi::set_vars()].
#'
#' @return A data.frame containing the full imputed dataset with one complete
#'   dataset per `IMPID` value. The structure matches the output of
#'   [get_imputed_data()].
#'
#' @details
#' For each imputation (identified by `IMPID`), this function:
#' \enumerate{
#'   \item Starts with the original data (observed values)
#'   \item Replaces missing outcome values with the corresponding imputed values
#'   \item Stacks all imputations together
#' }
#'
#' @seealso [reduce_imputed_data()] to create the reduced dataset,
#'   [get_imputed_data()] to extract imputed data from an rbmi imputation object.
#'
#' @examples
#' library(rbmi)
#' library(dplyr)
#'
#' # Example with package data
#' data("ADMI", package = "rbmiUtils")
#' data("ADEFF", package = "rbmiUtils")
#'
#' # Prepare original data to match ADMI structure
#' original <- ADEFF |>
#'   mutate(
#'     TRT = TRT01P,
#'     USUBJID = as.character(USUBJID)
#'   )
#'
#' vars <- set_vars(
#'   subjid = "USUBJID",
#'   visit = "AVISIT",
#'   group = "TRT",
#'   outcome = "CHG"
#' )
#'
#' # Reduce and then expand
#' reduced <- reduce_imputed_data(ADMI, original, vars)
#' expanded <- expand_imputed_data(reduced, original, vars)
#'
#' # Verify expansion
#' cat("Original ADMI rows:", nrow(ADMI), "\n")
#' cat("Expanded rows:", nrow(expanded), "\n")
#'
#' @export
expand_imputed_data <- function(reduced_data, original_data, vars) {

  assertthat::assert_that(
    is.data.frame(reduced_data),
    msg = "`reduced_data` must be a data.frame"
  )
  assertthat::assert_that(
    is.data.frame(original_data),
    msg = "`original_data` must be a data.frame"
  )
  assertthat::assert_that(
    is.list(vars),
    msg = "`vars` must be a list as returned by `rbmi::set_vars()`"
  )

  subjid <- vars$subjid
  visit <- vars$visit
  outcome <- vars$outcome

  # Validate required columns exist
  required_orig <- c(subjid, visit, outcome)
  missing_orig <- setdiff(required_orig, names(original_data))
  if (length(missing_orig) > 0) {
    stop(sprintf(
      "`original_data` missing column(s): %s",
      paste0("`", missing_orig, "`", collapse = ", ")
    ), call. = FALSE)
  }

  # Handle empty reduced_data (no missing values case)
  if (nrow(reduced_data) == 0) {
    # Return original data with IMPID = "1" as a single imputation
    result <- original_data
    result$IMPID <- "1"
    # Reorder to put IMPID first
    result <- result[, c("IMPID", setdiff(names(result), "IMPID")), drop = FALSE]
    return(result)
  }

  assertthat::assert_that(
    "IMPID" %in% names(reduced_data),
    msg = "`reduced_data` must contain an `IMPID` column"
  )

  required_red <- c("IMPID", subjid, visit, outcome)
  missing_red <- setdiff(required_red, names(reduced_data))
  if (length(missing_red) > 0) {
    stop(sprintf(
      "`reduced_data` missing column(s): %s",
      paste0("`", missing_red, "`", collapse = ", ")
    ), call. = FALSE)
  }

  imp_ids <- unique(reduced_data$IMPID)

  # Pre-compute keys for original data
  orig_keys <- paste(
    as.character(original_data[[subjid]]),
    as.character(original_data[[visit]]),
    sep = "|||"
  )

  result_list <- lapply(imp_ids, function(imp_id) {
    # Start with original data
    dat <- original_data
    dat$IMPID <- imp_id

    # Get imputed values for this IMPID
    imp_vals <- reduced_data[reduced_data$IMPID == imp_id, , drop = FALSE]

    if (nrow(imp_vals) > 0) {
      # Create lookup for imputed values
      imp_keys <- paste(
        as.character(imp_vals[[subjid]]),
        as.character(imp_vals[[visit]]),
        sep = "|||"
      )

      # Replace missing values with imputed (vectorized)
      match_idx <- match(imp_keys, orig_keys)
      valid <- !is.na(match_idx)
      dat[[outcome]][match_idx[valid]] <- imp_vals[[outcome]][valid]
    }

    dat
  })

  result <- dplyr::bind_rows(result_list)

  # Reorder columns to put IMPID first (matching get_imputed_data output)
  result <- result[, c("IMPID", setdiff(names(result), "IMPID")), drop = FALSE]

  result
}
