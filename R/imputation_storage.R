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
#' @seealso
#' * [rbmi::impute()] which creates the imputed datasets this function operates on
#' * [expand_imputed_data()] to reconstruct the full dataset
#' * [get_imputed_data()] to extract imputed data from an rbmi imputation object
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

  if (!is.data.frame(imputed_data)) {
    cli::cli_abort(
      "{.arg imputed_data} must be a {.cls data.frame}, not {.cls {class(imputed_data)}}.",
      class = "rbmiUtils_error_type"
    )
  }
  if (!"IMPID" %in% names(imputed_data)) {
    cli::cli_abort(
      "{.arg imputed_data} must contain an {.field IMPID} column.",
      class = "rbmiUtils_error_validation"
    )
  }

  if (!is.data.frame(original_data)) {
    cli::cli_abort(
      "{.arg original_data} must be a {.cls data.frame}, not {.cls {class(original_data)}}.",
      class = "rbmiUtils_error_type"
    )
  }
  if (!is.list(vars)) {
    cli::cli_abort(
      "{.arg vars} must be a list as returned by {.fn rbmi::set_vars}.",
      class = "rbmiUtils_error_type"
    )
  }

  subjid <- vars$subjid
  visit <- vars$visit
  outcome <- vars$outcome

  # Validate required columns exist
  required_orig <- c(subjid, visit, outcome)
  missing_orig <- setdiff(required_orig, names(original_data))
  if (length(missing_orig) > 0) {
    cli::cli_abort(
      "{.arg original_data} missing column{?s}: {.field {missing_orig}}.",
      class = "rbmiUtils_error_validation"
    )
  }

  required_imp <- c("IMPID", subjid, visit, outcome)
  missing_imp <- setdiff(required_imp, names(imputed_data))
  if (length(missing_imp) > 0) {
    cli::cli_abort(
      "{.arg imputed_data} missing column{?s}: {.field {missing_imp}}.",
      class = "rbmiUtils_error_validation"
    )
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

  # Preserve column attributes from imputed_data
  for (col_name in names(reduced)) {
    col_attrs <- attributes(imputed_data[[col_name]])
    if (!is.null(col_attrs)) {
      attrs_to_restore <- setdiff(names(col_attrs), c("names", "dim", "dimnames"))
      for (attr_name in attrs_to_restore) {
        attr(reduced[[col_name]], attr_name) <- col_attrs[[attr_name]]
      }
    }
  }

  # Compute digest of the original imputed_data for round-trip verification
  col_metadata <- lapply(names(imputed_data), function(cn) {
    list(
      class = class(imputed_data[[cn]]),
      typeof = typeof(imputed_data[[cn]])
    )
  })
  names(col_metadata) <- names(imputed_data)

  first_imp <- imputed_data[imputed_data$IMPID == unique(imputed_data$IMPID)[1], , drop = FALSE]
  digest_val <- rlang::hash(list(
    col_metadata = col_metadata,
    first_imp_hash = rlang::hash(first_imp),
    n_cols = ncol(imputed_data),
    col_names = names(imputed_data),
    n_imps = length(unique(imputed_data$IMPID))
  ))

  attr(reduced, "rbmiUtils_digest") <- digest_val
  attr(reduced, "rbmiUtils_col_metadata") <- col_metadata
  attr(reduced, "rbmiUtils_col_names") <- names(imputed_data)

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
#' @seealso
#' * [rbmi::impute()] which creates the imputed datasets this function operates on
#' * [reduce_imputed_data()] to create the reduced dataset
#' * [get_imputed_data()] to extract imputed data from an rbmi imputation object
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

  if (!is.data.frame(reduced_data)) {
    cli::cli_abort(
      "{.arg reduced_data} must be a {.cls data.frame}, not {.cls {class(reduced_data)}}.",
      class = "rbmiUtils_error_type"
    )
  }
  if (!is.data.frame(original_data)) {
    cli::cli_abort(
      "{.arg original_data} must be a {.cls data.frame}, not {.cls {class(original_data)}}.",
      class = "rbmiUtils_error_type"
    )
  }
  if (!is.list(vars)) {
    cli::cli_abort(
      "{.arg vars} must be a list as returned by {.fn rbmi::set_vars}.",
      class = "rbmiUtils_error_type"
    )
  }

  subjid <- vars$subjid
  visit <- vars$visit
  outcome <- vars$outcome

  # Validate required columns exist
  required_orig <- c(subjid, visit, outcome)
  missing_orig <- setdiff(required_orig, names(original_data))
  if (length(missing_orig) > 0) {
    cli::cli_abort(
      "{.arg original_data} missing column{?s}: {.field {missing_orig}}.",
      class = "rbmiUtils_error_validation"
    )
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

  if (!"IMPID" %in% names(reduced_data)) {
    cli::cli_abort(
      "{.arg reduced_data} must contain an {.field IMPID} column.",
      class = "rbmiUtils_error_validation"
    )
  }

  required_red <- c("IMPID", subjid, visit, outcome)
  missing_red <- setdiff(required_red, names(reduced_data))
  if (length(missing_red) > 0) {
    cli::cli_abort(
      "{.arg reduced_data} missing column{?s}: {.field {missing_red}}.",
      class = "rbmiUtils_error_validation"
    )
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

      # Preserve attributes before modifying
      outcome_attrs <- attributes(dat[[outcome]])

      # Replace missing values with imputed (vectorized)
      match_idx <- match(imp_keys, orig_keys)
      valid <- !is.na(match_idx)
      dat[[outcome]][match_idx[valid]] <- imp_vals[[outcome]][valid]

      # Restore attributes (except 'names' which we want to keep as modified)
      if (!is.null(outcome_attrs)) {
        attrs_to_restore <- setdiff(names(outcome_attrs), "names")
        for (attr_name in attrs_to_restore) {
          attr(dat[[outcome]], attr_name) <- outcome_attrs[[attr_name]]
        }
      }
    }

    dat
  })

  result <- dplyr::bind_rows(result_list)

  # Reorder columns to put IMPID first (matching get_imputed_data output)
  result <- result[, c("IMPID", setdiff(names(result), "IMPID")), drop = FALSE]

  # Restore column attributes from original_data
  for (col_name in names(original_data)) {
    if (col_name %in% names(result)) {
      col_attrs <- attributes(original_data[[col_name]])
      if (!is.null(col_attrs)) {
        attrs_to_restore <- setdiff(names(col_attrs), c("names", "dim", "dimnames"))
        for (attr_name in attrs_to_restore) {
          attr(result[[col_name]], attr_name) <- col_attrs[[attr_name]]
        }
      }
    }
  }

  # --- Round-trip digest verification ---
  stored_col_names <- attr(reduced_data, "rbmiUtils_col_names")
  stored_col_metadata <- attr(reduced_data, "rbmiUtils_col_metadata")

  if (!is.null(stored_col_names)) {
    # Verify that all original_data columns appear in the expanded result.
    # Note: imputed_data may have had extra columns not in original_data
    # (e.g., derived columns added during imputation). These won't appear in
    # the expanded result since expansion is built from original_data, so we
    # only check columns that exist in both stored metadata AND original_data.
    orig_cols <- names(original_data)
    expected_cols <- intersect(setdiff(stored_col_names, "IMPID"), orig_cols)
    expanded_cols <- setdiff(names(result), "IMPID")
    col_name_diff <- setdiff(expected_cols, expanded_cols)
    if (length(col_name_diff) > 0) {
      cli::cli_abort(
        c(
          "Round-trip integrity check failed: column name{?s} {.field {col_name_diff}} expected but not found in expanded data.",
          "i" = "The original imputed data had columns: {.field {stored_col_names}}.",
          "i" = "The expanded data has columns: {.field {names(result)}}."
        ),
        class = "rbmiUtils_error_integrity"
      )
    }
  }

  if (!is.null(stored_col_metadata)) {
    # Verify column types match
    type_mismatches <- character(0)
    for (cn in names(stored_col_metadata)) {
      if (cn %in% names(result)) {
        expected_class <- stored_col_metadata[[cn]]$class
        actual_class <- class(result[[cn]])
        if (!identical(expected_class, actual_class)) {
          type_mismatches <- c(
            type_mismatches,
            paste0(cn, " (expected ", paste(expected_class, collapse = "/"), ", got ", paste(actual_class, collapse = "/"), ")")
          )
        }
      }
    }
    if (length(type_mismatches) > 0) {
      cli::cli_abort(
        c(
          "Round-trip integrity check failed: column type mismatch.",
          stats::setNames(type_mismatches, rep("x", length(type_mismatches)))
        ),
        class = "rbmiUtils_error_integrity"
      )
    }
  }

  result
}
