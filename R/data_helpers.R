#' Validate Data Before Imputation
#'
#' Pre-flight validation of data, variable specification, and intercurrent event
#' data before calling [rbmi::draws()]. Collects all issues and reports them
#' together in a single error message.
#'
#' @param data A data.frame containing the analysis dataset.
#' @param vars A `vars` object as created by [rbmi::set_vars()].
#' @param data_ice An optional data.frame of intercurrent events. If provided,
#'   must contain columns corresponding to `vars$subjid`, `vars$visit`, and
#'   `vars$strategy`. Can be created using [prepare_data_ice()].
#'
#' @return Invisibly returns `TRUE` if all checks pass. Throws an error with
#'   collected messages if any issues are found.
#'
#' @details
#' The following checks are performed:
#' \itemize{
#'   \item `data` is a data.frame
#'   \item All columns named in `vars` exist in `data`
#'   \item `subjid`, `visit`, and `group` columns are factors
#'   \item `outcome` column is numeric
#'   \item Covariate columns have no missing values
#'   \item Data has one row per subject-visit combination
#'   \item If `data_ice` is provided: correct columns, valid subjects, valid
#'     visits, recognised strategies, and at most one row per subject
#' }
#'
#' **Recommended Workflow:**
#' 1. Call `validate_data()` to check your data
#' 2. Use [prepare_data_ice()] to create ICE data if needed
#' 3. Review missingness with [summarise_missingness()]
#' 4. Proceed with [rbmi::draws()] for imputation
#'
#' @seealso
#' * [prepare_data_ice()] to create intercurrent event data from flags
#' * [summarise_missingness()] to understand missing data patterns
#' * [rbmi::draws()] to perform imputation after validation
#'
#' @examples
#' library(rbmi)
#'
#' dat <- data.frame(
#'   USUBJID = factor(rep(c("S1", "S2", "S3"), each = 3)),
#'   AVISIT = factor(rep(c("Week 4", "Week 8", "Week 12"), 3),
#'                   levels = c("Week 4", "Week 8", "Week 12")),
#'   TRT = factor(rep(c("Placebo", "Drug A", "Drug A"), each = 3)),
#'   CHG = c(1.1, 2.2, 3.3, 0.5, NA, NA, 1.0, 2.0, NA),
#'   BASE = rep(c(10, 12, 11), each = 3),
#'   STRATA = factor(rep(c("A", "B", "A"), each = 3))
#' )
#'
#' vars <- set_vars(
#'   subjid = "USUBJID",
#'   visit = "AVISIT",
#'   group = "TRT",
#'   outcome = "CHG",
#'   covariates = c("BASE", "STRATA")
#' )
#'
#' validate_data(dat, vars)
#'
#' @export
validate_data <- function(data, vars, data_ice = NULL) {

  issues <- character(0)

  # --- Basic type checks ---
  if (!is.data.frame(data)) {
    issues <- c(issues, "`data` must be a data.frame")
  }

  assertthat::assert_that(
    is.list(vars),
    msg = "`vars` must be a list as returned by `rbmi::set_vars()`"
  )

  if (!is.null(data_ice) && !is.data.frame(data_ice)) {
    issues <- c(issues, "`data_ice` must be a data.frame or NULL")
  }

  # If data is not a data.frame, cannot proceed with column checks

  if (!is.data.frame(data)) {
    stop(paste(c("Data validation failed:", issues), collapse = "\n  - "))
  }

  # --- Column existence checks ---
  required_cols <- c(vars$subjid, vars$visit, vars$group, vars$outcome)
  covariate_cols <- vars$covariates

  # Parse interaction terms to get individual variable names
  all_covar_names <- unique(unlist(
    strsplit(trimws(covariate_cols), "[\\*\\:]")
  ))
  all_covar_names <- trimws(all_covar_names)
  all_covar_names <- all_covar_names[nzchar(all_covar_names)]

  all_required <- unique(c(required_cols, all_covar_names))
  missing_cols <- setdiff(all_required, names(data))

  if (length(missing_cols) > 0) {
    issues <- c(issues, sprintf(
      "Column(s) not found in `data`: %s",
      paste0("`", missing_cols, "`", collapse = ", ")
    ))
  }

  # Only proceed with type checks if columns exist
  existing_cols <- intersect(all_required, names(data))

  # --- Column type checks ---
  factor_cols <- intersect(c(vars$subjid, vars$visit, vars$group), existing_cols)
  for (col in factor_cols) {
    if (!is.factor(data[[col]]) && !is.character(data[[col]])) {
      issues <- c(issues, sprintf(
        "Column `%s` must be a factor (found %s)", col, class(data[[col]])[1]
      ))
    } else if (is.character(data[[col]])) {
      warning(sprintf(
        "Column `%s` is character and will be converted to factor by `rbmi::draws()`",
        col
      ), call. = FALSE)
    }
  }

  if (vars$outcome %in% existing_cols) {
    if (!is.numeric(data[[vars$outcome]])) {
      issues <- c(issues, sprintf(
        "Column `%s` (outcome) must be numeric (found %s)",
        vars$outcome, class(data[[vars$outcome]])[1]
      ))
    }
  }

  # --- Missing covariate checks ---
  existing_covars <- intersect(all_covar_names, existing_cols)
  for (col in existing_covars) {
    n_na <- sum(is.na(data[[col]]))
    if (n_na > 0) {
      issues <- c(issues, sprintf(
        "Covariate `%s` has %d missing value(s)", col, n_na
      ))
    }
  }

  # --- Duplicate subject-visit check ---
  if (all(c(vars$subjid, vars$visit) %in% existing_cols)) {
    n_rows <- nrow(data)
    n_unique <- nrow(unique(data[, c(vars$subjid, vars$visit), drop = FALSE]))
    if (n_rows != n_unique) {
      issues <- c(issues, sprintf(
        "Data has duplicate subject-visit rows (%d rows, %d unique combinations)",
        n_rows, n_unique
      ))
    }
  }

  # --- data_ice validation ---
  if (!is.null(data_ice) && is.data.frame(data_ice)) {
    # Validate that vars$strategy is defined
    if (is.null(vars$strategy) || !nzchar(vars$strategy)) {
      issues <- c(issues,
        "`vars$strategy` must be defined when `data_ice` is provided (use `rbmi::set_vars(strategy = ...)`)"
      )
    }

    ice_required <- c(vars$subjid, vars$visit, vars$strategy)
    ice_missing <- setdiff(ice_required, names(data_ice))

    if (length(ice_missing) > 0) {
      issues <- c(issues, sprintf(
        "`data_ice` missing column(s): %s",
        paste0("`", ice_missing, "`", collapse = ", ")
      ))
    }

    if (all(ice_required %in% names(data_ice))) {
      # Check subjects exist in data
      ice_subjects <- unique(data_ice[[vars$subjid]])
      data_subjects <- unique(data[[vars$subjid]])
      unknown_subjects <- setdiff(as.character(ice_subjects), as.character(data_subjects))
      if (length(unknown_subjects) > 0) {
        issues <- c(issues, sprintf(
          "`data_ice` contains %d subject(s) not found in `data`",
          length(unknown_subjects)
        ))
      }

      # Check visits are valid levels
      if (is.factor(data[[vars$visit]])) {
        valid_visits <- levels(data[[vars$visit]])
      } else {
        valid_visits <- unique(as.character(data[[vars$visit]]))
      }
      ice_visits <- unique(as.character(data_ice[[vars$visit]]))
      invalid_visits <- setdiff(ice_visits, valid_visits)
      if (length(invalid_visits) > 0) {
        issues <- c(issues, sprintf(
          "`data_ice` contains invalid visit(s): %s",
          paste0("`", invalid_visits, "`", collapse = ", ")
        ))
      }

      # Check strategies are valid
      valid_strategies <- c("MAR", "CR", "JR", "CIR", "LMCF")
      ice_strategies <- unique(as.character(data_ice[[vars$strategy]]))
      invalid_strategies <- setdiff(ice_strategies, valid_strategies)
      if (length(invalid_strategies) > 0) {
        issues <- c(issues, sprintf(
          "`data_ice` contains unrecognised strategy(ies): %s. Valid values: %s",
          paste0("`", invalid_strategies, "`", collapse = ", "),
          paste0("`", valid_strategies, "`", collapse = ", ")
        ))
      }

      # Check at most one row per subject
      ice_subj_counts <- table(data_ice[[vars$subjid]])
      dup_subjects <- names(ice_subj_counts[ice_subj_counts > 1])
      if (length(dup_subjects) > 0) {
        issues <- c(issues, sprintf(
          "`data_ice` has multiple rows for %d subject(s)", length(dup_subjects)
        ))
      }
    }
  }

  # --- Report results ---
  if (length(issues) > 0) {
    stop(paste(c("Data validation failed:", issues), collapse = "\n  - "),
         call. = FALSE)
  }

  invisible(TRUE)
}


#' Prepare Intercurrent Event Data
#'
#' Builds a `data_ice` data.frame from a column in the dataset that flags
#' intercurrent events. For each subject, the first visit (by factor level
#' order) where the flag is TRUE is used as the ICE visit.
#'
#' @param data A data.frame containing the analysis dataset.
#' @param vars A `vars` object as created by [rbmi::set_vars()].
#' @param ice_col Character string naming the column in `data` that indicates
#'   ICE occurrence. Accepted values are logical (`TRUE`/`FALSE`), character
#'   (`"Y"`/`"N"`), or numeric (`1`/`0`).
#' @param strategy Character string specifying the imputation strategy to assign.
#'   Must be one of `"MAR"`, `"CR"`, `"JR"`, `"CIR"`, or `"LMCF"`.
#'
#' @return A data.frame with columns corresponding to `vars$subjid`,
#'   `vars$visit`, and `vars$strategy`, suitable for passing to
#'   [rbmi::draws()].
#'
#' @examples
#' library(rbmi)
#'
#' dat <- data.frame(
#'   USUBJID = factor(rep(c("S1", "S2", "S3"), each = 3)),
#'   AVISIT = factor(rep(c("Week 4", "Week 8", "Week 12"), 3),
#'                   levels = c("Week 4", "Week 8", "Week 12")),
#'   TRT = factor(rep(c("Placebo", "Drug A", "Drug A"), each = 3)),
#'   CHG = rnorm(9),
#'   DISCFL = c("N","N","N", "N","Y","Y", "N","N","Y")
#' )
#'
#' vars <- set_vars(
#'   subjid = "USUBJID",
#'   visit = "AVISIT",
#'   group = "TRT",
#'   outcome = "CHG"
#' )
#'
#' ice <- prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "JR")
#' print(ice)
#'
#' @export
prepare_data_ice <- function(data, vars, ice_col, strategy) {

  assertthat::assert_that(
    is.data.frame(data),
    msg = "`data` must be a data.frame"
  )
  assertthat::assert_that(
    is.list(vars),
    msg = "`vars` must be a list as returned by `rbmi::set_vars()`"
  )
  assertthat::assert_that(
    is.character(ice_col) && length(ice_col) == 1,
    msg = "`ice_col` must be a single character string"
  )
  assertthat::assert_that(
    ice_col %in% names(data),
    msg = sprintf("Column `%s` not found in `data`", ice_col)
  )

  # Validate required vars fields
  required_vars <- c("subjid", "visit")
  missing_vars <- setdiff(required_vars, names(vars))
  if (length(missing_vars) > 0) {
    stop(sprintf(
      "`vars` must contain: %s. Use `rbmi::set_vars()` to create a valid vars object.",
      paste0("`", missing_vars, "`", collapse = ", ")
    ), call. = FALSE)
  }

  # Validate vars$strategy is defined (or will use default "strategy")
  if (is.null(vars$strategy) || !nzchar(vars$strategy)) {
    vars$strategy <- "strategy"
  }

  # Validate subjid and visit columns exist in data
  if (!vars$subjid %in% names(data)) {
    stop(sprintf(
      "Column `%s` (subjid) not found in `data`", vars$subjid
    ), call. = FALSE)
  }
  if (!vars$visit %in% names(data)) {
    stop(sprintf(
      "Column `%s` (visit) not found in `data`", vars$visit
    ), call. = FALSE)
  }

  valid_strategies <- c("MAR", "CR", "JR", "CIR", "LMCF")
  assertthat::assert_that(
    is.character(strategy) && length(strategy) == 1 && strategy %in% valid_strategies,
    msg = sprintf(
      "`strategy` must be one of: %s",
      paste0("`", valid_strategies, "`", collapse = ", ")
    )
  )

  # Convert ICE flag to logical
  ice_flag <- data[[ice_col]]
  if (is.logical(ice_flag)) {
    is_ice <- ice_flag & !is.na(ice_flag)
  } else if (is.character(ice_flag) || is.factor(ice_flag)) {
    is_ice <- as.character(ice_flag) == "Y"
    is_ice[is.na(is_ice)] <- FALSE
  } else if (is.numeric(ice_flag)) {
    is_ice <- ice_flag == 1 & !is.na(ice_flag)
  } else {
    stop(sprintf(
      "Column `%s` must be logical, character ('Y'/'N'), or numeric (1/0)",
      ice_col
    ), call. = FALSE)
  }

  # Filter to ICE rows
  ice_data <- data[is_ice, , drop = FALSE]

  if (nrow(ice_data) == 0) {
    # Return empty data.frame with correct structure
    result <- data.frame(
      subjid = character(0),
      visit = character(0),
      strategy = character(0),
      stringsAsFactors = FALSE
    )
    names(result) <- c(vars$subjid, vars$visit, vars$strategy)
    return(result)
  }

  # For each subject, take the first visit by factor level order
  visit_var <- vars$visit
  subjid_var <- vars$subjid

  if (is.factor(ice_data[[visit_var]])) {
    visit_order <- as.integer(ice_data[[visit_var]])
  } else {
    visit_order <- seq_len(nrow(ice_data))
  }

  ice_data$.visit_order <- visit_order

  # Keep first visit per subject
  ice_data <- ice_data |>
    dplyr::arrange(.data[[subjid_var]], .data$.visit_order) |>
    dplyr::distinct(.data[[subjid_var]], .keep_all = TRUE)

  # Build result
  result <- data.frame(
    subjid = ice_data[[subjid_var]],
    visit = ice_data[[visit_var]],
    strategy = strategy,
    stringsAsFactors = FALSE
  )
  names(result) <- c(vars$subjid, vars$visit, vars$strategy)

  result
}


#' Summarise Missing Data Patterns
#'
#' Tabulates missing outcome data by visit and treatment group, and classifies
#' each subject's missing data pattern as complete, monotone, or intermittent.
#'
#' @param data A data.frame containing the analysis dataset with one row per
#'   subject-visit combination.
#' @param vars A `vars` object as created by [rbmi::set_vars()].
#'
#' @return A list with three components:
#' \describe{
#'   \item{by_visit}{A tibble with columns: visit, group, n, n_miss, pct_miss}
#'   \item{patterns}{A tibble with columns: subjid, group, pattern
#'     ("complete", "monotone", or "intermittent"), dropout_visit (NA if not
#'     monotone)}
#'   \item{summary}{A tibble with columns: group, n_subjects, n_complete,
#'     n_monotone, n_intermittent}
#' }
#'
#' @examples
#' library(rbmi)
#'
#' dat <- data.frame(
#'   USUBJID = factor(rep(c("S1", "S2", "S3", "S4"), each = 3)),
#'   AVISIT = factor(rep(c("Week 4", "Week 8", "Week 12"), 4),
#'                   levels = c("Week 4", "Week 8", "Week 12")),
#'   TRT = factor(rep(c("Placebo", "Drug A"), each = 6)),
#'   CHG = c(1, 2, 3, 1, NA, NA, 1, 2, NA, 1, NA, 2)
#' )
#'
#' vars <- set_vars(
#'   subjid = "USUBJID",
#'   visit = "AVISIT",
#'   group = "TRT",
#'   outcome = "CHG"
#' )
#'
#' result <- summarise_missingness(dat, vars)
#' print(result$by_visit)
#' print(result$patterns)
#' print(result$summary)
#'
#' @export
summarise_missingness <- function(data, vars) {

  assertthat::assert_that(
    is.data.frame(data),
    msg = "`data` must be a data.frame"
  )
  assertthat::assert_that(
    is.list(vars),
    msg = "`vars` must be a list as returned by `rbmi::set_vars()`"
  )

  required_cols <- c(vars$subjid, vars$visit, vars$group, vars$outcome)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Column(s) not found in `data`: %s",
      paste0("`", missing_cols, "`", collapse = ", ")
    ), call. = FALSE)
  }

  subjid_var <- vars$subjid
  visit_var <- vars$visit
  group_var <- vars$group
  outcome_var <- vars$outcome

  # --- by_visit summary ---
  by_visit <- data |>
    dplyr::group_by(
      visit = .data[[visit_var]],
      group = .data[[group_var]]
    ) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_miss = sum(is.na(.data[[outcome_var]])),
      pct_miss = round(100 * .data$n_miss / .data$n, 1),
      .groups = "drop"
    ) |>
    dplyr::as_tibble()

  # --- Pattern classification ---
  # Get visit levels in order
  if (is.factor(data[[visit_var]])) {
    visit_levels <- levels(data[[visit_var]])
  } else {
    visit_levels <- unique(data[[visit_var]])
  }

  # For each subject, determine pattern
  subjects <- unique(data[[subjid_var]])
  pattern_list <- lapply(subjects, function(subj) {
    subj_data <- data[data[[subjid_var]] == subj, , drop = FALSE]
    grp <- as.character(subj_data[[group_var]][1])

    # Get missingness vector in visit order
    miss_by_visit <- vapply(visit_levels, function(v) {
      row <- subj_data[as.character(subj_data[[visit_var]]) == v, , drop = FALSE]
      if (nrow(row) == 0) return(TRUE)
      is.na(row[[outcome_var]][1])
    }, logical(1))

    if (!any(miss_by_visit)) {
      # Complete
      data.frame(
        subjid = subj,
        group = grp,
        pattern = "complete",
        dropout_visit = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      # Check if monotone: once missing starts, all subsequent are missing
      first_miss <- which(miss_by_visit)[1]
      is_monotone <- all(miss_by_visit[first_miss:length(miss_by_visit)])

      if (is_monotone) {
        data.frame(
          subjid = subj,
          group = grp,
          pattern = "monotone",
          dropout_visit = visit_levels[first_miss],
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          subjid = subj,
          group = grp,
          pattern = "intermittent",
          dropout_visit = NA_character_,
          stringsAsFactors = FALSE
        )
      }
    }
  })

  patterns <- dplyr::bind_rows(pattern_list) |>
    dplyr::as_tibble()
  names(patterns)[1:2] <- c(subjid_var, group_var)

  # --- Summary by group ---
  summary_tbl <- patterns |>
    dplyr::group_by(group = .data[[group_var]]) |>
    dplyr::summarise(
      n_subjects = dplyr::n(),
      n_complete = sum(.data$pattern == "complete"),
      n_monotone = sum(.data$pattern == "monotone"),
      n_intermittent = sum(.data$pattern == "intermittent"),
      .groups = "drop"
    ) |>
    dplyr::as_tibble()

  list(
    by_visit = by_visit,
    patterns = patterns,
    summary = summary_tbl
  )
}
