#' Apply Analysis Function to Multiple Imputed Datasets
#'
#' This function applies an analysis function (e.g., ANCOVA) to imputed datasets and stores the results for later pooling. It is designed to work with multiple imputed datasets and apply a given analysis function to each imputation iteration.
#'
#' @param data A data frame containing the imputed datasets. The data frame should include a variable (e.g., `IMPID`) that identifies distinct imputation iterations.
#'   Typically obtained from [get_imputed_data()] or [expand_imputed_data()].
#' @param vars A list specifying key variables used in the analysis (e.g., `subjid`, `visit`, `group`, `outcome`).
#'   Created using [rbmi::set_vars()]. Required.
#' @param method A method object specifying the imputation method used (e.g., Bayesian imputation).
#'   Created using [rbmi::method_bayes()], [rbmi::method_approxbayes()], or [rbmi::method_condmean()]. Required.
#' @param fun A function that will be applied to each imputed dataset. Defaults to [rbmi::ancova].
#'   Other options include [gcomp_responder_multi()] for binary outcomes. Must be a valid analysis function.
#' @param delta A `data.frame` used for delta adjustments, or `NULL` if no delta adjustments are needed. Defaults to `NULL`.
#'   Must contain columns matching `vars$subjid`, `vars$visit`, `vars$group`, and a `delta` column.
#' @param ... Additional arguments passed to the analysis function `fun`.
#'
#' @details
#' The function loops through distinct imputation datasets (identified by `IMPID`), applies the provided analysis function `fun`, and stores the results for later pooling. If a `delta` dataset is provided, it will be merged with the imputed data to apply the specified delta adjustment before analysis.
#'
#' **Workflow:**
#' 1. Prepare imputed data using [get_imputed_data()] or [expand_imputed_data()]
#' 2. Define variables using [rbmi::set_vars()]
#' 3. Call `analyse_mi_data()` to apply analysis to each imputation
#' 4. Pool results using [rbmi::pool()]
#' 5. Tidy results using [tidy_pool_obj()]
#'
#' @return An object of class `analysis` containing the results from applying the analysis function to each imputed dataset.
#'   Pass this to [rbmi::pool()] to obtain pooled estimates.
#'
#' @seealso
#' * [tidy_pool_obj()] to format pooled results for publication

#' * [get_imputed_data()] to extract imputed datasets from rbmi objects
#' * [expand_imputed_data()] to reconstruct full imputed data from reduced form
#' * [gcomp_responder_multi()] for binary outcome analysis
#' * [validate_data()] to check data before imputation
#'
#' @examples
#' # Example usage with an ANCOVA function
#' library(dplyr)
#' library(rbmi)
#' library(rbmiUtils)
#' set.seed(123)
#' data("ADMI")
#'
#' # Convert key columns to factors
#' ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
#' ADMI$USUBJID <- factor(ADMI$USUBJID)
#' ADMI$AVISIT <- factor(ADMI$AVISIT)
#'
#' # Define key variables for ANCOVA analysis
#'  vars <- set_vars(
#'   subjid = "USUBJID",
#'   visit = "AVISIT",
#'   group = "TRT",
#'   outcome = "CHG",
#'   covariates = c("BASE", "STRATA", "REGION")  # Covariates for adjustment
#'  )
#'
#' # Specify the imputation method (Bayesian) - need for pool step
#'  method <- rbmi::method_bayes(
#'  n_samples = 20,
#'  control = rbmi::control_bayes(
#'    warmup = 20,
#'    thin = 1
#'    )
#'  )
#'
#' # Perform ANCOVA Analysis on Each Imputed Dataset
#' ana_obj_ancova <- analyse_mi_data(
#'   data = ADMI,
#'   vars = vars,
#'   method = method,
#'   fun = ancova,  # Apply ANCOVA
#'   delta = NULL   # No sensitivity analysis adjustment
#' )
#'
#' @export
analyse_mi_data <- function(
  data = NULL,
  vars = NULL,
  method = NULL,
  fun = rbmi::ancova,
  delta = NULL,
  ...
) {
  # Check for missing inputs
  if (is.null(data)) stop("`data` cannot be NULL.", call. = FALSE)

  assertthat::assert_that(
    is.data.frame(data),
    msg = "`data` must be a data.frame"
  )

  # check IMPID is in data
  if (!"IMPID" %in% names(data))
    stop(
      "`data` must contain a variable `IMPID` to identify distinct imputation iterations.",
      call. = FALSE
    )

  if (is.null(vars)) stop("`vars` cannot be NULL. Specify key variables using `rbmi::set_vars()`.", call. = FALSE)

  assertthat::assert_that(
    is.list(vars),
    msg = "`vars` must be a list as returned by `rbmi::set_vars()`"
  )

  ## asset function
  assertthat::assert_that(
    is.function(fun),
    msg = "`fun` must be a function"
  )

  ## check on delta
  assertthat::assert_that(
    is.null(delta) | is.data.frame(delta),
    msg = "`delta` must be NULL or a data.frame"
  )

  # Validate required vars fields
  required_vars <- c("subjid", "visit", "group", "outcome")
  missing_vars <- required_vars[!required_vars %in% names(vars) | sapply(vars[required_vars], is.null)]
  if (length(missing_vars) > 0) {
    stop(sprintf(
      "`vars` must specify: %s. Use `rbmi::set_vars()` to create a valid vars object.",
      paste0("`", missing_vars, "`", collapse = ", ")
    ), call. = FALSE)
  }

  # Check method is provided
  if (is.null(method)) {
    stop("`method` cannot be NULL. Specify a method using `rbmi::method_bayes()` or similar.", call. = FALSE)
  }

  # Check for empty data
  if (nrow(data) == 0) {
    stop("`data` has no rows.", call. = FALSE)
  }

  # Check for empty IMPID groups
  n_imps <- length(unique(data$IMPID))
  if (n_imps == 0) {
    stop("`data` has no valid IMPID values.", call. = FALSE)
  }

  # Extract expected number of samples from method
  n_expected <- switch(
    class(method)[[2]],
    bayes = method$n_samples,
    approxbayes = method$n_samples,
    condmean = method$n_samples,
    bmlmi = method$n_samples,
    NULL
  )

  # Check and filter IMPID values to match expected sample size
  unique_impids <- sort(unique(data$IMPID))
  n_impids <- length(unique_impids)

  if (!is.null(n_expected) && n_impids != n_expected) {
    if (n_impids > n_expected) {
      # Filter to first n_expected imputations
      warning(
        sprintf(
          "Data contains %d imputations but method expects %d. Using first %d imputations.",
          n_impids, n_expected, n_expected
        ),
        call. = FALSE
      )
      # Filter data to only include the first n_expected IMPID values
      keep_impids <- unique_impids[seq_len(n_expected)]
      data <- data[data$IMPID %in% keep_impids, ]

      # Verify filtering worked
      n_after <- length(unique(data$IMPID))
      if (n_after != n_expected) {
        stop(
          sprintf("Internal error: filtering failed. Expected %d imputations, got %d", n_expected, n_after),
          call. = FALSE
        )
      }
    } else {
      stop(
        sprintf(
          "Data contains %d imputations but method expects %d. Need more imputations.",
          n_impids, n_expected
        ),
        call. = FALSE
      )
    }
  }

  ## check delta has correct variables and then apply
  if (!is.null(delta)) {
    expected_vars <- c(
      vars$subjid,
      vars$visit,
      "delta"
    )
    assertthat::assert_that(
      all(expected_vars %in% names(delta)),
      msg = sprintf(
        "The following variables must exist within `delta`: `%s`",
        paste0(expected_vars, collapse = "`, `")
      )
    )

    ## apply delta to data set adding to outcome
    data <- data |>
      dplyr::left_join(delta, by = c(vars$subjid, vars$visit, vars$group)) |>
      dplyr::mutate(
        !!rlang::sym(vars$outcome) := .data[[vars$outcome]] + .data$delta
      )
  }

  # Loop through distinct imputation data sets based on IMPID in a single data frame
  results <- data |>
    dplyr::group_split(IMPID) |>
    lapply(
      function(dat_subset, ...) {
        # Perform analysis on the subset of data corresponding to each imputation
        fun(dat_subset, vars, ...)
      },
      ...
    )

  fun_name <- deparse(substitute(fun))

  if (length(fun_name) > 1 || grepl("^function\\(", fun_name[1])) {
    fun_name <- "<Anonymous Function>"
  } else if (is.null(fun_name)) {
    fun_name <- "<NULL>"
  }

  ret <- as_analysis2(
    results = results,
    fun_name = fun_name,
    delta = delta,
    fun = fun,
    method = method
  )

  return(ret)
}


#' Construct an rbmi `analysis` object
#'
#' @description
#' This is a helper function to create an analysis object that stores the
#' results from multiple imputation analyses. It validates the results and
#' ensures proper class assignment.
#'
#' This is a modification of the rbmi::as_analysis function.
#'
#' @param results A list containing the analysis results for each imputation.
#' @param method The method object used for the imputation.
#' @param delta Optional. A delta dataset used for adjustment.
#' @param fun The analysis function that was used.
#' @param fun_name The name of the analysis function (used for printing).
#'
#' @return An object of class `analysis` with the results and associated metadata.
as_analysis2 <- function(
  results,
  method,
  delta = NULL,
  fun = NULL,
  fun_name = NULL
) {
  next_class <- switch(
    class(method)[[2]],
    bayes = "rubin",
    approxbayes = "rubin",
    condmean = ifelse(
      method$type == "jackknife",
      "jackknife",
      "bootstrap"
    ),
    bmlmi = "bmlmi"
  )

  assertthat::assert_that(
    is.list(results),
    length(next_class) == 1,
    is.character(next_class),
    next_class %in% c("jackknife", "bootstrap", "rubin", "bmlmi")
  )

  x <- list(
    results = rbmi::as_class(results, c(next_class, "list")),
    delta = delta,
    fun = fun,
    fun_name = fun_name,
    method = method
  )
  class(x) <- c("analysis", "list")

  return(x)
}


#' Print Method for Analysis Objects
#'
#' Prints a summary of an analysis object from [analyse_mi_data()].
#'
#' @param x An object of class `analysis`.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object.
#'
#' @examples
#' \donttest{
#' library(rbmi)
#' library(rbmiUtils)
#' data("ADMI")
#'
#' # Create analysis object
#' vars <- set_vars(
#'   subjid = "USUBJID", visit = "AVISIT", group = "TRT",
#'   outcome = "CHG", covariates = c("BASE", "STRATA")
#' )
#' method <- method_bayes(n_samples = 10, control = control_bayes(warmup = 10))
#'
#' ana_obj <- analyse_mi_data(ADMI, vars, method, fun = function(d, v, ...) 1)
#' print(ana_obj)
#' }
#'
#' @export
print.analysis <- function(x, ...) {
  cat("Analysis object from rbmiUtils\n")
  cat("-------------------------------\n")
  cat("Number of imputations:", length(x$results), "\n")
  cat("Analysis function:", x$fun_name, "\n")

  if (!is.null(x$delta)) {
    cat("Delta adjustment: Yes\n")
  } else {
    cat("Delta adjustment: No\n")
  }

  method_class <- class(x$method)[2]
  cat("Method type:", method_class, "\n")

  # Show pooling class
  result_class <- class(x$results)[1]
  cat("Pooling method:", result_class, "\n")

  cat("\nUse `rbmi::pool()` to obtain pooled estimates.\n")

  invisible(x)
}


#' Summary Method for Analysis Objects
#'
#' Provides a detailed summary of an analysis object from [analyse_mi_data()].
#'
#' @param object An object of class `analysis`.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list containing summary information (invisibly).
#'
#' @examples
#' \donttest{
#' library(rbmi)
#' library(rbmiUtils)
#' data("ADMI")
#'
#' # Create analysis object
#' vars <- set_vars(
#'   subjid = "USUBJID", visit = "AVISIT", group = "TRT",
#'   outcome = "CHG", covariates = c("BASE", "STRATA")
#' )
#' method <- method_bayes(n_samples = 10, control = control_bayes(warmup = 10))
#'
#' ana_obj <- analyse_mi_data(ADMI, vars, method, fun = function(d, v, ...) 1)
#' summary(ana_obj)
#' }
#'
#' @export
summary.analysis <- function(object, ...) {
  cat("Analysis Object Summary\n")
  cat("=======================\n\n")

  cat("Imputations:\n")
  cat("  Number of imputations:", length(object$results), "\n")

  cat("\nAnalysis:\n")
  cat("  Function:", object$fun_name, "\n")

  if (!is.null(object$delta)) {
    cat("  Delta adjustment: Applied\n")
    cat("  Delta rows:", nrow(object$delta), "\n")
  } else {
    cat("  Delta adjustment: None\n")
  }

  cat("\nMethod:\n")
  method_class <- class(object$method)[2]
  cat("  Type:", method_class, "\n")

  if (method_class %in% c("bayes", "approxbayes")) {
    if (!is.null(object$method$n_samples)) {
      cat("  Samples:", object$method$n_samples, "\n")
    }
  }

  cat("\nPooling:\n")
  result_class <- class(object$results)[1]
  cat("  Method:", result_class, "\n")

  # Check first result structure
  if (length(object$results) > 0) {
    first_result <- object$results[[1]]
    if (is.list(first_result)) {
      cat("  Parameters per imputation:", length(first_result), "\n")
      if (length(first_result) > 0 && !is.null(names(first_result))) {
        param_names <- names(first_result)
        if (length(param_names) > 5) {
          cat("  Parameter names:", paste(param_names[1:5], collapse = ", "), "...\n")
        } else {
          cat("  Parameter names:", paste(param_names, collapse = ", "), "\n")
        }
      }
    }
  }

  cat("\nNext steps:\n")
  cat("  1. Pool results: pool_obj <- rbmi::pool(analysis_obj)\n")
  cat("  2. Tidy results: tidy_df <- tidy_pool_obj(pool_obj)\n")

  summary_info <- list(
    n_imputations = length(object$results),
    fun_name = object$fun_name,
    has_delta = !is.null(object$delta),
    method_type = method_class,
    pooling_method = result_class
  )

  invisible(summary_info)
}
