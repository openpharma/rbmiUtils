#' Apply Analysis Function to Multiple Imputed Datasets
#'
#' This function applies an analysis function (e.g., ANCOVA) to imputed datasets and stores the results for later pooling. It is designed to work with multiple imputed datasets and apply a given analysis function to each imputation iteration.
#'
#' @param data A data frame containing the imputed datasets. The data frame should include a variable (e.g., `IMPID`) that identifies distinct imputation iterations.
#'   Typically obtained from [get_imputed_data()] or [expand_imputed_data()].
#' @param vars A list specifying key variables used in the analysis (e.g., `subjid`, `visit`, `group`, `outcome`).
#'   Created using [rbmi::set_vars()]. Required.
#' @param method A method object specifying the imputation method used (e.g., Bayesian imputation).
#'   Created using [rbmi::method_bayes()], [rbmi::method_approxbayes()], or [rbmi::method_condmean()].
#'   Optional if `pooling` is supplied.
#' @param fun A function that will be applied to each imputed dataset. Defaults to [rbmi::ancova].
#'   Other options include [gcomp_responder_multi()] for binary outcomes. Must be a valid analysis function.
#' @param delta A `data.frame` used for delta adjustments, or `NULL` if no delta adjustments are needed. Defaults to `NULL`.
#'   Must contain columns matching `vars$subjid`, `vars$visit`, `vars$group`, and a `delta` column.
#' @param ... Additional arguments passed to the analysis function `fun`.
#' @param pooling Optional length-one character giving the pooling strategy
#'   directly: `"rubin"`, `"bootstrap"`, `"jackknife"`, or `"bmlmi"`. Use this
#'   when the imputation method that produced `data` is unknown. Supply either
#'   `method` or `pooling`; if both are given they must agree (see
#'   [get_pooling()]). For an imputed dataset of unknown provenance,
#'   `pooling = "rubin"` is recommended: `"bootstrap"` and `"jackknife"`
#'   pooling assume a specific ordering of samples (original-data estimate
#'   first for bootstrap) that cannot be verified without the method object,
#'   and `"bmlmi"` cannot be selected via `pooling` alone because the number
#'   of analyses per imputation (D) is not inferable from the data. When only
#'   `pooling` is supplied, the number of imputations is taken from the data
#'   and no sample-count check is performed.
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
#' * [rbmi::analyse()] which this function wraps
#' * [rbmi::pool()] for pooling the analysis results
#' * The [rbmi quickstart vignette](https://CRAN.R-project.org/package=rbmi/vignettes/quickstart.html)
#' * [tidy_pool_obj()] to format pooled results for publication
#' * [get_pooling()] for the method-to-pooling mapping

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
#' # When the imputation method is unknown, specify pooling directly:
#' ana_obj <- analyse_mi_data(
#'   data = ADMI,
#'   vars = vars,
#'   pooling = "rubin",
#'   fun = ancova
#' )
#'
#' @export
analyse_mi_data <- function(
  data = NULL,
  vars = NULL,
  method = NULL,
  fun = rbmi::ancova,
  delta = NULL,
  ...,
  pooling = NULL
) {
  method_supplied <- !is.null(method)

  # Check for missing inputs
  if (is.null(data)) {
    cli::cli_abort(
      "{.arg data} cannot be NULL.",
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }

  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} must be a {.cls data.frame}, not {.cls {class(data)}}.",
      class = c("rbmiUtils_error_type", "rbmiUtils_error")
    )
  }

  # check IMPID is in data
  if (!"IMPID" %in% names(data)) {
    cli::cli_abort(
      "{.arg data} must contain an {.field IMPID} column to identify distinct imputation iterations.",
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }

  if (is.null(vars)) {
    cli::cli_abort(
      "{.arg vars} cannot be NULL. Specify key variables using {.fn rbmi::set_vars}.",
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }

  if (!is.list(vars)) {
    cli::cli_abort(
      "{.arg vars} must be a list as returned by {.fn rbmi::set_vars}, not {.cls {class(vars)}}.",
      class = c("rbmiUtils_error_type", "rbmiUtils_error")
    )
  }

  ## check function
  if (!is.function(fun)) {
    cli::cli_abort(
      "{.arg fun} must be a function, not {.cls {class(fun)}}.",
      class = c("rbmiUtils_error_type", "rbmiUtils_error")
    )
  }

  ## check on delta
  if (!is.null(delta) && !is.data.frame(delta)) {
    cli::cli_abort(
      "{.arg delta} must be NULL or a {.cls data.frame}, not {.cls {class(delta)}}.",
      class = c("rbmiUtils_error_type", "rbmiUtils_error")
    )
  }

  # Validate required vars fields
  required_vars <- c("subjid", "visit", "group", "outcome")
  missing_vars <- required_vars[!required_vars %in% names(vars) | sapply(vars[required_vars], is.null)]
  if (length(missing_vars) > 0) {
    cli::cli_abort(
      "{.arg vars} must specify: {.field {missing_vars}}. Use {.fn rbmi::set_vars} to create a valid vars object.",
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }

  # Resolve method / pooling: either may be supplied; both must agree
  valid_pooling <- c("rubin", "bootstrap", "jackknife", "bmlmi")
  if (!is.null(pooling) &&
      (!is.character(pooling) || length(pooling) != 1 ||
         !pooling %in% valid_pooling)) {
    cli::cli_abort(
      "{.arg pooling} must be one of {.val {valid_pooling}}.",
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }
  if (is.null(method) && is.null(pooling)) {
    cli::cli_abort(
      c(
        "Either {.arg method} or {.arg pooling} must be supplied.",
        "i" = "Use {.fn rbmi::method_bayes} or similar if the imputation method is known.",
        "i" = "If it is unknown, specify the pooling strategy directly, e.g. {.code pooling = \"rubin\"}."
      ),
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }
  if (!is.null(method)) {
    method_pooling <- get_pooling(method)
    if (!is.null(pooling) && !identical(pooling, method_pooling)) {
      cli::cli_abort(
        "{.arg pooling} ({.val {pooling}}) conflicts with {.arg method}, which implies {.val {method_pooling}}. Supply one or the other.",
        class = c("rbmiUtils_error_validation", "rbmiUtils_error")
      )
    }
    pooling <- method_pooling
  }

  # Check for empty data
  if (nrow(data) == 0) {
    cli::cli_abort(
      "{.arg data} has no rows.",
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }

  # Check for empty IMPID groups
  n_imps <- length(unique(data$IMPID))
  if (n_imps == 0) {
    cli::cli_abort(
      "{.arg data} has no valid IMPID values.",
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }

  # Count imputations present in the data
  unique_impids <- sort(unique(data$IMPID))
  n_impids <- length(unique_impids)

  if (method_supplied) {
    # Extract expected number of samples from method using inherits()
    n_expected <- if (inherits(method, "bayes") || inherits(method, "approxbayes")) {
      method$n_samples
    } else if (inherits(method, "condmean")) {
      method$n_samples
    } else if (inherits(method, "bmlmi")) {
      method$n_samples
    } else {
      NULL
    }

    # Check and filter IMPID values to match expected sample size
    if (!is.null(n_expected) && n_impids != n_expected) {
      if (n_impids > n_expected) {
        # Filter to first n_expected imputations
        cli::cli_warn(
          "Data contains {n_impids} imputation{?s} but method expects {n_expected}. Using first {n_expected} imputation{?s}."
        )
        keep_impids <- unique_impids[seq_len(n_expected)]
        data <- data[data$IMPID %in% keep_impids, ]

        n_after <- length(unique(data$IMPID))
        if (n_after != n_expected) {
          cli::cli_abort(
            "Internal error: filtering failed. Expected {n_expected} imputations, got {n_after}.",
            class = c("rbmiUtils_error_internal", "rbmiUtils_error")
          )
        }
      } else {
        cli::cli_abort(
          "Data contains {n_impids} imputation{?s} but method expects {n_expected}. Need more imputations.",
          class = c("rbmiUtils_error_validation", "rbmiUtils_error")
        )
      }
    }
  } else {
    # No method supplied: the data defines the number of imputations.
    # Build a stand-in method so downstream rbmi::pool() validation
    # (which asserts result counts against method$n_samples) passes.
    method <- make_standin_method(pooling, n_impids)
  }

  ## check delta has correct variables and then apply
  if (!is.null(delta)) {
    expected_vars <- c(
      vars$subjid,
      vars$visit,
      "delta"
    )
    if (!all(expected_vars %in% names(delta))) {
      cli::cli_abort(
        "The following variables must exist within {.arg delta}: {.field {expected_vars}}.",
        class = c("rbmiUtils_error_validation", "rbmiUtils_error")
      )
    }

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

  ret <- suppressWarnings(
    as_analysis2(
      results = results,
      fun_name = fun_name,
      delta = delta,
      fun = fun,
      method = method
    ),
    classes = "lifecycle_warning_deprecated"
  )

  return(ret)
}


#' Construct an rbmi `analysis` object
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This is a helper function to create an analysis object that stores the
#' results from multiple imputation analyses. It validates the results and
#' ensures proper class assignment.
#'
#' This is a modification of the rbmi::as_analysis function.
#' This function is deprecated and will be removed in a future version.
#'
#' @param results A list containing the analysis results for each imputation.
#' @param method The method object used for the imputation.
#' @param delta Optional. A delta dataset used for adjustment.
#' @param fun The analysis function that was used.
#' @param fun_name The name of the analysis function (used for printing).
#'
#' @return An object of class `analysis` with the results and associated metadata.
#' @keywords internal
as_analysis2 <- function(
  results,
  method,
  delta = NULL,
  fun = NULL,
  fun_name = NULL
) {
  lifecycle::deprecate_warn(
    "0.2.0",
    "as_analysis2()",
    details = "Internal helper will be removed. Use inherits()-based class detection directly."
  )

  next_class <- get_pooling(method)

  if (!is.list(results)) {
    cli::cli_abort(
      "{.arg results} must be a list.",
      class = c("rbmiUtils_error_type", "rbmiUtils_error")
    )
  }
  if (length(next_class) != 1 || !is.character(next_class) ||
      !next_class %in% c("jackknife", "bootstrap", "rubin", "bmlmi")) {
    cli::cli_abort(
      "Internal error: invalid pooling class {.val {next_class}}.",
      class = c("rbmiUtils_error_internal", "rbmiUtils_error")
    )
  }

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
  cli::cli_h1("Analysis Object")

  n_imp <- length(x$results)
  fun_name <- x$fun_name %||% "<unknown>"
  cli::cli_text("{n_imp} imputation{?s} analysed with {.fn {fun_name}}")

  cli::cli_rule()

  # Method detection (inherits-based, hardened in 01-02)
  method_class <- if (inherits(x$method, "bayes")) {
    "bayes"
  } else if (inherits(x$method, "approxbayes")) {
    "approxbayes"
  } else if (inherits(x$method, "condmean")) {
    "condmean"
  } else if (inherits(x$method, "bmlmi")) {
    "bmlmi"
  } else {
    "unknown"
  }

  result_class <- class(x$results)[1]
  delta_text <- if (!is.null(x$delta)) "Applied" else "None"

  cli::cli_text("{.field Method}: {method_class}")
  cli::cli_text("{.field Pooling}: {result_class}")
  cli::cli_text("{.field Delta}: {delta_text}")


  # Parameter count and visit info (PRT-03)
  if (length(x$results) > 0 && is.list(x$results[[1]]) &&
      !is.null(names(x$results[[1]]))) {
    param_names <- names(x$results[[1]])
    n_params <- length(param_names)
    cli::cli_text("{.field Parameters}: {n_params}")

    # Extract visit names from parameter names
    visits <- unique(sub("^(trt|lsm_ref|lsm_alt)_", "", param_names))
    visits_text <- paste(visits, collapse = ", ")
    cli::cli_text("{.field Visits}: {visits_text}")
  }

  cli::cli_text("")
  cli::cli_text("Next: {.code pool_obj <- rbmi::pool(analysis_obj)}")

  invisible(x)
}


#' Summary Method for Analysis Objects
#'
#' Provides a detailed summary of an analysis object from [analyse_mi_data()].
#'
#' @param object An object of class `analysis`.
#' @param n_preview Maximum number of parameters to show in the preview table.
#'   Defaults to 5.
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
summary.analysis <- function(object, n_preview = 5, ...) {
  cli::cli_h1("Analysis Object Summary")

  # Imputations section
  cli::cli_h2("Imputations")
  n_imp <- length(object$results)
  cli::cli_text("{.field Count}: {n_imp}")

  # Analysis section
  cli::cli_h2("Analysis")
  fun_name <- object$fun_name %||% "<unknown>"
  cli::cli_text("{.field Function}: {.fn {fun_name}}")
  delta_text <- if (!is.null(object$delta)) "Applied" else "None"
  cli::cli_text("{.field Delta}: {delta_text}")
  if (!is.null(object$delta)) {
    cli::cli_text("{.field Delta rows}: {nrow(object$delta)}")
  }

  # Method section (inherits-based, hardened in 01-02)
  cli::cli_h2("Method")
  method_class <- if (inherits(object$method, "bayes")) {
    "bayes"
  } else if (inherits(object$method, "approxbayes")) {
    "approxbayes"
  } else if (inherits(object$method, "condmean")) {
    "condmean"
  } else if (inherits(object$method, "bmlmi")) {
    "bmlmi"
  } else {
    "unknown"
  }
  cli::cli_text("{.field Type}: {method_class}")

  if (method_class %in% c("bayes", "approxbayes")) {
    if (!is.null(object$method$n_samples)) {
      n_samples <- object$method$n_samples
      cli::cli_text("{.field Samples}: {n_samples}")
    }
  }

  # Pooling section
  cli::cli_h2("Pooling")
  result_class <- class(object$results)[1]
  cli::cli_text("{.field Method}: {result_class}")

  n_params <- NULL
  if (length(object$results) > 0) {
    first_result <- object$results[[1]]
    if (is.list(first_result)) {
      n_params <- length(first_result)
      cli::cli_text("{.field Parameters}: {n_params}")
    }
  }

  # Parameter Preview Table (PRT-04)
  if (length(object$results) > 0 && is.list(object$results[[1]]) &&
      !is.null(names(object$results[[1]]))) {
    params <- object$results[[1]]
    param_names <- names(params)
    preview_names <- utils::head(param_names, n_preview)

    cli::cli_h2("Parameter Preview (from first imputation)")
    for (p in preview_names) {
      est <- params[[p]]$est
      se <- params[[p]]$se
      est_fmt <- round(est, 3)
      se_fmt <- round(se, 3)
      cli::cli_text("  {p}: est={est_fmt}, se={se_fmt}")
    }
    remaining <- length(param_names) - n_preview
    if (remaining > 0) {
      cli::cli_text("  ... and {remaining} more")
    }
  }

  cli::cli_text("")
  cli::cli_text("Next steps:")
  cli::cli_text("  1. {.code pool_obj <- rbmi::pool(analysis_obj)}")
  cli::cli_text("  2. {.code tidy_df <- tidy_pool_obj(pool_obj)}")

  summary_info <- list(
    n_imputations = length(object$results),
    fun_name = object$fun_name,
    has_delta = !is.null(object$delta),
    method_type = method_class,
    pooling_method = result_class
  )

  invisible(summary_info)
}


#' Construct a Stand-in Method Object for a Pooling Strategy
#'
#' When [analyse_mi_data()] is called with `pooling` but no `method`, a
#' method object consistent with the observed number of imputations is
#' required so that `rbmi::pool()` validation passes (`rbmi` asserts result
#' counts against `method$n_samples`).
#'
#' @param pooling Length-one character: `"rubin"`, `"bootstrap"`,
#'   `"jackknife"`, or `"bmlmi"`.
#' @param n_imps Number of distinct imputations observed in the data.
#'
#' @return An `rbmi` method object.
#' @keywords internal
#' @noRd
make_standin_method <- function(pooling, n_imps) {
  switch(
    pooling,
    rubin = rbmi::method_bayes(n_samples = n_imps),
    bootstrap = rbmi::method_condmean(type = "bootstrap", n_samples = n_imps - 1),
    jackknife = rbmi::method_condmean(type = "jackknife"),
    bmlmi = cli::cli_abort(
      c(
        "{.val bmlmi} pooling cannot be selected via {.arg pooling} alone.",
        "i" = "BMLMI pooling needs the number of analyses per imputation (D), which cannot be inferred from the data.",
        "i" = "Supply {.code method = rbmi::method_bmlmi(B = , D = )} instead."
      ),
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  )
}
