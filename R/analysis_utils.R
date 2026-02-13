#' G-computation Analysis for a Single Visit
#'
#' Performs logistic regression and estimates marginal effects for binary outcomes.
#'
#' @param data A data.frame with one visit of data.
#' @param vars A list containing `group`, `outcome`, `covariates`, and `visit`.
#' @param reference_levels Optional vector specifying reference level(s) of the treatment factor.
#' @param var_method Marginal variance estimation method (default: "Ge").
#' @param type Type of robust variance estimator (default: "HC0").
#' @param contrast Type of contrast to compute (default: "diff").
#'
#' @return A named list containing estimates and standard errors for treatment comparisons and within-arm means.
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(rbmi)
#' library(rbmiUtils)
#'
#' data("ADMI")
#'
#' # Prepare data for a single visit
#' ADMI <- ADMI |>
#'   mutate(
#'     TRT = factor(TRT, levels = c("Placebo", "Drug A")),
#'     STRATA = factor(STRATA),
#'     REGION = factor(REGION)
#'   )
#'
#' dat_single <- ADMI |>
#'   filter(AVISIT == "Week 24")
#'
#' vars <- set_vars(
#'   subjid = "USUBJID",
#'   visit = "AVISIT",
#'   group = "TRT",
#'   outcome = "CRIT1FLN",
#'   covariates = c("BASE", "STRATA", "REGION")
#' )
#'
#' result <- gcomp_responder(
#'   data = dat_single,
#'   vars = vars,
#'   reference_levels = "Placebo"
#' )
#'
#' print(result)
#' }
gcomp_responder <- function(
  data,
  vars,
  reference_levels = NULL,
  var_method = "Ge",
  type = "HC0",
  contrast = "diff"
) {
  outcome <- vars$outcome
  group <- vars$group
  covariates <- vars$covariates
  visit <- vars$visit

  # Local helper to avoid calling deprecated extract_covariates2()
  .extract_covars <- function(x) {
    if (is.null(x)) return(x)
    unique(trimws(unlist(strsplit(x, ":|\\*"))))
  }

  # --- Input validation (fail-early) ---
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} must be a {.cls data.frame}, not {.cls {class(data)}}.",
      class = "rbmiUtils_error_type"
    )
  }

  required_cols <- unique(c(group, outcome, .extract_covars(covariates)))
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Column{?s} {.field {missing_cols}} not found in {.arg data}.",
      class = "rbmiUtils_error_validation"
    )
  }

  n_arms <- length(unique(data[[group]]))
  if (n_arms < 2) {
    cli::cli_abort(
      "{.arg data} contains only {n_arms} treatment arm{?s}. G-computation requires at least 2 arms.",
      class = "rbmiUtils_error_validation"
    )
  }

  if (!is.numeric(data[[outcome]])) {
    cli::cli_abort(
      "Outcome column {.field {outcome}} must be numeric (0/1), not {.cls {class(data[[outcome]])}}.",
      class = "rbmiUtils_error_type"
    )
  }

  outcome_vals <- data[[outcome]][!is.na(data[[outcome]])]
  if (length(unique(outcome_vals)) < 2) {
    cli::cli_abort(
      "Outcome column {.field {outcome}} has zero variance (all values are {outcome_vals[1]}). G-computation requires variation in the outcome.",
      class = "rbmiUtils_error_validation"
    )
  }

  if (is.null(reference_levels)) {
    reference_levels <- levels(data[[group]])[1]
  }

  covars_for_model <- setdiff(unique(c(group, .extract_covars(covariates))), visit)
  frm <- stats::as.formula(
    paste0(outcome, " ~ 1 + ", paste0(covars_for_model, collapse = " + "))
  )
  environment(frm) <- globalenv()

  model <- stats::glm(frm, data = data, family = binomial)

  marginal_fit <- beeca::get_marginal_effect(
    model,
    trt = group,
    method = var_method,
    type = type,
    contrast = contrast,
    reference = reference_levels
  )

  # --- beeca output schema validation ---
  if (is.null(marginal_fit$marginal_results)) {
    cli::cli_abort(
      "beeca::get_marginal_effect() did not return {.field marginal_results}. This may indicate an incompatible beeca version.",
      class = "rbmiUtils_error_dependency"
    )
  }
  expected_cols <- c("STAT", "STATVAL", "TRTVAL")
  missing_beeca_cols <- setdiff(expected_cols, names(marginal_fit$marginal_results))
  if (length(missing_beeca_cols) > 0) {
    cli::cli_abort(
      "beeca output missing expected column{?s}: {.field {missing_beeca_cols}}. This may indicate an incompatible beeca version.",
      class = "rbmiUtils_error_dependency"
    )
  }

  res <- marginal_fit$marginal_results |>
    dplyr::filter(STAT %in% c("diff", "diff_se", "risk", "risk_se"))

  lsm <- res |>
    dplyr::filter(STAT %in% c("risk", "risk_se")) |>
    dplyr::group_by(TRTVAL) |>
    dplyr::group_map(
      ~ {
        list_name <- paste0("lsm_", .y$TRTVAL)
        named_list <- list(
          est = .x$STATVAL[.x$STAT == "risk"],
          se = .x$STATVAL[.x$STAT == "risk_se"],
          df = NA
        )
        setNames(list(named_list), list_name)
      }
    ) |>
    purrr::flatten()

  trt <- res |>
    dplyr::filter(STAT %in% c("diff", "diff_se")) |>
    dplyr::group_by(TRTVAL) |>
    dplyr::group_map(
      ~ {
        trtval <- sub("diff: ", "", .y$TRTVAL)
        list_name <- paste0("trt_", trtval)
        named_list <- list(
          est = .x$STATVAL[.x$STAT == "diff"],
          se = .x$STATVAL[.x$STAT == "diff_se"],
          df = NA
        )
        setNames(list(named_list), list_name)
      }
    ) |>
    purrr::flatten()

  return(c(trt, lsm))
}


#' G-computation for a Binary Outcome at Multiple Visits
#'
#' Applies `gcomp_responder()` separately for each unique visit in the data.
#'
#' @param data A data.frame containing multiple visits.
#' @param vars A list specifying analysis variables.
#' @param reference_levels Optional reference level for the treatment variable.
#' @param ... Additional arguments passed to `gcomp_responder()`.
#'
#' @return A named list of estimates for each visit and treatment group.
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(rbmi)
#' library(rbmiUtils)
#'
#' data("ADMI")
#'
#' ADMI <- ADMI |>
#'   mutate(
#'     TRT = factor(TRT, levels = c("Placebo", "Drug A")),
#'     STRATA = factor(STRATA),
#'     REGION = factor(REGION)
#'   )
#'
#' # Note: method must match the original used for imputation
#' method <- method_bayes(
#'   n_samples = 100,
#'   control = control_bayes(warmup = 20, thin = 2)
#' )
#'
#' vars_binary <- set_vars(
#'   subjid = "USUBJID",
#'   visit = "AVISIT",
#'   group = "TRT",
#'   outcome = "CRIT1FLN",
#'   covariates = c("BASE", "STRATA", "REGION")
#' )
#'
#' ana_obj_prop <- analyse_mi_data(
#'   data = ADMI,
#'   vars = vars_binary,
#'   method = method,
#'   fun = gcomp_responder_multi,
#'   reference_levels = "Placebo",
#'   contrast = "diff",
#'   var_method = "Ge",
#'   type = "HC0"
#' )
#'
#' pool(ana_obj_prop)
#' }
gcomp_responder_multi <- function(data, vars, reference_levels = NULL, ...) {
  visit_var <- vars$visit

  # --- Minimal validation ---
  if (!visit_var %in% names(data)) {
    cli::cli_abort(
      "Column {.field {visit_var}} (visit variable) not found in {.arg data}.",
      class = "rbmiUtils_error_validation"
    )
  }

  visits <- unique(data[[visit_var]])

  if (length(visits) == 0) {
    cli::cli_abort(
      "{.arg data} contains no visits in column {.field {visit_var}}.",
      class = "rbmiUtils_error_validation"
    )
  }

  results <- lapply(visits, function(v) {
    dat <- data[data[[visit_var]] == v, ]

    # Drop unused factor levels after subsetting
    dat <- droplevels(dat)

    res <- gcomp_responder(
      data = dat,
      vars = vars,
      reference_levels = reference_levels,
      ...
    )
    stats::setNames(res, paste0(names(res), "_", v))
  })

  return(unlist(results, recursive = FALSE))
}


#' Convert character variable list to formula
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated and will be removed in a future version.
#'
#' @param outcome Character string, the outcome variable.
#' @param covars Character vector of covariates.
#' @return A formula object.
#' @keywords internal
as_simple_formula2 <- function(outcome, covars) {
  lifecycle::deprecate_warn(
    "0.2.0",
    "as_simple_formula2()",
    details = "Internal helper will be removed in a future version."
  )
  frm <- stats::as.formula(
    paste0(
      outcome,
      " ~ 1 + ",
      paste0(covars, collapse = " + ")
    )
  )
  environment(frm) <- globalenv()
  frm
}

#' Extract variable names from model terms
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Takes a character vector including potentially model terms like `*` and `:` and
#' extracts out the individual variables.
#' This function is deprecated and will be removed in a future version.
#'
#' @param x A character vector of model terms.
#' @return A character vector of unique variable names.
#' @keywords internal
extract_covariates2 <- function(x) {
  lifecycle::deprecate_warn(
    "0.2.0",
    "extract_covariates2()",
    details = "Internal helper will be removed in a future version."
  )
  if (is.null(x)) return(x)
  x_split <- strsplit(x, ":|\\*")
  unique(trimws(unlist(x_split)))
}
