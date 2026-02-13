#' Get Imputed Data Sets as a data frame
#'
#' This function takes an imputed dataset and a mapping variable to return a dataset
#' with the original IDs mapped back and renamed appropriately.
#'
#' @param impute_obj The imputation object from which the imputed datasets are extracted.
#'
#' @return A data frame with the original subject IDs mapped and renamed.
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' library(rbmi)
#' library(rbmiUtils)
#'
#' set.seed(1974)
#' # Load example dataset
#' data("ADEFF")
#'
#' # Prepare data
#' ADEFF <- ADEFF |>
#'   mutate(
#'     TRT = factor(TRT01P, levels = c("Placebo", "Drug A")),
#'     USUBJID = factor(USUBJID),
#'     AVISIT = factor(AVISIT)
#'   )
#'
#' # Define variables for imputation
#' vars <- set_vars(
#'   subjid = "USUBJID",
#'   visit = "AVISIT",
#'   group = "TRT",
#'   outcome = "CHG",
#'   covariates = c("BASE", "STRATA", "REGION")
#' )
#'
#' # Define Bayesian imputation method
#' method <- method_bayes(
#'   n_samples = 100,
#'   control = control_bayes(warmup = 200, thin = 2)
#' )
#'
#' # Generate draws and perform imputation
#' draws_obj <- draws(data = ADEFF, vars = vars, method = method)
#' impute_obj <- impute(draws_obj,
#'   references = c("Placebo" = "Placebo", "Drug A" = "Placebo"))
#'
#' # Extract imputed data with original subject IDs
#' admi <- get_imputed_data(impute_obj)
#' head(admi)
#'}
get_imputed_data <- function(impute_obj) {
  # Check class of impute_obj
  if (!inherits(impute_obj, "imputation")) {
    stop(
      "impute_obj must be of an imputation object outputted from rbmi::impute"
    )
  }

  # Extract the `subjid` variable from the `vars` list
  uid <- impute_obj$data$vars$subjid

  # Extract imputed datasets
  imputed_dfs <- rbmi::extract_imputed_dfs(impute_obj, idmap = TRUE)

  # Extract the ID map from the attributes of the first imputed dataset
  idmap <- attributes(imputed_dfs[[1]])$idmap

  # Convert the imputed data list into a data frame, adding an IMPID variable
  imputed_dfs2 <- imputed_dfs |>
    purrr::map_dfr(~.x, .id = "IMPID")

  # Map original IDs back to the data
  imputed_dfs2$original_id <- idmap[match(imputed_dfs2[[uid]], names(idmap))]

  # Rename to ensure original ID has the correct name
  result <- imputed_dfs2 |>
    dplyr::rename(internal_id = {{ uid }}) |>
    dplyr::rename({{ uid }} := original_id)

  return(result)
}


#' Utility function for Generalized G-computation for Binary Outcomes
#'
#' Wrapper function for targeting a marginal treatment effect
#' using g-computation using the beeca package. Intended for binary endpoints.
#'
#' @param data A data.frame containing the analysis dataset.
#' @param outcome Name of the binary outcome variable (as string).
#' @param treatment Name of the treatment variable (as string).
#' @param covariates Character vector of covariate names to adjust for.
#' @param reference Reference level for the treatment variable (default: "Placebo").
#' @param contrast Type of contrast to compute (default: "diff").
#' @param method Marginal estimation method for variance (default: "Ge").
#' @param type Variance estimator type (default: "HC0").
#' @param ... Additional arguments passed to `beeca::get_marginal_effect()`.
#'
#' @return A named list with treatment effect estimate, standard error, and degrees of freedom (if applicable).
#'
#' @export
#'
#' @examples
#' # Load required packages
#' library(rbmiUtils)
#' library(beeca)      # for get_marginal_effect()
#' library(dplyr)
#' # Load example data
#' data("ADMI")
#' # Ensure correct factor levels
#' ADMI <- ADMI |>
#'   mutate(
#'     TRT = factor(TRT, levels = c("Placebo", "Drug A")),
#'     STRATA = factor(STRATA),
#'     REGION = factor(REGION)
#'   )
#' # Apply g-computation for binary responder
#' result <- gcomp_binary(
#'   data = ADMI,
#'   outcome = "CRIT1FLN",
#'   treatment = "TRT",
#'   covariates = c("BASE", "STRATA", "REGION"),
#'   reference = "Placebo",
#'   contrast = "diff",
#'   method = "Ge",    # from beeca: GEE robust sandwich estimator
#'   type = "HC0"      # from beeca: heteroskedasticity-consistent SE
#' )
#'
#' # Print results
#' print(result)
#'
gcomp_binary <- function(
  data,
  outcome = "CRIT1FLN",
  treatment = "TRT",
  covariates = c("BASE", "STRATA", "REGION"),
  reference = "Placebo",
  contrast = "diff",
  method = "Ge",
  type = "HC0",
  ...
) {
  # --- Input validation (fail-early) ---
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} must be a {.cls data.frame}, not {.cls {class(data)}}.",
      class = "rbmiUtils_error_type"
    )
  }

  required_cols <- unique(c(treatment, outcome, covariates))
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Column{?s} {.field {missing_cols}} not found in {.arg data}.",
      class = "rbmiUtils_error_validation"
    )
  }

  n_arms <- length(unique(data[[treatment]]))
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

  # Construct formula
  form <- stats::as.formula(
    paste0(outcome, " ~ ", paste(c(treatment, covariates), collapse = " + "))
  )

  # Fit logistic regression
  model <- stats::glm(form, data = data, family = binomial)

  # Compute marginal treatment effect
  marginal_fit <- beeca::get_marginal_effect(
    model,
    trt = treatment,
    method = method,
    type = type,
    contrast = contrast,
    reference = reference,
    ...
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

  res <- marginal_fit$marginal_results

  out <- list(
    trt = list(
      est = res[res$STAT == paste0(contrast), "STATVAL"][[1]],
      se = res[res$STAT == paste0(contrast, "_se"), "STATVAL"][[1]],
      df = NA
    )
  )

  return(out)
}
