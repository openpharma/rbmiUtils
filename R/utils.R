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
#' # Example usage:
#' # result <- get_imputed_data(impute_obj)
#'
get_imputed_data <- function(impute_obj) {

  # Check class of impute_obj
  if (!inherits(impute_obj, "imputation")) {
    stop("impute_obj must be of an imputation object outputted from rbmi::impute")
  }

  # Extract the `subjid` variable from the `vars` list
  uid <- impute_obj$data$vars$subjid

  # Extract imputed datasets
  imputed_dfs <- rbmi::extract_imputed_dfs(impute_obj, idmap = TRUE)

  # Extract the ID map from the attributes of the first imputed dataset
  # example code from issue https://github.com/insightsengineering/rbmi/issues/382
  idmap <- attributes(imputed_dfs[[1]])$idmap

  # Convert the imputed data list into a data frame, adding an IMPID variable
  imputed_dfs2 <- imputed_dfs |>
    purrr::map_dfr(~ .x, .id = "IMPID")

  # Map original IDs back to the data
  imputed_dfs2$original_id <- idmap[match(imputed_dfs2[[uid]], names(idmap))]

  # Rename to ensure original ID has the correct name
  result <- imputed_dfs2 |>
    dplyr::rename(internal_id = {{ uid }}) |>
    dplyr::rename({{ uid }} := original_id)

  return(result)
}



#' WIP: Utility function for Generalized G-computation for Binary Outcomes
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
#' @section Lifecycle:
#' [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
#'
#' @export
#'
#' @examples
#' # Load required packages
#' library(rbmiUtils)  # assuming gcomp_binary is defined in this package
#' library(beeca)      # for get_marginal_effect()
#' library(dplyr)
#' # Load example data
#' data("ADMI")
#' # Ensure correct factor levels
#' ADMI <- ADMI %>%
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
gcomp_binary <- function(data,
                         outcome = "CRIT1FLN",
                         treatment = "TRT",
                         covariates = c("BASE", "STRATA", "REGION"),
                         reference = "Placebo",
                         contrast = "diff",
                         method = "Ge",
                         type = "HC0",
                         ...) {
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




