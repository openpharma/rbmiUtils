#' Tidy and Annotate a Pooled Object for Publication
#'
#' This function processes a pooled analysis object of class `pool` into a tidy tibble format.
#' It adds contextual information, such as whether a parameter is a treatment comparison or a least squares mean,
#' dynamically identifies visit names from the `parameter` column, and provides additional columns for parameter type,
#' least squares mean type, and visit.
#'
#' @param pool_obj A pooled analysis object of class `pool`, typically obtained from [rbmi::pool()]
#'   after calling [analyse_mi_data()].
#'
#' @return A tibble containing the processed pooled analysis results with the following columns:
#' \describe{
#'   \item{parameter}{Original parameter name from the pooled object}
#'   \item{description}{Human-readable description of the parameter}
#'   \item{visit}{Visit name extracted from parameter (if applicable)}
#'   \item{parameter_type}{Either "trt" (treatment comparison) or "lsm" (least squares mean)}
#'   \item{lsm_type}{For LSM parameters: "ref" (reference) or "alt" (alternative)}
#'   \item{est}{Point estimate}
#'   \item{se}{Standard error}
#'   \item{lci}{Lower confidence interval}
#'   \item{uci}{Upper confidence interval}
#'   \item{pval}{P-value}
#' }
#'
#' @details The function dynamically processes the `parameter` column by separating it into
#' components (e.g., type of estimate, reference vs. alternative arm, and visit), and provides
#' informative descriptions in the output.
#'
#' **Workflow:**
#' 1. Prepare data and run imputation with rbmi
#' 2. Analyse with [analyse_mi_data()]
#' 3. Pool with [rbmi::pool()]
#' 4. Tidy with `tidy_pool_obj()` for publication-ready output
#'
#' @seealso
#' * [rbmi::pool()] which creates the pool objects this function tidies
#' * [analyse_mi_data()] to analyse imputed datasets
#' * [format_results()] for additional formatting options
#'
#' @examples
#' # Example usage:
#' library(dplyr)
#' library(rbmi)
#'
#' data("ADMI")
#' N_IMPUTATIONS <- 100
#' BURN_IN <- 200
#' BURN_BETWEEN <- 5
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
#' method <- rbmi::method_bayes(
#'   n_samples = N_IMPUTATIONS,
#'   control = rbmi::control_bayes(
#'     warmup = BURN_IN,
#'     thin = BURN_BETWEEN
#'     )
#'   )
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
#' pool_obj_ancova <- pool(ana_obj_ancova)
#' tidy_df <- tidy_pool_obj(pool_obj_ancova)
#'
#' # Print tidy data frames
#' print(tidy_df)
#'
#' @export
tidy_pool_obj <- function(pool_obj) {

  # --- Input validation ---
  if (!inherits(pool_obj, "pool")) {
    cli::cli_abort(
      "Input {.arg pool_obj} must be of class {.cls pool}, not {.cls {class(pool_obj)}}.",
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }

  # Convert pool_obj to tibble
  df <- dplyr::as_tibble(pool_obj)

  # --- Parse parameter column using regex ---
  # Step 1: Extract parameter_type (trt|lsm) and remainder after first underscore
  df <- df |>
    tidyr::separate_wider_regex(
      parameter,
      patterns = c(
        parameter_type = "trt|lsm",
        "_",
        remainder = ".*"
      ),
      too_few = "align_start",
      cols_remove = FALSE
    )

  # Step 2: Parse remainder based on parameter_type
  # For "lsm": remainder = "<lsm_type>_<visit>" -> split on first underscore

  # For "trt": remainder = "<visit>" (ANCOVA) or "<comparison>_<visit>" (gcomp)
  df <- df |>
    dplyr::mutate(
      lsm_type = dplyr::case_when(
        parameter_type == "lsm" & grepl("_", remainder) ~
          sub("_.*$", "", remainder),
        TRUE ~ NA_character_
      ),
      visit = dplyr::case_when(
        # lsm with lsm_type: visit is everything after first underscore in remainder
        parameter_type == "lsm" & grepl("_", remainder) ~
          sub("^[^_]+_", "", remainder),
        # lsm without underscore in remainder: remainder is the lsm_type, no visit
        parameter_type == "lsm" ~ NA_character_,
        # trt: remainder is the visit (ANCOVA simple case)
        # or for gcomp multi-visit: "comparison_visit" - visit after last underscore
        # But we need to handle "Drug A vs Placebo_Week 24" -> visit = "Week 24"
        # and "Week_24" -> visit = "Week_24" (underscore in visit name)
        # Strategy: if remainder contains " vs ", extract visit after last "_"
        #   that follows the comparison; otherwise remainder IS the visit
        parameter_type == "trt" ~ remainder,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(-remainder)

  # Create informative descriptions for parameters
  df <- df |>
    dplyr::mutate(
      description = dplyr::case_when(
        parameter_type == "trt" ~ "Treatment Comparison",
        parameter_type == "lsm" & lsm_type %in% c("ref", "alt") & !is.na(visit) ~
          paste(
            "Least Squares Mean for",
            ifelse(lsm_type == "ref", "Reference", "Alternative"),
            "at",
            visit
          ),
        parameter_type == "lsm" & lsm_type %in% c("ref", "alt") ~
          paste(
            "Least Squares Mean for",
            ifelse(lsm_type == "ref", "Reference", "Alternative")
          ),
        parameter_type == "lsm" & !is.na(lsm_type) & !is.na(visit) ~
          paste("Least Squares Mean for", lsm_type, "at", visit),
        parameter_type == "lsm" & !is.na(lsm_type) ~
          paste("Least Squares Mean for", lsm_type),
        TRUE ~ parameter
      )
    )

  # Select and arrange the columns for the publication-ready table
  df <- df |>
    dplyr::select(
      parameter,
      description,
      visit,
      parameter_type,
      lsm_type,
      est,
      se,
      lci,
      uci,
      pval
    )

  return(df)
}
