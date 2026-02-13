#' Convert Pool Object to ARD Format
#'
#' Converts an rbmi pool object to the pharmaverse Analysis Results Dataset
#' (ARD) standard using the \pkg{cards} package. The ARD format is a
#' long-format data frame where each row represents a single statistic for a
#' given parameter, with grouping columns for visit, parameter type, and
#' least-squares-mean type.
#'
#' @param pool_obj A pooled analysis object of class `"pool"`, typically
#'   obtained from [rbmi::pool()] after calling [analyse_mi_data()].
#' @param analysis_obj An optional analysis object (output of
#'   [analyse_mi_data()]), used to compute MI diagnostic statistics. When
#'   provided and the pooling method is Rubin's rules, the ARD includes
#'   additional stat rows for FMI, lambda, RIV, Barnard-Rubin adjusted df,
#'   complete-data df, relative efficiency, and the number of imputations per
#'   parameter. When `NULL` (the default), only the base ARD is returned.
#' @param conf.level Confidence level used for CI labels (e.g., `0.95` produces
#'   "95% CI Lower"). If `NULL` (the default), the value is taken from
#'   `pool_obj$conf.level`. If that is also `NULL`, defaults to `0.95`.
#'
#' @return A data frame of class `"card"` (ARD format) with grouping columns
#'   for visit (`group1`), parameter_type (`group2`), and lsm_type (`group3`).
#'   Each parameter produces rows for five statistics: estimate, std.error,
#'   conf.low, conf.high, and p.value, plus a method row. When `analysis_obj`
#'   is provided and the pooling method is Rubin's rules, additional diagnostic
#'   stat rows are included: fmi, lambda, riv, df.adjusted, df.complete, re,
#'   and m.imputations.
#'
#' @details
#' The function works by:
#' 1. Tidying the pool object via [tidy_pool_obj()]
#' 2. Reshaping each parameter into long-format ARD rows (one row per statistic)
#' 3. Adding grouping columns (visit, parameter_type, lsm_type)
#' 4. Optionally enriching with MI diagnostic statistics when `analysis_obj`
#'    is provided
#' 5. Applying [cards::as_card()] and [cards::tidy_ard_column_order()] for
#'    standard ARD structure
#'
#' When `analysis_obj` is provided:
#' - For Rubin's rules pooling: diagnostic statistics (FMI, lambda, RIV,
#'   Barnard-Rubin adjusted df, relative efficiency) are computed per parameter
#'   using the per-imputation estimates, standard errors, and degrees of freedom.
#' - For non-Rubin pooling methods: an informative message is emitted and the
#'   base ARD is returned without diagnostic rows.
#'
#' The resulting ARD passes [cards::check_ard_structure()] validation and is
#' suitable for downstream use with \pkg{gtsummary}.
#'
#' @seealso
#' * [rbmi::pool()] for creating pool objects
#' * [analyse_mi_data()] for creating analysis objects
#' * [tidy_pool_obj()] for the underlying data transformation
#' * [cards::as_card()] and [cards::check_ard_structure()] for ARD validation
#'
#' @examples
#' \donttest{
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   library(rbmi)
#'   data("ADMI", package = "rbmiUtils")
#'   ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
#'   ADMI$USUBJID <- factor(ADMI$USUBJID)
#'   ADMI$AVISIT <- factor(ADMI$AVISIT)
#'
#'   vars <- set_vars(
#'     subjid = "USUBJID", visit = "AVISIT", group = "TRT",
#'     outcome = "CHG", covariates = c("BASE", "STRATA", "REGION")
#'   )
#'   method <- method_bayes(
#'     n_samples = 20,
#'     control = control_bayes(warmup = 20, thin = 1)
#'   )
#'
#'   ana_obj <- analyse_mi_data(ADMI, vars, method, fun = ancova)
#'   pool_obj <- pool(ana_obj)
#'
#'   # Base ARD
#'   ard <- pool_to_ard(pool_obj)
#'
#'   # Enriched ARD with MI diagnostics (FMI, lambda, RIV, df)
#'   ard_diag <- pool_to_ard(pool_obj, analysis_obj = ana_obj)
#' }
#' }
#'
#' @export
pool_to_ard <- function(pool_obj, analysis_obj = NULL, conf.level = NULL) {

  # --- Dependency check ---
  if (!is_cards_available()) {
    cli::cli_abort(
      c(
        "Package {.pkg cards} is required for ARD conversion.",
        "i" = "Install with {.code install.packages(\"cards\")}."
      ),
      class = c("rbmiUtils_error_dependency", "rbmiUtils_error")
    )
  }

  # --- Input validation ---
  if (!inherits(pool_obj, "pool")) {
    cli::cli_abort(
      "Input {.arg pool_obj} must be of class {.cls pool}, not {.cls {class(pool_obj)}}.",
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }

  # --- Validate analysis_obj ---
  if (!is.null(analysis_obj)) {
    if (!is.list(analysis_obj) || is.null(analysis_obj$results) ||
        !is.list(analysis_obj$results)) {
      cli::cli_abort(
        "{.arg analysis_obj} must have a {.field $results} element that is a list.",
        class = c("rbmiUtils_error_validation", "rbmiUtils_error")
      )
    }
    # Verify parameter names match between pool_obj and analysis_obj
    pool_names <- sort(names(pool_obj$pars))
    analysis_names <- sort(names(analysis_obj$results[[1]]))
    if (!identical(pool_names, analysis_names)) {
      cli::cli_abort(
        c(
          "Parameter names in {.arg pool_obj} and {.arg analysis_obj} do not match.",
          "i" = "pool_obj parameters: {.val {names(pool_obj$pars)}}",
          "i" = "analysis_obj parameters: {.val {names(analysis_obj$results[[1]])}}"
        ),
        class = c("rbmiUtils_error_validation", "rbmiUtils_error")
      )
    }
  }

  # --- Extract conf.level ---
  if (is.null(conf.level)) {
    conf.level <- pool_obj$conf.level %||% 0.95
  }

  # --- Tidy the pool object ---
  tidy_df <- tidy_pool_obj(pool_obj)

  # --- Define statistic names and labels ---
  stat_names <- c("estimate", "std.error", "conf.low", "conf.high", "p.value")
  stat_labels <- c(
    "Estimate",
    "Std. Error",
    paste0(conf.level * 100, "% CI Lower"),
    paste0(conf.level * 100, "% CI Upper"),
    "p-value"
  )
  n_stats <- length(stat_names)

  # --- Extract method label ---
  method_label <- if (!is.null(pool_obj$method)) pool_obj$method else "unknown"

  # --- Build long-format ARD rows ---
  rows <- lapply(seq_len(nrow(tidy_df)), function(i) {
    r <- tidy_df[i, ]

    lsm_val <- if (is.na(r$lsm_type)) NA_character_ else r$lsm_type

    # Number of rows = n_stats + 1 (for method row)
    n_total <- n_stats + 1L

    data.frame(
      group1       = rep("visit", n_total),
      group1_level = I(as.list(rep(r$visit, n_total))),
      group2       = rep("parameter_type", n_total),
      group2_level = I(as.list(rep(r$parameter_type, n_total))),
      group3       = rep("lsm_type", n_total),
      group3_level = I(as.list(rep(lsm_val, n_total))),
      variable       = rep(r$parameter, n_total),
      variable_level = I(as.list(rep(NA, n_total))),
      context        = rep("rbmi_pool", n_total),
      stat_name  = c(stat_names, "method"),
      stat_label = c(stat_labels, "Method"),
      stat       = I(list(r$est, r$se, r$lci, r$uci, r$pval, method_label)),
      fmt_fun    = I(as.list(rep(1L, n_total))),
      warning    = I(as.list(rep(list(NULL), n_total))),
      error      = I(as.list(rep(list(NULL), n_total))),
      stringsAsFactors = FALSE
    )
  })

  # --- Assemble and convert to ARD ---
  ard_df <- do.call(rbind, rows)

  # --- Diagnostic enrichment ---
  if (!is.null(analysis_obj)) {
    diag_rows <- compute_mi_diagnostics(pool_obj, analysis_obj, tidy_df)
    if (!is.null(diag_rows)) {
      ard_df <- rbind(ard_df, diag_rows)
    }
  }

  cards::tidy_ard_column_order(cards::as_card(ard_df))
}


#' Check if cards package is available
#'
#' Internal helper extracted for testability. Wraps
#' `requireNamespace("cards", quietly = TRUE)`.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
is_cards_available <- function() {
  requireNamespace("cards", quietly = TRUE)
}


#' Compute MI Diagnostic Statistics for All Parameters
#'
#' Internal helper that orchestrates the computation of MI diagnostic
#' statistics across all parameters. Checks the pooling method, extracts
#' per-imputation estimates/SEs/dfs, calls [compute_rubin_diagnostics()]
#' for each parameter, and returns diagnostic ARD rows.
#'
#' @param pool_obj A pooled analysis object of class `"pool"`.
#' @param analysis_obj An analysis object from [analyse_mi_data()].
#' @param tidy_df Tidy data frame from [tidy_pool_obj()].
#'
#' @return A data.frame of diagnostic ARD rows to rbind with the base ARD,
#'   or `NULL` if diagnostics are not applicable (non-Rubin pooling).
#'
#' @keywords internal
#' @noRd
compute_mi_diagnostics <- function(pool_obj, analysis_obj, tidy_df) {

  # --- Check pooling method ---
  if (is.null(pool_obj$method) || pool_obj$method != "rubin") {
    cli::cli_inform(
      "MI diagnostic statistics are only available for Rubin's rules pooling. Diagnostics omitted from ARD."
    )
    return(NULL)
  }

  # --- Extract number of imputations ---
  M <- length(analysis_obj$results)

  # --- Compute diagnostics per parameter ---
  param_names <- names(analysis_obj$results[[1]])
  diag_row_list <- lapply(param_names, function(pname) {
    # Extract per-imputation estimates, SEs, and dfs
    ests <- vapply(analysis_obj$results, function(imp) imp[[pname]]$est, numeric(1))
    ses  <- vapply(analysis_obj$results, function(imp) imp[[pname]]$se, numeric(1))
    dfs  <- vapply(analysis_obj$results, function(imp) imp[[pname]]$df, numeric(1))

    # Complete-data df (constant across imputations)
    v_com <- unique(dfs)[1]

    # Compute Rubin's rules diagnostics
    diag <- compute_rubin_diagnostics(ests, ses, v_com, M)

    # Find matching row in tidy_df for grouping columns
    param_row <- tidy_df[tidy_df$parameter == pname, ]

    # Build diagnostic ARD rows
    build_diagnostic_ard_rows(diag, param_row, M)
  })

  do.call(rbind, diag_row_list)
}


#' Build Diagnostic ARD Rows for One Parameter
#'
#' Internal helper that constructs a data.frame of diagnostic stat rows
#' for a single parameter, using the same grouping columns as the base
#' ARD rows.
#'
#' @param diag Named list from [compute_rubin_diagnostics()].
#' @param param_row One-row data.frame from [tidy_pool_obj()] for this parameter.
#' @param M Integer, number of imputations.
#'
#' @return A data.frame of diagnostic ARD rows (7 rows: fmi, lambda, riv,
#'   df.adjusted, df.complete, re, m.imputations).
#'
#' @keywords internal
#' @noRd
build_diagnostic_ard_rows <- function(diag, param_row, M) {

  diag_stat_names <- c(
    "fmi", "lambda", "riv", "df.adjusted", "df.complete", "re", "m.imputations"
  )
  diag_stat_labels <- c(
    "Fraction of Missing Information",
    "Lambda",
    "Relative Increase in Variance",
    "Barnard-Rubin Adjusted df",
    "Complete-Data df",
    "Relative Efficiency",
    "Number of Imputations"
  )
  diag_stat_values <- list(
    diag$fmi, diag$lambda, diag$riv, diag$df_adj, diag$dfcom, diag$re, M
  )

  n_diag <- length(diag_stat_names)

  lsm_val <- if (is.na(param_row$lsm_type)) NA_character_ else param_row$lsm_type

  data.frame(
    group1       = rep("visit", n_diag),
    group1_level = I(as.list(rep(param_row$visit, n_diag))),
    group2       = rep("parameter_type", n_diag),
    group2_level = I(as.list(rep(param_row$parameter_type, n_diag))),
    group3       = rep("lsm_type", n_diag),
    group3_level = I(as.list(rep(lsm_val, n_diag))),
    variable       = rep(param_row$parameter, n_diag),
    variable_level = I(as.list(rep(NA, n_diag))),
    context        = rep("rbmi_pool", n_diag),
    stat_name  = diag_stat_names,
    stat_label = diag_stat_labels,
    stat       = I(diag_stat_values),
    fmt_fun    = I(as.list(rep(1L, n_diag))),
    warning    = I(as.list(rep(list(NULL), n_diag))),
    error      = I(as.list(rep(list(NULL), n_diag))),
    stringsAsFactors = FALSE
  )
}


#' Compute Rubin's Rules Diagnostic Statistics
#'
#' Pure computational function implementing Rubin's rules variance
#' decomposition for multiple imputation diagnostics. Computes within-
#' and between-imputation variance, fraction of missing information (FMI),
#' lambda, relative increase in variance (RIV), Barnard-Rubin adjusted
#' degrees of freedom, and relative efficiency.
#'
#' @param ests Numeric vector of per-imputation point estimates.
#' @param ses Numeric vector of per-imputation standard errors.
#' @param v_com Numeric scalar for complete-data degrees of freedom.
#' @param M Integer, number of imputations.
#'
#' @return Named list with elements: `var_w` (within-imputation variance),
#'   `var_b` (between-imputation variance), `var_t` (total variance),
#'   `lambda` (proportion of variance due to missingness),
#'   `riv` (relative increase in variance),
#'   `df_adj` (Barnard-Rubin adjusted degrees of freedom),
#'   `dfcom` (complete-data df, pass-through of `v_com`),
#'   `fmi` (adjusted fraction of missing information),
#'   `re` (relative efficiency).
#'
#' @details
#' Formulas follow Rubin (1987) and Barnard & Rubin (1999), verified
#' against `rbmi:::rubin_rules` and `rbmi:::rubin_df` source code.
#' The adjusted FMI follows the mice package convention:
#' `fmi = (riv + 2/(df_adj + 3)) / (1 + riv)`.
#'
#' Edge case handling:
#' - All SEs are NA: returns list with all values NA (except dfcom)
#' - `var_b == 0` (lambda == 0): `df_adj = v_obs` (skip `v_old`)
#' - `v_com` is Inf and `var_b == 0`: `df_adj = Inf`
#' - `v_com` is Inf and `lambda > 0`: `df_adj = v_old = (M-1)/lambda^2`
#' - `v_com` is NA: `df_adj = NA`, `fmi = NA`, `re = NA`
#'
#' @keywords internal
#' @noRd
compute_rubin_diagnostics <- function(ests, ses, v_com, M) {

  # --- Handle all-NA SEs ---
  if (all(is.na(ses))) {
    return(list(
      var_w  = NA_real_,
      var_b  = stats::var(ests),
      var_t  = NA_real_,
      lambda = NA_real_,
      riv    = NA_real_,
      df_adj = NA_real_,
      dfcom  = v_com,
      fmi    = NA_real_,
      re     = NA_real_
    ))
  }

  # --- Core Rubin's rules decomposition ---
  var_w <- mean(ses^2)                    # Within-imputation variance
  var_b <- stats::var(ests)               # Between-imputation variance
  var_t <- var_w + var_b + var_b / M      # Total variance

  # Lambda: proportion of total variance due to missingness
  lambda <- (1 + 1 / M) * var_b / var_t

  # RIV: relative increase in variance
  riv <- (1 + 1 / M) * var_b / var_w

  # --- Barnard-Rubin adjusted degrees of freedom ---
  if (is.na(v_com)) {
    # v_com is NA: cannot compute df_adj, fmi, or re
    df_adj <- NA_real_
  } else if (is.infinite(v_com) && var_b == 0) {
    # Large-sample with no between-imputation variance
    df_adj <- Inf
  } else {
    # v_old: degrees of freedom from old (Rubin 1987) formula
    if (lambda != 0) {
      v_old <- (M - 1) / lambda^2
    }

    # v_obs: observed-data degrees of freedom (Barnard-Rubin adjustment)
    if (!is.infinite(v_com)) {
      v_obs <- ((v_com + 1) / (v_com + 3)) * v_com * (1 - lambda)
    }

    # Combine v_old and v_obs
    if (lambda != 0) {
      df_adj <- if (is.infinite(v_com)) v_old else (v_old * v_obs) / (v_old + v_obs)
    } else {
      df_adj <- v_obs
    }
  }

  # --- Adjusted FMI (mice convention) ---
  if (is.na(df_adj)) {
    fmi <- NA_real_
  } else {
    fmi <- (riv + 2 / (df_adj + 3)) / (1 + riv)
  }

  # --- Relative efficiency ---
  if (is.na(fmi)) {
    re <- NA_real_
  } else {
    re <- 1 / (1 + fmi / M)
  }

  list(
    var_w  = var_w,
    var_b  = var_b,
    var_t  = var_t,
    lambda = lambda,
    riv    = riv,
    df_adj = df_adj,
    dfcom  = v_com,
    fmi    = fmi,
    re     = re
  )
}
