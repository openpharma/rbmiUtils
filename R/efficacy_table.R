#' Create Regulatory-Style Efficacy Summary Table
#'
#' Takes an rbmi pool object and produces a publication-ready gt table in the
#' style of CDISC/ICH Table 14.2.x. The table displays least squares means by
#' treatment arm, treatment differences, confidence intervals, and p-values,
#' organized by visit row groups.
#'
#' @param pool_obj A pooled analysis object of class `"pool"`, typically
#'   obtained from [rbmi::pool()] after calling [analyse_mi_data()].
#' @param title Optional character string for the table title.
#' @param subtitle Optional character string for the table subtitle.
#' @param digits Integer. Number of decimal places for estimates and standard
#'   errors. Default is 2.
#' @param ci_level Numeric. Confidence level for CI column labeling. If `NULL`
#'   (the default), extracted from `pool_obj$conf.level`. Falls back to 0.95 if
#'   neither is available.
#' @param arm_labels Named character vector with elements `"ref"` and `"alt"`
#'   providing custom labels for the reference and treatment arms. If `NULL`
#'   (the default), uses `"Reference"` and `"Treatment"`.
#' @param pval_digits Integer. Number of decimal places for p-values. Default
#'   is 3.
#' @param pval_threshold Numeric. P-values below this threshold are displayed
#'   as "< threshold". Default is 0.001.
#' @param font_family Optional character string specifying the font family for
#'   the table. When `NULL` (default), uses gt's default font. Applied via
#'   [gt::opt_table_font()].
#' @param font_size Optional numeric value specifying the table font size in
#'   pixels. When `NULL` (default), uses gt's default size. Applied via
#'   [gt::tab_options()].
#' @param row_padding Optional numeric value specifying the vertical padding
#'   for data rows in pixels. When `NULL` (default), uses gt's default padding.
#'   Smaller values (e.g., 2-3) create compact regulatory-style tables.
#' @param ... Additional arguments passed to [gt::gt()].
#'
#' @return A gt table object of class `gt_tbl`.
#'
#' @details
#' This function assumes a single-parameter-per-visit pool object (the standard
#' output from an rbmi ANCOVA or MMRM pipeline). It internally calls
#' [tidy_pool_obj()] to parse the pool object, then constructs the gt table.
#'
#' **Arm labels:** Use the `arm_labels` parameter to customize arm names in the
#' table. For example, `arm_labels = c(ref = "Placebo", alt = "Drug A")` will
#' display "LS Mean (Placebo)" and "LS Mean (Drug A)" instead of the defaults.
#'
#' **Customization:** The returned gt object can be further customized using
#' standard gt piping, e.g., `efficacy_table(pool_obj) |> gt::tab_options(...)`.
#'
#' **Example output:**
#'
#' \if{html}{\figure{efficacy_table-example.png}{options: width=80\%}}
#'
#' @seealso
#' * [tidy_pool_obj()] for the underlying data transformation
#' * [format_pvalue()] for p-value formatting rules
#' * [rbmi::pool()] to create pool objects
#'
#' @examples
#' \donttest{
#' if (requireNamespace("gt", quietly = TRUE)) {
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
#'   # Basic table
#'   tbl <- efficacy_table(pool_obj)
#'
#'   # Publication-styled table
#'   efficacy_table(
#'     pool_obj,
#'     title = "Table 14.2.1: ANCOVA of Change from Baseline",
#'     subtitle = "Mixed Model for Repeated Measures",
#'     arm_labels = c(ref = "Placebo", alt = "Drug A"),
#'     font_size = 12,
#'     row_padding = 4
#'   )
#' }
#' }
#'
#' @export
efficacy_table <- function(
    pool_obj,
    title = NULL,
    subtitle = NULL,
    digits = 2,
    ci_level = NULL,
    arm_labels = NULL,
    pval_digits = 3,
    pval_threshold = 0.001,
    font_family = NULL,
    font_size = NULL,
    row_padding = NULL,
    ...
) {

  # --- Dependency check ---
  if (!is_gt_available()) {
    cli::cli_abort(
      c(
        "Package {.pkg gt} is required for efficacy tables.",
        "i" = "Install with {.code install.packages(\"gt\")}."
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

  # --- Step A: Metadata extraction ---
  if (is.null(ci_level)) {
    ci_level <- pool_obj$conf.level %||% 0.95
  }

  method <- pool_obj$method %||% "unknown"
  n_imputations <- pool_obj$N

  # --- Step B: Data preparation ---

  tidy_df <- tidy_pool_obj(pool_obj)

  # --- Edge case: unexpected parameter types ---
  unexpected_types <- setdiff(unique(tidy_df$parameter_type), c("trt", "lsm"))
  if (length(unexpected_types) > 0) {
    cli::cli_warn(
      "Unexpected parameter types detected: {.val {unexpected_types}}. {.fn efficacy_table} is designed for standard ANCOVA pool objects.",
      class = "rbmiUtils_warning"
    )
  }

  # --- Edge case: NA visit rows ---
  na_visit_rows <- is.na(tidy_df$visit)
  if (any(na_visit_rows)) {
    cli::cli_warn(
      "{sum(na_visit_rows)} row{?s} with missing visit information excluded from table.",
      class = "rbmiUtils_warning"
    )
    tidy_df <- tidy_df[!na_visit_rows, ]
  }

  # --- Edge case: empty result after filtering ---
  if (nrow(tidy_df) == 0) {
    cli::cli_abort(
      "No rows remain after filtering. The pool object may not contain standard visit-level parameters.",
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }

  # Clean visit labels: underscore -> space, letter-digit boundary -> space, title case
  visit_clean <- gsub("_", " ", tidy_df$visit)
  visit_clean <- gsub("([a-zA-Z])(\\d)", "\\1 \\2", visit_clean)
  tidy_df$visit_label <- visit_clean

  # Preserve visit ordering from pool object (first-appearance order, not alphabetical)
  visit_levels <- unique(tidy_df$visit_label)
  tidy_df$visit_label <- factor(tidy_df$visit_label, levels = visit_levels)

  # Arm labels
  ref_label <- if (!is.null(arm_labels) && "ref" %in% names(arm_labels)) {
    arm_labels[["ref"]]
  } else {
    "Reference"
  }
  alt_label <- if (!is.null(arm_labels) && "alt" %in% names(arm_labels)) {
    arm_labels[["alt"]]
  } else {
    "Treatment"
  }

  # Row labels and ordering
  tidy_df$row_label <- dplyr::case_when(
    tidy_df$parameter_type == "lsm" & tidy_df$lsm_type == "ref" ~
      paste0("LS Mean (", ref_label, ")"),
    tidy_df$parameter_type == "lsm" & tidy_df$lsm_type == "alt" ~
      paste0("LS Mean (", alt_label, ")"),
    tidy_df$parameter_type == "lsm" & !is.na(tidy_df$lsm_type) ~
      paste0("LS Mean (", tidy_df$lsm_type, ")"),
    tidy_df$parameter_type == "trt" ~ "Treatment Difference",
    TRUE ~ tidy_df$parameter
  )

  # Determine reference arm name for g-comp row ordering
  # (first unique lsm_type that isn't "ref"/"alt" is treated as reference)
  lsm_types <- unique(tidy_df$lsm_type[tidy_df$parameter_type == "lsm" &
                                          !is.na(tidy_df$lsm_type)])
  gcomp_types <- setdiff(lsm_types, c("ref", "alt"))
  gcomp_ref <- if (length(gcomp_types) > 0) gcomp_types[1] else NA_character_

  tidy_df$row_order <- dplyr::case_when(
    tidy_df$parameter_type == "lsm" & tidy_df$lsm_type == "ref" ~ 1L,
    tidy_df$parameter_type == "lsm" & tidy_df$lsm_type == "alt" ~ 2L,
    tidy_df$parameter_type == "lsm" & !is.na(tidy_df$lsm_type) &
      tidy_df$lsm_type == gcomp_ref ~ 1L,
    tidy_df$parameter_type == "lsm" & !is.na(tidy_df$lsm_type) ~ 2L,
    tidy_df$parameter_type == "trt" ~ 3L,
    TRUE ~ 4L
  )

  # CI text column
  tidy_df$ci_text <- sprintf(
    "(%s, %s)",
    formatC(tidy_df$lci, format = "f", digits = digits),
    formatC(tidy_df$uci, format = "f", digits = digits)
  )

  # P-value text column: em dash for LSM rows, formatted for treatment rows
  tidy_df$pval_text <- ifelse(
    is.na(tidy_df$pval),
    "\u2014",
    format_pvalue(tidy_df$pval, digits = pval_digits, threshold = pval_threshold)
  )

  # Sort and select
  tidy_df <- tidy_df[order(tidy_df$visit_label, tidy_df$row_order), ]

  table_df <- tidy_df[, c("visit_label", "row_label", "est", "se",
                           "ci_text", "pval_text")]

  # --- Step C: gt table construction ---
  ci_label <- paste0(ci_level * 100, "% CI")

  tbl <- gt::gt(table_df, rowname_col = "row_label",
                groupname_col = "visit_label", ...)

  # Enforce visit group display order (prevents alphabetical sorting)
  tbl <- gt::row_group_order(tbl, groups = as.character(visit_levels))

  tbl <- gt::fmt_number(tbl, columns = c("est", "se"), decimals = digits)

  tbl <- gt::cols_label(
    tbl,
    est = "Estimate",
    se = "Std. Error",
    ci_text = ci_label,
    pval_text = "P-value"
  )

  tbl <- gt::cols_align(tbl, align = "center", columns = dplyr::everything())

  tbl <- gt::opt_align_table_header(tbl, align = "left")

  # Title and subtitle
  if (!is.null(title) || !is.null(subtitle)) {
    tbl <- gt::tab_header(tbl, title = title, subtitle = subtitle)
  }

  # Footnotes (source notes)
  tbl <- gt::tab_source_note(tbl, paste("Pooling method:", method))
  tbl <- gt::tab_source_note(tbl, paste("Number of imputations:", n_imputations))
  tbl <- gt::tab_source_note(tbl, paste0("Confidence level: ", ci_level * 100, "%"))

  # --- Publication styling (optional) ---
  if (!is.null(font_family)) {
    tbl <- gt::opt_table_font(tbl, font = font_family)
  }
  if (!is.null(font_size)) {
    tbl <- gt::tab_options(tbl, table.font.size = gt::px(font_size))
  }
  if (!is.null(row_padding)) {
    tbl <- gt::tab_options(tbl, data_row.padding = gt::px(row_padding))
  }

  tbl
}


#' Check if gt package is available
#'
#' Internal helper extracted for testability. Wraps
#' `requireNamespace("gt", quietly = TRUE)`.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
is_gt_available <- function() {
  requireNamespace("gt", quietly = TRUE)
}
