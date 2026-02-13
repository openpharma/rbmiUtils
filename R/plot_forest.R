#' Create a Forest Plot from an rbmi Pool Object
#'
#' Takes an rbmi pool object and produces a publication-quality, three-panel
#' forest plot using ggplot2 and patchwork. The plot displays treatment effect
#' point estimates with confidence interval whiskers, an aligned table panel
#' showing formatted estimates, and a p-value panel.
#'
#' @param pool_obj A pooled analysis object of class `"pool"`, typically
#'   obtained from [rbmi::pool()] after calling [analyse_mi_data()].
#' @param display Character string specifying the display mode. `"trt"`
#'   (the default) shows treatment differences across visits. `"lsm"` shows
#'   LS mean estimates by treatment arm with color-coded points.
#' @param ref_value Numeric. The reference value for the vertical reference
#'   line. Default is `0` for `display = "trt"` and `NULL` (no line) for
#'   `display = "lsm"`. Set explicitly to override.
#' @param ci_level Numeric. Confidence level for CI labeling. If `NULL`
#'   (the default), extracted from `pool_obj$conf.level`. Falls back to 0.95 if
#'   neither is available.
#' @param arm_labels Named character vector with elements `"ref"` and `"alt"`
#'   providing custom labels for the reference and treatment arms when
#'   `display = "lsm"`. If `NULL` (the default), uses `"Reference"` and
#'   `"Treatment"`.
#' @param title Optional character string for the plot title.
#' @param text_size Numeric. Text size for the table and p-value panels.
#'   Default is 3.
#' @param point_size Numeric. Point size for the forest plot. Default is 3.
#' @param show_pvalues Logical. Whether to display the p-value panel on the
#'   right side of the plot. Default is `TRUE`. Set to `FALSE` for a cleaner
#'   two-panel layout without p-values.
#' @param font_family Optional character string specifying the font family for
#'   all text in the plot. When `NULL` (default), uses ggplot2's default font
#'   (typically sans-serif). Applied to all `geom_text` layers and the forest
#'   panel theme.
#' @param panel_widths Optional numeric vector controlling the relative widths
#'   of the plot panels. When `show_pvalues = TRUE`, must be length 3 (table,
#'   forest, p-value panels). When `show_pvalues = FALSE`, must be length 2
#'   (table, forest panels). When `NULL` (default), uses `c(3, 4, 1.5)` for
#'   3-panel and `c(3, 5)` for 2-panel layouts.
#'
#' @return A patchwork/ggplot object that can be further customized using
#'   `& theme()` to modify all panels simultaneously.
#'
#' @details
#' The function calls [tidy_pool_obj()] internally to parse the pool object,
#' then constructs a three-panel composition:
#' \itemize{
#'   \item **Left panel:** Visit labels and formatted estimate with CI text
#'   \item **Middle panel:** Forest plot with point estimates and CI whiskers
#'   \item **Right panel:** Formatted p-values
#' }
#'
#' **Display modes:**
#' \itemize{
#'   \item `"trt"` -- Treatment differences with a reference line at zero
#'     (or custom `ref_value`). Significant results (CI excludes reference)
#'     are shown as filled circles; non-significant as open circles.
#'   \item `"lsm"` -- LS mean estimates by treatment arm, color-coded using
#'     the Okabe-Ito colorblind-friendly palette (blue for reference,
#'     vermilion for treatment). Points are dodged vertically within
#'     each visit.
#' }
#'
#' **Customization:** The returned patchwork object supports `& theme()` for
#' applying theme changes to all panels. For example:
#' `plot_forest(pool_obj) & theme(text = element_text(size = 14))`.
#'
#' **Example output (treatment difference mode):**
#'
#' \if{html}{\figure{plot_forest-trt.png}{options: width=80\%}}
#'
#' @seealso
#' * [rbmi::pool()] for creating pool objects
#' * [tidy_pool_obj()] for the underlying data transformation
#' * [efficacy_table()] for tabular presentation of the same data
#' * [format_pvalue()] for p-value formatting rules
#' * [format_estimate()] for estimate with CI formatting
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("patchwork", quietly = TRUE)) {
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
#'   # Treatment difference forest plot
#'   plot_forest(pool_obj, arm_labels = c(ref = "Placebo", alt = "Drug A"))
#'
#'   # LSM display with custom panel widths
#'   plot_forest(
#'     pool_obj,
#'     display = "lsm",
#'     arm_labels = c(ref = "Placebo", alt = "Drug A"),
#'     title = "LS Mean Estimates by Visit",
#'     panel_widths = c(3, 5, 1.5)
#'   )
#' }
#' }
#'
#' @export
plot_forest <- function(
    pool_obj,
    display = c("trt", "lsm"),
    ref_value = NULL,
    ci_level = NULL,
    arm_labels = NULL,
    title = NULL,
    text_size = 3,
    point_size = 3,
    show_pvalues = TRUE,
    font_family = NULL,
    panel_widths = NULL
) {

  # --- Dependency checks ---
  if (!is_ggplot2_available()) {
    cli::cli_abort(
      c(
        "Package {.pkg ggplot2} is required for forest plots.",
        "i" = "Install with {.code install.packages(\"ggplot2\")}."
      ),
      class = c("rbmiUtils_error_dependency", "rbmiUtils_error")
    )
  }

  if (!is_patchwork_available()) {
    cli::cli_abort(
      c(
        "Package {.pkg patchwork} is required for forest plots.",
        "i" = "Install with {.code install.packages(\"patchwork\")}."
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

  display <- match.arg(display)

  # Resolve font family for geom_text (empty string = ggplot2 default sans)
  geom_family <- font_family %||% ""

  # --- Metadata extraction ---
  if (is.null(ci_level)) {
    ci_level <- pool_obj$conf.level %||% 0.95
  }

  if (is.null(ref_value)) {
    ref_value <- if (display == "trt") 0 else NULL
  }

  # --- Data preparation ---
  tidy_df <- tidy_pool_obj(pool_obj)

  # Filter by display mode
  if (display == "trt") {
    plot_data <- tidy_df[tidy_df$parameter_type == "trt", ]
  } else {
    plot_data <- tidy_df[tidy_df$parameter_type == "lsm", ]
  }

  # Remove NA visit rows
  plot_data <- plot_data[!is.na(plot_data$visit), ]

  # Clean visit labels (same pattern as efficacy_table)
  visit_clean <- gsub("_", " ", plot_data$visit)
  visit_clean <- gsub("([a-zA-Z])(\\d)", "\\1 \\2", visit_clean)
  plot_data$visit_label <- tools::toTitleCase(visit_clean)

  # Preserve first-appearance order (not alphabetical)
  visit_levels <- unique(plot_data$visit_label)
  plot_data$visit_label <- factor(plot_data$visit_label, levels = visit_levels)

  # --- Significance detection ---
  if (!is.null(ref_value)) {
    plot_data$significant <- (plot_data$lci > ref_value) |
      (plot_data$uci < ref_value)
  } else {
    plot_data$significant <- FALSE
  }

  # --- Format text columns ---
  plot_data$est_ci_label <- format_estimate(
    plot_data$est, plot_data$lci, plot_data$uci
  )

  plot_data$pval_label <- ifelse(
    is.na(plot_data$pval),
    "\u2014",
    format_pvalue(plot_data$pval)
  )

  # --- Panel width validation ---
  if (is.null(panel_widths)) {
    panel_widths <- if (show_pvalues) c(3, 4, 1.5) else c(3, 5)
  }
  expected_n <- if (show_pvalues) 3L else 2L
  if (!is.numeric(panel_widths) || length(panel_widths) != expected_n) {
    cli::cli_abort(
      c("{.arg panel_widths} must be a numeric vector of length {expected_n}.",
        "i" = "Got length {length(panel_widths)}.",
        "i" = if (show_pvalues) {
          "Three widths needed: table panel, forest panel, p-value panel."
        } else {
          "Two widths needed: table panel, forest panel."
        }),
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }

  # --- LSM arm labels ---
  if (display == "lsm") {
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

    # Map lsm_type to arm label
    plot_data$arm <- ifelse(
      plot_data$lsm_type == "ref", ref_label,
      ifelse(plot_data$lsm_type == "alt", alt_label, plot_data$lsm_type)
    )

    # Factor to control legend order
    arm_levels <- c(ref_label, alt_label)
    plot_data$arm <- factor(plot_data$arm, levels = arm_levels)
  }

  # --- CI level label for header ---
  ci_label <- paste0(ci_level * 100, "% CI")

  # --- Build left panel (table) ---
  if (display == "trt") {
    p_left <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(y = .data$visit_label)
    ) +
      ggplot2::geom_text(
        ggplot2::aes(x = 0, label = .data$visit_label),
        hjust = 0, size = text_size, fontface = "bold",
        family = geom_family
      ) +
      ggplot2::geom_text(
        ggplot2::aes(x = 1, label = .data$est_ci_label),
        hjust = 0, size = text_size,
        family = geom_family
      ) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.3))) +
      ggplot2::scale_y_discrete(limits = rev) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)) +
      ggplot2::labs(subtitle = paste0("Visit / Estimate (", ci_label, ")"))
  } else {
    # For LSM mode, create a combined label per row
    plot_data$row_label <- paste0(
      plot_data$visit_label, " - ", plot_data$arm
    )
    # Preserve order: within each visit, ref before alt
    row_levels <- character(0)
    for (v in visit_levels) {
      v_rows <- plot_data[plot_data$visit_label == v, ]
      row_levels <- c(row_levels, unique(v_rows$row_label))
    }
    plot_data$row_label <- factor(plot_data$row_label, levels = row_levels)

    p_left <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(y = .data$row_label)
    ) +
      ggplot2::geom_text(
        ggplot2::aes(x = 0, label = .data$row_label),
        hjust = 0, size = text_size,
        family = geom_family
      ) +
      ggplot2::geom_text(
        ggplot2::aes(x = 1, label = .data$est_ci_label),
        hjust = 0, size = text_size,
        family = geom_family
      ) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.3))) +
      ggplot2::scale_y_discrete(limits = rev) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)) +
      ggplot2::labs(subtitle = paste0("Arm / Estimate (", ci_label, ")"))
  }

  # --- Build middle panel (forest plot) ---
  if (display == "trt") {
    p_mid <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data$est,
        y = .data$visit_label,
        xmin = .data$lci,
        xmax = .data$uci
      )
    ) +
      ggplot2::geom_linerange(linewidth = 0.6) +
      ggplot2::geom_point(
        ggplot2::aes(shape = .data$significant),
        size = point_size
      ) +
      ggplot2::scale_shape_manual(
        values = c("TRUE" = 16, "FALSE" = 1),
        guide = "none"
      ) +
      ggplot2::scale_y_discrete(limits = rev)

    # Add reference line
    if (!is.null(ref_value)) {
      p_mid <- p_mid +
        ggplot2::geom_vline(xintercept = ref_value, linewidth = 0.5)
    }

    p_mid <- p_mid +
      ggplot2::labs(x = "Treatment Difference") +
      theme_forest(base_family = geom_family)

  } else {
    # LSM mode with position_dodge
    okabe_ito <- grDevices::palette.colors(palette = "Okabe-Ito")
    forest_colors <- c(
      stats::setNames(unname(okabe_ito[6]), levels(plot_data$arm)[1]),
      stats::setNames(unname(okabe_ito[7]), levels(plot_data$arm)[2])
    )

    p_mid <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data$est,
        y = .data$row_label,
        xmin = .data$lci,
        xmax = .data$uci,
        colour = .data$arm
      )
    ) +
      ggplot2::geom_linerange(linewidth = 0.6) +
      ggplot2::geom_point(size = point_size, shape = 16) +
      ggplot2::scale_colour_manual(values = forest_colors, name = NULL) +
      ggplot2::scale_y_discrete(limits = rev)

    # Add reference line if provided
    if (!is.null(ref_value)) {
      p_mid <- p_mid +
        ggplot2::geom_vline(xintercept = ref_value, linewidth = 0.5)
    }

    p_mid <- p_mid +
      ggplot2::labs(x = "LS Mean Estimate") +
      theme_forest(base_family = geom_family)
  }

  # --- Build right panel (p-values) ---
  if (display == "trt") {
    p_right <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(y = .data$visit_label)
    ) +
      ggplot2::geom_text(
        ggplot2::aes(x = 0, label = .data$pval_label),
        hjust = 0.5, size = text_size,
        family = geom_family
      ) +
      ggplot2::scale_y_discrete(limits = rev) +
      ggplot2::theme_void() +
      ggplot2::labs(subtitle = "P-value")
  } else {
    p_right <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(y = .data$row_label)
    ) +
      ggplot2::geom_text(
        ggplot2::aes(x = 0, label = .data$pval_label),
        hjust = 0.5, size = text_size,
        family = geom_family
      ) +
      ggplot2::scale_y_discrete(limits = rev) +
      ggplot2::theme_void() +
      ggplot2::labs(subtitle = "P-value")
  }

  # --- Combine with patchwork ---
  if (show_pvalues) {
    combined <- p_left + p_mid + p_right +
      patchwork::plot_layout(widths = panel_widths)
  } else {
    combined <- p_left + p_mid +
      patchwork::plot_layout(widths = panel_widths)
  }

  if (!is.null(title)) {
    combined <- combined +
      patchwork::plot_annotation(title = title)
  }

  combined
}


#' Check if ggplot2 package is available
#'
#' Internal helper extracted for testability. Wraps
#' `requireNamespace("ggplot2", quietly = TRUE)`.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
is_ggplot2_available <- function() {
  requireNamespace("ggplot2", quietly = TRUE)
}


#' Check if patchwork package is available
#'
#' Internal helper extracted for testability. Wraps
#' `requireNamespace("patchwork", quietly = TRUE)`.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
is_patchwork_available <- function() {
  requireNamespace("patchwork", quietly = TRUE)
}


#' Clinical Forest Plot Theme
#'
#' Internal ggplot2 theme for the forest plot middle panel. White background,
#' minimal gridlines, no y-axis text or title.
#'
#' @param base_size Numeric. Base font size. Default is 11.
#' @param base_family Character. Base font family. Default is `""` (ggplot2
#'   default sans-serif).
#'
#' @return A ggplot2 theme object.
#' @keywords internal
#' @noRd
theme_forest <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}
