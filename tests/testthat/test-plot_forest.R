# =============================================================================
# Tests for plot_forest() function
# =============================================================================

# Helper to build a proper mock pool object matching rbmi internal structure
# (per decision 01-01-D3, same pattern as test-efficacy_table.R)
make_mock_pool <- function() {
  pool_obj <- list(
    pars = list(
      trt_Week4 = list(est = -2.5, se = 0.8, ci = c(-4.1, -0.9), pvalue = 0.002),
      lsm_ref_Week4 = list(est = 10.0, se = 0.5, ci = c(9.0, 11.0), pvalue = NA),
      lsm_alt_Week4 = list(est = 7.5, se = 0.6, ci = c(6.3, 8.7), pvalue = NA),
      trt_Week8 = list(est = -1.0, se = 1.2, ci = c(-3.4, 1.4), pvalue = 0.42)
    ),
    conf.level = 0.95,
    alternative = "two.sided",
    N = 100,
    method = "rubin"
  )
  class(pool_obj) <- "pool"
  pool_obj
}


# --- Dependency guard: ggplot2 ---

test_that("plot_forest errors when ggplot2 is not available", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  mock_pool <- make_mock_pool()

  local_mocked_bindings(is_ggplot2_available = function() FALSE)

  expect_error(
    plot_forest(mock_pool),
    class = "rbmiUtils_error_dependency"
  )
})


# --- Dependency guard: patchwork ---

test_that("plot_forest errors when patchwork is not available", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  mock_pool <- make_mock_pool()

  local_mocked_bindings(is_patchwork_available = function() FALSE)

  expect_error(
    plot_forest(mock_pool),
    class = "rbmiUtils_error_dependency"
  )
})


# --- Input validation: non-pool input ---

test_that("plot_forest errors on non-pool input", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  expect_error(
    plot_forest("not_a_pool"),
    class = "rbmiUtils_error_validation"
  )

  expect_error(
    plot_forest(42),
    class = "rbmiUtils_error_validation"
  )
})


# --- Returns patchwork/ggplot object ---

test_that("plot_forest returns patchwork/ggplot object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool)

  expect_s3_class(p, "patchwork")
  expect_s3_class(p, "ggplot")
})


# --- Treatment difference mode (default) ---

test_that("treatment difference mode works and renders without error", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool)

  expect_s3_class(p, "patchwork")

  # Should render without error
  expect_no_error(print(p))
})


# --- LSM display mode ---

test_that("LSM display mode works", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool, display = "lsm")

  expect_s3_class(p, "patchwork")
  expect_s3_class(p, "ggplot")

  # Should render without error
  expect_no_error(print(p))
})


# --- Custom title ---

test_that("custom title is accepted without error", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool, title = "My Forest Plot")

  expect_s3_class(p, "patchwork")
  expect_no_error(print(p))
})


# --- Custom ref_value ---

test_that("custom ref_value is accepted without error", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool, ref_value = -1.5)

  expect_s3_class(p, "patchwork")
  expect_no_error(print(p))
})


# --- Custom arm_labels in LSM mode ---

test_that("custom arm_labels work in LSM mode", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  mock_pool <- make_mock_pool()
  p <- plot_forest(
    mock_pool,
    display = "lsm",
    arm_labels = c(ref = "Placebo", alt = "Drug A")
  )

  expect_s3_class(p, "patchwork")
  expect_no_error(print(p))
})


# --- Visit ordering preserved (not alphabetical) ---

test_that("visit ordering follows pool object order, not alphabetical", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  # Create pool with non-alphabetical visit order: Week12, Week4, Week24
  mock_pool <- list(
    pars = list(
      trt_Week12 = list(est = -1.5, se = 0.7, ci = c(-2.9, -0.1), pvalue = 0.035),
      lsm_ref_Week12 = list(est = 8.0, se = 0.4, ci = c(7.2, 8.8), pvalue = NA),
      lsm_alt_Week12 = list(est = 6.5, se = 0.5, ci = c(5.5, 7.5), pvalue = NA),
      trt_Week4 = list(est = -0.8, se = 0.9, ci = c(-2.6, 1.0), pvalue = 0.38),
      lsm_ref_Week4 = list(est = 5.0, se = 0.3, ci = c(4.4, 5.6), pvalue = NA),
      lsm_alt_Week4 = list(est = 4.2, se = 0.4, ci = c(3.4, 5.0), pvalue = NA),
      trt_Week24 = list(est = -2.5, se = 0.8, ci = c(-4.1, -0.9), pvalue = 0.002),
      lsm_ref_Week24 = list(est = 10.0, se = 0.5, ci = c(9.0, 11.0), pvalue = NA),
      lsm_alt_Week24 = list(est = 7.5, se = 0.6, ci = c(6.3, 8.7), pvalue = NA)
    ),
    conf.level = 0.95,
    alternative = "two.sided",
    N = 100,
    method = "rubin"
  )
  class(mock_pool) <- "pool"

  p <- plot_forest(mock_pool)

  # Extract data from the forest plot panel (the main plot in patchwork)
  # The main plot stores its data in p$data
  # Alternatively, check factor levels directly from the build
  plot_build <- ggplot2::ggplot_build(p)

  # The middle panel is accessible via patchwork internals

  # Instead, we can inspect the data used by extracting from the patches
  # The key insight: visit_label factor levels should be in pool order
  # Pool order of trt params: Week12, Week4, Week24
  # This is NOT alphabetical (which would be Week 12, Week 24, Week 4)

  # Access data from the patchwork patches
  # p$patches$plots contains the first two panels, p itself is the last
  # The left panel (first patch) has visit_label with the factor
  left_data <- p$patches$plots[[1]]$data
  visit_levels <- levels(left_data$visit_label)

  expect_equal(visit_levels, c("Week 12", "Week 4", "Week 24"))
})


# --- Significance detection ---

test_that("significance detection works correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool)

  # Extract data from the forest plot (middle panel = last panel = p itself)
  # The middle panel in patchwork: p$patches$plots[[2]] or accessed via build
  # Actually, the "main" plot of patchwork is the last one added
  # p_left + p_mid + p_right -> patches$plots = [p_left, p_mid], main = p_right
  # Let's use the middle panel which has the significance column

  # Access the middle panel data (second in patches list)
  mid_data <- p$patches$plots[[2]]$data

  # Week 4: CI (-4.1, -0.9), ref_value = 0
  # uci = -0.9 < 0, so significant = TRUE
  week4_sig <- mid_data$significant[mid_data$visit_label == "Week 4"]
  expect_true(all(week4_sig))

  # Week 8: CI (-3.4, 1.4), ref_value = 0
  # lci = -3.4 < 0 and uci = 1.4 > 0, so significant = FALSE
  week8_sig <- mid_data$significant[mid_data$visit_label == "Week 8"]
  expect_false(any(week8_sig))
})


# --- ref_value NULL for LSM mode ---

test_that("ref_value defaults to NULL for LSM mode (no reference line)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool, display = "lsm")

  # When ref_value is NULL, all significant should be FALSE
  # Access the middle panel
  mid_data <- p$patches$plots[[2]]$data
  expect_true(all(mid_data$significant == FALSE))

  # Should render without error
  expect_no_error(print(p))
})


# --- Custom text_size and point_size ---

test_that("custom text_size and point_size are accepted", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool, text_size = 4, point_size = 5)

  expect_s3_class(p, "patchwork")
  expect_no_error(print(p))
})


# --- Patchwork object supports & theme() customization ---

test_that("patchwork object can be customized with & theme()", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool)

  # Apply theme to all panels -- should not error
  customized <- p & ggplot2::theme(text = ggplot2::element_text(size = 14))
  expect_s3_class(customized, "patchwork")
  expect_no_error(print(customized))
})


# --- font_family parameter tests (STYLE-03) ---

test_that("font_family parameter propagates to left panel text layers", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool, font_family = "serif")
  # Left panel is first in patches
  left_panel <- p$patches$plots[[1]]
  b <- ggplot2::ggplot_build(left_panel)
  # Both text layers should have family = "serif"
  expect_equal(b$data[[1]]$family[1], "serif")
  expect_equal(b$data[[2]]$family[1], "serif")
})

test_that("font_family parameter propagates to right panel text layer", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool, font_family = "mono")
  # In patchwork, the "main" plot wraps the last added panel (p_right)
  right_build <- ggplot2::ggplot_build(p)
  expect_equal(right_build$data[[1]]$family[1], "mono")
})

test_that("font_family NULL default does not override ggplot2 default", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool)
  left_panel <- p$patches$plots[[1]]
  b <- ggplot2::ggplot_build(left_panel)
  # Default should be empty string (ggplot2's default sans)
  expect_equal(b$data[[1]]$family[1], "")
})


# --- panel_widths parameter tests (STYLE-04) ---

test_that("panel_widths parameter controls layout without error", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool, panel_widths = c(2, 5, 1))
  expect_s3_class(p, "patchwork")
  expect_no_error(print(p))
})

test_that("panel_widths validates length against show_pvalues", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  # 3 widths with show_pvalues = FALSE should error
  expect_error(
    plot_forest(mock_pool, panel_widths = c(3, 4, 1.5), show_pvalues = FALSE),
    class = "rbmiUtils_error_validation"
  )
  # 2 widths with show_pvalues = TRUE should error
  expect_error(
    plot_forest(mock_pool, panel_widths = c(3, 5)),
    class = "rbmiUtils_error_validation"
  )
})

test_that("panel_widths works in 2-panel mode (show_pvalues = FALSE)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool, show_pvalues = FALSE, panel_widths = c(2, 6))
  expect_s3_class(p, "patchwork")
  expect_no_error(print(p))
})


# --- Left-panel alignment test (STYLE-05) ---

test_that("left panel text is left-aligned (hjust = 0)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool)
  left_panel <- p$patches$plots[[1]]
  b <- ggplot2::ggplot_build(left_panel)
  # Both text layers should use hjust = 0 (left-aligned)
  expect_equal(b$data[[1]]$hjust[1], 0)
  expect_equal(b$data[[2]]$hjust[1], 0)
})


# --- LSM mode styling ---

test_that("LSM mode respects font_family and left-alignment", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool, display = "lsm", font_family = "serif")
  left_panel <- p$patches$plots[[1]]
  b <- ggplot2::ggplot_build(left_panel)
  expect_equal(b$data[[1]]$family[1], "serif")
  expect_equal(b$data[[1]]$hjust[1], 0)
})


# --- Visual refinement tests (VIZ-01) ---

test_that("reference line is dashed in trt mode", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool)
  # Middle panel is second in patches

  mid_panel <- p$patches$plots[[2]]
  b <- ggplot2::ggplot_build(mid_panel)
  # Find the geom_vline layer -- it has xintercept in data
  vline_data <- NULL
  for (i in seq_along(b$data)) {
    if ("xintercept" %in% names(b$data[[i]])) {
      vline_data <- b$data[[i]]
      break
    }
  }
  expect_false(is.null(vline_data), info = "geom_vline layer should exist")
  # linetype "dashed" is numeric 2 in ggplot2 build data
  expect_equal(vline_data$linetype[1], "dashed")
})

test_that("forest panel has visible border", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool)
  mid_panel <- p$patches$plots[[2]]
  theme_bg <- ggplot2::theme_get()
  panel_bg <- mid_panel$theme$panel.background
  # Should be an element_rect with a non-NA colour
  expect_true(inherits(panel_bg, "element_rect"))
  expect_false(is.na(panel_bg$colour))
})

test_that("horizontal gridlines are present in forest panel", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool)
  mid_panel <- p$patches$plots[[2]]
  grid_y <- mid_panel$theme$panel.grid.major.y
  # Should be an element_line, not element_blank

  expect_true(inherits(grid_y, "element_line"))
})

test_that("x-axis title includes CI label in trt mode", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool)
  mid_panel <- p$patches$plots[[2]]
  x_label <- mid_panel$labels$x
  expect_true(grepl("95% CI", x_label))
  expect_true(grepl("Treatment Difference", x_label))
})

test_that("x-axis title includes CI label in lsm mode", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool, display = "lsm")
  mid_panel <- p$patches$plots[[2]]
  x_label <- mid_panel$labels$x
  expect_true(grepl("95% CI", x_label))
  expect_true(grepl("LS Mean Estimate", x_label))
})

test_that("default text size is 3.5 (updated from 3)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool)
  left_panel <- p$patches$plots[[1]]
  b <- ggplot2::ggplot_build(left_panel)
  # Text size in build data should be 3.5

  expect_equal(b$data[[1]]$size[1], 3.5)
})

test_that("panel subtitles are bold", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  mock_pool <- make_mock_pool()
  p <- plot_forest(mock_pool)
  left_panel <- p$patches$plots[[1]]
  subtitle_theme <- left_panel$theme$plot.subtitle
  expect_true(inherits(subtitle_theme, "element_text"))
  expect_equal(subtitle_theme$face, "bold")
})
