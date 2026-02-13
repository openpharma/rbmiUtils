# =============================================================================
# Tests for efficacy_table() function
# =============================================================================

# Helper to build a proper mock pool object matching rbmi internal structure
# (per decision 01-01-D3, copied from test-pool_methods.R for self-containment)
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


# --- Dependency guard ---

test_that("efficacy_table errors when gt is not available", {
  skip_if_not_installed("gt")

  mock_pool <- make_mock_pool()

  local_mocked_bindings(is_gt_available = function() FALSE)

  expect_error(
    efficacy_table(mock_pool),
    class = "rbmiUtils_error_dependency"
  )
})


# --- Input validation ---

test_that("efficacy_table errors on non-pool input", {
  skip_if_not_installed("gt")

  expect_error(
    efficacy_table("not_a_pool"),
    class = "rbmiUtils_error_validation"
  )

  expect_error(
    efficacy_table(42),
    class = "rbmiUtils_error_validation"
  )
})


# --- Returns gt_tbl object ---

test_that("efficacy_table returns gt_tbl object", {
  skip_if_not_installed("gt")

  mock_pool <- make_mock_pool()
  tbl <- efficacy_table(mock_pool)
  expect_s3_class(tbl, "gt_tbl")
})


# --- Visit labels ---

test_that("table contains cleaned visit labels", {
  skip_if_not_installed("gt")

  mock_pool <- make_mock_pool()
  tbl <- efficacy_table(mock_pool)
  html <- gt::as_raw_html(tbl)

  # Visit names should be cleaned: "Week4" -> "Week 4"
  expect_true(grepl("Week 4", html, fixed = TRUE))
  expect_true(grepl("Week 8", html, fixed = TRUE))
})


# --- Row labels ---

test_that("table contains correct row labels", {
  skip_if_not_installed("gt")

  html <- gt::as_raw_html(efficacy_table(make_mock_pool()))

  expect_true(grepl("LS Mean", html))
  expect_true(grepl("Treatment Difference", html))
})


# --- Custom arm labels ---

test_that("custom arm labels appear in table", {
  skip_if_not_installed("gt")

  tbl <- efficacy_table(
    make_mock_pool(),
    arm_labels = c(ref = "Placebo", alt = "Drug A")
  )
  html <- gt::as_raw_html(tbl)

  expect_true(grepl("Placebo", html))
  expect_true(grepl("Drug A", html))
})


# --- Default arm labels ---

test_that("default arm labels when not provided", {
  skip_if_not_installed("gt")

  html <- gt::as_raw_html(efficacy_table(make_mock_pool()))

  expect_true(grepl("Reference", html))
  expect_true(grepl("Treatment", html))
})


# --- Footnotes ---

test_that("footnotes are present", {
  skip_if_not_installed("gt")

  html <- gt::as_raw_html(efficacy_table(make_mock_pool()))

  # Pooling method
  expect_true(grepl("rubin", html, ignore.case = TRUE))
  # Number of imputations
  expect_true(grepl("100", html))
  # Confidence level
  expect_true(grepl("95%", html))
})


# --- Title and subtitle ---

test_that("title and subtitle appear", {
  skip_if_not_installed("gt")

  tbl <- efficacy_table(
    make_mock_pool(),
    title = "Table 14.2.1",
    subtitle = "ANCOVA Model"
  )
  html <- gt::as_raw_html(tbl)

  expect_true(grepl("Table 14.2.1", html))
  expect_true(grepl("ANCOVA Model", html))
})


# --- P-value formatting ---

test_that("p-value formatting: em dash for LSM, formatted for trt", {
  skip_if_not_installed("gt")

  html <- gt::as_raw_html(efficacy_table(make_mock_pool()))

  # Em dash present (for LSM rows with NA p-values)
  # Could be encoded as UTF-8, HTML entity, or raw character
  has_em_dash <- grepl("\u2014", html) ||
    grepl("&mdash;", html) ||
    grepl("&#8212;", html) ||
    grepl("\xe2\x80\x94", html, useBytes = TRUE)
  expect_true(has_em_dash)

  # Formatted p-value present (0.002 from trt_Week4, 0.420 from trt_Week8)
  has_pval <- grepl("0.002", html) || grepl("0.420", html)
  expect_true(has_pval)
})


# --- Digits parameter ---

test_that("digits parameter controls formatting", {
  skip_if_not_installed("gt")

  tbl3 <- efficacy_table(make_mock_pool(), digits = 3)
  html3 <- gt::as_raw_html(tbl3)

  # With 3 decimal places, the CI should show 3 decimal formatting
  # e.g., -4.100 or 9.000
  expect_true(grepl("\\d+\\.\\d{3}", html3))
})


# --- Custom CI level ---

test_that("custom ci_level label appears in table", {
  skip_if_not_installed("gt")

  mock_pool <- make_mock_pool()
  mock_pool$conf.level <- 0.90

  html <- gt::as_raw_html(efficacy_table(mock_pool))

  # 90% CI should be the column label and footnote
  expect_true(grepl("90%", html))
})


# --- CI level from argument overrides pool object ---

test_that("ci_level argument overrides pool_obj$conf.level", {
  skip_if_not_installed("gt")

  mock_pool <- make_mock_pool()
  # pool_obj has conf.level = 0.95
  html <- gt::as_raw_html(efficacy_table(mock_pool, ci_level = 0.99))

  expect_true(grepl("99%", html))
})


# =============================================================================
# Edge case tests (Plan 03-02)
# =============================================================================

# --- Visit ordering follows pool object order, not alphabetical ---

test_that("visit ordering follows pool object order, not alphabetical", {
  skip_if_not_installed("gt")

  # Create pool with visits in non-alphabetical order: Week12, Week4, Week24
  # Alphabetical would be: Week 12, Week 24, Week 4
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

  html <- gt::as_raw_html(efficacy_table(mock_pool))

  # Find first occurrence positions -- pool order is Week12, Week4, Week24
  w12_pos <- regexpr("Week 12", html)
  w4_pos <- regexpr("Week 4[^0-9]", html)
  w24_pos <- regexpr("Week 24", html)

  # Week 12 should appear before Week 4, and Week 4 before Week 24
  expect_true(
    w12_pos < w4_pos,
    label = "Week 12 appears before Week 4 (pool object order, not alphabetical)"
  )
  expect_true(
    w4_pos < w24_pos,
    label = "Week 4 appears before Week 24 (pool object order, not alphabetical)"
  )
})


# --- Single-visit pool object works ---

test_that("single-visit pool object works", {
  skip_if_not_installed("gt")

  mock_pool <- list(
    pars = list(
      trt_Week4 = list(est = -2.5, se = 0.8, ci = c(-4.1, -0.9), pvalue = 0.002),
      lsm_ref_Week4 = list(est = 10.0, se = 0.5, ci = c(9.0, 11.0), pvalue = NA),
      lsm_alt_Week4 = list(est = 7.5, se = 0.6, ci = c(6.3, 8.7), pvalue = NA)
    ),
    conf.level = 0.95,
    alternative = "two.sided",
    N = 50,
    method = "rubin"
  )
  class(mock_pool) <- "pool"

  tbl <- efficacy_table(mock_pool)
  expect_s3_class(tbl, "gt_tbl")

  html <- gt::as_raw_html(tbl)
  expect_true(grepl("Week 4", html, fixed = TRUE))
})


# --- NA visit rows produce warning and are excluded ---

test_that("NA visit rows produce warning and are excluded", {
  skip_if_not_installed("gt")

  # Add a parameter with no visit suffix (lsm_ref -> visit = NA)
  mock_pool <- list(
    pars = list(
      trt_Week4 = list(est = -2.5, se = 0.8, ci = c(-4.1, -0.9), pvalue = 0.002),
      lsm_ref_Week4 = list(est = 10.0, se = 0.5, ci = c(9.0, 11.0), pvalue = NA),
      lsm_alt_Week4 = list(est = 7.5, se = 0.6, ci = c(6.3, 8.7), pvalue = NA),
      lsm_ref = list(est = 5.0, se = 0.3, ci = c(4.4, 5.6), pvalue = NA)
    ),
    conf.level = 0.95,
    alternative = "two.sided",
    N = 100,
    method = "rubin"
  )
  class(mock_pool) <- "pool"

  expect_warning(
    tbl <- efficacy_table(mock_pool),
    class = "rbmiUtils_warning"
  )

  # Table should still render successfully
  expect_s3_class(tbl, "gt_tbl")
  html <- gt::as_raw_html(tbl)
  expect_true(grepl("Week 4", html, fixed = TRUE))
})


# --- gt object can be further customized via pipe ---

test_that("gt object can be further customized via pipe", {
  skip_if_not_installed("gt")

  tbl <- efficacy_table(make_mock_pool())
  customized <- tbl |> gt::tab_options(table.font.size = gt::px(10))
  expect_s3_class(customized, "gt_tbl")
})


# --- Empty result after filtering aborts ---

test_that("empty result after NA visit filtering aborts with clear error", {
  skip_if_not_installed("gt")

  # All parameters have no visit -- all will be filtered out
  mock_pool <- list(
    pars = list(
      lsm_ref = list(est = 5.0, se = 0.3, ci = c(4.4, 5.6), pvalue = NA),
      lsm_alt = list(est = 3.0, se = 0.4, ci = c(2.2, 3.8), pvalue = NA)
    ),
    conf.level = 0.95,
    alternative = "two.sided",
    N = 100,
    method = "rubin"
  )
  class(mock_pool) <- "pool"

  expect_error(
    suppressWarnings(efficacy_table(mock_pool)),
    class = "rbmiUtils_error_validation"
  )
})


# =============================================================================
# Publication styling parameter tests (Plan 10-01)
# =============================================================================

# --- font_family parameter ---

test_that("font_family parameter applies font to gt table HTML", {
  skip_if_not_installed("gt")

  mock_pool <- make_mock_pool()
  tbl <- efficacy_table(mock_pool, font_family = "Courier")
  html <- gt::as_raw_html(tbl)

  expect_true(grepl("Courier", html, fixed = TRUE))
})


# --- font_size parameter ---

test_that("font_size parameter applies pixel size to gt table HTML", {
  skip_if_not_installed("gt")

  mock_pool <- make_mock_pool()
  tbl <- efficacy_table(mock_pool, font_size = 10)
  html <- gt::as_raw_html(tbl)

  expect_true(grepl("10px", html, fixed = TRUE))
})


# --- row_padding parameter ---

test_that("row_padding parameter applies pixel padding to gt table HTML", {
  skip_if_not_installed("gt")

  mock_pool <- make_mock_pool()
  tbl <- efficacy_table(mock_pool, row_padding = 2)
  html <- gt::as_raw_html(tbl)

  expect_true(grepl("2px", html, fixed = TRUE))
})


# --- NULL defaults backward compatibility ---

test_that("NULL defaults do not override default styling", {
  skip_if_not_installed("gt")

  mock_pool <- make_mock_pool()
  tbl <- efficacy_table(mock_pool)
  html <- gt::as_raw_html(tbl)

  # Courier should NOT appear when font_family is NULL (default)
  expect_false(grepl("Courier", html, fixed = TRUE))
})


# --- Combined styling parameters ---

test_that("combined styling parameters work together", {
  skip_if_not_installed("gt")

  mock_pool <- make_mock_pool()
  tbl <- efficacy_table(
    mock_pool,
    font_family = "Georgia",
    font_size = 11,
    row_padding = 3
  )

  expect_s3_class(tbl, "gt_tbl")

  # Renders without error
  html <- gt::as_raw_html(tbl)
  expect_true(nchar(html) > 0)

  # All styling values present in HTML
  expect_true(grepl("Georgia", html, fixed = TRUE))
  expect_true(grepl("11px", html, fixed = TRUE))
  expect_true(grepl("3px", html, fixed = TRUE))
})
