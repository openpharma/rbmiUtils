# Tests for formatting functions

test_that("format_pvalue handles basic cases", {
  # Standard p-values

expect_equal(format_pvalue(0.5), "0.500")
  expect_equal(format_pvalue(0.05), "0.050")
  expect_equal(format_pvalue(0.001), "0.001")

  # Below threshold
  expect_equal(format_pvalue(0.0001), "< 0.001")
  expect_equal(format_pvalue(0.0009), "< 0.001")

  # Boundary cases
  expect_equal(format_pvalue(0), "< 0.001")
  expect_equal(format_pvalue(1), "1.000")
})

test_that("format_pvalue handles vector input", {
  pvals <- c(0.5, 0.05, 0.001, 0.0001)
  result <- format_pvalue(pvals)

  expect_length(result, 4)
  expect_equal(result[1], "0.500")
  expect_equal(result[4], "< 0.001")
})

test_that("format_pvalue handles NA values", {
  expect_true(is.na(format_pvalue(NA_real_)))

  pvals <- c(0.05, NA, 0.001)
  result <- format_pvalue(pvals)
  expect_true(is.na(result[2]))
  expect_equal(result[1], "0.050")
})

test_that("format_pvalue warns on invalid p-values", {
  expect_warning(format_pvalue(-0.1), "outside")
  expect_warning(format_pvalue(1.5), "outside")

  expect_warning(result <- format_pvalue(c(0.5, -0.1, 1.5)))
  expect_equal(result[1], "0.500")
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
})

test_that("format_pvalue respects digits parameter", {
  expect_equal(format_pvalue(0.12345, digits = 2), "0.12")
  expect_equal(format_pvalue(0.12345, digits = 4), "0.1235")
})

test_that("format_pvalue respects threshold parameter", {
  expect_equal(format_pvalue(0.005, threshold = 0.01), "< 0.01")
  expect_equal(format_pvalue(0.05, threshold = 0.01), "0.050")
})

test_that("format_pvalue supports HTML output", {
  expect_equal(format_pvalue(0.0001, html = TRUE), "&lt; 0.001")
  expect_equal(format_pvalue(0.05, html = TRUE), "0.050")
})

test_that("format_pvalue validates inputs", {
  expect_error(format_pvalue("0.05"), "numeric")
  expect_error(format_pvalue(0.05, digits = -1), "positive")
  expect_error(format_pvalue(0.05, threshold = 0), "positive")
})


# Tests for format_estimate

test_that("format_estimate handles basic cases", {
  result <- format_estimate(1.5, 0.8, 2.2)
  expect_equal(result, "1.50 (0.80, 2.20)")

  # Negative values
  result <- format_estimate(-2.5, -4.0, -1.0)
  expect_equal(result, "-2.50 (-4.00, -1.00)")
})

test_that("format_estimate handles vector input", {
  result <- format_estimate(
    estimate = c(-2.5, -1.8),
    lower = c(-4.0, -3.2),
    upper = c(-1.0, -0.4)
  )

  expect_length(result, 2)
  expect_equal(result[1], "-2.50 (-4.00, -1.00)")
  expect_equal(result[2], "-1.80 (-3.20, -0.40)")
})

test_that("format_estimate handles NA values", {
  expect_true(is.na(format_estimate(NA, 0.8, 2.2)))
  expect_true(is.na(format_estimate(1.5, NA, 2.2)))
  expect_true(is.na(format_estimate(1.5, 0.8, NA)))

  # Mixed NA
  result <- format_estimate(
    estimate = c(1.5, NA),
    lower = c(0.8, 0.5),
    upper = c(2.2, 1.5)
  )
  expect_equal(result[1], "1.50 (0.80, 2.20)")
  expect_true(is.na(result[2]))
})

test_that("format_estimate respects digits parameter", {
  result <- format_estimate(1.234, 0.567, 1.901, digits = 3)
  expect_equal(result, "1.234 (0.567, 1.901)")

  result <- format_estimate(1.5, 0.8, 2.2, digits = 0)
  expect_equal(result, "2 (1, 2)")
})

test_that("format_estimate respects sep parameter", {
  result <- format_estimate(1.5, 0.8, 2.2, sep = " to ")
  expect_equal(result, "1.50 (0.80 to 2.20)")

  result <- format_estimate(1.5, 0.8, 2.2, sep = "; ")
  expect_equal(result, "1.50 (0.80; 2.20)")
})

test_that("format_estimate validates inputs", {
  expect_error(format_estimate("1.5", 0.8, 2.2), "numeric")
  expect_error(format_estimate(1.5, "0.8", 2.2), "numeric")
  expect_error(format_estimate(1.5, 0.8, "2.2"), "numeric")

  # Length mismatch
  expect_error(format_estimate(c(1, 2), c(0.5), c(1.5, 2.5)), "same length")

  # Invalid digits
  expect_error(format_estimate(1.5, 0.8, 2.2, digits = -1), "non-negative")

  # Invalid sep
  expect_error(format_estimate(1.5, 0.8, 2.2, sep = 123), "character")
})


# Tests for format_results_table

test_that("format_results_table adds formatted columns", {
  data <- data.frame(
    parameter = c("trt_Week24", "lsm_ref_Week24"),
    est = c(-2.45, 5.20),
    se = c(0.89, 0.65),
    lci = c(-4.20, 3.93),
    uci = c(-0.70, 6.47),
    pval = c(0.006, NA)
  )

  result <- format_results_table(data)

  expect_true("est_ci" %in% names(result))
  expect_true("pval_fmt" %in% names(result))
  expect_s3_class(result, "tbl_df")
})

test_that("format_results_table formats estimates correctly", {
  data <- data.frame(
    est = c(-2.45, 5.20),
    lci = c(-4.20, 3.93),
    uci = c(-0.70, 6.47),
    pval = c(0.006, NA)
  )

  result <- format_results_table(data)

  expect_equal(result$est_ci[1], "-2.45 (-4.20, -0.70)")
  expect_equal(result$est_ci[2], "5.20 (3.93, 6.47)")
})

test_that("format_results_table formats p-values correctly", {
  data <- data.frame(
    est = c(-2.45, 5.20, 1.00),
    lci = c(-4.20, 3.93, 0.50),
    uci = c(-0.70, 6.47, 1.50),
    pval = c(0.006, 0.0001, NA)
  )

  result <- format_results_table(data)

  expect_equal(result$pval_fmt[1], "0.006")
  expect_equal(result$pval_fmt[2], "< 0.001")
  expect_true(is.na(result$pval_fmt[3]))
})

test_that("format_results_table respects custom parameters", {
  data <- data.frame(
    estimate = c(-2.456),
    lower = c(-4.200),
    upper = c(-0.700),
    p = c(0.0056)
  )

  result <- format_results_table(
    data,
    est_col = "estimate",
    lci_col = "lower",
    uci_col = "upper",
    pval_col = "p",
    est_digits = 3,
    pval_digits = 4,
    pval_threshold = 0.0001
  )

  expect_equal(result$est_ci, "-2.456 (-4.200, -0.700)")
  expect_equal(result$pval_fmt, "0.0056")
})

test_that("format_results_table works without pval column", {
  data <- data.frame(
    est = c(-2.45),
    lci = c(-4.20),
    uci = c(-0.70)
  )

  result <- format_results_table(data)

  expect_true("est_ci" %in% names(result))
  expect_false("pval_fmt" %in% names(result))
})

test_that("format_results_table preserves original columns", {
  data <- data.frame(
    parameter = "trt_Week24",
    description = "Treatment Effect",
    est = -2.45,
    se = 0.89,
    lci = -4.20,
    uci = -0.70,
    pval = 0.006,
    extra_col = "keep me"
  )

  result <- format_results_table(data)

  expect_true(all(names(data) %in% names(result)))
  expect_equal(result$extra_col, "keep me")
})

test_that("format_results_table validates inputs", {
  expect_error(format_results_table("not a dataframe"), "data.frame")

  data <- data.frame(est = 1, lci = 0.5)
  expect_error(format_results_table(data), "not found.*uci")
})

test_that("format_results_table respects ci_sep parameter", {
  data <- data.frame(
    est = c(-2.45),
    lci = c(-4.20),
    uci = c(-0.70)
  )

  result <- format_results_table(data, ci_sep = " to ")
  expect_equal(result$est_ci, "-2.45 (-4.20 to -0.70)")
})
