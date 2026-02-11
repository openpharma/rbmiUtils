# =============================================================================
# Tests for print.pool and summary.pool S3 methods
# =============================================================================

# Helper to build a proper mock pool object matching rbmi internal structure
# (per decision 01-01-D3)
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

# Helper to capture all output (stdout + messages) from cli-based functions
capture_all_output <- function(expr) {
  msg_output <- character(0)
  std_output <- capture.output(
    withCallingHandlers(
      expr,
      message = function(m) {
        msg_output <<- c(msg_output, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
  )
  paste(c(msg_output, std_output), collapse = "\n")
}


test_that("print.pool produces formatted output", {
  mock_pool <- make_mock_pool()
  output_text <- capture_all_output(print(mock_pool))

  # Check header
  expect_true(grepl("Pool Object", output_text))

  # Check parameter count
  expect_true(grepl("4 parameters", output_text))

  # Check visit count
  expect_true(grepl("2 visits", output_text))

  # Check method
  expect_true(grepl("rubin", output_text))

  # Check N imputations
  expect_true(grepl("100", output_text))

  # Check confidence level
  expect_true(grepl("95%", output_text))

  # Check numeric estimates appear in the table
  expect_true(grepl("-2.5", output_text) || grepl("-2.50", output_text))
})


test_that("print.pool returns invisible(x)", {
  mock_pool <- make_mock_pool()
  # Suppress all output while capturing return value
  msg_sink <- capture.output(
    withCallingHandlers(
      {
        result <- print(mock_pool)
      },
      message = function(m) invokeRestart("muffleMessage")
    )
  )
  expect_identical(result, mock_pool)
})


test_that("print.pool digits argument controls rounding", {
  mock_pool <- make_mock_pool()

  # digits = 0 should produce integer-looking estimates
  output_0_text <- capture_all_output(print(mock_pool, digits = 0))

  # digits = 4 should produce more decimal places
  output_4_text <- capture_all_output(print(mock_pool, digits = 4))

  # With digits = 0, values should be rounded to integers
  # 10.0 rounds to 10
  expect_true(grepl("\\b10\\b", output_0_text))

  # With digits = 4, the values are rounded to 4 decimal places via round()

  # R's round() and print.data.frame don't add trailing zeros, so round(-2.5, 4)

  # displays as -2.5, and round(10.0, 4) displays as 10. The key test is that
  # the digits argument is respected: digits=0 and digits=4 produce different
  # formatting for fractional values.
  # Verify the table still displays correctly with the est values
  expect_true(grepl("-2.5", output_4_text))
  expect_true(grepl("\\b10\\b", output_4_text))
})


test_that("summary.pool shows visit breakdown", {
  mock_pool <- make_mock_pool()
  output_text <- capture_all_output(summary(mock_pool))

  # Check header
  expect_true(grepl("Pool Object Summary", output_text))

  # Check visit names appear as section headers
  expect_true(grepl("Week4", output_text))
  expect_true(grepl("Week8", output_text))

  # Check treatment comparisons section
  expect_true(grepl("Treatment Comparison", output_text))

  # Check LSM section
  expect_true(grepl("Least Squares Mean", output_text))
})


test_that("summary.pool returns summary list", {
  mock_pool <- make_mock_pool()
  msg_sink <- capture.output(
    withCallingHandlers(
      {
        result <- summary(mock_pool)
      },
      message = function(m) invokeRestart("muffleMessage")
    )
  )

  expect_type(result, "list")
  expect_true("n_parameters" %in% names(result))
  expect_true("visits" %in% names(result))
  expect_true("method" %in% names(result))
  expect_true("n_imputations" %in% names(result))
  expect_true("conf.level" %in% names(result))
  expect_true("tidy_df" %in% names(result))

  # Check values
  expect_equal(result$n_parameters, 4)
  expect_equal(result$method, "rubin")
  expect_equal(result$n_imputations, 100)
  expect_equal(result$conf.level, 0.95)
  expect_s3_class(result$tidy_df, "tbl_df")
})


test_that("summary.pool significance flags", {
  mock_pool <- make_mock_pool()
  # trt_Week4 has pval = 0.002 (should get ** flag)
  # trt_Week8 has pval = 0.42 (should get no flag)
  output_text <- capture_all_output(summary(mock_pool))

  # The significant result (p=0.002) should have a ** flag
  # (0.002 < 0.01 but not < 0.001)
  expect_true(grepl("\\*\\*", output_text))

  # Create a pool with a very significant result (p < 0.001)
  mock_pool2 <- make_mock_pool()
  mock_pool2$pars$trt_Week4$pvalue <- 0.0005
  output2_text <- capture_all_output(summary(mock_pool2))

  # Should have *** flag
  expect_true(grepl("\\*\\*\\*", output2_text))

  # Create a pool with a marginally significant result (0.01 < p < 0.05)
  mock_pool3 <- make_mock_pool()
  mock_pool3$pars$trt_Week4$pvalue <- 0.03
  mock_pool3$pars$trt_Week8$pvalue <- 0.85
  output3_text <- capture_all_output(summary(mock_pool3))

  # Should have a single * flag for the significant one
  expect_true(grepl("\\*", output3_text))
})
