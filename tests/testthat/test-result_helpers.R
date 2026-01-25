# =============================================================================
# Tests for result helper functions
# =============================================================================


# --- Test fixtures ---

make_test_imputed_list <- function() {
  list(
    data.frame(USUBJID = c("S1", "S2", "S3"), CHG = c(1.0, 2.0, 3.0)),
    data.frame(USUBJID = c("S1", "S2", "S3"), CHG = c(1.1, 2.1, 3.1)),
    data.frame(USUBJID = c("S1", "S2", "S3"), CHG = c(0.9, 1.9, 2.9))
  )
}

make_test_tidy_results <- function() {
  dplyr::tibble(
    parameter = c("trt_Week 24", "lsm_ref_Week 24", "lsm_alt_Week 24",
                  "trt_Week 48", "lsm_ref_Week 48", "lsm_alt_Week 48"),
    description = c("Treatment Comparison", "LSM Reference", "LSM Alternative",
                    "Treatment Comparison", "LSM Reference", "LSM Alternative"),
    visit = c("Week 24", "Week 24", "Week 24", "Week 48", "Week 48", "Week 48"),
    parameter_type = c("trt", "lsm", "lsm", "trt", "lsm", "lsm"),
    lsm_type = c(NA, "ref", "alt", NA, "ref", "alt"),
    est = c(-5.2, 10.1, 4.9, -6.8, 8.5, 1.7),
    se = c(1.2, 0.8, 0.9, 1.5, 0.9, 1.0),
    lci = c(-7.5, 8.5, 3.1, -9.7, 6.7, -0.3),
    uci = c(-2.9, 11.7, 6.7, -3.9, 10.3, 3.7),
    pval = c(0.001, NA, NA, 0.0001, NA, NA)
  )
}


# =============================================================================
# create_impid() tests
# =============================================================================

test_that("create_impid returns correct structure", {
  imp_list <- make_test_imputed_list()
  result <- create_impid(imp_list)

  expect_s3_class(result, "data.frame")
  expect_true("IMPID" %in% names(result))
  expect_equal(names(result)[1], "IMPID")
})

test_that("create_impid creates correct number of rows", {
  imp_list <- make_test_imputed_list()
  result <- create_impid(imp_list)

  # 3 imputations x 3 subjects = 9 rows
  expect_equal(nrow(result), 9)
})

test_that("create_impid creates sequential IMPIDs", {
  imp_list <- make_test_imputed_list()
  result <- create_impid(imp_list)

  expect_equal(unique(result$IMPID), c("1", "2", "3"))
})

test_that("create_impid respects id_prefix", {
  imp_list <- make_test_imputed_list()
  result <- create_impid(imp_list, id_prefix = "IMP_")

  expect_equal(unique(result$IMPID), c("IMP_1", "IMP_2", "IMP_3"))
})

test_that("create_impid errors on empty list", {
  expect_error(create_impid(list()), "cannot be empty")
})

test_that("create_impid errors on non-list input", {
  expect_error(create_impid("not a list"), "must be a list")
})

test_that("create_impid errors if elements are not data.frames", {
  bad_list <- list(
    data.frame(a = 1),
    "not a data frame"
  )
  expect_error(create_impid(bad_list), "must be data.frames")
})


# =============================================================================
# combine_results() tests
# =============================================================================

test_that("combine_results combines multiple tibbles", {
  results1 <- make_test_tidy_results()
  results2 <- make_test_tidy_results()
  results2$est <- results2$est + 1  # Modify to differentiate

  combined <- combine_results(
    "Primary" = results1,
    "Sensitivity" = results2
  )

  expect_s3_class(combined, "tbl_df")
  expect_true("analysis" %in% names(combined))
  expect_equal(nrow(combined), 12)
  expect_equal(unique(combined$analysis), c("Primary", "Sensitivity"))
})

test_that("combine_results uses custom id_col", {
  results1 <- make_test_tidy_results()
  results2 <- make_test_tidy_results()

  combined <- combine_results(
    "A" = results1,
    "B" = results2,
    id_col = "endpoint"
  )

  expect_true("endpoint" %in% names(combined))
  expect_equal(names(combined)[1], "endpoint")
})

test_that("combine_results accepts results_list parameter", {
  results_list <- list(
    "Analysis1" = make_test_tidy_results(),
    "Analysis2" = make_test_tidy_results()
  )

  combined <- combine_results(results_list = results_list)

  expect_equal(nrow(combined), 12)
  expect_true("analysis" %in% names(combined))
})

test_that("combine_results errors with no inputs", {
  expect_error(combine_results(), "No results provided")
})

test_that("combine_results errors with non-dataframe inputs", {
  expect_error(
    combine_results("A" = "not a dataframe"),
    "must be data.frames"
  )
})


# =============================================================================
# format_results() tests
# =============================================================================

test_that("format_results creates formatted columns", {
  results <- make_test_tidy_results()
  formatted <- format_results(results)

  expect_true("estimate" %in% names(formatted))
  expect_true("ci" %in% names(formatted))
  expect_true("p_value" %in% names(formatted))
})

test_that("format_results respects digits parameter", {
  results <- make_test_tidy_results()

  formatted_2 <- format_results(results, digits = 2)
  formatted_3 <- format_results(results, digits = 3)

  # Check format differs
  expect_true(all(nchar(formatted_2$estimate) <= nchar(formatted_3$estimate) + 1))
})

test_that("format_results supports different ci_format options", {
  results <- make_test_tidy_results()

  parens <- format_results(results, ci_format = "parens")
  brackets <- format_results(results, ci_format = "brackets")
  dash <- format_results(results, ci_format = "dash")

  expect_true(all(grepl("^\\(", parens$ci)))
  expect_true(all(grepl("^\\[", brackets$ci)))
  expect_true(all(grepl(" - ", dash$ci)))
})

test_that("format_results includes SE when requested", {
  results <- make_test_tidy_results()

  without_se <- format_results(results, include_se = FALSE)
  with_se <- format_results(results, include_se = TRUE)

  expect_false("std_error" %in% names(without_se))
  expect_true("std_error" %in% names(with_se))
})

test_that("format_results errors on missing required columns", {
  bad_results <- data.frame(a = 1, b = 2)
  expect_error(format_results(bad_results), "Missing required columns")
})


# =============================================================================
# extract_trt_effects() tests
# =============================================================================

test_that("extract_trt_effects returns only treatment rows", {
  results <- make_test_tidy_results()
  trt_only <- extract_trt_effects(results)

  expect_true(all(trt_only$parameter_type == "trt"))
  expect_equal(nrow(trt_only), 2)
})

test_that("extract_trt_effects filters by visit", {
  results <- make_test_tidy_results()
  trt_w24 <- extract_trt_effects(results, visit = "Week 24")

  expect_equal(nrow(trt_w24), 1)
  expect_equal(trt_w24$visit, "Week 24")
})

test_that("extract_trt_effects errors without parameter_type", {
  bad_results <- data.frame(est = 1, se = 0.1)
  expect_error(extract_trt_effects(bad_results), "parameter_type")
})


# =============================================================================
# extract_lsm() tests
# =============================================================================

test_that("extract_lsm returns only LSM rows", {
  results <- make_test_tidy_results()
  lsm_only <- extract_lsm(results)

  expect_true(all(lsm_only$parameter_type == "lsm"))
  expect_equal(nrow(lsm_only), 4)
})

test_that("extract_lsm filters by visit", {
  results <- make_test_tidy_results()
  lsm_w48 <- extract_lsm(results, visit = "Week 48")

  expect_equal(nrow(lsm_w48), 2)
  expect_true(all(lsm_w48$visit == "Week 48"))
})

test_that("extract_lsm filters by arm", {
  results <- make_test_tidy_results()

  ref_only <- extract_lsm(results, arm = "ref")
  alt_only <- extract_lsm(results, arm = "alt")

  expect_true(all(ref_only$lsm_type == "ref"))
  expect_true(all(alt_only$lsm_type == "alt"))
  expect_equal(nrow(ref_only), 2)
  expect_equal(nrow(alt_only), 2)
})

test_that("extract_lsm errors with invalid arm", {
  results <- make_test_tidy_results()
  expect_error(extract_lsm(results, arm = "invalid"), "must be 'ref' or 'alt'")
})
