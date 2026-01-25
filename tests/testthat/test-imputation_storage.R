# --- Test fixtures ---

make_original_data <- function() {
  data.frame(
    USUBJID = factor(rep(c("S1", "S2", "S3", "S4"), each = 3)),
    AVISIT = factor(
      rep(c("Week 4", "Week 8", "Week 12"), 4),
      levels = c("Week 4", "Week 8", "Week 12")
    ),
    TRT = factor(rep(c("Placebo", "Drug A"), each = 6)),
    CHG = c(
      1.1, 2.2, 3.3,           # S1: complete
      0.5, NA, NA,             # S2: missing Week 8 and 12
      1.0, 2.0, NA,            # S3: missing Week 12
      1.5, 2.5, 3.5            # S4: complete
    ),
    BASE = rep(c(10, 12, 11, 9), each = 3),
    stringsAsFactors = FALSE
  )
}

make_imputed_data <- function() {
  # Simulate 3 imputations
  # Missing values: S2 Week 8, S2 Week 12, S3 Week 12
  original <- make_original_data()

  imps <- lapply(1:3, function(imp_id) {
    dat <- original
    dat$IMPID <- as.character(imp_id)
    # Fill in missing values with different imputed values per imputation
    dat$CHG[dat$USUBJID == "S2" & dat$AVISIT == "Week 8"] <- 1.0 + imp_id * 0.1
    dat$CHG[dat$USUBJID == "S2" & dat$AVISIT == "Week 12"] <- 2.0 + imp_id * 0.1
    dat$CHG[dat$USUBJID == "S3" & dat$AVISIT == "Week 12"] <- 3.0 + imp_id * 0.1
    dat
  })

  result <- do.call(rbind, imps)
  result <- result[, c("IMPID", setdiff(names(result), "IMPID"))]
  rownames(result) <- NULL
  result
}

make_test_vars <- function() {
  rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG"
  )
}


# =============================================================================
# reduce_imputed_data() tests
# =============================================================================

test_that("reduce_imputed_data returns only originally missing rows", {
  original <- make_original_data()
  imputed <- make_imputed_data()
  vars <- make_test_vars()

  reduced <- reduce_imputed_data(imputed, original, vars)

  # Should have 3 missing values * 3 imputations = 9 rows

  expect_equal(nrow(reduced), 9)
})

test_that("reduce_imputed_data preserves IMPID column", {
  original <- make_original_data()
  imputed <- make_imputed_data()
  vars <- make_test_vars()

  reduced <- reduce_imputed_data(imputed, original, vars)

  expect_true("IMPID" %in% names(reduced))
  expect_equal(sort(unique(reduced$IMPID)), c("1", "2", "3"))
})

test_that("reduce_imputed_data preserves all columns from imputed_data", {
  original <- make_original_data()
  imputed <- make_imputed_data()
  vars <- make_test_vars()

  reduced <- reduce_imputed_data(imputed, original, vars)

  expect_equal(names(reduced), names(imputed))
})

test_that("reduce_imputed_data returns correct subject-visit combinations", {
  original <- make_original_data()
  imputed <- make_imputed_data()
  vars <- make_test_vars()

  reduced <- reduce_imputed_data(imputed, original, vars)

  # Check that we have the right combinations
  combos <- unique(paste(reduced$USUBJID, reduced$AVISIT, sep = "-"))
  expect_setequal(combos, c("S2-Week 8", "S2-Week 12", "S3-Week 12"))
})

test_that("reduce_imputed_data returns empty data.frame if no missing values", {
  original <- make_original_data()
  original$CHG[is.na(original$CHG)] <- 999  # Fill all missing

  imputed <- make_imputed_data()
  vars <- make_test_vars()

  reduced <- reduce_imputed_data(imputed, original, vars)

  expect_equal(nrow(reduced), 0)
  expect_equal(names(reduced), names(imputed))
})

test_that("reduce_imputed_data errors on missing IMPID column", {
  original <- make_original_data()
  imputed <- make_imputed_data()
  imputed$IMPID <- NULL
  vars <- make_test_vars()

  expect_error(
    reduce_imputed_data(imputed, original, vars),
    "IMPID"
  )
})

test_that("reduce_imputed_data errors on missing required columns in original", {
  original <- make_original_data()
  original$CHG <- NULL
  imputed <- make_imputed_data()
  vars <- make_test_vars()

  expect_error(
    reduce_imputed_data(imputed, original, vars),
    "original_data.*CHG"
  )
})

test_that("reduce_imputed_data errors on missing required columns in imputed", {
  original <- make_original_data()
  imputed <- make_imputed_data()
  imputed$CHG <- NULL
  vars <- make_test_vars()

  expect_error(
    reduce_imputed_data(imputed, original, vars),
    "imputed_data.*CHG"
  )
})

test_that("reduce_imputed_data errors on non-dataframe inputs", {
  vars <- make_test_vars()

  expect_error(
    reduce_imputed_data("not a df", make_original_data(), vars),
    "must be a data.frame"
  )
  expect_error(
    reduce_imputed_data(make_imputed_data(), "not a df", vars),
    "must be a data.frame"
  )
})


# =============================================================================
# expand_imputed_data() tests
# =============================================================================

test_that("expand_imputed_data round-trip matches original imputed data",
{
  original <- make_original_data()
  imputed <- make_imputed_data()
  vars <- make_test_vars()

  reduced <- reduce_imputed_data(imputed, original, vars)
  expanded <- expand_imputed_data(reduced, original, vars)

  # Same number of rows

  expect_equal(nrow(expanded), nrow(imputed))

  # Same columns (though order may differ)
  expect_setequal(names(expanded), names(imputed))

  # Sort both and compare values
  imputed_sorted <- imputed[order(imputed$IMPID, imputed$USUBJID, imputed$AVISIT), ]
 expanded_sorted <- expanded[order(expanded$IMPID, expanded$USUBJID, expanded$AVISIT), ]

  # Compare key columns
  expect_equal(expanded_sorted$IMPID, imputed_sorted$IMPID)
  expect_equal(as.character(expanded_sorted$USUBJID), as.character(imputed_sorted$USUBJID))
  expect_equal(as.character(expanded_sorted$AVISIT), as.character(imputed_sorted$AVISIT))
  expect_equal(expanded_sorted$CHG, imputed_sorted$CHG)
})

test_that("expand_imputed_data correctly replaces missing values", {
  original <- make_original_data()
  imputed <- make_imputed_data()
  vars <- make_test_vars()

  reduced <- reduce_imputed_data(imputed, original, vars)
  expanded <- expand_imputed_data(reduced, original, vars)

  # Check that originally missing values are now filled
  for (imp_id in c("1", "2", "3")) {
    exp_subset <- expanded[expanded$IMPID == imp_id, ]

    # S2 Week 8 should have imputed value
    val <- exp_subset$CHG[exp_subset$USUBJID == "S2" & exp_subset$AVISIT == "Week 8"]
    expect_false(is.na(val))
    expect_equal(val, 1.0 + as.numeric(imp_id) * 0.1)
  }
})

test_that("expand_imputed_data preserves observed values", {
  original <- make_original_data()
  imputed <- make_imputed_data()
  vars <- make_test_vars()

  reduced <- reduce_imputed_data(imputed, original, vars)
  expanded <- expand_imputed_data(reduced, original, vars)

  # Check that observed values are unchanged
  for (imp_id in c("1", "2", "3")) {
    exp_subset <- expanded[expanded$IMPID == imp_id, ]

    # S1 Week 4 should have original value
    val <- exp_subset$CHG[exp_subset$USUBJID == "S1" & exp_subset$AVISIT == "Week 4"]
    expect_equal(val, 1.1)

    # S4 all complete
    s4_vals <- exp_subset$CHG[exp_subset$USUBJID == "S4"]
    expect_equal(s4_vals, c(1.5, 2.5, 3.5))
  }
})

test_that("expand_imputed_data puts IMPID column first", {
  original <- make_original_data()
  imputed <- make_imputed_data()
  vars <- make_test_vars()

  reduced <- reduce_imputed_data(imputed, original, vars)
  expanded <- expand_imputed_data(reduced, original, vars)

  expect_equal(names(expanded)[1], "IMPID")
})

test_that("expand_imputed_data handles single imputation", {
  original <- make_original_data()
  # Create single imputation
  imputed <- original
  imputed$IMPID <- "1"
  imputed$CHG[is.na(imputed$CHG)] <- 99.9
  imputed <- imputed[, c("IMPID", setdiff(names(imputed), "IMPID"))]

  vars <- make_test_vars()

  reduced <- reduce_imputed_data(imputed, original, vars)
  expanded <- expand_imputed_data(reduced, original, vars)

  expect_equal(nrow(expanded), nrow(original))
  expect_equal(unique(expanded$IMPID), "1")
})

test_that("expand_imputed_data handles empty reduced_data", {
  original <- make_original_data()
  original$CHG[is.na(original$CHG)] <- 999  # No missing values

  imputed <- make_imputed_data()
  vars <- make_test_vars()

  reduced <- reduce_imputed_data(imputed, original, vars)
  expect_equal(nrow(reduced), 0)

  expanded <- expand_imputed_data(reduced, original, vars)

  # Should return original data with single IMPID
  expect_equal(nrow(expanded), nrow(original))
  expect_true("IMPID" %in% names(expanded))
})

test_that("expand_imputed_data errors on missing required columns", {
  original <- make_original_data()
  imputed <- make_imputed_data()
  vars <- make_test_vars()

  reduced <- reduce_imputed_data(imputed, original, vars)
  reduced$CHG <- NULL

  expect_error(
    expand_imputed_data(reduced, original, vars),
    "reduced_data.*CHG"
  )
})

test_that("expand_imputed_data errors on non-dataframe inputs", {
  vars <- make_test_vars()
  reduced <- data.frame(IMPID = "1", USUBJID = "S1", AVISIT = "Week 4", CHG = 1)

  expect_error(
    expand_imputed_data("not a df", make_original_data(), vars),
    "must be a data.frame"
  )
  expect_error(
    expand_imputed_data(reduced, "not a df", vars),
    "must be a data.frame"
  )
})


# =============================================================================
# Integration tests with package data
# =============================================================================

test_that("reduce/expand works with ADMI and ADEFF package data", {
  skip_if_not_installed("rbmiUtils")

  data("ADMI", package = "rbmiUtils")
  data("ADEFF", package = "rbmiUtils")

  # Prepare original data to match ADMI structure
  original <- ADEFF
  original$TRT <- original$TRT01P
  original$USUBJID <- as.character(original$USUBJID)

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG"
  )

  # Reduce
  reduced <- reduce_imputed_data(ADMI, original, vars)

  # Should be much smaller than ADMI
  expect_lt(nrow(reduced), nrow(ADMI))

  # Expand back
  expanded <- expand_imputed_data(reduced, original, vars)

  # Should have same number of rows as ADMI
  expect_equal(nrow(expanded), nrow(ADMI))

  # Verify imputed values match
  # Sort both for comparison
  admi_sorted <- ADMI[order(ADMI$IMPID, ADMI$USUBJID, ADMI$AVISIT), ]
  exp_sorted <- expanded[order(expanded$IMPID, expanded$USUBJID, expanded$AVISIT), ]

  expect_equal(as.numeric(exp_sorted$CHG), as.numeric(admi_sorted$CHG))
})

test_that("storage savings are significant with many imputations", {
  original <- make_original_data()

  # Simulate 100 imputations
  imps <- lapply(1:100, function(imp_id) {
    dat <- original
    dat$IMPID <- as.character(imp_id)
    dat$CHG[is.na(dat$CHG)] <- runif(sum(is.na(dat$CHG)))
    dat
  })

  imputed <- do.call(rbind, imps)
  imputed <- imputed[, c("IMPID", setdiff(names(imputed), "IMPID"))]

  vars <- make_test_vars()

  reduced <- reduce_imputed_data(imputed, original, vars)

  # 12 rows * 100 imps = 1200 rows full
  # 3 missing * 100 imps = 300 rows reduced
  expect_equal(nrow(imputed), 1200)
  expect_equal(nrow(reduced), 300)

  # 25% of full size
  compression <- nrow(reduced) / nrow(imputed)
  expect_equal(compression, 0.25)
})
