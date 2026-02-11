# --- Test fixtures ---

make_test_data <- function() {
  data.frame(
    USUBJID = factor(rep(c("S1", "S2", "S3", "S4"), each = 3)),
    AVISIT = factor(
      rep(c("Week 4", "Week 8", "Week 12"), 4),
      levels = c("Week 4", "Week 8", "Week 12")
    ),
    TRT = factor(rep(c("Placebo", "Drug A"), each = 6)),
    CHG = c(1.1, 2.2, 3.3, 0.5, NA, NA, 1.0, 2.0, NA, 1.5, NA, 2.5),
    BASE = rep(c(10, 12, 11, 9), each = 3),
    STRATA = factor(rep(c("A", "B", "A", "B"), each = 3)),
    stringsAsFactors = FALSE
  )
}

make_test_vars <- function() {
  rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA")
  )
}


# =============================================================================
# validate_data() tests
# =============================================================================

test_that("validate_data passes with valid data", {
  dat <- make_test_data()
  vars <- make_test_vars()
  expect_true(validate_data(dat, vars))
})

test_that("validate_data errors when data is not a data.frame", {
  vars <- make_test_vars()
  expect_error(
    validate_data(list(a = 1), vars),
    class = "rbmiUtils_error_validation"
  )
})

test_that("validate_data errors when required columns are missing", {
  dat <- make_test_data()
  vars <- make_test_vars()
  dat$CHG <- NULL
  expect_error(
    validate_data(dat, vars),
    "CHG",
    class = "rbmiUtils_error_validation"
  )
})

test_that("validate_data errors when multiple columns are missing", {
  dat <- make_test_data()
  vars <- make_test_vars()
  dat$CHG <- NULL
  dat$BASE <- NULL
  expect_error(validate_data(dat, vars), "CHG", class = "rbmiUtils_error_validation")
  expect_error(validate_data(dat, vars), "BASE", class = "rbmiUtils_error_validation")
})

test_that("validate_data errors when subjid is not factor or character", {
  dat <- make_test_data()
  dat$USUBJID <- as.integer(dat$USUBJID)
  vars <- make_test_vars()
  expect_error(
    validate_data(dat, vars),
    "must be a factor",
    class = "rbmiUtils_error_validation"
  )
})

test_that("validate_data warns when subjid is character", {
  dat <- make_test_data()
  dat$USUBJID <- as.character(dat$USUBJID)
  vars <- make_test_vars()
  expect_warning(
    validate_data(dat, vars),
    class = "rbmiUtils_warning_coercion"
  )
})

test_that("validate_data errors when outcome is not numeric", {
  dat <- make_test_data()
  dat$CHG <- as.character(dat$CHG)
  vars <- make_test_vars()
  expect_error(
    validate_data(dat, vars),
    "must be numeric",
    class = "rbmiUtils_error_validation"
  )
})

test_that("validate_data errors when covariates have NAs", {
  dat <- make_test_data()
  dat$BASE[1] <- NA
  vars <- make_test_vars()
  expect_error(
    validate_data(dat, vars),
    "missing",
    class = "rbmiUtils_error_validation"
  )
})

test_that("validate_data errors on duplicate subject-visit rows", {
  dat <- make_test_data()
  dat <- rbind(dat, dat[1, ])
  vars <- make_test_vars()
  expect_error(
    validate_data(dat, vars),
    "duplicate",
    class = "rbmiUtils_error_validation"
  )
})

test_that("validate_data collects multiple issues in one error", {
  dat <- make_test_data()
  dat$CHG <- as.character(dat$CHG)
  dat$BASE[1] <- NA
  vars <- make_test_vars()
  err <- tryCatch(
    validate_data(dat, vars),
    error = function(e) e
  )
  msg <- conditionMessage(err)
  expect_true(grepl("must be numeric", msg))
  expect_true(grepl("missing", msg))
  expect_s3_class(err, "rbmiUtils_error_validation")
})

test_that("validate_data handles interaction terms in covariates", {
  dat <- make_test_data()
  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE*STRATA")
  )
  expect_true(validate_data(dat, vars))
})

test_that("validate_data errors on interaction term with missing variable", {
  dat <- make_test_data()
  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE*MISSING_VAR")
  )
  expect_error(
    validate_data(dat, vars),
    "MISSING_VAR",
    class = "rbmiUtils_error_validation"
  )
})


# --- HRD-01: Malformed interaction terms ---

test_that("validate_data errors on empty covariate term", {
  dat <- make_test_data()
  vars <- rbmi::set_vars(
    subjid = "USUBJID", visit = "AVISIT", group = "TRT",
    outcome = "CHG", covariates = c("BASE", "")
  )
  expect_error(
    validate_data(dat, vars),
    "Empty covariate",
    class = "rbmiUtils_error_validation"
  )
})

test_that("validate_data errors on malformed interaction term with trailing operator", {
  dat <- make_test_data()
  vars <- rbmi::set_vars(
    subjid = "USUBJID", visit = "AVISIT", group = "TRT",
    outcome = "CHG", covariates = c("A*")
  )
  expect_error(
    validate_data(dat, vars),
    "Malformed",
    class = "rbmiUtils_error_validation"
  )
})

test_that("validate_data errors on malformed interaction term with leading operator", {
  dat <- make_test_data()
  vars <- rbmi::set_vars(
    subjid = "USUBJID", visit = "AVISIT", group = "TRT",
    outcome = "CHG", covariates = c(":B")
  )
  expect_error(
    validate_data(dat, vars),
    "Malformed",
    class = "rbmiUtils_error_validation"
  )
})

test_that("validate_data errors on consecutive operators in interaction term", {
  dat <- make_test_data()
  vars <- rbmi::set_vars(
    subjid = "USUBJID", visit = "AVISIT", group = "TRT",
    outcome = "CHG", covariates = c("A**B")
  )
  expect_error(
    validate_data(dat, vars),
    "Malformed",
    class = "rbmiUtils_error_validation"
  )
})


# --- HRD-04: Empty data frame ---

test_that("validate_data errors on empty data frame", {
  dat <- make_test_data()[0, ]
  vars <- make_test_vars()
  expect_error(
    validate_data(dat, vars),
    "0 rows",
    class = "rbmiUtils_error_validation"
  )
})


# --- HRD-05: All-NA covariates ---

test_that("validate_data warns on all-NA covariate column", {
  dat <- make_test_data()
  dat$BASE <- NA_real_
  vars <- make_test_vars()
  expect_warning(
    validate_data(dat, vars),
    class = "rbmiUtils_warning_coercion"
  )
})

test_that("validate_data still errors on partially-NA covariate column", {
  dat <- make_test_data()
  dat$BASE[1] <- NA
  vars <- make_test_vars()
  expect_error(
    validate_data(dat, vars),
    "missing",
    class = "rbmiUtils_error_validation"
  )
})


# --- HRD-06: Batched character column warnings ---

test_that("validate_data batches character column warnings", {
  dat <- make_test_data()
  dat$USUBJID <- as.character(dat$USUBJID)
  dat$AVISIT <- as.character(dat$AVISIT)
  dat$TRT <- as.character(dat$TRT)
  vars <- make_test_vars()
  # Should get exactly one warning (batched), not three
  w <- testthat::capture_warnings(validate_data(dat, vars))
  # Filter to character coercion warnings
  coercion_warnings <- w[grepl("character", w)]
  expect_equal(length(coercion_warnings), 1)
})


# --- All-NA outcome ---

test_that("validate_data errors when all outcome values are NA", {
  dat <- make_test_data()
  dat$CHG <- NA_real_
  vars <- make_test_vars()
  expect_error(
    validate_data(dat, vars),
    "All.*NA",
    class = "rbmiUtils_error_validation"
  )
})


# --- data_ice validation within validate_data ---

test_that("validate_data passes with valid data_ice", {
  dat <- make_test_data()
  vars <- make_test_vars()
  data_ice <- data.frame(
    USUBJID = "S2",
    AVISIT = "Week 8",
    strategy = "JR",
    stringsAsFactors = FALSE
  )
  expect_true(validate_data(dat, vars, data_ice = data_ice))
})

test_that("validate_data errors when data_ice is not a data.frame", {
  dat <- make_test_data()
  vars <- make_test_vars()
  expect_error(
    validate_data(dat, vars, data_ice = "bad"),
    "data_ice",
    class = "rbmiUtils_error_validation"
  )
})

test_that("validate_data errors when data_ice missing required columns", {
  dat <- make_test_data()
  vars <- make_test_vars()
  data_ice <- data.frame(USUBJID = "S2", stringsAsFactors = FALSE)
  expect_error(
    validate_data(dat, vars, data_ice = data_ice),
    "data_ice",
    class = "rbmiUtils_error_validation"
  )
})

test_that("validate_data errors when data_ice has unknown subjects", {
  dat <- make_test_data()
  vars <- make_test_vars()
  data_ice <- data.frame(
    USUBJID = "S99",
    AVISIT = "Week 8",
    strategy = "JR",
    stringsAsFactors = FALSE
  )
  expect_error(
    validate_data(dat, vars, data_ice = data_ice),
    "not found",
    class = "rbmiUtils_error_validation"
  )
})

test_that("validate_data errors when data_ice has invalid visits", {
  dat <- make_test_data()
  vars <- make_test_vars()
  data_ice <- data.frame(
    USUBJID = "S2",
    AVISIT = "Week 99",
    strategy = "JR",
    stringsAsFactors = FALSE
  )
  expect_error(
    validate_data(dat, vars, data_ice = data_ice),
    "invalid visit",
    class = "rbmiUtils_error_validation"
  )
})

test_that("validate_data errors when data_ice has invalid strategy", {
  dat <- make_test_data()
  vars <- make_test_vars()
  data_ice <- data.frame(
    USUBJID = "S2",
    AVISIT = "Week 8",
    strategy = "INVALID",
    stringsAsFactors = FALSE
  )
  expect_error(
    validate_data(dat, vars, data_ice = data_ice),
    "unrecognised strategy",
    class = "rbmiUtils_error_validation"
  )
})

test_that("validate_data errors when data_ice has duplicate subjects", {
  dat <- make_test_data()
  vars <- make_test_vars()
  data_ice <- data.frame(
    USUBJID = c("S2", "S2"),
    AVISIT = c("Week 8", "Week 12"),
    strategy = c("JR", "JR"),
    stringsAsFactors = FALSE
  )
  expect_error(
    validate_data(dat, vars, data_ice = data_ice),
    "multiple rows",
    class = "rbmiUtils_error_validation"
  )
})


# --- validate_data() edge case tests (HRD-07) ---

test_that("validate_data handles single subject without error", {
  dat <- data.frame(
    USUBJID = factor(rep("S1", 3)),
    AVISIT = factor(c("Week 4", "Week 8", "Week 12"),
                    levels = c("Week 4", "Week 8", "Week 12")),
    TRT = factor(rep("Drug A", 3)),
    CHG = c(1.0, 2.0, 3.0),
    BASE = rep(10, 3),
    STRATA = factor(rep("A", 3))
  )
  vars <- make_test_vars()
  expect_true(validate_data(dat, vars))
})

test_that("validate_data handles single visit without error", {
  dat <- data.frame(
    USUBJID = factor(c("S1", "S2", "S3")),
    AVISIT = factor(rep("Week 4", 3)),
    TRT = factor(c("Placebo", "Drug A", "Drug A")),
    CHG = c(1.0, 2.0, 3.0),
    BASE = c(10, 12, 11),
    STRATA = factor(c("A", "B", "A"))
  )
  vars <- make_test_vars()
  expect_true(validate_data(dat, vars))
})

test_that("validate_data handles single subject single visit without error", {
  dat <- data.frame(
    USUBJID = factor("S1"),
    AVISIT = factor("Week 4"),
    TRT = factor("Drug A"),
    CHG = 1.0,
    BASE = 10,
    STRATA = factor("A")
  )
  vars <- make_test_vars()
  expect_true(validate_data(dat, vars))
})

test_that("validate_data emits info message when all outcomes complete", {
  dat <- make_test_data()
  dat$CHG <- seq_len(nrow(dat))  # no NAs
  vars <- make_test_vars()
  expect_message(
    validate_data(dat, vars),
    class = "rbmiUtils_info"
  )
})

test_that("validate_data handles single subject with missing outcome", {
  dat <- data.frame(
    USUBJID = factor(rep("S1", 3)),
    AVISIT = factor(c("Week 4", "Week 8", "Week 12"),
                    levels = c("Week 4", "Week 8", "Week 12")),
    TRT = factor(rep("Drug A", 3)),
    CHG = c(1.0, NA, NA),
    BASE = rep(10, 3),
    STRATA = factor(rep("A", 3))
  )
  vars <- make_test_vars()
  expect_true(validate_data(dat, vars))
})

test_that("validate_data passes cleanly with complete covariates", {
  dat <- make_test_data()
  dat$CHG <- seq_len(nrow(dat))  # complete outcomes to avoid info message
  vars <- make_test_vars()
  # Should not produce any errors (may produce informational message about complete data)
  result <- validate_data(dat, vars)
  expect_true(result)
})


# =============================================================================
# prepare_data_ice() tests
# =============================================================================

test_that("prepare_data_ice returns correct structure", {
  dat <- make_test_data()
  dat$DISCFL <- c("N","N","N", "N","Y","Y", "N","N","Y", "N","N","N")
  vars <- make_test_vars()

  result <- prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "JR")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("USUBJID", "AVISIT", "strategy"))
})

test_that("prepare_data_ice takes first visit per subject", {
  dat <- make_test_data()
  # S2 has ICE at Week 8 and Week 12
  dat$DISCFL <- c("N","N","N", "N","Y","Y", "N","N","N", "N","N","N")
  vars <- make_test_vars()

  result <- prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "CR")

  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$USUBJID), "S2")
  expect_equal(as.character(result$AVISIT), "Week 8")
  expect_equal(result$strategy, "CR")
})

test_that("prepare_data_ice handles logical flag", {
  dat <- make_test_data()
  dat$ICE <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
               FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  vars <- make_test_vars()

  result <- prepare_data_ice(dat, vars, ice_col = "ICE", strategy = "CIR")

  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$USUBJID), "S2")
})

test_that("prepare_data_ice handles numeric 0/1 flag", {
  dat <- make_test_data()
  dat$ICE_NUM <- c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0)
  vars <- make_test_vars()

  result <- prepare_data_ice(dat, vars, ice_col = "ICE_NUM", strategy = "MAR")

  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$USUBJID), "S2")
})

test_that("prepare_data_ice returns empty data.frame when no ICE flags", {
  dat <- make_test_data()
  dat$DISCFL <- rep("N", nrow(dat))
  vars <- make_test_vars()

  result <- suppressMessages(
    prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "JR")
  )

  expect_equal(nrow(result), 0)
  expect_named(result, c("USUBJID", "AVISIT", "strategy"))
})

test_that("prepare_data_ice handles multiple subjects with ICE", {
  dat <- make_test_data()
  dat$DISCFL <- c("N","N","N", "N","Y","Y", "N","N","Y", "N","Y","Y")
  vars <- make_test_vars()

  result <- prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "LMCF")

  expect_equal(nrow(result), 3)
  expect_true(all(result$strategy == "LMCF"))
})

test_that("prepare_data_ice errors on missing ice_col", {
  dat <- make_test_data()
  vars <- make_test_vars()
  expect_error(
    prepare_data_ice(dat, vars, ice_col = "NONEXIST", strategy = "JR"),
    "not found",
    class = "rbmiUtils_error_validation"
  )
})

test_that("prepare_data_ice errors on invalid strategy", {
  dat <- make_test_data()
  dat$DISCFL <- c("N","N","N", "N","Y","Y", "N","N","N", "N","N","N")
  vars <- make_test_vars()
  expect_error(
    prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "INVALID"),
    "must be one of",
    class = "rbmiUtils_error_validation"
  )
})

test_that("prepare_data_ice handles NA values in ice_col", {
  dat <- make_test_data()
  dat$DISCFL <- c(NA, "N", "N", "N", "Y", NA, "N", "N", "N", "N", "N", "N")
  vars <- make_test_vars()

  result <- prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "JR")

  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$USUBJID), "S2")
})


# --- HRD-02: NULL strategy error in prepare_data_ice ---

test_that("prepare_data_ice errors when vars$strategy is NULL", {
  dat <- make_test_data()
  dat$DISCFL <- c("N","N","N", "N","Y","Y", "N","N","N", "N","N","N")
  vars <- make_test_vars()
  vars$strategy <- NULL
  expect_error(
    prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "JR"),
    class = "rbmiUtils_error_validation"
  )
})


# --- HRD-03: Character visit warning in prepare_data_ice ---

test_that("prepare_data_ice warns when visit column is character", {
  dat <- make_test_data()
  dat$AVISIT <- as.character(dat$AVISIT)
  dat$DISCFL <- c("N","N","N", "N","Y","Y", "N","N","N", "N","N","N")
  vars <- make_test_vars()
  expect_warning(
    prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "JR"),
    class = "rbmiUtils_warning_coercion"
  )
})


# --- prepare_data_ice() edge case tests (HRD-07) ---

test_that("prepare_data_ice handles single subject with ICE", {
  dat <- data.frame(
    USUBJID = factor(rep("S1", 3)),
    AVISIT = factor(c("Week 4", "Week 8", "Week 12"),
                    levels = c("Week 4", "Week 8", "Week 12")),
    TRT = factor(rep("Drug A", 3)),
    CHG = c(1.0, NA, NA),
    DISCFL = c("N", "Y", "Y")
  )
  vars <- make_test_vars()
  result <- prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "JR")
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$USUBJID), "S1")
  expect_equal(as.character(result$AVISIT), "Week 8")
})

test_that("prepare_data_ice handles single subject without ICE", {
  dat <- data.frame(
    USUBJID = factor(rep("S1", 3)),
    AVISIT = factor(c("Week 4", "Week 8", "Week 12"),
                    levels = c("Week 4", "Week 8", "Week 12")),
    TRT = factor(rep("Drug A", 3)),
    CHG = c(1.0, 2.0, 3.0),
    DISCFL = c("N", "N", "N")
  )
  vars <- make_test_vars()
  result <- suppressMessages(
    prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "JR")
  )
  expect_equal(nrow(result), 0)
  expect_named(result, c("USUBJID", "AVISIT", "strategy"))
})

test_that("prepare_data_ice handles single visit", {
  dat <- data.frame(
    USUBJID = factor(c("S1", "S2", "S3")),
    AVISIT = factor(rep("Week 4", 3)),
    TRT = factor(c("Placebo", "Drug A", "Drug A")),
    CHG = c(1.0, NA, 3.0),
    DISCFL = c("N", "Y", "N")
  )
  vars <- make_test_vars()
  result <- prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "CR")
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$USUBJID), "S2")
})

test_that("prepare_data_ice emits info message when no ICE flags found", {
  dat <- make_test_data()
  dat$DISCFL <- rep("N", nrow(dat))
  vars <- make_test_vars()
  expect_message(
    result <- prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "JR"),
    class = "rbmiUtils_info"
  )
  expect_equal(nrow(result), 0)
})

test_that("prepare_data_ice handles all subjects with ICE at first visit", {
  dat <- make_test_data()
  dat$DISCFL <- rep("Y", nrow(dat))
  vars <- make_test_vars()
  result <- prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "MAR")
  # Each subject should have exactly one row with first visit
  expect_equal(nrow(result), length(unique(dat$USUBJID)))
  expect_true(all(as.character(result$AVISIT) == "Week 4"))
})

test_that("prepare_data_ice handles all-NA ice flag column", {
  dat <- make_test_data()
  dat$DISCFL <- NA_character_
  vars <- make_test_vars()
  result <- suppressMessages(
    prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "JR")
  )
  expect_equal(nrow(result), 0)
})

test_that("prepare_data_ice handles single subject single visit with ICE", {
  dat <- data.frame(
    USUBJID = factor("S1"),
    AVISIT = factor("Week 4"),
    TRT = factor("Drug A"),
    CHG = NA_real_,
    DISCFL = "Y"
  )
  vars <- make_test_vars()
  result <- prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "CIR")
  expect_equal(nrow(result), 1)
  expect_equal(result$strategy, "CIR")
})


# =============================================================================
# summarise_missingness() tests
# =============================================================================

test_that("summarise_missingness returns correct structure", {
  dat <- make_test_data()
  vars <- make_test_vars()

  result <- summarise_missingness(dat, vars)

  expect_type(result, "list")
  expect_named(result, c("by_visit", "patterns", "summary"))
  expect_s3_class(result$by_visit, "tbl_df")
  expect_s3_class(result$patterns, "tbl_df")
  expect_s3_class(result$summary, "tbl_df")
})

test_that("summarise_missingness by_visit has correct columns", {
  dat <- make_test_data()
  vars <- make_test_vars()

  result <- summarise_missingness(dat, vars)

  expect_true(all(c("visit", "group", "n", "n_miss", "pct_miss") %in%
                    names(result$by_visit)))
})

test_that("summarise_missingness computes correct missing counts", {
  dat <- make_test_data()
  vars <- make_test_vars()

  result <- summarise_missingness(dat, vars)

  # S1: complete, S2: NA at Week 8 and 12, (Placebo group)
  # S3: NA at Week 12, S4: NA at Week 8 (Drug A group)
  placebo_w4 <- result$by_visit[
    result$by_visit$visit == "Week 4" & result$by_visit$group == "Placebo", ]
  expect_equal(placebo_w4$n_miss, 0)

  placebo_w8 <- result$by_visit[
    result$by_visit$visit == "Week 8" & result$by_visit$group == "Placebo", ]
  expect_equal(placebo_w8$n_miss, 1)
})

test_that("summarise_missingness identifies complete pattern", {
  dat <- make_test_data()
  vars <- make_test_vars()

  result <- summarise_missingness(dat, vars)

  s1_pattern <- result$patterns[result$patterns$USUBJID == "S1", ]
  expect_equal(s1_pattern$pattern, "complete")
  expect_true(is.na(s1_pattern$dropout_visit))
})

test_that("summarise_missingness identifies monotone dropout", {
  dat <- make_test_data()
  vars <- make_test_vars()

  result <- summarise_missingness(dat, vars)

  # S2 has NA at Week 8 and Week 12 -> monotone starting at Week 8
  s2_pattern <- result$patterns[result$patterns$USUBJID == "S2", ]
  expect_equal(s2_pattern$pattern, "monotone")
  expect_equal(s2_pattern$dropout_visit, "Week 8")
})

test_that("summarise_missingness identifies intermittent pattern", {
  dat <- make_test_data()
  vars <- make_test_vars()

  result <- summarise_missingness(dat, vars)

  # S4 has NA at Week 8, observed at Week 12 -> intermittent
  s4_pattern <- result$patterns[result$patterns$USUBJID == "S4", ]
  expect_equal(s4_pattern$pattern, "intermittent")
  expect_true(is.na(s4_pattern$dropout_visit))
})

test_that("summarise_missingness summary counts are correct", {
  dat <- make_test_data()
  vars <- make_test_vars()

  result <- summarise_missingness(dat, vars)

  total <- result$summary
  expect_equal(sum(total$n_subjects), 4)
  expect_equal(sum(total$n_complete), 1)  # S1
  expect_equal(sum(total$n_monotone), 2)  # S2, S3
  expect_equal(sum(total$n_intermittent), 1)  # S4
})

test_that("summarise_missingness works with no missing data", {
  dat <- make_test_data()
  dat$CHG <- seq_len(nrow(dat))
  vars <- make_test_vars()

  result <- summarise_missingness(dat, vars)

  expect_true(all(result$by_visit$n_miss == 0))
  expect_true(all(result$patterns$pattern == "complete"))
  expect_equal(sum(result$summary$n_complete), 4)
})

test_that("summarise_missingness works with single visit", {
  dat <- make_test_data()
  dat <- dat[dat$AVISIT == "Week 4", ]
  dat$AVISIT <- factor("Week 4")
  vars <- make_test_vars()

  result <- summarise_missingness(dat, vars)

  expect_equal(nrow(result$by_visit), 2)  # 2 groups
})

test_that("summarise_missingness errors on missing columns", {
  dat <- make_test_data()
  dat$CHG <- NULL
  vars <- make_test_vars()

  expect_error(summarise_missingness(dat, vars), "CHG")
})

test_that("summarise_missingness errors on non-dataframe input", {
  vars <- make_test_vars()
  expect_error(summarise_missingness("bad", vars), "must be a data.frame")
})
