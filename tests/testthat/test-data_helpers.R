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
  expect_error(validate_data(list(a = 1), vars), "must be a data.frame")
})

test_that("validate_data errors when required columns are missing", {
  dat <- make_test_data()
  vars <- make_test_vars()
  dat$CHG <- NULL
  expect_error(validate_data(dat, vars), "CHG")
})

test_that("validate_data errors when multiple columns are missing", {
  dat <- make_test_data()
  vars <- make_test_vars()
  dat$CHG <- NULL
  dat$BASE <- NULL
  expect_error(validate_data(dat, vars), "CHG")
  expect_error(validate_data(dat, vars), "BASE")
})

test_that("validate_data errors when subjid is not factor or character", {
  dat <- make_test_data()
  dat$USUBJID <- as.integer(dat$USUBJID)
  vars <- make_test_vars()
  expect_error(validate_data(dat, vars), "must be a factor")
})

test_that("validate_data warns when subjid is character", {
  dat <- make_test_data()
  dat$USUBJID <- as.character(dat$USUBJID)
  vars <- make_test_vars()
  expect_warning(validate_data(dat, vars), "character")
})

test_that("validate_data errors when outcome is not numeric", {
  dat <- make_test_data()
  dat$CHG <- as.character(dat$CHG)
  vars <- make_test_vars()
  expect_error(validate_data(dat, vars), "outcome.*must be numeric")
})

test_that("validate_data errors when covariates have NAs", {
  dat <- make_test_data()
  dat$BASE[1] <- NA
  vars <- make_test_vars()
  expect_error(validate_data(dat, vars), "BASE.*missing")
})

test_that("validate_data errors on duplicate subject-visit rows", {
  dat <- make_test_data()
  dat <- rbind(dat, dat[1, ])
  vars <- make_test_vars()
  expect_error(validate_data(dat, vars), "duplicate")
})

test_that("validate_data collects multiple issues in one error", {
  dat <- make_test_data()
  dat$CHG <- as.character(dat$CHG)
  dat$BASE[1] <- NA
  vars <- make_test_vars()
  err <- tryCatch(validate_data(dat, vars), error = function(e) e$message)
  expect_true(grepl("outcome.*must be numeric", err))
  expect_true(grepl("BASE.*missing", err))
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
  expect_error(validate_data(dat, vars), "MISSING_VAR")
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
  expect_error(validate_data(dat, vars, data_ice = "bad"), "data_ice.*must be a data.frame")
})

test_that("validate_data errors when data_ice missing required columns", {
  dat <- make_test_data()
  vars <- make_test_vars()
  data_ice <- data.frame(USUBJID = "S2", stringsAsFactors = FALSE)
  expect_error(validate_data(dat, vars, data_ice = data_ice), "data_ice.*missing")
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
  expect_error(validate_data(dat, vars, data_ice = data_ice), "not found in `data`")
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
  expect_error(validate_data(dat, vars, data_ice = data_ice), "invalid visit")
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
  expect_error(validate_data(dat, vars, data_ice = data_ice), "unrecognised strategy")
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
  expect_error(validate_data(dat, vars, data_ice = data_ice), "multiple rows")
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

  result <- prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "JR")

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
    "not found"
  )
})

test_that("prepare_data_ice errors on invalid strategy", {
  dat <- make_test_data()
  dat$DISCFL <- c("N","N","N", "N","Y","Y", "N","N","N", "N","N","N")
  vars <- make_test_vars()
  expect_error(
    prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "INVALID"),
    "must be one of"
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
