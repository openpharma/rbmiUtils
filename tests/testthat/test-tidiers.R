test_that("positive test tidy_pool_obj", {
  data("ADMI")
  set.seed(122)
  N_IMPUTATIONS <- 100
  BURN_IN <- 200
  BURN_BETWEEN <- 5

  # Convert key columns to factors
  ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
  ADMI$USUBJID <- factor(ADMI$USUBJID)
  ADMI$AVISIT <- factor(ADMI$AVISIT)

  # Define key variables for ANCOVA analysis
  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION") # Covariates for adjustment
  )

  # Specify the imputation method (Bayesian) - need for pool step
  method <- rbmi::method_bayes(
    n_samples = N_IMPUTATIONS,
    control = rbmi::control_bayes(
      warmup = BURN_IN,
      thin = BURN_BETWEEN
    )
  )

  # Perform ANCOVA Analysis on Each Imputed Dataset
  ana_obj_ancova <- analyse_mi_data(
    data = ADMI,
    vars = vars,
    method = method,
    fun = rbmi::ancova, # Apply ANCOVA
    delta = NULL # No sensitivity analysis adjustment
  )

  pool_obj_ancova <- rbmi::pool(ana_obj_ancova)

  # Just check the class of pool_obj_ancova before passing to tidy_pool_obj
  expect_s3_class(pool_obj_ancova, "pool")

  tidy_df <- tidy_pool_obj(pool_obj_ancova)

  # Test output
  # class check
  expect_s3_class(tidy_df, c("tbl_df", "tbl", "data.frame"))

  # column names check
  expect_named(
    tidy_df,
    c(
      "parameter",
      "description",
      "visit",
      "parameter_type",
      "lsm_type",
      "est",
      "se",
      "lci",
      "uci",
      "pval"
    )
  )

  # column types check
  expect_identical(
    lapply(tidy_df, class),
    list(
      parameter = "character",
      description = "character",
      visit = "character",
      parameter_type = "character",
      lsm_type = "character",
      est = "numeric",
      se = "numeric",
      lci = "numeric",
      uci = "numeric",
      pval = "numeric"
    )
  )

  # Columns created by tidy_pool_obj are:
  # - parameter_type
  # - lsm_type
  # - visit
  # - description
  expect_identical(
    tidy_df[, c('description', 'visit', 'parameter_type', 'lsm_type')],
    dplyr::as_tibble(
      data.frame(
        description = c(
          "Treatment Comparison",
          "Least Squares Mean for Reference at Week 24",
          "Least Squares Mean for Alternative at Week 24",
          "Treatment Comparison",
          "Least Squares Mean for Reference at Week 48",
          "Least Squares Mean for Alternative at Week 48"
        ),
        visit = c(
          "Week 24",
          "Week 24",
          "Week 24",
          "Week 48",
          "Week 48",
          "Week 48"
        ),
        parameter_type = c("trt", "lsm", "lsm", "trt", "lsm", "lsm"),
        lsm_type = c(NA, "ref", "alt", NA, "ref", "alt")
      )
    )
  )
})

test_that("tidy_pool_obj requires a pool object", {
  expect_true(class(mtcars) != "pool")

  expect_error(
    tidy_pool_obj(mtcars),
    "Input object must be of class 'pool'"
  )
})


# =============================================================================
# Additional comprehensive tests for tidy_pool_obj
# =============================================================================

test_that("tidy_pool_obj handles single visit data", {
  data("ADMI")
  set.seed(234)

  # Filter to single visit
  ADMI_single <- ADMI[ADMI$AVISIT == "Week 24", ]
  ADMI_single$TRT <- factor(ADMI_single$TRT, levels = c("Placebo", "Drug A"))
  ADMI_single$USUBJID <- factor(ADMI_single$USUBJID)
  ADMI_single$AVISIT <- factor(ADMI_single$AVISIT)

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = 50,
    control = rbmi::control_bayes(warmup = 100, thin = 2)
  )

  ana_obj <- analyse_mi_data(
    data = ADMI_single,
    vars = vars,
    method = method,
    fun = rbmi::ancova
  )

  pool_obj <- rbmi::pool(ana_obj)
  tidy_df <- tidy_pool_obj(pool_obj)

  expect_s3_class(tidy_df, "tbl_df")
  expect_true(all(tidy_df$visit == "Week 24" | is.na(tidy_df$visit)))
})


test_that("tidy_pool_obj preserves numeric precision", {
  data("ADMI")
  set.seed(345)

  ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
  ADMI$USUBJID <- factor(ADMI$USUBJID)
  ADMI$AVISIT <- factor(ADMI$AVISIT)

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = 50,
    control = rbmi::control_bayes(warmup = 100, thin = 2)
  )

  ana_obj <- analyse_mi_data(
    data = ADMI,
    vars = vars,
    method = method,
    fun = rbmi::ancova
  )

  pool_obj <- rbmi::pool(ana_obj)
  tidy_df <- tidy_pool_obj(pool_obj)

  # Check numeric columns are numeric
  expect_type(tidy_df$est, "double")
  expect_type(tidy_df$se, "double")
  expect_type(tidy_df$lci, "double")
  expect_type(tidy_df$uci, "double")
  expect_type(tidy_df$pval, "double")
})


test_that("tidy_pool_obj correctly identifies lsm_ref parameters", {
  data("ADMI")
  set.seed(456)

  ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
  ADMI$USUBJID <- factor(ADMI$USUBJID)
  ADMI$AVISIT <- factor(ADMI$AVISIT)

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = 50,
    control = rbmi::control_bayes(warmup = 100, thin = 2)
  )

  ana_obj <- analyse_mi_data(
    data = ADMI,
    vars = vars,
    method = method,
    fun = rbmi::ancova
  )

  pool_obj <- rbmi::pool(ana_obj)
  tidy_df <- tidy_pool_obj(pool_obj)

  # Check that ref and alt are correctly identified
  ref_rows <- tidy_df[tidy_df$lsm_type == "ref" & !is.na(tidy_df$lsm_type), ]
  alt_rows <- tidy_df[tidy_df$lsm_type == "alt" & !is.na(tidy_df$lsm_type), ]

  expect_true(nrow(ref_rows) > 0)
  expect_true(nrow(alt_rows) > 0)
  expect_true(all(grepl("Reference", ref_rows$description)))
  expect_true(all(grepl("Alternative", alt_rows$description)))
})


test_that("tidy_pool_obj correctly identifies treatment comparison parameters", {
  data("ADMI")
  set.seed(567)

  ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
  ADMI$USUBJID <- factor(ADMI$USUBJID)
  ADMI$AVISIT <- factor(ADMI$AVISIT)

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = 50,
    control = rbmi::control_bayes(warmup = 100, thin = 2)
  )

  ana_obj <- analyse_mi_data(
    data = ADMI,
    vars = vars,
    method = method,
    fun = rbmi::ancova
  )

  pool_obj <- rbmi::pool(ana_obj)
  tidy_df <- tidy_pool_obj(pool_obj)

  # Check treatment comparison rows
  trt_rows <- tidy_df[tidy_df$parameter_type == "trt", ]

  expect_true(nrow(trt_rows) > 0)
  expect_true(all(grepl("Treatment Comparison", trt_rows$description)))
  expect_true(all(is.na(trt_rows$lsm_type)))
})


test_that("tidy_pool_obj output can be filtered by visit", {
  data("ADMI")
  set.seed(678)

  ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
  ADMI$USUBJID <- factor(ADMI$USUBJID)
  ADMI$AVISIT <- factor(ADMI$AVISIT)

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = 50,
    control = rbmi::control_bayes(warmup = 100, thin = 2)
  )

  ana_obj <- analyse_mi_data(
    data = ADMI,
    vars = vars,
    method = method,
    fun = rbmi::ancova
  )

  pool_obj <- rbmi::pool(ana_obj)
  tidy_df <- tidy_pool_obj(pool_obj)

  # Filter by visit should work
  week24_df <- tidy_df[tidy_df$visit == "Week 24", ]
  week48_df <- tidy_df[tidy_df$visit == "Week 48", ]

  expect_true(nrow(week24_df) > 0)
  expect_true(nrow(week48_df) > 0)
})


test_that("tidy_pool_obj errors with NULL input", {
  expect_error(tidy_pool_obj(NULL), "must be of class 'pool'")
})


test_that("tidy_pool_obj errors with list input", {
  expect_error(tidy_pool_obj(list(a = 1, b = 2)), "must be of class 'pool'")
})
