# =============================================================================
# Integration tests for complete rbmiUtils workflows
# =============================================================================

# These tests verify that functions work together correctly in realistic
# analysis pipelines.


# --- Test fixtures ---

make_integration_data <- function() {
  set.seed(42)
  n_subjects <- 50
  visits <- c("Week 4", "Week 8", "Week 12")

  data <- expand.grid(
    USUBJID = sprintf("S%03d", 1:n_subjects),
    AVISIT = visits,
    stringsAsFactors = FALSE
  )

  data$USUBJID <- factor(data$USUBJID)
  data$AVISIT <- factor(data$AVISIT, levels = visits)
  data$TRT <- factor(
    rep(c("Placebo", "Drug A"), each = n_subjects / 2)[as.integer(factor(data$USUBJID))],
    levels = c("Placebo", "Drug A")
  )
  data$BASE <- rep(rnorm(n_subjects, 50, 10), each = length(visits))
  data$STRATA <- factor(rep(sample(c("A", "B"), n_subjects, replace = TRUE), each = length(visits)))
  data$REGION <- factor(rep(sample(c("US", "EU", "ASIA"), n_subjects, replace = TRUE), each = length(visits)))

  # Generate outcome with treatment effect
  trt_effect <- ifelse(data$TRT == "Drug A", -5, 0)
  visit_effect <- as.integer(data$AVISIT) * 2
  data$CHG <- data$BASE * 0.1 + trt_effect + visit_effect + rnorm(nrow(data), 0, 5)

  # Add some missing values (monotone pattern for some subjects)
  # ~20% of subjects have dropout
  dropout_subjects <- sample(unique(data$USUBJID), n_subjects * 0.2)
  dropout_visits <- sample(2:3, length(dropout_subjects), replace = TRUE)

  for (i in seq_along(dropout_subjects)) {
    subj <- dropout_subjects[i]
    drop_visit <- dropout_visits[i]
    mask <- data$USUBJID == subj & as.integer(data$AVISIT) >= drop_visit
    data$CHG[mask] <- NA
  }

  # Add binary outcome for g-computation tests
  data$RESP <- as.integer(data$CHG < 0)
  data$RESP[is.na(data$CHG)] <- NA

  data
}


make_integration_vars <- function(with_strategy = FALSE) {
  if (with_strategy) {
    rbmi::set_vars(
      subjid = "USUBJID",
      visit = "AVISIT",
      group = "TRT",
      outcome = "CHG",
      covariates = c("BASE", "STRATA", "REGION"),
      strategy = "strategy"
    )
  } else {
    rbmi::set_vars(
      subjid = "USUBJID",
      visit = "AVISIT",
      group = "TRT",
      outcome = "CHG",
      covariates = c("BASE", "STRATA", "REGION")
    )
  }
}


# =============================================================================
# Test: Complete ANCOVA workflow
# =============================================================================

test_that("Complete ANCOVA workflow: validate -> analyse -> pool -> tidy", {
  data("ADMI")
  set.seed(123)

  # Step 1: Prepare data
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

  # Step 2: Analyse
  ana_obj <- analyse_mi_data(
    data = ADMI,
    vars = vars,
    method = method,
    fun = rbmi::ancova
  )

  expect_s3_class(ana_obj, "analysis")

  # Step 3: Pool
  pool_obj <- rbmi::pool(ana_obj)

  expect_s3_class(pool_obj, "pool")

  # Step 4: Tidy
 tidy_df <- tidy_pool_obj(pool_obj)

  expect_s3_class(tidy_df, "tbl_df")
  expect_true(nrow(tidy_df) > 0)
  expect_true(all(c("parameter", "description", "est", "se", "lci", "uci", "pval") %in%
                   names(tidy_df)))

  # Verify we have results for both visits
  expect_true("Week 24" %in% tidy_df$visit | "Week 48" %in% tidy_df$visit)
})


# =============================================================================
# Test: Data preparation workflow
# =============================================================================

test_that("Data preparation workflow: validate -> prepare_ice -> summarise_missingness", {
  dat <- make_integration_data()
  vars <- make_integration_vars(with_strategy = TRUE)

  # Add ICE flag for subjects with missing data
  dat$DISCFL <- ifelse(is.na(dat$CHG), "Y", "N")

  # Step 1: Validate data
  expect_true(validate_data(dat, vars))

  # Step 2: Prepare ICE data
  data_ice <- prepare_data_ice(dat, vars, ice_col = "DISCFL", strategy = "JR")

  expect_s3_class(data_ice, "data.frame")
  expect_true(nrow(data_ice) > 0)
  expect_true(all(c("USUBJID", "AVISIT", "strategy") %in% names(data_ice)))

  # Step 3: Summarise missingness
  miss_summary <- summarise_missingness(dat, vars)

  expect_type(miss_summary, "list")
  expect_true("by_visit" %in% names(miss_summary))
  expect_true("patterns" %in% names(miss_summary))
  expect_true("summary" %in% names(miss_summary))

  # Verify missingness summary is consistent
  total_miss <- sum(miss_summary$by_visit$n_miss)
  expected_miss <- sum(is.na(dat$CHG))
  expect_equal(total_miss, expected_miss)
})


# =============================================================================
# Test: Storage reduction workflow
# =============================================================================

test_that("Storage workflow: reduce -> expand roundtrip preserves data", {
  data("ADMI", package = "rbmiUtils")
  data("ADEFF", package = "rbmiUtils")

  # Prepare original data
  original <- ADEFF
  original$TRT <- original$TRT01P
  original$USUBJID <- as.character(original$USUBJID)

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG"
  )

  # Prepare imputed data
  ADMI$USUBJID <- as.character(ADMI$USUBJID)

  # Step 1: Reduce
  reduced <- reduce_imputed_data(ADMI, original, vars)

  expect_true(nrow(reduced) < nrow(ADMI))
  expect_true("IMPID" %in% names(reduced))

  # Step 2: Expand
  expanded <- expand_imputed_data(reduced, original, vars)

  expect_equal(nrow(expanded), nrow(ADMI))

  # Verify outcome values match for non-missing original values
  imp_ids <- unique(ADMI$IMPID)
  for (imp_id in imp_ids[1:min(3, length(imp_ids))]) {
    admi_subset <- ADMI[ADMI$IMPID == imp_id, ]
    expanded_subset <- expanded[expanded$IMPID == imp_id, ]

    # Sort both by subject and visit for comparison
    admi_subset <- admi_subset[order(admi_subset$USUBJID, admi_subset$AVISIT), ]
    expanded_subset <- expanded_subset[order(expanded_subset$USUBJID, expanded_subset$AVISIT), ]

    # Compare CHG values
    expect_equal(
      admi_subset$CHG,
      expanded_subset$CHG,
      tolerance = 1e-10
    )
  }
})


# =============================================================================
# Test: G-computation workflow
# =============================================================================

test_that("G-computation workflow: gcomp_responder_multi with pooling", {
  testthat::skip_if_not_installed("beeca")

  data("ADMI")
  set.seed(456)

  # Prepare data
  ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
  ADMI$USUBJID <- factor(ADMI$USUBJID)
  ADMI$AVISIT <- factor(ADMI$AVISIT)
  ADMI$STRATA <- factor(ADMI$STRATA)
  ADMI$REGION <- factor(ADMI$REGION)

  # Create binary outcome
  ADMI$RESP <- as.integer(ADMI$CHG < 0)

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "RESP",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = 50,
    control = rbmi::control_bayes(warmup = 100, thin = 2)
  )

  # Step 1: Analyse with g-computation
  ana_obj <- analyse_mi_data(
    data = ADMI,
    vars = vars,
    method = method,
    fun = gcomp_responder_multi,
    reference_levels = "Placebo",
    contrast = "diff",
    var_method = "Ge",
    type = "HC0"
  )

  expect_s3_class(ana_obj, "analysis")

  # Step 2: Pool results
  pool_obj <- rbmi::pool(ana_obj)

  expect_s3_class(pool_obj, "pool")
  expect_true(nrow(pool_obj) > 0)
})


# =============================================================================
# Test: Combined validation and analysis workflow
# =============================================================================

test_that("Validation catches issues before analysis", {
  dat <- make_integration_data()
  vars <- make_integration_vars()

  # Introduce validation issue - duplicate rows
  dat_with_dups <- rbind(dat, dat[1, ])

  expect_error(
    validate_data(dat_with_dups, vars),
    "duplicate"
  )

  # Introduce validation issue - missing covariate
  dat_missing_cov <- dat
  dat_missing_cov$BASE[1] <- NA

  expect_error(
    validate_data(dat_missing_cov, vars),
    "BASE.*missing"
  )
})


# =============================================================================
# Test: End-to-end with delta adjustment
# =============================================================================

test_that("End-to-end workflow with delta sensitivity analysis", {
  data("ADMI")
  set.seed(789)

  # Prepare data
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

  # Create delta adjustment (add 2 to Drug A arm)
  delta <- unique(ADMI[, c("USUBJID", "AVISIT", "TRT")])
  delta$delta <- ifelse(delta$TRT == "Drug A", 2, 0)

  # Analyse with delta
  ana_obj_delta <- analyse_mi_data(
    data = ADMI,
    vars = vars,
    method = method,
    fun = rbmi::ancova,
    delta = delta
  )

  expect_s3_class(ana_obj_delta, "analysis")
  expect_false(is.null(ana_obj_delta$delta))

  # Pool and tidy
  pool_obj <- rbmi::pool(ana_obj_delta)
  tidy_df <- tidy_pool_obj(pool_obj)

  expect_s3_class(tidy_df, "tbl_df")
  expect_true(nrow(tidy_df) > 0)
})


# =============================================================================
# Test: Workflow with custom analysis function
# =============================================================================

test_that("Workflow supports custom analysis functions", {
  data("ADMI")
  set.seed(101)

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
    n_samples = 20,
    control = rbmi::control_bayes(warmup = 50, thin = 2)
  )

  # Custom function that returns treatment means by visit
  custom_means <- function(data, vars, ...) {
    visits <- unique(data[[vars$visit]])
    result <- list()

    for (v in visits) {
      visit_data <- data[data[[vars$visit]] == v, ]

      for (grp in unique(visit_data[[vars$group]])) {
        grp_data <- visit_data[visit_data[[vars$group]] == grp, ]
        key <- paste0("mean_", gsub(" ", "", as.character(grp)), "_", gsub(" ", "", as.character(v)))
        result[[key]] <- list(
          est = mean(grp_data[[vars$outcome]], na.rm = TRUE),
          se = sd(grp_data[[vars$outcome]], na.rm = TRUE) / sqrt(nrow(grp_data)),
          df = nrow(grp_data) - 1
        )
      }
    }

    result
  }

  ana_obj <- analyse_mi_data(
    data = ADMI,
    vars = vars,
    method = method,
    fun = custom_means
  )

  expect_s3_class(ana_obj, "analysis")
  expect_true(length(ana_obj$results) > 0)
})
