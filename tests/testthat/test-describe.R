# tests/testthat/test-describe.R
# Tests for describe_draws() and print.describe_draws()

# ---------------------------------------------------------------------------
# Mock object helpers
# ---------------------------------------------------------------------------

#' Create a minimal mock sample_single object
make_mock_sample <- function(failed = FALSE, n_ids = 5) {
  ids <- paste0("SUBJ", seq_len(n_ids))
  if (failed) {
    structure(
      list(
        failed = TRUE,
        ids = ids,
        ids_samp = ids,
        beta = NA,
        sigma = NA,
        theta = NA
      ),
      class = "sample_single"
    )
  } else {
    sigma_list <- list(grp1 = matrix(1, 2, 2))
    structure(
      list(
        failed = FALSE,
        ids = ids,
        ids_samp = ids,
        beta = c(intercept = 1.0, trt = -0.5),
        sigma = sigma_list,
        theta = c(1.0, 0.5, 0.2)
      ),
      class = "sample_single"
    )
  }
}

#' Create a mock draws object for condmean method
#' @param type "jackknife" or "bootstrap"
#' @param n_resampled Number of resampled draws (total samples = 1 + n_resampled)
#' @param n_failures Number of failed samples
make_mock_draws_condmean <- function(type = "jackknife",
                                     n_resampled = 10,
                                     n_failures = 0) {
  method <- structure(
    list(
      covariance = "us",
      threshold = 0.01,
      same_cov = TRUE,
      REML = TRUE,
      type = type
    ),
    class = c("method", "condmean")
  )

  n_total <- 1 + n_resampled
  samples <- lapply(seq_len(n_total), function(i) make_mock_sample())
  class(samples) <- "sample_list"

  structure(
    list(
      data = NULL,
      method = method,
      samples = samples,
      n_failures = n_failures,
      fit = NULL,
      formula = CHG ~ 1 + TRT + AVISIT + BASE
    ),
    class = c("condmean", "draws", "list")
  )
}

#' Create a mock draws object for bayes method (no stanfit)
#' @param n_samples Number of samples
#' @param n_failures Number of failed samples
make_mock_draws_bayes <- function(n_samples = 100, n_failures = 0) {
  method <- structure(
    list(
      covariance = "us",
      same_cov = FALSE,
      n_samples = n_samples,
      prior_cov = NULL,
      control = list(
        warmup = 200,
        thin = 1,
        chains = 4,
        seed = 123
      )
    ),
    class = c("method", "bayes")
  )

  samples <- lapply(seq_len(n_samples), function(i) make_mock_sample())
  class(samples) <- "sample_list"

  structure(
    list(
      data = NULL,
      method = method,
      samples = samples,
      n_failures = n_failures,
      fit = NULL,
      formula = CHG ~ 1 + TRT + AVISIT + BASE
    ),
    class = c("random", "draws", "list")
  )
}

#' Create a mock draws object for approxbayes method
#' @param n_samples Number of samples
#' @param n_failures Number of failed samples
make_mock_draws_approxbayes <- function(n_samples = 100, n_failures = 0) {
  method <- structure(
    list(
      covariance = "us",
      threshold = 0.01,
      same_cov = TRUE,
      REML = TRUE,
      n_samples = n_samples
    ),
    class = c("method", "approxbayes")
  )

  samples <- lapply(seq_len(n_samples), function(i) make_mock_sample())
  class(samples) <- "sample_list"

  structure(
    list(
      data = NULL,
      method = method,
      samples = samples,
      n_failures = n_failures,
      fit = NULL,
      formula = CHG ~ 1 + TRT + AVISIT + BASE
    ),
    class = c("random", "draws", "list")
  )
}


# ===========================================================================
# Test: Input validation
# ===========================================================================

test_that("describe_draws rejects non-draws objects", {
  expect_error(
    describe_draws(list(a = 1)),
    class = "rbmiUtils_error_type"
  )

  expect_error(
    describe_draws("not a draws"),
    class = "rbmiUtils_error_type"
  )

  expect_error(
    describe_draws(NULL),
    class = "rbmiUtils_error_type"
  )
})


# ===========================================================================
# Test: S3 class of result
# ===========================================================================

test_that("describe_draws returns S3 class c('describe_draws', 'list')", {
  draws_obj <- make_mock_draws_condmean()
  desc <- describe_draws(draws_obj)

  expect_s3_class(desc, "describe_draws")
  expect_true(inherits(desc, "list"))
  expect_equal(class(desc), c("describe_draws", "list"))
})


# ===========================================================================
# Test: Condmean jackknife
# ===========================================================================

test_that("describe_draws extracts correct info for condmean jackknife", {
  draws_obj <- make_mock_draws_condmean(type = "jackknife", n_resampled = 10)
  desc <- describe_draws(draws_obj)

  expect_equal(desc$method, "Conditional Mean (jackknife)")
  expect_equal(desc$method_class, "condmean")
  expect_equal(desc$n_samples, 11)
  expect_equal(desc$n_failures, 0)
  expect_equal(desc$covariance, "us")
  expect_true(desc$same_cov)
  expect_match(desc$formula, "CHG")

  # Condmean-specific fields

  expect_equal(desc$condmean_type, "jackknife")
  expect_equal(desc$n_primary, 1)
  expect_equal(desc$n_resampled, 10)
})


# ===========================================================================
# Test: Condmean bootstrap
# ===========================================================================

test_that("describe_draws returns correct condmean_type for bootstrap", {
  draws_obj <- make_mock_draws_condmean(type = "bootstrap", n_resampled = 5)
  desc <- describe_draws(draws_obj)

  expect_equal(desc$method, "Conditional Mean (bootstrap)")
  expect_equal(desc$condmean_type, "bootstrap")
  expect_equal(desc$n_primary, 1)
  expect_equal(desc$n_resampled, 5)
  expect_equal(desc$n_samples, 6)
})


# ===========================================================================
# Test: Condmean with 0 resampled
# ===========================================================================

test_that("describe_draws handles condmean with 0 resampled samples", {
  draws_obj <- make_mock_draws_condmean(type = "jackknife", n_resampled = 0)
  desc <- describe_draws(draws_obj)

  expect_equal(desc$n_samples, 1)
  expect_equal(desc$n_primary, 1)

  expect_equal(desc$n_resampled, 0)
})


# ===========================================================================
# Test: Bayesian without rstan / without stanfit
# ===========================================================================

test_that("describe_draws returns NULL mcmc when fit is NULL (bayes)", {
  draws_obj <- make_mock_draws_bayes(n_samples = 50)
  desc <- describe_draws(draws_obj)

  expect_equal(desc$method, "Bayesian (MCMC via Stan)")
  expect_equal(desc$method_class, "bayes")
  expect_equal(desc$n_samples, 50)
  expect_null(desc$mcmc)

  # Bayes-specific fields
  expect_equal(desc$bayes_control$warmup, 200)
  expect_equal(desc$bayes_control$thin, 1)
  expect_equal(desc$bayes_control$chains, 4)
  expect_equal(desc$bayes_control$seed, 123)
})


# ===========================================================================
# Test: Bayesian with stanfit (requires rstan)
# ===========================================================================

test_that("describe_draws extracts MCMC diagnostics from stanfit", {
  skip_if_not_installed("rstan")

  # Create a mock stanfit-like object that rstan::summary can handle
  # This test uses a real stanfit if available from a minimal model
  # For now, we create a mock that simulates the describe_draws mcmc extraction


  # Build a mock draws with a fake stanfit to test the extraction logic
  # We test the actual stanfit integration separately
  draws_obj <- make_mock_draws_bayes(n_samples = 50)

  # Simulate a stanfit-like object: we need draws_obj$fit to be "stanfit" class
  # and rstan::summary to work on it. Instead, we test with mocking the path.
  # If rstan is installed, try building a real tiny model
  skip("Stanfit mock requires real Stan compilation - tested via integration")
})


# ===========================================================================
# Test: approxbayes (no MCMC diagnostics)
# ===========================================================================

test_that("describe_draws returns NULL mcmc for approxbayes", {
  draws_obj <- make_mock_draws_approxbayes(n_samples = 80)
  desc <- describe_draws(draws_obj)

  expect_equal(desc$method, "Approximate Bayesian")
  expect_equal(desc$method_class, "approxbayes")
  expect_equal(desc$n_samples, 80)
  expect_null(desc$mcmc)

  # approxbayes should not have condmean-specific fields
  expect_null(desc$condmean_type)
  expect_null(desc$n_primary)
  expect_null(desc$n_resampled)

  # approxbayes should not have bayes_control
  expect_null(desc$bayes_control)
})


# ===========================================================================
# Test: Failures count
# ===========================================================================

test_that("describe_draws reports failure count correctly", {
  draws_obj <- make_mock_draws_condmean(n_resampled = 10, n_failures = 3)
  desc <- describe_draws(draws_obj)

  expect_equal(desc$n_failures, 3)
})


# ===========================================================================
# Test: print method returns invisible(x)
# ===========================================================================

test_that("print.describe_draws returns invisible(x) and produces output", {
  draws_obj <- make_mock_draws_condmean(type = "jackknife", n_resampled = 10)
  desc <- describe_draws(draws_obj)

  # Capture cli output (writes to stderr/message connection)
  out <- capture.output(result <- print(desc), type = "message")

  # Returns invisible(x) - same object
  expect_identical(result, desc)

  # Produces some output
  expect_true(length(out) > 0)
})


# ===========================================================================
# Test: print method shows correct content for condmean
# ===========================================================================

test_that("print.describe_draws shows 1 + N format for condmean", {
  draws_obj <- make_mock_draws_condmean(type = "jackknife", n_resampled = 10)
  desc <- describe_draws(draws_obj)

  out <- paste(capture.output(print(desc), type = "message"), collapse = "\n")

  # Should contain "1 + 10" format
  expect_match(out, "1 \\+ 10", fixed = FALSE)
  expect_match(out, "jackknife", ignore.case = TRUE)
})


# ===========================================================================
# Test: print method shows Bayesian info
# ===========================================================================

test_that("print.describe_draws shows Bayesian method info", {
  draws_obj <- make_mock_draws_bayes(n_samples = 50)
  desc <- describe_draws(draws_obj)

  out <- paste(capture.output(print(desc), type = "message"), collapse = "\n")

  expect_match(out, "Bayesian", ignore.case = TRUE)
  expect_match(out, "50")
})


# ===========================================================================
# Test: Formula extraction
# ===========================================================================

test_that("describe_draws extracts formula as character string", {
  draws_obj <- make_mock_draws_condmean()
  desc <- describe_draws(draws_obj)

  expect_type(desc$formula, "character")
  expect_match(desc$formula, "CHG")
  expect_match(desc$formula, "TRT")
  expect_match(desc$formula, "BASE")
})


# ===========================================================================
# Test: same_cov field
# ===========================================================================

test_that("describe_draws captures same_cov correctly", {
  # condmean has same_cov = TRUE by default in mock
  draws_obj <- make_mock_draws_condmean()
  desc <- describe_draws(draws_obj)
  expect_true(desc$same_cov)

  # bayes mock has same_cov = FALSE
  draws_obj2 <- make_mock_draws_bayes()
  desc2 <- describe_draws(draws_obj2)
  expect_false(desc2$same_cov)
})


# ###########################################################################
# ###########################################################################
# describe_imputation() tests
# ###########################################################################
# ###########################################################################

# ---------------------------------------------------------------------------
# Mock imputation object helper
# ---------------------------------------------------------------------------

#' Create a mock imputation object for testing describe_imputation
#' @param n_subjects Number of subjects
#' @param visits Character vector of visit names
#' @param groups Named list mapping subject IDs to group labels
#' @param missing_pattern Named list mapping subject IDs to logical vectors (by visit)
#' @param method_class Class name for the method ("bayes", "approxbayes", "condmean")
#' @param references Named character vector of reference arm mappings (or NULL)
#' @param n_imputations Number of imputations (M)
#' @param condmean_type "jackknife" or "bootstrap" (only for condmean)
make_mock_impute <- function(n_subjects = 6,
                             visits = c("Visit 1", "Visit 2", "Visit 3"),
                             groups = NULL,
                             missing_pattern = NULL,
                             method_class = "condmean",
                             references = c("TRT" = "PBO", "PBO" = "PBO"),
                             n_imputations = 10,
                             condmean_type = "jackknife") {

  # Derive IDs from custom groups if provided, otherwise generate defaults
  if (!is.null(groups)) {
    ids <- names(groups)
  } else {
    ids <- paste0("SUBJ", seq_len(n_subjects))
  }

  # Default groups: split evenly across two arms
  if (is.null(groups)) {
    half <- n_subjects %/% 2
    groups <- as.list(c(rep("TRT", half), rep("PBO", n_subjects - half)))
    names(groups) <- ids
  }

  # Default missing_pattern: no missing data
  if (is.null(missing_pattern)) {
    missing_pattern <- lapply(ids, function(id) {
      stats::setNames(rep(FALSE, length(visits)), visits)
    })
    names(missing_pattern) <- ids
  }

  # Build mock longdata as environment
  mock_longdata <- new.env(parent = emptyenv())
  mock_longdata$visits <- visits
  mock_longdata$ids <- ids
  mock_longdata$is_missing <- missing_pattern
  mock_longdata$group <- groups

  # Build method with correct class hierarchy
  method <- switch(
    method_class,
    "condmean" = structure(
      list(covariance = "us", same_cov = TRUE, type = condmean_type),
      class = c("method", "condmean")
    ),
    "bayes" = structure(
      list(covariance = "us", same_cov = FALSE),
      class = c("method", "bayes")
    ),
    "approxbayes" = structure(
      list(covariance = "us", same_cov = TRUE),
      class = c("method", "approxbayes")
    )
  )

  # Build imputations list (contents don't matter, just length)
  imputations <- vector("list", n_imputations)

  structure(
    list(
      data = mock_longdata,
      method = method,
      imputations = imputations,
      references = references
    ),
    class = c("imputation", "list")
  )
}


# ===========================================================================
# Test: Input validation for describe_imputation
# ===========================================================================

test_that("describe_imputation rejects non-imputation objects", {
  expect_error(
    describe_imputation(list(a = 1)),
    class = "rbmiUtils_error_type"
  )

  expect_error(
    describe_imputation("not an imputation"),
    class = "rbmiUtils_error_type"
  )

  expect_error(
    describe_imputation(NULL),
    class = "rbmiUtils_error_type"
  )
})


# ===========================================================================
# Test: S3 class of result
# ===========================================================================

test_that("describe_imputation returns S3 class c('describe_imputation', 'list')", {
  imp_obj <- make_mock_impute()
  desc <- describe_imputation(imp_obj)

  expect_s3_class(desc, "describe_imputation")
  expect_true(inherits(desc, "list"))
  expect_equal(class(desc), c("describe_imputation", "list"))
})


# ===========================================================================
# Test: Method name mapping
# ===========================================================================

test_that("describe_imputation maps method class to human-readable name", {
  # condmean jackknife
  imp1 <- make_mock_impute(method_class = "condmean", condmean_type = "jackknife")
  desc1 <- describe_imputation(imp1)
  expect_equal(desc1$method, "Conditional Mean (jackknife)")
  expect_equal(desc1$method_class, "condmean")

  # condmean bootstrap
  imp2 <- make_mock_impute(method_class = "condmean", condmean_type = "bootstrap")
  desc2 <- describe_imputation(imp2)
  expect_equal(desc2$method, "Conditional Mean (bootstrap)")

  # bayes
  imp3 <- make_mock_impute(method_class = "bayes")
  desc3 <- describe_imputation(imp3)
  expect_equal(desc3$method, "Bayesian (MCMC via Stan)")
  expect_equal(desc3$method_class, "bayes")

  # approxbayes
  imp4 <- make_mock_impute(method_class = "approxbayes")
  desc4 <- describe_imputation(imp4)
  expect_equal(desc4$method, "Approximate Bayesian")
  expect_equal(desc4$method_class, "approxbayes")
})


# ===========================================================================
# Test: n_imputations equals length of imputations list
# ===========================================================================

test_that("describe_imputation n_imputations equals length of imputations list", {
  imp_obj <- make_mock_impute(n_imputations = 25)
  desc <- describe_imputation(imp_obj)

  expect_equal(desc$n_imputations, 25)
})


# ===========================================================================
# Test: n_subjects is correct
# ===========================================================================

test_that("describe_imputation counts subjects correctly", {
  imp_obj <- make_mock_impute(n_subjects = 8)
  desc <- describe_imputation(imp_obj)

  expect_equal(desc$n_subjects, 8)
})


# ===========================================================================
# Test: visits extracted correctly
# ===========================================================================

test_that("describe_imputation extracts visits", {
  visits <- c("Baseline", "Week 4", "Week 8", "Week 12")
  imp_obj <- make_mock_impute(visits = visits)
  desc <- describe_imputation(imp_obj)

  expect_equal(desc$visits, visits)
})


# ===========================================================================
# Test: references extracted from impute_obj
# ===========================================================================

test_that("describe_imputation extracts references", {
  refs <- c("TRT" = "PBO", "PBO" = "PBO")
  imp_obj <- make_mock_impute(references = refs)
  desc <- describe_imputation(imp_obj)

  expect_equal(desc$references, refs)
})


test_that("describe_imputation handles NULL references gracefully", {
  imp_obj <- make_mock_impute(references = NULL)
  desc <- describe_imputation(imp_obj)

  expect_null(desc$references)
})


# ===========================================================================
# Test: Missingness structure has required columns
# ===========================================================================

test_that("describe_imputation missingness has required columns", {
  imp_obj <- make_mock_impute()
  desc <- describe_imputation(imp_obj)

  expect_s3_class(desc$missingness, "data.frame")
  expect_true(all(c("visit", "group", "n_total", "n_miss", "pct_miss") %in%
    names(desc$missingness)))
})


# ===========================================================================
# Test: Missingness values are correct for known mock data
# ===========================================================================

test_that("describe_imputation computes missingness correctly", {
  ids <- c("S1", "S2", "S3", "S4")
  visits <- c("V1", "V2")
  groups <- list(S1 = "TRT", S2 = "TRT", S3 = "PBO", S4 = "PBO")

  # S1: V1 observed, V2 missing
  # S2: V1 observed, V2 observed
  # S3: V1 missing, V2 missing
  # S4: V1 observed, V2 observed
  missing_pattern <- list(
    S1 = c(V1 = FALSE, V2 = TRUE),
    S2 = c(V1 = FALSE, V2 = FALSE),
    S3 = c(V1 = TRUE, V2 = TRUE),
    S4 = c(V1 = FALSE, V2 = FALSE)
  )

  imp_obj <- make_mock_impute(
    n_subjects = 4,
    visits = visits,
    groups = groups,
    missing_pattern = missing_pattern
  )
  desc <- describe_imputation(imp_obj)

  miss <- desc$missingness

  # TRT group: S1, S2 (n_total = 2)
  # V1: 0 missing, V2: 1 missing (S1)
  trt_v1 <- miss[miss$visit == "V1" & miss$group == "TRT", ]
  expect_equal(trt_v1$n_total, 2)
  expect_equal(trt_v1$n_miss, 0)
  expect_equal(trt_v1$pct_miss, 0.0)

  trt_v2 <- miss[miss$visit == "V2" & miss$group == "TRT", ]
  expect_equal(trt_v2$n_total, 2)
  expect_equal(trt_v2$n_miss, 1)
  expect_equal(trt_v2$pct_miss, 50.0)

  # PBO group: S3, S4 (n_total = 2)
  # V1: 1 missing (S3), V2: 1 missing (S3)
  pbo_v1 <- miss[miss$visit == "V1" & miss$group == "PBO", ]
  expect_equal(pbo_v1$n_total, 2)
  expect_equal(pbo_v1$n_miss, 1)
  expect_equal(pbo_v1$pct_miss, 50.0)

  pbo_v2 <- miss[miss$visit == "V2" & miss$group == "PBO", ]
  expect_equal(pbo_v2$n_total, 2)
  expect_equal(pbo_v2$n_miss, 1)
  expect_equal(pbo_v2$pct_miss, 50.0)
})


# ===========================================================================
# Test: No missing data edge case
# ===========================================================================

test_that("describe_imputation handles no missing data", {
  imp_obj <- make_mock_impute(n_subjects = 4)
  desc <- describe_imputation(imp_obj)

  expect_true(all(desc$missingness$n_miss == 0))
  expect_true(all(desc$missingness$pct_miss == 0.0))
})


# ===========================================================================
# Test: Single visit edge case
# ===========================================================================

test_that("describe_imputation handles single visit", {
  imp_obj <- make_mock_impute(visits = c("Week 1"), n_subjects = 4)
  desc <- describe_imputation(imp_obj)

  # Should have one row per group
  expect_equal(nrow(desc$missingness), 2)
  expect_equal(unique(desc$missingness$visit), "Week 1")
})


# ===========================================================================
# Test: Single group edge case
# ===========================================================================

test_that("describe_imputation handles single group", {
  ids <- c("S1", "S2", "S3")
  groups <- list(S1 = "TRT", S2 = "TRT", S3 = "TRT")
  visits <- c("V1", "V2")
  missing_pattern <- list(
    S1 = c(V1 = FALSE, V2 = FALSE),
    S2 = c(V1 = FALSE, V2 = TRUE),
    S3 = c(V1 = FALSE, V2 = FALSE)
  )

  imp_obj <- make_mock_impute(
    n_subjects = 3,
    visits = visits,
    groups = groups,
    missing_pattern = missing_pattern
  )
  desc <- describe_imputation(imp_obj)

  # Should have one row per visit (only 1 group)
  expect_equal(nrow(desc$missingness), 2)
  expect_equal(unique(desc$missingness$group), "TRT")
})


# ===========================================================================
# Test: print method returns invisible(x) and produces output
# ===========================================================================

test_that("print.describe_imputation returns invisible(x) and produces output", {
  imp_obj <- make_mock_impute()
  desc <- describe_imputation(imp_obj)

  out <- capture.output(result <- print(desc), type = "message")

  expect_identical(result, desc)
  expect_true(length(out) > 0)
})


# ===========================================================================
# Test: print method shows key content
# ===========================================================================

test_that("print.describe_imputation shows method and references", {
  refs <- c("TRT" = "PBO", "PBO" = "PBO")
  imp_obj <- make_mock_impute(references = refs)
  desc <- describe_imputation(imp_obj)

  out <- paste(capture.output(print(desc), type = "message"), collapse = "\n")

  expect_match(out, "Imputation Summary", ignore.case = TRUE)
  expect_match(out, "Conditional Mean", ignore.case = TRUE)
  expect_match(out, "TRT", ignore.case = FALSE)
  expect_match(out, "PBO", ignore.case = FALSE)
})


test_that("print.describe_imputation handles NULL references", {
  imp_obj <- make_mock_impute(references = NULL)
  desc <- describe_imputation(imp_obj)

  out <- paste(capture.output(print(desc), type = "message"), collapse = "\n")

  # Should show "No explicit references" or similar

  expect_match(out, "No explicit references|No references", ignore.case = TRUE)
})
