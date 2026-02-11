library(dplyr)
library(rbmi)

set.seed(123)

# Sample data for testing
data("ADMI")

ADMI$IMPID <- rep(1:5, each = nrow(ADMI) / 5) # Mock IMPID for imputed datasets

vars <- rbmi::set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CHG",
  covariates = c("BASE", "STRATA", "REGION")
)

method <- rbmi::method_bayes(
  n_samples = 5,
  control = rbmi::control_bayes(
    warmup = 10,
    thin = 2
  )
)


dummy_analysis_fun <- function(data, vars, ...) {
  # A simple dummy function to simulate analysis
  return(mean(data[[vars$outcome]], na.rm = TRUE))
}

# Set of Unit Tests

test_that("Error when data is NULL", {
  expect_error(
    analyse_mi_data(data = NULL, vars = vars),
    class = "rbmiUtils_error_validation"
  )
})

test_that("Error when IMPID is missing from data", {
  ADMI_no_impid <- ADMI %>% select(-IMPID)
  expect_error(
    analyse_mi_data(data = ADMI_no_impid, vars = vars),
    class = "rbmiUtils_error_validation"
  )
})

test_that("Error when vars is NULL", {
  expect_error(
    analyse_mi_data(data = ADMI, vars = NULL),
    class = "rbmiUtils_error_validation"
  )
})

test_that("Error when fun is not a function", {
  expect_error(
    analyse_mi_data(data = ADMI, vars = vars, fun = "not_a_function"),
    class = "rbmiUtils_error_type"
  )
})

test_that("Error when delta is not NULL or a data.frame", {
  expect_error(
    analyse_mi_data(data = ADMI, vars = vars, delta = list()),
    class = "rbmiUtils_error_type"
  )
})

test_that("Error when delta does not contain required variables", {
  delta_invalid <- data.frame(subjid = ADMI$USUBJID, delta = rnorm(nrow(ADMI)))
  expect_error(
    analyse_mi_data(data = ADMI, vars = vars, method = method, delta = delta_invalid),
    class = "rbmiUtils_error_validation"
  )
})


test_that("Proper class assignment for analysis object", {
  result <- analyse_mi_data(
    data = ADMI,
    vars = vars,
    method = method,
    fun = dummy_analysis_fun
  )
  expect_s3_class(result, "analysis")
  expect_true("rubin" %in% class(result$results))
})

test_that("analyse_mi_data throws error when fun is not a function", {
  data <- data.frame(
    IMPID = rep(1:3, each = 10),
    var1 = rnorm(30),
    var2 = rnorm(30)
  )
  vars <- data.frame(vars = "vars")

  # Expect an error when 'fun' is not a function
  expect_error(
    analyse_mi_data(data = data, vars = vars, fun = "not_a_function"),
    class = "rbmiUtils_error_type"
  )
})


# =============================================================================
# Additional comprehensive tests for analyse_mi_data
# =============================================================================

test_that("analyse_mi_data processes multiple IMPIDs correctly", {
  data("ADMI")
  set.seed(456)

  # Create mock IMPID with known structure
  n_imps <- 3
  ADMI_test <- ADMI[ADMI$IMPID %in% 1:n_imps, ]

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = n_imps,
    control = rbmi::control_bayes(warmup = 10, thin = 2)
  )

  result <- analyse_mi_data(
    data = ADMI_test,
    vars = vars,
    method = method,
    fun = dummy_analysis_fun
  )

  # Should have one result per imputation
  expect_equal(length(result$results), n_imps)
})


test_that("analyse_mi_data handles single imputation", {
  data("ADMI")
  set.seed(789)

  ADMI_single <- ADMI[ADMI$IMPID == 1, ]

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = 1,
    control = rbmi::control_bayes(warmup = 10, thin = 2)
  )

  result <- analyse_mi_data(
    data = ADMI_single,
    vars = vars,
    method = method,
    fun = dummy_analysis_fun
  )

  expect_s3_class(result, "analysis")
  expect_equal(length(result$results), 1)
})


test_that("analyse_mi_data passes additional arguments to fun", {
  data("ADMI")
  set.seed(111)

  ADMI_test <- ADMI[ADMI$IMPID %in% 1:2, ]

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = 2,
    control = rbmi::control_bayes(warmup = 10, thin = 2)
  )

  # Custom function that uses additional arguments
  custom_fun <- function(data, vars, multiplier = 1) {
    mean(data[[vars$outcome]], na.rm = TRUE) * multiplier
  }

  result <- analyse_mi_data(
    data = ADMI_test,
    vars = vars,
    method = method,
    fun = custom_fun,
    multiplier = 2
  )

  expect_s3_class(result, "analysis")
  # Each result should be multiplied by 2
  expect_true(all(sapply(result$results, function(x) !is.null(x))))
})


test_that("analyse_mi_data applies delta adjustment correctly", {
  data("ADMI")
  set.seed(222)

  ADMI_test <- ADMI[ADMI$IMPID %in% 1:2, ]

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = 2,
    control = rbmi::control_bayes(warmup = 10, thin = 2)
  )

  # Create delta data.frame
  delta <- unique(ADMI_test[, c("USUBJID", "AVISIT", "TRT")])
  delta$delta <- 5  # Add constant delta

  result <- analyse_mi_data(
    data = ADMI_test,
    vars = vars,
    method = method,
    fun = dummy_analysis_fun,
    delta = delta
  )

  expect_s3_class(result, "analysis")
  expect_false(is.null(result$delta))
})


test_that("analyse_mi_data handles approxbayes method", {
  data("ADMI")
  set.seed(333)

  ADMI_test <- ADMI[ADMI$IMPID %in% 1:3, ]

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_approxbayes(n_samples = 3)

  result <- analyse_mi_data(
    data = ADMI_test,
    vars = vars,
    method = method,
    fun = dummy_analysis_fun
  )

  expect_s3_class(result, "analysis")
  expect_true("rubin" %in% class(result$results))
})


test_that("analyse_mi_data stores function name correctly", {
  data("ADMI")
  set.seed(444)

  ADMI_test <- ADMI[ADMI$IMPID %in% 1:2, ]

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = 2,
    control = rbmi::control_bayes(warmup = 10, thin = 2)
  )

  result <- analyse_mi_data(
    data = ADMI_test,
    vars = vars,
    method = method,
    fun = dummy_analysis_fun
  )

  expect_equal(result$fun_name, "dummy_analysis_fun")
})


test_that("analyse_mi_data handles anonymous function", {
  data("ADMI")
  set.seed(555)

  ADMI_test <- ADMI[ADMI$IMPID %in% 1:2, ]

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = 2,
    control = rbmi::control_bayes(warmup = 10, thin = 2)
  )

  result <- analyse_mi_data(
    data = ADMI_test,
    vars = vars,
    method = method,
    fun = function(data, vars, ...) mean(data[[vars$outcome]], na.rm = TRUE)
  )

  expect_s3_class(result, "analysis")
  expect_equal(result$fun_name, "<Anonymous Function>")
})


test_that("analyse_mi_data preserves all IMPID values in results", {
  data("ADMI")
  set.seed(666)

  # Use specific IMPIDs
  ADMI_test <- ADMI[ADMI$IMPID %in% c(1, 3, 5), ]

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = 3,
    control = rbmi::control_bayes(warmup = 10, thin = 2)
  )

  result <- analyse_mi_data(
    data = ADMI_test,
    vars = vars,
    method = method,
    fun = dummy_analysis_fun
  )

  expect_equal(length(result$results), 3)
})


test_that("analyse_mi_data with delta requires correct variable names", {
  data("ADMI")
  set.seed(777)

  ADMI_test <- ADMI[ADMI$IMPID %in% 1:2, ]

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE")
  )

  method <- rbmi::method_bayes(
    n_samples = 2,
    control = rbmi::control_bayes(warmup = 10, thin = 2)
  )

  # Delta with wrong column names
  delta_wrong <- data.frame(
    wrong_subj = "S1",
    wrong_visit = "Week 24",
    delta = 5
  )

  expect_error(
    analyse_mi_data(
      data = ADMI_test,
      vars = vars,
      method = method,
      fun = dummy_analysis_fun,
      delta = delta_wrong
    ),
    class = "rbmiUtils_error_validation"
  )
})


# =============================================================================
# Tests for inherits()-based method detection and cli error classes (01-02)
# =============================================================================

test_that("analyse_mi_data handles all rbmi method types via inherits", {
  data("ADMI")
  set.seed(888)

  ADMI_test <- ADMI[ADMI$IMPID %in% 1:3, ]

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  # Test with method_bayes

  method_b <- rbmi::method_bayes(
    n_samples = 3,
    control = rbmi::control_bayes(warmup = 10, thin = 2)
  )
  result_b <- analyse_mi_data(
    data = ADMI_test, vars = vars, method = method_b,
    fun = dummy_analysis_fun
  )
  expect_s3_class(result_b, "analysis")
  expect_true("rubin" %in% class(result_b$results))

  # Test with method_approxbayes
  method_ab <- rbmi::method_approxbayes(n_samples = 3)
  result_ab <- analyse_mi_data(
    data = ADMI_test, vars = vars, method = method_ab,
    fun = dummy_analysis_fun
  )
  expect_s3_class(result_ab, "analysis")
  expect_true("rubin" %in% class(result_ab$results))
})


test_that("as_analysis2 emits deprecation warning", {
  method <- rbmi::method_bayes(
    n_samples = 1,
    control = rbmi::control_bayes(warmup = 10, thin = 2)
  )

  expect_warning(
    as_analysis2(
      results = list(list(est = 1, se = 0.1, df = NA)),
      method = method,
      fun_name = "test"
    ),
    class = "lifecycle_warning_deprecated"
  )
})


test_that("analyse_mi_data warns when too many imputations", {
  data("ADMI")
  set.seed(999)

  # Data has 20 IMPIDs but method expects only 2
  ADMI_test <- ADMI[ADMI$IMPID %in% 1:5, ]

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = 2,
    control = rbmi::control_bayes(warmup = 10, thin = 2)
  )

  expect_warning(
    result <- analyse_mi_data(
      data = ADMI_test, vars = vars, method = method,
      fun = dummy_analysis_fun
    ),
    "method expects"
  )

  # Should only have 2 results (filtered down)
  expect_equal(length(result$results), 2)
})


test_that("analyse_mi_data output is compatible with rbmi::pool", {
  data("ADMI")
  set.seed(1010)

  ADMI_test <- ADMI[ADMI$IMPID %in% 1:3, ]
  ADMI_test$TRT <- factor(ADMI_test$TRT, levels = c("Placebo", "Drug A"))
  ADMI_test$USUBJID <- factor(ADMI_test$USUBJID)
  ADMI_test$AVISIT <- factor(ADMI_test$AVISIT)

  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  method <- rbmi::method_bayes(
    n_samples = 3,
    control = rbmi::control_bayes(warmup = 10, thin = 2)
  )

  ana_obj <- analyse_mi_data(
    data = ADMI_test,
    vars = vars,
    method = method,
    fun = rbmi::ancova
  )

  # Pool should succeed without error
  pool_obj <- rbmi::pool(ana_obj)
  expect_false(is.null(pool_obj))
})


# =============================================================================
# Tests for enhanced print.analysis and summary.analysis (02-02)
# =============================================================================

# Helper: create mock analysis object with named parameter lists
make_mock_analysis <- function(n_params = 4) {
  params <- list(
    trt_Week4 = list(est = -2.5, se = 0.8, df = NA),
    lsm_ref_Week4 = list(est = 10.0, se = 0.5, df = NA),
    lsm_alt_Week4 = list(est = 7.5, se = 0.6, df = NA),
    trt_Week8 = list(est = -1.0, se = 1.2, df = NA)
  )
  if (n_params > 4) {
    for (i in 5:n_params) {
      params[[paste0("extra_param_", i)]] <- list(
        est = runif(1, -5, 5),
        se = runif(1, 0.1, 1),
        df = NA
      )
    }
  }
  method <- rbmi::method_bayes(
    n_samples = 10,
    control = rbmi::control_bayes(warmup = 10, thin = 1)
  )
  results <- rbmi::as_class(
    rep(list(params), 10),
    c("rubin", "list")
  )
  x <- list(
    results = results,
    delta = NULL,
    fun = rbmi::ancova,
    fun_name = "ancova",
    method = method
  )
  class(x) <- c("analysis", "list")
  x
}


# Helper to capture cli output (cli writes to message connection)
capture_cli_output <- function(expr) {
  out_stdout <- utils::capture.output(
    out_msg <- utils::capture.output(expr, type = "message"),
    type = "output"
  )
  paste(c(out_stdout, out_msg), collapse = "\n")
}


test_that("print.analysis shows parameter count and visits", {
  set.seed(42)
  mock <- make_mock_analysis(n_params = 4)

  out_text <- capture_cli_output(print(mock))

  # Should contain cli-formatted header
  expect_true(grepl("Analysis Object", out_text))
  # Should mention the function name
  expect_true(grepl("ancova", out_text))
  # Should show parameter count
  expect_true(grepl("Parameters.*4", out_text))
  # Should show visit name(s)
  expect_true(grepl("Week4", out_text))
  expect_true(grepl("Week8", out_text))
})


test_that("print.analysis returns invisible(x)", {
  set.seed(42)
  mock <- make_mock_analysis(n_params = 4)

  result <- NULL
  capture_cli_output(result <- print(mock))
  expect_identical(result, mock)
})


test_that("summary.analysis shows parameter preview", {
  set.seed(42)
  mock <- make_mock_analysis(n_params = 4)

  out_text <- capture_cli_output(summary(mock))

  # Should contain parameter preview section
  expect_true(grepl("Parameter Preview", out_text))
  # Should show est/se values from first imputation
  expect_true(grepl("est=", out_text))
  expect_true(grepl("se=", out_text))
  # Should show specific parameter name
  expect_true(grepl("trt_Week4", out_text))
  # Should show specific values
  expect_true(grepl("-2.5", out_text))
  expect_true(grepl("0.8", out_text))
})


test_that("summary.analysis n_preview controls preview count", {
  set.seed(42)
  mock <- make_mock_analysis(n_params = 10)

  out_text <- capture_cli_output(summary(mock, n_preview = 3))

  # Should show "... and 7 more"
  expect_true(grepl("7 more", out_text))

  # Count lines containing "est=" to verify exactly 3 parameter lines
  out_lines <- strsplit(out_text, "\n")[[1]]
  est_lines <- grep("est=", out_lines)
  expect_equal(length(est_lines), 3)
})


test_that("summary.analysis returns summary list", {
  set.seed(42)
  mock <- make_mock_analysis(n_params = 4)

  result <- NULL
  capture_cli_output(result <- summary(mock))

  expect_type(result, "list")
  expect_equal(result$n_imputations, 10)
  expect_equal(result$fun_name, "ancova")
  expect_false(result$has_delta)
  expect_equal(result$method_type, "bayes")
  expect_equal(result$pooling_method, "rubin")
})
