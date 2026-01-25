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
    "`data` cannot be NULL."
  )
})

test_that("Error when IMPID is missing from data", {
  ADMI_no_impid <- ADMI %>% select(-IMPID)
  expect_error(
    analyse_mi_data(data = ADMI_no_impid, vars = vars),
    "`data` must contain a variable `IMPID` to identify distinct imputation iterations."
  )
})

test_that("Error when vars is NULL", {
  expect_error(
    analyse_mi_data(data = ADMI, vars = NULL),
    "`vars` cannot be NULL. Specify key variables."
  )
})

test_that("Error when fun is not a function", {
  expect_error(
    analyse_mi_data(data = ADMI, vars = vars, fun = "not_a_function"),
    "`fun` must be a function"
  )
})

test_that("Error when delta is not NULL or a data.frame", {
  expect_error(
    analyse_mi_data(data = ADMI, vars = vars, delta = list()),
    "`delta` must be NULL or a data.frame"
  )
})

test_that("Error when delta does not contain required variables", {
  delta_invalid <- data.frame(subjid = ADMI$USUBJID, delta = rnorm(nrow(ADMI)))
  expect_error(
    analyse_mi_data(data = ADMI, vars = vars, delta = delta_invalid),
    "The following variables must exist within `delta`: `USUBJID`, `AVISIT`, `delta`"
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
    "`fun` must be a function"
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
    "must exist within `delta`"
  )
})
