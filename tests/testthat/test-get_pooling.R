test_that("get_pooling maps method classes to pooling strategies", {
  expect_identical(
    get_pooling(rbmi::method_bayes(n_samples = 5)),
    "rubin"
  )
  expect_identical(
    get_pooling(rbmi::method_approxbayes(n_samples = 5)),
    "rubin"
  )
  expect_identical(
    get_pooling(rbmi::method_condmean(type = "bootstrap", n_samples = 5)),
    "bootstrap"
  )
  expect_identical(
    get_pooling(rbmi::method_condmean(type = "jackknife")),
    "jackknife"
  )
  expect_identical(
    get_pooling(rbmi::method_bmlmi(B = 5, D = 2)),
    "bmlmi"
  )
})

test_that("get_pooling errors informatively on unrecognised input", {
  expect_error(
    get_pooling(list(n_samples = 5)),
    class = "rbmiUtils_error_validation"
  )
  expect_error(
    get_pooling(NULL),
    class = "rbmiUtils_error_validation"
  )
})
