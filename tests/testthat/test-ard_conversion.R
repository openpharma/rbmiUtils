# =============================================================================
# Tests for pool_to_ard() ARD conversion
# =============================================================================

# Helper to build a proper mock pool object matching rbmi internal structure
# (per decision 01-01-D3)
make_mock_pool <- function() {
  pool_obj <- list(
    pars = list(
      trt_Week4 = list(est = -2.5, se = 0.8, ci = c(-4.1, -0.9), pvalue = 0.002),
      lsm_ref_Week4 = list(est = 10.0, se = 0.5, ci = c(9.0, 11.0), pvalue = NA),
      lsm_alt_Week4 = list(est = 7.5, se = 0.6, ci = c(6.3, 8.7), pvalue = NA),
      trt_Week8 = list(est = -1.0, se = 1.2, ci = c(-3.4, 1.4), pvalue = 0.42)
    ),
    conf.level = 0.95,
    alternative = "two.sided",
    N = 100,
    method = "rubin"
  )
  class(pool_obj) <- "pool"
  pool_obj
}


test_that("pool_to_ard returns valid ARD structure", {
  skip_if_not_installed("cards")

  mock_pool <- make_mock_pool()
  ard <- pool_to_ard(mock_pool)

  # Result has class "card"
  expect_s3_class(ard, "card")

  # Result has required columns
  required_cols <- c(
    "group1", "group1_level",
    "group2", "group2_level",
    "group3", "group3_level",
    "variable", "variable_level",
    "stat_name", "stat_label",
    "stat", "warning", "error"
  )
  for (col in required_cols) {
    expect_true(col %in% names(ard), info = paste("Missing column:", col))
  }

  # cards::check_ard_structure() does not error
  expect_no_error(cards::check_ard_structure(ard))
})


test_that("pool_to_ard includes all five statistics per parameter", {
  skip_if_not_installed("cards")

  mock_pool <- make_mock_pool()
  ard <- pool_to_ard(mock_pool)

  expected_stats <- c("estimate", "std.error", "conf.low", "conf.high", "p.value")
  unique_vars <- unique(ard$variable)

  for (v in unique_vars) {
    var_ard <- ard[ard$variable == v, ]
    var_stat_names <- var_ard$stat_name

    # Each variable should have the 5 statistics (plus method)
    for (s in expected_stats) {
      expect_true(
        s %in% var_stat_names,
        info = paste("Variable", v, "missing stat_name:", s)
      )
    }
    # Also has method row
    expect_true("method" %in% var_stat_names,
                info = paste("Variable", v, "missing method row"))
  }
})


test_that("pool_to_ard preserves grouping columns (ARD-03)", {
  skip_if_not_installed("cards")

  mock_pool <- make_mock_pool()
  ard <- pool_to_ard(mock_pool)

  # group1 is always "visit"
  expect_true(all(ard$group1 == "visit"))

  # group2 is always "parameter_type"
  expect_true(all(ard$group2 == "parameter_type"))

  # group3 is always "lsm_type"
  expect_true(all(ard$group3 == "lsm_type"))

  # group1_level values should match visits from tidy_pool_obj
  tidy_df <- tidy_pool_obj(mock_pool)
  expected_visits <- sort(unique(tidy_df$visit))
  ard_visits <- sort(unique(unlist(ard$group1_level)))
  expect_equal(ard_visits, expected_visits)

  # parameter_type values should include both "trt" and "lsm"
  ard_ptypes <- unique(unlist(ard$group2_level))
  expect_true("trt" %in% ard_ptypes)
  expect_true("lsm" %in% ard_ptypes)
})


test_that("pool_to_ard stat column is list column", {
  skip_if_not_installed("cards")

  mock_pool <- make_mock_pool()
  ard <- pool_to_ard(mock_pool)

  # stat must be a list column (Pitfall 2)
  expect_true(is.list(ard$stat))

  # group level columns must be list columns (Pitfall 3)
  expect_true(is.list(ard$group1_level))
  expect_true(is.list(ard$group2_level))
  expect_true(is.list(ard$group3_level))
  expect_true(is.list(ard$variable_level))

  # warning and error must be list columns
  expect_true(is.list(ard$warning))
  expect_true(is.list(ard$error))

  # fmt_fun must be a list column
  expect_true(is.list(ard$fmt_fun))
})


test_that("pool_to_ard handles NA p-values", {
  skip_if_not_installed("cards")

  mock_pool <- make_mock_pool()
  ard <- pool_to_ard(mock_pool)

  # lsm_ref_Week4 has pvalue = NA
  lsm_ref_rows <- ard[ard$variable == "lsm_ref_Week4", ]
  pval_row <- lsm_ref_rows[lsm_ref_rows$stat_name == "p.value", ]

  # stat should contain NA (not NULL, not error)
  expect_true(is.na(pval_row$stat[[1]]))

  # lsm_alt_Week4 also has pvalue = NA
  lsm_alt_rows <- ard[ard$variable == "lsm_alt_Week4", ]
  pval_row2 <- lsm_alt_rows[lsm_alt_rows$stat_name == "p.value", ]
  expect_true(is.na(pval_row2$stat[[1]]))
})


test_that("pool_to_ard errors without cards package", {
  # Mock the internal helper that checks for cards availability
  local_mocked_bindings(
    is_cards_available = function() FALSE
  )

  mock_pool <- make_mock_pool()
  expect_error(
    pool_to_ard(mock_pool),
    class = "rbmiUtils_error_dependency"
  )
})


test_that("pool_to_ard validates input class", {
  skip_if_not_installed("cards")

  # Plain list without pool class
  bad_input <- list(pars = list(), method = "rubin")
  expect_error(
    pool_to_ard(bad_input),
    class = "rbmiUtils_error_validation"
  )

  # Numeric input
  expect_error(
    pool_to_ard(42),
    class = "rbmiUtils_error_validation"
  )

  # NULL input
  expect_error(
    pool_to_ard(NULL),
    class = "rbmiUtils_error_validation"
  )
})


test_that("pool_to_ard numeric values match tidy_pool_obj", {
  skip_if_not_installed("cards")

  mock_pool <- make_mock_pool()
  ard <- pool_to_ard(mock_pool)
  tidy_df <- tidy_pool_obj(mock_pool)

  # For each parameter in tidy_df, check that ARD estimate matches

  for (i in seq_len(nrow(tidy_df))) {
    param <- tidy_df$parameter[i]
    expected_est <- tidy_df$est[i]
    expected_se <- tidy_df$se[i]
    expected_lci <- tidy_df$lci[i]
    expected_uci <- tidy_df$uci[i]

    param_ard <- ard[ard$variable == param, ]

    # Estimate
    est_row <- param_ard[param_ard$stat_name == "estimate", ]
    expect_equal(est_row$stat[[1]], expected_est,
                 info = paste("Estimate mismatch for", param))

    # Std. Error
    se_row <- param_ard[param_ard$stat_name == "std.error", ]
    expect_equal(se_row$stat[[1]], expected_se,
                 info = paste("SE mismatch for", param))

    # Conf. Low
    lci_row <- param_ard[param_ard$stat_name == "conf.low", ]
    expect_equal(lci_row$stat[[1]], expected_lci,
                 info = paste("LCI mismatch for", param))

    # Conf. High
    uci_row <- param_ard[param_ard$stat_name == "conf.high", ]
    expect_equal(uci_row$stat[[1]], expected_uci,
                 info = paste("UCI mismatch for", param))
  }
})


# =============================================================================
# Tests for compute_rubin_diagnostics() internal function
# =============================================================================

test_that("compute_rubin_diagnostics returns correct values for known inputs", {
  # Test case 1: Known values with hand-calculated expected results
  M <- 5L
  ests <- c(1.0, 1.2, 0.8, 1.1, 0.9)
  ses <- c(0.5, 0.6, 0.4, 0.55, 0.45)
  v_com <- 100

  result <- compute_rubin_diagnostics(ests, ses, v_com, M)

  # Should return a named list

  expect_type(result, "list")
  expect_named(result, c("var_w", "var_b", "var_t", "lambda", "riv",
                          "df_adj", "dfcom", "fmi", "re"))

  # Verify intermediates
  expect_equal(result$var_w, 0.255, tolerance = 1e-10)
  expect_equal(result$var_b, 0.025, tolerance = 1e-10)
  expect_equal(result$var_t, 0.285, tolerance = 1e-10)

  # Verify derived quantities
  expect_equal(result$lambda, 0.105263157894737, tolerance = 1e-10)
  expect_equal(result$riv, 0.117647058823529, tolerance = 1e-10)
  expect_equal(result$df_adj, 70.582240254527292, tolerance = 1e-6)
  expect_equal(result$dfcom, 100)
  expect_equal(result$fmi, 0.129582527324379, tolerance = 1e-6)
  expect_equal(result$re, 0.974738192312120, tolerance = 1e-6)
})


test_that("compute_rubin_diagnostics handles lambda=0 (identical estimates)", {
  # Test case 2: All estimates identical -> var_b = 0, lambda = 0
  M <- 5L
  ests <- rep(1.0, 5)
  ses <- c(0.5, 0.6, 0.4, 0.55, 0.45)
  v_com <- 100

  result <- compute_rubin_diagnostics(ests, ses, v_com, M)

  expect_equal(result$var_b, 0)
  expect_equal(result$lambda, 0)
  expect_equal(result$riv, 0)

  # df_adj should be v_obs when lambda=0
  expected_v_obs <- ((v_com + 1) / (v_com + 3)) * v_com * (1 - 0)
  expect_equal(result$df_adj, expected_v_obs, tolerance = 1e-10)

  # fmi should be near 0 (small positive due to df adjustment term)
  expect_true(result$fmi >= 0)
  expect_true(result$fmi < 0.1)

  # re should be near 1
  expect_true(result$re > 0.99)
  expect_true(result$re <= 1.0)
})


test_that("compute_rubin_diagnostics handles v_com=Inf (large-sample)", {
  # Test case 3: v_com = Inf with lambda > 0
  M <- 5L
  ests <- c(1.0, 1.2, 0.8, 1.1, 0.9)
  ses <- c(0.5, 0.6, 0.4, 0.55, 0.45)
  v_com <- Inf

  result <- compute_rubin_diagnostics(ests, ses, v_com, M)

  # df_adj should be v_old = (M-1)/lambda^2 when v_com is Inf and lambda > 0
  expected_v_old <- (M - 1) / result$lambda^2
  expect_equal(result$df_adj, expected_v_old, tolerance = 1e-6)

  # dfcom pass-through
  expect_equal(result$dfcom, Inf)

  # fmi and re should be valid numeric
  expect_true(is.finite(result$fmi))
  expect_true(is.finite(result$re))
  expect_equal(result$fmi, 0.110179294389821, tolerance = 1e-6)
  expect_equal(result$re, 0.978439250749816, tolerance = 1e-6)
})


test_that("compute_rubin_diagnostics handles v_com=Inf and lambda=0", {
  # Edge case: v_com = Inf AND var_b = 0 -> df_adj = Inf
  M <- 5L
  ests <- rep(1.0, 5)
  ses <- c(0.5, 0.6, 0.4, 0.55, 0.45)
  v_com <- Inf

  result <- compute_rubin_diagnostics(ests, ses, v_com, M)

  expect_equal(result$var_b, 0)
  expect_equal(result$lambda, 0)
  expect_true(is.infinite(result$df_adj))
  expect_equal(result$dfcom, Inf)
})


test_that("compute_rubin_diagnostics handles all NA SEs", {
  # Test case 4: All SEs are NA -> all diagnostics should be NA
  M <- 5L
  ests <- c(1.0, 1.2, 0.8, 1.1, 0.9)
  ses <- rep(NA_real_, 5)
  v_com <- 100

  result <- compute_rubin_diagnostics(ests, ses, v_com, M)

  # All values should be NA
  expect_true(is.na(result$var_w))
  expect_true(is.na(result$lambda))
  expect_true(is.na(result$riv))
  expect_true(is.na(result$df_adj))
  expect_true(is.na(result$fmi))
  expect_true(is.na(result$re))

  # dfcom should still be passed through
  expect_equal(result$dfcom, v_com)
})


test_that("compute_rubin_diagnostics: fmi is distinct from lambda", {
  # Test case 5: Verify fmi != lambda (adjusted FMI differs from lambda)
  M <- 5L
  ests <- c(1.0, 1.2, 0.8, 1.1, 0.9)
  ses <- c(0.5, 0.6, 0.4, 0.55, 0.45)
  v_com <- 100

  result <- compute_rubin_diagnostics(ests, ses, v_com, M)

  # fmi and lambda must both be numeric
  expect_true(is.numeric(result$fmi))
  expect_true(is.numeric(result$lambda))

  # They must be different (adjusted FMI accounts for finite df)
  expect_false(isTRUE(all.equal(result$fmi, result$lambda)),
               info = "FMI and lambda should be distinct quantities")

  # fmi should be greater than lambda (adjusted FMI >= lambda in general)
  expect_true(result$fmi > result$lambda)
})


test_that("compute_rubin_diagnostics handles NA v_com", {
  # Edge case: v_com is NA
  M <- 5L
  ests <- c(1.0, 1.2, 0.8, 1.1, 0.9)
  ses <- c(0.5, 0.6, 0.4, 0.55, 0.45)
  v_com <- NA_real_

  result <- compute_rubin_diagnostics(ests, ses, v_com, M)

  # var_w, var_b, var_t, lambda, riv should still be computable
  expect_true(is.finite(result$var_w))
  expect_true(is.finite(result$var_b))
  expect_true(is.finite(result$var_t))
  expect_true(is.finite(result$lambda))
  expect_true(is.finite(result$riv))

  # df_adj, fmi, re should be NA when v_com is NA
  expect_true(is.na(result$df_adj))
  expect_true(is.na(result$fmi))
  expect_true(is.na(result$re))

  # dfcom should be NA (pass-through)
  expect_true(is.na(result$dfcom))
})


# =============================================================================
# Tests for pool_to_ard() enriched ARD with MI diagnostics (Plan 08-02)
# =============================================================================

# Helper to build a mock analysis object compatible with make_mock_pool()
make_mock_analysis <- function(M = 5L) {
  set.seed(42)
  param_names <- c("trt_Week4", "lsm_ref_Week4", "lsm_alt_Week4", "trt_Week8")

  # Base estimates matching pool_obj$pars
  base_ests <- c(-2.5, 10.0, 7.5, -1.0)
  base_ses  <- c(0.8, 0.5, 0.6, 1.2)
  base_dfs  <- c(100, 100, 100, 100)

  results <- lapply(seq_len(M), function(m) {
    imp <- lapply(seq_along(param_names), function(j) {
      list(
        est = base_ests[j] + rnorm(1, sd = base_ses[j] * 0.3),
        se  = base_ses[j] * runif(1, 0.8, 1.2),
        df  = base_dfs[j]
      )
    })
    names(imp) <- param_names
    imp
  })

  class(results) <- c("rubin", "list")

  analysis_obj <- list(
    results = results,
    fun_name = "ancova",
    method = structure(list(n_samples = M), class = "bayes")
  )
  class(analysis_obj) <- c("analysis", "list")
  analysis_obj
}


test_that("pool_to_ard with analysis_obj returns diagnostic stat rows", {
  skip_if_not_installed("cards")

  mock_pool <- make_mock_pool()
  mock_analysis <- make_mock_analysis()
  ard <- pool_to_ard(mock_pool, mock_analysis)

  # Diagnostic stat_names expected per parameter
  diag_stats <- c("fmi", "lambda", "riv", "df.adjusted", "df.complete",
                   "re", "m.imputations")

  unique_vars <- unique(ard$variable)
  for (v in unique_vars) {
    var_ard <- ard[ard$variable == v, ]
    var_stat_names <- var_ard$stat_name

    for (s in diag_stats) {
      expect_true(
        s %in% var_stat_names,
        info = paste("Variable", v, "missing diagnostic stat_name:", s)
      )
    }

    # Verify stat values are numeric (not NA, not NULL) for normal case
    for (s in diag_stats) {
      row <- var_ard[var_ard$stat_name == s, ]
      expect_true(is.numeric(row$stat[[1]]),
                  info = paste("Variable", v, "stat", s, "is not numeric"))
      expect_false(is.na(row$stat[[1]]),
                   info = paste("Variable", v, "stat", s, "is NA"))
    }

    # Verify m.imputations equals M (5)
    m_row <- var_ard[var_ard$stat_name == "m.imputations", ]
    expect_equal(m_row$stat[[1]], 5)

    # Verify stat_label values are populated (not empty strings)
    for (s in diag_stats) {
      row <- var_ard[var_ard$stat_name == s, ]
      expect_true(nchar(row$stat_label) > 0,
                  info = paste("Variable", v, "stat", s, "has empty label"))
    }
  }

  # Verify NO variance component stat_names (locked decision: curated essentials)
  variance_stats <- c("var.within", "var.between", "var.total")
  for (vs in variance_stats) {
    expect_false(
      vs %in% ard$stat_name,
      info = paste("Variance component", vs, "should NOT be in ARD")
    )
  }
})


test_that("pool_to_ard backward compatibility without analysis_obj", {
  skip_if_not_installed("cards")

  mock_pool <- make_mock_pool()

  # Call without analysis_obj (pre-Plan-02 behavior)
  ard <- pool_to_ard(mock_pool)

  # Base stat_names only
  base_stats <- c("estimate", "std.error", "conf.low", "conf.high", "p.value", "method")
  all_stat_names <- unique(ard$stat_name)
  expect_true(all(all_stat_names %in% base_stats),
              info = "Base ARD should only contain base stat_names")

  # Diagnostic stat_names should NOT be present
  diag_stats <- c("fmi", "lambda", "riv", "df.adjusted", "df.complete",
                   "re", "m.imputations")
  for (s in diag_stats) {
    expect_false(s %in% all_stat_names,
                 info = paste("Diagnostic stat", s, "should NOT be in base ARD"))
  }
})


test_that("enriched ARD passes cards::check_ard_structure()", {
  skip_if_not_installed("cards")

  mock_pool <- make_mock_pool()
  mock_analysis <- make_mock_analysis()
  ard <- pool_to_ard(mock_pool, mock_analysis)

  # cards validation

  expect_no_error(cards::check_ard_structure(ard))

  # Class check
  expect_s3_class(ard, "card")

  # All list columns are actually lists
  list_cols <- c("stat", "group1_level", "group2_level", "group3_level",
                 "variable_level", "fmt_fun", "warning", "error")
  for (col in list_cols) {
    expect_true(is.list(ard[[col]]),
                info = paste("Column", col, "should be a list"))
  }
})


test_that("pool_to_ard with non-Rubin pool omits diagnostics with message", {
  skip_if_not_installed("cards")

  # Create a mock pool with jackknife method
  jackknife_pool <- make_mock_pool()
  jackknife_pool$method <- "jackknife"

  mock_analysis <- make_mock_analysis()

  # Should emit informative message about Rubin's rules
  expect_message(
    ard <- pool_to_ard(jackknife_pool, mock_analysis),
    "Rubin"
  )

  # Result should have NO diagnostic stat_names (same as base ARD)
  diag_stats <- c("fmi", "lambda", "riv", "df.adjusted", "df.complete",
                   "re", "m.imputations")
  all_stat_names <- unique(ard$stat_name)
  for (s in diag_stats) {
    expect_false(s %in% all_stat_names,
                 info = paste("Non-Rubin ARD should NOT contain", s))
  }

  # Should still have base stats
  expect_true("estimate" %in% all_stat_names)
  expect_true("method" %in% all_stat_names)
})


test_that("pool_to_ard method row present in enriched ARD", {
  skip_if_not_installed("cards")

  mock_pool <- make_mock_pool()
  mock_analysis <- make_mock_analysis()
  ard <- pool_to_ard(mock_pool, mock_analysis)

  unique_vars <- unique(ard$variable)
  for (v in unique_vars) {
    var_ard <- ard[ard$variable == v, ]

    # method row should exist
    method_rows <- var_ard[var_ard$stat_name == "method", ]
    expect_equal(nrow(method_rows), 1,
                 info = paste("Variable", v, "should have exactly 1 method row"))

    # method value should be "rubin"
    expect_equal(method_rows$stat[[1]], "rubin",
                 info = paste("Variable", v, "method should be 'rubin'"))
  }
})


test_that("pool_to_ard diagnostic values are numerically reasonable", {
  skip_if_not_installed("cards")

  mock_pool <- make_mock_pool()
  mock_analysis <- make_mock_analysis()
  ard <- pool_to_ard(mock_pool, mock_analysis)

  unique_vars <- unique(ard$variable)
  for (v in unique_vars) {
    var_ard <- ard[ard$variable == v, ]

    # FMI between 0 and 1
    fmi_val <- var_ard[var_ard$stat_name == "fmi", ]$stat[[1]]
    expect_true(fmi_val >= 0 && fmi_val <= 1,
                info = paste("FMI for", v, "should be in [0, 1], got", fmi_val))

    # Lambda between 0 and 1
    lambda_val <- var_ard[var_ard$stat_name == "lambda", ]$stat[[1]]
    expect_true(lambda_val >= 0 && lambda_val <= 1,
                info = paste("Lambda for", v, "should be in [0, 1], got", lambda_val))

    # RIV >= 0
    riv_val <- var_ard[var_ard$stat_name == "riv", ]$stat[[1]]
    expect_true(riv_val >= 0,
                info = paste("RIV for", v, "should be >= 0, got", riv_val))

    # RE between 0 and 1
    re_val <- var_ard[var_ard$stat_name == "re", ]$stat[[1]]
    expect_true(re_val >= 0 && re_val <= 1,
                info = paste("RE for", v, "should be in [0, 1], got", re_val))

    # df.adjusted > 0
    df_val <- var_ard[var_ard$stat_name == "df.adjusted", ]$stat[[1]]
    expect_true(df_val > 0,
                info = paste("df.adjusted for", v, "should be > 0, got", df_val))
  }
})


test_that("pool_to_ard validates analysis_obj parameter names match pool_obj", {
  skip_if_not_installed("cards")

  mock_pool <- make_mock_pool()

  # Create a mock analysis with mismatched parameter names
  bad_analysis <- make_mock_analysis()
  for (i in seq_along(bad_analysis$results)) {
    names(bad_analysis$results[[i]]) <- c("wrong_param1", "wrong_param2",
                                           "wrong_param3", "wrong_param4")
  }

  expect_error(
    pool_to_ard(mock_pool, bad_analysis),
    class = "rbmiUtils_error_validation"
  )
})
