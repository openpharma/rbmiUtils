test_that("positive test get_imputed_data (from vignettes)", {

  set.seed(122)
  data("ADEFF")

  # Constants
  N_IMPUTATIONS <- 100  # Set to 1000 for final analysis

  # Prepare the data
  ADEFF <- ADEFF %>%
    dplyr::mutate(
      TRT = factor(TRT01P, levels = c("Placebo", "Drug A")),
      USUBJID = factor(USUBJID),
      AVISIT = factor(AVISIT)
    )

  # Define key variables for rbmi
  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  # Define the imputation method (Bayesian)
  method <- rbmi::method_bayes(
    n_samples = N_IMPUTATIONS,
    burn_in = 200,
    burn_between = 5
  )

  # Subset relevant columns
  dat <- ADEFF %>%
    select(USUBJID, STRATA, REGION, REGIONC, TRT, BASE, CHG, AVISIT)

  # Fit the imputation model and perform imputation
  draws_obj <- rbmi::draws(data = dat, vars = vars, method = method, quiet = TRUE)
  impute_obj <- rbmi::impute(draws_obj, references = c("Placebo" = "Placebo", "Drug A" = "Placebo"))

  # Get the imputed data as a data frame
  ADMI <- get_imputed_data(impute_obj)


  # TESTS

  # From the function I think we want to test the following
  # - Rather than an imputation object we have a data.frame
  # - original subject IDs are mapped to the appropriate variable
  # - internal_id column contains the internally created id's
  # - remaining columns are the same as they were in the imputation object

  # class check
  expect_s3_class(ADMI, "data.frame")

  # original subject IDs are in USUBJID column (pattern match against expected format)
  expect_true(
    all(
      grepl("^ID[0-9][0-9][0-9]$", ADMI[, "USUBJID"])
    )
  )

  # check internal_ids
  expect_true(
    all(
      grepl("^new_pt_[0-9]{1,3}$", ADMI[, "internal_id"])
    )
  )

  # we expect the same number of unique internal_ids an USUBJID's
  expect_identical(
    length(unique(ADMI[, "USUBJID"])),
    length(unique(ADMI[, 'internal_id']))
  )

  # Compare the rest of the data
  extracted_dfs <- rbmi::extract_imputed_dfs(impute_obj, idmap = TRUE)

  imputed_dfs2 <- extracted_dfs |>
    purrr::map_dfr(~ .x, .id = "IMPID")

  expect_identical(
    imputed_dfs2[, c("STRATA", "REGION", "REGIONC", "TRT", "BASE", "CHG", "AVISIT")],
    ADMI[, c("STRATA", "REGION", "REGIONC", "TRT", "BASE", "CHG", "AVISIT")]
  )

})

test_that("get_imputed_data errors with incorrect inputs", {

  # Assert mtcars is not the required class
  expect_false(inherits(mtcars, "imputation"))

  expect_error(
    get_imputed_data(mtcars),
    "impute_obj must be of an imputation object outputted from rbmi::impute"
  )

})
