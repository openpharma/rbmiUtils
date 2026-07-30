# Issue #50: `pooling` Argument for `analyse_mi_data()` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let users call `analyse_mi_data()` on an already-imputed (ADMI) dataset without knowing the imputation method, by supplying a `pooling` strategy directly, and export the `get_pooling()` method→pooling mapping helper.

**Architecture:** `get_pooling(method)` becomes the single source of truth for the method-class → pooling-string mapping (new file `R/get_pooling.R`). `analyse_mi_data()` gains a `pooling` argument; when `method` is absent, a stand-in method object is constructed internally from real rbmi constructors so that `rbmi::pool()`'s validation (`length(results) == method$n_samples`, `+1` for bootstrap) passes. `as_analysis2()` keeps its deprecation shell but delegates its mapping to `get_pooling()`.

**Tech Stack:** R package; roxygen2 docs; testthat (edition 3) tests; cli errors with classed conditions; rbmi as the upstream dependency.

## Global Constraints

- Branch: `50-analyse_mi_data-when-the-exact-imputation-method-is-unknown`.
- Backward compatibility: every existing call with `method` supplied must behave byte-identically (existing tests in `tests/testthat/test-analyse_mi_data.R` must pass unchanged).
- All new errors use `cli::cli_abort()` with class `c("rbmiUtils_error_validation", "rbmiUtils_error")` (house style — see `R/analyse_mi_data.R:91-176`).
- Valid pooling values: `"rubin"`, `"bootstrap"`, `"jackknife"`, `"bmlmi"` — but `"bmlmi"` without `method` errors (D not inferable from data).
- `pooling = "rubin"` is the documented recommendation for ADMI of unknown provenance.
- Test-first throughout: write the failing test, watch it fail, implement, watch it pass, commit.
- Run tests with: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/<file>")'`

---

### Task 1: `get_pooling()` exported helper

**Files:**
- Create: `R/get_pooling.R`
- Test: `tests/testthat/test-get_pooling.R`

**Interfaces:**
- Consumes: nothing from this plan (rbmi method objects only).
- Produces: `get_pooling(method)` → one of `"rubin"`, `"bootstrap"`, `"jackknife"`, `"bmlmi"`; classed error `rbmiUtils_error_validation` for unrecognised input. Tasks 2–3 call this exact function.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-get_pooling.R`:

```r
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
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-get_pooling.R")'`
Expected: FAIL with `could not find function "get_pooling"`

- [ ] **Step 3: Write the implementation**

Create `R/get_pooling.R`:

```r
#' Determine the Pooling Strategy for an Imputation Method
#'
#' @description
#' Maps an `rbmi` method object to the pooling strategy that
#' [rbmi::pool()] will apply to the analysis results:
#'
#' * [rbmi::method_bayes()] / [rbmi::method_approxbayes()] -> `"rubin"`
#' * [rbmi::method_condmean()] with `type = "bootstrap"` -> `"bootstrap"`
#' * [rbmi::method_condmean()] with `type = "jackknife"` -> `"jackknife"`
#' * [rbmi::method_bmlmi()] -> `"bmlmi"`
#'
#' This is the single source of truth used internally by
#' [analyse_mi_data()]; it is exported so the mapping is transparent and
#' usable in user code (see issue #50).
#'
#' @param method A method object created by [rbmi::method_bayes()],
#'   [rbmi::method_approxbayes()], [rbmi::method_condmean()], or
#'   [rbmi::method_bmlmi()].
#'
#' @return A length-one character vector: `"rubin"`, `"bootstrap"`,
#'   `"jackknife"`, or `"bmlmi"`.
#'
#' @seealso [analyse_mi_data()]
#'
#' @examples
#' get_pooling(rbmi::method_bayes(n_samples = 10))
#' get_pooling(rbmi::method_condmean(type = "jackknife"))
#'
#' @export
get_pooling <- function(method) {
  if (inherits(method, "bayes") || inherits(method, "approxbayes")) {
    "rubin"
  } else if (inherits(method, "condmean")) {
    if (identical(method$type, "jackknife")) "jackknife" else "bootstrap"
  } else if (inherits(method, "bmlmi")) {
    "bmlmi"
  } else {
    cli::cli_abort(
      "Unrecognized method class: {.cls {class(method)}}. Expected one of: bayes, approxbayes, condmean, bmlmi.",
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }
}
```

- [ ] **Step 4: Document and run tests to verify they pass**

Run: `Rscript -e 'devtools::document(quiet = TRUE)'`
Then: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-get_pooling.R")'`
Expected: PASS (all assertions). `NAMESPACE` should now contain `export(get_pooling)`.

- [ ] **Step 5: Refactor `as_analysis2()` to delegate to `get_pooling()` (DRY)**

In `R/analyse_mi_data.R`, replace the mapping block at lines ~308–319:

```r
  next_class <- if (inherits(method, "bayes") || inherits(method, "approxbayes")) {
    "rubin"
  } else if (inherits(method, "condmean")) {
    if (method$type == "jackknife") "jackknife" else "bootstrap"
  } else if (inherits(method, "bmlmi")) {
    "bmlmi"
  } else {
    cli::cli_abort(
      "Unrecognized method class: {.cls {class(method)}}. Expected one of: bayes, approxbayes, condmean, bmlmi.",
      class = c("rbmiUtils_error_dependency", "rbmiUtils_error")
    )
  }
```

with:

```r
  next_class <- get_pooling(method)
```

Note: the error class changes from `rbmiUtils_error_dependency` to
`rbmiUtils_error_validation` for the unrecognised-method path. Check for a
test asserting the old class: `grep -rn "error_dependency" tests/` — if one
exists, update it to `rbmiUtils_error_validation` in the same commit.

- [ ] **Step 6: Run the full analyse_mi_data test file (regression)**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-analyse_mi_data.R")'`
Expected: PASS, no changes to existing behaviour.

- [ ] **Step 7: Commit**

```bash
git add R/get_pooling.R tests/testthat/test-get_pooling.R R/analyse_mi_data.R NAMESPACE man/get_pooling.Rd
git commit -m "feat: add get_pooling() helper mapping method to pooling strategy (#50)"
```

---

### Task 2: `pooling` argument — validation rules

**Files:**
- Modify: `R/analyse_mi_data.R:82-89` (signature), `R/analyse_mi_data.R:153-159` (method-NULL check)
- Test: `tests/testthat/test-analyse_mi_data.R` (append)

**Interfaces:**
- Consumes: `get_pooling(method)` from Task 1.
- Produces: `analyse_mi_data(data = NULL, vars = NULL, method = NULL, fun = rbmi::ancova, delta = NULL, ..., pooling = NULL)`. After this task, calls supplying only `pooling` pass validation but do not yet produce a working analysis object (that is Task 3). Task 3 relies on a validated length-one `pooling` string being available past the validation block.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-analyse_mi_data.R` (the file's shared setup at the top already defines `ADMI` with mock `IMPID`, `vars`, `method`, and `dummy_analysis_fun` — reuse them):

```r
test_that("Error when both method and pooling are NULL", {
  expect_error(
    analyse_mi_data(data = ADMI, vars = vars, fun = dummy_analysis_fun),
    class = "rbmiUtils_error_validation"
  )
})

test_that("Error when pooling is not a valid strategy", {
  expect_error(
    analyse_mi_data(
      data = ADMI, vars = vars, pooling = "banana", fun = dummy_analysis_fun
    ),
    class = "rbmiUtils_error_validation"
  )
  expect_error(
    analyse_mi_data(
      data = ADMI, vars = vars, pooling = c("rubin", "bootstrap"),
      fun = dummy_analysis_fun
    ),
    class = "rbmiUtils_error_validation"
  )
})

test_that("Error when method and pooling conflict", {
  expect_error(
    analyse_mi_data(
      data = ADMI, vars = vars, method = method, pooling = "jackknife",
      fun = dummy_analysis_fun
    ),
    class = "rbmiUtils_error_validation"
  )
})

test_that("Consistent method and pooling together are accepted", {
  ana_obj <- analyse_mi_data(
    data = ADMI, vars = vars, method = method, pooling = "rubin",
    fun = dummy_analysis_fun
  )
  expect_s3_class(ana_obj, "analysis")
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-analyse_mi_data.R")'`
Expected: the four new tests FAIL (`unused argument (pooling = ...)` /
the both-NULL test fails because the current error message check may pass —
verify it fails for the right reason: currently it DOES error when method is
NULL, so that test will pass already; the others fail).

- [ ] **Step 3: Implement signature and validation**

In `R/analyse_mi_data.R`, change the signature (lines 82–89) to:

```r
analyse_mi_data <- function(
  data = NULL,
  vars = NULL,
  method = NULL,
  fun = rbmi::ancova,
  delta = NULL,
  ...,
  pooling = NULL
) {
```

Replace the method-NULL check (lines 153–159):

```r
  # Check method is provided
  if (is.null(method)) {
    cli::cli_abort(
      "{.arg method} cannot be NULL. Specify a method using {.fn rbmi::method_bayes} or similar.",
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }
```

with:

```r
  # Resolve method / pooling: either may be supplied; both must agree
  valid_pooling <- c("rubin", "bootstrap", "jackknife", "bmlmi")
  if (!is.null(pooling) &&
      (!is.character(pooling) || length(pooling) != 1 ||
         !pooling %in% valid_pooling)) {
    cli::cli_abort(
      "{.arg pooling} must be one of {.val {valid_pooling}}.",
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }
  if (is.null(method) && is.null(pooling)) {
    cli::cli_abort(
      c(
        "Either {.arg method} or {.arg pooling} must be supplied.",
        "i" = "Use {.fn rbmi::method_bayes} or similar if the imputation method is known.",
        "i" = "If it is unknown, specify the pooling strategy directly, e.g. {.code pooling = \"rubin\"}."
      ),
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  }
  if (!is.null(method)) {
    method_pooling <- get_pooling(method)
    if (!is.null(pooling) && !identical(pooling, method_pooling)) {
      cli::cli_abort(
        "{.arg pooling} ({.val {pooling}}) conflicts with {.arg method}, which implies {.val {method_pooling}}. Supply one or the other.",
        class = c("rbmiUtils_error_validation", "rbmiUtils_error")
      )
    }
    pooling <- method_pooling
  }
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-analyse_mi_data.R")'`
Expected: all four new tests PASS. Existing tests PASS except any that
call `analyse_mi_data()` with only `pooling` — none exist yet. (The
pooling-only happy path still fails inside the body at this point — that is
expected and is completed in Task 3; do not add happy-path pooling-only
tests in this task.)

- [ ] **Step 5: Commit**

```bash
git add R/analyse_mi_data.R tests/testthat/test-analyse_mi_data.R
git commit -m "feat: add pooling argument validation to analyse_mi_data() (#50)"
```

---

### Task 3: pooling-only path — stand-in method and `rbmi::pool()` integration

**Files:**
- Modify: `R/analyse_mi_data.R` (imputation-count block, lines ~178–217; add internal helper at end of file)
- Test: `tests/testthat/test-analyse_mi_data.R` (append)

**Interfaces:**
- Consumes: validated `pooling` string from Task 2; `get_pooling()` from Task 1.
- Produces: internal helper `make_standin_method(pooling, n_imps)` returning a real rbmi method object; `analyse_mi_data(..., pooling = "rubin")` returns an `analysis` object that `rbmi::pool()` accepts. This is the deliverable issue #50 asked for.

**Background for the implementer (why stand-ins are required):**
`rbmi:::validate.analysis` asserts `length(x$results) == x$method$n_samples`
for rubin pooling and `== x$method$n_samples + 1` for bootstrap. A `NULL`
method therefore breaks `rbmi::pool()`. The stand-in encodes "a method
consistent with the observed data": rubin → `n_samples = n_imps`;
bootstrap → `n_samples = n_imps - 1` (rbmi convention: first sample is the
full-data estimate, the rest are bootstrap samples); jackknife → no count
assertion. `bmlmi` pooling additionally needs `method$D` (analyses per
imputation), which cannot be inferred from an ADMI dataset — so
pooling-only bmlmi errors with guidance.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-analyse_mi_data.R`:

```r
# Analysis function returning the structure rbmi::pool() expects:
# a named list of parameters, each with est/se/df
pool_ready_fun <- function(data, vars, ...) {
  x <- data[[vars$outcome]]
  list(
    trt = list(
      est = mean(x, na.rm = TRUE),
      se = stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))),
      df = sum(!is.na(x)) - 1
    )
  )
}

test_that("pooling-only path produces an object rbmi::pool() accepts", {
  ana_obj <- analyse_mi_data(
    data = ADMI, vars = vars, pooling = "rubin", fun = pool_ready_fun
  )
  expect_s3_class(ana_obj, "analysis")
  expect_s3_class(ana_obj$results, "rubin")

  pool_obj <- rbmi::pool(ana_obj)
  expect_s3_class(pool_obj, "pool")
  expect_true(is.finite(pool_obj$pars$trt$est))
})

test_that("pooling-only and method paths give identical pooled results", {
  ana_pooling <- analyse_mi_data(
    data = ADMI, vars = vars, pooling = "rubin", fun = pool_ready_fun
  )
  ana_method <- analyse_mi_data(
    data = ADMI, vars = vars, method = method, fun = pool_ready_fun
  )
  expect_identical(
    rbmi::pool(ana_pooling)$pars,
    rbmi::pool(ana_method)$pars
  )
})

test_that("pooling-only path skips the n_samples check", {
  # method expects 5 imputations; take only 3 and use pooling directly
  admi_3 <- ADMI[ADMI$IMPID %in% 1:3, ]
  expect_no_warning(
    ana_obj <- analyse_mi_data(
      data = admi_3, vars = vars, pooling = "rubin", fun = pool_ready_fun
    )
  )
  expect_length(ana_obj$results, 3)
  expect_s3_class(rbmi::pool(ana_obj), "pool")
})

test_that("n_samples check still enforced when method is supplied", {
  admi_3 <- ADMI[ADMI$IMPID %in% 1:3, ]
  expect_error(
    analyse_mi_data(
      data = admi_3, vars = vars, method = method, fun = pool_ready_fun
    ),
    class = "rbmiUtils_error_validation"
  )
})

test_that("pooling = 'bmlmi' without method errors with guidance", {
  expect_error(
    analyse_mi_data(
      data = ADMI, vars = vars, pooling = "bmlmi", fun = pool_ready_fun
    ),
    class = "rbmiUtils_error_validation"
  )
})

test_that("jackknife pooling-only path sets the jackknife results class", {
  ana_obj <- analyse_mi_data(
    data = ADMI, vars = vars, pooling = "jackknife", fun = pool_ready_fun
  )
  expect_s3_class(ana_obj$results, "jackknife")
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-analyse_mi_data.R")'`
Expected: new tests FAIL — the pooling-only calls reach
`as_analysis2()` with `method = NULL` (or fail earlier at the n_expected
block) and error.

- [ ] **Step 3: Implement the stand-in construction**

In `R/analyse_mi_data.R`, the block at lines ~178–217 currently starts:

```r
  # Extract expected number of samples from method using inherits()
  n_expected <- if (inherits(method, "bayes") || inherits(method, "approxbayes")) {
```

Wrap the entire n_expected/filtering block so it only runs when a real
method was supplied, and add the stand-in branch. Immediately BEFORE that
block, the code has computed nothing about IMPIDs yet — the counts are
computed inside it (`unique_impids`, `n_impids`). Restructure to:

```r
  # Count imputations present in the data
  unique_impids <- sort(unique(data$IMPID))
  n_impids <- length(unique_impids)

  if (method_supplied) {
    # Extract expected number of samples from method using inherits()
    n_expected <- if (inherits(method, "bayes") || inherits(method, "approxbayes")) {
      method$n_samples
    } else if (inherits(method, "condmean")) {
      method$n_samples
    } else if (inherits(method, "bmlmi")) {
      method$n_samples
    } else {
      NULL
    }

    # Check and filter IMPID values to match expected sample size
    if (!is.null(n_expected) && n_impids != n_expected) {
      if (n_impids > n_expected) {
        # Filter to first n_expected imputations
        cli::cli_warn(
          "Data contains {n_impids} imputation{?s} but method expects {n_expected}. Using first {n_expected} imputation{?s}."
        )
        keep_impids <- unique_impids[seq_len(n_expected)]
        data <- data[data$IMPID %in% keep_impids, ]

        n_after <- length(unique(data$IMPID))
        if (n_after != n_expected) {
          cli::cli_abort(
            "Internal error: filtering failed. Expected {n_expected} imputations, got {n_after}.",
            class = c("rbmiUtils_error_internal", "rbmiUtils_error")
          )
        }
      } else {
        cli::cli_abort(
          "Data contains {n_impids} imputation{?s} but method expects {n_expected}. Need more imputations.",
          class = c("rbmiUtils_error_validation", "rbmiUtils_error")
        )
      }
    }
  } else {
    # No method supplied: the data defines the number of imputations.
    # Build a stand-in method so downstream rbmi::pool() validation
    # (which asserts result counts against method$n_samples) passes.
    method <- make_standin_method(pooling, n_impids)
  }
```

`method_supplied` must be captured at the TOP of the function body (before
the validation block from Task 2 mutates anything), i.e. insert as the first
line of the function body:

```r
  method_supplied <- !is.null(method)
```

Add the internal helper at the end of `R/analyse_mi_data.R`:

```r
#' Construct a Stand-in Method Object for a Pooling Strategy
#'
#' When [analyse_mi_data()] is called with `pooling` but no `method`, a
#' method object consistent with the observed number of imputations is
#' required so that `rbmi::pool()` validation passes (`rbmi` asserts result
#' counts against `method$n_samples`).
#'
#' @param pooling Length-one character: `"rubin"`, `"bootstrap"`,
#'   `"jackknife"`, or `"bmlmi"`.
#' @param n_imps Number of distinct imputations observed in the data.
#'
#' @return An `rbmi` method object.
#' @keywords internal
#' @noRd
make_standin_method <- function(pooling, n_imps) {
  switch(
    pooling,
    rubin = rbmi::method_bayes(n_samples = n_imps),
    bootstrap = rbmi::method_condmean(type = "bootstrap", n_samples = n_imps - 1),
    jackknife = rbmi::method_condmean(type = "jackknife"),
    bmlmi = cli::cli_abort(
      c(
        "{.val bmlmi} pooling cannot be selected via {.arg pooling} alone.",
        "i" = "BMLMI pooling needs the number of analyses per imputation (D), which cannot be inferred from the data.",
        "i" = "Supply {.code method = rbmi::method_bmlmi(B = , D = )} instead."
      ),
      class = c("rbmiUtils_error_validation", "rbmiUtils_error")
    )
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-analyse_mi_data.R")'`
Expected: ALL tests PASS — the six new ones and every pre-existing one.
If the identical-results test fails on `df`/`se` details, inspect with
`waldo::compare()` — the two paths must produce the same results lists
because the analysis loop does not use `method` at all.

- [ ] **Step 5: Run the full test suite (cross-file regression)**

Run: `Rscript -e 'devtools::test()'`
Expected: 0 failures (integration tests in `test-integration.R` and
consumers like `test-pool_methods.R` unaffected).

- [ ] **Step 6: Commit**

```bash
git add R/analyse_mi_data.R tests/testthat/test-analyse_mi_data.R
git commit -m "feat: support pooling-only analyse_mi_data() via stand-in method (#50)"
```

---

### Task 4: Documentation, NEWS, spec sync, full check

**Files:**
- Modify: `R/analyse_mi_data.R` (roxygen block, lines 1–81), `NEWS.md`, `docs/superpowers/specs/2026-07-30-issues-50-51-design.md`, `_pkgdown.yml` (if it has a reference index)
- Create: `man/get_pooling.Rd` (generated)

**Interfaces:**
- Consumes: the completed API from Tasks 1–3.
- Produces: user-facing documentation; no code behaviour changes.

- [ ] **Step 1: Update `analyse_mi_data()` roxygen**

In the roxygen block of `R/analyse_mi_data.R`:

Change the `@param method` entry (lines ~9–10) to:

```r
#' @param method A method object specifying the imputation method used (e.g., Bayesian imputation).
#'   Created using [rbmi::method_bayes()], [rbmi::method_approxbayes()], or [rbmi::method_condmean()].
#'   Optional if `pooling` is supplied.
```

Add after the `@param delta` entry:

```r
#' @param pooling Optional length-one character giving the pooling strategy
#'   directly: `"rubin"`, `"bootstrap"`, `"jackknife"`, or `"bmlmi"`. Use this
#'   when the imputation method that produced `data` is unknown. Supply either
#'   `method` or `pooling`; if both are given they must agree (see
#'   [get_pooling()]). For an imputed dataset of unknown provenance,
#'   `pooling = "rubin"` is recommended: `"bootstrap"` and `"jackknife"`
#'   pooling assume a specific ordering of samples (original-data estimate
#'   first for bootstrap) that cannot be verified without the method object,
#'   and `"bmlmi"` cannot be selected via `pooling` alone because the number
#'   of analyses per imputation (D) is not inferable from the data. When only
#'   `pooling` is supplied, the number of imputations is taken from the data
#'   and no sample-count check is performed.
```

Add to the `@seealso`/details section (near line 34) a cross-reference:

```r
#' * [get_pooling()] for the method-to-pooling mapping
```

Add an example after the existing example block showing the unknown-method
workflow:

```r
#' # When the imputation method is unknown, specify pooling directly:
#' ana_obj <- analyse_mi_data(
#'   data = ADMI,
#'   vars = vars,
#'   pooling = "rubin",
#'   fun = ancova
#' )
```

(Match the existing example's `\dontrun{}`/executable style — inspect lines
40–80 first and follow whatever wrapper the current example uses.)

- [ ] **Step 2: Add NEWS entry**

In `NEWS.md`, under the development version heading (`# rbmiUtils 0.3.0.9000`
— create the heading at the top if absent):

```markdown
## New features

* `analyse_mi_data()` gains a `pooling` argument so already-imputed (ADMI)
  data can be analysed when the imputation method is unknown. Supply either
  `method` or `pooling`; `pooling = "rubin"` is recommended for data of
  unknown provenance (#50).
* New exported helper `get_pooling()` maps an `rbmi` method object to its
  pooling strategy (`"rubin"`, `"bootstrap"`, `"jackknife"`, `"bmlmi"`) (#50).
```

- [ ] **Step 3: Sync the design spec**

In `docs/superpowers/specs/2026-07-30-issues-50-51-design.md`, Part A "Risk
to verify first" section: replace the final paragraph (the "If `pool()` does
require..." sentence) with a short note recording what implementation found:

```markdown
Verified during implementation: `rbmi:::validate.analysis` asserts
`length(results) == method$n_samples` (rubin) / `n_samples + 1` (bootstrap),
so a stand-in method object is constructed whenever `method` is absent.
`pooling = "bmlmi"` alone errors with guidance, since D (analyses per
imputation) cannot be inferred from the data.
```

- [ ] **Step 4: pkgdown reference index**

Run: `grep -n "reference:" _pkgdown.yml && grep -n "analyse_mi_data" _pkgdown.yml`
If `_pkgdown.yml` has an explicit reference index, add `get_pooling` to the
same section as `analyse_mi_data`. If there is no reference index, skip.

- [ ] **Step 5: Regenerate docs and run the full package check**

Run: `Rscript -e 'devtools::document(quiet = TRUE)'`
Then: `Rscript -e 'devtools::check(args = "--no-manual", quiet = TRUE)'`
Expected: 0 errors, 0 warnings. Pre-existing NOTEs (if any) are acceptable;
new NOTEs caused by this change are not — fix them (most likely candidates:
undocumented arguments, missing `stats::` import for the test helper —
tests do not need imports, but examples do).

- [ ] **Step 6: Commit**

```bash
git add R/analyse_mi_data.R NEWS.md man/ _pkgdown.yml docs/superpowers/specs/2026-07-30-issues-50-51-design.md
git commit -m "docs: document pooling argument and get_pooling(), add NEWS (#50)"
```

---

## Out of Scope (this plan)

- Issue #51 (`tidy_pool_obj()` structured output) — separate plan/PR.
- Posting any GitHub comments (drafts already prepared; maintainer posts).
- Renaming `as_analysis2()` or removing its deprecation shell.
- rbmi core upstreaming (rbmi#589).
