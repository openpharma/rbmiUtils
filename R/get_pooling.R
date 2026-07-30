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
