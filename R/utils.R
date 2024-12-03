#' Get Imputed Data Sets as a data frame
#'
#' This function takes an imputed dataset and a mapping variable to return a dataset
#' with the original IDs mapped back and renamed appropriately.
#'
#' @param impute_obj The imputation object from which the imputed datasets are extracted.
#'
#' @return A data frame with the original subject IDs mapped and renamed.
#' @export
#' @examples
#' # Example usage:
#' # result <- get_imputed_data(impute_obj)
#'
get_imputed_data <- function(impute_obj) {

  # Check class of impute_obj
  if (!inherits(impute_obj, "imputation")) {
    stop("impute_obj must be of an imputation object outputted from rbmi::impute")
  }

  # Extract the `subjid` variable from the `vars` list
  uid <- impute_obj$data$vars$subjid

  # Extract imputed datasets
  imputed_dfs <- rbmi::extract_imputed_dfs(impute_obj, idmap = TRUE)

  # Extract the ID map from the attributes of the first imputed dataset
  # example code from issue https://github.com/insightsengineering/rbmi/issues/382
  idmap <- attributes(imputed_dfs[[1]])$idmap

  # Convert the imputed data list into a data frame, adding an IMPID variable
  imputed_dfs2 <- imputed_dfs |>
    purrr::map_dfr(~ .x, .id = "IMPID")

  # Map original IDs back to the data
  imputed_dfs2$original_id <- idmap[match(imputed_dfs2[[uid]], names(idmap))]

  # Rename to ensure original ID has the correct name
  result <- imputed_dfs2 |>
    dplyr::rename(internal_id = {{ uid }}) |>
    dplyr::rename({{ uid }} := original_id)

  return(result)
}
