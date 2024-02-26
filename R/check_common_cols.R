#' check for common columns between dataframes.
#'
#' This function checks whether multiple dataframes have overlapping column names.
#'
#' @param ... dataframes
#'
#' @return A character vector containing column names.
#' @export
check_common_cols <- function(...) {
  list_of_dfs <- list(...)
  dfColList <- lapply(list_of_dfs, names)
  commonCols <- Reduce(intersect, dfColList)
  
  return(commonCols)
}
