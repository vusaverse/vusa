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

#' Identify columns with .x or .y suffixes
#'
#' This function checks if any column names in a data frame end with .x or .y,
#' which might indicate join mistakes, and returns the names of those columns.
#'
#' @param df A data frame to check.
#' @return A character vector of column names with .x or .y suffixes.
#' @export
#' @examples
#' # Create a data frame with .x and .y suffixes
#' df <- data.frame(a.x = 1:3, b.y = 4:6, c = 7:9)
#' # Identify columns with .x or .y suffixes
#' identify_join_suffixes(df)
identify_join_suffixes <- function(df) {
  # Check if the column names end with .x or .y
  suffixes <- grepl("\\.x$|\\.y$", names(df))
  # Return the names of columns with .x or .y suffixes
  return(names(df)[suffixes])
}
