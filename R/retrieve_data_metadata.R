#' Check classes
#'
#' Retrieve number of rows and number of columns of a data file without reading it in.
#' Additionally, for csv files, it is possible to retrieve the column names.
#' Todo: make column names work for other file types as well.
#'
#' @param filepath Path to data file
#' @param verbose whether to print number of columns and number of rows.
#'
#' @return Column names
#' @export
retrieve_data_metadata <- function(filepath, verbose = FALSE) {
  column_names <- system(paste0('head -1 "', filepath, '"'), intern = T)
  num_cols <- length(unlist(strsplit(column_names, split = ";")))


  row_number <- system(paste0('find /c /v "" "', filepath, '"'), intern = T)
  num_rows <- as.integer(sub(".*: ", "", row_number[2])) - 1

  if (verbose) {
    print(paste("number of columns:", num_cols))
    print(paste("number of rows:", num_rows))
  }


  return(strsplit(column_names, "\\;|\\,|\t"))
}
