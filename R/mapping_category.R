#' mapping Category
#'
#' Analogous to 'mapping_translate', takes a csv from the documentation folder
#' 'mapping tables'. The input of this function is a continuous value, which is mapped
#' is moved to a category based on intervals.
#'
#' @param Data The data frame.
#' @param current Current name.
#' @param new New name.
#' @param mapping_table_input Defaults to NULL. If a mapping
#' table must be read.
#' @param mapping_table_name Default NULL. If there is an existing mapping table
#' must be read from the name in the folder
#' @family mapping
#' @return A new data frame with new values based on the 'mapping tables'.
#' @export
mapping_category <- function(Data, current, new, mapping_table_input = NULL, mapping_table_name = NULL) {
  ## Check that mapping_table contains the columns "lower", "upper" and "category". If that
  ## is not so, an error message is given
  if (!is.null(mapping_table_input)) {
    if (!(any(names(mapping_table_input) == "lower") &&
      any(names(mapping_table_input) == "upper") &&
      any(names(mapping_table_input) == "category"))) {
      stop("mapping_table must contain the columns 'lower', 'upper' and 'category'.")
    }
  }

  Data$CURRENT <- Data[, current]
  Data$CURRENT <- unlist(Data$CURRENT) ## To use data from a tibble

  # If a mapping table is included, there is no need to read a csv file anymore.
  if (!is.null(mapping_table_input)) {
    categories <- mapping_table_input
  } else if (!is.null(mapping_table_name)) {
    categories <- utils::read.csv2(paste0(Sys.getenv("MAP_TABLE_DIR"), mapping_table_name))
  } else {
    categories <- utils::read.csv2(paste0(Sys.getenv("MAP_TABLE_DIR"),
      "Mapping_", current, "_", new, ".csv",
      sep = ""
    ))
  }

  boundaries <- append(categories$lower, utils::tail(categories$upper, n = 1))
  Data$TO <- cut(Data$CURRENT, boundaries, categories$category, right = F) # labels=categories$category)

  colnames(Data)[which(names(Data) == "TO")] <- new
  Data$CURRENT <- NULL
  return(Data)
}
