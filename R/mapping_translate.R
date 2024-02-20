#' Mapping Translate
#'
#' Translates a categorical value into a new categorical value. This function is
#' set up in such a way that it uses the information in the mapping tables and for this
#' all such 'mappings' are documented immediately.
#'
#' @param Data The data frame.
#' @param current Current name.
#' @param new New name.
#' @param KeepOriginal Defaults to TRUE to keep the original value.
#' If FALSE then the values of 'current' are not preserved.
#' @param mapping_table_input Defaults to NULL. If a mapping
#' table must be read.
#' @param mapping_table_name Default NULL. If there is an existing mapping table
#' must be read from the name in the folder
#' @family mapping
#' @return A new data frame with new values based on the 'mapping tables'.
#' @export
mapping_translate <- function(Data, current, new, mapping_table_input = NULL, mapping_table_name = NULL, KeepOriginal = T) {
  Data$CURRENT <- unlist(Data[, current]) ## To use data from a tibble

  ## Check that mapping_table contains the columns "from" and "to". If that
  ## is not so, an error message is given
  if (!is.null(mapping_table_input)) {
    if (!(any(names(mapping_table_input) == "from") &&
      any(names(mapping_table_input) == "to"))) {
      stop("mapping_table must contain the columns 'from' and 'to'.")
    }
  }

  # If a mapping table is included, there is no need to read a csv file anymore.
  if (!is.null(mapping_table_input)) {
    translate <- mapping_table_input
  } else if (!is.null(mapping_table_name)) {
    translate <- utils::read.csv2(paste0(Sys.getenv("MAP_TABLE_DIR"), mapping_table_name), stringsAsFactors = F)
  } else {
    translate <- utils::read.csv2(paste0(Sys.getenv("MAP_TABLE_DIR"),
      "Mapping_", current, "_", new, ".csv",
      sep = ""
    ), stringsAsFactors = F)
  }


  ## Translate to factors with sorted levels as in the csv file
  translate$to <- factor(translate$to, levels = unique(translate$to))
  Data$TO <- translate$to[match(Data$CURRENT, translate$from)]

  if (!KeepOriginal) {
    Data <- Data[, !names(Data) %in% c(current)]
  }
  ## Change factors to characters
  if (is.factor(Data$TO)) {
    Data$TO <- as.character(Data$TO)
  }

  ## If a column already exists with the name of the new variable name
  if (new %in% colnames(Data)) {
    stop("the specified new column name already exists in the specified dataframe")
  }
  colnames(Data)[which(names(Data) == "TO")] <- new
  Data$CURRENT <- NULL

  return(Data)
}
