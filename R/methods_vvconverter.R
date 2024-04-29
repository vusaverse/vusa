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

#' mapping fix
#'
#' Translate "wrong" values from a column according to a given mapping
#' table. Only the values that appear in the mapping table in the column "from"
#' are translated to the corresponding "to" value. If a value is not in
#' occurs in the mapping table, the original value is preserved.
#' You can use this function to fix incorrectly encoded values.
#'
#' @param x The vector to be translated.
#' @param mapping_table The mapping table: a data frame with the columns "from"
#' and to". The classes of these columns must be equal to the class of
#' the specified "x".
#' @param mapping_table_name The name of the mapping table in folder Mapping Tables fix/ without .csv
#' @param merge_original If TRUE, original values are also returned
#' if they cannot be mapped. if FALSE only the mapped values will be
#' returned.
#' @family mapping
#' @export
mapping_fix <- function(x, mapping_table = NULL, mapping_table_name = NULL, merge_original = T) {
  from <- to <- NULL
  ## Check that mapping_table contains the columns "from" and "to". If that
  ## is not so, an error message is given
  
  if (is.null(mapping_table_name) & is.null(mapping_table)) {
    stop("no mapping table is given")
  } else if (is.null(mapping_table) & !is.null(mapping_table_name)) {
    mapping_table <- read_documentation(filename = paste0("Mapping Tables fixes/", mapping_table_name, ".csv"))
  }
  
  if (!(any(names(mapping_table) == "from") &&
        any(names(mapping_table) == "to"))) {
    stop("mapping_table must contain the columns 'from' and 'to'.")
  }
  
  if (length(mapping_table$from) != length(unique(mapping_table$from))) {
    stop("There are duplicate values in the column mapping_table$from")
  }
  
  ## Check if the classes of the vectors match
  if (class(x) != class(mapping_table$from)) {
    stop("class of 'x' does not match class of mapping_table$from")
  }
  if (class(x) != class(mapping_table$to)) {
    stop("class of 'x' does not match class of mapping_table$to")
  }
  
  ## First create a tibble containing only the specified vector "x", if the column
  ## "from"
  Mapping <- tibble::tibble(from = x) %>%
    ## Do a left_join from the mapping table, on the column "from"
    dplyr::left_join(mapping_table, by = "from") %>%
    ## Create a new column where "to" is leading, and if it is NA, it becomes
    ## padded with the original value
    dplyr::mutate(to_from_merged = dplyr::coalesce(to, from))
  
  ## Return the new column containing the values contained in the mapping table
  ## have been transformed.
  if (merge_original == T) {
    return(Mapping$to_from_merged)
  } else {
    return(Mapping$to)
  }
}

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

#' Sort column names
#'
#' Function to sort column names alphabetically.
#'
#' @param data Data frame
#'
#' @return New data frame with column names sorted.
#' @export
sort_columnnames <- function(data) {
  data <- data[, order(colnames(data))]
  return(data)
}
