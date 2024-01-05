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

  if(is.null(mapping_table_name) & is.null(mapping_table)){
      stop("no mapping table is given")
  } else if (is.null(mapping_table) & !is.null(mapping_table_name)){
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
  if (class(x) != class(mapping_table$from)){
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
