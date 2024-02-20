#' Documentation generate
#'
#' A function to create a record in the data frame based on a variable in a data frame
#' generate documentation file. Category and common values become
#' completed based on the prefix of the variable name and the occurring values
#' in the dates.
#'
#' @param data A data frame containing the variable to be documented
#' @param description The description of the variable
#' @param source The source of the data
#' @param privacy The privacy sensitivity of this variable. Possibilities are:
#' "no" (default), "yes", "Yes, disability" and "Yes, ethnicity".
#' @param extra_info Optionally, extra information can be added
#'
#' @export
documentation_generate <- function(data, description, source, privacy = "no", extra_info = NA) {
  Veldnaam <- Categorie <- Prefix <- value <- NULL
  ## =============================================== ==============================
  ## Get the documentation
  Documentation <- documentation_get()

  ## =============================================== ==============================
  ## Create a link table of categories / prefix
  Category_prefix <- Documentation %>%
    ## Get the prefix from the field name
    dplyr::mutate(Prefix = substr(Veldnaam, 1, 3)) %>%
    ## select only the relevant columns
    dplyr::select(
      Categorie,
      Prefix
    ) %>%
    ## Keep only the unique values
    dplyr::distinct()

  ## =============================================== ==============================
  ## Do the edits

  Documentation_variable <- data %>%
    dplyr::distinct() %>%
    ## Gather the data, with the column name in Field name, and the occurring
    ## values in value
    tidyr::gather(Veldnaam, value) %>%
    ## Make 1 row per field name and put all occurring values in 1 cell.
    dplyr::group_by(Veldnaam) %>%
    dplyr::arrange(value) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::summarise(Common_values = paste(unique(value), collapse = ", ")) %>%
    ## Get the prefix from the field name, and join the category to it
    dplyr::mutate(Prefix = substr(Veldnaam, 1, 3)) %>%
    dplyr::left_join(Category_prefix) %>%
    dplyr::select(-Prefix) %>%
    ## Add the remaining columns.
    dplyr::mutate(
      Omschrijving = description,
      Privacygevoelig = privacy,
      Oorspronkelijke_bron = source,
      Extra_info = extra_info
    )




  ## =============================================== ==============================
  ## Return the documentation of the variable
  return(Documentation_variable)
}
