#' Translate colnames documentation
#'
#' Function to translate the column names of a dataset to the columns contained in
#' the documentation file are defined. The empty column names will be removed
#' from the data.
#'
#' @param data A dataframe whose column names need to be translated
#' @param fieldname A column with fieldnames the dataframe will be changed to
#' @param fieldname_export a column with the original names of the dataframe, that wil be changed
#' @param drop_na Defaults to TRUE so that empty column names are dropped.
#'
#' @return a dataframe with translated column names
#' @export
translate_colnames_documentation <- function(data, fieldname, fieldname_export, drop_na = T){
  ## Translate the column names
  colnames(data) <- fieldname[match(colnames(data), fieldname_export)]
  if (drop_na == T) {
    ## Delete the empty columns without column name
    data <- drop_na_column_names(data)
  }
  return(data)
}

#' Wrapper translate colnames documentation
#'
#' Function that wraps the used variabalbes of SA-script around the translate
#' colnames documentation function.
#'
#' @param data A dataframe whose column names need to be translated
#' @param documentatie_df A dataframe with a fielname and fieldname_export column,
#' voor the translations of the colnames.
#' @param drop_na Defaults to TRUE so that empty column names are dropped.
#'
#' @return a dataframe with translated column names
#' @export
wrapper_translate_colnames_documentation <- function(data, documentatie_df, drop_na = T) {
  fieldname <- documentatie_df$Veldnaam
  fieldname_export <- documentatie_df$Veldnaam_export
  translate_colnames_documentation(data, fieldname, fieldname_export, drop_na)
}


#' Translate Tableau colnames
#'
#' Fucntion to translate the columns of a dataset with regards to those in
#' the documentation file.
#' Empty column names are kept, for Tableau purposes
#' @param dataframe A dataframe for which column names need to be translated.

#' @param keep_other_colnames Default TRUE, whether to keep empty columns names.
#' @param fieldname A column with fieldnames the dataframe will be changed to
#' @param fieldname_export a column with the original names of the dataframe, that wil be changed
#' @return a dataframe with translated column names
#' @export
tableau_colnames <- function(dataframe, fieldname, fieldname_export, keep_other_colnames = T)
{

  if (keep_other_colnames == T) {
    index_check <- which(names(dataframe) %in% fieldname_export)
    for (col in index_check) {
      colnames(dataframe)[col] <- fieldname[match(colnames(dataframe)[col], fieldname_export)]
    }
  } else {
    colnames(dataframe) <- fieldname[match(colnames(dataframe),
                                                    fieldname_export)]
  }
  return(dataframe)
}
