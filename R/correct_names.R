#' Correct var name
#'
#' This function changes the name of the variable for plots so that prefixes are removed
#' become.
#'
#' @param var_name The variable name
#'
#' @return The corrected variable name
#' @export
correct_var_name <- function(var_name){
  var_name <- gsub("Succes_|School_|_cat|_Cat|egorie|_Percentage|_SAP|Inschrijvingen_", "", var_name)
  var_name <- gsub("_", " ", var_name)
  return(var_name)
}

#' Correct file name
#'
#' This function changes the name of the filename variable
#' (compared to correct_var_name() the underscore is preserved).
#'
#' @param file_name the File name
#'
#' @return The corrected file name
#' @export
correct_file_name <- function(file_name){
  file_name <- gsub("Succes_|School_|_cat|_Cat|egorie|_Percentage|_SAP|Inschrijvingen_", "", file_name)
  ## Because variable content does contain spaces:
  gsub(" ", "_", file_name)
  return(file_name)
}