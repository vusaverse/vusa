#' Write Tableau friendly dataframe
#'
#' This function takes a dataframe, renames its columns based on a mapping file,
#' applies some transformations, and saves the result in a specified location.
#'
#' @param df A dataframe.
#' @param name A character string specifying the name of the output files.
#' @param output_path A character string specifying the directory where the output files should be saved.
#' @param current_year An integer specifying the current year.
#' @param student_number_col Character string specifying the column name for student numbers. Default is "INS_Studentnummer".
#' @param enrolment_year_col Character string specifying the column name for enrolment years. Default is "INS_Inschrijvingsjaar".
#' @param mapping_file Character string specifying the name of the mapping file. Default is "Documentatie_Tableau_vriendelijke_variabelnamen_UT.csv".
#' @param save_csv Logical indicating whether to save the output as a CSV file. Default is TRUE.
#' @param save_rds Logical indicating whether to save the output as an RDS file. Default is FALSE.
#' @param offset_years An integer specifying the range of years to consider when filtering the 'INS_Inschrijvingsjaar' column. Default is 10.
#' @return The modified dataframe.
#' @examples
#' \dontrun{
#' write_tableau(df,
#'   "output",
#'   "/path/to/directory",
#'   2024,
#'   save_csv = TRUE,
#'   save_rds = FALSE,
#'   offset_years = 10
#' )
#' }
write_tableau <- function(df, name, output_path, current_year, student_number_col = "INS_Studentnummer",
                          enrolment_year_col = "INS_Inschrijvingsjaar",
                          mapping_file = "Documentatie_Tableau_vriendelijke_variabelnamen_UT.csv",
                          save_csv = TRUE, save_rds = FALSE, offset_years = 10) {
  dfTableau_naming <- read_documentation(mapping_file)

  matching_cols <- colnames(df)[colnames(df) %in% dfTableau_naming$Veldnaam_export]

  df <- rename_columns(df, matching_cols, dfTableau_naming)

  if (student_number_col %in% colnames(df)) {
    df <- df %>%
      mutate(!!sym(student_number_col) := hash_var(!!sym(student_number_col)))
  }

  if (enrolment_year_col %in% colnames(df)) {
    df <- df %>%
      filter(!!sym(enrolment_year_col) %in% (current_year - offset_years):current_year)
  }

  write_file(df, name,
    destination = output_path,
    save_csv = save_csv,
    save_rds = save_rds
  )

  return(df)
}


#' Rename columns in a dataframe
#'
#' This function takes a dataframe and a list of matching columns, along with a mapping file,
#' and renames the columns in the dataframe based on the mapping file.
#'
#' @param df A dataframe.
#' @param matching_cols A character vector of column names in df that match with the mapping file.
#' @param dfTableau_naming A dataframe containing the mapping between old and new column names.
#' @return The dataframe with renamed columns.
#' @examples
#' \dontrun{
#' df <- rename_columns(df, matching_cols, dfTableau_naming)
#' }
rename_columns <- function(df, matching_cols, dfTableau_naming) {
  # Create a named vector for renaming
  rename_vec <- setNames(dfTableau_naming$Veldnaam[match(matching_cols, dfTableau_naming$Veldnaam_export)], matching_cols)

  # Loop through matching_cols and rename each column individually
  for (col in matching_cols) {
    new_name <- dfTableau_naming$Veldnaam[which(dfTableau_naming$Veldnaam_export == col)]
    if (!is.na(new_name)) {
      df <- rename(df, !!sym(new_name) := !!sym(col))
    }
  }

  return(df)
}
