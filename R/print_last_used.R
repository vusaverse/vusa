#' print_last_used
#'
#' Prints the data on which the given script was last run succesfully.
#' @param path The filepath
#'
#' @export
print_last_used <- function(path = NULL) {
  ## als het path niet is ingevuld, wordt het document dat in rstudio
  ## open staat gecontroleerd
  if(is.null(path) & check_installed_package("rstudioapi",
                                                    check = TRUE)) {
    path <- rstudioapi::getSourceEditorContext()$path
  }
  ## print de datum van het het laatste output bestand
  output_bestanden <- determine_output_files(path)
  file_loc <- paste(sa_network_dir_get(),
                    "Output/",
                    sa_branch_get(),
                    "/",
                    dplyr::last(output_bestanden),
                    sep = "")
  print_last_modified(file_loc)
}
