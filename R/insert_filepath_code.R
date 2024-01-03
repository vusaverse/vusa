#' Insert File Path Code
#'
#' This function prompts the user to select a file and inserts R code for reading the file based on the selected path.
#' The code is inserted into the RStudio editor.
#'
#' @importFrom rstudioapi selectFile insertText
#' @importFrom tools file_ext
#'
#' @examples
#' \dontrun{
#' insert_filepath_code()
#' }
#'
#' @export
insert_filepath_code <- function() {
  # Prompt the user to select a file
  addin_filepath <- rstudioapi::selectFile()

  # Get the file extension
  extension <- tools::file_ext(x = addin_filepath)


  if (grepl(Sys.getenv("NETWORK_DIR"), addin_filepath)) {
    # Remove "Network_directory" from the file path
    newpath <- gsub(Sys.getenv("NETWORK_DIR"), "", addin_filepath)

    if (extension == "csv") {
      # Generate code for reading a CSV file
      filepathcode <- paste0("readrds_csv('", newpath, "')")
      rstudioapi::insertText(filepathcode)
    } else if (extension == "xlsx") {
      # Generate code for reading an Excel file
      filepathcode <- paste0("read_xlsx(paste0(Network_directory, '", newpath, "'))")
      rstudioapi::insertText(filepathcode)
    }

  } else if (grepl(Sys.getenv("DOCUMENTATION_DIR"), addin_filepath)) {
    # Extract the file name from the path
    newpath <- gsub(".*XX. Documentatie/", "", addin_filepath)

    # Generate code for reading a documentation file
    filepathcode <- paste0("read_documentation('", newpath, "')")
    rstudioapi::insertText(filepathcode)
  }

}
