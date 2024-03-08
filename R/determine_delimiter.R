#' Determine the Delimiter of a File
#'
#' This function attempts to determine the delimiter of a file by inspecting
#' the first few lines. It checks for the presence of commas or semicolons.
#' However, this method may not always be accurate, especially if the data
#' itself contains these characters or if the file uses a different delimiter.
#'
#' @param file_path A character string specifying the path to the file.
#' @return A character string indicating the delimiter used in the file.
#'         Returns "," if commas are detected, ";" if semicolons are detected,
#'         and stops with an error message if neither is detected.
#' @note This method is not foolproof and may not accurately determine the delimiter
#'       for all files, especially if the delimiter is not consistently used or if
#'       the data contains the delimiter characters. This function is intended for
#'       text files containing delimited data and is not suitable for binary data files
#' @examples
#' \dontrun{
#' # Example usage
#' delimiter <- determine_delimiter("path/to/your/file.csv")
#' }
determine_delimiter <- function(file_path) {
  # Read the first few lines of the file to determine the delimiter
  first_lines <- readLines(file_path, n = 5)
  if (any(str_detect(first_lines, ","))) {
    return(",")
  } else if (any(str_detect(first_lines, ";"))) {
    return(";")
  } else {
    stop("Delimiter could not be determined for file: ", file_path)
  }
}
