#' Validate filenames based on a specified separator
#'
#' This function checks if all filenames in a given vector use a specified
#' separator consistently and return FALSE if there's any inconsistency.
#' 
#' @param filenames A character vector of filenames to validate.
#' @param separator A character string specifying the separator to check for.
#'                 Valid options are "hyphen", "underscore", or "space".
#' @return A logical value indicating whether all filenames use the specified
#'         separator consistently. Returns FALSE if any inconsistency is found.
#' @examples
#' # Test case for inconsistent separator usage
#' filenames <- c("file name-1.txt", "file name 2.csv", "file name 3.doc")
#' validate_filenames(filenames, "space") # Should return FALSE
#' 
#' filenames <- c("file-name-1.txt", "file_name-2.csv", "file_name-3.doc")
#' validate_filenames(filenames, "underscore") # Should return TRUE
#' 
#' filenames <- c("file-name-1.txt", "file_name-2.csv", "file name-3.doc")
#' validate_filenames(filenames, "underscore") # Should return FALSE
#' @export
validate_filenames <- function(filenames, separator) {
  # Define valid separators and their corresponding regex patterns
  valid_separators <- c("hyphen", "underscore", "space")
  separator_patterns <- c(".*-.*", ".*_.*", ".*\\s+.*")
  
  # Check if the specified separator is valid
  if (!(separator %in% valid_separators)) {
    stop("Invalid separator specified. Please use 'hyphen', 'underscore', or 'space'.")
  }
  
  # Get the index of the selected separator
  separator_index <- match(separator, valid_separators)
  
  # Check for inconsistent separator usage in filenames
  # Use purrr::map_lgl to check if any filename contains any other separator
  inconsistent <- purrr::map_lgl(
    setdiff(valid_separators, separator),
    ~ any(stringr::str_detect(filenames, stringr::regex(separator_patterns[match(., valid_separators)])))
  )
  
  # If any inconsistency is found, return FALSE
  if (any(inconsistent)) {
    return(FALSE)
  }
  
  # If no inconsistencies are found, return TRUE
  return(TRUE)
}
