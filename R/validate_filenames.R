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


#' Check naming conventions from a YAML file
#'
#' This function reads a YAML file containing naming conventions and checks if a given file name adheres to these conventions.
#'
#' @param x A character string representing the file name to check.
#' @param yaml_file A character string representing the path to the YAML file containing the naming conventions.
#'
#' @return A list of logical values indicating whether each naming convention is met by the file name.
#'
#' @export
check_naming_convention_from_yaml <- function(x, yaml_file) {
  conventions <- yaml::read_yaml(yaml_file)$conventions
  
  file_name <- basename(x)
  parts <- unlist(strsplit(file_name, "[[:space:]]|_"))
  
  results <- list()
  
  for (convention in conventions) {
    check_parts <- min(convention$parts, length(parts))
    part_to_check <- paste(parts[1:check_parts], collapse = "")
    
    result <- stringr::str_detect(part_to_check, convention$pattern)
    
    results[[convention$name]] <- all(result)
  }
  
  return(results)
}

#' Check Entire Naming Convention
#'
#' This function validates the filename's separator usage and checks the filename against the naming conventions specified in a YAML file. It returns TRUE if all conventions are met, FALSE otherwise.
#'
#' @param filename A character string representing the file name to check.
#' @param yaml_file A character string representing the path to the YAML file containing the naming conventions.
#' @param separator A character string specifying the separator used in the filename.
#'
#' @return A logical value indicating whether the filename adheres to all the naming conventions specified in the YAML file.
#' @export
check_entire_naming_convention <- function(filename, yaml_file, separator) {
  # First, validate the filename's separator usage
  if (!validate_filenames(c(filename), separator)) {
    return(FALSE)
  }
  
  # Then, check the filename against the naming conventions
  convention_results <- check_naming_convention_from_yaml(filename, yaml_file)
  
  # Return TRUE if all conventions are met, FALSE otherwise
  return(all(unlist(convention_results)))
}

