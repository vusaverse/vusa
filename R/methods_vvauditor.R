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


#' Count more than 1
#'
#' Function to count the number of values greater than 1 in a vector
#' This function is used in the function Check_columns_for_double_rows
#' to count duplicate values.
#' @param x The vector to test
#' @return Number of values greater than 1.
#' @family vector calculations
#' @examples
#' count_more_than_1(c(1, 1, 4))
#'
#' @export
count_more_than_1 <- function(x) {
  y <- sum(x > 1, na.rm = T)
  return(y)
}


#' Drop NA column names
#'
#' Deletes columns whose name is NA or whose name is empty
#'
#' @param x dataframe
#'
#' @return dataframe without columns that are NA
#' @export
drop_na_column_names <- function(x) {
  return(x[!is.na(names(x)) & !(names(x) == "")])
}


#' @importFrom rlang %||%
#'
#' @title Determine a script's filename from within the script itself
#'
#' @description
#' A small set of functions wrapping up the call stack and command line
#' inspection needed to determine a running script's filename from within the
#' script itself.
#'
#' @details
#' `current_filename()` returns the result of `current_source_filename()` if
#' not NULL, otherwise the result of `current_cli_filename()`, which might be
#' NULL.  You should use this wrapper function rather than the more-specific
#' functions unless you have a very specific need.
#'
#' `current_source_filename()` returns the filename from the most recent call
#' to \code{\link[base]{source}} in the current call stack.  From within a
#' sourced script, this is the filename of the script itself.
#'
#' `current_cli_filename()` returns the filename found on the command line
#' invocation of R or Rscript.  This may or may not be the caller's file if
#' there's been an intervening \code{\link[base]{source}}.
#'
#' @return
#' A character vector of length 1 if a script name can be found, otherwise
#' NULL.  No manipulation is done to the filename, so it may be relative or
#' absolute.
#'
#' @examples
#' # Put this in example.R and try running source("example.R")
#' # and `Rscript example.R`
#' filename <- current_filename()
#' message(filename)
#'
#' @export
#'
current_filename <- function() {
  current_source_filename() %||%
    current_cli_filename()
}


#' @rdname current_filename
#' @export
#'
current_source_filename <- function() {
  purrr::pluck(closest_source_frame(), "env", "filename")
}

get_stack <- function(n = NULL, trim = 0) {
  stack <- sys.calls()
  stack <- rev(stack[-length(stack)])
  env <- sys.frames()
  env <- rev(env[-length(env)])
  if (length(stack) == 0) {
    return(NULL)
  }
  purrr::transpose(list(fn_name = lapply(stack, rlang::call_name), env = env))
}

closest_source_frame <- function() {
  purrr::detect(get_stack(), function(x) {
    if (is.null(x$fn_name)) {
      return(FALSE)
    }
    x$fn_name == "source"
  })
}



#' @rdname current_filename
#' @export
#'
current_cli_filename <- function() {
  # Examples:
  #   R --slave --no-save --no-restore -f foo.R
  #   Rscript foo.R becomes R … --file=foo.R
  #
  # The last provided -f/--file argument wins (although all named files must
  # _exist_, only the last is executed), so we search in reverse since it's
  # easier to grab the first item.
  args <- rev(commandArgs(F))
  
  file_index <- grep("^(--file=|-f$)", args)[1]
  
  # No argument found
  if (is.na(file_index)) {
    NULL
  } # -f filename: return the next argument (minus 1 because we're reversed)
  else if (args[file_index] == "-f") {
    args[file_index - 1]
  } # --file=filename: remove the option name
  else {
    sub("^--file=", "", args[file_index])
  }
}


#' Find System Variables in a Package
#'
#' This function scans a specified R package for calls to `Sys.getenv()` and extracts the names of system variables used within the package's functions.
#' It returns a data frame listing the functions that use system variables and the names of those variables.
#'
#' @param package_name The name of the R package to scan for system variable usage.
#'
#' @return A data frame with two columns: `Function` and `System_Variable`. Each row represents a function within the package that uses a system variable, along with the name of the variable.
#'
#' @export
find_sys_variables <- function(package_name) {
  # Load the package
  library(package_name, character.only = TRUE)
  
  # Get the environment of the package
  package_env <- asNamespace(package_name)
  
  # Get a list of all functions in the package
  package_functions <- ls(envir = package_env)
  
  # Initialize an empty list to store system variables
  sys_variables <- list()
  
  # Iterate through each function in the package
  for(func in package_functions) {
    # Get the function object using get
    func_obj <- get(func, envir = package_env)
    
    # Extract the function body as a character vector
    func_body <- capture.output(print(func_obj))
    
    # Search for Sys.getenv() calls in the function body and extract variable names
    matches <- gregexpr("Sys.getenv\\(\"([^\"]+)\"\\)", func_body, perl = TRUE)
    
    if (any(sapply(matches, length) > 0)) {
      # Extract variable names from matches
      var_names <- regmatches(func_body, matches)
      var_names <- gsub("Sys.getenv\\(\"", "", var_names)
      var_names <- gsub("\"\\)", "", var_names)
      
      # Add the function name and variable names to the results
      for (i in 1:length(var_names)) {
        if (var_names[[i]] != "character(0)") {
          sys_variables <- c(sys_variables, list(data.frame(Function = func, System_Variable = var_names[[i]])))
        }
      }
    }
  }
  
  # Combine the list of data frames into a single data frame
  result_df <- do.call(rbind, sys_variables)
  
  return(result_df)
}

#' Identify unused objects in a script file
#'
#' This function reads a script file, identifies objects that are assigned but not used,
#' and returns a list of unused objects. If no unused objects are found, it returns NA.
#'
#' @param filepath A character string specifying the path to the script file.
#'
#' @return A character vector of unused objects in the script, or NA if no unused objects are found.
#'
#' @export
check_unused_objects <- function(filepath) {
  script_lines <- readr::read_lines(filepath)
  script_text <- paste(script_lines, collapse = "\n")
  
  assigned_objects <- stringr::str_extract_all(script_text, "\\b[a-zA-Z_][a-zA-Z0-9_.]*\\b(?=(\\s*<-))") %>% unlist()
  
  used_objects <- character()
  for (obj in assigned_objects) {
    if (!grepl(paste0("\\b", obj, "\\b"), script_text) || sum(grepl(paste0("\\b", obj, "\\b"), script_lines)) > 1) {
      used_objects <- c(used_objects, obj)
    }
  }
  
  unused_objects <- setdiff(assigned_objects, used_objects)
  
  # Check if unused_objects is empty and return NA if true
  if (length(unused_objects) == 0) {
    return(NA)
  } else {
    return(unused_objects)
  }
}
