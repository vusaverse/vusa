#' read Documentation
#'
#' A generic function that can read a documentation file.
#'
#' @param filename The filename
#' @param readr Defaults to FALSE, if TRUE then it uses
#' of 'read_delim'.
#' @param documentation_location Location of documentation files,
#' if NULL system variables will be used.
#' @param ... The other parameters are passed to the function
#'
#' @return Imported data object
#' @export
read_documentation <- function(filename = "", documentation_location = NULL, readr = F, ...) {
  if (is.null(documentation_location)) {
    if (!Sys.getenv("DOCUMENTATION_DIR") == "") {
      documentation_location <- Sys.getenv("DOCUMENTATION_DIR")
    } else {
      stop("system variables for documentation_location are missing")
    }
  }

  if (readr == TRUE) {
    ## If function arguments are specified in the ..., they must be the arguments
    ## overwrite, without generating an error message.
    ## Determine the function arguments for read_delim
    function_args <- list(
      file = paste(Sys.getenv("DOCUMENTATION_DIR"), filename, sep = ""),
      delim = ";",
      escape_double = FALSE,
      locale = readr::locale(
        decimal_mark = ",",
        grouping_mark = "."
      ),
      trim_ws = TRUE,
      col_types = readr::cols(.default = readr::col_guess())
    )
    ## Overwrite the arguments from the function_args with those from the dots (...)
    function_args <- overwrite_dot_arguments(function_args, ...)

    ## Run the function with do.call
    return(do.call(readr::read_delim, function_args))
  } else {
    return(utils::read.csv2(paste(Sys.getenv("DOCUMENTATION_DIR"), filename, sep = ""), stringsAsFactors = F, ...))
  }
}

#' read_import_definitions
#'
#' A function that reads an import definitions file.
#'
#' @param filename The filename of the import definitions file.
#' @param metadata_location Location of the import definitions files,
#' if NULL system variables will be used.
#' @param ... Additional parameters to be passed to the file reading function.
#'
#' @return A data frame containing the import definitions.
#' @export
read_import_definitions <- function(filename = "", metadata_location = NULL, ...) {
  if (is.null(metadata_location)) {
    if (!Sys.getenv("METADATA_IMPORT_DEFINITIONS_DIR") == "") {
      metadata_location <- Sys.getenv("METADATA_IMPORT_DEFINITIONS_DIR")
    } else {
      stop("System variable for metadata_location is missing.")
    }
  }
  
  file_path <- file.path(metadata_location, filename)
  
  # Determine the file reading function based on the file extension
  file_extension <- tools::file_ext(filename)
  if (file_extension == "csv") {
    return(utils::read.csv(file_path, stringsAsFactors = FALSE, ...))
  } else if (file_extension == "txt" || file_extension == "dat") {
    # Assume tab-separated file
    return(utils::read.delim(file_path, stringsAsFactors = FALSE, ...))
  } else {
    stop("Unsupported file format. Only CSV, TXT, and DAT files are supported.")
  }
}


#' Overwrite the arguments with the specified dots (...)
#'
#' This function can be used within a function to ensure that
#' the ... arguments override the specified function arguments. Thereafter
#' function arguments can be used with do.call()
#'
#'
#' @param function_args The function arguments in a named list
#' @param ... the ... arguments passed in the function
#'
#' @return a named list of function arguments
overwrite_dot_arguments <- function(function_args, ...) {
  dots <- list(...)
  return(c(function_args[setdiff(names(function_args), names(dots))], dots))
}
