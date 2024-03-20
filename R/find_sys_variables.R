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
