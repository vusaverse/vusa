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