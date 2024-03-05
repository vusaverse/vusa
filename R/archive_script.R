#' Archive scripts to the archive repo
#'
#' Function to archive script to a locally cloned archive repository.
#' Both character strings as lists of character vectors are accepted as input.
#'
#' @param File_path File(s) to archive.
#' @param Archive_path Name of the path to your used Archive. Use in format: "../archive_example".
#' If NULL, system variables will be used.
#' @export
#' @examples archive_script("20. Test/Tomer_3170.R")
#' @examples archive_script(c("20. Test/Tomer_3209.R", "20. Test/Tomer_23432.R", "20. Test/Tomer_3179.R"))
archive_script <- function(File_path, Archive_path = NULL) {
  if (is.character(File_path)) {
    File_paths <- as.list(File_path)
  }

  for (file in File_paths) {
    if (!file.exists(file)) {
      message("File not found in current directory")
      next
    } else {
      if (is.null(Archive_path)) {
        message("No input for Archive_path, so looking for system variables")
        if (!Sys.getenv("ARCHIVE_DIR") == "") {
          message("system variables present, so this will be used for Archive_path")
          Archive_path <- Sys.getenv("ARCHIVE_DIR")
        } else {
          stop("system variables for Archive_path is missing")
        }
      }
      if (dir.exists(Archive_path)) {
        new_path <- paste0(Archive_path, "/", file)
        archive_directory <- dirname(new_path)
        if (!dir.exists(archive_directory)) {
          dir.create(archive_directory, recursive = TRUE)
          file.copy(
            from = file,
            to = new_path
          )
          message(paste0("File ", file, " has been archived succesfully"))
          file.remove(file)
          message(paste0("File ", file, " has been removed from the current repository"))
        } else {
          file.copy(
            from = file,
            to = new_path
          )
          message(paste0("File ", file, " has been archived succesfully"))
          file.remove(file)
          message(paste0("File ", file, " has been removed from the current "))
        }
      } else {
        message("Archive repository not found, have you cloned it locally?")
      }
    }
  }
}



#' Archive Current Script
#'
#' Function to archive the current script open in the source pane in RStudio.
#' @export
archive_current_script <- function() {
  # Get the path of the current script open in the source pane
  current_script <- rstudioapi::getSourceEditorContext()$path
  
  # Check if there is a script open in the source pane
  if (current_script == "") {
    message("No script is currently open in the source pane.")
  } else {
    # Get the current working directory
    cwd <- paste0(getwd(), "/")
    
    # Make the path relative to the current working directory
    # Use sub() to replace the current working directory path from the full path of the script
    current_script_relative <- sub(cwd, "", current_script)
    
    # Confirmation step before archiving
    confirmation <- utils::menu(choices = c("Yes", "No"), 
                                title = paste0("Are you sure you want to archive the script: ", current_script_relative, "?"))
    
    if (confirmation == 1) {
      # Archive the script using the relative path
      archive_script(File_path = current_script_relative)
    } else {
      message("The script was not archived.")
    }
  }
}


