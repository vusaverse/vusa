#' schedule job
#'
#' Schedule a job on the R server by writing file paths to a txt file
#' on the shared network drive.
#' @param file The relative path to the script in the project.
#' @export
schedule_job <- function(file) {
  # Check if the file exists
  if (!file.exists(file)) {
    # If the file does not exist, stop the function and provide an error message
    stop(paste("The file does not exist:", file))
  } else {
    # If the file exists, write the file path and branch to the txt file
    # Use cat with file argument to write directly to the file
    cat(paste(file, "branch: ", current_git_branch()),
      file = Sys.getenv("SCHEDULE_PATH"),
      append = TRUE, sep = "\n"
    )
  }
}
