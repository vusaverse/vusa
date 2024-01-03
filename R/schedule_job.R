#' schedule job
#'
#' Schedule a job on the R server by writing file paths to a txt file
#' on the shared network drive.
#' @param file The relative path to the script in the project.
#' @export
schedule_job <- function(file) {
    write(x      = paste(file, "branch: ", vvcommander::current_git_branch()),
          file   = paste0(sa_network_dir_get(), "Server_wachtrij/Wachtrij.txt"),
          append = TRUE)
}
