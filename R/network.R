#'SA Network dir set
#'
#'Sets global option determining the current network directory
#'
#'@param branch String containing the name of the branch
#'
#'@export
sa_network_dir_set <- function(branch) {
  options(
    list(
      VUStudentAnalytics.network_dir = branch))
}

#'SA Network dir get
#'
#'Retrieves the global option to determine working network directory
#'
#'@export
sa_network_dir_get <- function() {
  getOption("VUStudentAnalytics.network_dir")
}