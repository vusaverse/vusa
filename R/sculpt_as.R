#' sculpt_as
#'
#' Funtion to sculpt the analysis set.
#'
#' @param Analysis_set the Analysis set
#' @param files_list list of files to run
#' @return Analysis set
#' @export
sculpt_as <- function(Analysis_set, files_list) {
  Network_directory <- "G:/DSZ/SA2016/Datasets/"

  for (file in files_list) {
    if (length(file) > 1) {
      Analysis_set <- sculpt_analysis_set(
        Analysis_set,
        file[1],
        file[-1]
      )
    } else {
      Analysis_set <- sculpt_analysis_set(
        Analysis_set,
        file
      )
    }
  }
  return(Analysis_set)
}
