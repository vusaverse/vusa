#' Get path to vvconverter data
#'
#' vvconverter comes bundled with a number of data files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' vvconverter_data()
#' vvconverter_data("Dates.csv")
vvconverter_data <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "vvconverter"))
  } else {
    datafile <- system.file("extdata", file, package = "vvconverter", mustWork = TRUE)
    dataframe <- utils::read.csv(datafile, sep = ";")
    return(dataframe)
  }
}
