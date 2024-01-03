#' Source clean
#'
#' Source script without saving objct to global environment.
#'
#'
#' @param file path to script.
#' @param ... optional parameters to parse to base::source()
#' @export
src_clean <- function(file, ...) {
    source(file, local = T, ...)
}
