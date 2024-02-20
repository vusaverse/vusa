#' Get documentation analysis set
#'
#' Function to load the latest version of the documentation.
#' @param AS_doc Path to the documentation file of the analysisset
#' @export
documentation_get <- function(AS_doc = NULL) {
  if (is.null(AS_doc)) {
    if (!Sys.getenv("AS_DOCUMENTATION") == "") {
      AS_doc <- Sys.getenv("AS_DOCUMENTATION")
    } else {
      stop("system variable for as_doc is missing")
    }
  }

  return(readrds_csv(dataloc = AS_doc))
}
