#' Get the analysis set from the network drive
#'
#' @param columns List of specific columns to read in.
#' @param AS_path Path of the location for the analysis set
#' @return OPLAS analysis set
#' @export
get_analysisset_opl <- function(columns = NULL, AS_path = NULL) {
  if (is.null(AS_path)) {
    if (!any(Sys.getenv(c("OUTPUT_DIR", "OPLAS_PATH")) == "")) {
      AS <- paste0(Sys.getenv("OUTPUT_DIR"), Sys.getenv("OPLAS_PATH"))
    } else {
      stop("system variables for as_path are missing")
    }
  }

  if (is.null(columns)) {
    returnvar <- readr::read_rds(AS)
  } else {
    returnvar <- readr::read_rds(AS) %>%
      dplyr::select(columns)
  }

  returnvar
}

