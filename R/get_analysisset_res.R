#' Get the analysis set from the network drive
#'
#' @param columns List of specific columns to read in.
#' @param AS_path Path of the location for the analysis set
#' @return Analysis set 1, 1R = random set, 7 or 7 Extra features from the branch set
#' @export
get_analysisset_res <- function(columns = NULL, AS_path = NULL) {
  if (is.null(AS_path)) {
    if (!any(Sys.getenv(c("OUTPUT_DIR", "RESAS_PATH")) == "")) {
      AS <- paste0(Sys.getenv("OUTPUT_DIR"), Sys.getenv("BRANCH"), "/", Sys.getenv("RESAS_PATH"))
    } else {
      stop("system variables for as_path are missing")
    }
  }

  returnvar <- readr::read_rds(AS) %>%
    dplyr::select(columns)

  return(returnvar)
}
