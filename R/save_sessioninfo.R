#' Save session info
#'
#' Stores session info.
#'
#' @param path The path to search for the file.
#'
#' @return .txt file containing the session info.
#' @export
save_sessioninfo <- function(path) {
  writeLines(utils::capture.output(utils::sessionInfo()), paste(path, "sessionInfo.txt", sep = ""))
}
