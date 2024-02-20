#' Check classes
#'
#' Checks whether classes correspond to those in the naming documentation file.
#'
#' @param x Data frame.
#' @param naming dataframecontaining the column "Type_informatie".
#'
#' @return Either nothing or a warning when the classes do not correspond.
#' @export
check_classes <- function(x, naming) {
  Opleidingen <- NULL

  Differences_count <- as.integer(0)
  Missing_count <- as.integer(0)
  Column_differences <- vector()
  Missing_columns <- vector()


  for (col in naming$Veldnaam[!is.na(naming$Veldnaam)]) {
    if (is.na(naming$Type_informatie[naming$Veldnaam %in% col])) {
      Missing_count <- Missing_count + 1
      Missing_columns <- c(Missing_columns, col)
    } else {
      if (class(Opleidingen[[col]]) == as.character(naming$Type_informatie[naming$Veldnaam %in% col])) {
      } else {
        Differences_count <- Differences_count + 1
        Column_differences <- c(Column_differences, col)
      }
    }
  }
  if (Differences_count > 0) {
    warning(paste(c(
      paste("The classes of these", Differences_count, "columns in the file do not correspond with those in the documentation:\n  ", sep = " "),
      paste(Column_differences, sep = "", collapse = "\n  ")
    )))
  }
  if (Missing_count > 0) {
    warning(paste(c(
      paste("The classes of these", Missing_count, "columns in the file are missing in the documentation:\n  ", sep = " "),
      paste(Missing_columns, sep = "", collapse = "\n  ")
    )))
  }
}
