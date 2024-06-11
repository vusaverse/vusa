#' Negate `%in%` operator
#'
#' This function is a convenience wrapper to negate the `%in%` operator in R.
#'
#' @param x The value(s) to be checked.
#' @param table The vector to be checked against.
#' @return A logical vector indicating if elements of x are not in table.
#' @examples
#' x <- c(1, 2, 3)
#' table <- c(2, 3, 4)
#' x %notin% table # Returns TRUE, as 1 is not in table
#' @export
`%notin%` <- function(x, table) {
  if (missing(x) || missing(table)) {
    stop("Arguments 'x' and 'table' must be provided.")
  }
  if (!is.vector(table)) {
    stop("'table' must be a vector.")
  }
  return(!x %in% table)
}
