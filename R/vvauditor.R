#' Count more than 1
#'
#' Function to count the number of values greater than 1 in a vector
#' This function is used in the function Check_columns_for_double_rows
#' to count duplicate values.
#' @param x The vector to test
#' @return Number of values greater than 1.
#' @family vector calculations
#' @examples
#' count_more_than_1(c(1, 1, 4))
#'
#' @export
count_more_than_1 <- function(x) {
  y <- sum(x > 1, na.rm = T)
  return(y)
}


#' Drop NA column names
#'
#' Deletes columns whose name is NA or whose name is empty
#'
#' @param x dataframe
#'
#' @return dataframe without columns that are NA
#' @export
drop_na_column_names <- function(x) {
  return(x[!is.na(names(x)) & !(names(x) == "")])
}