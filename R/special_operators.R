#' Negate `%in%` operator
#'
#' This operator negates the `%in%` operator in R. It checks if an element is not present in a vector.
#'
#' @param ... Other arguments passed to the function.
#' @return A logical value indicating if the element is not present in the vector.
#' @examples
#' 1 %notin% seq(5)  # FALSE
#' 6 %notin% seq(5)  # TRUE
#' @export
`%notin%` <- Negate(`%in%`)