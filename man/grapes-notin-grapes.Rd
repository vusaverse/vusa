% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/special_operators.R
\name{\%notin\%}
\alias{\%notin\%}
\title{Negate \code{\%in\%} operator}
\usage{
x \%notin\% table
}
\arguments{
\item{x}{The value(s) to be checked.}

\item{table}{The vector to be checked against.}
}
\value{
A logical vector indicating if elements of x are not in table.
}
\description{
This function is a convenience wrapper to negate the \code{\%in\%} operator in R.
}
\examples{
x <- c(1, 2, 3)
table <- c(2, 3, 4)
x \%notin\% table # Returns TRUE, as 1 is not in table
}
