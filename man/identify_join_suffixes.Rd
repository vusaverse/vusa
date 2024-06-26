% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods_vvsculptor.R
\name{identify_join_suffixes}
\alias{identify_join_suffixes}
\title{Identify columns with .x or .y suffixes}
\usage{
identify_join_suffixes(df)
}
\arguments{
\item{df}{A data frame to check.}
}
\value{
A character vector of column names with .x or .y suffixes.
}
\description{
This function checks if any column names in a data frame end with .x or .y,
which might indicate join mistakes, and returns the names of those columns.
}
\examples{
# Create a data frame with .x and .y suffixes
df <- data.frame(a.x = 1:3, b.y = 4:6, c = 7:9)
# Identify columns with .x or .y suffixes
identify_join_suffixes(df)
}
