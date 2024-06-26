% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods_vvauditor.R
\name{check_entire_naming_convention}
\alias{check_entire_naming_convention}
\title{Check Entire Naming Convention}
\usage{
check_entire_naming_convention(filename, yaml_file, separator)
}
\arguments{
\item{filename}{A character string representing the file name to check.}

\item{yaml_file}{A character string representing the path to the YAML file containing the naming conventions.}

\item{separator}{A character string specifying the separator used in the filename.}
}
\value{
A logical value indicating whether the filename adheres to all the naming conventions specified in the YAML file.
}
\description{
This function validates the filename's separator usage and checks the filename against the naming conventions specified in a YAML file. It returns TRUE if all conventions are met, FALSE otherwise.
}
