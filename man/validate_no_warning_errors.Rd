% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_proj.R
\name{validate_no_warning_errors}
\alias{validate_no_warning_errors}
\title{Validate a script for warnings and errors}
\usage{
validate_no_warning_errors(filepath, export_to_dataframe = FALSE)
}
\arguments{
\item{filepath}{The complete file path of the script to be tested.}

\item{export_to_dataframe}{A logical value indicating whether to export results to a data frame. Default is FALSE.}
}
\value{
A list with warnings, errors, and messages. If \code{export_to_dataframe} is TRUE, the result is a list with the warning and error messages. Otherwise, a summary of the results is printed to the console.
}
\description{
This function checks if a script produces any warnings or errors.
}
\examples{
\dontrun{
# Validate a script for warnings and errors
validate_no_warning_errors(filepath = "/path/to/script.R")

# Validate a script and export the results to a dataframe
validate_no_warning_errors(filepath = "/path/to/script.R", export_to_dataframe = TRUE)
}

}
