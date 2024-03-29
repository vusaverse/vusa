% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/script_validation.R
\name{validate_clear_script_objects}
\alias{validate_clear_script_objects}
\title{Valideer clear_script_objects}
\usage{
validate_clear_script_objects(filepath, export_naar_dataframe = FALSE)
}
\arguments{
\item{filepath}{Het complete filepath van het script}

\item{export_naar_dataframe}{Default: FALSE. Whether to export results to a data frame.}
}
\description{
Test die checkt of de functie clear_script_objects wordt aangeroepen om de
script specifieke objecten te verwijderen.
}
\seealso{
Other Script validatie: 
\code{\link{compare_input_output}()},
\code{\link{validate_assertions_present}()},
\code{\link{validate_introduction}()},
\code{\link{validate_rm_objects}()},
\code{\link{validate_script}()},
\code{\link{validate_write_files}()}
}
\concept{Script validatie}
