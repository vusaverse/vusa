% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/script_validation.R
\name{validate_write_files}
\alias{validate_write_files}
\title{Valideer schrijven bestanden}
\usage{
validate_write_files(filepath, export_naar_dataframe = FALSE)
}
\arguments{
\item{filepath}{Het complete filepath van het script}

\item{export_naar_dataframe}{Default: FALSE. Whether to export results to a data frame.}
}
\description{
Test of weggeschreven bestanden aan de documentatie voldoen
}
\seealso{
Other Script validatie: 
\code{\link{compare_input_output}()},
\code{\link{validate_assertions_present}()},
\code{\link{validate_clear_script_objects}()},
\code{\link{validate_introduction}()},
\code{\link{validate_rm_objects}()},
\code{\link{validate_script}()}
}
\concept{Script validatie}
