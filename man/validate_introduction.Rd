% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/script_validation.R
\name{validate_introduction}
\alias{validate_introduction}
\title{Valideer introductie}
\usage{
validate_introduction(filepath, export_naar_dataframe = FALSE)
}
\arguments{
\item{filepath}{Het complete filepath van het script}

\item{export_naar_dataframe}{Default: FALSE. Whether to export results to a data frame.}
}
\description{
Functie om script te valideren op volledigheid van de introductie.
}
\seealso{
Other Script validatie: 
\code{\link{compare_input_output}()},
\code{\link{validate_assertions_present}()},
\code{\link{validate_clear_script_objects}()},
\code{\link{validate_rm_objects}()},
\code{\link{validate_script}()},
\code{\link{validate_write_files}()}
}
\concept{Script validatie}
