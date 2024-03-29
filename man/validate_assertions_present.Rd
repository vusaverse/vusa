% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/script_validation.R
\name{validate_assertions_present}
\alias{validate_assertions_present}
\title{Valideer assertions aanwezig}
\usage{
validate_assertions_present(filepath, export_naar_dataframe = FALSE)
}
\arguments{
\item{filepath}{Het complete filepath van het script dat getest moet worden}

\item{export_naar_dataframe}{Default: FALSE. Whether to export results to a data frame.}
}
\description{
Een test die checkt of assertions worden gebruikt als er csv/xlsx bestanden worden ingelezen.
Hierbij wordt alleen gekeken of er eenmaling assertions aanwezig zijn en niet voor elke
output bestand omdat het voorkomt dat er loops worden gebruikt voor de documentatie.
}
\seealso{
Other Script validatie: 
\code{\link{compare_input_output}()},
\code{\link{validate_clear_script_objects}()},
\code{\link{validate_introduction}()},
\code{\link{validate_rm_objects}()},
\code{\link{validate_script}()},
\code{\link{validate_write_files}()}
}
\concept{Script validatie}
