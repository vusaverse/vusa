% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_quality_report.R
\name{create_quality_report}
\alias{create_quality_report}
\title{Create quality report}
\usage{
create_quality_report(
  df,
  ProjectName,
  Network_directory = NULL,
  export_map = "/6. Exports/",
  path = NULL
)
}
\arguments{
\item{df}{Dataframe for which a quality report must be created.}

\item{ProjectName}{name Name of the project, this will become part of the title of the report, among other things.}

\item{Network_directory}{Location of used Network. If Null, system variables will be used}

\item{export_map}{name of the map the exports will be placed in. Default = "/6. Exports/"}

\item{path}{the path the quality report will be placed. if null; the export map
together with the project name will be used}
}
\description{
This function generates a quality report in a previously created folder at the specified location.
The quality report is partly created on the basis of the analysis set documentation,
a description of the variables can therefore only be added when the
variable is documented.
}
