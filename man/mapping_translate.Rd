% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods_vvconverter.R
\name{mapping_translate}
\alias{mapping_translate}
\title{Mapping Translate}
\usage{
mapping_translate(
  Data,
  current,
  new,
  mapping_table_input = NULL,
  mapping_table_name = NULL,
  KeepOriginal = T
)
}
\arguments{
\item{Data}{The data frame.}

\item{current}{Current name.}

\item{new}{New name.}

\item{mapping_table_input}{Defaults to NULL. If a mapping
table must be read.}

\item{mapping_table_name}{Default NULL. If there is an existing mapping table
must be read from the name in the folder}

\item{KeepOriginal}{Defaults to TRUE to keep the original value.
If FALSE then the values of 'current' are not preserved.}
}
\value{
A new data frame with new values based on the 'mapping tables'.
}
\description{
Translates a categorical value into a new categorical value. This function is
set up in such a way that it uses the information in the mapping tables and for this
all such 'mappings' are documented immediately.
}
\seealso{
Other mapping: 
\code{\link{mapping_category}()},
\code{\link{mapping_fix}()}
}
\concept{mapping}
