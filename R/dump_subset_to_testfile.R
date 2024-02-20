#' Dump subset to testfile
#'
#' Data from the analysis set is exported from the specified student to a
#' csv. The file name contains a suffix with the date and time.
#' @param df The data file containing the analysis set
#' @param studentnummer number The student number as integer. A single value or a vector with multiple
#' values.
#' @param output The location in the output directory where the file will be written.
#' @param dataloc optional: If no output directory is specified, a
#' data location on the network drive can be specified
#' @param suffix A suffix that is placed after the file name
#' @return A csv file with a subset based on 1 or more student numbers
#' @export
dump_subset_to_testfile <- function(df, studentnummer = NULL, output = "XX. Test/", dataloc, suffix = NULL) {
  if (missing(dataloc) == TRUE) {
    dataloc <- paste("Output/", Sys.getenv("BRANCH"), "/", output, sep = "")
  }
  if (!is.null(studentnummer)) {
    df <- subset(df, df[, "INS_Studentnummer"] %in% studentnummer)
  }
  current_time <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
  if (!is.null(suffix)) {
    filename <- paste("Dump_subset_", current_time, "_", suffix, ".csv", sep = "")
  } else {
    filename <- paste("Dump_subset_", current_time, ".csv", sep = "")
  }
  utils::write.csv2(df, paste(Sys.getenv("NETWORK_DIR"), dataloc, filename, sep = ""), col.names = FALSE, sep = ";")
}
