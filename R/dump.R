#' dump subset to file
#'
#' Data uit de analyseset wordt van de opgegeven student geexporteerd naar een csv
#' NA's worden verwijderd.
#' @param df Het databestand met de analyseset
#' @param studentnummer Het studennummer als integer
#' @param dataloc De locatie waar het bestand naar weggeschreven wordt
#' @param suffix Een suffix dat achter de bestandsnaam geplaatst wordt
#' @return een xlsx-bestand met een subset obv 1 studentnummer
#' @export
dump_subset_to_file <- function(df, studentnummer, dataloc = "Exports/Student_Exports/", suffix = NULL){
  INS_Studentnummer <- NULL
    df <- dplyr:: filter(df, INS_Studentnummer==studentnummer)
    df <- df[, colSums(is.na(df)) != nrow(df)]
    df <- t(df)
    current_time <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
    if(!is.null(suffix)){
        filename <- paste("dump_subset_", current_time, "_", suffix, ".xlsx", sep = '')
    } else {
        filename <- paste("dump_subset_", current_time, ".xlsx", sep = '')
    }
    utils::write.csv2(df, paste(Sys.getenv("NETWORK_DIR"), dataloc, filename, sep = ""), col.names=FALSE)
}


#' dump to testfile
#'
#' Data wordt geexporteerd naar een csv. In de bestandsnaam staat een suffix
#' met de datum en tijd.
#' @param df Het databestand met de analyseset
#' @param output De locatie in de output-directory waar het bestand naar weggeschreven wordt.
#' @param dataloc optioneel: Als er geen output directory is opgegeven, kan hier een
#' datalocatie op de netwerkschijf opgegeven worden
#' @param suffix Een suffix dat achter de bestandsnaam geplaatst wordt
#' @return Het databestand wordt weggeschreven als csv-bestand
#' @export
dump_to_testfile <- function(df, output = "XX. Test/", dataloc, suffix = NULL){
    if(missing(dataloc) == TRUE) {
        dataloc <- paste("Output/", Sys.getenv("BRANCH"), "/", output, sep = "")
    }
    current_time <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
    if(!is.null(suffix)){
        filename <- paste("dump_subset_", current_time, "_", suffix, ".csv", sep = '')
    } else {
        filename <- paste("dump_subset_", current_time, ".csv", sep = '')
    }
    utils::write.csv2(df, paste(Sys.getenv("NETWORK_DIR"), dataloc, filename, sep = ""), col.names=FALSE, sep = ";")
}
