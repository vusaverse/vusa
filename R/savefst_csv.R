#' Save FST / CSV
#'
#' Function to write dataframes as fst and csv files on the shared network drive.
#' Default behaviour is to write file as fst. Optional is to write the file as csv as well.
#'
#' @param Object_to_save Het object dat opgeslagen moet worden
#' @param Name_to_save De bestandsnaam dat het bestand moet krijgen (zonder extensie)
#' @param output Datalocatie op de netwerkschijf, afhankelijk van de branch
#' die is ingesteld.startend van "G:/DSZ/SA2016/Datasets/Output/(branch)/"
#' @param dataloc Datalocatie op de netwerkschijf, startend van "G:/DSZ/SA2016/Datasets/"
#' @param fileEncoding De fileencoding voor write.csv2 kan hier meegegeven worden.
#' Als dit niet opgegeven wordt, wordt de standaard fileencoding gebruikt.
#' @param save_csv Standaard FALSE: Er wordt geen CSV weggeschreven. Als TRUE wordt
#' zowel een .rds als een .csv weggeschreven.
#' @param session_info Standaard FALSE. Moet de sessie-info in dezelfde folder
#' weggeschreve worden
#' @param overwrite whether to overwrite existing file when copying to other directory.
#' @export
savefst_csv <- function(Object_to_save, Name_to_save, output = "1. Ingelezen data/",
                        dataloc, fileEncoding = "", save_csv = FALSE,
                        session_info = FALSE,
                        overwrite = TRUE) {
  if (missing(dataloc) == TRUE) {
    dataloc <- paste("Output/", Sys.getenv("BRANCH"),
      "/", output,
      sep = ""
    )
  }


  fst::write_fst(Object_to_save, paste(Sys.getenv("NETWORK_DIR"),
    dataloc, Name_to_save, ".fst",
    sep = ""
  ), compress = 100)
  if (length(dataloc) > 1) {
    for (location in dataloc[-1]) {
      file.copy(
        paste(Sys.getenv("NETWORK_DIR"),
          dataloc[1], Name_to_save, ".fst",
          sep = ""
        ),
        paste(Sys.getenv("NETWORK_DIR"), location,
          Name_to_save, ".fst",
          sep = ""
        ),
        overwrite = overwrite
      )
    }
  }

  if (save_csv == TRUE) {
    if (fileEncoding == "") {
      data.table::fwrite(Object_to_save, paste(Sys.getenv("NETWORK_DIR"),
        dataloc, Name_to_save, ".csv",
        sep = ""
      ),
      row.names = F,
      na = "",
      sep = ";",
      dec = ",",
      qmethod = "double"
      )
    } else {
      data.table::fwrite(Object_to_save, paste(Sys.getenv("NETWORK_DIR"),
        dataloc, Name_to_save, ".csv",
        sep = ""
      ),
      row.names = F,
      na = "", fileEncoding = fileEncoding,
      sep = ";",
      dec = ",",
      qmethod = "double"
      )
    }
    if (length(dataloc) > 1) {
      for (location in dataloc[-1]) {
        file.copy(
          paste(Sys.getenv("NETWORK_DIR"),
            dataloc[1], Name_to_save, ".csv",
            sep = ""
          ),
          paste(Sys.getenv("NETWORK_DIR"), location,
            Name_to_save, ".csv",
            sep = ""
          ),
          overwrite = overwrite
        )
      }
    }
  }
  if (session_info == TRUE) {
    save_sessioninfo(paste(Sys.getenv("NETWORK_DIR"),
      dataloc, Name_to_save, "_",
      sep = ""
    ))
  }
}
