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
#' @param Test_documentatie boolean die aangeeft of test documentatie gemaakt moet worden
#' @param Documentatie boolean die aangeeft of documentatie gemaakt moet worden
#' @param overwrite whether to overwrite existing file when copying to other directory.
#' @export
savefst_csv <- function(Object_to_save, Name_to_save, output = "1. Ingelezen data/",
                        dataloc, fileEncoding = "", save_csv = FALSE,
                        session_info = FALSE, Test_documentatie = FALSE, Documentatie = FALSE,
                        overwrite = TRUE) {
  if (missing(dataloc) == TRUE) {
    dataloc <- paste("Output/", sa_branch_get(),
                     "/", output,
                     sep = ""
    )
  }
  if (Test_documentatie == TRUE) {
    Naam_doc <- paste0(output, Name_to_save)
    create_documentatie_tests(
      df = Object_to_save, Naam = Naam_doc,
      Limietwaarden_bestandspad = "Testdocumentatie/Numerieke_limietwaarden.csv"
    )
  }
  if (Documentatie == TRUE) {
    create_documentatie(df = Object_to_save, Naam = paste0(
      "Documentatie_",
      Name_to_save
    ), Limietwaarden_bestandspad = "Testdocumentatie/Numerieke_limietwaarden.csv")
  }

  fst::write_fst(Object_to_save, paste(sa_network_dir_get(),
                                       dataloc, Name_to_save, ".fst",
                                       sep = ""
  ), compress = 100)
  if (length(dataloc) > 1) {
    for (location in dataloc[-1]) {
      file.copy(
        paste(sa_network_dir_get(),
              dataloc[1], Name_to_save, ".fst",
              sep = ""
        ),
        paste(sa_network_dir_get(), location,
              Name_to_save, ".fst",
              sep = ""
        ),
        overwrite = overwrite
      )
    }
  }

  if (save_csv == TRUE) {
    if (fileEncoding == "") {
      data.table::fwrite(Object_to_save, paste(sa_network_dir_get(),
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
      data.table::fwrite(Object_to_save, paste(sa_network_dir_get(),
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
          paste(sa_network_dir_get(),
                dataloc[1], Name_to_save, ".csv",
                sep = ""
          ),
          paste(sa_network_dir_get(), location,
                Name_to_save, ".csv",
                sep = ""
          ),
          overwrite = overwrite
        )
      }
    }
  }
  if (session_info == TRUE) {
    save_sessioninfo(paste(sa_network_dir_get(),
                           dataloc, Name_to_save, "_",
                           sep = ""
    ))
  }
}
