#' Save RDS / CSV / FST
#'
#' Een generieke functie die een R object in zowel een RDS, CSV als fst opslaat.
## RDS is het meest efficiente formaat voor R en CSV kan eenvoudig door Excel geopend worden.
## Fst is een binair format dat per variabele ingelezen kan worden.
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
#' @param save_fst Standaard FALSE: Er wordt geen fst weggeschreven. Als TRUE wordt
#' zowel een .rds als een .fst weggeschreven.
#' @param session_info Standaard FALSE. Moet de sessie-info in dezelfde folder
#' weggeschreve worden
#' @param overwrite whether to overwrite existing file when copying to other directory.
#' @export
saverds_csv <- function(Object_to_save, Name_to_save, output = "1. Ingelezen data/", dataloc, fileEncoding = "",
                        save_csv = FALSE, save_fst = FALSE, session_info = FALSE,
                        overwrite = TRUE) {
  ## bepaal de dataloc op basis van de branch
  if (missing(dataloc) == TRUE) {
    dataloc <- paste("Output/", sa_branch_get(), "/", output, sep = "")
  }

  ## save RDS
  saveRDS(Object_to_save, paste(sa_network_dir_get(), dataloc, Name_to_save, ".rds", sep = ""), version = 2)
  if (length(dataloc) > 1) {
    for (location in dataloc[-1]) {
      file.copy(
        paste(sa_network_dir_get(),
          dataloc[1], Name_to_save, ".rds",
          sep = ""
        ),
        paste(sa_network_dir_get(),
          location, Name_to_save, ".rds",
          sep = ""
        ),
        overwrite = overwrite
      )
    }
  }
  if (save_fst == TRUE) {
    fst::write_fst(Object_to_save, paste(sa_network_dir_get(), dataloc, Name_to_save, ".fst", sep = ""), compress = 100)
    if (length(dataloc) > 1) {
      for (location in dataloc[-1]) {
        file.copy(
          paste(sa_network_dir_get(),
            dataloc[1], Name_to_save, ".fst",
            sep = ""
          ),
          paste(sa_network_dir_get(),
            location, Name_to_save, ".fst",
            sep = ""
          ),
          overwrite = overwrite
        )
      }
    }
  }

  ## save CSV (staat standaard op FALSE)
  if (save_csv == TRUE) {
    if (fileEncoding == "") {
      utils::write.csv2(Object_to_save, paste(sa_network_dir_get(), dataloc, Name_to_save, ".csv", sep = ""), row.names = F, na = "")
    } else {
      utils::write.csv2(Object_to_save, paste(sa_network_dir_get(), dataloc, Name_to_save, ".csv", sep = ""), row.names = F, na = "", fileEncoding = fileEncoding)
    }
    if (length(dataloc) > 1) {
      for (location in dataloc[-1]) {
        file.copy(
          paste(sa_network_dir_get(),
            dataloc[1], Name_to_save, ".csv",
            sep = ""
          ),
          paste(sa_network_dir_get(),
            location, Name_to_save, ".csv",
            sep = ""
          ),
          overwrite = overwrite
        )
      }
    }
  }
  ## sla de Sessioninfo op
  if (session_info == TRUE) {
    save_sessioninfo(paste(sa_network_dir_get(), dataloc, Name_to_save, "_", sep = ""))
  }
}

#' sa_defineer_test_documentatie
#'
#' Global option to generate a new documentation file each time saverds_csv is used.
#' @param value Logical: Whether to edit the test documentation.
#' @export
sa_defineer_test_documentatie <- function(value = TRUE) {
  options(
    list(
      VUStudentAnalytics.Defineer_test_documentatie = value
    )
  )
}

#' sa_defineer_test_documentatie
#'
#' Internal function read what was set in sa_defineer_test_documentatie()
sa_defineer_test_documentatie_get <- function() {
  optie <- getOption("VUStudentAnalytics.Defineer_test_documentatie")
  if (is.null(optie)) {
    optie <- FALSE
  }
  return(optie)
}
