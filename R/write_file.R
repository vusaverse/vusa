#' Save RDS / CSV / FST
#'
#' Een generieke functie die een R object in zowel een RDS, CSV als fst opslaat.
## RDS is het meest efficiente formaat voor R en CSV kan eenvoudig door Excel geopend worden.
## Fst is een binair format dat per variabele ingelezen kan worden.
#'
#' @param Object_to_save Data object to write.
#' @param Name_to_save File name to use when writing (without extension).
#' @param destination Optional argument to set write location.
#' @param save_csv Default FALSE: whether to write file as a csv file.
#' @param save_fst Default FALSE: whether to write file as a fst file.
#' @param save_rds Default FALSE: whether to write file as a rds file.
#' @param show_rownames Whether to write row names in file
#' @export
write_file <- function(Object_to_save, Name_to_save, destination = NULL,
                       save_csv = FALSE,
                       save_fst = FALSE,
                       save_rds = FALSE,
                       show_rownames = FALSE) {
  if (Sys.getenv("RSTUDIO") == "1") {
    docname <- basename(rstudioapi::getActiveDocumentContext()$path)
    directory <- rstudioapi::getActiveDocumentContext()$path
  } else {
    docname <- basename(scriptName::current_filename())
    directory <- this.path::sys.dir()
  }

  if (is.null(destination)) {
    ## First check if in 20. Test/ directory
    if (stringr::str_detect(directory, "20. Test")) {
      destination <- "20. Test/"
    } else {
      ## Otherwise use the script name
      if (stringr::str_detect(docname, "Inlezen")) {
        destination <- "1. Ingelezen data/"
      }

      if (stringr::str_detect(docname, "Manipuleren")) {
        destination <- "2. Geprepareerde data/"
      }

      if (stringr::str_detect(docname, "Rapport")) {
        destination <- "5. Rapportages/"
      }
    }
  }

  if (stringr::str_detect(destination, "Tableau")) {
    dataloc <- paste0(Sys.getenv("NETWORK_DIR"), destination)
  } else {
    dataloc <- paste(Sys.getenv("NETWORK_DIR"),
      "Output/", Sys.getenv("BRANCH"),
      "/", destination,
      sep = ""
    )
  }

  if (!save_fst & !save_rds & !save_csv) {
    stop("No file extension has been provided")
  }


  if (save_csv) {
    data.table::fwrite(Object_to_save, paste(dataloc, Name_to_save, ".csv",
      sep = ""
    ),
    row.names = show_rownames,
    na = "",
    sep = ";",
    dec = ","
    )
  }

  if (save_fst) {
    fst::write_fst(Object_to_save, paste(dataloc, Name_to_save, ".fst",
      sep = ""
    ), compress = 100)
  }

  if (save_rds) {
    saveRDS(Object_to_save, paste(dataloc, Name_to_save, ".rds", sep = ""), version = 2)
  }
}
