#' Save to mulitple tableau drectories
#'
#' A generic function to write data to multiple locations in different formats,
#' these being rds, fst and csv.
#'
#' @param Object_to_save The object to save
#' @param filepaths To the datalocation of the project
#' @param overwrite whether to overwrite existing file when copying to other directory.
#' @param save_csv Default FALSE: No CSV is saved. When TRUE becomes
#' both a .rds and a .csv are saved.
#' @param save_fst Default FALSE: No fst is written. When TRUE becomes
#' @param fileEncoding The fileencoding for write.csv2 can be passed here.
#' If not specified, the default fileencoding will be used.
#' @export
save_multiple_tableau <- function(Object_to_save,
                                  filepaths,
                                  overwrite = TRUE,
                                  save_fst = FALSE,
                                  save_csv = FALSE,
                                  fileEncoding = "") {
  saveRDS(Object_to_save,
    paste(Sys.getenv("NETWORK_DIR"),
      filepaths[1], ".rds",
      sep = ""
    ),
    version = 2
  )

  ## save RDS
  if (length(filepaths) > 1) {
    for (location in filepaths[-1]) {
      file.copy(
        paste(Sys.getenv("NETWORK_DIR"),
          filepaths[1], ".rds",
          sep = ""
        ),
        paste(Sys.getenv("NETWORK_DIR"),
          location, ".rds",
          sep = ""
        ),
        overwrite = overwrite
      )
    }
  }

  if (save_fst == TRUE) {
    fst::write_fst(Object_to_save, paste(Sys.getenv("NETWORK_DIR"),
      filepaths[1], ".fst",
      sep = ""
    ), compress = 100)
    ## copy file to alternative locations
    if (length(filepaths) > 1) {
      for (location in filepaths[-1]) {
        file.copy(
          paste(Sys.getenv("NETWORK_DIR"),
            filepaths[1], ".fst",
            sep = ""
          ),
          paste(Sys.getenv("NETWORK_DIR"),
            location, ".fst",
            sep = ""
          ),
          overwrite = overwrite
        )
      }
    }
  }

  if (save_csv == TRUE) {
    if (fileEncoding == "") {
      utils::write.csv2(Object_to_save,
        paste(Sys.getenv("NETWORK_DIR"),
          filepaths[1], ".csv",
          sep = ""
        ),
        row.names = F,
        na = ""
      )
    } else {
      utils::write.csv2(Object_to_save,
        paste(Sys.getenv("NETWORK_DIR"),
          filepaths[1], ".csv",
          sep = ""
        ),
        row.names = F,
        na = "",
        fileEncoding = fileEncoding
      )
    }
    ## copy file to alternative locations
    if (length(filepaths) > 1) {
      for (location in filepaths[-1]) {
        file.copy(
          paste(Sys.getenv("NETWORK_DIR"),
            filepaths[1], ".csv",
            sep = ""
          ),
          paste(Sys.getenv("NETWORK_DIR"),
            location, ".csv",
            sep = ""
          ),
          overwrite = overwrite
        )
      }
    }
  }
}
