#' Is a generic function that stores an R object in CSV.
#'
#' @param Object_to_save The object to save
#' @param Name_to_save The file name that the file should be given (without extension)
#' @param output Data location on the network drive, depending on the branch
#' set.starting from "G:/DSZ/SA2016/Datasets/Output/(branch)/"
#' @param dataloc Data location on the network drive, starting from "G:/DSZ/SA2016/Datasets/"
#' @param fileEncoding If not specified, UTF-8 is used
#' @param session_info Default FALSE. Must the session info be in the same folder
#' be written off
#'
#'@export
savecsv <- function(Object_to_save, Name_to_save, output = "1. Ingelezen data/", dataloc, fileEncoding = "", session_info = FALSE) {

  ## determine the dataloc based on the branch
  if(missing(dataloc) == TRUE) {
    dataloc <- paste("Output/", Sys.getenv("BRANCH"), "/", output, sep = "")
  }
  ##save CSV
  if(fileEncoding == '') {
    utils::write.csv2(Object_to_save, paste(Sys.getenv("NETWORK_DIR"),dataloc,Name_to_save,".csv",sep=""),row.names=F,na="")
  } else {
    utils::write.csv2(Object_to_save, paste(Sys.getenv("NETWORK_DIR"),dataloc,Name_to_save,".csv",sep=""),row.names=F,na="", fileEncoding = fileEncoding)
  }

  ## save the Session info
  if(session_info == TRUE) {
    save_sessioninfo(paste(Sys.getenv("NETWORK_DIR"),dataloc,Name_to_save, "_", sep=""))
  }
}
