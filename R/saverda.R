#' Save RDA
#'
#' a function that saves an R object as an .rda file. Before the file
#' is stored, it is first transformed to a data.frame, so that
#' Tableau can read the file
#'
#' @param Object_to_save The object to save
#' @param Name_to_save The file name that the file should be given (without extension)
#' @param output Data location on the network drive, depending on the branch
#' set.starting from "G:/DSZ/SA2016/Datasets/Output/(branch)/"
#' @param dataloc Data location on the network drive, starting from "G:/DSZ/SA2016/Datasets/"
#' @param session_info Default FALSE. Must the session info be in the same folder
#' be written off
#' @export
saverda <- function(Object_to_save, Name_to_save, output = "1. Ingelezen data/", dataloc, session_info = FALSE) {
  ## determine the dataloc based on the branch
  if(missing(dataloc) == TRUE) {
    dataloc <- paste("Output/", vvcommander::sa_branch_get(), "/", output, sep = "")
  }

  ## Tableau can only load data frames, therefore the data must be transformed first
  ## be sent to a data.frame
  Object_to_save <- as.data.frame(Object_to_save)

  ##save RDA
  save(Object_to_save,
       file = paste(vvcommander::sa_network_dir_get(),dataloc,Name_to_save,".rda",sep=""), version = 2)

  ## save the Session info
  if(session_info == TRUE) {
    save_sessioninfo(paste(vvcommander::sa_network_dir_get(),dataloc,Name_to_save, "_", sep=""))
  }
}