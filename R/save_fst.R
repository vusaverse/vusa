#' save fst
#'
#' write R data frame as fst..
#'
#' @param Object_to_save Data frame.
#' @param Name_to_save File name to write without extension.
#' @param output Location on network drive, depending on the value of branch
#' "G:/DSZ/SA2016/Datasets/Output/(branch)/"
#' @param dataloc Location on network drive, having as root "G:/DSZ/SA2016/Datasets/"
#' @param session_info Default: FALSE. Whether to save session info to same directory.
#' @param ... dot parameters to parse to write_fst
#' @export
save_fst <- function(Object_to_save, Name_to_save, output = "1. Ingelezen data/", dataloc, session_info = FALSE, ...){

    if(missing(dataloc) == TRUE) {
        dataloc <- paste("Output/", Sys.getenv("BRANCH"), "/", output, sep = "")
    }

    fst::write_fst(Object_to_save, paste(Sys.getenv("NETWORK_DIR"),
                                    dataloc,
                                    Name_to_save,
                                    ".fst",
                                    sep = "",
                                    ...))

    ## Write session info
    if(session_info == TRUE) {
        save_sessioninfo(paste(Sys.getenv("NETWORK_DIR"),
                               dataloc,
                               Name_to_save,
                               "_",
                               sep = ""))
    }
}
