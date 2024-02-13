#' Get Analysis set 7
#'
#' Analysis set 7 is retrieved, and the columns specified are from
#' Analysis set 1 added to Analysis set 7.
#' @param cols Variable name as text value, or vector with multiple
#' text values of variables to be added to Analysis Set 7
#' @param cols_match Matching columns for joining AS_1 and AS_7, the default
#' columns are: "INS_Studentnummer","INS_Opleidingsnaam_2002","INS_Inschrijvingsjaar"
#' @param AS_7 Path for the location of Analysis set 7
#' @export
get_analysisset_7 <- function(cols = NULL, cols_match = c("INS_Studentnummer",
              "INS_Opleidingsnaam_2002","INS_Inschrijvingsjaar"), AS_7 = NULL) {

  if (is.null(AS_7)) {

    if (!Sys.getenv("AS_7_DIR") == "") {

      AS_7 <- paste0(Sys.getenv("OUTPUT_DIR"), Sys.getenv("BRANCH"), "/", Sys.getenv("AS_7_DIR"))
    }
    else {
      stop("system variables for AS_7 are missing")
    }
  }

  ## If columns for AS1 are specified, they will be loaded
  if (length(cols > 0)) {

    cols <- c(
      ## First the columns to join
      cols_match,
      cols
    )
    # Prevent duplicate columns
    cols <- unique(cols)
    ## Load AS1 and select the relevant columns
    Analysis_set_1 <- get_analysisset() %>%
      dplyr::select(dplyr::all_of(cols))


    ## Load AS7 and join it with the columns of AS1
    Analysis_set_7 <- readRDS(AS_7) %>%
      dplyr::select(dplyr::all_of(cols_match), !dplyr::any_of(cols)) %>%
      dplyr::left_join(
        Analysis_set_1,
        by = cols_match
      )
  } else {
    ## If no columns are specified, AS7 will just be loaded
    Analysis_set_7 <- readRDS(AS_7)
  }

  ## Return Analysis_set_7
  return(Analysis_set_7)
}
