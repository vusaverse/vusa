#' Get the archived result files to calculate the end results
#'
#' @param academic_year The academic year that we are looking at
#' @param last_date The date of the last checked result file (Date object)
#' @export
get_archived_result_files <- function(academic_year, last_date = 0) {
  Files <- Week <- Academisch_jaar <- NULL
  dfBestanden <-
    data.frame(
      Files = list.files(path = paste0(Sys.getenv("RAW_DATA_DIR"), "\\SAP SLM\\PROD\\XX. Archief"), full.names = TRUE)
    ) %>%
    dplyr::filter(grepl(".zip$", Files)) %>%
    ## Verkrijg de datum waarin het bestand is gedownload
    dplyr::mutate(
      Week = as.Date(stringr::str_extract(Files, "[0-9]{8}"), "%Y%m%d"),
      Academisch_jaar = as.numeric(gsub(".*MODBOOK_|_.*$", "", Files))
    ) %>%
    ## Behoud alleen de bestanden van het academisch jaar
    dplyr::filter(Academisch_jaar == academic_year) %>%
    ## Rangschik de weken
    dplyr::arrange(Week) %>%
    dplyr::filter(Week > last_date)

  return(dfBestanden)
}
