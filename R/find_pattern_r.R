#' Find pattern in R scripts
#'
#' Function to search for a pattern in R scripts.
#'
#' @param pattern Pattern to search on
#' @param path de folder waar gezocht moet worden, default working directory
#' @param case.sensitive Whether pattern is case sensitve or ntot
#' @param comments whether to search in commented lines
#' @return dataframe met R script paths
#' @export
#' @examples
#' \dontrun{
#' find_pattern_r(pattern = "DUO_", path = "05. Rapporten/")
#' find_pattern_r("DUO_", c("05. Rapporten/", "02. Manipuleren/"))
#' }
find_pattern_r <- function(pattern, path = ".", case.sensitive = TRUE, comments = FALSE) {
  if ("renv" %in% fs::dir_ls(path)) {
    path <- fs::dir_ls(type = "directory", regexp = "renv", invert = T)
  }
  dt <- findR::findRscript(path, pattern = pattern, case.sensitive = case.sensitive, show.results = TRUE, comments = comments)
  dt <- unique(dt[1])
  return(dt)
}

#' Find pattern in csv files.
#'
#' Function to search for a pattern in csv files.
#'
#' @param pattern Pattern to search on
#' @param path de folder waar gezocht moet worden, default working directory
#' @param case.sensitive Whether pattern is case sensitve or ntot
#' @param show.results whether to show the results in the console
#' @return dataframe met R script paths
#' @export
#' @examples
#' \dontrun{
#' find_pattern_csv(pattern = "croho")
#' find_pattern_csv("croho", case.sensitive = FALSE)
#' }
find_pattern_csv <- function(pattern = "Hello World",
                             path = ".",
                             case.sensitive = TRUE,
                             show.results = TRUE) {
  if ("renv" %in% fs::dir_ls(path)) {
    path <<- fs::dir_ls(type = "directory", regexp = "renv", invert = T)
  }

  fls <- list.files(path, pattern = "\\.csv$", full.names = T, ignore.case = T, recursive = T)

  if (length(fls) > 0) {
    hits <- NULL

    for (i in 1:length(fls)) {
      if (case.sensitive == FALSE) {
        pattern <- tolower(pattern)
        a <- tolower(readLines(fls[i], warn = T))
      } else {
        a <- readLines(fls[i], warn = F)
      }

      if (length(grep(pattern, a)) > 0) {
        path_to_file <- fls[i]
        line <- which(grepl(pattern, a))
        hit <- cbind.data.frame(path_to_file, line)
        hits <- rbind.data.frame(hits, hit)

        rm(hit)
      }
    }

    message(paste0("Number of csv files scanned: ", length(fls)))

    if (!is.null(hits)) {
      message(paste0("Number of csv files with matching content: ", length(unique(hits$path_to_file))))
      message(paste0("Total number of matches: ", nrow(hits)))

      if (show.results == TRUE) hits
    } else {
      message("Number of text files with matching content: 0")
      message("Total number of matches: 0")
    }
  } else {
    message(paste0("No csv files found!"))
  }

  return(hits$path_to_file)
}
