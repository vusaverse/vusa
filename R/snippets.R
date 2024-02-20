#' Download a file from a GitHub repository
#'
#' This function takes a URL pointing to a raw file in a GitHub repository and a
#' temporary file path where the downloaded content will be saved. It makes a GET
#' request to the provided URL and, if successful, writes the raw content of the
#' response to the specified temporary file path.
#'
#' @param url A character string specifying the URL of the file to download.
#' @param temp_file_path A character string specifying the path to the temporary
#' file where the downloaded content will be saved.
#'
#' @return A character string indicating the path to the temporary file where the
#' downloaded content has been saved.
#'
#' @export
download_github_file <- function(url, temp_file_path) {
  response <- httr::GET(url)
  if (response$status_code == 200) {
    writeBin(httr::content(response, "raw"), temp_file_path)
    return(temp_file_path)
  } else {
    stop("Failed to download the file from GitHub.")
  }
}


#' Get the path to the snippets file for a given type
#'
#' This function determines the appropriate path to the snippets file based on
#' the type of the file and the version of RStudio being used. It handles the
#' differences in file locations between old and new versions of RStudio.
#'
#' @param type A character string specifying the type of the snippets file.
#'              The default is "r". Other valid types include "markdown",
#'              "c_cpp", "css", "html", "java", "javascript", "python",
#'              "sql", "stan", "tex".
#'
#' @return A character string indicating the path to the snippets file.
#'
#' @export
get_snippets_file <- function(type = c(
                                "r", "markdown", "c_cpp", "css", "html",
                                "java", "javascript", "python", "sql", "stan", "tex"
                              )) {
  type <- tolower(type)
  type <- match.arg(type)
  file <- fs::path_ext_set(type, "snippets")
  new_rstudio <- !rstudioapi::isAvailable() || rstudioapi::getVersion() >=
    "1.3.0"
  old_path <- fs::path_home_r(".R", "snippets", file)
  new_path <- rstudio.prefs::rstudio_config_path("snippets", file)
  if (new_rstudio && file.exists(old_path) && !file.exists(new_path)) {
    dir.create(dirname(new_path))
    file.copy(old_path, new_path)
    ui_done("Copying snippets file to {ui_path(new_path)}")
  }
  path <- if (new_rstudio) {
    new_path
  } else {
    old_path
  }

  return(path)
}

#' Split a file into chunks based on empty lines
#'
#' This function reads a file, splits it into chunks based on empty lines,
#' and preserves the indentation of each line within a chunk.
#'
#' @param filename A character string specifying the path to the file to be split.
#'
#' @return A list of character vectors, where each vector represents a chunk of the file.
#'
#' @export
split_file <- function(filename) {
  # read in the file
  lines <- readLines(filename)

  # split the lines into chunks
  chunks <- split(lines, cumsum(lines == ""))

  # remove any empty chunks
  chunks <- chunks[sapply(chunks, length) > 1]

  # split each chunk into lines and preserve indentation
  for (i in seq_along(chunks)) {
    lines <- unlist(strsplit(chunks[[i]], "\n"))
    leading_whitespace <- regexpr("\\S", lines)
    chunks[[i]] <- paste0(substr(lines, 1, leading_whitespace - 1), substr(lines, leading_whitespace, nchar(lines)), collapse = "\n")
  }

  return(chunks)
}
