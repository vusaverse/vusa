#' Clear global.
#'
#' Clear global environment except for project specific objects.
#' @param ... which object(s) to keep.
#'
#' @export
clear_global <- function(...) {
  dots <- match.call(expand.dots = FALSE)$...
  if (length(dots) &&
      !all(vapply(dots, function(x) is.symbol(x) || is.character(x), NA, USE.NAMES = FALSE))) {
    stop("... must contain names or character strings")
  }
  names <- vapply(dots, as.character, "")
  if (length(names) == 0L) names <- character()
  list <- .Primitive("c")(list, names)
  rm(
    list = setdiff(
      ls(envir = .GlobalEnv),
      c("Documentatie", "Branch", "Network_directory", list)
    ),
    envir = .GlobalEnv
  )
}

#' clear script objects.
#'
#' Clear objects which are created in the current script.
#' @param ... which object(s) to keep.
#' @param filepath path to script
#' @param list list of objects
#' @param pos what position in the environment to clear
#' @param envir which environment to clear
#' @param line_start from which line
#' @param line_end until which line
#' @param silent whether to mute console output
#' @export
clear_script_objects <- function (..., filepath = NULL, list = character(), pos = -1, envir = as.environment(pos), line_start = 0, line_end = -1L, silent = TRUE){
  if (missing(filepath)){
    filepath <- this.path::this.path()
  }
  
  dots <- match.call(expand.dots=FALSE)$...
  if(length(dots) &&
     !all(vapply(dots, function(x) is.symbol(x) || is.character(x), NA, USE.NAMES=FALSE)))
    stop("... must contain names or character strings")
  names <- vapply(dots, as.character, "")
  if (length(names) == 0L) names <- character()
  list <- .Primitive("c")(list, names)
  
  Teststring_assignment <- "^[a-zA-Z_0-9]*(?=(\\s<-))"
  Regels <-
    stringr::str_extract(readr::read_lines(filepath, skip = line_start, n_max = line_end),
                         Teststring_assignment)
  Regels <- base::unique(Regels[!base::is.na(Regels)])
  
  Regels2 <- setdiff(Regels, list)
  rm(list = Regels2, pos = ".GlobalEnv")
  if (!silent) {
    base::cat(cli::style_bold(cli::col_red("De volgende variabelen worden verwijderd: \n")))
    base::cat(cli::style_bold(cli::col_red(paste(Regels2,
                                                 collapse = ", \n"))))
    base::cat(paste("\n"))
  }
}



#' Source script and clear its objects
#'
#' @param ... Objects to keep in the global environment which are created in the sourced scripts
#' @param script The script to source
#'
#' @export
source_index <- function(..., script) {
  source(script)
  dots <- match.call(expand.dots = FALSE)$...
  if (length(dots) && !all(vapply(dots, function(x) is.symbol(x) ||
                                  is.character(x), NA, USE.NAMES = FALSE)))
    stop("... must contain names or character strings")
  names <- vapply(dots, as.character, "")
  if (length(names) == 0L)
    names <- character()
  list <- .Primitive("c")(list, names)
  Teststring_assignment <- "^[a-zA-Z_0-9]*(?=(\\s<-))"
  Regels <- stringr::str_extract(readr::read_lines(script), Teststring_assignment)
  Regels <- base::unique(Regels[!base::is.na(Regels)])
  Regels2 <- setdiff(Regels, list)
  rm(list = Regels2, pos = ".GlobalEnv")
}