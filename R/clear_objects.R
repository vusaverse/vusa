#' Clears the global environment except for objects specified in the DEFAULT_KEEP_LIST system variable.
#'
#' This function removes all objects from the global environment except for those specified
#' in the DEFAULT_KEEP_LIST system variable. The variable is expected to be a space-separated
#' list of object names. If the variable is not set, the function will not run.
#'
#' @note This function should be used with caution as it modifies the global environment.
#'
#' @seealso \code{\link[base]{rm}} for the underlying mechanism used to remove objects.
#'
#' @keywords internal
#' @export
clear_global_proj <- function() {
  # Retrieve the default keep list from the environment variable
  env_default_keep_list <- Sys.getenv("DEFAULT_KEEP_LIST")

  # Check if the environment variable is set and convert it to a list of object names
  if (!env_default_keep_list == "") {
    default_keep_list <- strsplit(env_default_keep_list, " ")[[1]]
  } else {
    # Handle the case where the environment variable is not set
    # For example, you could throw an error or use a fallback list
    stop("The 'DEFAULT_KEEP_LIST' environment variable is not set.")
  }

  # Get the names of all objects in the global environment
  all_objects <- ls(envir = .GlobalEnv)

  # Determine which objects to remove by finding the difference between all objects and the default keep list
  objects_to_remove <- setdiff(all_objects, default_keep_list)

  # Remove the objects from the global environment
  # rm(list = objects_to_remove, envir = .GlobalEnv)
  rm(list = objects_to_remove, envir = parent.frame())
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


#' Remove objects assigned in the calling script
#'
#' A scope-aware alternative to `vusa::clear_script_objects()` that works in
#' Positron and RStudio.
#'
#' @param ... Unquoted or quoted names of objects to keep.
#' @param filepath Optional path to the script.
#' @param list Character vector of additional object names to keep.
#' @param pos Position of the environment from which objects are removed.
#' @param envir Environment from which objects are removed.
#' @param line_start Number of lines to skip.
#' @param line_end Maximum number of lines to inspect; a negative value reads
#'   the rest of the script.
#' @param silent Whether to suppress the removal message.
#'
#' @return The names of the removed objects, invisibly.
clear_script_objects <- function(...,
                                        filepath = NULL,
                                        list = character(),
                                        pos = -1,
                                        envir = as.environment(pos),
                                        line_start = 0L,
                                        line_end = -1L,
                                        silent = TRUE) {
  # By default, clean the environment in which the script called this helper.
  if (missing(envir)) {
    envir <- if (identical(pos, -1)) parent.frame() else as.environment(pos)
  }

  if (is.null(filepath)) {
    # n = 1 selects the file containing this function call. This keeps A as
    # the target when another script B sources A.
    filepath <- this.path::this.path(n = 1L, original = TRUE)
  }

  # Positron may return paths such as /C%3A/project/My%20script.R.
  filepath <- utils::URLdecode(filepath)
  filepath <- sub("^file://", "", filepath, ignore.case = TRUE)
  if (.Platform$OS.type == "windows" && grepl("^/[[:alpha:]]:[/\\\\]", filepath)) {
    filepath <- substring(filepath, 2L)
  }

  # Build the keep list without evaluating unquoted names passed through `...`.
  dots <- match.call(expand.dots = FALSE)$...
  if (length(dots) && !all(vapply(
    dots,
    function(x) is.symbol(x) || (is.character(x) && length(x) == 1L),
    logical(1)
  ))) {
    stop("... must contain unquoted names or character strings", call. = FALSE)
  }

  keep <- c(list, vapply(dots, as.character, character(1)))

  # Restrict the search to the requested line range.
  lines <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  if (line_start > 0L) {
    lines <- tail(lines, -as.integer(line_start))
  }
  if (line_end >= 0L) {
    lines <- head(lines, as.integer(line_end))
  }

  # Parse the code so assignment-looking text inside strings is ignored.
  expressions <- parse(text = lines, keep.source = FALSE)

  # Recursively collect assignments that execute in the script's scope.
  # Function bodies and quoted expressions belong to another scope (or are not
  # evaluated), so recursion stops when one of those constructs is reached.
  find_assignments <- function(expr) {
    if (is.expression(expr)) {
      return(unlist(lapply(as.list(expr), find_assignments), use.names = FALSE))
    }
    if (!is.call(expr)) {
      return(character())
    }

    operator <- if (is.symbol(expr[[1L]])) as.character(expr[[1L]]) else ""
    if (operator %in% c("function", "quote", "expression", "substitute", "alist", "~")) {
      return(character())
    }

    if (operator %in% c("<-", "=", "<<-")) {
      name <- if (is.symbol(expr[[2L]])) as.character(expr[[2L]]) else character()
      return(c(name, find_assignments(expr[[3L]])))
    }
    if (operator %in% c("->", "->>")) {
      name <- if (is.symbol(expr[[3L]])) as.character(expr[[3L]]) else character()
      return(c(name, find_assignments(expr[[2L]])))
    }
    if (operator == "for") {
      iterator <- if (is.symbol(expr[[2L]])) as.character(expr[[2L]]) else character()
      return(c(
        iterator,
        find_assignments(expr[[3L]]),
        find_assignments(expr[[4L]])
      ))
    }

    unlist(lapply(as.list(expr)[-1L], find_assignments), use.names = FALSE)
  }

  assigned <- unique(find_assignments(expressions))

  # Remove only names that exist in the target environment and are not kept.
  remove <- setdiff(intersect(assigned, ls(envir, all.names = TRUE)), keep)

  if (length(remove)) {
    rm(list = remove, envir = envir)
  }
  if (!isTRUE(silent)) {
    message("Removed: ", paste(remove, collapse = ", "))
  }

  invisible(remove)
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
  if (length(dots) && !all(vapply(dots, function(x) {
    is.symbol(x) ||
      is.character(x)
  }, NA, USE.NAMES = FALSE))) {
    stop("... must contain names or character strings")
  }
  names <- vapply(dots, as.character, "")
  if (length(names) == 0L) {
    names <- character()
  }
  list <- .Primitive("c")(list, names)
  Teststring_assignment <- "^[a-zA-Z_0-9]*(?=(\\s<-))"
  Regels <- stringr::str_extract(readr::read_lines(script), Teststring_assignment)
  Regels <- base::unique(Regels[!base::is.na(Regels)])
  Regels2 <- setdiff(Regels, list)
  rm(list = Regels2, pos = ".GlobalEnv")
}
