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