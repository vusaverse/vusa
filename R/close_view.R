#' close_view.
#'
#' Close all view tabs.
#' Note: this also closes unsaved files.
#' You will be prompted to save before these files are closed.
#'
#' @export
close_view <- function() {
  Tabs <- c()

  doc <- rstudioapi::documentPath()

  while (is.null(doc)||!doc%in% Tabs) {
    if (is.null(doc)) {
      rstudioapi::executeCommand('closeSourceDoc')
    }
    rstudioapi::executeCommand('nextTab')

    Tabs <- c(Tabs, doc);

    doc <- rstudioapi::documentPath()
  }
}