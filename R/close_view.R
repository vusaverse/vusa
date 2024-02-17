#' Close all tabs that are not already in the Tabs vector and close view panes
#'
#' This function iterates through all tabs in RStudio, closing those that are not
#' already listed in the Tabs vector. It also handles view panes by closing them
#' if they do not have a corresponding document context.
#'
#' @return NULL
#' @export
close_view <- function() {
  Tabs <- c()
  
  doc <- rstudioapi::getSourceEditorContext()
  
  while (is.null(doc) || !doc$id %in% Tabs) {
    if (is.null(doc)) {
      rstudioapi::executeCommand('closeSourceDoc')
    }
    rstudioapi::executeCommand('nextTab')
    
    Tabs <- c(Tabs, doc$id);
    
    doc <- rstudioapi::getSourceEditorContext()
  }
}