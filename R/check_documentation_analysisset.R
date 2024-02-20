#' Check documentation Analysis set
#'
#' Function that checks if the columns in the analysis set match the
#' columns in the analysis set documentation file
#' @param x A data frame from Analysis Set 1
#' @return A data frame showing the differences between the analysis set and the documentation
#' @export
check_documentation_analysisset <- function(x) {
  ## read in the documentation file
  Documentation <- documentation_get()
  ## Create empty data frame
  Differences <- tibble::tibble(
    Veldnaam = character(),
    Missing_in = character()
  )

  ## Variable is in not in Analysis set, not in documentation
  if (length(setdiff(names(x), Documentation$Veldnaam)) > 0) {
    missing_in_documentation <- tibble::tibble(
      Veldnaam = setdiff(
        names(x),
        Documentation$Veldnaam
      ),
      Missing_in = "Documentation"
    )
    ## add to the data frame
    Differences <- dplyr::bind_rows(Differences, missing_in_documentation)
  }

  ## Variable is in Documentation, not in Analysis Set
  if (length(setdiff(Documentation$Veldnaam, names(x))) > 0) {
    missing_in_analysisset <- tibble::tibble(
      Veldnaam = setdiff(
        Documentation$Veldnaam,
        names(x)
      ),
      Missing_in = "Analysis Set"
    )
    ## add to the data frame
    Differences <- dplyr::bind_rows(Differences, missing_in_analysisset)
  }
  ## send a warning if there are differences between the documentation and the analysis set
  if (nrow(Differences) > 0) {
    warning(
      paste(
        "The analysis set does not match the documentation.",
        nrow(Differences),
        "columns differ"
      )
    )
  }
  ## Give the dataframe me differences as output
  return(Differences)
}
