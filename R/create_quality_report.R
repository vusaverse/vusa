#' Create quality report
#'
#' This function generates a quality report in a previously created folder at the specified location.
#' The quality report is partly created on the basis of the analysis set documentation,
#' a description of the variables can therefore only be added when the
#' variable is documented.
#'
#' @param df Dataframe for which a quality report must be created.
#' @param ProjectName name Name of the project, this will become part of the title of the report, among other things.
#' @param Network_directory Location of used Network. If Null, system variables will be used
#' @param export_map name of the map the exports will be placed in. Default = "/6. Exports/"
#' @param path the path the quality report will be placed. if null; the export map
#' together with the project name will be used
#' @export
create_quality_report <- function(df, ProjectName, Network_directory = NULL,
                                  export_map = "/6. Exports/", path = NULL) {

  if (is.null(Network_directory)) {
    print("No given Network_directory, so looking for system variables")
    if (!Sys.getenv("OUTPUT_DIR") == "") {
      print("system variable present, so this will be used")
      Network_directory <- Sys.getenv("OUTPUT_DIR")
    }
    else {
      stop("system variable for Network_directory is missing")
    }
  }

  check_installed_package("dataMaid")

  ## Load documentation
  Documentation <- documentation_get()

  ## Variables present in the export
  vars <- dput(names(df))

  ## Make selection and adjust dates for datamaid
  selected_data <- df %>%
    # If dates are given, they must be adjusted due to compatibility package
    dplyr::mutate_if(lubridate::is.Date, as.character) %>%
    # Select desired columns
    dplyr::select(if(!is.null(vars)){!!vars})

  ## Add labels and descriptions to selected_data
  selected_data <- purrr::map2_dfc(selected_data,
                                   names(selected_data),
                                   add_attribute,
                                   Documentation = Documentation)

  ## Create path with date if missing
  if (is.null(path)) {
    path <- paste0(vvcommander::sa_branch_get(), export_map, ProjectName, "/", Sys.Date())
  }

  ## Create quality report directory
  dir.create(paste(Network_directory, path, "Quality Report/", sep = ''))

  ## Create the dataMaid quality report
  dataMaid::makeCodebook(selected_data,
                         # Title of the report
                         reportTitle = paste0("Quality report ", ProjectName),
                         # The name of the file
                         file = paste0(Network_directory, path, "Quality Report/", ProjectName, ".Rmd"),
                         # what kind of output
                         output="html",
                         # replace old file
                         replace = TRUE,
                         # output to be generated and saved
                         render = TRUE,
                         # maximum unique values printed
                         maxProbVals = 10)

}

#' Adjust dataframe for dataMaid
#' Automatically add based on description and category in documentation
#' the attribute shortDescription and label to the variables of the Analysis Set
#' @param x vector
#' @param varname Variable name from analysis set
#' @param Documentation Documentation of the analysis set
#'
#' @return Analysis set with labels
add_attribute <- function(x, varname, Documentation){
  ## temporarily also remove accents globally until a replacement function comes
  Veldnaam <- NULL
  description <- dplyr::filter(Documentation, Veldnaam == varname)

  attr(x, "shortDescription") <- stringi::stri_trans_general(str = description$Omschrijving[1], id = "Latin-ASCII")
  attr(x, "label") <- stringi::stri_trans_general(str = description$Categorie[1], id = "Latin-ASCII")
  return(x)
}
