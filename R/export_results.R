#' Export Results.R
#'
#' This function creates a data frame with the results for each student
#' in the entered analysis set. NB! To compare it uses an standart VUSA
#' Analysis set, together with its columns: INS_Opleidingsnaam_2002",
#' "INS_Faculteit", "INS_Studentnummer", "INS_Inschrijvingsjaar", and
#' "RES_Academisch_jaar_beoordeling". So make sure you have this columns present
#' in your Analysis_set when using this function
#'
#' Dependencies: Index.R
#' Export Analysis Set.R
#'
#' @param Analysis_file This must be a data file of a subset of the analysis set.
#' The script provides the results of the student numbers that are present
#' are in this file.
#' @param Results_location The path to your registered results. If NULL, system
#' variables are used
#' @param Analysis_set An Analysis_set that is in the format of the VUSA analysis set,
#' wit the columns: INS_Opleidingsnaam_2002", "INS_Faculteit", "INS_Studentnummer",
#' "INS_Inschrijvingsjaar", and "RES_Academisch_jaar_beoordeling". If NULL,
#' system variables are used
#'
#' @export
#'
export_results <- function(Analysis_file, Results_location = NULL, Analysis_set = NULL) {
  INS_Opleidingsnaam_2002 <- INS_Faculteit <- NULL

  ## find system variables for Results location and Analysis_set
  if (is.null(Results_location)) {
    print("No given Results_location, so looking for system variables")
    if (!Sys.getenv("RESULTS_DIR") == "") {
      print("system variable present, so this will be used")
      Results_location <- Sys.getenv("RESULTS_DIR")
    }
    else {
      stop("system variable for Results_location is missing")
    }
  }

  if (is.null(Analysis_set)) {
    print("No given Analysis_set, so looking for system variables")
    if (!Sys.getenv("AS_1_DIR") == "") {
      print("system variable present, so this will be used")
      Analysis_set <- Sys.getenv("AS_1_DIR")
    }
    else {
      stop("system variable for Analysis_set is missing")
    }
  }

  # Read in required files
  Results <- readrds_csv(output = Results_location)
  Analysis_export <- Analysis_file

  ## columns are reuired columns from the VUSA Analysis_set for using this function
  Analysis_set <- readrds_csv(output = Analysis_set,
                              columns = c("INS_Opleidingsnaam_2002",
                                          "INS_Faculteit"))

  # See which Student/education/year combinations there are (again use VUSA columns)
  Results_export <-
    data.frame(key = with(
      Analysis_export,
      paste(
        INS_Studentnummer,
        INS_Inschrijvingsjaar,
        INS_Opleidingsnaam_2002,
        sep = '-'
      )
    )) %>% dplyr::distinct()

  # Create the same key in the result data
  Results$key <- paste(with(
    Results,
    paste(
      INS_Studentnummer,
      RES_Academisch_jaar_beoordeling,
      INS_Opleidingsnaam_2002,
      sep = '-'
    )
  ))

  # Select only the results of the required student/education/year combinations
  Results_export <- merge(Results_export,
                          Results,
                          by = 'key',
                          all.x = T)


  # Map the factorial name to the results export
  Faculty_folders <- subset(Analysis_set, select = c(INS_Opleidingsnaam_2002,
                                                     INS_Faculteit))
  Faculty_folders <- unique(Faculty_folders)

  ## Make the export
  Results_export <-
    merge(Faculty_folders,
          Results_export,
          by = 'INS_Opleidingsnaam_2002',
          all.Y = T)

  return(Results_export)
}
