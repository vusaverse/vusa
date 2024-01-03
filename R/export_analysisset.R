#' Export analysisset.R
#'
#'
#' This function makes a subset of the analysisset.
#' NB! A analysisset and documentation like the ones of VU student analytics is
#' required. Because this functions uses the same columns and fieldnames.
#'
#' Dependencies: Index.R
#'               Export results.R
#'
#' Datasets: Output/3. Analyseset/Analysis_set_1.rds
#'                     Output/2. Geprepareerde data/RES_Resultaten.rds
#'
#' @param Faculty_selection Option between list of faculties or "All"
#'          use with format: c('faculty1','faculty2', etc)
#' @param Study_selection Option between list of studies or "All"
#'          use with format: c('study1','study2', etc)
#' @param Phase_selection Option between list of phases or "All". use with format:
#'          c("B", "P", "M") NB! Premaster students often have the same study name
#'          as Bachelor students, but they don't need to be selected.
#'          Keep this in mind when selecting study names.
#' @param Academic_year Option between list of academic years or "All".
#'          use with format c(1:3), 1 or c(1,3,6).
#' @param Category_selection Option of a list of categories.
#'          use format: c('category1','category2', etc)
#'          The standard used variables are: INS_Studentnummer, Opleiding, inschrijvingsjaar
#'          (the ID of the VUSA analysisset).  Layout of the different variables
#'          in categories can be fined in the documentation of the VUSA analysisset
#'          All usable categories are:
#'           c('BSA', 'CBS', 'Functiebeperking', 'Inschrijvingen', 'Introductieweek',
#'             'Matching', 'Middelbareschool', 'NSE', 'Orientatie', 'Resultaten',
#'             'Succesvariabelen', 'Taaltoets', 'Voorspelmodel')
#' @param Column_selection If there is asked for other specific columns, then is
#'          also possible the use those. Default is NULL and their will also
#'          be looked ad category selection use with format: c('Column1','Columns2')
#'          Correct column names can be found in the documentation of the VUSA AS
#' @param Enrollment_year List with enrollment years,
#'          use with format: c(2013,2014)
#' @param Enrollment_year_EOI List with enrollment years EOI,
#'          use with format: c(2013,2014)
#' @param Name_project name of the exported project.
#'          use with format: "Name_project"
#' @param Special_personal_data Option of FALSE(default) en 'Yes, etnicity' and
#'          'yes, disability' (format: c('yes, etnicity',Ja, disability'), for both).
#'          In defalut don't deliver sensitive data. To do so:
#'          explicit permission is required
#' @param Hash Option between TRUE (default) and FALSE.
#' @param Export_results Option between FALSE (default) en TRUE.
#'          In addition of the analysisset, the results will also be exported.
#' @param Save_as_CSV Option between FALSE (default) en TRUE. If TRUE  not only
#'          an rds, but also a csv of the data will be saved.
#' @param Analysis_set_input If an analysisset is made in advance, this can used.
#'          if NULL system variable for AS will be used
#' @param Analysis_set_option Options 1, 1R, 7, or 7X
#' @param Documentation_new_variables Variables that are not in the documentation.
#' @param Qualityrapport Option between FALSE (default) and TRUE.
#'                       If TRUE a qualityrapport will be generated
#' @param Make_zip Option between FALSE (default) en TRUE. If TRUE a Zip of all
#'         files will be saved
#' @param Save_hash Option between FALSE (default) en TRUE. If TRUE the Hash
#'          mapping will be saved in a map higher than the export files.
#' @param Network_directory Location of Network
#' @param Extract_location name of the map were extracts are saved.
#'          Default is "6. Extracten wp"
#' @param Export_name Start of the name of your exported results
#'          Default is "Export_Student_Analytics_"
#'
#' +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' TODO:
#' 1) Functie herschrijven: vereenvoudigen; kolommen kunnen toevoegen
#' 2) Assertion per jaar en per opleiding?
#' 3) generiek maken van kolomen
#'
#' +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @export
export_analysisset <- function(Faculty_selection = 'All',
                               Study_selection ='All',
                               Phase_selection = 'All',
                               Academic_year = 'All',
                               Category_selection = NULL,
                               Enrollment_year = 'All',
                               Enrollment_year_EOI = 'All',
                               Column_selection = NULL,
                               Name_project,
                               Special_personal_data = FALSE,
                               Hash = TRUE,
                               Export_results = FALSE,
                               Save_as_CSV = FALSE,
                               Analysis_set_input = NULL,
                               Analysis_set_option = NULL,
                               Documentation_new_variables = NULL,
                               Qualityrapport = TRUE,
                               Make_zip = FALSE,
                               Save_hash = FALSE,
                               Network_directory = NULL,
                               Extract_location = "/6. Extracten WP/",
                               Export_name = "Export_Student_Analytics_") {

  ## These are all columns and fieldnames from the VUSA analysisset
  INS_Periode_afgerond <- INS_Periode <- Veldnaam <- Categorie <- Omschrijving <-
    Voorkomende_waarden <- Extra_info <- INS_Opleidingsnaam_2002 <-
    INS_Inschrijvingsjaar <- INS_Faculteit <- INS_Opleidingsfase_BPM <-
    INS_Studiejaar <- INS_Studentnummer <- INS_Inschrijvingsjaar_EOI <- NULL

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

  ## These columns always have to be present, these are keys of the VUSA analysisset
  Required_columns <- c('INS_Studentnummer',
                        'INS_Opleidingsnaam_2002',
                        'INS_Inschrijvingsjaar')

  if(!is.null(Column_selection)) {
    Column_selection <- append(Required_columns,
                               Column_selection)
  }

  # no duplicated columns
  Column_selection <- unique(Column_selection)

  ## Read, if necessary, an Analyseset with selected columns
  if (!is.null(Analysis_set_input)) {
    Analysis_set <- Analysis_set_input
  } else if (!is.null(Analysis_set_option) && Analysis_set_option == 7) {
    Analysis_set <- get_analysisset_7(cols = Column_selection)
    Analysis_set <- Analysis_set %>%
      dplyr::filter(INS_Periode_afgerond == T,
                    !(INS_Periode == 0)
      )
  } else {
    Analysis_set <- get_analysisset(columns = Column_selection)
  }

  Analysis_set_documentation <- documentation_get()

  ## Make documentation for new variables that are not already in the
  ## documentation (like participant ID of researchers in effectstudy)
  ## Only relevant if the Analyseset is made beforehand (So not NULL)
  if (!is.null(Documentation_new_variables) & !is.null(Analysis_set_input)) {
    rows_analysisset <- names(Analysis_set)
    rows_documentation <- Analysis_set_documentation$Veldnaam
    rows_not_in_documentation <- dplyr::setdiff(rows_analysisset, rows_documentation)
    data_documentation <- Analysis_set %>%
      dplyr::select(rows_not_in_documentation)
    new_documentation <- documentation_generate(data_documentation,
                                                description = NA,
                                                source = NA,
                                                extra_info = "Variable is added for export")
    Analysis_set_documentation <- dplyr::bind_rows(Analysis_set_documentation, new_documentation)

  }

  Analysis_set_documentation_no_bpg <- subset(Analysis_set_documentation,
                                              !(substr(Veldnaam,1,3) %in% c("ETN",
                                                                            "FBP",
                                                                            "VRZ")))

  ## Make filters based on variables and presence of current Analysisset
  if (length(Faculty_selection) == 1) {
    if (Faculty_selection == 'All') {
      Faculty_selection <- unique(Analysis_set$INS_Faculteit)
    } else {
      Faculty_selection <- Faculty_selection
    }
  } else {
    Faculty_selection <- Faculty_selection
  }
  print(Faculty_selection)

  if (length(Study_selection) == 1) {
    if (Study_selection == 'All') {
      Study_selection <- unique(Analysis_set$INS_Opleidingsnaam_2002)
    }
  }
  print(Study_selection)

  if ('All' %in% Phase_selection) {
    Phase_selection <- unique(Analysis_set$INS_Opleidingsfase_BPM)
  }

  if ('All' %in% Academic_year) {
    Academic_year <- unique(Analysis_set$INS_Studiejaar)
  }

  if (!is.null(Category_selection)) {
    if ('All' %in% Category_selection) {
      Category_selection <- unique(Analysis_set_documentation$Categorie)
    }
  }
  if ('All' %in% Enrollment_year) {
    Enrollment_year <- unique(Analysis_set$INS_Inschrijvingsjaar)
  }
  if ('All' %in% Enrollment_year_EOI) {
    Enrollment_year_EOI <- unique(Analysis_set$INS_Inschrijvingsjaar_EOI)
  }

  ## (De)select special personal data
  if (Special_personal_data == FALSE ) {
    Analysis_set_documentation <- Analysis_set_documentation_no_bpg
    print('Analysisset contains no special personal data')
  } else {
    Analysis_set_documentation_bpg <- subset(Analysis_set_documentation, substr(Veldnaam,1,3) %in% Special_personal_data)
    Analysis_set_documentation <- rbind(Analysis_set_documentation_no_bpg, Analysis_set_documentation_bpg)
    print(paste('Analysisset contains special personal data: ', paste(Special_personal_data, collapse = ", "), sep = ""))
  }

  ## Select the necessary column names from the documentation and filter on this in the analysisset
  Analysis_set_documentation_export <-
    subset(
      Analysis_set_documentation,
      Categorie %in% Category_selection |
        Veldnaam %in% Column_selection|
        Veldnaam %in% names(Analysis_set),
      ## NB: This is select as 'argument' in the subset-function, not dplyr::select
      select = c(
        Categorie,
        Veldnaam,
        Omschrijving,
        Voorkomende_waarden,
        Extra_info
      )
    )

  ## Sort the documentation
  Analysis_set_documentation_export <- Analysis_set_documentation_export[order(Analysis_set_documentation_export$Categorie,
                                                                               Analysis_set_documentation_export$Veldnaam),]



  ## Filter the analysisset with earlier made filters.
  Analysis_set <- subset(Analysis_set,
                         INS_Opleidingsnaam_2002 %in% Study_selection &
                           INS_Inschrijvingsjaar %in% Enrollment_year &
                           INS_Inschrijvingsjaar_EOI %in% Enrollment_year_EOI &
                           INS_Faculteit %in% Faculty_selection &
                           INS_Opleidingsfase_BPM %in% Phase_selection &
                           INS_Studiejaar %in% Academic_year)
  table(Analysis_set$INS_Faculteit)

  ## Count the amount of rows in the start; these have to be the same in the end
  Start_amount_rows <- nrow(Analysis_set)

  ## make eventual results.
  if (Export_results == TRUE) {
    Results <- export_results(Analysis_set)
  }

  ## hashing
  Hash_mapping <- NULL
  if (Hash == TRUE) {
    # Make a hash mapping table
    Hash_mapping <- subset(Analysis_set, select = INS_Studentnummer)
    Hash_mapping <- unique(Hash_mapping)
    Hash_mapping$Hash <- hash_var(Hash_mapping$INS_Studentnummer)

    # Translate studentnumbers to hashed studentnumbers
    Analysis_set <- dplyr::left_join(Analysis_set, Hash_mapping)
    Analysis_set$INS_Studentnummer <- Analysis_set$Hash
    Analysis_set$Hash <- NULL

    if (Export_results == TRUE) {
      Results <- dplyr::left_join(Results, Hash_mapping)
      Results$INS_Studentnummer <- Results$Hash
      Results$Hash <- NULL
    }
  }


  ## Determine current date
  current_date <- paste0("/", Sys.Date(), "/")

  ## Determine file name
  if (Hash == TRUE) {
    Filename <- paste(Name_project, "hashed", sep = "_")
  } else {
    Filename <- Name_project
  }

  ## Make the correct folders for saving the exports
  # projectfolder <- paste(vvcommander::sa_branch_get(), Extract_location, Name_project, sep = '')
  projectfolder <- paste(Extract_location, Name_project, sep = '')
  projectfolder_today <- paste0(projectfolder, current_date)
  dir.create(paste0(Network_directory, vvcommander::sa_branch_get(), projectfolder))
  dir.create(paste0(Network_directory, vvcommander::sa_branch_get(), projectfolder_today))


  ## Save hash mapping if present and indicated
  if (Hash == TRUE && Save_hash == TRUE) {
    ## Save this in a map higher than the export file
    ## If there is a zip made for the export files, these do not need to be in the zip.
    saverds_csv(Hash_mapping,
                paste0("/Hash_mapping_", Sys.Date()),
                output = projectfolder)
    rm(Hash_mapping)
  } else if (Hash == TRUE) {
    rm(Hash_mapping)
  }


  ## Save the export and documentation
  saverds_csv(Analysis_set,
              paste(Export_name, Sys.Date(), "_", Filename, sep = ''),
              # dataloc = projectfolder_today,
              output = projectfolder_today,
              save_csv = TRUE)
  saverds_csv(Analysis_set_documentation_export,
              paste("Documentation_", Export_name, Sys.Date(), "_", Filename, sep = ''),
              # dataloc = projectfolder_today,
              output = projectfolder_today,
              save_csv = TRUE)

  ## Make Qualityrapport
  ## Because of the function that is used (Datamaid::makeCodebook)
  ## this has to be saved inside the function 'create_quality_report'
  if (Qualityrapport) {
    create_quality_report(
      Analysis_set,
      ProjectName = Name_project,
      path = paste0(vvcommander::sa_branch_get(), projectfolder_today))
  }

  # Save the results
  if (Export_results == TRUE) {
    # Create the correct folders to save the export
    Results_map <- paste0(projectfolder_today, "/Results")
    dir.create(paste0(Network_directory, vvcommander::sa_branch_get(), Results_map))
    # Sla de export op
    saverds_csv(Results,
                paste(Export_name, "Results_", Sys.Date(), "_", Filename, sep = ''),
                output = paste(Results_map, "/", sep = ""), save_csv = Save_as_CSV)
    # Throw away created variables
    rm(Results, Results_map)
  }

  ## Make and save a zip if indicated
  ## NB: A zip from a can take a while.
  if(Make_zip == TRUE) {
    Zip_folder_target <- paste0("/", Sys.Date(), "_", Name_project, ".zip")
    utils::zip(paste0(Network_directory, vvcommander::sa_branch_get(),
                      projectfolder, Zip_folder_target),
               paste0(Network_directory, vvcommander::sa_branch_get(),
                      projectfolder_today),extras = '-j')
  }

  ## Count the amount of rows again
  count_rows_analysis_set(Analysis_set, Name_project, Start_amount_rows, type = "Export")

  # Throw away created variables
  rm(Analysis_set_documentation_export,
     Analysis_set_documentation,
     Analysis_set,
     Analysis_set_documentation_no_bpg,
     projectfolder,
     current_date)

  if (Special_personal_data == TRUE ) {
    rm(Analysis_set_documentation_bpg)
  }
}
