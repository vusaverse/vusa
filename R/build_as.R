#' build_as
#'
#' A function used to update the analysis set based on an archived version
#' and a list containing r scripts.
#'
#' @param Analysis_set the Analysis set
#' @param AS_archive the archived version of the analysis set
#' @param latest latest date analysis set was built
#' @param files_list list of files to run
#' @param complete_rebuild logical, set to TRUE to always completely rebuild AS, regardless of changes
#' @return Analysis set
#' @export
build_as <- function(Analysis_set, AS_archive, latest, files_list, complete_rebuild = FALSE) {
  .Deprecated("sculpt_as")
  Network_directory <- "G:/DSZ/SA2016/Datasets/"
  date_AS_archive <- format(file.mtime(latest), "%Y-%m-%d")
  ## Check if there are new registrations and the entire analysis set needs to be run again
  if (nrow(Analysis_set) == nrow(AS_archive) & !complete_rebuild) {
    ## Number of registrations has remained the same
    ## leave most recent AS with the columns to add as the AS
    Analysis_set <- AS_archive
    files_list_unchanged <- list()
    ## check for all files that are used or that have been updated and therefore there is new data
    for (file in files_list) {
      ## main_file is the file that will be added
      main_file <- file[1]
      ## Determine the last updated date of the main file
      main_file_date <- format(file.mtime(main_file), "%Y-%m-%d")
      ## determine the input files of the main_file
      input_file <- vvauditor::determine_inputfiles(main_file)
      ## Determine the last updated date of those input files
      input_date <- format(file.mtime(paste0(Network_directory, "Output/main/", input_file)), "%Y-%m-%d")

      print(input_date)
      ## check whether it is an input to which new columns are also added
      if (length(file) > 1) {
        new_columns <- file[-1]
        # There may also be new code in the main file, so if it has been updated, run again
        if (main_file_date >= date_AS_archive) {
          print(paste0(main_file, " has been updated, meaning: run this data again"))
          Analysis_set <- vusa::Build_Analysis_set(Analysis_set, main_file, new_columns)
        }
        ##Update the AS because the input files have been updated
        ## (for loop because multiple input files can be used in a script and therefore also multiple data)
        else if (any(input_date >= date_AS_archive)) {
          print(paste0(main_file, " input file has been updated, meaning: run this data again"))
          Analysis_set <- vusa::Build_Analysis_set(Analysis_set, main_file, new_columns)
        }
        else {
          files_list_unchanged[[length(files_list_unchanged) +1 ]] <- file
        }

      }
      ## input without new columns.
      else {
        if (any(input_date >= date_AS_archive)) {
          print(paste0(file, "input file has been updated, meaning: run this data again"))
          Analysis_set <- vusa::Build_Analysis_set(Analysis_set, main_file)
        }
        else if (main_file_date >= date_AS_archive) {
          print(paste0(main_file, " has been updated, meaning: run this data again"))
          Analysis_set <- vusa::Build_Analysis_set(Analysis_set, main_file)
        }
        else {
          files_list_unchanged[[length(files_list_unchanged) +1 ]] <- file
        }
      }

    }
    ##
    print("the following scripts have no changed files:")
    for (file in files_list_unchanged) {
      print(file)
    }
    # otherwise run all build files
  } else {
    print("A new entry has been added, so everything has to be run again")
    for (file in files_list) {
      # check again if it is a file where new columns are added
      if (length(file) > 1) {
        Analysis_set <- vusa::Build_Analysis_set(Analysis_set, file[1], file[-1])

      } else {
        Analysis_set <- vusa::Build_Analysis_set(Analysis_set, file)
      }
    }
  }

  return(Analysis_set)
}
