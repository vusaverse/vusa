#' Load and Save Configuration Project
#'
#' This function is responsible for loading and saving configurations for a project. It retrieves
#' the configuration path from the system environment variable "load_and_save_config".
#'
#' @param find_loc_in_env if true (the default), a system env variable can be given as a location
#' within the settings config file can be.
#' @return A data frame of the load and save configurations if the configuration file is valid.
#' If there are issues with the configuration file, the function issues a warning and returns NULL.
#'
#' @note For more information on how to use this function, see the vignette:
#' vignette("Using the load_and_save_config_proj function") and more elaborate documentation at:
#'  https://r.mtdv.me/load_save_config.
#'
#' @export
#' @examples
#' \dontrun{
#' load_and_save_config_proj()
#'
#'
#' ## Create a data frame with the given column names
#' colnames_config <- c(
#'   "script_dir",
#'   "type",
#'   "load_data_dir",
#'   "load_base_data_dir",
#'   "save_data_dir",
#'   "save_base_data_dir",
#'   "add_branch",
#'   "save_csv",
#'   "save_rds",
#'   "save_fst",
#'   "read_extension",
#'   "load_message",
#'   "save_message",
#'   "notes"
#' )
#' }

#' # Initialize a data frame
#' load_and_save_config_df <- data.frame(matrix(ncol = length(colnames_config), nrow = 0))
#' names(load_and_save_config_df) <- colnames_config

#' # Populate the data frame with some dummy data (you would replace this with your own data)
#' load_and_save_config_df[1,] <- c(
#'  "dir/of/script", # Used for str_detect the current path of full script
#'   NA_character_,
#'   "specific_load_dir_based_on_script_dir",
#'   "load/base/dir",
#'   "specific_save_dir_based_on_script_dir",
#'   "save/base/dir",
#'   FALSE,
#'   TRUE,
#'   TRUE,
#'   TRUE,
#'   "rds",
#'   NA_character_, # This is shown once per session if filled
#'   NA_character_, # This is shown once per session if filled
#'   "Some notes"
#' )
#' }

load_and_save_config_proj <- function(find_loc_in_env = TRUE) {
  # set variables
  script_dir <- type <- n <- non_unique <- save_rds <- save_csv <- NULL
  read_extension <- supported_extension <- NULL

  load_and_save_config_path <- Sys.getenv("load_and_save_config")

  supported_read_extensions <- c("rds", "csv", "fst")


  documentation_reference <- "See documentation at:\n https://r.mtdv.me/load_save_config."

  if (load_and_save_config_path == "") {
    rlang::warn("You have not set load and save settings",
      .frequency = "once", .frequency_id =
        "load_and_save_config_no_env_var"
    )
    return(NULL)
  }

  tryCatch(
    error = function(e) {
      rlang::warn(
        paste0(
          "Your load_and_save_config_path doesn't lead to a correct semicolon separated csv.\n",
          documentation_reference,
        ),
        .frequency = "once",
        .frequency_id = "load_and_save_config_file_path"
      )
      return(NULL)
    },
    {
      load_and_save_config_df <- readr::read_delim(load_and_save_config_path, delim = ";", show_col_types = FALSE)

      if (find_loc_in_env == TRUE) {
        load_and_save_config_df <- load_and_save_config_df %>%
          dplyr::mutate(dplyr::across(dplyr::ends_with("data_dir"), ~ dplyr::if_else(.x %in% names(Sys.getenv()),
            Sys.getenv(.x),
            .x
          )))
      }
    }
  )

  # TODO This is been required in the config file
  colnames_config <-
    c(
      "script_dir",
      "type",
      "load_data_dir",
      "load_base_data_dir",
      "save_data_dir",
      "save_base_data_dir",
      "add_branch",
      "save_csv",
      "save_rds",
      "save_fst",
      "read_extension",
      "load_message",
      "save_message",
      "notes"
    )

  ## Two potential issues with colnames
  colnames_config_warning <- FALSE
  if (length(colnames_config) != length(names(load_and_save_config_df))) {
    colnames_config_warning <- TRUE
  } else if (all(names(load_and_save_config_df) != colnames_config)) {
    colnames_config_warning <- TRUE
  }

  if (colnames_config_warning == TRUE) {
    rlang::warn(
      paste0(
        "Your load and save config file has other column names than expected.\n",
        "See the config file at:\n",
        load_and_save_config_path,
        "\n",
        documentation_reference
      ),
      .frequency = "once",
      .frequency_id = "load_and_save_config_colnames"
    )
    return(NULL)
  }

  ## Test duplicates
  duplications_df <- load_and_save_config_df %>%
    dplyr::group_by(script_dir, type) %>%
    dplyr::tally() %>%
    dplyr::filter(n > 1)

  if (nrow(duplications_df) > 0) {
    duplications_str <- duplications_df %>%
      dplyr::mutate(non_unique = dplyr::if_else(n == 1, NA_character_, paste(script_dir, type, sep = " - "))) %>%
      dplyr::pull(non_unique)

    duplication_info_single_str <- paste(duplications_str, collapse = " | ")

    rlang::warn(
      paste0(
        "The following directory-types appear more than once:\n",
        duplication_info_single_str,
        "\n",
        documentation_reference
      ),
      .frequency = "once",
      .frequency_id = "load_and_save_config_duplicated"
    )
    return(NULL)
  }

  script_dirs_without_save <- load_and_save_config_df %>%
    dplyr::mutate(save = purrr::pmap(list(save_rds, save_csv, save_fst), sum)) %>%
    dplyr::filter(save == 0) %>%
    dplyr::pull(script_dir)

  if (length(script_dirs_without_save > 0)) {
    rlang::warn(
      paste0(
        "The following directories don't have any save_* argument on TRUE:\n",
        script_dirs_without_save,
        "\n",
        documentation_reference
      ),
      .frequency = "once",
      .frequency_id = "load_and_save_config_"
    )
    return(NULL)
  }

  script_dirs_without_supported_read_extension <- load_and_save_config_df %>%
    dplyr::mutate(supported_extension = read_extension %in% supported_read_extensions) %>%
    dplyr::filter(supported_extension == FALSE) %>%
    dplyr::pull(script_dir)

  if (length(script_dirs_without_supported_read_extension > 0)) {
    rlang::warn(
      paste0(
        "The following directories don't have a supported read extension:",
        paste(script_dirs_without_supported_read_extension),
        "\n",
        "Supported read extension are:",
        paste(supported_read_extensions),
        "\n",
        documentation_reference
      ),
      .frequency = "once",
      .frequency_id = "load_and_save_config_duplicated"
    )
    return(NULL)
  }


  return(load_and_save_config_df)
}


#' Filter Settings Based on Type
#'
#' Filters the settings script directory based on the supplied settings type.
#' Handles edge cases and provides appropriate messages and errors.
#'
#' @param settings_script_dir Data frame containing the settings script directory.
#' @param settings_type Character, the type of setting to filter on. If NULL, the function will select the first type.
#'
#' @return A data frame containing the filtered settings script directory.
#' @export
filter_settings_on_type <- function(settings_script_dir, settings_type) {
  if (is.null(settings_type)) {
    # No settings
    if (nrow(settings_script_dir) > 1) {
      rlang::inform(
        paste0(
          "The first type is selected from the following possibilties:\n",
          dplyr::pull(settings_script_dir$type)
        ),
        .frequency = "once",
        .frequency_id = "write_file_proj_type"
      )

      settings_script_dir <- settings_script_dir %>%
        dplyr::slice_head(n = 1)
    }
  } else if (!is.null(settings_type)) {
    settings_script_dir_type <- settings_script_dir %>%
      dplyr::filter(type = settings_type)

    ## Guard clause
    if (nrow(settings_script_dir_type) == 0) {
      if (nrow(settings_script_dir == 1)) {
        rlang::abort(
          "The supplied type is not available. Please remove the type parameter.\n",
          "This will select the only values for this script directory."
        )
      }
      if (nrow(settings_script_dir >= 1)) {
        rlang::abort(paste0(
          "The supplied type is not available. The available types are:\n",
          dplyr::pull(settings_script_dir$type),
          "\n",
          "Remove the type parameter for the first choice or provide one of the given values."
        ))
      }
    }

    # The good case
    settings_script_dir <- settings_script_dir_type
  }
  return(settings_script_dir)
}


#' Read File Project
#'
#' Reads a file from the project with extension-based reading functions, and applies specified encoding fixes if necessary.
#' Utilizes both manual and configuration settings for reading the file.
#'
#' @param name Character, name of the file to be read.
#' @param settings_df Data frame containing settings, defaults to the result of `load_and_save_config_proj()`.
#' @param settings_type Character, the type of setting to use, default is NULL.
#' @param dir Character, directory path, default is NULL.
#' @param extension Character, file extension, default is NULL.
#' @param fix_encoding Logical, whether to fix encoding, default is FALSE.
#' @param encoding Character, encoding to be used, default is "latin1".
#' @param sub_dir Character, sub-directory, default is NA_character_.
#' @param csv_readr_function_args List, additional arguments for reading CSV files, default specified.
#' @param ... Additional arguments to be passed to the reading functions.
#'
#' @return The data read from the specified file.
#' @export
read_file_proj <- function(
    name,
    settings_df = load_and_save_config_proj(),
    settings_type = NULL,
    dir = NULL,
    extension = NULL,
    fix_encoding = FALSE,
    encoding = "latin1",
    sub_dir = NA_character_,
    csv_readr_function_args = list(
      trim_ws = T,
      delim = ";",
      locale = readr::locale(
        decimal_mark = ",",
        grouping_mark = "."
      )
    ),
    ...) {
  # set variables
  script_dir <- NULL

  if (Sys.getenv("RSTUDIO") == "1") {
    docname <- basename(rstudioapi::getActiveDocumentContext()$path)
    path <- rstudioapi::getActiveDocumentContext()$path
    directory <- stringr::str_remove(path, docname)
  } else {
    docname <- basename(scriptName::current_filename())
    directory <- this.path::this.dir()
  }

  # Check the manual and config setting for completeness
  manual_settings <- FALSE
  config_settings <- TRUE

  if (sum(!is.null(dir), !is.null(extension)) < 2) {
    manual_settings <- FALSE
  }

  if (is.null(settings_df)) {
    config_settings <- FALSE
  }

  if (!is.null(settings_df)) {
    settings_script_dir <- settings_df %>%
      dplyr::filter(stringr::str_detect(directory, script_dir))

    if (nrow(settings_script_dir) == 0) {
      config_settings <- FALSE
    } else {
      settings_script_dir <- filter_settings_on_type(settings_script_dir, settings_type)

      if (!is.na(settings_script_dir$load_message)) {
        rlang::inform(settings_script_dir$load_message,
          .frequency = "once",
          .frequency_id = "read_file_proj_message"
        )
      }
    }
  }

  if (sum(manual_settings, config_settings) == 0) {
    rlang::abort(paste0(
      "You have not given enough variables to save the file properly.\n",
      "Either add a correct config with current directory in it or set at least",
      "the dir and one of the save_* arguments"
    ))
  }


  # dir_final <- NULL

  # Set full path for read
  if (is.null(dir)) {
    base_data_dir <- settings_script_dir[["load_base_data_dir"]]
    data_sub_dir <- settings_script_dir[["load_data_dir"]]

    if (settings_script_dir[["add_branch"]] == TRUE) {
      branch <- system("git branch --show-current", intern = TRUE)

      dir_elements <- c(base_data_dir, branch, data_sub_dir, sub_dir)
      dir_elements <- dir_elements[!is.na(dir_elements)]

      dir <- paste(dir_elements, collapse = "/")
    } else {
      dir_elements <- c(base_data_dir, data_sub_dir, sub_dir)
      dir_elements <- dir_elements[!is.na(dir_elements)]

      dir <- paste(base_data_dir, data_sub_dir, sep = "/")
    }
  }

  if (is.null(extension)) {
    extension <- settings_script_dir[["read_extension"]]
  }

  file_name <- paste(name, extension, sep = ".")
  full_path <- paste(dir, file_name, sep = "/")

  # read with function based on extension
  if (extension == "rds") {
    if (fix_encoding == TRUE) {
      df <- fixencoding(readRDS(full_path, ...), encoding)
    } else {
      df <- readRDS(full_path, ...)
    }
  }

  if (extension == "csv") {
    if (fix_encoding) {
      df <- fixencoding(
        readr::read_delim(
          file = full_path,
          csv_readr_function_args,
          ...
        ),
        encoding
      )
    } else {
      df <- readr::read_delim(
        file = full_path,
        csv_readr_function_args,
        ...
      )
    }
  }

  if (extension == "fst") {
    if (fix_encoding) {
      df <- fixencoding(fst::read_fst(full_path, ...) %>%
        dplyr::mutate_if(is.character, .funs = function(x) {
          return(`Encoding<-`(x, "UTF-8"))
        }), encoding)
    } else {
      df <- fst::read_fst(full_path, ...) %>% dplyr::mutate_if(is.character,
        .funs = function(x) {
          return(`Encoding<-`(x, "UTF-8"))
        }
      )
    }
  }

  return(df)
}


#' Write File to Project
#'
#' Writes an object to a file in the project directory with specified saving options (RDS, CSV, or FST).
#' Utilizes both manual and configuration settings for writing the file.
#'
#' @param object The object to be written.
#' @param name Character, name of the file.
#' @param settings_df Data frame containing settings, defaults to the result of `load_and_save_config_proj()`.
#' @param settings_type Character, the type of setting to use, default is NULL.
#' @param dir Character, directory path, default is NULL.
#' @param save_rds Logical, whether to save as RDS, default is NULL.
#' @param save_csv Logical, whether to save as CSV, default is NULL.
#' @param save_fst Logical, whether to save as FST, default is NULL.
#' @param sub_dir Character, sub-directory, default is NA_character_.
#' @param rds_version Numeric, version for RDS files, default is 2.
#' @param csv_na Character, NA representation for CSV files, default is "".
#' @param csv_sep Character, separator for CSV files, default is ";".
#' @param csv_dec Character, decimal separator for CSV files, default is ",".
#' @param fst_compress Numeric, compression level for FST files, default is 100.
#' @param ... Additional arguments to be passed to the saving functions.
#'
#' @return NULL, the file is saved to the specified path.
#' @export
write_file_proj <- function(
    object,
    name,
    settings_df = load_and_save_config_proj(),
    settings_type = NULL,
    dir = NULL,
    save_rds = NULL,
    save_csv = NULL,
    save_fst = NULL,
    sub_dir = NA_character_,
    rds_version = 2,
    csv_na = "",
    csv_sep = ";",
    csv_dec = ",",
    fst_compress = 100,
    ...) {
  # set variables
  script_dir <- NULL

  if (Sys.getenv("RSTUDIO") == "1") {
    docname <- basename(rstudioapi::getActiveDocumentContext()$path)
    path <- rstudioapi::getActiveDocumentContext()$path
    directory <- stringr::str_remove(path, docname)
  } else {
    docname <- basename(scriptName::current_filename())
    directory <- this.path::this.dir()
  }

  # Check the manual and config setting for completeness
  manual_settings <- FALSE
  config_settings <- TRUE


  # Check the manual setting and config
  if (sum(!is.null(dir), max(!is.null(save_rds), !is.null(save_csv), !is.null(save_fst))) < 2) {
    manual_settings <- FALSE
  }

  if (is.null(settings_df)) {
    config_settings <- FALSE
  }

  if (!is.null(settings_df)) {
    settings_script_dir <- settings_df %>%
      dplyr::filter(stringr::str_detect(directory, script_dir))

    if (nrow(settings_script_dir) == 0) {
      config_settings <- FALSE
    } else {
      # Deal with type, this generates an error if type is incorrectly supplied
      settings_script_dir <- filter_settings_on_type(settings_script_dir, settings_type)

      if (!is.na(settings_script_dir$save_message)) {
        rlang::inform(settings_script_dir$save_message,
          .frequency = "once",
          .frequency_id = "write_file_proj_message"
        )
      }
    }
  }

  if (sum(manual_settings, config_settings) == 0) {
    rlang::abort(paste0("You have not given enough variables to save the file properly.\n,
                          Either add a correct config with current directory in it or set at least
                          the dir and one of the save_* arguments"))
  }


  # Set save variables
  if (is.null(dir)) {
    base_data_dir <- settings_script_dir[["save_base_data_dir"]]
    data_sub_dir <- settings_script_dir[["save_data_dir"]]

    if (settings_script_dir[["add_branch"]] == TRUE) {
      branch <- system("git branch --show-current", intern = TRUE)

      dir_elements <- c(base_data_dir, branch, data_sub_dir, sub_dir)
      dir_elements <- dir_elements[!is.na(dir_elements)]

      dir <- paste(dir_elements, collapse = "/")
    } else {
      dir_elements <- c(base_data_dir, data_sub_dir, sub_dir)
      dir_elements <- dir_elements[!is.na(dir_elements)]

      dir <- paste(dir_elements, collapse = "/")
    }
  }

  if (dir.exists(dir) == FALSE) {
    rlang::abort(paste0(
      "The constructed directory doesn't exist.\n",
      "Run dir.create(", dir, ", recursive = TRUE) or change the input or config file."
    ))
  }

  name <- paste(dir, name, sep = "/")

  if (is.null(save_csv) && config_settings) {
    save_rds <- settings_script_dir[["save_rds"]]
  } else {
    save_rds <- FALSE
  }

  if (is.null(save_csv) && config_settings) {
    save_csv <- settings_script_dir[["save_csv"]]
  } else {
    save_csv <- FALSE
  }

  if (is.null(save_csv) && config_settings) {
    save_fst <- settings_script_dir[["save_fst"]]
  } else {
    save_fst <- FALSE
  }


  if (save_csv) {
    data.table::fwrite(object,
      paste0(name, ".csv"),
      na = csv_na,
      sep = csv_sep,
      dec = csv_dec
    )
  }

  if (save_fst) {
    fst::write_fst(object,
      paste0(name, ".fst"),
      compress = fst_compress,
      ...
    )
  }

  if (save_rds) {
    saveRDS(object,
      paste0(name, ".rds"),
      version = rds_version,
      ...
    )
  }
}
