#' Validate a script project
#'
#' This function validates a script project by checking for warnings and errors,
#' retrieving commit information, calculating the number of lines and size of the script,
#' and validating its style. It can optionally export the results to a dataframe.
#'
#' @param filepath A character string specifying the path to the script file to validate.
#' If not provided, it defaults to the current working directory.
#' @param export_to_dataframe A logical value indicating whether to export the validation
#' results to a dataframe. Default is FALSE.
#'
#' @return A tibble with various validation results. If \code{export_to_dataframe} is TRUE,
#' the results are returned as a dataframe. Otherwise, a summary of the results is printed.
#'
#' @examples
#' \dontrun{
#' # Validate a script with default parameters
#' validate_script_proj()
#'
#' # Validate a specific script and export the results to a dataframe
#' validate_script_proj(filepath = "/path/to/script.R", export_to_dataframe = TRUE)
#' }
#'
#' @export
#' 
validate_script_proj <- function(filepath = NULL, export_to_dataframe = FALSE) {
  start.time <- Sys.time()
  
  if (is.null(filepath)) {
    filepath <- this.path::this.path()
  }
  
  if (export_to_dataframe == FALSE) {
    validate_scipt_addin(filepath, export_to_dataframe)
  } else {
    warnings_errors_messages <- validate_no_warning_errors(
      filepath,
      export_to_dataframe
    )
    error_message <- warnings_errors_messages$error
    # warning_message <- warnings_errors_messages$warning
    warning_message <- as.character(list(warnings_errors_messages$warning))
    
    ## TODO: Check for warnings boolean
    # warning_result --> boolean
    error <- warnings_errors_messages$result
    
    last_commit_date <- get_last_n_commit_date(filepath)
    
    git_info <- get_last_n_commit_info(filepath)
    last_commit_author <- git_info$author
    last_commit_hash <- git_info$hash
    last_commit_message <- git_info$message
    
    script_number_of_lines <- length(readLines(filepath))
    script_size <- file.size(filepath)
    
    style <- validate_style(filepath, export_to_dataframe)
    
    style_issues <- !is.na(style)
    
    end.time <- Sys.time()
    
    execution_time <- end.time - start.time
    
    ## These should be used in a wrapper function, as they are project specific
    assert_naming <- any(grep("assert_naming", readLines(filepath)))
    clear_script_objects <- any(grep("clear_script_objects", readLines(filepath)))
    
    return(tibble::tibble(
      assert_naming,
      clear_script_objects,
      error,
      error_message,
      execution_time,
      last_commit_author,
      last_commit_date,
      last_commit_hash,
      last_commit_message,
      script_number_of_lines,
      script_size,
      style,
      style_issues,
      warning_message
    ))
  }
}

#' Validate a script addin
#'
#' This function validates a script project by checking for warnings and errors,
#' retrieving commit information, calculating the number of lines and size of the script,
#' and validating its style. It can optionally export the results to a dataframe.
#'
#' @param filepath A character string specifying the path to the script file to validate.
#' If not provided, it defaults to the current working directory.
#' @param export_to_dataframe A logical value indicating whether to export the validation
#' results to a dataframe. Default is FALSE.
#'
#' @return A tibble with various validation results. If \code{export_to_dataframe} is TRUE,
#' the results are returned as a dataframe. Otherwise, a summary of the results is printed.
#'
#' @examples
#' \dontrun{
#' # Validate a script with default parameters
#' validate_script_addin()
#'
#' # Validate a specific script and export the results to a dataframe
#' validate_script_addin(filepath = "/path/to/script.R", export_to_dataframe = TRUE)
#' }
#'
#' @export
#' 
validate_script_addin <- function(filepath, export_to_dataframe = FALSE) {
  validate_no_warning_errors(filepath, export_to_dataframe)
  
  validate_assertions_present(filepath, export_to_dataframe)
  
  validate_write_files(filepath)
  
  validate_introduction(filepath, export_to_dataframe)
  
  validate_clear_script_objects(filepath, export_to_dataframe)
  
  if (check_installed_package("lintr", check = TRUE)) {
    validate_style(filepath, export_to_dataframe)
  } else {
    base::cat(cli::style_bold(cli::col_cyan("Install lintr using \"library(\"lintr\")\" to test for style")))
  }
  
  compare_input_output(filepath, export_to_dataframe)
  
  base::cat("\n")
  clear_script_objects(bestandspad = filepath, envir = globalenv())
}

#' Retrieve the last N commit information for a file
#'
#' This function uses the `git log` command to retrieve the last N commit
#' information for a specified file. The information includes the commit hash,
#' author, and commit message.
#'
#' @param filepath A character string specifying the path to the file for which
#' commit information is to be retrieved.
#' @param n An integer specifying the number of commit information to retrieve.
#' The default is  1, which retrieves the most recent commit information.
#'
#' @return A flat list of commit information, including the commit hash, author,
#' and message for the last N commits.
#'
#' @note This function currently only works on Windows systems due to the use of
#' the `shell()` function.
#'
#' @examples
#' \dontrun{
#' # Retrieve the last commit information for a file
#' get_last_n_commit_info(filepath = "/path/to/file.R")
#'
#' # Retrieve the last  5 commit information for a file
#' get_last_n_commit_info(filepath = "/path/to/file.R", n =  5)
#' }
#'
#' @export
get_last_n_commit_info <- function(filepath, n = 1) {
  # Execute the git log command and capture the output
  git_output <- shell(paste0("git log -", n, ' --format=%H%n%an%n%s -- "', filepath, '"'), intern = TRUE)
  
  # Initialize an empty data frame to store the commit information
  commit_info_df <- data.frame(
    hash = character(),
    author = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
  
  # Loop through the git log output and extract the commit information
  for (i in 1:length(git_output)) {
    if (i %% 3 == 1) {
      # Add the commit information to the data frame
      commit_info_df <- rbind(commit_info_df, data.frame(
        hash = git_output[i],
        Nauthor = git_output[i + 1],
        message = git_output[i + 2],
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Return a flat list of commit information
  return(as.list(commit_info_df))
}




#' Strip punctuation characters from variable name
#'
#' Function to strip the punctuation characters
#'
#' @param x Variable name to strip
#'
strip_names <- function(x) {
  x <-
    rex::re_substitutes(x, rex::rex(start, some_of(".", quote, "`", "%", "$", "@")), "")
  x <-
    rex::re_substitutes(x, rex::rex(some_of(quote, "`", "<", "-", "%", "$", "@"), end), "")
  x
}

#' Make lint object
#'
#' Function to make lint objects for the Validate_variable_name
#'
#' @param expr XML parsed content
#' @param source_file XML document
#'
make_lint_object <- function(expr, source_file) {
  name <- strip_names(as.character(xml2::xml_find_first(
    expr,
    "./text()"
  )))
  if (identical(name, "") | is.na(name)) {
    return(NULL)
  }
  
  if (!exists(name, envir = globalenv(), inherits = FALSE)) {
    return(NULL)
  }
  
  new_name <- check_variable(name)
  
  if (new_name == name) {
    return(NULL)
  }
  ## Turn the XML2 object into a list to more easily get the attributes
  symbol <- xml2::as_list(expr)
  lint_msg <- paste0("Variable name should be ", new_name)
  lintr::Lint(
    filename = source_file$filename,
    line_number = as.integer(attr(symbol, "line1")),
    column_number = as.integer(attr(symbol, "col1")),
    message = lint_msg,
    # type = "style",
    line = source_file$file_lines[as.numeric(attr(symbol, "line1"))]
    # ranges = list(as.numeric(c(
    # attr(symbol, "col1"), attr(symbol, "col2")
    # )))
  )
}


#' Lint function to check variable names
#'
#' Function to generate lint functionconcerning variable names not following the "stijlgids"
#'
variable_name_lint <- function() {
  lintr::Linter(function(source_file) {
    if (exists("file_lines", source_file)) {
      x <- source_file$full_xml_parsed_content
    } else {
      return()
    }
    
    ## Get all the variable assignments "<-/="
    xpath <-
      paste0(
        "//expr[SYMBOL or STR_CONST][following-sibling::LEFT_ASSIGN or following-sibling::EQ_ASSIGN]/*",
        " | ",
        "//expr[SYMBOL or STR_CONST][preceding-sibling::RIGHT_ASSIGN]/*",
        " | ",
        "//SYMBOL_FORMALS"
      )
    variable_assignemnts <- xml2::xml_find_all(x, xpath)
    lapply(variable_assignemnts, make_lint_object, source_file)
  })
}


#' Validate script style
#'
#' This function tests a script for style errors using the lintr package.
#'
#' @param filepath The complete filepath of the script.
#' @param export_to_dataframe A logical value indicating whether to export results to a data frame. Default is FALSE.
#'
#' @family Script validation
#'
#' @examples
#' \dontrun{
#' # Validate the style of a script
#' validate_style(filepath = "/path/to/script.R")
#'
#' # Validate the style of a script and export the results to a dataframe
#' validate_style(filepath = "/path/to/script.R", export_to_dataframe = TRUE)
#' }
validate_style <-
  function(filepath, export_to_dataframe = FALSE) {
    ## Display a message indicating the style test is in progress
    if (export_to_dataframe == FALSE) {
      base::cat("Style check in progress: ")
    }
    ## Create a lintr object with the tests that lintr should perform on style
    ## These tests are designed to comply with the style guide.
    ## For more information, see: https://cran.r-project.org/web/packages/lintr/lintr.pdf
    output <-
      dplyr::as_tibble(lintr::lint(
        filepath,
        lintr::linters_with_defaults(
          commented_code_linter = NULL,
          pipe_continuation_linter = NULL,
          object_name_linter = NULL,
          # no_tab_linter = NULL,
          object_usage_linter = NULL,
          object_length_linter = NULL,
          line_length_linter = lintr::line_length_linter(100)
        )
      ))
    
    dfVariable_names <- dplyr::as_tibble(lintr::lint(
      filepath,
      variable_name_lint()
    ))
    output <- rbind(output, dfVariable_names)
    
    output_improved <-
      output %>%
      arrange(line_number) %>%
      unite("messages", line, message, sep = " ") %>%
      unite("line", line_number, messages, sep = " : ") %>%
      select(filename, line) %>%
      group_by(filename) %>%
      summarise(across(everything(), str_c, collapse = "\n"))
    ## Add exclusions to the "line_length_linter". These ensure
    ## that strings (text within double quotes) do not trigger a warning
    ## if they exceed  50 characters. The same applies to variables (text between spaces).
    if (base::nrow(output) >  0) {
      tmp <- output %>% filter(linter == "line_length_linter")
      if (nrow(tmp) >  0) {
        tmp %>%
          mutate(
            length_of_string =
              nchar(
                stringi::stri_extract_all_regex(line, '(?<=").*?(?=")')
              )
          ) %>%
          mutate_if(is.integer, ~ replace(., is.na(.),  0)) %>%
          mutate(max_length_of_object_name = max(nchar(unlist(
            strsplit(trimws(line), "\\s+")
          )))) %>%
          filter(length_of_string >=  50 |
                   max_length_of_object_name >=  50)
      }
      output <- output %>%
        filter(!line_number %in% tmp$line_number) %>%
        arrange(line_number)
    }
    
    ## Check if there are style errors and report the test result
    ## as a marker
    if (base::nrow(output) >  0) {
      if (export_to_dataframe == FALSE) {
        base::cat(cli::style_bold(cli::col_red("Style errors detected. See Markers\n")))
      }
      markers <- output %>%
        select(
          type,
          filename,
          line_number,
          column_number,
          message
        ) %>%
        as.data.frame()
      colnames(markers) <-
        c("type", "file", "line", "column", "message")
      if (export_to_dataframe == TRUE) {
        return(output_improved$line)
      } else {
        rstudioapi::sourceMarkers(output$filename, markers = markers)
      }
    } else {
      if (export_to_dataframe == TRUE) {
        return(NA_character_)
      } else {
        base::cat(cli::style_bold(cli::col_green("No style errors found\n")))
      }
    }
  }



#' Validate a script for warnings and errors
#'
#' This function checks if a script produces any warnings or errors.
#'
#' @param filepath The complete file path of the script to be tested.
#' @param export_to_dataframe A logical value indicating whether to export results to a data frame. Default is FALSE.
#'
#' @return A list with warnings, errors, and messages. If \code{export_to_dataframe} is TRUE, the result is a list with the warning and error messages. Otherwise, a summary of the results is printed to the console.
#'
#' @examples
#' \dontrun{
#' # Validate a script for warnings and errors
#' validate_no_warning_errors(filepath = "/path/to/script.R")
#'
#' # Validate a script and export the results to a dataframe
#' validate_no_warning_errors(filepath = "/path/to/script.R", export_to_dataframe = TRUE)
#' }
#'
#' @export
validate_no_warning_errors <- function(filepath, export_to_dataframe = FALSE) {
  ## Construct three boolean variables to track the absence of errors, warnings, and messages
  no_error <- TRUE
  no_warning <- TRUE
  no_message <- TRUE
  
  ## Determine where Clear_script is run and remove it from the file so it is not executed
  lines <- scan(filepath, what = character(), sep = "\n", quiet = TRUE)
  clear_line <- grep("clear_script_object", lines)[1]
  if (!is.na(clear_line)) {
    lines <- lines[1:clear_line -  1]
  }
  
  ## Function to capture error messages generated by tryCatch into a list
  catchToList <- function(expr) {
    val <- NA
    myWarnings <- NA
    wHandler <- function(w) {
      myWarnings <<- c(myWarnings, paste(format(Sys.time()), w$message))
      invokeRestart("muffleWarning")
    }
    myError <- NA
    
    eHandler <- function(e) {
      myError <<- paste("Error:", format(Sys.time()), capture.output(e), collapse = "\n")
      NA
    }
    val <-
      tryCatch(
        withCallingHandlers(expr, warning = wHandler),
        error = eHandler
      )
    if (length(myWarnings) >  1) {
      myWarnings <- myWarnings[!is.na(myWarnings)]
    }
    
    list(warning = myWarnings, error = myError)
  }
  
  ## Execute the script using tryCatch to capture any errors, warnings, and messages
  ## The boolean values are updated to remember that there were undesired results in case of an error/warning or message
  if (export_to_dataframe == FALSE) {
    errors <-
      catchToList(eval(parse(text = lines), envir = globalenv()))
    
    ## Print the result of the test to the console
    base::cat("Warnings/ errors/ messages: ")
    for (message in na.omit(errors$warning)) {
      cat(base::cat(cli::col_red(message)), "\n")
    }
    for (message in na.omit(errors$error)) {
      cat(base::cat(cli::col_red(message)), "\n")
    }
    if (all(is.na(errors$warning)) && all(is.na(errors$error))) {
      base::cat(cli::style_bold(cli::col_green("Passed\n")))
    } else {
      base::cat(cli::style_bold(
        cli::col_red("Failed. See Message(s) above\n")
      ))
    }
  } else {
    # Save warning and error messages into an object
    errors <-
      catchToList(source(filepath))
    # If there are no warning/error messages, then the result is "Passed"
    if (all(is.na(errors$warning)) && all(is.na(errors$error))) {
      errors$result <- FALSE
    } else {
      errors$result <- TRUE
    }
    return(errors)
  }
}
