#' check education name
#'
#' Check whether the specified dataframe uses education names that
#' do not appear in the original education names table.
#' If there are other training name names, an error is returned.
#' If the training names all match, a confirmation is given.
#' @param df A data frame
#' @param actual_edu_names a column with the actual education names, default is null,
#' than it will check for system variables
#' @param col_edu_names a column with the education names that you want to check,
#' default is null, than it will check for system variables
#' @family tests
#' @export
check_education_name <- function(df, actual_edu_names = NULL, col_edu_names = NULL) {

  ## Carry out checks
  stopifnot(is.data.frame(df))
  stopifnot(col_edu_names %in% names(df))

  if (is.null(col_edu_names)) {
    print("col_edu_names not given, so look for system variables")
    if (!Sys.getenv("EDU_COLUMN") == "") {
      print("system variables present, so these wil be used for col_edu_names")
      col_edu_names <- Sys.getenv("EDU_COLUMN")
    }
    else {
      stop("system variables for col_edu_names is missing")
    }
  }

  if (is.null(actual_edu_names)) {
    print("actual_edu_names not given, so look for system variables")
    if (!Sys.getenv("EDU_DATA") == "") {
      print("system variables present, so these wil be used for actual_edu_names")

      # Read in edu_data to create a list of education names
      edu_data <- readrds_csv(output = Sys.getenv("EDU_DATA"))
      actual_edu_names <- unique(edu_data$INS_Opleidingsnaam_Z08)
    }
    else {
      stop("system variables for actual_edu_names is missing")
    }
  }

  ## =============================================== ==============================
  ## List unique education names from the specified dataframe and its education column
  Edu_x <- unique(df[[col_edu_names]])
  ## Remove NAs from the list
  Edu_x <- Edu_x[!is.na(Edu_x)]
  ## Check if there are any education names in the specified dataframe, which
  ## do not appear in the actual_edu_names column
  if (any(!Edu_x %in% actual_edu_names)) {
    ## If this is the case, the otherwise coded education names
    ## returned.
    Wrong_edu <- !Edu_x %in% actual_edu_names
    Wrong_edu <- Edu_x[Wrong_edu]
    ## Print the training names that are not in actual_edu_names
    md_list(Wrong_edu, "The following education names do not exist")
    stop(paste("There are wrong education names in the column", col_edu_names))
  }
  ## if the previous condition is not true, the test is passed
  else {
    print("Test is passed, all education names are correct")
  }
}


#' MD list
#'
#' Print a list in markdown.
#'
#' @param x A vector with a length greater than 0.
#' @param header Indicates whether a header is present.
#' @param ordered Defaults to FALSE, if TRUE then prefix is "." otherwise "-".
#'
#' @export
md_list <- function(x, header = NULL, ordered = F){
  ## x must be a vector with length greater than 0
  stopifnot(is.vector(x))
  stopifnot(length(x) > 0)

  ## Print a header if specified
  if (exists("header", inherits = FALSE)) {
    cat("**",header,"** \n\n", sep = "")
  }

  ## For each item in the vector
  for (i in 1:length(x)) {
    # Determine the prefix for each list item
    if (ordered) {
      pre <- paste0(i, ".")
    } else {
      pre <- "-"
    }
    cat(pre, x[i], " \n")
  }

}
