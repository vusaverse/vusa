#' Read RDS CSV
#'
#' a generic function that can read an R object into an RDS, fst and CSV.
#' First determine the extension and then apply the appropriate read function
#'
#' @param dataloc Data location on the network drive, starting from "G:/DSZ/SA2016/Datasets/"
#' @param output Data location on the network drive, depending on the branch
#' set.starting from "G:/DSZ/SA2016/Datasets/Output/(branch)/"
#' @param readr Use readr to read CSV files
#' @param print TRUE/FALSE: Print the date on which the imported file was last modified
#' @param fix.encoding TRUE/FALSE: Apply an encoder to the columns
#' @param encoding Which encoding to use
#' @param ... Pass arguments for readr, read.csv2, fst or readRDS
#' @return Dataframe
#' @importFrom dplyr %>%
#' @export
readrds_csv <- function(dataloc = "", output, readr = FALSE, print = FALSE, fix.encoding = FALSE, encoding = "latin1", ...){
  if (missing(output) == FALSE) {
    ## The output is written per branch
    dataloc <- paste("Output/", Sys.getenv("BRANCH"), "/", output, sep = "")
  }
  ## determine the file path, by combining the network directory with
  ## the dataloc parameter.
  file_loc <- paste(Sys.getenv("NETWORK_DIR"), dataloc, sep = "")

  ## Print the date the file was last modified.
  if (print) {
    print_last_modified(file_loc)
  }

  ## Determine the file extension
  extension <- sub('.*\\.', '', dataloc)
  if (extension == "csv") {
    if (readr == TRUE) {
      ## Use readr if specified. It is hereby possible to
      ## override the function arguments with the ... arguments
      function_args <- list(file = file_loc,
                            trim_ws = T,
                            delim = ";",
                            locale = readr::locale(decimal_mark = ",",
                                             grouping_mark = "."))
      ## overwrite the ... arguments and then run the function
      function_args <- overwrite_dot_arguments(function_args, ...)
      if (fix.encoding) {
        df <- fixencoding(do.call(readr::read_delim, function_args), encoding)
        return(df)
      } else{
        df <- do.call(readr::read_delim, function_args)
        return(df)
      }

    } else {
      if (fix.encoding) {
        df <- fixencoding(utils::read.csv2(file_loc, ...), encoding)
        return(df)
      } else {
        df <- utils::read.csv2(file_loc, ...)
        return(df)
      }
    }
  }
  else if (extension == "fst") {
    if (fix.encoding) {
      df <- fixencoding(fst::read_fst(file_loc, ...) %>%
                    dplyr::mutate_if(is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))}), encoding)
      return(df)
    } else{
      df <- fst::read_fst(file_loc, ...) %>%
        dplyr::mutate_if(is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))})
      return(df)
    }
  }
  else if (extension == "rds") {
    if (fix.encoding) {
      df <- fixencoding(readRDS(file_loc, ...), encoding)
      return(df)
    } else {
      df <- readRDS(file_loc, ...)
      return(df)}
  } else {
    message("abort")
  }

}
