#' @title Inspect given data frame in Student Analytics VU.
#' @description For each variable it returns: Quantity and percentage of zeros (q_zeros and p_zeros respectevly). Same metrics for NA values (q_NA/p_na), and infinite values (q_inf/p_inf). Last two columns indicates data type and quantity of unique values.
#' The standard deviation, variance, Q1/Median/Q3 & IQR, Min/Max and Mean.
#' This function prints and returns the results. It is based on the funModeling package.
#' @param data data frame
#' @param print_results if FALSE then there is not a print in the console, TRUE by default.
#' @param dump Dump the output to the XX. Inspect folder
#' @param dump_location Path with location to save the dump file, default is NULL
#' @return Metrics data frame
#' @export
df_inspect <- function(data, print_results, dump = F, dump_location = NULL) {
  if (missing(print_results)) {
    print_results <- T
  }

  df_inspect_res <- data.frame(
    q_zeros = sapply(data, function(x) sum(x == 0, na.rm = T)), # Quantity of zeros
    p_zeros = round(100 * sapply(data, function(x) sum(x == 0, na.rm = T)) / nrow(data), 2), # Percentage of zeros
    q_na = sapply(data, function(x) sum(is.na(x))), # Quantity of NA's
    p_na = round(100 * sapply(data, function(x) sum(is.na(x))) / nrow(data), 2), # Percentage of NA's
    q_inf = sapply(data, function(x) sum(is.infinite(x))), # Quantity of infinite values
    p_inf = round(100 * sapply(data, function(x) sum(is.infinite(x))) / nrow(data), 2), # Percentage of infinite values
    type = sapply(data, get_type_v), # Type
    unique = sapply(data, function(x) sum(!is.na(unique(x)))), # unique values
    sd = sapply(data, function(x) ifelse(is.numeric(x), round(stats::sd(x, na.rm = TRUE), 2), NA)), # SD
    var = sapply(data, function(x) ifelse(is.numeric(x), round(stats::var(x, na.rm = TRUE), 2), NA)), # SD
    q1 = sapply(data, function(x) ifelse(is.numeric(x), round(stats::quantile(x, 0.25, na.rm = TRUE), 2), NA)), # Q1
    median = sapply(data, function(x) ifelse(is.numeric(x), round(stats::median(x, na.rm = TRUE), 2), NA)), # Median
    q3 = sapply(data, function(x) ifelse(is.numeric(x), round(stats::quantile(x, 0.75, na.rm = TRUE), 2), NA)), # Q3
    iqr = sapply(data, function(x) ifelse(is.numeric(x), round(stats::IQR(x, 0.75, na.rm = TRUE), 2), NA)), # IQR
    min = sapply(data, function(x) ifelse(is.numeric(x), round(min(x, na.rm = TRUE), 2), NA)), # Min
    max = sapply(data, function(x) ifelse(is.numeric(x), round(max(x, na.rm = TRUE), 2), NA)), # Max
    mean = sapply(data, function(x) ifelse(is.numeric(x), round(mean(x, na.rm = TRUE), 2), NA)) # Mean
  )

  ## check for sys variables if dump_location is null
  if (is.null(dump_location) && dump == T) {
    print("dump_location is missing, so we need to use system variables")
    if (!any(Sys.getenv(c("OUTPUT_DIR", "INSPECT_DIR")) == "")) {
      print("Neccesary system variables existing, these will be dump_location")
      dump_location <- paste(Sys.getenv("OUTPUT_DIR"), current_git_branch(), Sys.getenv("INSPECT_DIR"), sep = "")
    } else {
      stop("system variables OUTPUT_DIR and INSPECT_DIR are not in your system variables")
    }
  }

  ## Create new variable for column name
  df_inspect_res$variable <- rownames(df_inspect_res)
  rownames(df_inspect_res) <- NULL

  ## Reordering columns
  df_inspect_res <- df_inspect_res[, c(18, 1:17)]

  ## Dump output
  if (dump) {
    current_time <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
    utils::write.csv2(
      df_inspect_res,
      paste(dump_location, "/Inspect_", current_time, ".csv", sep = "")
    )
  }

  ## Print or return results
  if (print_results) {
    ## Temporarily increase max.print setting.
    max.print.tmp <- getOption("max.print")
    options(max.print = 99999999)
    ## print
    print(df_inspect_res)
    ## Reset max.print setting.
    options(max.print = max.print.tmp)
  } else {
    return(df_inspect_res)
  }
}

is.POSIXct <- function(x) {
  return(inherits(x, "POSIXct"))
}
is.POSIXlt <- function(x) {
  return(inherits(x, "POSIXlt"))
}
is.POSIXt <- function(x) {
  return(inherits(x, "POSIXt"))
}


get_type_v <- function(x) {
  ## handler for posix object, because class function returns a list in this case
  posix <- ifelse(is.POSIXct(x), "POSIXct", "")
  posix <- ifelse(is.POSIXlt(x), paste(posix, "POSIXlt", sep = "/"), posix)
  posix <- ifelse(is.POSIXt(x), paste(posix, "POSIXt", sep = "/"), posix)

  # ifnot posix..then something else
  if (posix == "") {
    cl <- class(x)
    return(ifelse(length(cl) > 1, paste(cl, collapse = "-"), cl))
  } else {
    return(posix)
  }
}
