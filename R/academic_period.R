#' Academic period
#'
#' In this function a date is translated to the academic year or period
#' in which that date falls. This is based on the periods contained in the file
#' Dates.csv which is an given dataset in the package vvconverter.
#' This information is entered manually maintained on the basis of the
#' academic year calendar of the VU. However, the user can also use his own created
#' dataset, with the same column names, by using the argument date_table
#'
#' @param x A date, or vector with multiple dates. POSIXct is also accepted.
#' @param type Default: "period": Returns the academic period.
#' at "year" the academic year is returned.
#' @param date_table A table in which the correct academic year and period can
#' be found. When the argument is null, Dates.csv from vvconverter will be used.
#' @return The academic year or period in which the specified date falls.
#' @family vector calculations
#' @importFrom dplyr %>%
#' @export
academic_period <- function(x, type = "period", date_table = NULL){
  Measure_date_prev <- Measure_date <- Academic_year <- Period <- NULL

  ## Convert POSIXct and POSIXt to date class
  if (any(class(x) %in% c("POSIXct", "POSIXt"))) {
    x <- lubridate::as_date(x)
  }

  ## if the class is not a date, then this function cannot be executed
  stopifnot(class(x) == "Date")

  if (is.null(date_table)) {
    date_table <- vvconverter_data("Dates.csv")
  }

  ## Add the temporary variable TMP = T so that a cartetic product
  ## can be made
  date_table <- date_table %>%
    dplyr::mutate(TMP = T)

  ## For performance reasons, create a tibble of all unique dates in the vector first
  unique_df <- dplyr::tibble(x = unique(x)) %>%
    ## Add the temporary variable TMP = T so that a cartetic product
    ## can be made
    dplyr::mutate(TMP = T) %>%
    ## Create cartetic product by joining Dates on TMP
    dplyr::left_join(date_table, by = "TMP") %>%
    ## Filter the rows where x corresponds to the key dates
    dplyr::filter(x >= Measure_date_prev,
                  x < Measure_date) %>%
    dplyr::select(x, Academic_year, Period)

  ## Join the calculated period and registration year to a tibble with vector
  ## x
  x_df <- dplyr::tibble(x = x) %>%
    dplyr::left_join(unique_df, by = "x")

  ## Return the value, depending on the parameter type passed
  if (type == "period") {
    y <- x_df$Period
  }
  if (type == "year") {
    y <- x_df$Academic_year
  }

  ## If the used date_table file is not filled enough (future date has not yet been filled in),
  ## throw an error indicating that it:
  ## date_table file must be completed with the new academic year.
  if (sum(is.na(x)) != sum(is.na(y))) {
    warning("Dates.csv file must be completed with the new academic year")
  }

  return(y)
}
