#' Check columns for double rows
#'
#' Check which columns lead to double rows in a data frame.
#'
#' @param data Data frame.
#' @param group_vars a vector containing the column names to group by.
#' In the scope of the vu student analytics project, these columns are:
#' c(INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar)
#'
#' @return table with the results.
#' @export
check_columns_for_double_rows <- function(data,
                                          group_vars = c(
                                            "INS_Studentnummer",
                                            "INS_Opleidingsnaam_2002",
                                            "INS_Inschrijvingsjaar"
                                          )) {
  variable <- value <- Double_count <- NULL

  result <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(group_vars))) %>%
    dplyr::summarise_all(dplyr::n_distinct) %>%
    dplyr::ungroup() %>%
    tidyr::gather(variable, value, -dplyr::one_of(group_vars)) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(Double_count = count_more_than_1(value)) %>%
    dplyr::filter(Double_count > 0) %>%
    dplyr::arrange(dplyr::desc(Double_count))

  return(result)
}
