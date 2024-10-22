#' Create Documentation for a Data Frame
#'
#' This function creates documentation for a given data frame, including field names,
#' types, missing values, limits, patterns, and unique identifiers.
#'
#' @param df A data frame to document
#'
#' @return A tibble containing the documentation
#'
#' @export
create_dataframe_documentation <- function(df) {
  field_names <- names(df)
  field_types <- purrr::map_chr(df, first_class)
  p_nas <- colMeans(is.na(df)) * 100
  lower <- purrr::map_dbl(df, ~ifelse(all(is.na(.)), NA, minnum(.)))
  upper <- purrr::map_dbl(df, ~ifelse(all(is.na(.)), NA, maxnum(.)))
  pattern <- purrr::map_chr(df, match_kolom_met_patroon)
  unique <- purrr::map_lgl(field_names, is_unieke_identifier, df = df)
  in_use <- rep(TRUE, length(field_names))
  percentages <- purrr::map_chr(df, get_ratio)
  distribution <- purrr::map_chr(df, get_dist)
  
  documentation <- tibble::tibble(
    field_name = field_names,
    field_type = field_types,
    p_na = p_nas,
    lower = lower,
    upper = upper,
    pattern = pattern,
    unique = unique,
    percentages = percentages,
    distribution = distribution
  )
  
  return(documentation)
}

