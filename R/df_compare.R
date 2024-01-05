#' df compare
#'
#' Compare two dataframes in order to check whether their properties are identical.
#'
#' @param df1 First dataframe
#' @param df2 Second dataframe
#'
#' @return A dataframe with comparisons between the input dataframes..
#' @export
df_compare <- function(df1, df2) {
  type_df1 <- type_df2 <- variable <- Type_equal <- NULL

  df1_inspect <- df_inspect(df1, print_results = F) %>%
    dplyr::mutate_if(is.factor, as.character)

  df2_inspect <- df_inspect(df2, print_results = F) %>%
    dplyr::mutate_if(is.factor, as.character)

  ## apply join and add the suffixes _df1 and _df2
  df_merged <- dplyr::full_join(df1_inspect,
                                df2_inspect,
                                by = "variable",
                                suffix = c("_df1", "_df2")) %>%
    ## Check for equal types
    dplyr::mutate(Type_equal = type_df1 == type_df2) %>%
    ## Sort variable names
    dplyr::select(variable,
                  Type_equal,
                  sort(dplyr::current_vars()))
  return(df_merged)
}
