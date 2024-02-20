#' @title df_inspect_without_warnings
#' @description Wrapper for df_inspect to suppress warnings.
#' @param df Dataframe
#' @return suppressWarnings(df_inspect(df, print_results = F))
#' @examples \dontrun{
#' df_inspect_without_warnings(df)
#' }
#' @export
df_inspect_without_warnings <- function(df) {
  dfInspect <- suppressWarnings(df_inspect(df, print_results = F))
  return(dfInspect)
}
