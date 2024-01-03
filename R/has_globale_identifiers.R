#' Has globale identifiers
#'
#' Check whether dataframe has "globale identifiers".
#' In the scope of the vu student analytics project, these are:
#' INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Opleidingscode_actueel,
#' INS_Inschrijvingsjaar, INS_Inschrijvingsjaar_EOI.
#' @param df Data frame.
#' @export
has_globale_identifiers <- function(df) {
  global_identifiers <- NULL
    Identifiers_in_df <- dplyr::intersect(global_identifiers, names(df))

    if (!purrr::is_empty(Identifiers_in_df)) {
        return(Identifiers_in_df)
    }
}
