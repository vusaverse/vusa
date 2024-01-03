#' Join Tkoppel isat
#'
#' Een functie om opleidingsnaam (en eventueel de faculteit) uit Tkoppel_isat
#' toe te voegen aan het dataframe.
#' Om deze functie uit te voeren is het nodig dat in de outputfolder het volgende
#' bestand staat: "1. Ingelezen data/INS_Tkoppel_isat.rds"
#'
#' @param df Een data frame dat de numerieke column INS_Opleidingscode_actueel bevat,
#' en waaraan de opleidingsnaam 2002 moet worden toegevoegd.
#' @param faculteit Als FALSE (default) wordt de column INS_Faculteit niet toegevoegd.
#' Als TRUE wordt deze column wel toegevoegd.
#' @return df
#' @export
join_tkoppel_isat <- function(df, faculteit = F) {
  INS_Faculteit <- NULL

    ## Bestaat de column INS_Opleidingscode_actueel, en is die numeriek?

    if (!any(names(df) %in% "INS_Opleidingscode_actueel")) {
        stop("The column INS_Opleidingscode_actueel does not exists in the data frame")
    }

    if (!is.numeric(df$INS_Opleidingscode_actueel)) {
        stop("The column INS_Opleidingscode_actueel is not numeric")
    }

    ## Bestaat de column INS_Opleidingsnaam_2002 of INS_Faculteit?

    if (any(names(df) %in% "INS_Opleidingsnaam_2002")) {
        stop("The column INS_Opleidingsnaam_2002 already exists in the data frame")
    }

    if (faculteit == T) {

        if (any(names(df) %in% "INS_Faculteit")) {
            stop("The column INS_Faculteit already exists in the data frame")
        }

    }

    ## Lees de Tkoppel isat tabel in, om de opleidingsnaam toe te voegen
    Tkoppel_isat <- vvmover::readrds_csv(output = "1. Ingelezen data/INS_Tkoppel_isat.rds")

    ## Voeg opleidingsnaam toe aan df: koppel df met Tkoppel_isat en verwijder faculteit
    df <- df %>%
        strict_left_join(Tkoppel_isat, by = "INS_Opleidingscode_actueel")

    if (faculteit == F) {
        df <- df %>%
            dplyr::select(-INS_Faculteit)
    }

    return(df)

}
