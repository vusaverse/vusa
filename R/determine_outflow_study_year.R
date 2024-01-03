#' determine_outflow_study_year
#'
#' Bepaal uitstroom voor het betreffende studiejaar obv
#' uiteindelijke uitstroom en studiejaar
#' @param SUC_Type_uitstroom Soort uitstroom
#' @param INS_Studiejaar Jaar van uitstroom
#' @param OPL_Studielast_nominaal Nominale hoeveelheid studielast (EC)
#'
#' @export
determine_outflow_study_year <-
    function (SUC_Type_uitstroom, INS_Studiejaar, OPL_Studielast_nominaal) {

        ## Maak onderscheid tussen nominaal en uitval
        SUC_Afgestudeerd <-
            ifelse(startsWith(SUC_Type_uitstroom, "Nominaal"), TRUE, FALSE)

        ## Haal het cijfer uit de SUC_Type_Uitstroom variabele
        ## VB: "Nominaal +1" wordt 1, "Uitval jaar 3" wordt 3
        SUC_Type_uitstroom_studiejaar <-
            ifelse(is.na(SUC_Type_uitstroom) | SUC_Type_uitstroom == "Nominaal",
                   NA,
                   utils::tail(stringr::str_split(
                       SUC_Type_uitstroom, " "
                   )[[1]], n = 1))
        ## Doe de variabele met nominaal + nominale studiejaar om zo het studiejaar te krijgen
        SUC_Type_uitstroom_studiejaar <-
            dplyr::case_when(
                SUC_Afgestudeerd == T &&
                    !is.na(SUC_Type_uitstroom_studiejaar) ~ as.double(SUC_Type_uitstroom_studiejaar) + (OPL_Studielast_nominaal / 60),
                SUC_Afgestudeerd == T &&
                    is.na(SUC_Type_uitstroom_studiejaar) ~ (OPL_Studielast_nominaal / 60),
                TRUE ~ as.double(SUC_Type_uitstroom_studiejaar)
            )
        ## Bepaal per studiejaar of dit ook uitstroomjaar was, indien dat niet
        ## het geval is, wordt dit één groep (Nog studerend)
        SUC_Type_uitstroom_studiejaar <-
            dplyr::case_when(
                is.na(SUC_Type_uitstroom_studiejaar) |
                    SUC_Type_uitstroom_studiejaar != INS_Studiejaar ~ "Nog studerend",
                SUC_Afgestudeerd == TRUE ~ "Diploma",
                SUC_Afgestudeerd == FALSE ~ "Uitval"
            )
    }


#' determine_outflow_study_year
#'
#' Applies determine_outflow_study_year() for each row in a dataframe.
#' @param df Data frame
#'
#' @export
wrapper_determine_outflow_study_year <- function(df) {
  SUC_Type_uitstroom <- INS_Studiejaar <- OPL_Studielast_nominaal <- NULL
    df <- df %>%
        mutate(SUC_Type_uitstroom_studiejaar = purrr::pmap_chr(
            list(
                SUC_Type_uitstroom,
                INS_Studiejaar,
                OPL_Studielast_nominaal
            ),
            determine_outflow_study_year
        ))

}
