## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Transitiematrix functies.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2020 VU
## Web Page: http://www.vu.nl
## Contact: Theo Bakker (t.c.bakker@vu.nl)
## Verspreiding buiten de VU: Ja
##
## Doel: Een script dat alle functies bevat voor het bouwen van een transitiematrix
##
## Afhankelijkheden: Geen
##
## Datasets: Geen
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 24-06-2020: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#' @title maak_transitiematrix_grid
#' @description Functie om een Transitiematrix_grid te maken van bachelor studenten aan de VU van de faculteiten "BETA", "FGB", "FGW", "FRT", "FSW", "GNK", "RCH", "SBE", "THK". Er wordt verschil gemaakt tussen "Hoofdinschrijving" en "Inschrijving na 1 oktober" en soorten uitstroom.
#' @return Transitiematrix van alle bachelor studenten aan de VU
#' @examples maak_transitiematrix_grid()
#' @importFrom purrr as_vector map
#' @importFrom readr parse_number
#' @importFrom dplyr mutate
#' @export
maak_transitiematrix_grid <- function() {

  ## Basisgegevens
  INS_Opleidingsfase_BPM <- c("B")
  INS_Faculteit <- c("BETA", "FGB", "FGW", "FRT", "FSW", "GNK", "RCH", "SBE", "THK")
  OPL_Studielast_nominaal <- c(180)
  INS_Hoofdneven <- c("Hoofdinschrijving", "Inschrijving na 1 oktober")

  ## Uitval, Diploma, Herinschrijving in jaar 1-9
  Maak_SUC_Soort_en_jaar_uitstroom <- function(soort) {
    1:9 %>%
      map(~ paste(soort, "jaar", .x)) %>%
      as_vector()
  }
  SUC_Soort_en_jaar_uitstroom <-
    map(c("Uitval", "Diploma", "Herinschrijving"),
        Maak_SUC_Soort_en_jaar_uitstroom) %>%
    as_vector()

  df <- expand.grid(
    INS_Opleidingsfase_BPM,
    INS_Faculteit,
    OPL_Studielast_nominaal,
    INS_Hoofdneven,
    SUC_Soort_en_jaar_uitstroom,
    stringsAsFactors = FALSE
  )
  names(df) <- c(
    "INS_Opleidingsfase_BPM",
    "INS_Faculteit",
    "OPL_Studielast_nominaal",
    "INS_Hoofdneven",
    "SUC_Soort_en_jaar_uitstroom"
  )

  ## Voeg een kolom Studiejaar toe
  df <- df %>%
    mutate(INS_Studiejaar = parse_number(SUC_Soort_en_jaar_uitstroom))

  return(df)
}

#' @title bepaal_subtotalen_met_transitiematrix
#' @description Functie om subtotalen te berekenen met een transitiematrix
#' @param df Dataframe
#' @param aantal Soorten uitstroom
#' @return Transitiematrix in lang formaat
#' @examples \dontrun{bepaal_subtotalen_met_transitiematrix(df, aantal)}
#' @export
bepaal_subtotalen_met_transitiematrix <- function(df, aantal) {
  SUC_Soort_uitstroom_aantal <- Herinschrijving <- Uitval <-
    SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm <- SUC_Soort_uitstroom <- NULL

  ## Bewaar het totaal in een variabele om zo te kunnen gebruiken
  thisMatrix <- df %>%

    ## Klap om naar een wijd formaat en bewaar de stroomvariabelen
    tidyr::pivot_wider(names_from = SUC_Soort_uitstroom,
                values_from = SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm) %>%
    dplyr::select(Uitval:Herinschrijving) %>%

    ## Maak er een matrix van en bereken de aantallen
    data.matrix() * aantal

  ## Zet de matrix om naar een lang formaat
  thisMatrix <- thisMatrix %>%

    ## Converteer naar een dataframe
    dplyr::as_tibble %>%

    ## Bewaar de rijen in de variabele INS_Studiejaar
    tibble::rowid_to_column(var = "INS_Studiejaar") %>%

    ## Plaats de waarden van de matrix terug
    tidyr::pivot_longer(cols = Uitval:Herinschrijving,
                 names_to = "SUC_Soort_uitstroom",
                 values_to = "SUC_Soort_uitstroom_aantal") %>%

    ## Rond de waarden af naar hele getallen
    mutate(SUC_Soort_uitstroom_aantal = round(SUC_Soort_uitstroom_aantal, 0))
  return(thisMatrix)
}

#' @title voeg_transitie_subtotalen_toe
#' @description Functie om de transitie aantallen toe te voegen aan de dataset (alleen nodig bij ongeneste data)
#' @param df_lang Lange data frame (bvb output van bepaal_subtotalen_met_transitiematrix)
#' @param df Dataframe
#' @return De transitiematrix na 'df %>% left_join(df_lang)' plus nog andere aanpassingen
#' @examples \dontrun{voeg_transitie_subtotalen_toe(df_lang, df)}
#' @export
voeg_transitie_subtotalen_toe <- function(df_lang, df) {
  SUC_Soort_uitstroom_aantal <- SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm <-
    SUC_Soort_en_jaar_uitstroom <- OPL_Studielast_nominaal <- INS_Faculteit <-
    INS_Opleidingsfase_BPM <- INS_Studiejaar <- SUC_Soort_uitstroom <- NULL
  df <- df %>%
    dplyr::left_join(df_lang,
              by = c("INS_Studiejaar", "SUC_Soort_uitstroom")) %>%

    ## Reconstrueer SUC_Soort_en_jaar_uitstroom
    mutate(SUC_Soort_en_jaar_uitstroom = paste(SUC_Soort_uitstroom,
                                               "jaar",
                                               INS_Studiejaar)) %>%
    dplyr::select(
      INS_Opleidingsfase_BPM,
      INS_Faculteit,
      OPL_Studielast_nominaal,
      INS_Studiejaar,
      SUC_Soort_en_jaar_uitstroom,
      SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm,
      SUC_Soort_uitstroom_aantal
    )
  return(df)
}

#' @title maak_transitiematrix_uitstroom
#' @description Functie om een Transitiematrix voor uitstroom te maken
#' @param df Dataframe
#' @return dfTransitiematrix_uitstroom
#' @examples \dontrun{maak_transitiematrix_uitstroom(df)}
#' @export
maak_transitiematrix_uitstroom <- function(df) {
  INS_Opleidingsfase_BPM <- INS_Faculteit <- OPL_Studielast_nominaal <-
    INS_Hoofdneven <- SUC_Soort_en_jaar_uitstroom <- SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm <-
    PRG_Totaal <- dfTransitiematrix_grid <- Groepeervariabelen <-
    SUC_Soort_uitstroom <- PRG_Subtotaal <-  NULL

  dfTransitiematrix_uitstroom <- df %>%

    ## Degroepeer
    dplyr::ungroup() %>%
    ## Tijdelijk filter
    ## filter(INS_Faculteit == "BETA") %>%
    dplyr::select(
      INS_Opleidingsfase_BPM,
      INS_Faculteit,
      OPL_Studielast_nominaal,
      INS_Hoofdneven,
      SUC_Soort_en_jaar_uitstroom,
      SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm,
      PRG_Totaal,
      PRG_Subtotaal

    ) %>%

    ## Bepaal het studiejaar
    mutate(INS_Studiejaar = parse_number(SUC_Soort_en_jaar_uitstroom)) %>%

    ## Sorteer en maak uniek
    dplyr::distinct() %>%

    ## Join het df met het grid, zodat alle combinaties voorkomen
    dplyr::right_join(
      dfTransitiematrix_grid,
      by = c(
        "INS_Opleidingsfase_BPM",
        "INS_Faculteit",
        "OPL_Studielast_nominaal",
        "INS_Hoofdneven",
        "SUC_Soort_en_jaar_uitstroom",
        "INS_Studiejaar"
      )
    ) %>%

    ## Groepeer en sorteer op de Groepeervariabelen
    dplyr::group_by_at(Groepeervariabelen) %>%
    dplyr::arrange_at(Groepeervariabelen) %>%

    ## Vul de missende waarden op van PRG_Totaal (voorwaarts en achterwaarts)
    ##fill(PRG_Totaal) %>%
    tidyr::fill(PRG_Totaal, .direction = "downup") %>%

    ## Vul alle overige missende waarden op met 0
    replace(is.na(plyr::.), 0) %>%

    ## Hergroepeer op de groepeervariabelen en het studiejaar,
    ## om per jaar te kunnen sorteren
    dplyr::group_by_at(c(Groepeervariabelen, "INS_Studiejaar")) %>%

    ## Voeg een extra kolom toe met het soort uitstroom
    ## en sorteer per groep
    mutate(SUC_Soort_uitstroom = stringr::word(SUC_Soort_en_jaar_uitstroom, 1)) %>%
    dplyr::arrange(factor(
      SUC_Soort_uitstroom,
      levels = c("Uitval",
                 "Diploma",
                 "Herinschrijving")
    ), .by_group = TRUE) %>%

    ## Hergroepeer op de groepeervariabelen
    dplyr::group_by_at(Groepeervariabelen) %>%

    ## Bereken het % herinschrijvers per jaar op basis van de cumulatieve uitstroom
    mutate(
      SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm = dplyr::case_when(
        SUC_Soort_uitstroom == "Herinschrijving" ~ (1 - cumsum(
          SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm
        )),
        TRUE ~ SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm
      )
    ) %>%

    ## Degroepeer en hersorteer
    dplyr::ungroup() %>%
    dplyr::arrange_at(Groepeervariabelen)

  return(dfTransitiematrix_uitstroom)
}


#' @title maak_transitiematrix_uitstroom_met_subtotalen
#' @description Functie om de transitieaantallen voor gegroepeerde (zoals Studiejaar, Uitstroom etc.) en geneste data te berekenen
#' @param df Dataframe
#' @return Transitiematrix uitstroom met subtotalen
#' @examples \dontrun{maak_transitiematrix_uitstroom_met_subtotalen(df)}
#' @export
maak_transitiematrix_uitstroom_met_subtotalen <- function(df) {
  SUC_Soort_en_jaar_uitstroom <- PRG_Subtotaal <- Groepeervariabelen <-
    data <- SUC_Soort_uitstroom <- INS_Studiejaar <- NULL

  df <- df %>%

    ## Verwijder SUC_Soort_en_jaar_uitstroom en PRG_Subtotaal en groepeer
    dplyr::select(-SUC_Soort_en_jaar_uitstroom,
           -PRG_Subtotaal) %>%
    dplyr::group_by_at(c(Groepeervariabelen, "PRG_Totaal")) %>%

    ## Nest de data, bereken de aantallen, selecteer die met pluck
    ## en join deze weer met unnest
    tidyr::nest() %>%
    mutate(
      PRG_Subtotaal = map(data,
                          ~ bepaal_subtotalen_met_transitiematrix(.x,
                                                                  PRG_Totaal))
      %>%
        purrr::pluck("SUC_Soort_uitstroom_aantal")
    ) %>%
    tidyr::unnest(c(data, PRG_Subtotaal)) %>%

    ## Reconstrueer SUC_Soort_en_jaar_uitstroom
    mutate(SUC_Soort_en_jaar_uitstroom = paste(SUC_Soort_uitstroom,
                                               "jaar",
                                               INS_Studiejaar)) %>%

    ## Herorden kolommen
    dplyr::select_at(c(Groepeervariabelen,
                "INS_Studiejaar",
                "SUC_Soort_uitstroom",
                "SUC_Soort_en_jaar_uitstroom",
                "SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm",
                "PRG_Totaal",
                "PRG_Subtotaal"
    ))
  return(df)
}


#' @title maak_transitiematrix_voor_prognosejaren
#' @description Functie om de transitiematrix toe passen voor alle prognosejaren
#' @param df Dataframe
#' @param huidig_academisch_jaar Default: 'Huidig_academisch_jaar'
#' @return Transitiematrix voor prognose jaren
#' @examples \dontrun{maak_transitiematrix_voor_prognosejaren(df)}
#' @export
maak_transitiematrix_voor_prognosejaren <- function(df,
                                                    huidig_academisch_jaar = Huidig_academisch_jaar) {
  Huidig_academisch_jaar <- INS_Inschrijvingsjaar_EOI <- INS_Studiejaar <-
    PRG_Subtotaal <- SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm <-
    Groepeervariabelen <- NULL
  lPrognosejaren <- huidig_academisch_jaar:(huidig_academisch_jaar + 5)

  df <- df %>%
    mutate(INS_Inschrijvingsjaar_EOI = huidig_academisch_jaar,
           INS_Opleidingsnaam_2002 = NA) %>%

    ## Maak een cartesisch product met lPrognosejaren
    tidyr::expand_grid(lPrognosejaren) %>%

    ## Transponeer de lPrognosejaren naar INS_Inschrijvingsjaar_EOI
    ## Bereken voor ieder cohort de inschrijvingsjaren
    ## en maak aantallen voor toekomstige cohorten leeg
    mutate(
      INS_Inschrijvingsjaar_EOI = lPrognosejaren,
      INS_Inschrijvingsjaar = INS_Inschrijvingsjaar_EOI + (INS_Studiejaar -
                                                             1),
      PRG_Subtotaal = ifelse(
        INS_Inschrijvingsjaar_EOI > Huidig_academisch_jaar,
        0,
        PRG_Subtotaal
      ),
      SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm = ifelse(
        INS_Inschrijvingsjaar_EOI > Huidig_academisch_jaar,
        0,
        SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm
      )
    ) %>%

    ## Verwijder de Prognosejaren
    dplyr::select(-lPrognosejaren) %>%

    ## Sorteer
    dplyr::arrange_at(
      c(
        Groepeervariabelen,
        "INS_Inschrijvingsjaar_EOI",
        "INS_Studiejaar",
        "SUC_Soort_uitstroom"
      )
    )
  return(df)
}

#' @title maak_instroomscenario_met_subtotalen_tableau
#' @description Functie om de matrix om te zetten naar het instroomscenarioformaat voor Tableau
#' @param df Dataframe
#' @param scenario PRG_scenario
#' @param subscenario Default: NA
#' @return Matrix instroomscenario met subtotalen klaar voor Tableau
#' @examples \dontrun{maak_instroomscenario_met_subtotalen_tableau(df, scenario, subscenario = NA)}
#' @export
maak_instroomscenario_met_subtotalen_tableau <- function(df, scenario, subscenario = NA) {
  PRG_Scenario <- PRG_Subtotaal <- PRG_Totaal <- INS_Faculteit <-
    OPL_Studielast_nominaal <- INS_Opleidingsfase_BPM <- INS_Hoofdneven <-
    INS_Inschrijvingsjaar_EOI <- INS_Prognose <- n <- Instroom_prognose_multiplier <-
    SUC_Soort_en_jaar_uitstroom <- SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm <-
    n_origineel <- INS_Studiejaar <- INS_Inschrijvingsjaar <- PRG_Subscenario <-
    INS_Opleidingsnaam_2002 <- Huidig_academisch_jaar <- NULL

  df <- df %>%

    ## Voeg missende kolommen toe met default waarden
    mutate(

      ## Bepaal het scenario en subscenario
      INS_Prognose = "Scenario",
      PRG_Scenario = scenario,
      PRG_Subscenario = ifelse(is.na(subscenario), PRG_Scenario, subscenario),

      ## Bepaal de aantallen
      SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm = 0, ## Moet hier niet de originele waarde
      n = PRG_Subtotaal, #Subtotaal,
      n_origineel = PRG_Totaal, #Groepstotaal
      Instroom_prognose_multiplier = 0

    ) %>%

    ## Selecteer de variabelen in de gewenste volgorde
    dplyr::select(
      INS_Faculteit,
      OPL_Studielast_nominaal,
      INS_Opleidingsfase_BPM,
      INS_Hoofdneven,
      INS_Inschrijvingsjaar_EOI,
      INS_Prognose,
      n,
      Instroom_prognose_multiplier,
      SUC_Soort_en_jaar_uitstroom,
      SUC_Soort_en_jaar_uitstroom_p_in_jaar_norm,
      n_origineel,
      INS_Studiejaar,
      INS_Inschrijvingsjaar, #mist
      PRG_Scenario,
      PRG_Subscenario,
      INS_Opleidingsnaam_2002 ## Moet deze erin?
    ) %>%

    ## Filter voor een prognose voor de komende 5 jaar
    dplyr::filter(INS_Inschrijvingsjaar <= Huidig_academisch_jaar + 5)
  return(df)
}

#' @title maak_inschrijvingen_grid
#' @description Functie om een Inschrijvingen_grid te maken per opleidingsniveau ("B", "P", "M"), faculteit ("BETA", "FGB", "FGW", "FRT", "FSW", "GNK", "RCH", "SBE", "THK"), nominale studielasten (30, 60, 120, 180) en inschrijfmomenten ("Hoofdinschrijving", "Inschrijving na 1 oktober")
#' @return Een inschrijvingen grid
#' @examples maak_inschrijvingen_grid()
#' @export
maak_inschrijvingen_grid <- function() {
  INS_Inschrijvingsjaar_EOI <- 2010:2024
  INS_Opleidingsfase_BPM <- c("B", "P", "M")
  INS_Faculteit <- c("BETA", "FGB", "FGW", "FRT", "FSW", "GNK", "RCH", "SBE", "THK")
  OPL_Studielast_nominaal <- c(30, 60, 120, 180)
  INS_Hoofdneven <- c("Hoofdinschrijving", "Inschrijving na 1 oktober")
  df <- expand.grid(
    INS_Inschrijvingsjaar_EOI,
    INS_Opleidingsfase_BPM,
    INS_Faculteit,
    OPL_Studielast_nominaal,
    INS_Hoofdneven,
    stringsAsFactors = FALSE
  )
  names(df) <- c(
    "INS_Inschrijvingsjaar_EOI",
    "INS_Opleidingsfase_BPM",
    "INS_Faculteit",
    "OPL_Studielast_nominaal",
    "INS_Hoofdneven"
  )
  return(df)
}
