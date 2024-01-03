#' Bepaal doorstroom van bachelor naar master
#'
#' Met bepaal_doorstroom wordt een self-join op een deel van de analyseset uitgevoerd, zodat
#' de bachelor of premaster opleidingen en de masteropleidingen naast elkaar in een
#' dataframe worden geplaatst. Dit dataframe bevat alleen doorgestroomde studenten.
#' @param df Dataset waarover een self-join uitgevoerd moet worden voor verschillende
#' opleidingsfasen. Dit is de Analyseset of een subset van de analyseset.
#' @param Opleidingsfase_vanaf De doorstroom kan bepaald worden vanaf de bachelor of premaster, aangegeven met "B" of "P".
#' @param Opleidingsfase_naar De doorstroom wordt bepaald naar de master, aangegeven met "M".
#' @param vars Variabelen die meegenomen moeten worden, deze variabelen worden meegegeven in het nieuwe dataframe.
#' De variabelen die in ieder geval meegegeven moeten worden zijn: INS_Studentnummer,
#' INS_Opleidingsnaam_2002, INS_Opleidingsfase_BPM, INS_Studiejaar, INS_Eerste_jaar_opleiding_en_instelling. Voor bachelor naar
#' master moet ook INS_Doorstroom_van_bachelor_naar_master meegegeven worden. Voor premaster naar master moet ook
#' INS_Doorstroom_van_premaster_naar_master meegegeven worden.
#'
#' @return Dataframe waarbij de bachelor en masteropleidingen in dezelfde rij naast elkaar staan.
#' @family doorstroom
#' @export
#'
bepaal_doorstroom <- function (df,
                               Opleidingsfase_vanaf,
                               Opleidingsfase_naar,
                               vars)
{
  . <- NULL
  INS_Studiejaar <- INS_Opleidingsfase_BPM <- INS_Doorstroom_van_bachelor_naar_master <-
    INS_Eerste_jaar_opleiding_en_instelling_M <- INS_Eerste_jaar_opleiding_en_instelling_B <-
    INS_Studentnummer <- INS_Opleidingsnaam_2002_B <- INS_Opleidingsnaam_2002_M <-
    INS_Doorstroom_van_premaster_naar_master <- INS_Eerste_jaar_opleiding_en_instelling_P <-
    INS_Opleidingsnaam_2002_P <- NULL
  if (Opleidingsfase_vanaf == "B" & Opleidingsfase_naar ==
      "M") {
    noodzakelijke_variabelen <- c("INS_Opleidingsfase_BPM",
                                  "INS_Doorstroom_van_bachelor_naar_master",
                                  "INS_Studentnummer",
                                  "INS_Opleidingsnaam_2002",
                                  "INS_Studiejaar",
                                  "INS_Eerste_jaar_opleiding_en_instelling")

    verschillen <- dplyr::setdiff(noodzakelijke_variabelen, vars)

    if (!purrr::is_empty(verschillen)) {
      stop(error = paste0("Er missen noodzakelijke variabelen: ",
                          verschillen))
    }

    nieuwe_data <- df %>%
      dplyr::select(vars) %>%
      dplyr::filter(INS_Studiejaar == 1) %>%
      dplyr::left_join(x = dplyr::filter(., INS_Opleidingsfase_BPM == "B" & INS_Doorstroom_van_bachelor_naar_master),
                       y = dplyr::filter(., INS_Opleidingsfase_BPM == "M" & INS_Doorstroom_van_bachelor_naar_master),
                       by = c("INS_Studentnummer"),
                       suffix = c("_B", "_M")) %>%
      dplyr::filter(INS_Eerste_jaar_opleiding_en_instelling_M >= INS_Eerste_jaar_opleiding_en_instelling_B) %>%
      dplyr::distinct()


    if (nrow(dplyr::distinct(nieuwe_data, INS_Studentnummer, INS_Opleidingsnaam_2002_B,
                      INS_Opleidingsnaam_2002_M, INS_Eerste_jaar_opleiding_en_instelling_B)) !=
        nrow(nieuwe_data)) {
      message(paste0("There are ", (nrow(nieuwe_data) -
                                    nrow(dplyr::distinct(nieuwe_data, INS_Studentnummer,
                                                  INS_Opleidingsnaam_2002_B, INS_Opleidingsnaam_2002_M,
                                                  INS_Eerste_jaar_opleiding_en_instelling_B))),
                     " double rows"))
    }


    return(nieuwe_data)

  } else if (Opleidingsfase_vanaf == "P" & Opleidingsfase_naar ==
             "M") {
    noodzakelijke_variabelen <- c("INS_Opleidingsfase_BPM",
                                  "INS_Studentnummer",
                                  "INS_Opleidingsnaam_2002",
                                  "INS_Eerste_jaar_opleiding_en_instelling",
                                  "INS_Opleidingsfase_BPM",
                                  "INS_Studiejaar",
                                  "INS_Doorstroom_van_premaster_naar_master")
    verschillen <- dplyr::setdiff(noodzakelijke_variabelen, vars)
    if (!purrr::is_empty(verschillen)) {
      stop(error = paste0("Necessary variables are not present: ",
                          verschillen))
    }
    rm(noodzakelijke_variabelen, verschillen)

    nieuwe_data <- df %>%
      dplyr::select(vars) %>%
      dplyr::filter(INS_Studiejaar == 1) %>%
      dplyr::left_join(x = dplyr::filter(., INS_Opleidingsfase_BPM == "P" & INS_Doorstroom_van_premaster_naar_master),
                       y = dplyr::filter(., INS_Opleidingsfase_BPM == "M" & INS_Doorstroom_van_premaster_naar_master),
                       by = c("INS_Studentnummer"),
                       suffix = c("_P", "_M")) %>%
      dplyr::filter(INS_Eerste_jaar_opleiding_en_instelling_M >= INS_Eerste_jaar_opleiding_en_instelling_P) %>%
      dplyr::distinct()

    if (nrow(dplyr::distinct(nieuwe_data, INS_Studentnummer, INS_Opleidingsnaam_2002_P,
                             INS_Opleidingsnaam_2002_M, INS_Eerste_jaar_opleiding_en_instelling_P)) !=
        nrow(nieuwe_data)) {
      message(paste0("There are ", (nrow(nieuwe_data) -
                                    nrow(dplyr::distinct(nieuwe_data, INS_Studentnummer,
                                                         INS_Opleidingsnaam_2002_P, INS_Opleidingsnaam_2002_M,
                                                         INS_Eerste_jaar_opleiding_en_instelling_P))),
                     " double rows"))
    }

    return(nieuwe_data)

  } else {
    stop(error = "Deze functie is enkel geschikt voor de doorstroom van bachelor naar master of premaster naar master.")
  }

}


#' Aanpassen doorstroomvariabele o.b.v. bepaal_doorstroom
#'
#' Omdat de variabelen INS_Doorstroom_van_bachelor_naar_master en INS_Doorstroom_van_premaster_naar_master
#' enkele doorstromers niet juist categoriseren, maar consistentie van deze variabelen vereist is voor
#' het maken van een dashboard, wordt deze variabele aangepast op basis van een dataframe bepaald door
#' bepaal_doorstroom. Studenten die geen master volgen worden gecorrigeerd.
#'
#' @param df1 Dataframe gecreeerd met behulp van de functie bepaal_doorstroom(). Dit dataframe bevat de variabelen
#' INS_Studentnummer, INS_Opleidingsnaam_2002_B, INS_Eerste_jaar_opleiding_en_instelling_B, INS_Doorstroom_van_bachelor_naar_master_B,
#' of deze variabelen voor de premaster, in dat geval is de suffix _P in plaats van _B.
#' @param df2 Analyseset of subset van de analyseset waar de doorstroom variabele consistent gemaakt dient te worden.
#' Dit dataframe bevat de variabelen INS_Studentnummer, INS_Opleidingsfase_BPM, INS_Datum_diploma, INS_Opleidingsnaam_2002,
#' INS_Studiejaar, INS_Eerste_jaar_opleiding_en_instelling en
#' INS_Doorstroom_van_bachelor_naar_master of INS_Doorstroom_van_premaster_naar_master.
#' @param bachelor_naar_master Standaard op TRUE, wanneer de variabele INS_Doorstroom_van_premaster_naar_master aangepast moet worden,
#' dan moet deze variabele op FALSE gezet worden.
#'
#' @return Aangepaste analyseset of subset van de analyseset.
#' @family doorstroom
#' @export
#'
pas_doorstroomvariabele_aan <- function(df1, df2, bachelor_naar_master = TRUE){
  INS_Studentnummer <- INS_Opleidingsnaam_2002_B <- INS_Eerste_jaar_opleiding_en_instelling_B <-
    INS_Doorstroom_van_bachelor_naar_master_B <- INS_Opleidingsfase_BPM <-
    INS_Datum_diploma <- INS_Studiejaar <- INS_Opleidingsnaam_2002 <-
    INS_Eerste_jaar_opleiding_en_instelling <- INS_Doorstroom_van_bachelor_naar_master <-
    INS_Doorstroom_van_bachelor_naar_master_correct <- INS_Opleidingsnaam_2002_P <-
    INS_Eerste_jaar_opleiding_en_instelling_P <- INS_Doorstroom_van_premaster_naar_master_P <-
    INS_Doorstroom_van_premaster_naar_master <- INS_Doorstroom_van_premaster_naar_master_correct <- NULL
  # Probleem: sommige studenten hebben TRUE voor INS_Doorstroom_van_bachelor_naar_master,
  # terwijl ze niet worden opgenomen in nieuwe doorstroom data omdat er geen master
  # bekend is. INS_Doorstroom_van_bachelor_naar_master moet worden aangepast in analyseset
  # om problemen in het dashboard te voorkomen.

  if(bachelor_naar_master){
    # Bepaal mismatch tussen df1 en df2
    # Df 1 is de nieuwe doorstroom data
    df1 <- df1 %>%
      dplyr::select(INS_Studentnummer,
                    INS_Opleidingsnaam_2002_B,
                    INS_Eerste_jaar_opleiding_en_instelling_B,
                    INS_Doorstroom_van_bachelor_naar_master_B) %>%
      dplyr::rename(INS_Opleidingsnaam_2002 = INS_Opleidingsnaam_2002_B,
                    INS_Eerste_jaar_opleiding_en_instelling = INS_Eerste_jaar_opleiding_en_instelling_B,
                    INS_Doorstroom_van_bachelor_naar_master = INS_Doorstroom_van_bachelor_naar_master_B) %>%
      dplyr::distinct()

    # Df2 is de analyseset die moet worden aangepast
    df3 <- df2 %>%
      dplyr::filter(INS_Opleidingsfase_BPM == "B",
                    !is.na(INS_Datum_diploma),
                    INS_Studiejaar == 1) %>%
      dplyr::select(INS_Studentnummer,
                    INS_Opleidingsnaam_2002,
                    INS_Eerste_jaar_opleiding_en_instelling,
                    INS_Doorstroom_van_bachelor_naar_master) %>%
      dplyr::distinct()

    # Bepaal het verschil tussen df1 en df2
    verschil <- dplyr::setdiff(df3, df1) %>%
      # filter alle studenten die wel doorstromen volgens de variabele
      # INS_Doorstroom_van_bachelor_naar_master maar niet in de nieuwe
      # doorstroom data zijn gekomen doordat ze geen master inschrijving
      # hebben
      dplyr::filter(INS_Doorstroom_van_bachelor_naar_master) %>%
      dplyr::mutate(INS_Doorstroom_van_bachelor_naar_master_correct = FALSE) %>%
      dplyr::select(-INS_Doorstroom_van_bachelor_naar_master)

    if(nrow(verschil) != 0){
      df2 <- df2 %>%
        dplyr::left_join(verschil,
                         by = c("INS_Studentnummer",
                                "INS_Opleidingsnaam_2002",
                                "INS_Eerste_jaar_opleiding_en_instelling")) %>%
        dplyr::mutate(INS_Doorstroom_van_bachelor_naar_master =
                        dplyr::if_else(is.na(INS_Doorstroom_van_bachelor_naar_master_correct),
                                INS_Doorstroom_van_bachelor_naar_master,
                                INS_Doorstroom_van_bachelor_naar_master_correct))

      Aantal_aanpassingen <- df2 %>%
        dplyr::select(INS_Doorstroom_van_bachelor_naar_master_correct) %>%
        dplyr::filter(!is.na(INS_Doorstroom_van_bachelor_naar_master_correct))

      print(paste0(nrow(Aantal_aanpassingen),
                   " rijen van de originele variabele INS_Doorstroom_van_bachelor_naar_master zijn aangepast."))

      df2 <- df2 %>%
        dplyr::select(-INS_Doorstroom_van_bachelor_naar_master_correct) %>%
        dplyr::ungroup()

      return(df2)

    } else {
      print("INS_Doorstroom_van_bachelor_naar_master hoeft niet aangepast te worden omdat er geen discrepanties zijn.")
    }
  }

  if(!bachelor_naar_master){
    # Bepaal mismatch tussen df1 en df2
    # Df 1 is de nieuwe doorstroom data
    df1 <- df1 %>%
      dplyr::select(INS_Studentnummer,
                    INS_Opleidingsnaam_2002_P,
                    INS_Eerste_jaar_opleiding_en_instelling_P,
                    INS_Doorstroom_van_premaster_naar_master_P) %>%
      dplyr::rename(INS_Opleidingsnaam_2002 = INS_Opleidingsnaam_2002_P,
                    INS_Eerste_jaar_opleiding_en_instelling = INS_Eerste_jaar_opleiding_en_instelling_P,
                    INS_Doorstroom_van_premaster_naar_master = INS_Doorstroom_van_premaster_naar_master_P) %>%
      dplyr::distinct()

    # Df2 is de analyseset die moet worden aangepast
    df3 <- df2 %>%
      dplyr::filter(INS_Opleidingsfase_BPM == "P",
                    !is.na(INS_Datum_diploma),
                    INS_Studiejaar == 1) %>%
      dplyr::select(INS_Studentnummer,
                    INS_Opleidingsnaam_2002,
                    INS_Eerste_jaar_opleiding_en_instelling,
                    INS_Doorstroom_van_premaster_naar_master) %>%
      dplyr::distinct()

    # Bepaal het verschil tussen df1 en df2
    verschil <- dplyr::setdiff(df3, df1) %>%
      # filter alle studenten die wel doorstromen volgens de variabele
      # INS_Doorstroom_van_bachelor_naar_master maar niet in de nieuwe
      # doorstroom data zijn gekomen doordat ze geen master inschrijving
      # hebben
      dplyr::filter(INS_Doorstroom_van_premaster_naar_master) %>%
      dplyr::mutate(INS_Doorstroom_van_premaster_naar_master_correct = FALSE) %>%
      dplyr::select(-INS_Doorstroom_van_premaster_naar_master)

    if(nrow(verschil) != 0){
      df2 <- df2 %>%
        dplyr::left_join(verschil,
                         by = c("INS_Studentnummer",
                                "INS_Opleidingsnaam_2002",
                                "INS_Eerste_jaar_opleiding_en_instelling")) %>%
        dplyr::group_by(INS_Studentnummer,
                        INS_Opleidingsnaam_2002,
                        INS_Eerste_jaar_opleiding_en_instelling) %>%
        dplyr::mutate(INS_Doorstroom_van_premaster_naar_master =
                        dplyr::if_else(is.na(INS_Doorstroom_van_premaster_naar_master_correct),
                                INS_Doorstroom_van_premaster_naar_master,
                                INS_Doorstroom_van_premaster_naar_master_correct))

      Aantal_aanpassingen <- df2 %>%
        dplyr::select(INS_Doorstroom_van_premaster_naar_master_correct) %>%
        dplyr::filter(!is.na(INS_Doorstroom_van_premaster_naar_master_correct))

      print(paste0(nrow(Aantal_aanpassingen),
                   " rijen van de originele variabele INS_Doorstroom_van_premaster_naar_master zijn aangepast."))

      df2 <- df2 %>%
        dplyr::select(-INS_Doorstroom_van_premaster_naar_master_correct) %>%
        dplyr::ungroup()

      return(df2)

    } else {
      print("INS_Doorstroom_van_premaster_naar_master hoeft niet aangepast te worden omdat er geen discrepanties zijn.")
    }
  }

}

#' maak_doorstroomvar_premaster_master
#'
#' Bepaal of de student is doorgestroomd van premaster naar master.
#' @param df Een dataframe
#'
#' @return De boolean INS_Doorstroom_van_premaster_naar_master
#' @export
#' @family doorstroom
maak_doorstroomvar_premaster_master <- function(df){
  INS_Studentnummer <- INS_Opleidingsnaam_2002 <- INS_Opleidingsfase_BPM <-
    INS_Eerste_jaar_opleiding_en_instelling <- min_jaar_master <-
    min_jaar_premaster <- INS_Instelling <- NULL

  df <- df %>%
    dplyr::group_by(INS_Studentnummer,
                    INS_Opleidingsnaam_2002) %>%
    dplyr::mutate(
      # Bepaal per opleiding het eerste jaar obv INS_Eerste_jaar_opleiding_en_instelling
      # Dit doen we zodat we deze variabelen later kunnen gebruiken om te bepalen
      # of het EOI van de masterfase eerder begon dat het EOI van de bachelorfase
      min_jaar_master = dplyr::if_else(
        INS_Opleidingsfase_BPM == "M",
        INS_Eerste_jaar_opleiding_en_instelling,
        NA_real_
      ),
      # Idem als hierboven, maar nu voor de premaster
      min_jaar_premaster = dplyr::if_else(
        INS_Opleidingsfase_BPM == "P",
        INS_Eerste_jaar_opleiding_en_instelling,
        NA_real_
      )
    ) %>%
    dplyr::ungroup() %>%
    # Omdat we per student bepalen of deze is doorgestroomd en daarvoor data combineren,
    # uit verschillende opleidingsfasen, groeperen we nu enkel op studentnummer
    dplyr::group_by(INS_Studentnummer) %>%
    dplyr::mutate(
      # We moeten de min_jaar variabelen generaliseren, zodat elke rij is opgevuld.
      # NA's zorgen ervoor dat we later niet weten of de student is doorgestroomd.
      # We sorteren eerst de variabele oplopend, zodat het laatste jaartal het
      # laagste jaartal is. Daarna vullen we de variabelen op met fill_missing,
      # zodat in elke rij een minimaal jaar staat.
      min_jaar_master = vvfiller::fill_missing(min_jaar_master, type = "min"),
      min_jaar_premaster = vvfiller::fill_missing(min_jaar_premaster, type = "min"),
      # We maken een boolean variabele INS_Doorstroom_van_premaster_naar_master
      INS_Doorstroom_van_premaster_naar_master =
        # De student is ingeschreven geweest aan de VU voor een Premaster
        dplyr::if_else((((INS_Instelling == "VU") %in% (INS_Opleidingsfase_BPM == "P")) &
                          # De student heeft zich ingeschreven voor een master
                          any(INS_Opleidingsfase_BPM == "M") &
                          # EOI-jaar master >= EOI-jaar premaster
                          min_jaar_master >= min_jaar_premaster
        ), T, F)
    ) %>%
    dplyr::select(-min_jaar_master,-min_jaar_premaster)

  return(df)

}

