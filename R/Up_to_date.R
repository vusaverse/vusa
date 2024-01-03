#' up_to_date
#'
#' Check whether the read in data is up-to-date.
#'
#' Function to be called inside of read scripts.
#' A slack message is sent when the data is not up-tp-date.
#'
#' @param bestandspad Bestandspad naar het ingelezen bestand
#'
#'
#' @param correctie_tijdstip Optioneel indien de data eerder geleverd kon worden.
#' @param frequentie De frequentie van de levering. Hier dient een integer opgegeven te worden die
#' de frequentie van de levering aangeeft. Bijvoorbeeld: Een jaarlijkse levering krijgt de waarde
#' 365
#'
#' @param contact Person to contact
#'
#' @param inleesscript Naam van het inleesscript. Bijvoorbeeld "Inlezen Aanmeldingen.R"
#'
#' @importFrom dplyr %>%
#' @export
## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Up to date check
## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
up_to_date <- function(bestandspad, correctie_tijdstip, frequentie, contact, inleesscript){
  if (!Sys.getenv("RSTUDIO") == "1") {
    naam <- basename(current_filename())
  } else {
    naam <- tools::file_path_sans_ext(basename(this.path::this.path()))
  }
  Naam <- Inleesscript <- Frequentie <- NULL
  # Bij het uitvoeren van deze functie wordt een trycatch gebruikt zodat excecutie van
  # het script waar deze functie in gebruikt wordt niet gestopt wordt door een error
  tryCatch({
    ## gebruik correctie_tijdstip ipv leverdatum
    if (!missing(correctie_tijdstip)) {
      Leverdatum <- correctie_tijdstip %>% lubridate::ymd()
      lubridate::year(Leverdatum) <- lubridate::year(lubridate::today())
    } else {
    # Verkrijg filenaam
    file <- basename(bestandspad)
    # Als de filenaam de leverdatum bevat:
    if (T %in% grepl("^[0-9]{8}|_[0-9]{8}|^[0-9]{6}|20[0-9]{2}-[0-9]{2}-[0-9]{2}|S[0-9]{8}",file)){
      # Verkrijg de meest recente leverdatum
      Leverdata <- stringr::str_extract_all(file,
                                            "^[0-9]{8}|_[0-9]{8}|^[0-9]{6}|20[0-9]{2}-[0-9]{2}-[0-9]{2}|S[0-9]{8}")%>%
        # Als er twee datums in de filenaam zitten moet de maximale geëxtract worden
        unlist()
      if (length(Leverdata) > 1){
        bestandspad <- suppressWarnings(file[which.max(Leverdata)])
        Leverdatum <- max(Leverdata)
      } else {
        Leverdatum <- Leverdata
      }


      # omzetten van data met format "_20161115" naar "2016-11-15
      Leverdatum[grepl("^_",Leverdatum)] <-
        substr(Leverdatum[grepl("^_",Leverdatum)],2,10) %>%
        as.character()
      # omzetten van data met format "191002" en "20191002" naar "2019-10-02
      Leverdatum[!grepl("-",Leverdatum)] <- Leverdatum[!grepl("-",Leverdatum)] %>%
        unlist()
      Leverdatum <- Leverdatum %>% lubridate::ymd()
    }else{
      # Als de filenaam geen leverdatum bevat:
      Leverdata <- file.info(bestandspad)$mtime %>%
        stats::na.omit() %>%
        as.Date()

      if (length(Leverdata) > 1){
        bestandspad <- suppressWarnings(file[which.max(Leverdata)])
        Leverdatum <- max(Leverdata)
      } else {
        Leverdatum <- Leverdata
      }
    }
    }

    # Lees documentatie in
    Documentatie_uptodate <- vvmover::readrds_csv(
      "Tableau/177 VU-Datasets up to date/Data/Documentatie uptodate check.rds")

    # Verzamel metadata
    Levering <- tibble::tribble(
      ~Naam,
      ~Contact,
      ~Bestandspad,
      ~Inleesscript,
      ~Frequentie,
      ~Leverdatum,
      ~Volgende_levering,
      naam,
      contact,
      bestandspad,
      inleesscript,
      frequentie,
      Leverdatum,
      Leverdatum + frequentie
    )
    if (Levering$Volgende_levering < Sys.Date()){
      Levering <- Levering %>%
        dplyr::mutate(Actie_vereist ="Ja")
    }else{
      Levering <- Levering %>%
        dplyr::mutate(Actie_vereist ="Nee")
    }
    # Voeg metadata toe aan documentatie
    Documentatie_uptodate <- Documentatie_uptodate %>%
      dplyr::bind_rows(Levering)
    # Definieer variabele "Actie_vereist" die aangeeft of de huidige datum groter is dan de
    # datum van de verwachte levering.
    # Kijk check of er inleesscripts zijn die twee keer voorkomen in dataframe
    Dubbele_bestand <- Documentatie_uptodate[duplicated(Documentatie_uptodate$Naam),] %>%
      dplyr::pull(Naam)
    if (length(Dubbele_bestand)>0){
      Documentatie_uptodate <- Documentatie_uptodate %>%
        dplyr::filter(Naam %in% Dubbele_bestand) %>%
        dplyr::select(Naam,Leverdatum) %>%
        # groupeer bij inleesscript
        dplyr::group_by(Naam) %>%
        # Pak de meest recente leverdatum
        dplyr::summarize(Leverdatum = max(Leverdatum)) %>%
        # Filter het  oude dataframe met de juiste leverdata
        dplyr::full_join(Documentatie_uptodate, by = c("Naam", "Leverdatum"))
    }
    # Verwijder dubbele rijen
    Documentatie_uptodate <- Documentatie_uptodate  %>% dplyr::distinct()
    # Stuur slack message als de huidige datum groter is dan de volgende levering
    if ("slackr" %in% utils::installed.packages()){
      if(Levering$Volgende_levering < Sys.Date()){
        # en als het niet de eerste keer is dat het bestand voorkomt
        if(Levering$Inleesscript %in% Documentatie_uptodate$Inleesscript){
          # en als de leverdatum van de huidige levering de meest recente levering is
          if(Levering$Leverdatum >= (Documentatie_uptodate %>%
                                     dplyr::filter(Inleesscript == inleesscript) %>%
                                     dplyr::pull(Leverdatum))[1]){
            slackr::slackr_msg(paste0(
              Levering$Naam," Bestand is niet up to date (Contact: ", Levering$Contact,")"))
          }

        }}}
    ## Verwijder dubbele rijen door te filteren op de laatste waarde
    Documentatie_uptodate <- Documentatie_uptodate %>%
      dplyr::group_by(Naam, Inleesscript,Frequentie) %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::ungroup()

    # Overwrite de documentatie
    vvmover::saverds_csv(Documentatie_uptodate,
                "Documentatie uptodate check",
                dataloc =  "Tableau/177 VU-Datasets up to date/Data/",
                save_csv = TRUE)
  }
  ,
  ## Indien er een error  is, wordt er een error message geprint
  error = function(e) {
    print(e)
    cat("Error met uitvoeren Up to date functie: Check input")
  })
}

