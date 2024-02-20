#' documentation search
#'
#' Doorzoekt de documentatie aan de hand van de opgegeven zoekterm. Als
#' resultaat wordt de documentatie van de gevonden kolommen teruggegeven.
#'
#' @param search Zoekterm waarop de documentatie doorzocht wordt
#' @param output "veldnaam", voor een vector met alle gevonden veldnamen. "tabel" voor
#' een tabel met alleen Categorie, Veldnaam, en Omschrijving. "all" voor alle informatie
#' @param omschrijving T/F indien hierin wel of niet in gezocht moet worden
#' @param veldnaam T/F indien hierin wel of niet in gezocht moet worden
#' @param categorie T/F indien hierin wel of niet in gezocht moet worden
#' @export
documentation_search <- function(search, output = "veldnaam", omschrijving = T, veldnaam = T, categorie = T) {
  # laad de documentatie
  documentation <- documentation_get()

  # zoek naar de string in de veldnaam en de omschrijving
  Matchend <- !is.na(documentation$Categorie) & stringr::str_detect(documentation$Categorie, stringr::coll(search, ignore_case = TRUE)) & categorie == T |
    !is.na(documentation$Omschrijving) & stringr::str_detect(documentation$Omschrijving, stringr::coll(search, ignore_case = TRUE)) & omschrijving == T |
    !is.na(documentation$Veldnaam) & stringr::str_detect(documentation$Veldnaam, stringr::coll(search, ignore_case = TRUE)) & veldnaam == T


  Zoekresultaat <- documentation[Matchend, ]
  # output: alle rijen die overeen komen met de documentatie
  documentation_search_output(Zoekresultaat, output, search)
}

#' documentation search output
#'
#' Om de output van het documentatiebestand te message.
#' Deze functie wordt in principe alleen opgeroepen vanuit de functie documentation_search()
#'
#' @param Zoekresultaat Een data frame met de variabelen die als output geleverd moeten
#' worden
#' @param output "veldnaam", voor een vector met alle gevonden veldnamen. "tabel" voor
#' een tabel met alleen Categorie, Veldnaam, en Omschrijving. "all" voor alle informatie
#' @param search De zoekterm
documentation_search_output <- function(Zoekresultaat, output, search) {
  Categorie <- Omschrijving <- Veldnaam <- NULL
  ## Maak het bericht dat getoond wordt
  bericht <- paste(length(Zoekresultaat$Veldnaam), " kolommen gevonden met zoekterm '", search, "' \n", sep = "")

  ## Alleen de kolomnamen
  if (output == "veldnaam") {
    message(paste(bericht, "Voor meer details, gebruik , output = 'tabel' of , output = 'all'"))
    return(Zoekresultaat$Veldnaam)
  }
  ## Een tabel met de categorie, veldnaam en de omschrijving
  if (output == "tabel") {
    message(paste(bericht, "Voor meer details, gebruik , output = 'all'"))
    Zoekresultaat <- Zoekresultaat %>%
      dplyr::select(Categorie, Veldnaam, Omschrijving)

    return(Zoekresultaat)
  }
  ## Een tabel met alle details van de kolommen
  if (output == "all") {
    return(Zoekresultaat)
  }
}


#' documentation save
#'
#' Functie om een nieuw documentatiebestand op te slaan in /documentation/ en in het archief. Opslaan is alleen
#' mogelijk als er geen duplicaten in de kolom "Veldnaam" zitten.
#' @param data Het documentatiebestand in de vorm van een data frame
documentation_save <- function(data) {
  Veldnaam <- NULL
  ## Sorteer de tabel
  data <- data %>% dplyr::arrange(Veldnaam)
  ## Controleren op duplicates
  duplicaten <- duplicates_in_column(data, "Veldnaam")

  if (nrow(duplicaten) > 0) {
    savecsv(duplicaten, Name_to_save = paste0("Duplicaten", format(Sys.time(), "%Y-%M-%d_%H%M")), dataloc = "/documentation/")
    stop("Er zitten duplicaten in de documentatie. Opslaan is mislukt. Een dump van de duplicaten is in /documentation/ neergezet.")
  }

  ## Maak character waardes van alle kolommen
  data <- data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
  ## Sla de documentatie op
  saverds_csv(data, Name_to_save = "documentation Analyseset", dataloc = "/documentation/", save_csv = T)
  saverds_csv(data, Name_to_save = paste0("documentation Analyseset_", Sys.Date()), dataloc = "/documentation/XX. Archief/", save_csv = T)
  message("Nieuwe versie van de documentatie is opgeslagen")
}

#' documentation add
#'
#' Functie om rijen toe te voegen aan het documentatiebestand. Vervolgens wordt deze geupdate versie
#' opgeslagen.
#' @param data Een data frame met de nieuwe rijen voor in het documentatiebestand
#' @param overwrite FALSE: de rijen worden toegevoegd, zonder dat de dubbele rijen eerst verwijderd worden.
#' Als TRUE overschrijven de nieuwe rijen de oorspronkelijke rijen.
#' @export
documentation_add <- function(data, overwrite = F) {
  Veldnaam <- NULL
  ## Haal de documentatie op
  documentation <- documentation_get()

  ## Als overwrite == T worden de veldnamen die in de Update tabel zitten verwijderd
  ## uit de documentatie, voordat de update bij de analyseset gevoegd wordt.
  if (overwrite == T) {
    documentation <- documentation %>%
      dplyr::filter(!Veldnaam %in% data$Veldnaam)
  }
  ## Voeg de update samen met de documentatie
  documentation <- dplyr::bind_rows(documentation, data)

  ## =============================================================================
  ## Opslaan sla de documentatie weer op
  documentation_save(documentation)
}

#' documentation add csv
#'
#' Wrapper rond documentation_add(), hier kan een locatie van een csv opgegeven worden. Deze wordt
#' toegevoegd aan het documentatiebestand.
#' @param dataloc De locatie van een databestand met de nieuwe rijen voor in het documentatiebestand
#' @param overwrite FALSE: de rijen worden toegevoegd, zonder dat de dubbele rijen eerst verwijderd worden.
#' Als TRUE overschrijven de nieuwe rijen de oorspronkelijke rijen.
#' @export
documentation_add_csv <- function(dataloc = "/documentation/Update.csv", overwrite = F) {
  ## Laad de csv in
  data <- readrds_csv(dataloc = dataloc, stringsAsFactors = F)
  ## Voeg deze toe aan de documentatie
  documentation_add(data, overwrite)
}


#' documentation dump csv
#'
#' Wrapper rond documentation_search(). Het resultaat van de zoekterm wordt opgeslagen in een csv bestand.
#' @param search De zoekterm om te zoeken in het documentatiebestand.
#' @param dataloc De netwerklocatie waar de dump geplaatst wordt.
#' @param ... Deze parameters worden doorgegeven aan documentation_search()
#' @export
documentation_dump_csv <- function(search, dataloc = "/documentation/", ...) {
  ## Doorzoek de documentatie, en geef deze data terug
  Dump <- documentation_search(search, output = "all", ...)
  ## Sla deze dump op als csv
  savecsv(Dump, Name_to_save = paste0("dump_", search), dataloc = "/documentation/")
}

#' documentation remove
#'
#' Functie om rijen te verwijderen uit het documentatiebestand. Vervolgens wordt deze geupdate versie
#' opgeslagen. De verwijderde rijen worden opgeslagen in /documentation/XX. Archief/
#' @param remove een string, of vector met strings om de variabelen uit de documentatie
#' te verwijderen.
#' @export
documentation_remove <- function(remove) {
  Veldnaam <- NULL
  ## =============================================================================
  ## Haal de documentatie op
  documentation <- documentation_get()

  ## =============================================================================
  ## Doe de bewerkingen

  ## Maak een backup van de rijen die verwijderd zijn
  documentation_removed <- documentation %>%
    dplyr::filter(Veldnaam %in% remove)
  ## Sla deze dump op als csv
  savecsv(documentation_removed, Name_to_save = paste0("removed_", format(Sys.time(), "%Y-%M-%d_%H%M%S")), dataloc = "/documentation/XX. Archief/")

  ## Verwijder de rijen
  documentation <- documentation %>%
    dplyr::filter(!Veldnaam %in% remove)

  ## =============================================================================
  ## Sla de documentatie weer op
  documentation_save(documentation)
}

#' documentation change
#'
#' Functie om rijen te verwijderen uit het documentatiebestand. Vervolgens wordt deze geupdate versie
#' opgeslagen. De verwijderde rijen worden opgeslagen in /documentation/XX. Archief/
#' @param old de oude naam die veranderd moet worden
#' @param new de nieuwe naam die veranderd moet worden, moet tussen "" ingevuld worden
#' te verwijderen.
#' @export
documentation_change <- function(old, new) {
  ## =============================================================================
  ## Haal de documentatie op
  documentation <- documentation_get()

  ## =============================================================================
  ## Doe de bewerkingen

  # ## Maak een backup van de rijen die verwijderd zijn
  # documentation_removed <- documentation %>%
  #     dplyr::filter(Veldnaam %in% old)
  # ## Sla deze dump op als csv
  # savecsv(documentation_old, Name_to_save = paste0("removed_", format(Sys.time(), "%Y-%M-%d_%H%M%S")), dataloc = "/documentation/XX. Archief/")


  ## Verander de oude waarde in de nieuwe waarde
  documentation <- documentation %>%
    dplyr::mutate(Veldnaam = dplyr::case_when(
      Veldnaam == old ~ new,
      TRUE ~ Veldnaam
    ))


  ## =============================================================================
  ## Sla de documentatie weer op
  documentation_save(documentation)
}
