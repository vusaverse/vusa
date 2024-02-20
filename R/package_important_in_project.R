## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## package_important_in_project
## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#' @title package_important_in_project
#' @description Een functie die gerund kan worden in een project (folder). Bij
#' selectie van een package (default: "vusa") wordt er een dataframe gereturnt
#' waarin elke functie uit de package een row is. Elke map in het project (folder)
#' is een kolom. De waardes in het dataframe geven aan hoe vaak een functie in welke
#' map voor komt. Zo kan worden onderzocht welke functies waar in een project
#' gebruikt worden.
#' -De functie checkt automatisch alle submappen van een project, zorg dat je
#' deze dus altijd vanuit de hoofd-folder runt.
#' -Omdat de functie nog niet geoptimaliseerd is (hij checkt nu alle scripts opnieuw
#' voor elke functie) duurt het even voordat alle scripts zijn doorlopen.
#' 200 functies duren +/- 10 minuten.
#' -Als je meer informatie over een specifieke submap wil kun je de functie dus
#' nog een keer in die submap specifiek laten runnen
#' @param package De package waarvan je wilt uitzoeken hoe vaak die wordt gebruikt
# in een project. Hiervan wordt een overzicht gemaakt van alle functies uit deze package
#' @param type type functies: 2 voor functies beschikbaar met '::', 3 voor functies
#' beschikbaar met ':::' en -1 voor het verschil er tussen
#' @param tellen 'aantal_keer' voor hoe vaak de functie voor komt, 'aantal_bestanden'
#' voor in hoe veel bestanden een functie voorkomt: Als een functie dan meerdere
#' keren in een bestand voorkomt wordt er dus maar een keer geteld
#' @param diepte Default = 1. Geeft aan hoe veel submappen er in de uiteindlijke
#' dataframe komen. De range is van 1-3. Bij 3 Komen dus bvb kolommen als :
#' "Map1/Map2/Map3". Voor verdere inspectie is het beter deze gehele functie in
#' die submap te runnen
#' @param kolommen which directories to check.
#' @return Dataframe met de functies van de package als rijen, en submappen van
#' huidig project als kolommen. De waardes in de dataframe geven aan hoe vaak een
#' functie in welke map gebruikt wordt
#' @examples \dontrun{
#' df_functies_in_package <- package_important_in_project()
#' }
#' @importFrom BurStMisc scriptSearch
#' @importFrom dplyr relocate select case_when
#' @importFrom data.table data.table setnames
#' @export
#'

package_important_in_project <- function(package = "vusa", type = 2,
                                         tellen = "aantal_keer", diepte = 1,
                                         kolommen =
                                           c(
                                             "00. Downloaden", "01. Inlezen", "02. Manipuleren",
                                             "03. Analyseset maken", "04. Analyseren", "05. Rapporten"
                                           )) {
  last_col <- NULL
  ## STAP 1: Maak een lijst van alle vusa functies:
  # Case_when gaf bug/error dus zo:
  # Voor functies beschikbaar met '::'
  if (type == 2) {
    functies_package <- data.table::data.table(getNamespaceExports(package))
    # Voor functies beschikbaar met ':::'
  } else if (type == 3) {
    functies_package <- data.table::data.table(ls(getNamespace(package)))
    # Voor het verschil
  } else if (type == -1) {
    functies_package <- data.table::data.table(setdiff(
      ls(getNamespace(package)),
      getNamespaceExports(package)
    ))
  }

  # Verander standaard naam naar 'Functie'
  setnames(functies_package, "V1", "Functie")

  ## STAP 2: Verzamel bestandspaden per functie
  lijst_met_lijst <- lapply(functies_package$Functie, BurStMisc::scriptSearch)
  functies_package <- cbind(functies_package, lijst_met_lijst)

  backupvroeg_functies <- functies_package

  ## STAP 3: Tel hoe vaak een functie in elke map voorkomt
  for (index in 1:length(functies_package$Functie)) {
    lijst <- functies_package$lijst_met_lijst[[index]]

    # Pak de namen van alle waardes in de lijst: dit is een lijst met
    # bestandspaden Waar de functie in voor komt
    bestandspaden <- names(lijst)

    # Pak de naam van elke lijst in de named list
    for (bestandspad in bestandspaden) {
      # Pak de folder waar het in gebeurdt.
      # Voorbeeld met maar 1 map: "./02. Manipuleren/Manipuleren Taaltoets.R"
      # De punt (.) is altijd 1 bij splitten, dus  folder 2
      pad_split <- strsplit(bestandspad, "/")[[1]]

      locatie <- dplyr::case_when(
        diepte == 1 ~ pad_split[2],
        # Lengte pad_split is dan 3. Als lengte meer dan 3 is is er dus een
        # submap, welke we apart op willen slaan
        diepte == 2 ~ ifelse(length(pad_split) > 3, # test
          paste(pad_split[2], pad_split[3], sep = "/"), # yes
          pad_split[2]
        ), # no
        diepte == 3 ~ dplyr::case_when(
          length(pad_split) > 4 ~ paste(pad_split[2], pad_split[3], pad_split[4], sep = "/"),
          length(pad_split) > 3 ~ paste(pad_split[2], pad_split[3], sep = "/"),
          TRUE ~ pad_split[2]
        )
      )

      # Tel hoe vaak de functie voorkomt in dit bestand
      komt_voor <- dplyr::case_when(
        # Er wordt geteld hoe vaak de functie in totaal voorkomt. De lengte
        # van lijst[[bestandspad]] geeft aan hoe vaak de functie in een
        # bestand voorkomt
        tellen == "aantal_keer" ~ length(lijst[[bestandspad]]),
        # Er wordt alleen bijgehouden hoe vaak een functie in een bestand
        # voor komt
        tellen == "aantal_bestanden" ~ as.integer(1)
      )

      # Als naam niet voor komt, maak kolom aan
      if (!(locatie %in% colnames(functies_package))) {
        nieuwe_kolom <- integer(length(functies_package$Functie))
        functies_package <- cbind(functies_package, nieuwe_kolom)
        setnames(functies_package, "nieuwe_kolom", locatie) # Verander naam
      }
      # Voeg de hoeveelheid toe
      functies_package[[locatie]][index] <- functies_package[[locatie]][index] + komt_voor
    }
  }

  ## STAP 3: Relatieve importantheid in de datastraat: Bereken per functie het
  # percentage van hoe uniek een functie is binnen de datastraat. 1.0 = 100%
  # uniek in de dit deel van de datastraat
  functies_package <- ratio_df_columns(functies_package, kolommen)

  ## Stap 4: Sorteer alphabetisch
  functies_package <- functies_package %>%
    dplyr::select(order(colnames(functies_package))) %>%
    dplyr::relocate("Functie") %>% # Verplaats Functie naar voren
    dplyr::relocate("lijst_met_lijst", .after = last_col()) # Verplaats lijst_met_lijst naar achter

  ## Stap 5: return dataframe
  return(functies_package)
}
