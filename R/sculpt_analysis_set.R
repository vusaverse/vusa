#' Sculpt Analysis set
#'
#' Wrapper rondom source() om de analyseset op te bouwen. Hierin wordt de analyseset
#' opgegeven en de naam van het script dat gesourced moet worden.
#' De functie maakt een kleine analyseset op basis van de volgende koppelvelden:
#' \itemize{
#'          \item INS_Studentnummer
#'          \item INS_Opleidingsnaam_2002
#'          \item INS_Inschrijvingsjaar
#'          \item INS_Studiejaar
#'          \item INS_Inschrijvingsjaar_EOI
#'          \item INS_Opleidingsfase_BPM
#'          \item INS_ID_Student_Opleiding_Jaar
#'          \item INS_ID_Student_Opleiding_Jaar_EOI
#'          \item Optioneel: De velden opgegeven in Extra_sleutels
#'          }
#' Daarnaast kunnen extra variabelen opgegeven worden als die gebruikt worden
#' in het script met behulp van de Extra_sleutels parameter.
#' @param Analysis_set De Analyseset
#' @param file De locatie van het script
#' @param Extra_sleutels Variabelenaam als textwaarde, of vector met meerdere
#' textwaaren van variabelenamen. Dn die gebruikt worden bij het uitvoeren van het
#' toevoegscript
#' @param slack Bij TRUE(standaard), worden debugging berichten ook naar Slack gestuurd.
#' @export
sculpt_analysis_set <- function(Analysis_set, file, Extra_sleutels = NULL, slack = T) {
  # .Deprecated("sculpt_analysis_set")
  # Analysis_set_7 <- NULL
  ## Zet de verrijkte Analyseset weg
  Analysis_set_voor <- Analysis_set

  ## Bepaal de key-values waarop gekoppeld moet worden
  ## Er zijn 8 sleutelkolommen die in veel gevallen nodig zijn,
  ## deze worden sowieso meegegevens
  Sleutelkolommen <- c(
    "INS_Studentnummer",
    "INS_Opleidingsnaam_2002",
    "INS_Inschrijvingsjaar",
    "INS_Studiejaar",
    "INS_Inschrijvingsjaar_EOI",
    "INS_Opleidingsfase_BPM",
    "INS_ID_Student_Opleiding_Jaar",
    "INS_ID_Student_Opleiding_Jaar_EOI",
    Extra_sleutels
  )

  ## Selecteer alleen deze key values
  Analysis_set <- Analysis_set %>%
    dplyr::select(dplyr::one_of(Sleutelkolommen))

  ## Voer de source out. Heel belangrijk: met local = T. Daarmee wordt dit script
  ## binnen deze functie uitgevoerd
  source(file, local = T)

  ## RETURN ANALYSESET 7 ALS DIE GEMAAKT WORDT
  ## inherits = FALSE zorgt ervoor dat Analysis_set_7 geretourneerd wordt als deze
  ## binnen deze functie is gesourced cq opgebouwd
  if (exists("Analysis_set_7", inherits = FALSE)) {
    print("Analyseset 7 generated")
    return(Analysis_set_7)
  }

  ## Bepaal de nieuwe kolommen
  Nieuwe_kolommen <- setdiff(names(Analysis_set), Sleutelkolommen)

  ## Bepaal of de kolommen in de oude Analyseset zitten
  Verwijder_kolommen <- intersect(Nieuwe_kolommen, names(Analysis_set_voor))

  ## Verwijder de nieuwe kolommen uit de verrijkte analyseset als die bestaan
  if (length(Verwijder_kolommen) > 0) {

    ## filter RES variabelen
    Verwijder_kolommen_zonder_RES <- Verwijder_kolommen[!grepl("^RES", Verwijder_kolommen)]
    ## Print deze kolommen naar de console
    vvauditor::md_list(Verwijder_kolommen, header = "Columns are already present in the analysis set. These will be overwritten:")

    ## stuur bericht naar slack kanaal
    if (length(Verwijder_kolommen_zonder_RES) > 0) {
    slackr::slackr(cat(paste("Columns are already present in the analysis set. These will be overwritten (sans RES_):"),
                       Verwijder_kolommen_zonder_RES,
                       sep = "\n"))
    }


    ## Verwijder de kolommen uit de complete dataset
    Analysis_set_voor <- Analysis_set_voor %>%
      dplyr::select(-dplyr::one_of(Verwijder_kolommen))

  }

  ## Voeg de nieuwe kolommen toe aan Analysis_set_compleet
  Analysis_set_compleet <- dplyr::left_join(Analysis_set_voor,
                                            Analysis_set,
                                            by = Sleutelkolommen)

  ## Tests uitvoeren op de analyseset

  vvmover::count_rows_analysis_set(Analysis_set_compleet,
                                   file,
                                   nrow(Analysis_set_voor),
                                   slack = slack)

  ## Return de analyseset
  return(Analysis_set_compleet)
}
