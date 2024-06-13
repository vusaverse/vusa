#' Assert Character named
#'
#' Een functie om checkmate::assert_character te gebruiken door een dataframe
#' op te geven en de kolomnamen in een vector. Dit is nuttig als je deze functie
#' wil aanroepen vanuit purrr
#' @param Kolom een Character vector of string met de kolomnaam die getest moet
#' worden
#' @param df het dataframe dat de kolom bevat
#' @param prefix_kolom standaard NULL. Als deze opgegeven wordt, wordt in de
#' assertion message een tekst voor de variabelenanaam geplakt.
#' @param pattern het pattern argument wordt doorgegeven aan de functie assert_character.
#' Als het argument NA is, wordt deze veranderd naar NULL
#' @param ... De overige parameters worden meegegeven aan de functie assert_character
#' @family assertions
#' @family tests
#' @export
assert_character_named <-
  function(Kolom, df, prefix_kolom = NULL, pattern, ...) {
    ## Als het pattern argument NA is, wordt deze NULL gemaakt.
    if (is.na(pattern) | pattern %in% c("", " ")) {
      pattern <- NULL
    }
    checkmate::assert_character(df[[Kolom]],
      .var.name = trimws(paste(prefix_kolom, Kolom)),
      pattern = pattern,
      ...
    )
  }



#' Assert Numeric named
#'
#' Een functie om checkmate::assert_numeric te gebruiken door een dataframe
#' op te geven en de kolomnamen in een vector. Dit is nuttig als je deze functie
#' wil aanroepen vanuit purrr
#' @param Kolom een Character vector of string met de kolomnaam die getest moet
#' worden
#' @param df het dataframe dat de kolom bevat
#' @param prefix_kolom standaard NULL. Als deze opgegeven wordt, wordt in de
#' assertion message een tekst voor de variabelenanaam geplakt.
#' @param Veldtype standaard "numeric". Als hier "integer" is opgegeven, wordt
#' getoetst of het data-type integer is.
#' @param ... De overige parameters worden meegegeven aan de functie assert_numeric
#' @family assertions
#' @family tests
#' @export
assert_numeric_named <- function(Kolom, df, prefix_kolom = NULL, Veldtype = "numeric", ...) {
  if (Veldtype == "integer") {
    checkmate::assert_integer(df[[Kolom]],
      .var.name = trimws(paste(prefix_kolom, Kolom)),
      ...
    )
  } else {
    checkmate::assert_numeric(df[[Kolom]],
      .var.name = trimws(paste(prefix_kolom, Kolom)),
      ...
    )
  }
}


#' Assert Character subset
#'
#' Een functie om checkmate::assert_subset te gebruiken door een dataframe op
#' te geven, de kolomnamen in een vector en de kolomwaarden waarmee gecheckt wordt.
#'
#' @param Kolom een Character vector of string met de kolomnaam die getest moet
#' worden
#' @param df het dataframe dat de kolom bevat
#' @param Kolomwaarden de mogelijke waarden die de geteste kolom kan bevatten
#' @param prefix_kolom standaard NULL. Als deze opgegeven wordt, wordt in de
#' assertion message een tekst voor de variabelenaam geplakt.
#' @param add Assertion collectie om de assertion-berichten aan toe te voegen
#' @param ... De overige parameters worden meegegeven aan de functie assert_subset
#' @family assertions
#' @family tests
#' @export
assert_character_subset <- function(Kolom, df, Kolomwaarden, prefix_kolom = NULL, add, ...) {
  mstop <- Kolomnaam <- NULL
  ## Neem alleen de huidige kolomnaam mee, om de juiste set van mogelijke waarden
  ## te krijgen en sla deze op in choices
  Kolomwaarden <- Kolomwaarden %>%
    dplyr::filter(Kolomnaam == Kolom)

  choices <- Kolomwaarden$Waarde
  var_name <- trimws(paste(prefix_kolom, Kolom))

  ## Gebruik structuur uit assert_subset functie om de subset assertion toe te
  ## kunnen passen en hier tekst bij te kunnen voegen
  res <- checkmate::checkSubset(
    stats::na.omit(unique(df[[Kolom]])),
    choices
  )
  if (!identical(res, TRUE)) {
    if (is.null(add)) {
      mstop("Assertion on '%s' failed: %s.", var_name, res)
    }
    checkmate::assertClass(add, "AssertCollection", .var.name = "add")
    Assert_info <- sprintf("Variable '%s': %s.", var_name, res)
  }

  ## Vind de voorkomende waarden in de kolom
  ## De waarden die niet in de kolomwaarden-documentatie voorkomen moeten worden
  ## meegegeven aan het failing-assertion bericht
  Voorkomende_waarden <- stats::na.omit(unique(df[[Kolom]]))
  Failing_values <- Voorkomende_waarden[c(!(Voorkomende_waarden %in% choices))]
  if (length(Failing_values) > 0) {
    add$push(paste(Assert_info, "Assertion fail op waarden:", Failing_values))
  }
}

#' Assert Character set equal
#'
#' Een functie om checkmate::assert_set_equal te gebruiken door een dataframe op
#' te geven, de kolomnamen in een vector en de kolomwaarden waarmee gecheckt wordt.
#'
#' @param Kolom een Character vector of string met de kolomnaam die getest moet
#' worden
#' @param df het dataframe dat de kolom bevat
#' @param Kolomwaarden de mogelijke waarden die de geteste kolom kan bevatten
#' @param prefix_kolom standaard NULL. Als deze opgegeven wordt, wordt in de
#' assertion message een tekst voor de variabelenaaam geplakt.
#' @param add Assertion collectie om de assertion-berichten aan toe te voegen
#' @param ... De overige parameters worden meegegeven aan de functie assert_set_equal
#' @family assertions
#' @family tests
#' @export
assert_character_set_equal <- function(Kolom, df, Kolomwaarden, prefix_kolom = NULL, add, ...) {
  mstop <- Kolomnaam <- NULL
  ## Neem alleen de huidige kolomnaam mee, om de juiste set van mogelijke waarden
  ## te krijgen en sla deze op in choices
  Kolomwaarden <- Kolomwaarden %>%
    dplyr::filter(Kolomnaam == Kolom)
  choices <- Kolomwaarden$Waarde
  var_name <- trimws(paste(prefix_kolom, Kolom))

  ## Gebruik structuur van de assert_set_equal functie om de set_equal assertion
  ## uit te voeren en om een extra tekst hieraan toe te voegen, over de waarden
  ## waarop de assertion faalt.
  res <- checkmate::checkSetEqual(
    stats::na.omit(unique(df[[Kolom]])),
    choices
  )
  if (!identical(res, TRUE)) {
    if (is.null(add)) {
      mstop("Assertion on '%s' failed: %s.", var_name, res)
    }
    checkmate::assertClass(add, "AssertCollection", .var.name = "add")
    Assert_info <- sprintf("Variable '%s': %s.", var_name, res)
  }

  ## Vind de voorkomende waarden in de kolom
  ## De waarden die niet in de kolomwaarden-documentatie voorkomen moeten worden
  ## meegegeven aan het failing-assertion bericht
  Voorkomende_waarden <- stats::na.omit(unique(df[[Kolom]]))
  Failing_values <- Voorkomende_waarden[c(!(Voorkomende_waarden %in% choices))]
  if (length(Failing_values) > 0) {
    add$push(paste(Assert_info, "Assertion fail op waarden:", Failing_values))
  }
}

#' Assert posixct named
#'
#' Een functie om checkmate::assert_posixct te gebruiken om de assertion uit te
#' voeren waarmee gecheckt wordt of de waarde een posixct type heeft.
#'
#' @param Kolom een Character vector of string met de kolomnaam die getest moet
#' worden
#' @param df het dataframe dat de kolom bevat
#' @param prefix_kolom standaard NULL. Als deze opgegeven wordt, wordt in de
#' assertion message een tekst voor de variabelenaaam geplakt.
#' @param ... De overige parameters worden meegegeven aan de functie assert_posixct
#' @family assertions
#' @family tests
#' @export
assert_posixct_named <- function(Kolom, df, prefix_kolom = NULL, ...) {
  checkmate::assert_posixct(df[[Kolom]],
    .var.name = trimws(paste(prefix_kolom, Kolom)),
    ...
  )
}


#' Assert date named
#'
#' Een functie om checkmate::assert_date te gebruiken om de assertion uit te
#' voeren waarmee gecheckt wordt of de waarde een Date type heeft.
#'
#' @param Kolom een Character vector of string met de kolomnaam die getest moet
#' worden
#' @param df het dataframe dat de kolom bevat
#' @param prefix_kolom standaard NULL. Als deze opgegeven wordt, wordt in de
#' assertion message een tekst voor de variabelenaaam geplakt.
#' @param ... De overige parameters worden meegegeven aan de functie assert_date
#' @family assertions
#' @family tests
#' @export
assert_date_named <- function(Kolom, df, prefix_kolom = NULL, ...) {
  checkmate::assert_date(df[[Kolom]],
    .var.name = trimws(paste(prefix_kolom, Kolom)),
    ...
  )
}


#' Assert logical named
#'
#' Een functie om checkmate::assert_logical te gebruiken om de assertion uit te
#' voeren waarmee gecheckt wordt of de waarde een logical type heeft.
#'
#' @param Kolom een Character vector of string met de kolomnaam die getest moet
#' worden
#' @param df het dataframe dat de kolom bevat
#' @param prefix_kolom standaard NULL. Als deze opgegeven wordt, wordt in de
#' assertion message een tekst voor de variabelenaam geplakt.
#' @param ... De overige parameters worden meegegeven aan de functie assert_logical
#' @family assertions
#' @family tests
#' @export
assert_logical_named <- function(Kolom, df, prefix_kolom = NULL, ...) {
  checkmate::assert_logical(df[[Kolom]],
    .var.name = trimws(paste(prefix_kolom, Kolom)),
    ...
  )
}


#' Assertion message
#'
#' Function that returns the assertion fail on the specified type. This can be with
#' an AssertCollection, a warning or a stop.
#' @param message The assertion message to return.
#' @param assertion_fail The type on which the assertion is returned. The possible
#' ways are "AssertCollection", where the message belongs to an assertion collection
#' is added, "warning", giving only a warning and the
#' function continues, or "stop", where the function stops and returns the message.
#' @export
assertion_message <- function(message, assertion_fail = "stop") {
  if (inherits(assertion_fail, "AssertCollection")) {
    assertion_fail$push(message)
  } else if (assertion_fail == "warning") {
    warning(message)
  } else if (assertion_fail == "stop") {
    stop(message)
  }
}

#' Assert geen duplicates in groep
#'
#' Controleer of er per groep precies 1 rij is. Als er meerdere rijen per groep
#' gevonden zijn, slaagt de assertion niet.
#'
#' @param df Het te controleren dataframe
#' @param group_vars De groep-variabelen als een character-vector. De default is
#' de combinatie van INS_studentnummer, INS_Opleidingsnaam_2002 en INS_Inschrijvingsjaar.
#' @param assertion_fail Hoe de functie reageert bij een fail. Dit is of een "warning",
#' waarbij enkel een warning wordt gegeven over de fail, of een "stop", waarbij de
#' uitvoering van de functie stopt en de melding weergeeft, of een "AssertCollection",
#' waarbij het fail-bericht aan een assertion collectie wordt toegevoegd.
#' @family assertions
#' @family tests
#' @export
assert_no_duplicates_in_group <- function(df,
                                          group_vars = c(
                                            "INS_Studentnummer",
                                            "INS_Opleidingsnaam_2002",
                                            "INS_Inschrijvingsjaar"
                                          ),
                                          assertion_fail = "stop") {
  Aantal_rijen <- NULL

  # Test of de variabelen waarop we testen voorkomen in df, en geef anders een melding
  if (!all(group_vars %in% names(df))) {
    assertion_message(
      paste("Onvoldoende kolommen om dubbele rijen te kunnen bepalen",
        "De volgende kolommen ontbreken:",
        paste(setdiff(group_vars, names(df)), collapse = "\n"),
        sep = "\n"
      ),
      assertion_fail = assertion_fail
    )
  }

  ## Check of de meegegeven kolommen wel in het dataframe zitten, voordat hierop
  ## gegroepeerd kan worden. Anders groeperen we op niet voorkomende kolommen
  ## en krijgen we een verkeerde assertion.
  if (any(group_vars %in% names(df))) {
    ## Maak en tibble
    result <- df %>%
      ## Haal de grouping informatie weg
      dplyr::ungroup() %>%
      ## Groepeer op de opgegeven variabelen
      dplyr::group_by_at(dplyr::vars(dplyr::one_of(group_vars))) %>%
      ## bepaal het aantal unieke waarden voor iedere variabele per groep
      dplyr::summarise(Aantal_rijen = dplyr::n()) %>%
      ## Bereken per variable het aantal waarden dat "te veel" is.
      dplyr::mutate(Aantal_dubbel_per_veld = Aantal_rijen - 1)

    ## Tel het aantal dubbelingen
    Aantal_dubbelingen <- count_more_than_1(result$Aantal_rijen)
    Totaal_dubbel <- sum(result$Aantal_dubbel_per_veld)

    ## Als assertion slaagt moet result op TRUE staan
    result <- TRUE
    variables <- paste(group_vars, collapse = ", ")
    ## Als aantal dubbelingen meer dan 0 wordt dit weergegeven in het assertbericht.
    if (Aantal_dubbelingen > 0) {
      result <- paste(
        "In de combinatie van de kolommen", variables, "zijn er",
        Aantal_dubbelingen,
        "rijen die vaker voorkomen. Het totaal aantal dubbele rijen is",
        Totaal_dubbel
      )
      assertion_message(result, assertion_fail)
    }
  }
  return(invisible(df))
}


#' Retourneer de assertion message van een collectie
#'
#' Geef een bericht over het wel of niet slagen van de assertion test. Hiervoor
#' moet een "assertion collection" van het package checkmate opgegeven worden.
#' Dit bericht wordt teruggegeven in de vorm van een error of een warning.
#'
#' Voor enkele assertions zijn alleen warnings toegestaan, aangezien door een error
#' een script stopt met runnen. Dit is gedaan voor de assertions: percentage missende
#' waarden, dubbelingen, subset en set_equal.
#'
#' @param Collection Een object met de classe AssertCollection
#' @param Name_collection De naam van de collectie. Deze naam wordt in de berichten
#' genoemd
#' @param fail "stop": Bij het falen van de assertions wordt een error gegeven en
#' de uitvoer van het script gestopt. Bij "warning" wordt alleen een warning gegeven
#' @param silent Bij FALSE (default) wordt de succesmelding in de console geprint. Bij TRUE wordt deze niet getoond
#' @param Outputmap Map, zoals 1. Ingelezen data, waarin bestand staat
#'
#' @return De melding of de assertion-test wel of niet geslaagd is
#' @export
return_assertions_message <- function(Collection, Name_collection, fail = "stop", silent = F, Outputmap = NULL) {
  ## Controleer of de collectie de classe AssertCollection heeft.
  stopifnot(class(Collection) == "AssertCollection")

  ## Bepaal het bericht dat wordt teruggegeven
  ## Als de collectie leeg is wordt een succesbericht geprint
  if (Collection$isEmpty()) {
    if (silent) {
      return(invisible(paste(Name_collection, "Assertions WEL geslaagd", sep = ": ")))
    } else {
      return(paste(Name_collection, "Assertions WEL geslaagd", sep = ": "))
    }
    ## Als collectie niet leeg is wordt aangegeven dat de assertions niet geslaagd zijn
  } else {
    ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    ## Bericht voor niet-geslaagde assertion

    ## Bepaal het aantal niet-geslaagde assertions
    aantal_berichten <- length(Collection$getMessages())

    ## De titel van het bericht
    Assertion_titel <- paste(Name_collection,
      ": ",
      aantal_berichten,
      " Assertion",
      ## Maak het woord "asserion" meervoud als er meer dan 1 bericht is
      (function(x) {
        if (x == 1) {
          ""
        } else {
          "s"
        }
      })(aantal_berichten),
      " NIET geslaagd",
      sep = ""
    )

    ## Zoek de assertions die alleen een warning mogen geven, geen error.
    ## Dit is gewenst bij:
    # - Percentage NAs
    # - Subset of setequal
    # - No duplicates
    assertionberichten_alleen_warning <- c()
    assertionberichten_ook_error <- c()
    for (message in Collection$getMessages()) {
      ## Zoek assertionberichten die niet als error mogen worden teruggegeven
      if (grepl("Must be a subset of", message) | grepl("Must be equal to set", message) | grepl("Toegestaan % NAs is", message) | grepl("aantal dubbele rijen", message)) {
        assertionberichten_alleen_warning <- c(assertionberichten_alleen_warning, message)
      } else {
        ## Andere assertionberichten mogen wel als error
        assertionberichten_ook_error <- c(assertionberichten_ook_error, message)
      }
    }

    ## Alle berichten onder elkaar, met een opsomming
    Assertion_berichten <- paste(
      ## Opsomming(nummering)
      cli::style_bold(paste0(1:aantal_berichten, ".")),
      ## Alle berichten onder elkaar
      Collection$getMessages(),
      collapse = "\n"
    )

    ## Voeg de titel en de assertions samen, en bepaal de opmaak
    Return_message <- paste(cli::style_bold(cli::col_red(Assertion_titel)),
      cli::col_cyan(Assertion_berichten),
      sep = "\n"
    )
  }

  message(Return_message)

  ## Afhankelijk van de fail parameter wordt een error of een warning gegeven.
  if (fail == "stop") {
    ## Afhankelijk van de aanwezigheid van een assertion die een error mag geven
    ## wordt een error of een warning gegeven
    ## De assertions Percentage missings, is subset/setequal en dubbelingen mogen
    ## alleen warnings geven; assertions op het veldtype mogen wel een error geven
    if (length(assertionberichten_ook_error) > 0) {
      stop(Return_message)
    } else {
      return(
        warning(Return_message)
      )
    }
  } else if (fail == "warning") {
    return(
      warning(Return_message)
    )
  }
}


#' Return assertions message testen
#'
#' Geef een dataframe over het wel of niet slagen van de test. Hiervoor
#' moet een "assertion collection" van het package checkmate opgegeven worden
#' @param Collection Een object met de classe AssertCollection
#' @param Name_collection De naam van de collectie. Deze naam wordt in de berichten
#' genoemd
#' @param silent Bij FALSE (default) wordt de succesmelding in de console geprint. Bij TRUE wordt deze niet getoond
#' @param Outputmap Map, zoals 1. Ingelezen data, waarin bestand staat
#'
#' @return De melding of de assertion-test wel of niet geslaagd is
#' @export
return_assertions_message_testen <- function(Collection, Name_collection, silent = F, Outputmap = NULL) {
  ## Controleer of de collectie de classe AssertCollection heeft.
  stopifnot(class(Collection) == "AssertCollection")

  ## Bepaal het bericht dat wordt teruggegeven
  ## Als de collectie leeg is wordt een succesbericht geprint
  if (Collection$isEmpty()) {
    Bestand <- Name_collection
    Outputmap <- Outputmap
    Geslaagd <- TRUE
    Variabele <- NA
    Reden_melding <- NA
    Message <- NA
    ## Als collectie niet leeg is wordt aangegeven dat de assertions niet geslaagd zijn
  } else {
    ## Als Testen_documentatie TRUE is worden de resultaten in een df teruggegeven,
    ## ipv in een bericht
    ## Haal berichten op
    Messages <- Collection$getMessages()
    Messages_te_bekijken <- Messages[stringr::str_detect(Messages, "Variable")]
    ## Assertion niet geslaagd want niet leeg
    Geslaagd <- FALSE
    ## Vind de reden van de melding (dus waar test op vastloopt) voor elke regel
    Reden_melding <- purrr::map_chr(Messages_te_bekijken, vind_assertion_soort)
    ## Vind de variabele waar het misgaat
    Variabele <- stringr::str_extract(Messages_te_bekijken, pattern = "(?<=Variable ')[a-zA-Z0-9_=, ]*(?=')")
    Bestand <- Name_collection
    Message <- purrr::map_chr(Messages_te_bekijken, stringr::str_extract, pattern = "(?<=: ).*")
    Outputmap <- Outputmap
  }

  ## Return dataframe met informatie
  result <- dplyr::tibble(
    Bestand = Bestand,
    Outputmap = Outputmap,
    Variabele = Variabele,
    Geslaagd = Geslaagd,
    Assertion_soort = Reden_melding,
    Message = Message
  )
  return(result)
}

#' Vind assertion soort
#'
#' Functie om per regel (assertion) te vinden welke specifieke assertion erbij hoort
#' @param Regel Regel/character waarin gezocht wordt naar soort assertion
#' @return Characters die in 1 woord omschrijven om welke assertion het gaat
vind_assertion_soort <- function(Regel) {
  if (grepl("Must be of type 'numeric'", Regel)) {
    return("Numeric")
  }
  if (grepl("All elements must be", Regel)) {
    return("Numeric")
  }
  if (grepl("Element [a-zA-Z0-9]* is not", Regel)) {
    return("Numeric")
  }
  if (grepl("Must be of type 'integer'", Regel)) {
    return("Integer")
  }
  if (grepl("Must be of type 'character'", Regel)) {
    return("Character")
  }
  if (grepl("Must be a subset of", Regel)) {
    return("Subset")
  }
  if (grepl("Must be equal to set", Regel)) {
    return("Set_equal")
  }
  if (grepl("Must be of class 'Date'", Regel)) {
    return("Date")
  }
  if (grepl("Must be of type 'POSIXct'", Regel)) {
    return("POSIXct")
  }
  if (grepl("Must be of type 'logical'", Regel)) {
    return("Logical")
  }
  if (grepl("Toegestaan % NAs", Regel)) {
    return("Missende_waarden")
  }
  if (grepl("Op deze kolommen wordt niet geassert", Regel)) {
    return("Kolom_niet_in_data")
  }
  ## Dit gaat over: 'Aantal_dubbelingen == 0': Must be TRUE.
  ## De melding is nu 'Variabelenaam': Must be TRUE, maar hier wordt gecheckt
  ## of het aantal dubbelingen voor deze variabele 0 is.
  if (grepl("dubbelingen", Regel)) {
    "Uniek"
  }
  # if (grepl("Assertion fail op waarden:", Regel)) {
  #
  # }
  else {
    NA
  }
}

#' Assert dataframe export
#'
#' Functie die checkt of data meer dan 0 rijen heeft, geen dubbele rijen heeft,
#' geen kolommen heeft met enkel 0 en geen kolommen heeft met enkel NA.
#' @param df data die moet worden geassert
#' @param kolommen_uniek Kolommen waar op duplicates wordt geassert. Default is
#' INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Studiejaar, INS_Inschrijvingsjaar.
#' @export
#'
assert_dataframe_export <- function(df, kolommen_uniek = c(
                                      "INS_Studentnummer",
                                      "INS_Opleidingsnaam_2002",
                                      "INS_Studiejaar",
                                      "INS_Inschrijvingsjaar"
                                    )) {
  Collectie <- checkmate::makeAssertCollection()

  ## Assert op meer dan 0 rijen
  assertion_meer_dan_0_rijen(df, Collectie)

  ## Als dataframe geen rijen heeft slagen de verdere assertions ook niet
  if (Collectie$isEmpty()) {
    ## Assert op geen dubbele rijen
    # assertion_geen_dubbele_rijen(df, Collectie, kolommen_uniek)
    assert_no_duplicates_in_group(group_vars = kolommen_uniek, df, Collectie)
    ## Assert op geen kolommen met enkel 0
    assertion_geen_kolommen_met_enkel_0(df, Collectie)
    ## Assert op geen kolommen met enkel NA
    assertion_geen_kolommen_met_enkel_na(df, Collectie)
  }

  ## Assertion message om terug te geven met naam van het dataframe.
  return_assertions_message(Collectie, deparse(substitute(df)), fail = "warning")
}


#' @title assertion_meer_dan_0_rijen
#' @description Check of er meer dan 0 rijen in de dataframe zitten.
#' @param df Dataframe
#' @param Collectie lijst
#' @return Collectie
#' @examples \dontrun{
#' assertion_meer_dan_0_rijen(df, Collectie)
#' }
#' @export
assertion_meer_dan_0_rijen <- function(df, Collectie) {
  # assert_true(nrow(df) > 0)
  if (!nrow(df) > 0) {
    Collectie$push("De dataset bevat 0 rijen")
  }
  Collectie
}


#' @title assertion_geen_dubbele_rijen
#' @description Check of er geen dubbele rijen zijn
#' @param df Dataframe
#' @param Collectie lijst
#' @param kolommen_uniek Default: c("INS_Studentnummer", "INS_Opleidingsnaam_2002", "INS_Studiejaar", "INS_Inschrijvingsjaar")
#' @return Collectie
#' @examples \dontrun{
#' assertion_geen_dubbele_rijen(df, Collectie)
#' }
#' @export
assertion_geen_dubbele_rijen <- function(df, Collectie, kolommen_uniek = c(
                                           "INS_Studentnummer",
                                           "INS_Opleidingsnaam_2002",
                                           "INS_Studiejaar",
                                           "INS_Inschrijvingsjaar"
                                         )) {
  # Test of de variabelen waarop we testen voorkomen in df;
  # Zo nee, geef een melding
  if (!checkmate::testNames(kolommen_uniek, subset.of = names(df))) {
    Collectie$push(paste("Onvoldoende kolommen om dubbele rijen te kunnen bepalen",
      "De volgende kolommen ontbreken:",
      paste(setdiff(kolommen_uniek, names(df)), collapse = "\n"),
      sep = "\n"
    ))
    return(Collectie)
  }
  ## Zo ja, ga door met controle duplicates met de unieke kolommen
  assert_no_duplicates_in_group(group_vars = kolommen_uniek, df, Collectie)

  return(Collectie)
}


#' @title assertion_geen_kolommen_met_enkel_0
#' @description Check of er geen kolommen met enkel 0 waarden zijn
#' @param df Dataframe
#' @param Collectie Lijst
#' @return Collectie
#' @examples \dontrun{
#' assertion_geen_kolommen_met_enkel_0(dfInspect, Collectie)
#' }
#' @export
assertion_geen_kolommen_met_enkel_0 <- function(df, Collectie) {
  p_zeros <- variable <- NULL

  ## Creëer dataframe met percentage 0 per kolom
  df_zeros <- data.frame(p_zeros = round(100 * sapply(
    df,
    function(x) sum(x == 0, na.rm = T)
  ) / nrow(df), 2))
  df_zeros$variable <- rownames(df_zeros)
  rownames(df_zeros) <- NULL

  if (any(df_zeros$p_zeros == 100)) {
    Variabelen <- df_zeros %>%
      dplyr::filter(p_zeros == 100) %>%
      dplyr::select(variable) %>%
      unlist() %>%
      toString()
    Collectie$push(paste(
      "De volgende kolommen bevatten 100% 0:",
      Variabelen
    ))
  }
  Collectie
}

#' @title assertion_geen_kolommen_met_enkel_na
#' @description Check of er geen kolommen met enkel NA values zijn
#' @param df Dataframe
#' @param Collectie Lijst
#' @return Collectie
#' @examples \dontrun{
#' assertion_geen_kolommen_met_enkel_na(df, Collectie)
#' }
#' @export
assertion_geen_kolommen_met_enkel_na <- function(df, Collectie) {
  p_na <- variable <- NULL

  ## Creëer dataframe met percentage NA per kolom
  df_na <- data.frame(p_na = round(100 * sapply(df, function(x) sum(is.na(x))) / nrow(df), 2))
  df_na$variable <- rownames(df_na)
  rownames(df_na) <- NULL

  if (any(df_na$p_na == 100)) {
    variabelen <- df_na %>%
      dplyr::filter(p_na == 100) %>%
      dplyr::select(variable) %>%
      unlist() %>%
      toString()
    Collectie$push(paste(
      "De volgende kolommen bevatten 100% NA's:",
      variabelen
    ))
  }
  Collectie
}
