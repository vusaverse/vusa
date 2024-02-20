#' Assert naming
#'
#' Functie voor assertions voor een dataframe.
#'
#' @param df Dataframe waar over geassert wordt
#' @param Naming Het bijbehorende documentatie bestand met assert informatie voor
#' het dataframe
#' @param Naam_bestand De naam van het databestand wat bekeken wordt. Deze naam moet
#' uniek zijn in het hele project
#' @param Maak_nieuwe_kolomwaarden True als de functie nog een andere functie, build_kolomwaarden,
#' moet aanroepen om een kolommen bestand te maken waarmee subset/list assertions gedaan
#' kunnen worden.
#' @param Assert_met_veldnaam True als de assertions gedaan worden op de vertaalde namen,
#' dus Veldnaam ipv Veldnaam_export
#' @param Bericht Bericht dat bij de assertion message bijgevoegd wordt.
#' @param fail "stop" geeft een error, "warning" geeft een warning als de assertions
#' niet geslaagd zijn.
#'
#' @family assertions
#' @family tests
#' @export
assert_naming <- function(df, Naming, Naam_bestand, Maak_nieuwe_kolomwaarden = F, Assert_met_veldnaam = F, Bericht = NULL, fail = "stop") {
  Veldnaam_export <- Veldnaam <- Kolom <- NULL
  ## =============================================================================
  ## 1. VOORBEREIDINGEN
  ## Vind patronen in naming bestand, check of ze nog niet bestaan, en sla op
  ## in csv bestand
  find_patterns(Naming)

  ## Als Assert_na_vertaling op TRUE staat zijn de namen van de kolommen in de
  ## df al vertaald. Dus in de Naming moeten niet de namen in Veldnaam_export,
  ## maar de namen in Veldnaam gebruikt worden voor assertions.
  if (Assert_met_veldnaam == T) {
    Naming <- Naming %>%
      ## Veldnaam_export is niet meer in gebruik, want kolommen zijn al vertaald
      dplyr::select(-Veldnaam_export) %>%
      ## Veldnaam kolom wordt Kolom, want deze wordt gebruikt bij het maken
      ## van de assertions
      dplyr::rename(Kolom = Veldnaam) %>%
      ## Filter de dubbele namen eruit, en selecteer alleen de bovenste rij
      dplyr::distinct(Kolom, .keep_all = T) %>%
      ## Filter de na's eruit, omdat dat de enige kolommen zijn die zijn
      ## overgebleven na de vertaling
      dplyr::filter(!is.na(Kolom))
  } else {
    ## Als Veldnamen twee keer voorkomen in de Naming wordt hiervan
    ## slechts één geselecteerd. Hierbij krijgt een kolom die in het df
    ## voorkomt de voorkeur.
    Naming <- Naming %>%
      ## Vertaal kolom Veldnaam_export naar Kolom, voor consistentie met rest
      ## van de assertion functies
      dplyr::rename(Kolom = Veldnaam_export) %>%
      dplyr::arrange(dplyr::desc(Kolom %in% names(df))) %>%
      dplyr::distinct(Veldnaam, .keep_all = T)
  }

  ## -----------------------------------------------------------------------------
  ## 1. A. Definitiebestand Kolomwaarden: Lees deze in, of maak deze als dit
  ## is aangegeven met de parameter: build_nieuwe_kolommen
  ##
  ## Als het veldtype een subset of list is worden de mogelijke waarden van de kolom
  ## gedocumenteerd (bij eerste keer runnen) of gelezen (bij latere runs)

  if (is.element("subset", Naming$Veldtype) | is.element("set_equal", Naming$Veldtype) | is.element("list", Naming$Veldtype)) {
    # if (any("list","set_equal","subset") %in% Naming$Veldtype) {
    ## Voer build kolomwaarden  uit (optioneel)
    if (Maak_nieuwe_kolomwaarden == T) {
      build_kolomwaarden(df, Naming, Naam_bestand)
    }
    Kolomwaarden <- read_kolomwaarden(Naam_bestand)
  } else {
    Kolomwaarden <- c()
  }

  ## Roep de functie aan die de assertions/tests doet
  check_assertions(df,
    Naming,
    Naam_bestand,
    Maak_nieuwe_kolomwaarden,
    Assert_met_veldnaam,
    ## Geef de kolomwaarden mee waarmee list/subset/setequal assertions
    ## gedaan worden
    Kolomwaarden = Kolomwaarden,
    fail,
    Bericht
  )
}

#' Test naming
#'
#' Functie voor testen van een dataframe op basis van assertions.
#'
#' @param df Dataframe waar over geassert wordt
#' @param Naming Het bijbehorende documentatie bestand met assert informatie voor
#' het dataframe
#' @param Naam_bestand De naam van het databestand wat bekeken wordt. Deze naam moet
#' uniek zijn in het hele project
#' @param Bericht Bericht dat bij de assertion message bijgevoegd wordt.
#' @param Outputmap De Outputmap van het bestand, bijvoorbeeld 1. Ingelezen data
#'
#' @family assertions
#' @family tests
#' @export
test_naming <- function(df, Naming, Naam_bestand, Bericht = NULL, Outputmap = NULL) {
  Veldnaam <- Kolom <- NULL

  ## =============================================================================
  ## 1. VOORBEREIDINGEN
  ## Vind patronen in naming bestand, check of ze nog niet bestaan, en sla op
  ## in csv bestand
  # find_patterns(Naming)

  Naming <- Naming %>%
    dplyr::mutate(Kolom = Veldnaam) %>%
    # rename(Kolom = Veldnaam) %>%
    dplyr::arrange(dplyr::desc(Kolom %in% names(df))) %>%
    dplyr::distinct(Veldnaam, .keep_all = T)

  ## -----------------------------------------------------------------------------
  ## 1. A. Als het veldtype een list is worden de mogelijke waarden van de kolom gelezen

  if (is.element("subset", Naming$Veldtype) | is.element("set_equal", Naming$Veldtype) | is.element("list", Naming$Veldtype)) {
    ## Lees de kolomwaarden in uit het documentatiebestand
    Kolomwaarden <- read_kolomwaarden_testen(Naming)
  } else {
    Kolomwaarden <- c()
  }
  check_assertions(df, Naming, Naam_bestand, Kolomwaarden = Kolomwaarden, Bericht = Bericht, Outputmap = Outputmap, Test_documentatie = TRUE)
}


#' Check assertions
#'
#' Functie voor assertions voor een dataframe.
#'
#' @param df Dataframe waar over geassert wordt
#' @param Naming Het bijbehorende documentatie bestand met assert informatie voor
#' het dataframe
#' @param Naam_bestand De naam van het databestand wat bekeken wordt. Deze naam moet
#' uniek zijn in het hele project
#' @param Kolomwaarden de waarden die mogelijk in een kolom voor kunnen komen
#' @param build_nieuwe_kolommen True als de functie nog een andere functie, build_kolomwaarden,
#' moet aanroepen om een kolommen bestand te maken waarmee list assertions gedaan
#' kunnen worden.
#' @param Assert_na_vertaling True als de assertions gedaan worden op de vertaalde namen,
#' dus Veldnaam ipv Veldnaam_export
#' @param Bericht Bericht dat bij de assertion message bijgevoegd wordt.
#' @param fail "stop" geeft een error, "warning" geeft een warning als de assertions
#' niet geslaagd zijn.
#' @param Test_documentatie Boolean of testdocumentatie gebruikt moet worden of
#' assertdocumentatie
#' @param Outputmap De Outputmap van het bestand, bijvoorbeeld 1. Ingelezen data
#'
#' @family assertions
#' @family tests
#' @export
check_assertions <- function(df, Naming, Naam_bestand, Kolomwaarden = NULL, build_nieuwe_kolommen = F, Assert_na_vertaling = F, fail = "stop", Bericht = NULL, Test_documentatie = FALSE, Outputmap = NULL) {
  In_gebruik <- Kolom <- Uniek <- NULL
  # ## =============================================================================
  # ## 1. VOORBEREIDINGEN

  ## Check of alle veldnamen goed worden ingelezen. Bij vreemde tekens moet UTF8
  ## worden gebruikt.
  if (!all(validUTF8(Naming$Kolom))) {
    stop("De Encoding van het Namingbestand is niet UTF8. Pas de manier van inlezen aan.")
  }

  Naming <- Naming %>%
    dplyr::filter(In_gebruik)

  ## -----------------------------------------------------------------------------
  ## 1. A. Maak een assertion collectie
  Assertions_Collectie <- checkmate::makeAssertCollection()

  ## -----------------------------------------------------------------------------
  ## 1. B. Doe eerste checks op lege data en verkeerde kolommen

  ## Check of data wel rijen bevat
  assertion_meer_dan_0_rijen(df, Assertions_Collectie)

  ## Check of kolommen uit de documentatie voorkomen in de data
  Kolommen_verschil <- assert_aanwezige_kolommen(Naming, df, Assertions_Collectie)
  ## Als er kolommen zijn die niet voorkomen in de data moeten deze worden weggehaald
  ## uit de te testen kolommen
  if (!purrr::is_empty(Kolommen_verschil)) {
    for (Kol in Kolommen_verschil) {
      Naming <- Naming %>%
        dplyr::filter(Kolom != Kol)
    }
  }

  ## -----------------------------------------------------------------------------
  ## 1. C. Selecteer de juiste kolommen uit het namingbestand
  ## Vanuit de naming worden de Kolommen die in gebruik zijn voor assertions
  ## behouden. Voor de overige kolommen worden geen assertions uitgevoerd
  Naming <- Naming %>%
    dplyr::filter(In_gebruik)

  ## =============================================================================
  ## 2. VOER DE ASSERTIONS UIT

  ## -----------------------------------------------------------------------------
  ## 2. A. Unieke identifiers
  ## Als er unieke identifiers bestaan worden deze gefilterd om ze te gebruiken
  ## bij de assertion op duplicates.
  if (T %in% Naming$Uniek) {
    ## Vind de variabelen die geen duplicates hebben
    No_duplicates <- Naming %>%
      dplyr::filter(Uniek)

    ## Assert op duplicates in de combinatie van de kolommen die volgens de
    ## documentatie samen uniek zouden zijn
    assert_no_duplicates_in_group(df,
      group_vars = No_duplicates$Kolom,
      assertion_fail = Assertions_Collectie
    )
  }
  ## -----------------------------------------------------------------------------
  ## 2. B. Numerieke kolommen
  ## Kolommen van types numeric en integer worden gefilterd, en de nodige assertion-gegevens
  ## (any.missing, lower, upper) worden geselecteerd, om hierop te asserten

  ## Functie met assertprocedure per veldtype. Meegegeven wordt het namingbestand,
  ## de assertfunctie, het dataframe, de assertioncollectie voor het assertbericht
  ## de veldtypen die binnen diezelfde soort assertion vallen en de kolommen uit
  ## het namingbestand die nodig zijn voor de assertion.
  subassertfunctie(
    Naming,
    assert_numeric_named,
    df,
    Assertions_Collectie,
    ## Numerieke assertiosn hebben kolom, veldtype, lower en upper nodig
    c("Kolom", "Veldtype", "lower", "upper"),
    c("numeric", "integer")
  )

  ## -----------------------------------------------------------------------------
  ## 2. C. Character-kolommen
  ## Kolommen van type character worden gefilterd, en de nodige assertion-gegevens
  ## (any.missing, pattern) worden geselecteerd, om hierop te asserten
  subassertfunctie(
    Naming,
    assert_character_named,
    df,
    Assertions_Collectie,
    ## Kolom en patroon meegeven voor de assertion
    c("Kolom", "pattern"),
    c("list", "character", "subset")
  )

  ## -----------------------------------------------------------------------------
  ## 2. D. Subset- en set_equal-kolommen
  ## Kolommen van type subset worden gefilterd om hierop te asserten met de Kolomwaarden
  subassertfunctie(Naming,
    assert_character_subset,
    df,
    Assertions_Collectie,
    ## Deze assertion heeft alleen de veldnamen nodig
    c("Kolom"),
    c("list", "subset"),
    ## Kolomwaarden worden meegegeven
    kolomwaarden = Kolomwaarden
  )

  ## Kolommen van type set_equal worden gefilterd om hierop te asserten met de Kolomwaarden
  subassertfunctie(Naming,
    assert_character_set_equal,
    df,
    Assertions_Collectie,
    ## Deze assertion heeft alleen de veldnamen nodig
    c("Kolom"),
    c("set_equal"),
    ## Kolomwaarden worden meegegeven
    kolomwaarden = Kolomwaarden
  )

  ## -----------------------------------------------------------------------------
  ## 2. E. POSIXCT-kolommen
  ## Kolommen van type posixct worden gefilterd, en de nodige assertion-gegevens
  ## (any.missing) worden geselecteerd, om hierop te asserten
  subassertfunctie(
    Naming,
    assert_posixct_named,
    df,
    Assertions_Collectie,
    ## Deze assertion heeft alleen de veldnamen nodig
    c("Kolom"),
    c("posixct", "POSIXct")
  )

  ## 2. E.2 DATE-kolommen
  ## Kolommen van type posixct worden gefilterd, en de nodige assertion-gegevens
  ## (any.missing) worden geselecteerd, om hierop te asserten
  subassertfunctie(
    Naming,
    assert_date_named,
    df,
    Assertions_Collectie,
    ## Deze assertion heeft alleen de veldnamen nodig
    c("Kolom"),
    c("Date")
  )

  ## -----------------------------------------------------------------------------
  ## 2. F. Logical-kolommen
  ## Kolommen van type logical worden gefilterd, en de nodige assertion-gegevens
  ## (any.missing) worden geselecteerd, om hierop te asserten
  subassertfunctie(
    Naming,
    assert_logical_named,
    df,
    Assertions_Collectie,
    ## Deze assertion heeft alleen de veldnamen nodig
    c("Kolom"),
    c("Logical")
  )

  ## -----------------------------------------------------------------------------
  ## 2. G. Percentage NAs
  ## Voor elke kolom dat NAs bevat wordt berekend of het aantal NAs te veel is.
  subassertfunctie(
    Naming,
    controleer_na_waarden,
    df,
    Assertions_Collectie,
    c("Kolom"),
    ## Alle velden moeten worden gecontroleerd op NAs
    c("numeric", "integer", "character", "Date", "posixct", "POSIXct", "list", "subset", "set_equal", "Logical")
  )

  ## =============================================================================
  ## 3. AFRONDING
  ## Indien de check voor testen wordt gedaan, wordt de return_assertions_message_testen gebruikt
  ## om het assert bericht terug te geven in de vorm van een dataframe
  if (Test_documentatie == T) {
    message <- return_assertions_message_testen(Assertions_Collectie, trimws(paste(Naam_bestand, Bericht)), silent = T, Outputmap = Outputmap)
    ## Als de check voor assertions wordt gedaan, wordt de assertion message
    ## die een tekst teruggeeft aangeroepen.
  } else {
    message <- return_assertions_message(Assertions_Collectie, trimws(paste(Naam_bestand, Bericht)), fail = fail)
  }

  ## Retourneer het dataframe ontzichtbaar zodat deze functie gebruikt kan worden in
  ## een magritter-pipe (%>%)
  invisible(df)
  if (Test_documentatie == TRUE) {
    return(message)
  }
}


#' Assert Aanwezige kolommen
#'
#' Functie die checkt of de kolomnamen uit het documentatiebestand overeenkomen
#' met de kolomnamen in het te asserten dataframe
#' @param Naming Naming bestand van assertion
#' @param df dataframe dat geassert wordt
#' @param Collectie Assertions collectie
#' @export
#'
assert_aanwezige_kolommen <- function(Naming, df, Collectie) {
  Kolommen_niet_in_df <- setdiff(Naming$Kolom, names(df))
  if (!purrr::is_empty(Kolommen_niet_in_df)) {
    Collectie$push(paste0(
      "Variable \n\t",
      paste(Kolommen_niet_in_df, collapse = "\n\t"),
      "\n Op deze kolommen wordt niet geassert; ze zitten niet in de data, maar wel in de documentatie."
    ))
    Kolommen_niet_in_doc <- setdiff(names(df), Naming$Kolom)
    # message(paste0('Variable \n\t',
    #              paste(Kolommen_niet_in_doc, collapse = "\n\t"),
    #              '\n Op deze kolommen wordt niet geassert; ze zitten niet in de documentatie, maar wel in de data.\n'))

    return(Kolommen_niet_in_df)
  }
}


#' subassertfunctie
#'
#' Functie die de meegegeven assertion checkt op basis van het meegegeven
#' namingbestand. Dit zijn o.a. numerieke, character en date assertions.
#'
#' @param naming documentatie met de te testen assertions
#' @param assert_functie meegegeven assertfunctie
#' @param df dataframe met de data die geassert wordt
#' @param collectie assertion-collectie
#' @param veldtype Veldtype van de assertion in een character of een lijst met characters
#' @param selecteer_kolommen De kolommen uit de naming die nodig zijn voor de assertion
#' @param kolomwaarden kolomwaarden om de assertions list, subset en set_equal mee
#' te controleren
#' @export
#'
subassertfunctie <- function(naming, assert_functie, df, collectie, selecteer_kolommen, veldtype, kolomwaarden = NULL) {
  Veldtype <- lower <- upper <- NULL
  ## selecteer juiste kolommen en rijen uit namingbestand
  Assert_lijst <- naming %>%
    dplyr::filter(Veldtype %in% veldtype) %>%
    dplyr::select(selecteer_kolommen)

  ## Als de assertfunctie assert_numeric_named wordt gedaan moeten de lower en
  ## upper kolommen die leeg zijn infinity waarden krijgen.
  if (as.character(substitute(assert_functie)) %in% c("assert_numeric_named")) {
    Assert_lijst <- Assert_lijst %>%
      dplyr::mutate(
        lower = dplyr::coalesce(as.double(lower), -Inf),
        upper = dplyr::coalesce(as.double(upper), Inf)
      )
  }
  ## Als de assertion een subset of set_equal assertion is moeten ook de kolomwaarden
  ## mee worden genomen in de pwalk
  if (as.character(substitute(assert_functie)) %in% c("assert_character_subset", "assert_character_set_equal")) {
    ## Loop over alle observaties en roep de assertfunctie aan
    purrr::pwalk(
      ## Assertions van dit type
      Assert_lijst,
      ## De bijbehorende functie
      assert_functie,
      ## Geeft kolomwaarden mee voor testen voorkomende waarden
      Kolomwaarden = kolomwaarden,
      ## Het te testen dataframe
      df = df,
      ## De assertcollectie waar bericht aan moet worden toegevoegd
      add = collectie
    )
    ## Bij de assertion voor NAs moet ook de naming worden meegegeven
  } else if (as.character(substitute(assert_functie)) %in% c("controleer_na_waarden")) {
    ## Loop over alle observaties en roep de assertfunctie aan
    purrr::pwalk(
      ## Assertions van dit type
      Assert_lijst,
      ## De bijbehorende functie
      assert_functie,
      ## Het te testen dataframe
      df = df,
      ## Geef namingbestand mee voor de NA-assertfunctie
      Naming = naming,
      ## De assertcollectie waar bericht aan moet worden toegevoegd
      add = collectie
    )
    ## Bij andere assertfuncties wordt deze pwalk gebruikt
  } else {
    ## Loop over alle observaties en roep de assertfunctie aan
    purrr::pwalk(
      ## Assertions van dit type
      Assert_lijst,
      ## De bijbehorende functie
      assert_functie,
      ## Het te testen dataframe
      df = df,
      ## De assertcollectie waar bericht aan moet worden toegevoegd
      add = collectie
    )
  }
}

#' Controleer NA waarden
#'
#' Deze functie kijkt voor een kolom of het aantal NAs binnen de grenswaarde ligt
#'
#' @param Kolomwaarde De kolom uit het dataframe waar voor op NAs moet worden gecheckt
#' @param df Dataframe waaron moet worden gezocht
#' @param Naming Documentatiebestand waarin de gegevens staan
#' @param add Assertion collectie waar assertion bericht aan moet worden toegevoegd
#' @param fail Vermeld of asserion bij een failure moet stoppen ("stop") of waarschuwen ("warning")
#' @family assertions
#' @family tests
controleer_na_waarden <- function(Kolomwaarde, df, Naming, add, fail = "stop") {
  Kolom <- p_na <- NULL
  ## Tel aantal voorkomende NAs van de kolom in df
  Aantal_NAs <- sum(is.na(df[Kolomwaarde]))

  ## Tel aantal rijen in df
  Totaal_rijen <- nrow(df)
  ## Haal grenswaarde NAs uit naming bestand
  Grenswaarde_NAs <- Naming %>%
    dplyr::filter(Kolom == Kolomwaarde) %>%
    dplyr::select(p_na)

  ## Bereken percentage voorkomende NAs df
  Gevonden_NA_percentage <- Aantal_NAs / Totaal_rijen * 100

  ## Als het percentage NAs in df groter is dan de grenswaarde moet er een assertion message komen
  if (round(Gevonden_NA_percentage, digits = 2) > round(Grenswaarde_NAs, digits = 2)) {
    # add$push(paste0("Toegestaan % NAs is ", round(Grenswaarde_NAs, digits = 2), " en gevonden % NAs is ", round(Gevonden_NA_percentage, digits = 2), ", dus te veel NAs in Kolom ", Kolomwaarde, sep = ": "))
    add$push(paste0("Variable '", Kolomwaarde, "': ", "Toegestaan % NAs is ", round(Grenswaarde_NAs, digits = 2), " en gevonden % NAs is ", round(Gevonden_NA_percentage, digits = 2), ", dus te veel NAs in de Kolom."))
  }
}

#' Vind subset kolommen
#'
#' Deze functie geeft de namen weer van de kolommen uit een dataframe die in gebruik
#' zijn en het veldtype subset (of list) hebben meegekregen. Dit wordt in character type gegeven.
#'
#' @param Naming Een tabel of dataframe met de veldnamen van kolommen van het
#' desbetreffende dataframe met meer informatie.
#' @return Veldnamen die type list hebben
#' @family assertions
vind_subset_kolommen <- function(Naming) {
  In_gebruik <- Veldtype <- NULL
  df_new <- Naming %>%
    ## Factoren moeten worden omgezet naar character
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::filter(In_gebruik == T, Veldtype %in% c("subset", "list", "set_equal"))
  return(df_new$Veldnaam_export)
}

#' Build kolomwaarden
#'
#' Functie om unieke waarden van kolommen te vinden, en deze, met de bijbehorende
#' kolomnaam, weg te schrijven naar een csv bestand.
#'
#' @param df Dataframe waar de kolomnamen van geanalyseerd worden.
#' @param Naming Een tabel of dataframe met de veldnamen van kolommen van het
#' desbetreffende dataframe met meer informatie.
#' @param Naam_bestand Naam van het bestand waar de kolomwaarden voor gebouwd worden,
#' zonder bestandssoort (dus enkel "Namen" of "Gegevens")
#' @export
#' @family assertions
build_kolomwaarden <- function(df, Naming, Naam_bestand) {
  Waarde <- NULL
  ## Vind de kolommen die het subset-type hebben
  Waarden_te_gebruiken <- vind_subset_kolommen(Naming = Naming)

  df_uniek <- df %>%
    ## Selecteer de kolommen uit de df
    dplyr::select(dplyr::one_of(Waarden_te_gebruiken)) %>%
    ## Omklappen van de kolommen
    tidyr::gather(key = "Kolomnaam", value = "Waarde") %>%
    ## Neem alleen de unieke waarden mee, geen NA
    dplyr::distinct() %>%
    dplyr::filter(!is.na(Waarde))

  ## Sla de assertwaarden op in een tabel in een csv file
  path <- paste0(Sys.getenv("DOCUMENTATION_DIR"), "Kolomwaarden/Assert_", Naam_bestand, ".csv")
  utils::write.csv2(df_uniek, file = path, row.names = F, fileEncoding = "UTF-8")
}

#' Read kolomwaarden
#'
#' Functie die het kolomwaarden bestand van de opgegeven bestandsnaam uitleest en
#' doorgeeft als een tibble. Deze tibble bestaat uit Kolomnaam met bijbehorende
#' Waarden
#'
#' @param Naam_bestand Naam van het bestand waar de kolomwaarden voor gebouwd worden,
#' zonder bestandssoort (dus enkel "Namen" of "Gegevens"), maar met aanhalingstekens
#' @return Een tibble met een kolom met Kolomnaam en een kolom met Waarde
#' @family assertions
read_kolomwaarden <- function(Naam_bestand) {
  path <- paste0(Sys.getenv("DOCUMENTATION_DIR"), "Kolomwaarden/Assert_", Naam_bestand, ".csv")
  Kolomwaarden <- readr::read_delim(path,
    col_types = vroom::cols(
      Kolomnaam = vroom::col_character(),
      Waarde = vroom::col_character()
    ),
    delim = ";",
    locale = vroom::locale(
      decimal_mark = ",",
      grouping_mark = "."
    )
  )
  return(Kolomwaarden)
}

#' Read kolomwaarden testen
#'
#' Inlezen van de kolomwaarden bij het testen met de testdocumentatie
#' @param Testdoc Testdocumentatie
read_kolomwaarden_testen <- function(Testdoc) {
  Veldtype <- Veldnaam <- Kolomwaarden <- Waarde <- NULL
  ## Neem van de testdocumentatie alleen de veldnaam en kolomwaarden van de types subset en list
  Documentatie_kolomwaarden <- Testdoc %>%
    dplyr::filter(Veldtype %in% c("subset", "list", "set_equal")) %>%
    dplyr::select(Veldnaam, Kolomwaarden)
  ## Split kolomwaarden op de komma
  Split_kolomwaarden <- strsplit(Documentatie_kolomwaarden$Kolomwaarden, split = "', '")
  ## Creeër nieuwe rij voor iedere kolomwaarde
  df_kolomwaarden <- tibble::tibble(
    Kolomnaam = rep(
      Documentatie_kolomwaarden$Veldnaam,
      sapply(Split_kolomwaarden, length)
    ),
    Waarde = unlist(Split_kolomwaarden)
  ) %>%
    ## Verwijder onnodige tekens en spaties
    dplyr::mutate(Waarde = gsub(pattern = "'", replacement = "", x = Waarde))

  return(df_kolomwaarden)
}


#' Find patterns
#'
#' Functie die zoekt naar unieke patterns in naming bestand
#' @param Naming Naming bestand waarin voor patterns gezocht wordt
#' @family assertions
#' @family tests
find_patterns <- function(Naming) {
  ## Check of pattern-kolom bestaat
  patterns <- c()
  if ("pattern" %in% colnames(Naming)) {
    ## Loop door kolom, zoek naar niet NA velden en onthoud alleen de patronen die
    ## er niet al in zitten
    for (row in Naming$pattern) {
      if (!is.na(row) & !is.null(row) & !(row %in% patterns) & row != "") {
        patterns <- c(patterns, row)
      }
    }
  }
  ## Als de lijst met patronen niet leeg is, wordt de Write functie aangeroepen
  if (length(patterns) > 0) {
    write_pattern_table(patterns)
  }
}

#' Write patterns
#'
#' Functie die pattern wegschrijft naar csv bestand als deze er nog niet instaat
#' @param pattern Patroon dat moet worden weggeschreven
#' @family assertions
#' @family tests
write_pattern_table <- function(pattern) {
  ## Path waar de tabel moet worden opgeslagen
  path <- paste0(Sys.getenv("DOCUMENTATION_DIR"), "Testpatterns/Patterns.csv")
  Pattern_bestand <- utils::read.csv2(file = path)
  ## creeër map om bestanden in op te slaan
  # dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  for (Single_pattern in pattern) {
    if (!is.null(Single_pattern) & !is.na(Single_pattern) & !(Single_pattern %in% Pattern_bestand$Pattern)) {
      utils::write.table(Single_pattern, file = path, row.names = F, col.names = F, append = T)
      message(paste("Pattern added to testpatterns:", Single_pattern))
    }
  }
}
