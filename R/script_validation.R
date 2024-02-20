## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script validation.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2019 VU
## Web Page: http://www.vu.nl
## Contact: Theo Bakker (t.c.bakker@vu.nl)
## Verspreiding buiten de VU: Ja
##
## Doel: Met deze functies kunnen scripts worden gevalideerd
##
## Afhankelijkheden: Cli, lintr, purrr en stringr
##
## Datasets: Afhankelijk van script dat is meegegeven
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO:
## 1) ___.
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 25-06-2019: DK: Aanmaak bestand
## 03-07-2019: DK: Review verwerkt
## 04-07-2019: DK: Klaar gemaakt voor eerste release
## 31-07-2019: DK: Review tweede release verwerkt
## 27-11-2019: JJ: Toevoegen van export naar dataframe argument
## en toevoegen lintr exclusions
## 20-04-2020: JJ: Toevoegen van test wegschrijven bestand aan export dataframe
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#' Valideer script
#'
#' Een functie voor het valideren van een script met behulp van een aantal tests.
#'
#' Het script wordt gevalideerd op het volgende:
#'
#' - Geeft het script waarschuwingen of errors?
#'
#' - Zijn indien nodig assertions aanwezig?
#'
#' - Worden alle variabelen op het eind van het script opgeruimd?
#'
#' - Voldoen alle weggeschreven bestanden aan de documentatie?
#'
#' - Voldoet de introductie aan de stijlgids?
#'
#' - Volgt het script de stijlgids?
#'
#' - Welke scripts zijn afhankelijk van de output bestanden van het script?
#'
#' Deze functie is primair geschikt voor de inlees en manipuleer scripts. Scripts
#' die afhankelijk zijn van eerder uitgevoerde scripts kunnen gedeeltelijk niet werken.
#'
#' Let op: om de scripts te testen worden deze eenmaal uitgevoerd en worden bestanden
#' weg geschreven. Bij sommige scripts kan deze validatie daarom wat langer duren.
#'
#' @param filepath De map en bestandsnaam van het script dat getest moet worden binnen het project.
#' Bijvoorbeeld: "01. Inlezen/Inlezen Afhakers.R". Als het filepath niet is
#' opgegeven, wordt het script dat in rstudio open staat gecontroleerd
#' @param export_naar_dataframe Logical value die aangeeft of de resultaten
#' van de functie in een dataframe geëxporteerd moeten worden
#'
#' @return Per test wordt het resultaat weergeven. Dit gebeurd in de volgorde zoals hierboven beschreven.
#' "Geslaagd" geeft aan dat een test zonder problemen is afgerond.
#' Mocht dit niet het geval zijn dan geeft een waarschuwing in het rood meer informatie.
#' De laatste output weergeeft eventuele scripts die afhankelijk zijn van het script in blauw.
#'
#' @family Script validatie
#'
#' @export
validate_script <- function(filepath = NULL, export_naar_dataframe = FALSE) {
  start.time <- Sys.time()
  if (is.null(filepath)) {
    filepath <- this.path::sys.path()
  }
  if (export_naar_dataframe == FALSE) {
    validate_no_warning_errors(filepath, export_naar_dataframe)
    validate_assertions_present(filepath, export_naar_dataframe)
    validate_write_files(filepath)
    validate_introduction(filepath, export_naar_dataframe)
    validate_clear_script_objects(filepath, export_naar_dataframe)
    if (check_installed_package("lintr", check = TRUE)) {
      validate_style(filepath, export_naar_dataframe)
    } else {
      base::cat(cli::style_bold(cli::col_cyan("Instaleer lintr met \"library(\"lintr\")\" om te testen op stijl")))
    }
    compare_input_output(filepath, export_naar_dataframe)
    base::cat("\n")
    Clear_script_objects(bestandspad = filepath, envir = globalenv())
  } else {
    Waarschuwingen_errors_messages <- validate_no_warning_errors(
      filepath,
      export_naar_dataframe
    )
    error_message <- Waarschuwingen_errors_messages$error
    warning_message <- as.character(list(Waarschuwingen_errors_messages$warning))
    error_result <- Waarschuwingen_errors_messages$result
    Assertions_aanwezig <- validate_assertions_present(
      filepath,
      export_naar_dataframe
    )
    Opruimen_variabelen <- as.character(list(validate_clear_script_objects(
      filepath,
      export_naar_dataframe
    )))
    Introductie <- validate_introduction(filepath, export_naar_dataframe) %>%
      unique()
    Afhankelijkheden <- as.character(list(compare_input_output(
      filepath,
      export_naar_dataframe
    )))
    Stijl <- validate_style(filepath, export_naar_dataframe)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    return(tibble::tibble(
      filepath, error_result, error_message,
      warning_message, Assertions_aanwezig, Opruimen_variabelen,
      Introductie, Afhankelijkheden, time.taken, Stijl
    ))
  }
}


#' Valideer assertions aanwezig
#'
#' Een test die checkt of assertions worden gebruikt als er csv/xlsx bestanden worden ingelezen.
#' Hierbij wordt alleen gekeken of er eenmaling assertions aanwezig zijn en niet voor elke
#' output bestand omdat het voorkomt dat er loops worden gebruikt voor de documentatie.
#'
#' @param filepath Het complete filepath van het script dat getest moet worden
#' @param export_naar_dataframe Default: FALSE. Whether to export results to a data frame.
#'
#' @family Script validatie
#'
validate_assertions_present <-
  function(filepath, export_naar_dataframe = FALSE) {
    if (export_naar_dataframe == FALSE) {
      base::cat("Assertions aanwezig: ")
    }
    ## Lees de regels van het script in
    Regels <- base::readLines(filepath, warn = FALSE)
    ## Check of er csv/xlsx filepath in het script staan
    if (TRUE %in% base::grepl("01.", filepath)) {
      ## Check of Assert_naming functie wordt aangeroepen in het script
      if (TRUE %in% base::grepl("assert_naming\\(", Regels)) {
        ## Als aanwezig message dat de test is geslaagd
        if (export_naar_dataframe == TRUE) {
          return("Geslaagd")
        } else {
          base::cat(cli::style_bold(cli::col_green("Geslaagd\n")))
        }
      } else {
        ## Als er geen assertions aanwezig zijn, message dat deze moeten worden toegevoegd
        if (export_naar_dataframe == TRUE) {
          return("Niet geslaagd, voeg deze toe aan het script")
        } else {
          base::cat(cli::style_bold(
            cli::col_red("Niet geslaagd, voeg deze toe aan het script\n")
          ))
        }
      }
    } else {
      if (export_naar_dataframe == TRUE) {
        return("Overgeslagen")
      } else {
        ## Als er geen output bestanden zijn message dat deze zijn overgeslagen
        base::cat(cli::col_black("Overgeslagen\n"))
      }
    }
  }

#' Genereer outputbestanden database
#'
#' Functie die voor de stappen 1,2,3 en 5 de R bestanden die in script in leest
#' opslaat in een database. Deze database kan gevonden worden op de volgende locatie
#' Network_directory/XX. Testbestanden/Overzicht ingelezen bestanden.csv
#'
#' @export
#'
generate_output_files_database <- function() {
  ## Plaats alle R and Rproj bestanden in een list
  Scripts_bron <-
    base::list.files(pattern = "\\.R$|\\.Rproj$", recursive = T)
  ## Kijk voor alle bestanden of de methode ReadRDS_CSV wordt aangeroepen
  Opslaan <- purrr::map_lgl(Scripts_bron,
    str_detect_in_file,
    pattern = "readrds_csv\\(",
    collapse = T
  )
  ## Filter de scripts op stappen 1,2,3 en 5
  Opslaan <-
    Opslaan &
      stringr::str_detect(Scripts_bron, "^01\\.|^02\\.|^03\\.|^05\\.")
  Write_scripts <- Scripts_bron[Opslaan]
  ## Alle ingelezen bestanden naar dataframe en vervolgens naar een RDS-bestand schrijven
  Database <-
    purrr::map_df(Write_scripts, extract_filepaths_readrds_csv)
  saverds_csv(
    Database,
    base::paste0("Overzicht ingelezen bestanden"),
    "XX. Testbestanden/"
  )
}

#' Extraheer bestandspaden ReadRDS CSV
#'
#' Functie die voor Genereer_outputbestanden_database bestandspaden achterhaald van ingelezen bestanden
#'
#' @param bestandspad Het complete bestandspad van het script
#'
#' @export
#'
extract_filepaths_readrds_csv <- function(bestandspad) {
  ## Lees alle regels van het bestand in
  Regels <- base::readLines(bestandspad, warn = FALSE)
  ## Koppel deze achter elkaar zodat afgekapte regels een geheel worden
  Regels_collapsed <-
    base::paste(c(Regels, ""), collapse = "")
  ## Zoek met een regex naar plekken waar een rds of csv bestand wordt ingelezen
  ## en sla voor alle overeenkomsten alles tussen de () op in Regels_read.
  Regels_read <-
    unlist(
      stringr::str_extract_all(
        Regels_collapsed,
        "(?<=readrds_csv\\()(.*?)(?=(\\)))"
      )
    )
  ## Extraheer het bestandspad uit de gevonden ReadRDS_CSV regels
  ## met een regex die kijkt naar bestandspaden die beginnen met een nummer en punt en
  ## eindigen op .rds.
  Rds_string <- "[0-9]\\..*?\\.rds"
  Rds_paden <- stringr::str_extract(Regels_read, Rds_string)
  Rds_paden <- Rds_paden[!is.na(Rds_paden)]

  ## Koppel de gevonden bestandspaden aan het pad van het script waar ze uit komen
  ## en return dit als dataframe
  dependencies <- NULL
  for (pad in Rds_paden) {
    dependencies <-
      base::rbind(
        dependencies,
        base::data.frame(
          Bestand = bestandspad,
          Afhankelijkheid = pad
        )
      )
  }
  return(dependencies)
}

#' Valideer clear_script_objects
#'
#' Test die checkt of de functie clear_script_objects wordt aangeroepen om de
#' script specifieke objecten te verwijderen.
#'
#' @param filepath Het complete filepath van het script
#' @param export_naar_dataframe Default: FALSE. Whether to export results to a data frame.
#'
#' @family Script validatie
#'
validate_clear_script_objects <-
  function(filepath, export_naar_dataframe = FALSE) {
    if (export_naar_dataframe == FALSE) {
      base::cat("Opruimen variabelen: ")
    }
    lines <- scan(filepath,
      what = character(),
      sep = "\n",
      quiet = TRUE
    )
    hasClearScript <- any(grepl("clear_script_object", lines))
    if (!hasClearScript) {
      ## Als er variabelen niet worden opgeruimd message deze dan
      if (export_naar_dataframe == TRUE) {
        return(
          "clear_script_objects() ontbreekt. Stijl van variabelname kan niet worden gecheckt."
        )
      } else {
        base::cat(cli::style_bold(
          cli::col_red("De functie clear_script_objects() ontbreekt \n")
        ))
        base::cat(cli::style_bold(
          cli::col_red("Stijl van variabel namen kan niet gechekt worden \n")
        ))
        base::cat(paste("\n"))
      }
    } else {
      if (export_naar_dataframe == TRUE) {
        return("Geslaagd")
      } else {
        base::cat(cli::style_bold(cli::col_green("Geslaagd\n")))
      }
    }
  }


#' Valideer rm objecten
#'
#' Test die checkt of alle variabelen die worden aangemaakt ook weer
#' worden opgeruimd. De variabelen die niet worden opgeruimd, worden weergegeven
#'
#' @param filepath Het complete filepath van het script
#' @param export_naar_dataframe Default: FALSE. Whether to export results to a data frame.
#'
#' @family Script validatie
#'
validate_rm_objects <- function(filepath, export_naar_dataframe = FALSE) {
  ## message om welke test het gaat
  if (export_naar_dataframe == FALSE) {
    base::cat("Opruimen variabelen: ")
  }
  ## Regex voor het vinden van aanmaken van variabel namen: variable <- ...
  Teststring_assignment <- "^[a-zA-Z_0-9]*(?=(\\s<-))"
  ## Vind alle variabel namen in het script
  Regels <-
    stringr::str_extract(
      base::readLines(filepath, warn = FALSE),
      Teststring_assignment
    )
  Regels <- base::unique(Regels[!base::is.na(Regels)])
  ## Regex voor het vinden van verwijden van variabelen in script: rm(variable)
  Teststring_rm <- "(?<=rm\\()(.+?)(?=(\\)))"
  ## Vind alle regels waar variabelen worden verwijdert
  Delete_regels <-
    base::paste(base::c(base::readLines(filepath, warn = FALSE), ""), collapse = "")
  Delete_regels <-
    stringr::str_extract_all(Delete_regels, Teststring_rm)
  Delete_regels <-
    Delete_regels[!base::is.na(Delete_regels)]
  ## Regex voor het vinden van patronen in verwijderen van variabelen:
  ## rm(list = ls(pattern = pattern))
  Teststring_pattern <-
    "(?<=\\s\\\\\")[a-zA-Z0-9_*]*?(?=(\\\\\"))|(?<=\\s\\\")[a-zA-Z0-9_*]*?(?=(\\\"))"
  Delete_patterns <- NULL
  Delete_variables <- NULL
  ## Loop door alle verwijder regels
  for (i in Delete_regels[[1]]) {
    ## Check of er verwijdert wordt op een patroon, zo ja: sla deze op in Delete_patterns
    if (TRUE %in% grepl("pattern", i)) {
      Delete_patterns <-
        base::c(
          Delete_patterns,
          stringr::str_extract_all(i, Teststring_pattern)
        )
    } else {
      ## Anders sla de variabele op in Delete_variables om later te verwijderen
      Delete_variables <-
        base::c(
          Delete_variables,
          stringr::str_extract_all(i, "([a-zA-Z0-9_]+)")
        )
    }
  }
  ## Bepaal het Verschil tussen de variabelen die zijn aangemaakt en de variabelen
  ## die worden verwijderd
  Verschil <- base::setdiff(Regels, unlist(Delete_variables))

  ## Als er patronen zijn om op te verwijderen, verwijder deze variabelen ook uit het Verschil
  if (base::length(Delete_patterns) != 0) {
    Delete <-
      base::grep(base::paste(Delete_patterns, collapse = "|"), Verschil)
    Verschil <- Verschil[-Delete]
  }
  ## message het resultaat van deze test
  if (length(Verschil > 0)) {
    ## Als er variabelen niet worden opgeruimd message deze dan
    if (export_naar_dataframe == TRUE) {
      return(paste0("Ruim de volgende variabelen op: ", Verschil))
    } else {
      base::cat(cli::style_bold(cli::col_red("Ruim de volgende variabelen op: \n")))
      base::cat(cli::style_bold(cli::col_red(paste(Verschil, collapse = ", \n"))))
      base::cat(paste("\n"))
    }
  } else {
    if (export_naar_dataframe == TRUE) {
      return("Geslaagd")
    } else {
      base::cat(cli::style_bold(cli::col_green("Geslaagd\n")))
    }
  }
}

#' Bepaal databestanden
#'
#' Functie die achterhaald welke bestanden worden weg gelezen of worden weggeschreven
#'
#' @param filepath Het complete filepath van het script
#' @param testString "SaveRDS_CSV" of "ReadRDS_CSV" om te checken naar de files
#'
determine_data_files <- function(filepath, testString) {
  ## Sla de code in de haakjes in de RDS_CSV functies uit het bestand op(ook over meerdere regels)
  Regels <- base::readLines(filepath, warn = FALSE)
  Regels_collapsed <-
    base::paste(c(Regels, ""), collapse = "")
  Regels_RDS_CSV <-
    stringr::str_extract_all(
      Regels_collapsed,
      paste0("(?<=", testString, "\\()(.*?)(?=(\\)))")
    )
  Results_list <- NULL

  ## Vertaal de opgeslagen regels naar filepathen en sla deze op in Results_list
  for (Regel in Regels_RDS_CSV[[1]]) {
    Results_list <-
      base::c(
        Results_list,
        extract_output_file(Regel, testString)
      )
  }
  return(Results_list)
}

#' Bepaal inputbestanden
#'
#' Functie die achterhaald welke bestanden worden weg gelezen
#'
#' @param filepath Het complete filepath van het script
#'
#' @export
determine_input_files <- function(filepath) {
  ## Check of ReadRDS_CSV  wordt gebruikt in het bestand, ga daarna verder naar
  ## bepaal_databestanden.
  testString <- "readrds_csv|ReadRDS_CSV"
  if (TRUE %in% base::grepl(
    testString,
    base::readLines(filepath, warn = FALSE)
  )) {
    determine_data_files(filepath, testString)
  }
}

#' Bepaal outputbestanden
#'
#' Functie die achterhaald welke bestanden worden weg geschreven
#'
#' @param filepath Het complete filepath van het script
#'
#' @export
determine_output_files <- function(filepath) {
  ## Check of SaveRDS_CSV  wordt gebruikt in het bestand, ga daarna verder naar
  ## bepaal_databestanden.
  testString <- "SaveRDS_CSV|saverds_csv"
  if (TRUE %in% base::grepl(
    testString,
    base::readLines(filepath, warn = FALSE)
  )) {
    determine_data_files(filepath, testString)
  }
}

#' Regex content parameter
#'
#' Functie die een regex construeert voor het matchen van de content uit een
#' parameter van een functie
#'
#' @param parameter De parameter waarvan de waarde gezocht moet worden in een functie
#'
#' @export
#'
regex_content_parameter <- function(parameter) {
  base::paste0(
    "(?<=",
    parameter,
    "\\s{0,20}\\=\\s{0,20}\").*"
  )
}

#' Extraheer output filepath
#'
#' Functie die SaveRDS_CSV omschrijft naar kloppend filepath
#'
#' @param regel SaveRDS_CSV of ReadRDS_CSV regel die moet worden omgeschreven naar filepath
#' @param testString "SaveRDS_CSV" of "ReadRDS_CSV" om te vergelijken of het een Read of Save file is
#'
extract_output_file <- function(regel, testString) {
  # Check of het gebruik maakt van de SaveRDS_CSV functie, zo niet: ga door naar checken
  # of het gebruik maakt van een ReadRDS_CSV
  if (stringr::str_detect(testString, "Save")) {
    ## split de regel op komma's
    elementen <- base::unlist(strsplit(regel, ","))
    ## Selecteer het tweede element. Dat is altijd de naam
    naam <- gsub("\\\"|\"|\\s", "", elementen[2])
    ## Check of er meer dan twee variabelen worden meegegeven aan SaveRDS_CSV en of de
    ## derde variabele aangeeft in welke map het bestand moet worden opgeslagen door middel
    ## van output parameter
    if ((base::length(elementen) > 2) &
      (stringr::str_detect(elementen[3], "output"))) {
      ## Extraheer de bestandsmap door middel van een regex. De regex wordt geconstrueert
      ## met behulp van de regex_content_parameter() functie en wordt uit elementen[3]
      ## gehaald. Vervolgens wordt "\"" uit deze string verwijdert.
      pad <-
        base::gsub(
          "\\\"",
          "",
          stringr::str_extract(elementen[3], regex_content_parameter("output"))
        )
      ## plak de map en bestandsnaam aan elkaar en .rds achteraan
      path <- base::paste(pad, naam, ".rds", sep = "")
    }
    ## Check nu of er meer dan twee variabelen worden meegegeven aan SaveRDS_CSV en of de
    ## derde variabele het opgeslagen bestand aangeeft doormiddel van dataloc. Doe vervolgens
    ## hetzelfde als bij de output parameter.
    if ((base::length(elementen) > 2) &
      (stringr::str_detect(elementen[3], "dataloc"))) {
      pad <-
        base::gsub(
          "\\\"",
          "",
          stringr::str_extract(elementen[3], regex_content_parameter("dataloc"))
        )
      path <- base::paste(pad, naam, ".rds", sep = "")
    } else {
      ## Anders: construeer het pad met als map: 1. Ingelezen data. Deze map staat
      ## als default ingesteld
      path <-
        base::paste("1. Ingelezen data/", naam, ".rds", sep = "")
    }
  }
  # Check of het gebruik maakt van de ReadRDS_CSV functie.
  if (stringr::str_detect(testString, "Read")) {
    ## split de regel op komma's
    elementen <- base::unlist(strsplit(regel, ","))
    ## check of het eertse element gebruik maakt van de parameter output, zo ja:
    ## dan is dit de naam van het ingelezen bestand.
    if (stringr::str_detect(elementen[1], "output")) {
      path <- base::gsub(
        "\\\"",
        "",
        stringr::str_extract(elementen[1], regex_content_parameter("output"))
      )
    }
    ## check ander of het eertse element gebruik maakt van de parameter dataloc,
    ## dan is dit namelijk de naam van het ingelezen bestand.
    if (stringr::str_detect(elementen[1], "dataloc")) {
      path <- base::gsub(
        "\\\"",
        "",
        stringr::str_extract(elementen[1], regex_content_parameter("dataloc"))
      )
    }
  }
  return(path)
}

#' Vergelijk input output
#'
#' Functie die achterhaald welke bestanden afhankelijk zijn van de output van een script
#'
#' @param filepath Het complete filepath van het script
#' @param export_naar_dataframe Default: FALSE. Whether to export results to a data frame.
#'
#' @family Script validatie
#'
#' @export
#'
compare_input_output <- function(filepath,
                                 export_naar_dataframe = FALSE) {
  ## message om welke test het gaat
  if (export_naar_dataframe == FALSE) {
    base::cat("Afhankelijkheden: ")
  }
  ## Lees de database in
  Output <- determine_output_files(filepath)
  ## Achterhaal welke bestanden worden weggeschreven
  Scripts <-
    readrds_csv(output = "XX. Testbestanden/Overzicht ingelezen bestanden.rds")
  Dependencies <- base::data.frame()
  ## Selecteer de bestanden uit de database die afhankelijk zijn van het script
  for (Script in Output) {
    results <- Scripts %>%
      dplyr::filter(Bestand == filepath) %>%
      dplyr::select(Afhankelijkheid)

    Dependencies <- dplyr::bind_rows(Dependencies, results)
  }
  ## Filter op unieke waarden
  Dependencies <- unique(Dependencies$Afhankelijkheid)
  ## message het resultaat van de test
  if (length(Dependencies) > 0) {
    if (export_naar_dataframe == TRUE) {
      return(Dependencies)
    } else {
      ## message de dependensies wanneer deze aanwezig zijn
      base::cat(cli::style_bold(cli::col_cyan(paste(
        Dependencies, "\n"
      ))))
    }
  } else {
    if (export_naar_dataframe == TRUE) {
      return("Geen scripts die afhankelijk zijn")
    } else {
      ## message dat er geen scripts afhankelijk zijn wanneer dit geld
      base::cat(cli::style_bold(cli::col_green(
        "Geen scripts die afhankelijk zijn\n"
      )))
    }
  }
}


#' Valideer schrijven bestanden
#'
#' Test of weggeschreven bestanden aan de documentatie voldoen
#'
#' @param filepath Het complete filepath van het script
#' @param export_naar_dataframe Default: FALSE. Whether to export results to a data frame.
#'
#' @family Script validatie
#'
validate_write_files <- function(filepath,
                                 export_naar_dataframe = FALSE) {
  ## message om welke test het gaat
  if (export_naar_dataframe == FALSE) {
    base::cat("Test bij wegschrijven bestanden op documentatie: \n")
  }
  ## Bepaal de bestanden die worden weg geschreven
  Output_bestanden <- determine_output_files(filepath)
  if (is.null(Output_bestanden)) {
    return("Geslaagd")
  }
  ## controle documentatie alleen voor 01. Inlezen en 02. Manipuleren
  if (!any(stringr::str_detect(filepath, c("01.", "02.")))) {
    if (export_naar_dataframe == TRUE) {
      return("Overgeslagen")
    } else {
      base::cat(cli::col_black("Overgeslagen\n"))
    }
  } else {
    ## Test de bestanden die worden weggeschreven op documentatie door de
    ## test_file_on_documentation te mappen op de bestanden
    if (export_naar_dataframe == FALSE) {
      Test_scripts <- purrr::map_dfr(
        Output_bestanden,
        ## .x ipv Output_bestanden
        ~ test_file_on_documentation(.x)
      )
    } else if (export_naar_dataframe == TRUE) {
      Test_scripts <- purrr::map(
        Output_bestanden,
        ~ test_file_on_documentation(.x, TRUE)
      )
      class <- Test_scripts[[1]] %>% class()
      if (class == "character") {
        return(Test_scripts)
      } else {
        for (test in Test_scripts) {
          if (grepl("geen", test[1, 1])) {
            return(test %>% dplyr::distinct() %>%
              base::toString())
          }
          if (!grepl("geen", test[1, 1])) {
            Meldingen_tests <- test %>%
              dplyr::select(Bestand, Variabele, Message) %>%
              dplyr::group_by(Bestand, Variabele) %>%
              dplyr::summarise_all(~ (trimws(paste(
                .,
                collapse = ", "
              )))) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(Message = paste0(Variabele, ": ", Message)) %>%
              dplyr::group_by(Bestand) %>%
              dplyr::summarise_all(~ (trimws(paste(
                .,
                collapse = ", "
              )))) %>%
              dplyr::pull(Message)
            return(Meldingen_tests)
          }
        }
      }
    }
    ## Check of er outputbestanden en dus een dataframe is
    if (base::nrow(Test_scripts != 0)) {
      ## Check of alle tests zijn geslaagd
      if (base::any(Test_scripts$Geslaagd == FALSE)) {
        ## Selecteer gegevens die gemessage moeten worden als een tests niet is geslaagd
        Meldingen_tests <- Test_scripts %>%
          dplyr::filter(Geslaagd == FALSE) %>%
          dplyr::select(Bestand, Message) %>%
          dplyr::distinct() %>%
          base::toString()
        ## message resultaat van de test
        base::cat(cli::style_bold(cli::col_red(unlist(
          Meldingen_tests
        ))))
        if (export_naar_dataframe == TRUE) {
          return(unlist(Meldingen_tests))
        }
      } else {
        if (export_naar_dataframe == TRUE) {
          return("Geslaagd")
        } else if (export_naar_dataframe == FALSE) {
          base::cat(cli::style_bold(cli::col_green("Geslaagd\n")))
        }
      }
    }
  }
}


#' Test bestand op documentatie
#'
#' Functie om per document het documentatiebestand in te lezen, de data in te lezen
#' en de assertion uit te voeren. Zowel het databestand(.rds) als het documentatiebestand(.csv)
#' kan worden meegegeven als filepath. Deze functie wordt zowel gebruikt in de validate_script
#' functie als in XX. Testraamwerk/Testen_scripts.R.
#'
#' @param filepath Het verkorte filepath van het data- of documentatie bestand
#' @param export_naar_dataframe Default: FALSE. Whether to export results to a data frame.
#' @param processbar Mogelijkheid om processbar te weergeven bij gebruik van map en deze functie.
#'
#' @export
test_file_on_documentation <-
  function(filepath,
           export_naar_dataframe = FALSE,
           processbar = NULL) {
    ## Zet een tick op de progress bar
    if ((!is.null(processbar)) &&
      inherits(processbar, "progress_bar")) {
      processbar$tick()
    }
    ## Bepaal naam en folder van filepath
    Naam_folder <- base::dirname(filepath)
    Naam_document <-
      stringr::str_replace(base::basename(filepath), ".csv|.rds", "")

    ## Maak van het bestand de paden naar het documentatie(.csv) en het data(.rds) bestand
    Csv_path <-
      base::paste0(
        "Testdocumentatie/",
        Naam_folder,
        "/",
        Naam_document,
        ".csv"
      )
    Rds_path <-
      base::paste0(Naam_folder, "/", Naam_document, ".rds")

    ## Maak voor het data bestand ook een volledig filepath aan
    Rds_path_volledig <-
      base::paste(
        Sys.getenv("NETWORK_DIR"),
        base::paste(
          "Output",
          Sys.getenv("BRANCH"),
          Naam_folder,
          Naam_document,
          sep = "/"
        ),
        ".rds",
        sep = ""
      )

    ## Check of documentatie bestand aanwezig is en lees deze in
    if (!base::file.exists(base::paste0("XX. Documentatie/", Csv_path))) {
      if (export_naar_dataframe == FALSE) {
        ## message wanneer documentatie niet aanwezig
        base::cat(cli::style_bold(cli::col_red(
          base::paste0(Naam_document, ": er is geen documentatie aanwezig\n")
        )))
        base::return()
      } else if (export_naar_dataframe == TRUE) {
        return(paste0(Naam_document, ": er is geen documentatie aanwezig"))
      }
    } else {
      ## Lees documentatie bestand in
      Test_documentatie <-
        read_documentation(
          filename = Csv_path,
          readr = TRUE
        ) %>%
        ## Extra kolom toevoegen om met de Assert_naming functie te kunnen testen
        dplyr::mutate(Veldnaam_export = Veldnaam)
    }

    ## Controleer of het outputbestand bestaat
    if (!base::file.exists(Rds_path_volledig)) {
      ## Als het bestand niet bestaat, wordt dit teruggegeven
      base::return(
        tibble::tibble(
          Bestand = Naam_document,
          Outputmap = Naam_folder,
          Geslaagd = FALSE,
          Message = "Fout bij uitvoeren van de test: Outputbestand bestaat niet",
          Laatste_update = Sys.time()
        )
      )
    }

    ## Lees databestand in
    data <- readrds_csv(output = Rds_path, readr = TRUE)

    ## Voer tests uit
    assertedData <- tryCatch(
      {
        test_naming(data,
          Test_documentatie,
          Naam_bestand = Naam_document,
          Outputmap = Naam_folder
        ) %>%
          ## Voeg de datum van de laatste update toe.
          dplyr::mutate(Laatste_update = file.info(Rds_path_volledig)$mtime)
      },
      error = function(e) {
        base::return(
          tibble::tibble(
            Bestand = Naam_document,
            Outputmap = Naam_folder,
            Geslaagd = FALSE,
            ## Voeg de foutmelding toe
            Message = base::paste("Fout bij uitvoeren van de test:", e[1]),
            ## Voeg de datum van de laatste update toe.
            Laatste_update = file.info(Rds_path_volledig)$mtime
          )
        )
      }
    )

    assertedData
  }

#' Valideer introductie
#'
#' Functie om script te valideren op volledigheid van de introductie.
#'
#' @param filepath Het complete filepath van het script
#' @param export_naar_dataframe Default: FALSE. Whether to export results to a data frame.
#'
#' @family Script validatie
#'
validate_introduction <-
  function(filepath, export_naar_dataframe = FALSE) {
    ## message welke test wordt uitgevoerd
    if (export_naar_dataframe == FALSE) {
      base::cat("Introductie: ")
    }
    ## Genereer rapportage met generate_introduction_validation() functie
    rapportage <-
      generate_introduction_validation(filepath, Rapportage = FALSE)[, 1:9]
    ## Filter de enventuele fouten en sla deze op
    fouten <-
      colnames(rapportage)[which(rapportage == "TRUE")]
    ## message resultaat van de test
    if (base::length(fouten) > 0) {
      if (export_naar_dataframe == TRUE) {
        return(paste0(fouten, collapse = "; "))
      } else {
        base::cat(cli::style_bold(cli::col_red("Onvolledig: ")))
        ## message de fouten
        base::cat(cli::style_bold(cli::col_red(paste(fouten, "\n"))))
      }
    } else {
      if (export_naar_dataframe == TRUE) {
        return("Geslaagd")
      } else {
        base::cat(cli::style_bold(cli::col_green("Geslaagd\n")))
      }
    }
  }

#' Genereer introductie validatie
#'
#' Functie om scripts te testen op de volledigheid van de introductie. De functie geeft
#' een dataframe terug met daarin de resultaten van de verschillende tests worden
#' uitgevoerd. Optioneel kan er een kleine rapportage worden gemessage. Deze functie wordt gebruikt
#' in de validate_script functie en in 05. Rapporten/XX Rapport SA Controle introducties en bestandsnamen.R
#'
#' @param Script List met filepath(en) van de/het script(s) die gevalideerd moeten worden
#' @param Rapportage Mogelijkheid om samenvattende rapportage te message bij het uitvoeren van het script
#'
#' @export
generate_introduction_validation <-
  function(Script, Rapportage = TRUE) {
    ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    ## 01. Vind bestanden met ontbrekende onderdelen in de introductie
    ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

    # Titel van het bestand
    # Doel: Doel
    # Afhankelijkheden: Afhankelijkheid
    # Datasets: Datasets
    # Titel moet gelijk zijn aan de naam van het bestand

    ## _____________________________________________________________________________
    ## 02.1 Maak een object per soort probleem


    check_pattern_in_file <- function(x, pattern) {
      lines <- readLines(x, warn = TRUE)
      any(grepl(pattern, lines))
    }

    check_title_mismatch <- function(x) {
      lines <- readLines(x, warn = FALSE)
      title_of_file <- utils::tail(strsplit(x, split = "/")[[1]], 1)
      ## We verwachten op de tweede line de titel
      !grepl(title_of_file, lines)[2]
    }

    ## Als 'R code voor Student Analytics Vrije Universiteit Amsterdam' er niet in voorkomt, is er geen introductie aanwezig.
    Ontbrekend_Introductie <-
      !check_pattern_in_file(x = Script, pattern = "R code voor Student Analytics Vrije Universiteit Amsterdam")

    ## Als 'Titel van het bestand:' erin voorkomt, dan is de titel niet ingevuld.
    Ontbrekend_Titel <-
      check_pattern_in_file(x = Script, pattern = "Titel van het bestand:")

    ## Test of 'Verspreiding buiten de VU:' er wel in voorkomt
    Ontbrekend_Verspreiding <-
      !check_pattern_in_file(x = Script, pattern = "Verspreiding buiten de VU:")

    ## Test of 'Doel:' wel is ingevuld
    Ontbrekend_Doel <-
      check_pattern_in_file(x = Script, pattern = "Doel: Doel")

    ## Test of 'Afhankelijkheden:' wel is ingevuld
    Ontbrekend_Afhankelijkheden <-
      check_pattern_in_file(x = Script, pattern = "Afhankelijkheden: Afhankelijkheid")

    ## Test of 'Datasets:' wel is ingevuld
    Ontbrekend_Datasets <-
      check_pattern_in_file(x = Script, pattern = "Datasets: Datasets")

    ## Test of de titel van het bestand wel overeenstemt met de titel in de introductie
    Titel_mismatch <- check_title_mismatch(x = Script)

    ## Test of de titel van het bestand wel overeenstemt met de naamconventie
    ## De conventie is:
    ## - Eerste en tweede woord met hoofdletter
    ## - Spaties in plaats van underscores
    ## - Overige woorden met hoofdletters alleen bij afkortingen (nog niet klaar)
    ## Regex:

    # filenames <-
    #     c(
    #         "Test met een zin.R",
    #         "Test Met een zin.R",
    #         "Test.R",
    #         "test met een zin.R",
    #         "TEST",
    #         "Test Met Een zin.R",
    #         "Test Met EEN zin.R"
    #     )
    #
    # ## Het 3e woord moet niet men hoofdletter starten, tenzij het alleen maar hoofdletters zijn
    # ## Of als het 1e woord XX is.
    # lBestandsnaam <- as.list(strsplit(filenames[7], "[[:space:]]")[[1]])
    # if (length(lBestandsnaam) > 2) {
    #     lBestandsnaam[3]
    #     str_detect(lBestandsnaam[3], "^[:lower:].+$") | str_detect(lBestandsnaam[3], "^[:upper:]+$")
    # } else {
    #     message("Bestandsnaam bestaat uit minder dan 3 woorden")
    # }

    check_naam_conventie_incorrect <- function(x) {
      Bestandsnaam <- utils::tail(strsplit(x, split = "/")[[1]], 1)
      lBestandsnaam <-
        as.list(strsplit(Bestandsnaam, "[[:space:]]|_")[[1]])
      suppressWarnings(any(!stringr::str_detect(lBestandsnaam[1:2], pattern = "^[A-Z].*$")) |
        stringr::str_detect(Bestandsnaam, "_") |
        if (is.null(lBestandsnaam[3][[1]])) {
          FALSE
        } else {
          stringr::str_detect(lBestandsnaam[3], pattern = "^[A-Z].*$")
        })
    }

    Incorrecte_naamconventie <- check_naam_conventie_incorrect(x = Script)

    # ## Rapporteer de uitkomsten van de test
    # if (Rapportage) {
    #   Rapporteer_Uitkomst_test_introductie(Ontbrekend_Introductie, "zonder introductie")
    #   Rapporteer_Uitkomst_test_introductie(Ontbrekend_Titel, "zonder titel")
    #   Rapporteer_Uitkomst_test_introductie(Ontbrekend_Verspreiding, "zonder verspreiding")
    #   Rapporteer_Uitkomst_test_introductie(Ontbrekend_Doel, "zonder ingevuld doel")
    #   Rapporteer_Uitkomst_test_introductie(Ontbrekend_Afhankelijkheden,
    #                                        "zonder ingevulde afhankelijkheden")
    #   Rapporteer_Uitkomst_test_introductie(Ontbrekend_Datasets, "zonder ingevulde datasets")
    #   Rapporteer_Uitkomst_test_introductie(Titel_mismatch,
    #                                        "waarvan de titel niet overeenstemt met de bestandsnaam")
    #   Rapporteer_Uitkomst_test_introductie(
    #     Bestandsnaam_naamconventie_1,
    #     "waarvan de bestandsnaam niet overeenstemt met de naamconventie"
    #   )
    #   Rapporteer_Uitkomst_test_introductie(
    #     Bestandsnaam_naamconventie_2,
    #     "waarvan de bestandsnaam niet overeenstemt met de naamconventie"
    #   )
    #   Rapporteer_Uitkomst_test_introductie(
    #     Bestandsnaam_naamconventie_3,
    #     "waarvan de bestandsnaam niet overeenstemt met de naamconventie"
    #   )
    # }

    ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    ## 02. Maak het rapport
    ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

    ## Maak een dataframe met alle bestanden
    # dfBestanden <-
    #   base::data.frame(
    #     "Bestand" = base::matrix(
    #       base::unlist(Scripts),
    #       nrow = base::length(Scripts),
    #       byrow = T
    #     ),
    #     stringsAsFactors = F
    #   )
    #
    # ## Maak een dataframe per ontbrekend element
    # dfOnvolledige_Introductie <-
    #   if (base::length(Ontbrekend_Introductie)) {
    #     base::data.frame(
    #       "Bestand" = base::matrix(
    #         base::names(base::unlist(Ontbrekend_Introductie)),
    #         nrow = base::length(Ontbrekend_Introductie),
    #         byrow = T
    #       ),
    #       "Introductie" = T,
    #       stringsAsFactors = F
    #     )
    #   } else {
    #     base::data.frame(
    #       "Bestand" = base::character(),
    #       "Introductie" = base::logical(),
    #       stringsAsFactors = F
    #     )
    #   }
    #
    # ## Maak een dataframe per ontbrekend element
    # dfOnvolledige_Titel <-
    #   if (base::length(Ontbrekend_Titel)) {
    #     base::data.frame(
    #       "Bestand" = base::matrix(
    #         base::names(base::unlist(Ontbrekend_Titel)),
    #         nrow = base::length(Ontbrekend_Titel),
    #         byrow = T
    #       ),
    #       "Titel" = T,
    #       stringsAsFactors = F
    #     )
    #   } else {
    #     base::data.frame(
    #       "Bestand" = base::character(),
    #       "Titel" = base::logical(),
    #       stringsAsFactors = F
    #     )
    #   }
    #
    # dfOnvolledig_Verspreiding <-
    #   if (base::length(Ontbrekend_Verspreiding)) {
    #     base::data.frame(
    #       "Bestand" = base::matrix(
    #         base::unlist(base::names(Ontbrekend_Verspreiding)),
    #         nrow = base::length(Ontbrekend_Verspreiding),
    #         byrow = T
    #       ),
    #       "Verspreiding" = T,
    #       stringsAsFactors = F
    #     )
    #   } else {
    #     base::data.frame(
    #       "Bestand" = base::character(),
    #       "Verspreiding" = base::logical(),
    #       stringsAsFactors = F
    #     )
    #   }
    #
    # dfOnvolledig_Doel <-
    #   if (base::length(Ontbrekend_Doel)) {
    #     base::data.frame(
    #       "Bestand" = base::matrix(
    #         base::names(base::unlist(Ontbrekend_Doel)),
    #         nrow = base::length(Ontbrekend_Doel),
    #         byrow = T
    #       ),
    #       "Doel" = T,
    #       stringsAsFactors = F
    #     )
    #   } else {
    #     base::data.frame(
    #       "Bestand" = base::character(),
    #       "Doel" = base::logical(),
    #       stringsAsFactors = F
    #     )
    #   }
    #
    # dfOnvolledig_Afhankelijkheden <-
    #   if (base::length(Ontbrekend_Afhankelijkheden)) {
    #     base::data.frame(
    #       "Bestand" = base::matrix(
    #         base::names(base::unlist(Ontbrekend_Afhankelijkheden)),
    #         nrow = base::length(Ontbrekend_Afhankelijkheden),
    #         byrow = T
    #       ),
    #       "Afhankelijkheden" = T,
    #       stringsAsFactors = F
    #     )
    #   } else {
    #     base::data.frame(
    #       "Bestand" = base::character(),
    #       "Afhankelijkheden" = base::logical(),
    #       stringsAsFactors = F
    #     )
    #   }
    #
    # dfOnvolledig_Datasets <-
    #   if (base::length(Ontbrekend_Datasets)) {
    #     base::data.frame(
    #       "Bestand" = base::matrix(
    #         base::names(base::unlist(Ontbrekend_Datasets)),
    #         nrow = base::length(Ontbrekend_Datasets),
    #         byrow = T
    #       ),
    #       "Datasets" = T,
    #       stringsAsFactors = F
    #     )
    #   } else {
    #     base::data.frame(
    #       "Bestand" = base::character(),
    #       "Datasets" = base::logical(),
    #       stringsAsFactors = F
    #     )
    #   }
    #
    # dfTitel_mismatch <-
    #   if (base::length(Titel_mismatch)) {
    #     base::data.frame(
    #       "Bestand" = base::matrix(
    #         base::names(base::unlist(Titel_mismatch)),
    #         nrow = base::length(Titel_mismatch),
    #         byrow = T
    #       ),
    #       "Titel_mismatch" = T,
    #       stringsAsFactors = F
    #     )
    #   } else {
    #     base::data.frame(
    #       "Bestand" = base::character(),
    #       "Titel_mismatch" = base::logical(),
    #       stringsAsFactors = F
    #     )
    #   }
    #
    # dfBestandsnaam_naamconventie <-
    #   if (base::length(Bestandsnaam_naamconventie)) {
    #     base::data.frame(
    #       "Bestand" = base::matrix(
    #         base::unique(base::names(
    #           base::unlist(Bestandsnaam_naamconventie)
    #         )),
    #         nrow = base::length(Bestandsnaam_naamconventie),
    #         byrow = T
    #       ),
    #       "Bestandsnaam_naamconventie" = T,
    #       stringsAsFactors = F
    #     )
    #   } else {
    #     base::data.frame(
    #       "Bestand" = base::character(),
    #       "Bestandsnaam_naamconventie" = base::logical(),
    #       stringsAsFactors = F
    #     )
    #   }


    # ## Combineer deze tot 1 dataframe
    # dfOnvolledig <- dfBestanden %>%
    #   dplyr::left_join(dfOnvolledige_Introductie, by = "Bestand") %>%
    #   dplyr::left_join(dfOnvolledige_Titel, by = "Bestand") %>%
    #   dplyr::left_join(dfOnvolledig_Verspreiding, by = "Bestand") %>%
    #   dplyr::left_join(dfOnvolledig_Doel, by = "Bestand") %>%
    #   dplyr::left_join(dfOnvolledig_Afhankelijkheden, by = "Bestand") %>%
    #   dplyr::left_join(dfOnvolledig_Datasets, by = "Bestand") %>%
    #   dplyr::left_join(dfTitel_mismatch, by = "Bestand") %>%
    #   dplyr::left_join(dfBestandsnaam_naamconventie, by = "Bestand") %>%
    #   dplyr::mutate(Bestand = stringr::str_replace(Bestand, paste0(wd, "/"), ""))


    # ## Verrijk met de eerste auteur
    # dfOnvolledig$Auteur <-
    #   base::sapply(dfOnvolledig$Bestand, Bepaal_Auteur)[1]

    wd <- base::getwd()
    dfOnvolledig <<- tibble_row(
      "Bestand" = stringr::str_replace(Script, paste0(wd, "/"), ""),
      "Introductie ontbreekt" = Ontbrekend_Introductie,
      "Titel ontbreekt" = Ontbrekend_Titel,
      "Titel mismatch" = Titel_mismatch,
      "Bestandsnaam volgt niet naamconventie" = Incorrecte_naamconventie,
      "Verspreiding ontbreekt" = Ontbrekend_Verspreiding,
      "Doel ontbreekt" = Ontbrekend_Doel,
      "Afhankelijkeden ontbreekt" = Ontbrekend_Afhankelijkheden,
      "Datasets ontbreekt" = Ontbrekend_Datasets
    )

    ## Verrijk met het aantal issues
    dfOnvolledig$Issues_Introductie <-
      rowSums(dfOnvolledig[-1], na.rm = TRUE)

    return(dfOnvolledig)
  }

#' Bepaal Auteur
#'
#' Bepaal auteur van script. Functie voor generate_introduction_validation.
#'
#' @param x Script
#'
determine_author <- function(x) {
  base::unlist(purrr::compact(
    stringr::str_extract_all(base::readLines(x),
      pattern = "([a-zA-Z]{2,3}): Aanmaak bestand"
    )
  ))
}

#' Rapporteer Uitkomst test introductie
#'
#' Functie om de uitkomst van een test te melden. Functie voor generate_introduction_validation.
#'
#' @param obj Object dat moet worden gerapporteerd
#' @param melding Melding die moet worden weergeven
#'
rapport_result_test_introduction <- function(obj, melding) {
  base::message(base::paste(
    base::length(obj),
    dplyr::if_else(base::length(obj) == 1, "bestand", "bestanden"),
    melding
  ))
  if (base::length(obj) > 0) {
    obj
  }
}

#' Check variable names
#'
#' Function to check whether the variable name is according the "stijlgids"
#'
#' @param name Variable name to check
#'
check_variable <- function(name) {
  ## Turn variable name into variable value
  variable <- eval(as.symbol(name))
  if (length(variable) != 1 & !is.list(variable)) {
    class <- "vector"
  } else {
    class <- class(variable)
  }
  replacer <- switch(class[1],
    "character" = "s",
    "list" = "l",
    "vector" = "v",
    "logical" = "b",
    "numeric" = "n",
    "data.frame" = "df",
    "tbl_df" = "df",
    "spec_tbl_df" = "df",
    "tbl" = "df",
    "grouped_df" = "df",
    ""
  )
  ## Get the position of all capital letters
  capitals <<- unlist(gregexpr("[A-Z]", name))
  ## Check if variable has correct name
  if (replacer == substr(name, 1, nchar(replacer)) &
    str_detect(substr(name, nchar(replacer) + 1, nchar(replacer) + 1), "^[:upper:]+$")) {
    return(name)
    ## If there is no prefix or it is incorrect. Capitalize and add correct prefix
  } else {
    name <-
      paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))
    return(paste0(replacer, name))
  }
}
