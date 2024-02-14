#' automatically update the documentation
#'
#' A function to automatically update the documentation.
#'
#' @export
auto_document <- function() {
  Veldnaam <- Categorie <- NULL
    ## 0. Read in analysis set
    analyseset <- get_analysisset()

    ## 0.1 Read in documentatie
    Documentatie <- documentation_get()

    ## 1.1 get all field names from documentatie----
    documentatie_names <- unique(Documentatie$Veldnaam)

    ## 1.2 get all names from analyseset----
    analyseset_names <- names(analyseset)

    ## 1.3 show columns in documentatie, but not in analyseset----
    doc_not_in_analyse <- setdiff(documentatie_names, analyseset_names)
    message(paste("Number or rows to delete:", length(doc_not_in_analyse)))

    ## 1.4 Show columns present in the analyseset, but not in documentatie
    analyse_not_in_doc <- setdiff(analyseset_names, documentatie_names)
    base::cat(cli::style_bold(cli::col_red("The following variables are not present in the documentation: \n")))
    base::cat(cli::col_red(paste(analyse_not_in_doc, "\n")))
    base::cat(paste("\n"))
    message(paste("aantal rijen om toe te voegen:", length(analyse_not_in_doc)))

    doc_2 <- subset(Documentatie, !(Veldnaam %in% doc_not_in_analyse))
    base::cat(cli::style_bold(cli::col_red("The following variables must be deleted from the documentation: \n")))
    base::cat(cli::col_red(paste(doc_not_in_analyse, "\n")))
    base::cat(paste("\n"))

    if (length(doc_not_in_analyse) > 0){
        Documentatie_remove(doc_not_in_analyse)
    }

    if (length(analyse_not_in_doc) > 0) {
        ## create dataframe
        df <- data.frame(Veldnaam = analyse_not_in_doc)

        voorkomende_waarden <- purrr::map_chr(analyse_not_in_doc,
                                       get_values,
                                       analyseset = analyseset)

        df <- data.frame(Veldnaam = analyse_not_in_doc,
                         Omschrijving = "",
                         Voorkomende_waarden = voorkomende_waarden,
                         Privacygevoelig = "Nee, tenzij ja. Zie APG --> Bijzonder persoonsgegevens ",
                         Oorspronkelijke_bron = "",
                         Extra_info = "")

        df <- df %>%
            dplyr::mutate(code = substr(Veldnaam, 1, 3))

        df <-
            mapping_translate(df,
                              "code",
                              "Categorie",
                              mapping_table_name = "Mapping_Doc_Cat.csv",
                              KeepOriginal = F)
        df <- df %>%
            dplyr::select(Categorie, dplyr::everything())



        save_name <- paste0("Documentatie_update_", Sys.Date())

        savecsv(df,
                Name_to_save = save_name,
                dataloc = "Documentatie/",
                output = "Documentatie")

        shell.exec(paste0(Sys.getenv("NETWORK_DIR"),
                          "Documentatie/",
                          save_name,
                          ".csv"))

        ask <- utils::menu(c("Yes", "No"),
                           title="Have you finished editing?")

        if (ask == 1) {
            Documentatie_add_csv(paste0("Documentatie/", save_name, ".csv"))
            return(paste("Rows to add to the documentation: ", analyse_not_in_doc))
        } else {return()}
    }

}
