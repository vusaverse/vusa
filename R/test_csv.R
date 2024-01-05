#' Test csv
#'
#' Vergelijkt de kolomnamen tussen read_csv2 en read.csv2. Deze functie is gemaakt om
#' te inventariseren wat de impact is van het gebruiken van de readr package om csv's in
#' te lezen.
#'
#' @param dataloc De locatie waar het bestand naar weggeschreven wordt
#'
#' @return Een dataframe dat de verschillen tussen de kolomnamen en classes van de twee
#' inleesmethode weergeeft
#' @export
test_csv <- function(dataloc = ""){
    df1 <- readr::read_csv2(paste(sa_network_dir_get(), dataloc, sep = ""))  %>%
        dplyr::mutate_if(is.character, factor) ## readr

    df2 <- utils::read.csv2(paste(sa_network_dir_get(), dataloc, sep = "")) ## read.csv2

    # maak een df van read_csv2
    class_df1               <- data.frame(unlist(lapply(df1, class)))
    class_df1               <- tibble::rownames_to_column(class_df1)
    colnames(class_df1)     <- c("var", "class_readr")
    class_df1$var_readr <- class_df1$var

    # Maak een df van read.csv2
    class_df2               <- data.frame(unlist(lapply(df2, class)))
    class_df2               <- tibble::rownames_to_column(class_df2)
    colnames(class_df2)     <- c("var", "class_read.csv2")
    class_df2$var_read.csv2 <- class_df2$var

    # Merge de lijsten
    Comparison            <- merge(class_df1, class_df2, all = TRUE)

    # Maak een df met de gelijke rijen
    Equal                  <- dplyr::filter(Comparison,
                                             (as.character(Comparison$var_readr) == as.character(Comparison$var_read.csv2)) &
                                                 (as.character(Comparison$class_readr) == as.character(Comparison$class_read.csv2)))
    # Maak een df met alle rijen die niet gelijk zijn
    Differences             <- Comparison[!Comparison$var %in% Equal$var,]

    # Geef dit df als output
    Differences
}
