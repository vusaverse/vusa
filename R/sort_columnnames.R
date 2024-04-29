


#' Sort column names analysis set
#'
#' Function to sort analysis set column names by category and alphabetically.
#' The order of the categories is as follows:
#' 1. Keys to pair with
#' 2. Registration fields
#' 3. Pre-study data
#' 4. Data from during the study
#' 5. Alumni details
#'
#' @param data The data frame.
#'
#' @return New data frame with column names sorted.
#' @export
sort_columnnames_analysis_set <- function(data) {
  value <- prioriteit <- kolomnaam <- NULL
  ## Create table with ordering of the column names
  Kolomnamen <- dplyr::as_tibble(names(data)) %>%
    dplyr::rename(kolomnaam = value) %>%
    ## Add the category
    ## 1. Keys to join on
    dplyr::mutate(
      prioriteit = dplyr::case_when(
        kolomnaam %in%
          c(
            "INS_Studentnummer",
            "INS_Opleidingsnaam_2002",
            "INS_Inschrijvingsjaar",
            "INS_Inschrijvingsjaar_EOI",
            "INS_Studiejaar",
            "INS_Opleidingsfase_BPM"
          ) ~ "Keys",
        ## 2. Inschrijvingsvelden
        substr(kolomnaam, 1, 3) == "INS" ~ "Inschrijving",
        ## 3. Voor aanvang van de studie
        substr(kolomnaam, 1, 3) %in% c(
          "VOP",
          "MAC",
          "MVR",
          "ORI",
          "INT"
        ) ~ "Voor aanvang",
        ## 5. Alumni
        substr(kolomnaam, 1, 3) == "ALU" ~ "Alumnus",
        ## 4. de rest is tijdens de studie
        TRUE ~ "Tijdens studie"
      ),
      ## Maak kolom prioriteit een factor en sorteer deze
      prioriteit = factor(prioriteit, levels = c(
        "Keys",
        "Inschrijving",
        "Voor aanvang",
        "Tijdens studie",
        "Alumnus"
      ))
    ) %>%
    ## Sort the list
    dplyr::arrange(prioriteit, kolomnaam)

  ## Sort the column names of the analysis set
  data <- data %>% dplyr::select(!!Kolomnamen$kolomnaam)

  return(data)
}
