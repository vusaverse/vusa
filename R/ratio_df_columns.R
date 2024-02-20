#' @title relatief_belangrijke_waardes_columns
#' @description Een ondersteunende functie voor package_belangrijk_in_project,
#' maar kan voor elke dataframe gebruikt worden. Met gebruik van deze functie
#' worden columns toegevoegd die aangeven hoe belangrijk een (tel) waarde is
#' binnen een selectie columns. De functie werkt als volgt:
#' 1) Specificeer de columns waar het om gaat als 'columns'
#' ---De functie maakt de column 'sum' aan met de opsomming van alle columns
#' ---Voor elke column uit 'columns' wordt een column aangemaakt met de originele
#' waarde uit de column gedeeld door de opsomming (column/sum)
#' => Dit getal geeft aan hoe uniek een waarde uit een row is:
#' 1.0 = 100% van de keren dat deze waarde voor komt is binnen deze column
#' 0.0 =  0% van de keren dat deze waarde voor komt is binnen deze column
#' @param df Dataframe
#' @param columns De columns uit het dataframe waarover de relatieve belangrijkheid
#' wordt berekent (en waar tevens voor elke column een extra column wordt toegevoegd)
#' @return Dataframe met toegevoegde columns. In deze columns staat een waarde
#' tussen 0.0 en 1.0.
#' 1.0 = 100% van de keren dat deze waarde voor komt is binnen deze column
#' 0.0 =  0% van de keren dat deze waarde voor komt is binnen deze column
#' @examples \dontrun{
#' dfTest <- ratio_df_columns(dfTest, c("column1", "column2"))
#' }
#' @importFrom dplyr mutate across
#' @importFrom rlang :=
#' @export
#'
ratio_df_columns <- function(df, columns = c(
                               "00. Downloaden", "01. Inlezen",
                               "02. Manipuleren", "03. Analyseset maken",
                               "04. Analyseren", "05. Rapporten"
                             )) {
  . <- NULL
  ## STAP 1: Som de waardes op uit de columns
  df <- df %>%
    dplyr::mutate(sum = rowSums(
      dplyr::across(columns)
    ))

  ## STAP 2: Bereken percentage aandeel binnen de columns
  # 1.0 = 100% van de keren dat deze waarde voor komt is binnen deze column
  # 0.0 =  0% van de keren dat deze waarde voor komt is binnen deze column
  for (column in columns) {
    nameNewColumn <- paste(column, "_aandeel", sep = "")
    df <- df %>%
      # Let op syntax zodat variabele gebruikt kan worden : '!!' en ':='
      # 'as.name(x)' zodat variabele aangeroepen kan worden, normaal in R
      # mag een variable namelijk geen spaties hebben of beginnen met een
      # cijfer (bij ons allebei het geval)
      dplyr::mutate(!!nameNewColumn := !!as.name(column) / sum)
  }

  ## STAP 3: Verwijder NA's en rond af
  df <- df %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), round, 2))

  ## STAP 4: Return df
  return(df)
}
