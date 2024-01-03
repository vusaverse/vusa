#' Join Tkoppel
#'
#' A function to extract the program name (and possibly the faculty) from T Koppel_Z08
#' to the data frame.
#' To perform this function it is necessary that in the output folder:
#' file states: "1. Read data/INS_T couple_Z08.rds"
#'
#' @param df A data frame containing the numeric column INS_Programme_code_actual,
#' and to which the training name 2002 must be added.
#' The program name 2002 will be filled in this position by program names from the program name Z08
#' column, because these names are the most recent. However, the column will be added under Education Name 2002
#' because this is the most commonly used column for education names.
#' @param faculteit If FALSE (default) the column INS_Faculty is not added.
#' If TRUE, this column will be added.
#' @importFrom dplyr %>%
#' @export
join_tkoppel <- function(df, faculteit = F) {
  INS_Opleidingsnaam_2002 <- INS_Opleidingsnaam_Z08 <- INS_Opleidingscode_Z08 <-
    INS_Faculteit <- NULL

  ## Does the INS_Opleidingscode_actueel column exist, and is it numeric?

  if (!any(names(df) %in% "INS_Opleidingscode_actueel")) {
    stop("The column INS_Opleidingscode_actueel does not exist in the data frame")
  }

  if (!is.numeric(df$INS_Opleidingscode_actueel)) {
    stop("The column INS_Opleidingscode_actueel is not numeric")
  }

  ## Does the column INS_Programme_name_2002 or INS_Faculty exist?

  if (any(names(df) %in% "INS_Opleidingsnaam_2002")) {
    stop("The column INS_Opleidingsnaam_20022 already exists in the data frame")
  }

  if (faculteit == T) {

    if (any(names(df) %in% "INS_Faculteit")) {
      stop("The column INS_Faculteit already exists in the data frame")
    }

  }

  ## Read in the T couple isat table, to add the training name
  Tkoppel_Z08 <- vvmover::readrds_csv(output = "2. Geprepareerde data/INS_Tkoppel_Z08.rds")

  ## Add education name to df: link df with Tkoppel_isat and remove factorial
  df <- df %>%
    strict_left_join(Tkoppel_Z08, by = "INS_Opleidingscode_actueel") %>%
    dplyr::select(-INS_Opleidingsnaam_2002) %>%
    dplyr::rename(INS_Opleidingsnaam_2002 = INS_Opleidingsnaam_Z08) %>%
    dplyr::select(-INS_Opleidingscode_Z08)

  if (faculteit == F) {
    df <- df %>%
      dplyr::select(-INS_Faculteit)
  }

  return(df)

}
