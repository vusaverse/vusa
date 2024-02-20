#' @title Use RStudio Preferences Without Confirmation
#' @description A wrapper for \code{rstudio.prefs::use_rstudio_prefs} that updates RStudio preferences without prompting for confirmation.
#' @param ... A series of RStudio preferences to update, e.g. \code{always_save_history = FALSE}, \code{rainbow_parentheses = TRUE}.
#' @return NULL, updates RStudio \code{rstudio-prefs.json} file.
#' @details This function is designed to be used in startup scripts or automated processes where user confirmation is not desired. It bypasses the standard confirmation prompt by applying the preferences directly.
#' @note This function is a wrapper for \code{rstudio.prefs::use_rstudio_prefs} and is intended for use in environments where user interaction is not possible or desired.
#' @seealso \code{\link[rstudio.prefs]{use_rstudio_prefs}} for the original function that includes user confirmation.
#' @export
use_rstudio_prefs_silent <- function(...) {
  # Save lists of existing and updated prefs
  list_updated_prefs <- rlang::dots_list(...)
  if (!rlang::is_named(list_updated_prefs)) {
    rlang::abort("Each argument must be named.")
  }

  list_current_prefs <-
    names(list_updated_prefs) %>%
    purrr::map(~ rstudioapi::readRStudioPreference(.x, default = NULL)) %>%
    stats::setNames(names(list_updated_prefs)) %>%
    purrr::compact()

  # Print updates that will be made
  any_update <- pretty_print_updates(list_current_prefs, list_updated_prefs)
  # If no updates, abort function execution
  if (!any_update) {
    return(invisible(NULL))
  }

  # Update prefs without user interaction
  list_updated_prefs %>%
    purrr::iwalk(~ rstudioapi::writeRStudioPreference(name = .y, value = .x))
  return(invisible(list_updated_prefs))
}

#' @title Pretty Print RStudio Preference Updates
#' @description A function that creates a formatted output of RStudio preference updates, comparing old and new values.
#' @param old A list of old RStudio preferences.
#' @param new A list of new RStudio preferences.
#' @return A logical value indicating if there were any updates.
#' @details This function generates a data frame with old and new preferences, pads each column with trailing spaces for alignment, and prints updates. It also returns a logical value indicating if there were any updates.
#' @note This function is useful for comparing and documenting changes in RStudio preferences.
#' @export
pretty_print_updates <- function(old, new) {
  # create data frame with old and new preferences -----------------------------
  df_updates <-
    # data frame of old prefs
    tibble::tibble(
      pref =
        names(old) %>%
          intersect(names(new)) %||%
          character(0), # if no overlap with old and new, drop in a placeholder,
      old_value =
        old[names(old) %>% intersect(names(new))] %>%
          unname() %>% lapply(as.character) %>% unlist() %||%
          character(0) # if no overlap with old and new, drop in a placeholder
    ) %>%
    dplyr::full_join(
      # data frame of new prefs
      tibble::tibble(
        pref = names(new),
        new_value =
          new %>%
            unname() %>%
            lapply(function(x) ifelse(is.null(x), "*", as.character(x))) %>%
            unlist()
      ),
      by = "pref"
    ) %>%
    dplyr::mutate(
      old_value = ifelse(is.na(.data$old_value), "*", .data$old_value),
      new_value = ifelse(is.na(.data$new_value), "*", .data$new_value),
      updated = .data$old_value != .data$new_value
    )

  # pad each column with trailing spaces ---------------------------------------
  length_total <- df_updates %>% lapply(function(x) nchar(x) %>% max())
  length_total[["pref"]] <- length_total[["pref"]] + 3
  length_total[["old_value"]] <- length_total[["old_value"]] + 1
  for (i in seq_len(nrow(df_updates))) {
    for (col in setdiff(names(df_updates), "updated")) {
      df_updates[i, col] <-
        paste0(
          df_updates[i, col],
          rep_len(" ", length_total[[col]] - nchar(df_updates[i, col])) %>%
            paste(collapse = "")
        )
    }
  }

  # print updates --------------------------------------------------------------
  if (sum(!df_updates$updated) > 0L) {
    cat(cli::rule("No Changes", line = 2), "\n")
    df_updates %>%
      dplyr::filter(!.data$updated) %>%
      dplyr::mutate(message = paste0(
        "- ",
        .data$pref, "[",
        .data$old_value, " --> ",
        .data$new_value,
        "]"
      )) %>%
      dplyr::pull(.data$message) %>%
      paste(collapse = "\n") %>%
      cat()
    cat("\n\n")
  }

  if (sum(df_updates$updated) > 0L) {
    cat(cli::rule("Updates", line = 2), "\n")
    df_updates %>%
      dplyr::filter(.data$updated) %>%
      dplyr::mutate(message = paste0(
        "- ",
        .data$pref, "[",
        .data$old_value, " --> ",
        .data$new_value,
        "]"
      )) %>%
      dplyr::pull(.data$message) %>%
      paste(collapse = "\n") %>%
      cat()
    cat("\n\n")
  }

  # return a logical indicating if there were any updates
  return(sum(df_updates$updated) > 0L)
}
