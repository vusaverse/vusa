#' hash var
#'
#' Hashes a vector.
#'
#' @param Vector_to_hash The vector to be hashed.
#' @param seed A given seed to hash the vectors, default is NULL
#'
#' @return return_var (=hashed vector)
#' @export
hash_var <- function(Vector_to_hash, seed = NULL) {
  ## Check if system variable exists, when argument for seed is NULL.
  if (is.null(seed)) {
    message("Seed is missing, so checking for existing system variable for seed")
    if (!Sys.getenv("LOCAL_SEED") == "") {
      message("Neccesary system variable is present , this will be used for seed ")
      seed <- getOption(Sys.getenv("LOCAL_SEED"))
    } else {
      stop("system variable for LOCAL_SEED")
    }
  }

  ## Set seed, to be able to reproduce
  set.seed(seed)

  ## Step 1 - anonymize with seed (This happened first in anonymizer)
  ## Taking unique values and then hashing is faster than old solution.
  ## TODO: combine this with salts
  dfHash <- tibble::tibble(to_hash = unique(Vector_to_hash))
  dfHash$var_seed <- purrr::map_chr(dfHash$to_hash, digest::digest,
    algo = "sha256", seed = 0
  )
  dfInput <- tibble::tibble(to_hash = Vector_to_hash) %>%
    dplyr::left_join(dfHash)
  var_seed <- dfInput$var_seed
  unique_var_seed <- unique(var_seed)

  ## Step 2 - add salt
  ## Create a unique vector
  ## Create a salt vector based on this new vector
  ## Then link back to the original vector with a lookup and join function

  unique_var_seed <- unique(var_seed)

  ## Create a random string of 5 lowercase characters with the same length of the object
  set.seed(seed)
  salt_vector <- random_string_vector(length(unique_var_seed), 5)
  unique_var_salt <- paste0(salt_vector, unique_var_seed)

  ## Create a lookup table from these two vectors
  lookup <- data.frame(unique_var_seed, unique_var_salt)
  names(lookup) <- c("unique_var_seed", "unique_var_salt")

  ## Create a dataframe base on the input and join (dplyr)
  input <- data.frame(var_seed)
  names(input) <- c("var_seed")
  input <- dplyr::left_join(input, lookup, by = c("var_seed" = "unique_var_seed"))

  ## Create return var
  return_var <- input$unique_var_salt

  return(return_var)
}

#' Generate a random string vector
#'
#' @param n The number of items in the vector
#' @param length the number of characters in a string
#' @param characters A vector containing the characters to include
random_string_vector <- function(n = 500, length, characters = c(letters[1:6], 0:9)) {
  ## Generate a random vector with strings of 20 characters
  return(do.call(
    paste0,
    replicate(
      length,
      ## Random letters and numbers are drawn
      sample(
        characters,
        n,
        TRUE
      ),
      FALSE
    )
  ))
}
