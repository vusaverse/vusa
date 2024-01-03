#' check name
#'
#' This function checks if a variable name is correct following the style guide.
#'
#' @param string The variable name. Can also be a list of names.
#'
#' @return The result of the check. This can be: "Correct","spaces in name",
#' "Wrong characters in name","No 3-letter prefix",
#' "Word after prefix has no capital letter"
#' and "Words after first word have capital letters"
#' @export
check_name <- function(string){
  # make a "boolean" for every name check
  correct_name <- rep("correct", length(string))

  # Check the input
  for(i in string){
    if(is.null(i) || identical(i, character(0)) || !is.character(i)) {
      correct_name[i] <- "incorrect string"
    }
  }
  for (name in seq(length(correct_name))){
    # check for special characters in name
    # a character is 'special' if it is not an underscore/letter/number
    if(grepl("\\s|[[:punct:]]", gsub("_","",string[name]))){
      if (correct_name[name] == "correct" ){
        correct_name[name] <- "Name has special characters or spaces."
      }else{
        # if a earlier check failed, give the new status
        correct_name[name] <-
          paste0(correct_name[name],
                 ", Name has special characters or spaces.")
      }
    }
    # Split name
    name_parts <- unlist(strsplit(string[name], split = "_"))
    # pas through all name parts
    for(index in seq(length(name_parts))){
      # Specify if a part of a name is abbreviation.
      # If the amount of capital letters in the word is as much as half the word
      # or the same amount as the word, than it is an abbreviation:
      abbriviation <- length(
        unlist( gregexpr( "[A-Z]",
                          name_parts[index]))) >= ceiling(nchar(
                            name_parts[index]) /2)
      # If the word contains VU (like VUdata) than also an abbreviation
      if(grepl("VU", name_parts[index])){
        abbriviation <- TRUE}

      # Check every variable if it has no 3-letter prefix
      if( index == 1 & nchar(name_parts[index]) != 3){
        # If this is the first failed check, than assign the new status.
        if ( correct_name[name] == "correct" ) {
          correct_name[name] <- "Name has no 3-letter prefix"
        }else{
          # if a earlier check failed, give the new status
          correct_name[name] <-
            paste0( correct_name[name],
                    ", Name has no 3-letter prefix")
        }
      }
      # check for only capital letters in prefix
      if( index == 1 & nchar(name_parts[index]) == 3 & stringr::str_detect(name_parts[index],"[[:lower:]]") == TRUE){
        # If this is the first failed check, than assign the new status.
        if ( correct_name[name] == "correct" ) {
          correct_name[name] <- "Prefix has not excactly 3 capital letters."
        }else{
          # if a earlier check failed, give the new status
          correct_name[name] <-
            paste0( correct_name[name],
                    ", Prefix has not excactly 3 capital letters.")
        }
      }
      # check if the word after the prefix starts with a capital letter
      if(index == 2 & abbriviation == FALSE){
        if(unlist( gregexpr("[A-Z]", name_parts[index]))[1] != 1){
          if ( correct_name[name] == "correct" ){
            correct_name[name] <- "Word after prefix does not start with a capital letter."
          }else{
            correct_name[name] <-
              paste0(correct_name[name],
                     ", Word after prefix does not start with a capital letter.")
          }
        }
      }
      # If the word after the previx is an abbreviation,
      # than the first word after has to start with a capital letter.
      if(index == 2 & abbriviation == TRUE){
        # first check if the word exists
        if (is.na(unlist(gregexpr("[A-Z]", name_parts[index + 1]))[1]) |
            (unlist(gregexpr("[A-Z]", name_parts[index + 1]))[1])  != 1) {
          if ( correct_name[name] == "correct" ){
            correct_name[name] <- "Word after abbriviation does not start with a capital letter."
          }else{
            correct_name[name] <-
              paste0(correct_name[name],
                     ", Word after abbriviation does not start with a capital letter.")
          }
        }
      }
      # Check if the words after the first word only exist of small letters
      if(index > 2 && abbriviation == FALSE){
        if(stringr::str_detect(name_parts[index],"[[:upper:]]") == TRUE){
          if (correct_name[name] == "correct" ){
            correct_name[name] <- "Words after first word (not an abbriviation) contain capital letters."
          }else{
            correct_name[name] <-
              paste0(correct_name[name],
                     ", Words after first word (not an abbriviation) contain capital letters.")
          }
        }
      }
    }
  }
  return(correct_name)
}
