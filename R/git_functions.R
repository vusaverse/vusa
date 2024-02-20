#' Retrieve the last n commit dates of a file
#'
#' @param filepath Path to file
#' @param n Integer number to look back in the git log
#' @export
get_last_n_commit_date <- function(filepath, n = 1) {
  return(shell(paste0(
    "git log -", n, ' --format=%cd --date=format:"', "%Y-%m-%d",
    '"  -- "', filepath, '"'
  ), intern = T))
}

#' Current git branch
#'
#' Determine the current git branch
#' @export
current_git_branch <- function() {
  ## Get a list of all available branches
  Branches <- system("git branch", intern = TRUE)
  ## The branch with the asterisk before the name is the current branch
  Current_Branch <- Branches[grepl("\\* ", Branches)]
  ## Remove the asterisks from the text
  Branch <- gsub("\\* ", "", Current_Branch)
  ## Return the branch name
  return(Branch)
}
