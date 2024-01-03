#'SA Branch set
#'
#'Sets a global option determining the current branch
#'
#'@param branch String object with name of the branch
#'
#'@export
sa_branch_set <- function(branch) {
  options(
    list(
      VUStudentAnalytics.branch = branch))
}

#'SA Branch get
#'
#'Retrieves the global option determining the current branch
#'
#'@export
sa_branch_get <- function() {
  getOption("VUStudentAnalytics.branch")
}


#'Current git branch
#'
#'Determine the current git branch
#'@export
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