#' Retrieve the last n commit dates of a file
#'
#' @param filepath Path to file
#' @param n Integer number to look back in the git log
#' @export
get_last_n_commit_date <- function(filepath, n = 1) {
  return(shell(paste0('git log -', n, ' --format=%cd --date=format:"', "%Y-%m-%d",
                      '"  -- "', filepath, '"'), intern = T))
}


#' Retrieve the last n commits of a file
#'
#' @param filepath Path to file
#' @param type The type of commit, according to commit style guide
#' @param n Integer number to look back in the git log
#' @examples
#' \dontrun{
#' # get the last three commits which were bug fixes
#' get_last_n_commit_info("01. Inlezen/Inlezen Aanmeldingen.R", type = "fix", n = 3)
#' }
#' @export
get_last_n_commit_info <- function(filepath, type = "", n = 1) {
  if (type == ""){
    return(shell(paste0('git log --follow -', n, ' "', filepath, '"')))
  } else {
    return(shell(paste0('git log --follow --grep=', type, ' -n', n, ' "', filepath, '"')))
  }}