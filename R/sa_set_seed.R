#' Set the seed for the Student Analytics project.
#'
#' test
#'
#' @param seed The seed.
#'
#' @export
sa_set_seed <- function(seed) {
  options(
    list(
      VUStudentAnalytics.seed = seed
    )
  )
}
