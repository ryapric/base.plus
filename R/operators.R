#' Which elements of X are "not in" Y?
#'
#' @param x,y Vectors to compare. Order matters, just like when using
#'   [\%in\%][base::match()]; for a symmetric version of
#'   [setdiff()][base::setdiff()], see [symdiff()] in this package.
#'
#' @export
`%not.in%` <- function(x, y) {
  !(x %in% y)
}
