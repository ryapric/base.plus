#' Vectorized Near-Equality
#'
#' This function serves as a vectorized alternative to
#' [all.equal()][base::all.equal()].
#'
#' @param x,y Vectors of elements to compare.
#' @param tol Numeric tolerance for comparison. Default value should be
#'   sufficient for most.
#'
#' @export
is.equal <- function(x, y, tol = .Machine$double.eps) {
  abs(x - y) < tol
}



#' Symmetric Set Difference
#'
#' Like [setdiff()][base::setdiff()], but symmetric! Not a new idea, but at
#' least exported here.
#'
#' @param x,y Vectors to check for symmetric set difference.
#'
#' @export
symdiff <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}
