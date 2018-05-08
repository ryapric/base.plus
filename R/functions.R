#' Vectorized Near-Equality
#'
#' This function serves as a vectorized alternative to [base::all.equal()].
#'
#' @export
is.equal <- function(x, y, tol = .Machine$double.eps) {
  abs(x - y) < tol
}



#' Symmetric Set Difference
#'
#' Like [base::setdiff()], but symmetric! Not a new idea, but at least exported
#' here.
#'
#' @export
symdiff <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}
