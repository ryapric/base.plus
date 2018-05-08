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
is_equal <- function(x, y, tol = .Machine$double.eps) {
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

# Placeholder for pre-R-3.5 isFALSE



#' Find and Replace Text in an Rstudio Project Directory
#'
#' This function takes the current project directory (by default), and peforms a
#' project-wide, global find & replace in all files (via [gsub()][base::gsub()]);
#' this is similar to how one might use
#' [sed](https://www.gnu.org/software/sed/manual/sed.html), but in R syntax.
#' This function is designed to be only run interactively, and is strict in its
#' request for confirmation, to prevent any accidental replacement.
#'
#' @param pattern Pattern to search for. Pattern is taken as a regular
#'   expression (since it is passed to `gsub()`), so care needs to be taken when
#'   passing this argument, both in terms of what you _want_ to change vs. what
#'   you _don't_ want to change (e.g. `find_replace("ya.da", "yadda", "yabda")`
#'   will return "yadda").
#' @param replacement Replacement for `pattern`.
#' @param proj_dir Project directory to search. By default, calls [here::here()]
#'   to guess.
#' @param ... Other arguments passed on to [gsub][base::gsub()].
#'
#' @seealso [gsub][base::gsub()]
#'
#' @export
find_replace <- function(pattern, replacement, proj_dir = here::here(), ...) {

  proj_files <- dir(proj_dir, recursive = TRUE, full.names = TRUE)


  # Need this for testthat to run
  if (testthat::is_testing())
    confirm <- "yes, please"
  else
    confirm <- readline(prompt = sprintf(paste0("\n",
                                                "Replace:       '%s'\n",
                                                "With:          '%s'\n",
                                                "In directory:  '%s'\n\n",
                                                "Ok? ('yes, please'/'no')\n"),
                                         pattern, replacement, proj_dir))

  # Strict confirmation
  if (!(confirm %in% c("yes, please", "no"))) {
    stop("Please type 'yes, please' or 'no' exactly to confirm. Aborting")
  } else if (confirm == "yes, please") {
    for (f in proj_files) {
      oldfile <- readLines(f)
      newfile <- gsub(pattern, replacement, oldfile,
                      ignore.case = ignore.case,
                      perl = perl,
                      fixed = fixed,
                      useBytes = useBytes)
      writeLines(newfile, con = f, sep = "\n")
    }
  } else if (confirm == "no") {
    base::message("Aborting.")
  }

}
