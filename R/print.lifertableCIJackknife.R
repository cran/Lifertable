
#' @describeIn lifertable
#'     Print a \code{lifertableCIJackknife} object, this is the object showing the Confidence Interval obtained with Jackknife.
#'
#' @param title If \code{TRUE} (the default), displays the title of the object (this is for internal use only).
#'
#' @export
#'
print.lifertableCIJackknife <- function(x, title = TRUE, ...) {
  if (title) {
    cat("\n   -----  CONFIDENCE INTERVALS  -----\n")
    cat("------ USING JACKKNIFE ESTIMATION ------\n\n")
  }
  if ("Lower Limit" %in% names(x)){
    print(unclass(x), ...)
    cat("\n\n")
  } else if ("Ro" %in% names(x)) {
    for (i in seq_along(x) ) {
      cat(" - PARAMETER :", names(x)[i], "\n\n")
      print(as.data.frame(x[[i]]), row.names = FALSE, title = FALSE, ...)
      cat("\n")
    }
  } else {
    print(as.data.frame(x), row.names = FALSE, title = FALSE, ...)
    cat("\n")
  }
  if (title)
    cat(" -------------------------\n\n")
  invisible(x)
}
