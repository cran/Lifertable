
#' @describeIn lifertable
#'     Print a \code{lifertableParmEst} object, this is the object showing the Estimated Parameters
#'
#' @export
#'
print.lifertableParmEst <- function(x, ...) {
  cat("\n ----- PARAMETER ESTIMATIONS -----\n\n")
  if ("Ro" %in% names(x)) {
    print(as.data.frame(x), row.names = FALSE)
    cat("\n\n")
  } else {
    for (i in seq_along(x) ) {
      cat(" - GROUP :", names(x)[i], "\n\n")
      print(as.data.frame(x[[i]]), row.names = FALSE)
      cat("\n\n")
    }
  }
  cat(" -------------------------\n\n")
  invisible(x)
}
