
#' @describeIn lifertable
#'     Print a \code{lifertableLFT} object, this is the object showing the Life and Fertility Table
#'
#' @export
#'
print.lifertableLFT <- function(x, ...) {
  cat("\n ----- LIFE AND FERTILITY TABLE -----\n\n")
  if ("AGE" %in% names(x)) {
    print(as.data.frame(x), ...)
    cat("\n\n")
  } else {
    for (i in seq_along(x) ) {
      cat(" - GROUP :", names(x)[i], "\n\n")
      print(as.data.frame(x[[i]]), ...)
      cat("\n\n")
    }
  }
  cat(" -------------------------\n\n")
  invisible(x)
}
