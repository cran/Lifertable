
#' @describeIn lifertable
#'     Print a \code{lifertableTest} object, this is the object showing the Student t - test
#'
#' @export
#'
print.lifertableTest <- function(x, ...){
  cat("\n ----- STUDENT T - TEST -----\n\n")
  for (i in seq_along(x) ) {
    cat(" - ", names(x)[i], "\n\n")
    print(x[[i]], row.names = FALSE, ...)
    cat("\n\n")
  }
  cat(" -------------------------\n\n")
  invisible(x)
}
