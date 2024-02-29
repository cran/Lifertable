
#' @describeIn lifertable
#'     Print a \code{lifertable} object
#'
#' @param x Object to be displayed.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @export
#'
print.lifertable <- function(x, ...) {
  print(x$LIFERTABLE)
  print(x$PARAMETERS)
  if (!is.null(x$TOTAL.EGGS))
    print(x$TOTAL.EGGS)
  if(!is.null(x$CI))
    print(x$CI)
  if (!is.null(x$T.TEST))
    print(x$T.TEST)

  cat(" --------------------\n\n")
  invisible(x)
}
