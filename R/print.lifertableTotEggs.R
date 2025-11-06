
#' @describeIn lifertable
#'     Print a \code{lifertableTotEggs} object, this is the object showing the Eggs laid per Female
#'
#' @export
#'
print.lifertableTotEggs <- function(x, ...) {
  cat("\n ----- EGGS LAID PER FEMALE -----\n\n")
  if ("Total Eggs" %in% names(x)) {
    print(as.data.frame( unclass(x) , check.names = FALSE),
          row.names = FALSE, ...)

    cat("\n     MEAN : ", mean(x[[2]], na.rm=TRUE) ) #Nuevo 1.0.1
    cat("\n\n")


  } else {
    for (i in seq_along(x) ) {
      cat(" - GROUP :", names(x)[i], "\n\n")
      print(as.data.frame(unclass(x[[i]]), check.names = FALSE ),
            row.names = FALSE, ...)

      cat("\n     MEAN : ", mean(x[[i]][[2]], na.rm=TRUE) ) #Nuevo 1.0.1
      cat("\n\n")

    }
  }
  cat(" -------------------------\n\n")
  invisible(x)
}
