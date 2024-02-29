
#' Coerce Life Table to a Data Frame
#'
#' Function to coerce the object displaying Life Table into a data frame.
#'
#' @param x A lifertableLFT object
#' @inheritParams base::as.data.frame
#'
#' @return \code{as.data.frame.lifertableLFT} returns a data frame.
#' @export
#'
#' @examples
#' ## Make the main object:
#' lft <- lifertable(Female, Age, Eggs, Sexrate,
#'                   ColumnGroups = Group, data = Insects)
#'
#'
#' as.data.frame(lft$LIFERTABLE)
#'
as.data.frame.lifertableLFT <- function (x, row.names = NULL, ...) {

  if ("AGE" %in% names(x)){
    data.frame(AGE = x$AGE,
               FEMALES = x$FEMALES,
               NEGG = x$NEGG,
               LX = x$LX,
               MX = x$MX,
               LXMX = x$LXMX,
               XLXMX = x$XLXMX,
               row.names = row.names)
  } else {
    for (i in seq_along(x) ) {
      x[[ i ]] <- as.data.frame(unclass(x[[ i ]]),
                                check.names = FALSE )
      x[[ i ]]$GROUPS <- names(x)[i]
    }
    x <- do.call(rbind, x)
    row.names(x) <- row.names
    return(x)
  }
}
