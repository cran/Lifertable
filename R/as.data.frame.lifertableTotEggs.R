
#' Coerce Total Eggs to a Data Frame
#'
#' Function to coerce the object displaying Total Eggs into a data frame.
#'
#' @param x A lifertableTotEggs object.
#' @inheritParams base::as.data.frame
#'
#' @return \code{as.data.frame.lifertableTotEggs} returns a data frame.
#' @export
#'
#' @examples
#' ## Make the main object:
#' lft <- lifertable(ColumnFemale = Female,
#'                   ColumnAge = Age,
#'                   ColumnEggs = Eggs,
#'                   SexRate = Sexrate,
#'                   ColumnGroups = Group,
#'                   data = Insects,
#'                   TotalEggs = TRUE)
#'
#'
#' as.data.frame(lft$TOTAL.EGGS)
#'
as.data.frame.lifertableTotEggs <- function (x, row.names = NULL, ...) {

  if ("Total Eggs" %in% names(x)) {
    as.data.frame( unclass(x) , check.names = FALSE, row.names = row.names, ...)
  } else {
    for (i in seq_along(x) ) {
      x[[ i ]] <- as.data.frame(unclass(x[[ i ]]),
                           check.names = FALSE, ... )
      x[[ i ]]$GROUPS <- names(x)[i]
    }
    x <- do.call(rbind, x)
    row.names(x) <- row.names
    return(x)
  }
}
