
#' Coerce Parameters to a Data Frame
#'
#' Function to coerce the object displaying Parameters into a data frame.
#'
#' @param x A lifertableParmEst object
#' @inheritParams base::as.data.frame
#'
#' @return \code{as.data.frame.lifertableParmEst} returns a data frame.
#' @export
#'
#' @examples
#' ## Make the main object:
#' lft <- lifertable(ColumnFemale = Female,
#'                   ColumnAge = Age,
#'                   ColumnEggs = Eggs,
#'                   SexRate = Sexrate,
#'                   ColumnGroups = Group,
#'                   data = Insects)
#'
#'
#' as.data.frame(lft$PARAMETERS)
#'
as.data.frame.lifertableParmEst <- function (x, row.names = NULL, ...) {
  if ("Ro" %in% names(x)){
    data.frame(Ro = x$Ro,
               Rm = x$Rm,
               GT = x$GT,
               DT = x$DT,
               Lambda = x$Lambda, row.names = row.names, ...)
  } else {
    for (i in seq_along(x) ) {
      x[[ i ]] <- data.frame(Group = names(x)[i],
                             as.data.frame(x[[i]]), ...)
    }
    x <- do.call(rbind, x)
    row.names(x) <- row.names
    return(x)
  }
}
