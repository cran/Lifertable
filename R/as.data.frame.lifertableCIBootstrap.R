
#' Coerce CI to a Data Frame
#'
#' Function to coerce the object displaying the Confidence Interval into a data frame.
#'
#' @param x A lifertableCIBootstrap object.
#' @inheritParams base::as.data.frame
#'
#' @return \code{as.data.frame.lifertableCIBootstrap} returns a data frame.
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
#'                   CI = TRUE,
#'                   technique = "bootstrap", reSamples = 100)
#'
#'
#' as.data.frame(lft$CI)
#'
as.data.frame.lifertableCIBootstrap <- function (x, row.names = NULL, ...) {
  if(is.list(x)) {
    if ("Ro" %in% names(x)) {
      for (i in seq_along(x)){
        x[[i]] <- data.frame(Parameter = names(x)[i],
                   as.data.frame(x[[i]]), ...)
      }
      x <- do.call(rbind, x)
      row.names(x) <- row.names
      return(x)
    } else {
      dafr <- data.frame(Group = names(x),
                 do.call(rbind, x),
                 check.names = FALSE,
                 row.names = NULL, ...)
      row.names(dafr) <- row.names
      return(dafr)
    }
  } else { data.frame(t(unclass(x)), ...) }
}
