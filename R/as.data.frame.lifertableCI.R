
#' Coerce CI to a Data Frame
#'
#' Function to coerce the object displaying the Confidence Interval into a data frame.
#'
#' @param x A lifertableCI object.
#' @inheritParams base::as.data.frame
#'
#' @return \code{as.data.frame.lifertableCI} returns a data frame.
#' @export
#'
#' @examples
#' ## Make the main object:
#' lft <- lifertable(Female, Age, Eggs, Sexrate,
#'                   ColumnGroups = Group,
#'                   data = Insects, jackknife = TRUE)
#'
#'
#' as.data.frame(lft$CI)
#'
as.data.frame.lifertableCI <- function (x, row.names = NULL, ...) {
  if(is.list(x)) {
    if ("Ro" %in% names(x)) {
      for (i in seq_along(x)){
        x[[i]] <- data.frame(Parameter = names(x)[i],
                   as.data.frame(x[[i]]))
      }
      x <- do.call(rbind, x)
      row.names(x) <- row.names
      return(x)
    } else {
      dafr <- data.frame(Group = names(x),
                 do.call(rbind, x),
                 check.names = FALSE,
                 row.names = NULL)
      row.names(dafr) <- row.names
      return(dafr)
    }
  } else { data.frame(t(unclass(x))) }
}
