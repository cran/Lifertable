
#' Compute the confidence interval
#'
#' Function to compute the confidence interval from a numeric vector.
#'
#' @param x A numeric vector
#'
#' @return
#' \code{intconf} returns an object of class "lifertableCIJackknife".
#' An object of class "lifertableCIJackknife" is a list containing the following components:
#'
#' \item{Lower Limit}{ Lower limit of the confidence interval.}
#' \item{Mean}{ Vector average.}
#' \item{Upper Limit}{ Upper limit of the confidence interval.}
#' \item{Var}{ Vector variance.}
#'
#' @noRd
#'
intconfJK <-  function (x) {
  if (length(unique(x)) == 1) {
    ci <- c(
      "Lower Limit" = 0,
      Mean = mean(x),
      "Upper Limit" = 0,
      Var = stats::var(x)
    )
  } else {
    a <- stats::t.test(x)
    ic <- a$conf.int
    ci <- c("Lower Limit" = min(ic),
            Mean = mean(x),
            "Upper Limit" = max(ic),
            Var = stats::var(x))
  }
  class(ci) <- "lifertableCIJackknife"
  return(ci)
}
