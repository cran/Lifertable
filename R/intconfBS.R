
#' Compute the confidence interval
#'
#' Function to compute the confidence interval from a numeric vector. To Bootstrap sample.
#'
#' @param x A numeric vector
#'
#' @return
#' \code{intconf} returns an object of class "lifertableCIBootstrap".
#' An object of class "lifertableCIBootstrap" is a list containing the following components:
#'
#' \item{Lower Limit}{ Lower limit of the confidence interval.}
#' \item{Mean}{ Vector average.}
#' \item{Upper Limit}{ Upper limit of the confidence interval.}
#' \item{Var}{ Vector variance.}
#'
#' @noRd
#'
intconfBS <-  function (x) {
  if (length(unique(x)) == 1) {
    ci <- c(
      "Lower Limit" = 0,
      Mean = mean(x),
      "Upper Limit" = 0,
      Var = stats::var(x)
    )
  } else {
    ci <- c(
      "Lower Limit" = stats::quantile(x, 0.025)[[1]],
      Mean = mean(x),
      "Upper Limit" = stats::quantile(x, 0.975)[[1]],
      Var = stats::var(x)
    )
  }
  class(ci) <- "lifertableCIBootstrap"
  return(ci)
}
