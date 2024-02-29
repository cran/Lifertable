
#' Plot for the Pseudo-Values of Parameters
#'
#' Construction of Plots for the pseudo-values of associated Life Table parameters.
#'
#' @param object An object inheriting from \code{\link[=lifertable]{lifertable()}}, representing the
#'         Life and Fertility Table.
#'
#' @importFrom ggplot2 aes geom_point geom_boxplot facet_wrap labs theme element_text ggsave
#'
#' @return Returns an object of [`class`][base::class] c("gg", "ggplot").
#'
#' @export
#'
#' @examples
#' ## The main object will be created using the Insects database:
#' lft <- lifertable(Female, Age, Eggs, Sexrate, ColumnGroups = Group,
#'                   data = Insects, jackknife = TRUE)
#'
#' PlotPseudoVals(lft)
#'
#'
PlotPseudoVals <- function (object) {
  Parameter <- NULL #It is not used, its to avoid NOTE
  Value <- NULL #It is not used, its to avoid NOTE
  Group <- NULL #It is not used, its to avoid NOTE

  if (!methods::is(object, "lifertable"))
    stop("Please enter the variable resulting from the 'lifertable()' function.")

  if (is.null(object$PSEUDOS)) {
    stop("The plot cannot be generated because the 'PSEUDOS' component was not created when using the 'lifertable()' function.")

  } else {
    if ("Ro" %in% names(object$PSEUDOS)) {

      psvs <- tidyr::gather(as.data.frame(object$PSEUDOS), "Parameter", "Value", factor_key = TRUE)

      ggplot2::ggplot(psvs, mapping = aes(Parameter, Value) ) +
        geom_point(mapping = aes(color = Parameter), show.legend = FALSE) +
        facet_wrap(~ Parameter, scales = "free", ncol = 2) +
        labs(title = "PSEUDOVALUES OF PARAMETERS",
             x = "", y = "" ) +
        theme(plot.title = element_text(hjust = 0.5))

    } else {
      Pseudos <- list()
      for (i in seq_along(object$PSEUDOS)) {
        Pseudos[[i]] <- data.frame(Group = names(object$PSEUDOS[i]),
                                   as.data.frame(object$PSEUDOS[[i]] ))
      }
      Pseudos <- do.call(rbind, Pseudos)

      psvs <- tidyr::gather(Pseudos, "Parameter", "Value", c("Ro", "Rm", "GT", "DT", "Lambda"),
                            factor_key = TRUE)

      ggplot2::ggplot(psvs, mapping = aes(Group, Value) ) +
        geom_boxplot(mapping = aes(color = Group), show.legend = FALSE ) +
        facet_wrap(~ Parameter, scales = "free", ncol = 2) +
        labs(title = "PSEUDOVALUES OF PARAMETERS",
             x = "", y = "" ) +
        theme(plot.title = element_text(hjust = 0.5))
    }
  }
}
