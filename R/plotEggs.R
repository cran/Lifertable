
#' Plot for the Number of Eggs Laid per Female
#'
#' This function generates a Plot representing the number of Eggs Laid by
#' each Female throughout the entire experiment.
#'
#' @param object \code{object} accepts 2 classes of objects:
#'   \itemize{
#'     \item{An object inheriting from \code{\link[=lifertable]{lifertable()}},
#'         representing the Life and Fertility Table.}
#'     \item{An object of class \code{lifertableTotEggs} inherited from
#'         \code{\link[=lifertable]{lifertable()}}. This is the object that
#'         displays the total number of eggs laid per female.}
#'   }
#'
#' @importFrom ggplot2 aes geom_point geom_boxplot labs theme element_text ggsave
#'
#' @return Returns an object of [`class`][base::class] c("gg", "ggplot").
#'
#' @export
#'
#' @examples
#' ## The main object will be created using the Insects database:
#' lft <- lifertable(ColumnFemale = Female,
#'                   ColumnAge = Age,
#'                   ColumnEggs = Eggs,
#'                   SexRate = Sexrate,
#'                   ColumnGroups = Group,
#'                   data = Insects,
#'                   TotalEggs = TRUE)
#'
#' ## Possible usage scenarios
#'
#' ## 1. Direct Usage of the "lft" Object:
#' plotEggs(lft)
#'
#' ## 2. Assigning the component "TOTAL.EGGS" to an object:
#' TEggs <- lft$TOTAL.EGGS
#' plotEggs(TEggs)
#'
#' ## 3. Direct usage of the component:
#' plotEggs(lft$TOTAL.EGGS)
#'
#'
plotEggs <- function (object) {

  if ( methods::is(object, "lifertable") || methods::is(object, "lifertableTotEggs") ) {

    if ( methods::is(object, "lifertable") && is.null(object$TOTAL.EGGS))
      stop("The plot cannot be generated because the 'TOTAL.EGGS' component was not created when using the 'lifertable()' function.")

    HT <- if ( methods::is(object, "lifertable") ) {
      as.data.frame.lifertableTotEggs(object$TOTAL.EGGS)
    } else if ( methods::is(object, "lifertableTotEggs") ) {
      as.data.frame.lifertableTotEggs(object)
    }

    if (is.null(HT$GROUPS)) {
      ggplot2::ggplot(mapping = aes(HT$Female, HT$`Total Eggs`)) +
        geom_point(mapping = aes(color = HT$Female), show.legend = FALSE) +
        labs(title = "TOTAL EGGS LAID PER FEMALE",
             y = "Eggs", x = "Females") +
        theme(plot.title = element_text(hjust = 0.5))

    } else {
      ggplot2::ggplot(mapping = aes( HT$GROUPS , HT$`Total Eggs`)) +
        geom_point() +
        geom_boxplot(mapping = aes(color = HT$GROUPS), show.legend = FALSE ) +
        labs(title = "TOTAL EGGS LAID PER FEMALE",
             y = "", x = "") +
        theme(plot.title = element_text(hjust = 0.5))
    }

  } else {
    stop("'object' must be the result of the 'lifertable()' function.")
  }
}
