
#' Survival curve for the Life and Fertility Table
#'
#' This function generates the graphical representation of the Life Table.
#' Survival (y-axis) versus Age (x-axis)
#'
#' @param object \code{object} accepts 2 classes of objects:
#'   \itemize{
#'     \item{An object inheriting from \code{\link[=lifertable]{lifertable()}}, representing the
#'         Life and Fertility Table.}
#'     \item{An object of class \code{lifertableLFT} inherited from \code{\link[=lifertable]{lifertable()}}.
#'         This is the object that displays the Life and Fertility Table.}
#'   }
#'
#' @importFrom ggplot2 aes geom_point geom_line facet_wrap labs theme element_text ggsave
#'
#' @return Returns an object of [`class`][base::class] c("gg", "ggplot").
#'
#' @export
#'
#' @examples
#' ## The main object will be created using the Insects database:
#' lft <- lifertable(Female, Age, Eggs, Sexrate, ColumnGroups = Group,
#'                   data = Insects)
#'
#' ## Possible usage scenarios
#'
#' ## 1. Direct Usage of the "lft" Object:
#' plotSurvivalCurve(lft)
#'
#' ## 2. Assigning the component "LIFERTABLE" to an object:
#' lifeTable <- lft$LIFERTABLE
#' plotSurvivalCurve(lifeTable)
#'
#' ## 3. Direct usage of the component:
#' plotSurvivalCurve(lft$LIFERTABLE)
#'
#'
#'
plotSurvivalCurve <- function (object) {
  AGE <- NULL #It is not used, its to avoid NOTE
  LX <- NULL #It is not used, its to avoid NOTE
  GROUPS <- NULL #It is not used, its to avoid NOTE

  if ( methods::is(object, "lifertable") || methods::is(object, "lifertableLFT") ) {

    LFT <- if ( methods::is(object, "lifertable") ) {
      as.data.frame.lifertableLFT(object$LIFERTABLE)
    } else if ( methods::is(object, "lifertableLFT") ){
      as.data.frame.lifertableLFT(object)
    }

    if (is.null(LFT$GROUPS)) {
      ggplot2::ggplot(LFT, mapping = aes(AGE, LX)) +
        geom_point(mapping = aes(color = AGE), show.legend = FALSE) +
        geom_line() +
        labs(title = "SURVIVORSHIP CURVE", x = "Age",
             y = "Standardized survivorship (lx)") +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      ggplot2::ggplot(LFT, mapping = aes(AGE, LX )) +
        geom_point(mapping = aes(color = GROUPS), show.legend = FALSE) +
        geom_line(mapping = aes(color = GROUPS), show.legend = FALSE) +
        facet_wrap( ~ GROUPS, scales = "free", ) +
        labs(title = "SURVIVORSHIP CURVE", x = "Age",
             y = "Standardized survivorship (lx)") +
        theme(plot.title = element_text(hjust = 0.5))
    }

  } else {
    stop("'object' must be the result of the 'lifertable()' function.")
  }
}
