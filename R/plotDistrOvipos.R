
#' Plot for Distribution of Age at Oviposition
#'
#' This function generates a plot illustrating the Distribution of Oviposition (y-axis)
#' versus Age (x-axis), with separate representations for each group if multiple groups exist.
#' The data is sourced from the original database.
#'
#' @inheritParams lifertable
#'
#' @importFrom ggplot2 aes geom_point geom_boxplot geom_line facet_wrap labs theme element_text ggsave
#'
#' @return Returns an object of [`class`][base::class] c("gg", "ggplot").
#'
#' @export
#'
#' @examples
#' ## The Insects database will be used to generate the plot.
#'
#' plotDistrOvipos(Female, Age, Eggs, Group, data = Insects)
#'
#' ## The following expression will yield the same result as described above:
#'
#' plotDistrOvipos(Insects$Female, Insects$Age, Insects$Eggs, Insects$Group)
#'
#'
plotDistrOvipos <- function (ColumnFemale,
                             ColumnAge,
                             ColumnEggs,
                             ColumnGroups,
                             data,
                             InitiationOfAdultStage = 0) {

  if (missing(data)) {
    Female <- ColumnFemale
    Age <- ColumnAge
    Eggs <- ColumnEggs
  } else {
    Female <- eval(substitute(ColumnFemale), data)
    Age <- eval(substitute(ColumnAge), data)
    Eggs <- eval(substitute(ColumnEggs), data)
  }
  Age <- Age + InitiationOfAdultStage

  if (!missing(ColumnGroups)) {
    Group <- tryCatch({ eval(substitute(ColumnGroups), data) },
                      error = function(e) { ColumnGroups } )

    DT <- data.frame(Group = Group, Female = Female, Age = Age, Eggs = Eggs)
    meansH <- stats::aggregate(Eggs ~ Age + Group, data = DT, FUN = mean)

    ggplot2::ggplot(DT, mapping = aes(Age, Eggs)) +
      geom_point() +
      geom_boxplot(mapping = aes(group = Age, color = Age), show.legend = FALSE ) +
      geom_line(data = meansH, mapping = aes(Age, Eggs)) +
      facet_wrap( ~ Group, scales = "free", ncol = 1) +
      labs(title = "DISTRIBUTION OF AGE AT OVIPOSITION",
           x = "Age", y = "Number of eggs") +
      theme(plot.title = element_text(hjust = 0.5))

  } else {
    DT <- data.frame(Female = Female, Age = Age, Eggs = Eggs)
    meansH <- stats::aggregate(Eggs ~ Age, data = DT, FUN = mean)

    ggplot2::ggplot(DT, mapping = aes(Age, Eggs)) +
      geom_point() +
      geom_boxplot(mapping = aes(group = Age, color = Age), show.legend = FALSE ) +
      geom_line(data = meansH, mapping = aes(Age, Eggs)) +
      labs(title = "DISTRIBUTION OF AGE AT OVIPOSITION",
           x = "Age", y = "Number of eggs") +
      theme(plot.title = element_text(hjust = 0.5))
  }

}
