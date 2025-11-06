
#' Plot for Distribution of Age at Oviposition
#'
#' This function generates a plot illustrating the Distribution of Oviposition (y-axis)
#' versus Age (x-axis), with separate representations for each group if multiple groups exist.
#' The data is sourced from the original database.
#'
#' @inheritParams lifertable
#'
#' @param time A string that defines the time period over which “Age” is measured
#'     ("days", "months", "years", etc). Default is "days".
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
#' plotDistrOvipos(ColumnFemale = Female,
#'                 ColumnAge = Age,
#'                 ColumnEggs = Eggs,
#'                 ColumnGroups = Group,
#'                 data = Insects)
#'
#' ## The following expression will yield the same result as described above:
#'
#' plotDistrOvipos(ColumnFemale = Insects$Female,
#'                 ColumnAge = Insects$Age,
#'                 ColumnEggs = Insects$Eggs,
#'                 ColumnGroups = Insects$Group)
#'
#'
plotDistrOvipos <- function (ColumnFemale,
                             ColumnAge,
                             ColumnEggs,
                             ColumnGroups,
                             data,
                             adultStage = 0,
                             time = "days") {

  if (missing(data)) {
    Female <- ColumnFemale
    Age <- ColumnAge
    Eggs <- ColumnEggs
  } else {
    Female <- eval(substitute(ColumnFemale), data)
    Age <- eval(substitute(ColumnAge), data)
    Eggs <- eval(substitute(ColumnEggs), data)
  }

  Init <- tryCatch({ eval(substitute(adultStage), data) },
                   error = function(e) { adultStage } )

  #Age <- Age + adultStage

  if (!missing(ColumnGroups)) {
    Group <- tryCatch({ eval(substitute(ColumnGroups), data) },
                      error = function(e) { ColumnGroups } )

    Groups <- Group[!duplicated(Group)]

    if ( (length(Init) == 1) || (length(Init) == length(Age))) {
      Age <- Age + Init
      DT <- data.frame(Group = Group, Female = Female, Age = Age, Eggs = Eggs)
    } else if (length(Init) == length(Groups)) {
      GroupInit <- data.frame(Group = Groups, Init)
      Init2 <- merge(data.frame(Group, Female, Age, Eggs), GroupInit, by = "Group")
      DT <- data.frame(Group = Init2$Group, Female = Init2$Female,
                       Age = Init2$Age+Init2$Init, Eggs = Init2$Eggs)
    } else {
      stop("`adultStage` has incorrect length")
    }

    meansH <- stats::aggregate(Eggs ~ Age + Group, data = DT, FUN = mean, na.rm = TRUE)

    ggplot2::ggplot(DT, mapping = aes(Age, Eggs)) +
      geom_point() +
      geom_boxplot(mapping = aes(group = Age, color = Age), show.legend = FALSE ) +
      geom_line(data = meansH, mapping = aes(Age, Eggs)) +
      facet_wrap( ~ Group, scales = "free", ncol = 1) +
      labs(title = "DISTRIBUTION OF AGE AT OVIPOSITION",
           x = paste0("Age (", time, ")"), y = "Number of eggs") +
      theme(plot.title = element_text(hjust = 0.5))

  } else {
    Age <- Age + Init

    DT <- data.frame(Female = Female, Age = Age, Eggs = Eggs)
    meansH <- stats::aggregate(Eggs ~ Age, data = DT, FUN = mean, na.rm = TRUE)

    ggplot2::ggplot(DT, mapping = aes(Age, Eggs)) +
      geom_point() +
      geom_boxplot(mapping = aes(group = Age, color = Age), show.legend = FALSE ) +
      geom_line(data = meansH, mapping = aes(Age, Eggs)) +
      labs(title = "DISTRIBUTION OF AGE AT OVIPOSITION",
           x = paste0("Age (", time, ")"), y = "Number of eggs") +
      theme(plot.title = element_text(hjust = 0.5))
  }

}
