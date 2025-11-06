
#' Life and Fertility Table, for more than 1 group
#'
#' This function is intended for internal use and supports the primary functionality of the \link{lifertable} function.
#'
#' @param ColGroups Data vector containing information on the Groups.
#' @param ColSexRate Sex rate of eggs laid by the female at a certain age.
#' @param ColSurvival Percent of offspring females alive until adulthood.
#' @inheritParams lifertable
#'
#' @return Return an object of class \link{lifertable}. Add the \code{T.TEST}
#'     and \code{GROUPS} components.
#'
#' @noRd
#'
lifertable.groups <- function(ColGroups,
                              ColumnFemale,
                              ColumnAge,
                              ColumnEggs,
                              ColSexRate,
                              ColSurvival,
                              CI,
                              technique,
                              reSamples,
                              TotalEggs,
                              InitAge) {

  Data <- data.frame(Group = ColGroups,
                     Female = ColumnFemale,
                     Age = ColumnAge,
                     Eggs = ColumnEggs)
  Groups <- ColGroups[!duplicated(ColGroups)]

  # # Separamos los grupos ----------------------------------------------------
  GROUPS <- split(Data, Data$Group)
  for (i in 1 : length(GROUPS)) {
    GROUPS[[i]] <- as.list(GROUPS[[i]])

    if (length(InitAge) == 1) {
      GROUPS[[i]]$InitAge <- InitAge
    } else if (length(InitAge) == length(GROUPS)) {
      GROUPS[[i]]$InitAge <- split(InitAge, Groups)[[i]]
    } else if (length(InitAge) == nrow(Data)) {
      GROUPS[[i]]$InitAge <- split(InitAge, ColGroups)[[i]]
    } else {
      stop("`adultStage` has incorrect length")
    }

    if (length(ColSexRate) == 1) {
      GROUPS[[i]]$SexRate <- ColSexRate
    } else if (length(ColSexRate) == length(GROUPS)) {
      GROUPS[[i]]$SexRate <- split(ColSexRate, Groups)[[i]]
    } else if (length(ColSexRate) == nrow(Data)) {
      GROUPS[[i]]$SexRate <- split(ColSexRate, ColGroups)[[i]]
    } else {
      stop("`SexRate` has incorrect length")
    }

    if (length(ColSurvival) == 1) {
      GROUPS[[i]]$Survival <- ColSurvival
    } else if (length(ColSurvival) == length(GROUPS)) {
      GROUPS[[i]]$Survival <- split(ColSurvival, Groups)[[i]]
    } else if (length(ColSurvival) == nrow(Data)) {
      GROUPS[[i]]$Survival <- split(ColSurvival, ColGroups)[[i]]
    } else {
      stop("`Survival` has incorrect length")
    }
  }

  TOTAL <- lapply(GROUPS,
                  FUN = function(x) {
                    lifertable(ColumnFemale = x$Female,
                               ColumnAge = x$Age,
                               ColumnEggs = x$Eggs,
                               SexRate = x$SexRate,
                               Survival = x$Survival,
                               CI = CI,
                               technique = technique,
                               reSamples = reSamples,
                               TotalEggs = TotalEggs,
                               adultStage = x$InitAge)
                  })


  Groups <- names(TOTAL)

  Lifertable <- list(
    LIFERTABLE = lapply(TOTAL, FUN = function(x) x$LIFERTABLE),
    PARAMETERS = lapply(TOTAL, FUN = function(x) x$PARAMETERS)
  )
  class(Lifertable$LIFERTABLE) <- "lifertableLFT"
  class(Lifertable$PARAMETERS) <- "lifertableParmEst"

  if (TotalEggs) {
    Lifertable$TOTAL.EGGS = lapply(TOTAL, FUN = function(x) x$TOTAL.EGGS)
    class(Lifertable$TOTAL.EGGS) <- "lifertableTotEggs"
  }

  if (CI) {
    Lifertable$CI <- lapply(TOTAL, FUN = function(x) x$CI)
    Lifertable$PSEUDOS <- lapply(TOTAL, FUN = function(x) x$PSEUDOS)

    Lifertable$CI <- list(
      Ro = lapply(Lifertable$CI, FUN = function(x) x$Ro),
      Rm = lapply(Lifertable$CI, FUN = function(x) x$Rm),
      GT = lapply(Lifertable$CI, FUN = function(x) x$GT),
      DT = lapply(Lifertable$CI, FUN = function(x) x$DT),
      Lambda = lapply(Lifertable$CI, FUN = function(x) x$Lambda)
    )

    if (technique == "jackknife") {
      class(Lifertable$CI$Ro) <- "lifertableCIJackknife"
      class(Lifertable$CI$Rm) <- "lifertableCIJackknife"
      class(Lifertable$CI$GT) <- "lifertableCIJackknife"
      class(Lifertable$CI$DT) <- "lifertableCIJackknife"
      class(Lifertable$CI$Lambda) <- "lifertableCIJackknife"

      class(Lifertable$CI) <- "lifertableCIJackknife"

    } else if (technique == "bootstrap"){
      class(Lifertable$CI$Ro) <- "lifertableCIBootstrap"
      class(Lifertable$CI$Rm) <- "lifertableCIBootstrap"
      class(Lifertable$CI$GT) <- "lifertableCIBootstrap"
      class(Lifertable$CI$DT) <- "lifertableCIBootstrap"
      class(Lifertable$CI$Lambda) <- "lifertableCIBootstrap"

      class(Lifertable$CI) <- "lifertableCIBootstrap"

    }


    Dpares <- list() ; k = 1
    for (i in 1 : (length(TOTAL) - 1) ) {
      for (j in (i + 1) : length(TOTAL) ) {

        Dpares[[ k ]] = list(A = TOTAL[[i]]$PSEUDOS, B = TOTAL[[j]]$PSEUDOS)

        names(Dpares[[ k ]]) = c(Groups[i], Groups[j])

        names(Dpares) = c(names(Dpares)[ - length(names(Dpares))],
                          paste("COMPARISON BETWEEN GROUPS :",
                                Groups[i], "-", Groups[j] ) )
        k = k + 1
      }
    }


    Lifertable$T.TEST <- lapply(Dpares, pruebast)
    class(Lifertable$T.TEST) <- "lifertableTest"
  }

  Lifertable$GROUPS <- Groups

  class(Lifertable) <- "lifertable"
  return(Lifertable)
}
