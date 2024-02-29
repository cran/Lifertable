
#' Obtain Jackknife Estimates
#'
#' This function is intended for internal use and supports the primary functionality of the \link{lifertable} function.
#'
#' @param Female Data vector containing information on Females.
#' @param Age Data vector containing information on Age.
#' @param Eggs Data vector containing information on the Number of Eggs Laid.
#' @param SexRate Sex rate of eggs laid by the female at a certain age.
#' @param Survival Percent of offspring females alive until adulthood.
#' @param ... Further arguments, passed to other methods.
#'
#' @return Return an object of class \link{lifertable}. Add the \code{CI} and
#'     \code{PSEUDOS} components.
#'
#' @noRd
#'
lifertable.jackknife <- function(Female,
                                 Age,
                                 Eggs,
                                 SexRate,
                                 Survival,
                                 ...) {

  Ages <- Age[!duplicated(Age)]
  JK <- data.frame(Female = Female,
                   Age = Age,
                   Eggs = Eggs)

  if (length(SexRate) == length(Age) || length(SexRate) == 1) {
    JK$SexRate <- SexRate
  } else if (length(SexRate) == length(Ages)) {
    sr <- data.frame(Age = Ages, SexRate)
    JK <- merge(JK, sr, by = "Age")
  } else {
    stop("`SexRate` has incorrect length")
  }

  if (length(Survival) == length(Age) || length(Survival) == 1) {
    JK$Survival <- Survival
  } else if (length(Survival) == length(Ages)) {
    surv <- data.frame(Age = Ages, Survival)
    JK <- merge(JK, surv, by = "Age")
  } else {
    stop("`Survival` has incorrect length")
  }

  Females <- Female[!duplicated(Female)]
  n <- length(Females)

  ITER <- list(V0 = JK)
  for (i in 1:n) {
    D <- JK[-(which(JK$Female == Females[i])), ]
    ITER[[i + 1]] <- D
    names(ITER) <- c(names(ITER)[-length(ITER)], paste0("V", i))
  }


  DJK <- lapply(ITER, function(x){
    as.data.frame(lifertable(ColumnFemale = x$Female,
                             ColumnAge = x$Age,
                             ColumnEggs = x$Eggs,
                             SexRate = x$SexRate,
                             Survival = x$Survival,
                             jackknife = FALSE,
                             TotalEggs = FALSE)$PARAMETERS)
    })

  PARAMETERS <- DJK[[1]]
  partials <- do.call(rbind, DJK)

  pseudos <- lapply( partials,
                     FUN = function(x) { (n * x[1]) - ((n - 1) * x) } )

  pseudos <- lapply( pseudos,
                     FUN = function(x){ x[-1] } )

  Lifertable <- lifertable(ColumnFemale = JK$Female,
                           ColumnAge = JK$Age,
                           ColumnEggs = JK$Eggs,
                           SexRate = JK$SexRate,
                           Survival = JK$Survival,
                           jackknife = FALSE, ...)

  Lifertable$CI <- lapply(pseudos, FUN = intconf )
  class(Lifertable$CI) <- "lifertableCI"

  Lifertable$PSEUDOS <- pseudos


  return(Lifertable)
}
