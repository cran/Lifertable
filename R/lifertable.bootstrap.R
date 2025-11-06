
#' Obtain Boostrap Estimates
#'
#' This function is intended for internal use and supports the primary functionality of the \link{lifertable} function.
#'
#' @param Female Data vector containing information on Females.
#' @param Age Data vector containing information on Age.
#' @param Eggs Data vector containing information on the Number of Eggs Laid.
#' @param SexRate Sex rate of eggs laid by the female at a certain age.
#' @param Survival Percent of offspring females alive until adulthood.
#' @param reSamples Number of sub-samples for calculate the Bootstrap estimates.
#'
#' @param ... Further arguments, passed to other methods.
#'
#' @return Return an object of class \link{lifertable}. Add the \code{CI} and
#'     \code{PSEUDOS} components.
#'
#' @noRd
#'
lifertable.bootstrap <- function(Female,
                                 Age,
                                 Eggs,
                                 SexRate,
                                 Survival,
                                 reSamples,
                                 ...) {

  Ages <- Age[!duplicated(Age)]

  BS <- data.frame(Female = Female,
                   Age = Age,
                   Eggs = Eggs)

  if (length(SexRate) == length(Age) || length(SexRate) == 1) {
    BS$SexRate <- SexRate
  } else if (length(SexRate) == length(Ages)) {
    sr <- data.frame(Age = Ages, SexRate)
    BS <- merge(BS, sr, by = "Age")
  } else {
    stop("`SexRate` has incorrect length")
  }

  if (length(Survival) == length(Age) || length(Survival) == 1) {
    BS$Survival <- Survival
  } else if (length(Survival) == length(Ages)) {
    surv <- data.frame(Age = Ages, Survival)
    BS <- merge(BS, surv, by = "Age")
  } else {
    stop("`Survival` has incorrect length")
  }

  Females <- Female[!duplicated(Female)]
  n <- length(Females)



  boot <- list()
  for (i in 1:reSamples) {
    boot[[i]] <- sample(Females, n, replace = TRUE)
  }



  ITER <- lapply(boot, function(x){
    DT <- data.frame()
    for (j in x) {
      D <- BS[(which(BS$Female == j)), ]
      DT <- rbind(DT, D)
    }
    return(DT)
  })
  names(ITER) <- c(paste0("V", c(1:reSamples)))



  DBS <- lapply(ITER, function(x){
    as.data.frame(lifertable(ColumnFemale = x$Female,
                             ColumnAge = x$Age,
                             ColumnEggs = x$Eggs,
                             SexRate = x$SexRate,
                             Survival = x$Survival,
                             CI = FALSE,
                             TotalEggs = FALSE)$PARAMETERS)
    })

  bootstrap <- do.call(rbind, DBS)

  bootstrap <- lapply(bootstrap, FUN = function(x){ x })

  Lifertable <- lifertable(ColumnFemale = BS$Female,
                           ColumnAge = BS$Age,
                           ColumnEggs = BS$Eggs,
                           SexRate = BS$SexRate,
                           Survival = BS$Survival,
                           CI = FALSE, ...)

  Lifertable$CI <- lapply(bootstrap, FUN = intconfBS )



  # Nuevo
  Lifertable$PARAMETERS$Ro <- Lifertable$CI$Ro[[2]]
  Lifertable$PARAMETERS$Rm <- Lifertable$CI$Rm[[2]]
  Lifertable$PARAMETERS$GT <- Lifertable$CI$GT[[2]]
  Lifertable$PARAMETERS$DT <- Lifertable$CI$DT[[2]]
  Lifertable$PARAMETERS$Lambda <- Lifertable$CI$Lambda[[2]]




  class(Lifertable$CI) <- "lifertableCIBootstrap"

  Lifertable$PSEUDOS <- bootstrap


  return(Lifertable)
}
