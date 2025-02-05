
#' Life and Fertility Table, for a single group and the generic form
#'
#' This function is intended for internal use and supports the primary functionality of the \link{lifertable} function.
#'
#' @param SexRate Sex rate of eggs laid by the female at a certain age.
#' @param Survival Percent of offspring females alive until adulthood.
#' @inheritParams lifertable
#'
#' @return Return an object of class \link{lifertable}. Contains only the
#'     \code{LIFERTABLE} and \code{PARAMETERS} components.
#'
#' @noRd
#'
lifertable.individual <- function(ColumnAge,
                                  ColumnFemale,
                                  ColumnEggs,
                                  SexRate,
                                  Survival) {

  n <- max(ColumnFemale, na.rm = TRUE)
  # # LIFERTABLE ------------------------------------------------------------
  LIFERTABLE <- data.frame(AGE = ColumnAge,
                           FEMALES = ColumnFemale,
                           NEGG = ColumnEggs )

  if (length(Survival) != length(LIFERTABLE$AGE) && length(Survival) != 1) {
    stop("`Survival` has incorrect length")
  }
  if (length(SexRate) != length(LIFERTABLE$AGE) && length(SexRate) != 1) {
    stop("`SexRate` has incorrect length")
  }

  LIFERTABLE$Lx <- Survival * (LIFERTABLE$FEMALES / n)
  LIFERTABLE$Mx <- (LIFERTABLE$NEGG / LIFERTABLE$FEMALES) * SexRate
  LIFERTABLE$LxMx <- LIFERTABLE$Lx * LIFERTABLE$Mx
  LIFERTABLE$xLxMx <- LIFERTABLE$AGE * LIFERTABLE$LxMx

  # NUEVOS CÁLCULOS - -
  Lx1 <- c(LIFERTABLE$Lx[-1], 0)
  LIFERTABLE$gx <- Lx1 / LIFERTABLE$Lx

  Lxprom <- (Lx1 + LIFERTABLE$Lx) / 2
  Tx <- rev(cumsum(rev(Lxprom)))
  LIFERTABLE$ex <- Tx / LIFERTABLE$Lx



  LIFERTABLE[is.na(LIFERTABLE)] <- 0

  # # PARAMETERS ------------------------------------------------------------
  RO <- sum(LIFERTABLE$LxMx, na.rm = TRUE)
  TG <- (sum(LIFERTABLE$xLxMx, na.rm = TRUE)) / RO
  RM <- (log(RO)) / TG

  ITER <- merge(cbind(LIFERTABLE[c("AGE", "LxMx")], RM),
                seq(0.8,1.2,0.005))
  ITER$R <- RM * ITER$y
  ITER$iter <- ITER$LxMx * exp( (-ITER$R) * ITER$AGE )

  APROXs <- stats::aggregate(cbind(Sum = iter) ~ R,
                             data = ITER,
                             FUN = sum,
                             na.rm = TRUE  )
  APROXs$aprox <- abs(1 - APROXs$Sum)


  PARAMETERS <- data.frame(Ro = RO,
                           Rm = APROXs[match(min(APROXs$aprox, na.rm = TRUE),
                                             APROXs$aprox), "R"])


  PARAMETERS$GT <- log(PARAMETERS$Ro)/PARAMETERS$Rm
  PARAMETERS$DT <- log(2)/PARAMETERS$Rm
  PARAMETERS$Lambda <- exp(PARAMETERS$Rm)

  # # Lifertable ------------------------------------------------------------
  Lifertable <- list(LIFERTABLE = LIFERTABLE, PARAMETERS = PARAMETERS)

  class(Lifertable) <- "lifertable"
  class(Lifertable$LIFERTABLE) <- "lifertableLFT"
  class(Lifertable$PARAMETERS) <- "lifertableParmEst"

  return(Lifertable)
}
