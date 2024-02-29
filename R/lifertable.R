
#' Life and Fertility Table
#'
#' This function enables users to obtain life and fertility tables, offering
#' various configuration options for optimal usage. See "Details" section.
#'
#' @param ColumnFemale Data vector containing information on Females.
#'
#' @param ColumnAge Data vector containing information on Age.
#'
#' @param ColumnEggs Data vector containing information on the Number of Eggs Laid.
#'
#' @param SexRate Sex rate of eggs laid by the female at a certain age.
#'
#' @param Survival Percent of offspring females alive until adulthood.
#'     By default, the value is set to 1, assuming that all offspring will survive to adulthood.
#'
#' @param ColumnGroups Optional data vector containing information on the Groups.
#'     It is optional if the database only contains information about one group.
#'
#' @param data An optional data frame containing the variables. If not found in
#' \code{data}, the variables are taken from environment.
#'
#' @param InitiationOfAdultStage Age at which females became adults.
#'     If the database contains records from birth, entering this value is
#'     unnecessary. ONLY ENTER THIS VALUE if the database begins from the adult
#'     stage, and the values in \code{ColumnAge} do not reflect the preceding
#'     stage (i.e. they contain the ages: 1, 2, 3, ...).
#'
#' @param jackknife Logical. If \code{TRUE}, Jackknife estimations will be
#'     conducted to obtain Confidence Intervals for the Parameters and, if
#'     necessary, to compare between groups. Default is FALSE
#'
#' @param TotalEggs Logical. If  \code{TRUE}, the calculation of the number of
#'     eggs laid by each female during the entire experiment will be conducted.
#'     Default is FALSE.
#'
#' @details
#' \code{ColumnFemale} and \code{ColumnGroups} can be either a numeric vector or
#' a character vector. This means they may contain either numerical values or
#' labels corresponding to the female and to their respective group assignments.
#'
#' The standard approach for storing the Sex Rate and Survival rate during the
#' experiment is to input this information into the corresponding columns for
#' each variable. If this information remains consistent within a group, you can
#' input that value without the need to repeat it every time. If your database
#' encompasses a single experimental group, simply enter the corresponding value
#' in the \code{SexRate} and \code{Survival} arguments. In the case of having
#' more than one group, you can input the values of \code{SexRate} and
#' \code{Survival} correspondingly into a vector containing as many elements as
#' there are groups (one sex ratio and one survival rate for each group).
#'
#' \bold{Estimated Parameters:}
#'   \describe{
#'     \item{\emph{Net Reproductive Rate (Ro)}}{ Mean net contribution per female to the
#'       next generation.}
#'     \item{\emph{Intrinsic Rate of Increase (Rm)}}{ Rate of natural increase in a closed
#'       population that has been subject to a constant age-specific schedule of
#'       fertility and mortality for a long period, and has converged to be a
#'       stable population.}
#'     \item{\emph{Mean Generation Time (GT)}}{ Mean time span between the birth of
#'       individuals of a generation and that of the next generation.}
#'     \item{\emph{Doubling Time (DT)}}{ Time span necessary for doubling the initial
#'       population.}
#'     \item{\emph{Finite Rate of Increase (Lambda)}}{ It is a multiplication factor of
#'           the original population at each time period.}
#'   }
#'
#' \bold{Rm} it was determined by analytical approximation using Lotka’s (1907, 1913) equation:
#'
#' \deqn{\sum_{x=0}^{\infty}{\exp^{-R_{m}x}l_x m_x} = 1 }
#'
#'
#' @references
#' Maia, A. H., Luis, A. J., & Campanhola, C. (2000).
#' "Statistical Inference on Associated Fertility Life Table Parameters Using
#' Jackknife Technique: Computational Aspects". \emph{Journal of Economic Entomology},
#' 93(2), 511-518.
#' \doi{https://doi.org/10.1603/0022-0493-93.2.511}
#'
#' Portilla, M., Morales-Ramos, J. A., Guadalupe Rojas, M., & Blanco, C. A. (2014).
#' "Chapter 8 - Life Tables as Tools of Evaluation and Quality Control for Arthropod
#' Mass Production". \emph{Mass Production of Beneficial Organisms} (241-275).
#' \doi{https://doi.org/10.1016/B978-0-12-391453-8.00008-X}
#'
#'
#'
#'
#' @return
#' \code{lifertable} returns an object of [`class`][base::class] "lifertable".
#'
#' An object of class "lifertable" is a list containing the following components:
#'
#' \item{LIFERTABLE  }{ An object of class \code{lifertableLFT} containing the
#'     Life and Fertility Table.}
#' \item{PARAMETERS  }{ An object of class \code{lifertableParmEst} containing the
#'     Parameter Estimations}
#'
#' \item{TOTAL.EGGS  }{ If requested, an object of class \code{lifertableTotEggs}
#'     containing the total number of eggs laid by each female throughout the
#'     entire experiment.}
#' \item{CI  }{ If requested, an object of class \code{lifertableCI}
#'     containing the Confidence Intervals for the Parameter Estimates.}
#'
#' \item{T.TEST  }{ An object of class \code{lifertableTest} containing the
#'     Student t-test for pairwise group comparison. This component only appears
#'     if the experiment in question contains more than one group and a
#'     Jackknife estimation has been performed.}
#'
#' \item{PSEUDOS  }{ A list containing the pseudo values generated from the
#'     Jackknife estimation}
#' \item{GROUPS  }{ A list of the groups involved in the experiment.}
#'
#' @export
#'
#' @examples
#' ## The Insects database will be utilized:
#'
#' lifertable(Female, Age, Eggs, Sexrate, Survival, Group, data = Insects,
#'            jackknife = TRUE, TotalEggs = TRUE)
#'
#' ## The following expressions will yield the same result as above:
#'
#' ## lifertable(Insects$Female, Insects$Age, Insects$Eggs, Insects$Sexrate,
#' ##            Insects$Survival, Insects$Group, jackknife = TRUE,
#' ##            TotalEggs = TRUE)
#'
#' ## lifertable(Insects$Female, Insects$Age, Insects$Eggs,
#' ##            SexRate = 0.7, Survival = 0.9, Insects$Group,
#' ##            jackknife = TRUE, TotalEggs = TRUE)
#'
#' ## lifertable(Insects$Female, Insects$Age, Insects$Eggs,
#' ##            SexRate = c(0.7, 0.7), Survival = c(0.9, 0.9),
#' ##            Insects$Group, jackknife = TRUE, TotalEggs = TRUE)
#'
#'
lifertable <- function(ColumnFemale,
                       ColumnAge,
                       ColumnEggs,
                       SexRate,
                       Survival = 1,
                       ColumnGroups,
                       data,
                       InitiationOfAdultStage = 0,
                       jackknife = FALSE,
                       TotalEggs = FALSE) {

  if (missing(data)) {
    Female <- ColumnFemale
    Age <- ColumnAge
    Eggs <- ColumnEggs
  } else {
    Female <- eval(substitute(ColumnFemale), data)
    Age <- eval(substitute(ColumnAge), data)
    Eggs <- eval(substitute(ColumnEggs), data)
  }

  sr <- tryCatch({ eval(substitute(SexRate), data) },
                 error = function(e) { SexRate } )
  surv <- tryCatch({ eval(substitute(Survival), data) },
                   error = function(e) { Survival } )

  Age <- Age + InitiationOfAdultStage


  if (!missing(ColumnGroups)) {
    Group <- tryCatch({ eval(substitute(ColumnGroups), data) },
                      error = function(e) { ColumnGroups } )

    lifertable.groups(ColGroups = Group, ColumnFemale = Female,
                      ColumnAge = Age, ColumnEggs = Eggs,
                      ColSexRate = sr, ColSurvival = surv,
                      jackknife = jackknife, TotalEggs = TotalEggs)

  } else {

    SR <- if (length(sr) == length(Age)) {
      stats::aggregate(cbind(SR = sr),
                by = list(AGE = Age),
                FUN = mean, na.rm = TRUE)$SR
    } else { sr }
    Surv <- if (length(surv) == length(Age)) {
      stats::aggregate(cbind(Surv = surv),
                by = list(AGE = Age),
                FUN = mean, na.rm = TRUE)$Surv
    } else { surv }

    # # Jackknife ---------------------------------------------------------------
    compJK <- stats::aggregate(Female ~ Age, FUN = length, na.action = stats::na.pass)

    if (length(compJK$Female) != length(Female)){
      if (jackknife) {
        lifertable.jackknife(Female = Female, Age = Age,
                             Eggs = Eggs, SexRate = SR,
                             Survival = Surv, TotalEggs = TotalEggs)
      } else {

        # # LIFERTABLE JK -------------------------------------------------------
        FEMALES <- stats::aggregate(cbind(FEMALES = Female),
                             by = list(AGE = Age),
                             FUN = length)
        NEGG <- stats::aggregate(cbind(NEGG = Eggs),
                          by = list(AGE = Age),
                          FUN = sum, na.rm = TRUE)

        LIFERTABLE <- merge(FEMALES, NEGG)

        Lifertable <- lifertable.individual(ColumnAge = LIFERTABLE$AGE,
                                            ColumnFemale = LIFERTABLE$FEMALES,
                                            ColumnEggs = LIFERTABLE$NEGG,
                                            SexRate = SR,
                                            Survival = Surv)
        if (TotalEggs) {
          Lifertable$TOTAL.EGGS <- stats::aggregate(cbind("Total Eggs" = Eggs) ~ Female,
                                                    FUN = sum)
          class(Lifertable$TOTAL.EGGS) <- "lifertableTotEggs"
        }

        return(Lifertable)
      }

    } else {

      lifertable.individual(ColumnAge = Age, ColumnFemale = Female,
                            ColumnEggs = Eggs, SexRate = SR,
                            Survival = Surv)

    }
  }
}
