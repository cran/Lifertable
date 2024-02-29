
#' Compute the t-test for a pair of groups
#'
#' Function to perform a paired t-test for all groups. It is an internal function.
#'
#' @param base A list containing two more lists, the pseudo-values of each group to be evaluated.
#'
#' @return
#' \code{pruebast} returns a list containing data.frames with the t-test comparison between pairs of groups.
#'
#' @noRd
#'
pruebast <- function(base){
  groups <- names(base)
  ttst <- data.frame(1:7)

  for(i in 1 : 5 ){
    vec1 <- base[[1]][[i]]
    vec2 <- base[[2]][[i]]
    prueba <- c( MED1 = mean(vec1),
                 MED2 = mean(vec2),
                 STD1 = sqrt(stats::var(vec1) / length(vec1)),
                 STD2 = sqrt(stats::var(vec2) / length(vec2)),
                 BILATERAL = stats::t.test(vec1, vec2)$p.value,
                 RIGHT = stats::t.test(vec1, vec2, alternative = "greater")$p.value,
                 LEFT = stats::t.test(vec1, vec2, alternative = "less")$p.value )
    ttst[i] = prueba
  }
  ttst <- data.frame( PARAMETRO = names(base[[1]]) , t(ttst) )
  names(ttst) = c("Parameter",
                  paste("Mean", groups[1]),
                  paste("Mean", groups[2]),
                  paste("sd", groups[1]),
                  paste("sd", groups[2]),
                  "Two-tailed P-Value","Right-tailed P-Value","Left-tailed P-value")
  ttst <- ttst[ order(ttst$Parameter), ]; row.names(ttst) = NULL

  return(ttst)
}

