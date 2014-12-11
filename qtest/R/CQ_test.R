#' standarized Q test and p-value
#'
#' This function calculates Chen and Qin two sample multivariate test.
#' @param x multivariate sample
#' @param y multivariate sample
#' @export
CQ_test <- function(x,y){
  aux <- q_stat(x,y)
  test <-  as.numeric( aux[1] / sqrt(aux[2]))
  p.val <- 2*pnorm( abs(test),lower.tail = FALSE)

  return(cbind(data.frame(CQ.stat=test,p.val=p.val),aux))
}
