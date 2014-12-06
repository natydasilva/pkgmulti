#' standarized Q test and p-value
#'
#' This function calculates Chen and Qin two sample multivariate test.
#' @param x multivariate sample
#' @param y multivariate sample
#' @export
CQ_test <- function(x,y){
 aux <- q_stat(x,y)
test<-  aux[1]/sqrt(aux[2])
p.val <- 1-pnorm(abs(test))
return(list(test,p.val))
}
