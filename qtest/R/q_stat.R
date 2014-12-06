#' Q statistic
#'
#' This function calculates Chen and Qin statistic two sample multivariate test.
#' @param x multivariate sample
#' @param y multivariate sample
#' @export
q_stat<-function(x,y){

  nx <- nrow(x)

  ny <- nrow(y)

  aux.x<-tcrossprod(x,x)

  aux.y<-tcrossprod(y,y)

  aux.xy <- tcrossprod(x,y)

  tu.x <- 2*sum(aux.x[upper.tri(aux.x, diag = FALSE)])

  tu.y <- 2*sum(aux.y[upper.tri(aux.y, diag = FALSE)])

  tu.xy <- sum(aux.xy)

  Q <- tu.x/(nx*(nx-1))+tu.y/(ny*(ny-1))-2*tu.xy/(nx*ny)

  Q.var <- (2*tr2_sigma(x))/(nx*(nx-1)) + (2*tr2_sigma(y))/(ny*(ny-1))+(4/nx*ny)*tr_sgxsgy(x,y)

  return(c(Q.test=Q,Q.var=Q.var))

}

