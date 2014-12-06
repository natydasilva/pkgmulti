#' Trace of variance estimation
#'
#' This function calculates Chen and Qin trace of varince estimation .
#' @param x multivariate sample
#' @param y multivariate sample
#' @export
#' @example
#' x <- rmvnorm(50,mean=c(1,2,3),3*diag(3))
#' y <- rmvnorm(40,mean=c(1,2,3)+5,diag(3))
#' q_stat(x,y)
tr_var <- function(data){
  n <- dim(data)[[1]]

  tr_jk <- function(j,k) {
    m <- (n/(n-2))*(apply(data,2,mean)-(data[j,]+data[k,]))
    sum(t(tcrossprod( t(data[-k,]) - m , t(data[-k,])) ) * tcrossprod( t(data[-j,]) - m , t(data[-j,]) )  )
  }

  mm <- matrix(1:(n*n), ncol=n)
  aux <- mm[lower.tri(mm)]
  ind <- expand.grid(j=1:n, k=1:n)[aux,]

  trs <- mdply(ind, tr_jk)
}
