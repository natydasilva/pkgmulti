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
tr_var <- function(data,j,k){
n <- dim(data)[[1]]
m <- (n/(n-2))*(apply(data,2,mean)-(data[j,]+data[k,]))

trvar <- sum(diag((data[-k,]-m)%*%t(data)%*%t((data[-k,]-m)%*%t(data))))
}
