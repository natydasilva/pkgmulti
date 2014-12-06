#' Trace of square of variance matrix estimator
#'
#' Compute Trace of square of variance matrix estimator.
#' @param data multivariate sample
#' @export
tr2_sigma <- function(data){
  n <- dim(data)[[1]]
  # compute trace for A_jk
  tr_jk <- function(j,k) {
    m <- (n/(n-2))*(apply(data,2,mean)-(data[j,]+data[k,]))
    sum(t(tcrossprod( t(data[-j,])-m, t(data[-j,]))) * tcrossprod( t(data[-k,])-m, t(data[-k,])))
  }

  # get index for j and k
  mm <- matrix(1:(n*n), ncol=n)
  aux <- mm[lower.tri(mm)]
  ind <- expand.grid(j=1:n,k=1:n)[aux,]

  # add up all tr(A_jk)
  trs <- plyr::mdply(ind, tr_jk)
  2*sum(trs$V1)/(n*(n-1))
}


