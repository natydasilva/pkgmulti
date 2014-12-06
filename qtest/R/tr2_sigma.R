#' Trace of square of variance matrix estimator
#'
#' Compute Trace of square of variance matrix estimator.
#' @param data multivariate sample
#' @export
tr2_sigma <- function(data){
  n <- dim(data)[[1]]
  # compute trace for A_jk
  tr_jk <- function(j,k) {
    m <- (1/(n-2))*(apply(data,2,sum)-(data[j,]+data[k,]))
    A <- outer(data[j,]-m, data[j,])
    B <- outer(data[k,]-m,data[k,])
    sum(A*B)
  }

  # get index for j and k
  mm <- matrix(1:(n*n), ncol=n)
  ind <- expand.grid(j=1:n,k=1:n)[-diag(mm),]

  # add up all tr(A_jk)
  trs <- plyr::mdply(ind, tr_jk)
  sum(trs$V1)/(n*(n-1))
}


