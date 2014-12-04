#' Trace of product of variance matrices estimator
#'
#' Compute Trace of the product of two variance matrices estimator.
#' @param x multivariate sample
#' @param y multivariate sample
#' @export
tr_sgxsgy <- function(x,y){
  nx <- dim(x)[1]
  ny <- dim(y)[1]

  # compute trace for A_jk
  tr_jk <- function(j,k) {
    mx <- (nx/(nx-1)) * (apply(x,2,mean) - x[j,]  )
    my <- (ny/(ny-1)) * (apply(y,2,mean) - y[k,]  )
    sum(t(tcrossprod( t(x[-j,])-mx, t(x[-j,]))) * tcrossprod( t(y[-k,])-my, t(y[-k,])))
  }

  # get index for j and k and add up all tr(A_jk)
  ind <- expand.grid(j=1:nx,k=1:ny)
  trs <- plyr::mdply(ind, tr_jk)

  sum(trs$V1)/(nx*ny)
}

