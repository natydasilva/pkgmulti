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
    mx <- (1/(nx-1)) * (apply(x,2,sum) - x[j,]  )
    my <- (1/(ny-1)) * (apply(y,2,sum) - y[k,]  )

    A <- outer(x[j,]-mx, x[j,])
    B <- outer(y[k,]-my,y[k,])
    sum(A*B)
  }

  # get index for j and k and add up all tr(A_jk)
  ind <- expand.grid(j=1:nx,k=1:ny)
  trs <- plyr::mdply(ind, tr_jk)

  sum(trs$V1)/(nx*ny)
}

