#' Q statistic
#'
#' This function calculates Chen and Qin statistic two sample multivariate test.
#' @param x multivariate sample
#' @param y multivariate sample
#' @export
q_stat<-function(x,y){
  nx <- nrow(x)
  ny <- nrow(y)

  f1 <- function(dd,n) {
    mm <- matrix(1:(n*n), ncol=n)
    ind <- expand.grid(j=1:n,k=1:n)[-diag(mm),]
    ss <- plyr::mdply(ind, function(j,k) sum(dd[j,]*dd[k,]) )
    sum(ss$V1)
  }

  f2 <- function(dd1,dd2,n1,n2) {
    ind <- expand.grid(j=1:n1,k=1:n2)
    ss  <- plyr::mdply(ind, function(j,k) sum(dd1[j,]*dd2[k,]) )
    sum(ss$V1)
  }

  tu.x <- f1(x,nx)
  tu.y <- f1(y,ny)
  tu.xy <- f2(x,y,nx,ny)

  tr2.x <- tr2_sigma(x)
  tr2.y <- tr2_sigma(y)
  tr.xy <- tr_sgxsgy(x,y)

  Q <- tu.x/(nx*(nx-1)) + tu.y/(ny*(ny-1)) - 2*tu.xy/(nx*ny)
  Q.var <- 2*tr2.x/(nx*(nx-1)) + 2*tr2.y/(ny*(ny-1)) + 4*tr.xy/(nx*ny)

  return(data.frame(Q=Q,Q.var=Q.var,tr2.x,tr2.y,tr.xy))
}

# aux.x<-tcrossprod(x,x)
# aux.y<-tcrossprod(y,y)
# aux.xy <- tcrossprod(x,y)
# tu.x <- 2*sum(aux.x[upper.tri(aux.x, diag = FALSE)])
# tu.y <- 2*sum(aux.y[upper.tri(aux.y, diag = FALSE)])
# tu.xy <- sum(aux.xy)

