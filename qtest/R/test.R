#' A test
#'
#' This function allows you to express your love of cats.
#' @param x jjja
#' @param y jjja
#' @export

chen<-function(x,y){
  
  nx <- nrow(x)
  
  ny <- nrow(y)
  
  aux.x<-(x)%*%t(x)
  
  aux.y<-(y)%*%t(y)
  
  aux.xy <- x%*%t(y)  
  
  tu.x <- sum(upper.tri(aux.x, diag = FALSE))
  
  tu.y <- sum(upper.tri(aux.y, diag = FALSE))
  
  tu.xy <- sum(upper.tri(aux.xy, diag = FALSE))
  
  Q <- tu.x/(nx(nx-1))+tu.y/(ny(ny-1))-2*tu.xy/(nx*ny)
  
  return(Q)
  
}

