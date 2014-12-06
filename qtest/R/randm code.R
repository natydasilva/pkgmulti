library(mvtnorm)



x<-rmvnorm(5,mean=c(1,2,3),diag(3))
y<-rmvnorm(4,mean=c(1,2,3),diag(3))

x%*%t(x) - t(x%*%t(x))

tr2_sigma(x)
CQ_test(x,y)
q_stat(x,y)

sim <- function(mu){

  x<-rmvnorm(5,mean=mu,diag(3))
  y<-rmvnorm(4,mean=mu,diag(3))
  CQ_test(x,y)
  }

library(plyr)
res <- rdply(100,sim(c(1,2,3)))
