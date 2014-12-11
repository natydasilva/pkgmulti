library(mvtnorm)
p <- 3
SS <- diag(p)
x<-rmvnorm(50,mean=rep(0,p),SS)
y<-rmvnorm(50,mean=rep(1,p),SS)
dif <- apply(x,2,mean) - apply(y,2,mean)
sum(dif^2)


tr2_sigma(x)
CQ_test(x,y)
q_stat(x,y)

sim <- function(mu, Gam, n){
  p <- length(mu)
  zs.x <- matrix(rnorm( (p+2)*n ), ncol=n)
  zs.y <- matrix(rnorm( (p+2)*n ), ncol=n)

  x <- t( Gam %*% zs.x + mu )
  y <- t( Gam %*% zs.x + mu )

  CQ_test(x,y)
  }

library(plyr)
library(SoDA)

p <- 10
n <- 50
#G <- triDiag(2.883, 2.794, 2.849, ncol=p+2, nrow=p+2)
G <- triDiag(1,1,1, ncol=p+2, nrow=p+2)[-c(1,(p+2)),]

# True traces and Var(Q)
Sig <- G %*% t(G)
tr2 <- sum(diag(Sig %*% Sig))
VQ  <- 4*tr2/(n*(n-1)) + 4*tr2/n^2
c(VQ, tr2)

# simulations
res <- rdply(500, sim( mu = rep(0,p), Gam=G, n=n), .progress='text')
summary(res)

mean(res$p.val < .05)



# bai simulation
p <- 4
G1 <- triDiag(1,.5,0, ncol=p+1, nrow=p)
S1 <- (G1 %*% (4*diag(p+1)) %*% t(G1))  # el cuatro viene de la gamma(4,1)...
sum( diag( S1 %*% S1 ))

rho <- .5

S1.th <- triDiag(5,2,2, ncol=p, nrow=p)

sum( diag( S1.th %*% S1.th ))









