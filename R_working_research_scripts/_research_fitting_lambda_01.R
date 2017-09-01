#install.packages("GLDreg")
#install.packages("gld")
library(GLDreg)
library(gld)

set.seed(10)

## Create dataset
#x<-rnorm(200,3,2)
#y<-3*x+rnorm(200)
p <- c(.1, .5, .95)
q <- c(1200,1342,1789)

dat<-data.frame(p,q)
dat

## Fit FKML GLD regression with 3 simulations
fit <- GLD.lm.full(q~p,data=dat,fun=fun.RMFMKL.ml.m,param="fkml",n.simu=3)

l1 <- fit[[2]][1,4]
l2 <- fit[[2]][1,5]
l3 <- fit[[2]][1,6]
l4 <- fit[[2]][1,7]

l1
l2
l3
l4

#remove.packages("GLDEX")
d1 <- qgl(p=seq(from=0, to=1,by=.05),lambda1=l1,lambda2=l2,lambda3=l3,lambda4=l4)
plot(d1)
d2 <- dgl(x=seq(from=0, to=100,by=.1),lambda1=l1,lambda2=l2,lambda3=l3,lambda4=l4)
plot(d2)

