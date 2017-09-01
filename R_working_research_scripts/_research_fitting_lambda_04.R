## gldFit -
# Simulate Random Variates:
set.seed(1954)
s = rgld(n = 1000, lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8)

## gldFit -
# Fit Parameters:
gldFit(s, lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8,
       doplot = TRUE, trace = TRUE)
help(gldFit)

#help(dgld)
x <- seq(from = -20, to = 20, by = .01)
l1=0
l2=-.5
l3=-.2
l4=-.2
d <- dgld(x,lambda1=l1,lambda2=l2,lambda3=l3,lambda4=l4)
plot(d)
help(dgld)
help(gldFit)


fit <- gldFit(c(.1,.5,.9))
class(fit)
fit
fit["estimate"]
fit[["lambda1"]]
df <- function(x){dgld(x=x,lambda1=fit.lambda)}





