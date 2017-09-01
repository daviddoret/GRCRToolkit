# Status: this approach looks good, to be completed and properly tested

#install.packages("fBasics")
#install.packages("stats")

library(fBasics)
library(stats)

# the input parameters of the PERT estimation
known_cumul_p <- c(.05,.5,.95)

# initialize a vector of unknown lambda values.
# these are the values we want to find.
unknown_l <- c(.4,-.3,-.4,-.2)
unknown_l
unknown_q <- qgld(p=known_cumul_p,lambda1=unknown_l[1],lambda2=unknown_l[2],lambda3 = unknown_l[3],lambda4 = unknown_l[4])
unknown_q
unknown_d <- dgld(x=unknown_q,lambda1=unknown_l[1],lambda2=unknown_l[2],lambda3 = unknown_l[3],lambda4 = unknown_l[4])
unknown_d
check_cumul_p <- pgld(q=unknown_q,lambda1=unknown_l[1],lambda2=unknown_l[2],lambda3 = unknown_l[3],lambda4 = unknown_l[4])
check_cumul_p

target_q <- unknown_q
target_p <- known_cumul_p

to_be_minimized <- function(l){
  if(l[2]>0) return(Inf)
  if(l[3]>0) return(Inf)
  if(l[4]>0) return(Inf)

  #NEXT STEP: TRY WITH NATIVE stats4::mle FUNCTION!!!!

 cumul_p <- pgld(q=target_q,lambda1=l[1],lambda2=l[2],lambda3=l[3],lambda4=l[4])
 #conso <- sum(abs(log(cumul_p - target_p)))
 conso <- prod(abs(cumul_p - target_p))
 message(cat(cumul_p,conso,sep=";"))
 return(conso)
}

message("SHOULD MATCH PERFECTLY, i.e. SHOULD BE EQUAL 0")
to_be_minimized(l=unknown_l)

initial_l <- c(
  target_q[1], #as a first approximation
  -.5,-.5,-.5)

## PAS DE BONS RESULTATS
#fitx <- optimx(par = initial_l,
##       method = "CG", #spg", #bobyqa",
#       fn = to_be_minimized,
#       control = list(all.methods = TRUE),
##       itnmax = 100000,
#       upper = c(1000000,0.0001,0.0001,0.0001))
#l1 <- fitx["p1"]
#l2 <- fitx["p2"]
#l3 <- fitx["p3"]
#l4 <- fitx["p4"]
#unknown_l

#COOLS RESULTATS
fit <- optim(par = initial_l,
             fn = to_be_minimized,
             control = list(
               maxit = 1000000,
               parscale = c(1,.01,.01,.01),
               ndeps=.00001))
fit

fitted_l <- fit$par
fitted_d <- dgld(x=unknown_q,lambda1=fitted_l[1],lambda2=unknown_l[2],lambda3 = unknown_l[3],lambda4 = unknown_l[4])
fitted_d


message("****************")
message("COMPARE RESULTS")
fit$par
unknown_l


plot_probability_density_function(fun=unknown_d, x_start=0, x_end=2)
plot_probability_density_function(fun, x_start, x_end, ...)
