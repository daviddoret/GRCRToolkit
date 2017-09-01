

library(gld)
help(pgl)


install.packages("optimx")
library(optimx)
help(optimx)

mean <- 75.43
sd <- 7.12
r <- rnorm(n=50, mean=mean, sd=sd)
r
p1 <- c(.05,.5,.95)
q <- qnorm(p=p1, mean=mean, sd=sd)
q
p2 <- pnorm(q=q, mean=mean, sd=sd)
p2
fn <- function(vect){
  return(
    - sum(
      log(
        abs(
    qnorm(p=p1,sd=vect[1],mean=vect[2])
    - q
      )
  )
  ))
}
vect <- c(10,9)
#vect[1]
#vect[2]
fn(vect)

optimx(
  c(10,9),
  function(x){
    return(
      - sum(
        log(
          abs(
            qnorm(p=p1,sd=x[1],mean=x[2])
            - q
          )
        )
      ))
  },
  lower=c(0,0),
  upper=c(1000,100),
  control=list(all.methods=TRUE, save.failures=TRUE, trace=0)
)
  #gs=100)

fn(c(10,9))
