# STATUS: OPTIMS RUNS VERY NICELY AND YIELDS EXCELLENT RESULT
#         UNTIL WE TWEAK L4!!!!!


# Package preparation
if (!require(pacman)) install.packages(pacman)
pacman::p_load(gld)

# Fit a Generalized Lambda Distribution with 3 point estimates within a given range

low <- 20
mode <- 50
high <- 95
range <- .90
range_absolute <- high - low

low_proba <- (1 - range) / 2
mode_proba <- .5
high_proba <- 1 - (1 - range) / 2

estim_probas <- c(low_proba,mode_proba,high_proba)
estim_values <- c(low, mode, high)

lambda_parameterization <- "fmkl" # The FMKL parameterisation gives a valid statistical distribution for any real values of lambda1, lambda3,lambda4 and any positive real values of lambda2.

# lambda1: location parameter, or α for the gpd parameterisation
# this parameter is obvious
lambda1 <- mode

# lambda2: scale (shape) parameter (β for gpd)
# this is basically the denominator
# we can start with an initial value,
# and we decide to start with an approximation
# of a normal distribution.
lambda_normal_approximation <- 0.14 # Reference: http://www.itl.nist.gov/div898/handbook/eda/section3/eda366f.htm
lambda2 <- lambda_normal_approximation #1 / range_absolute / ( 1 - range ) * 3.1514872

# lambda3: first shape parameter (δ, a skewness parameter for gpd)
lambda3 <- -1

# lambda4: second shape parameter (λ, a tail-shape parameter for gpd)
lambda4 <- -1

# lambda5: a skewing parameter, in the fm5 parameterisation
lambda5 <- NULL

dfun <- function(x){return(dgl(x = x,lambda1 = lambda1,lambda2 = lambda2,lambda3 = lambda3,lambda4 = lambda4))}
qfun <- function(p){return(qgl(p = p,lambda1 = lambda1,lambda2 = lambda2,lambda3 = lambda3,lambda4 = lambda4))}
pfun <- function(q){return(pgl(q = q,lambda1 = lambda1,lambda2 = lambda2,lambda3 = lambda3,lambda4 = lambda4))}
rfun <- function(n){return(rgl(n = n,lambda1 = lambda1,lambda2 = lambda2,lambda3 = lambda3,lambda4 = lambda4))}

lambda_legend <- function()
{
  return(
    paste0("λ1: ", lambda1, ", λ2: ", lambda2, ", λ3: ", lambda3, ", λ4: ", lambda4)
  )
}

show_situation <- function(info) {

  dist_low_value <- low
  dist_mode_value <- mode
  dist_high_value <- high

  dist_low_proba <- pfun(dist_low_value)
  dist_mode_proba <- pfun(dist_mode_value)
  dist_high_proba <- pfun(dist_high_value)
  dist_probas <- c(dist_low_proba,dist_mode_proba,dist_high_proba)

  delta_probas <- estim_probas / dist_probas

  range_quantiles <- c(qfun(p=estim_probas
                           ))

  graph_low <- low - range_absolute
  graph_high <- high + range_absolute

  g1 <- plot_probability_density_function(fun = dfun, x_start = graph_low, x_end = graph_high)
  g2 <- overplot_estimates(
    graph = g1,
    estim_quantiles = estim_values,
    estim_labels = paste0(round(estim_values,4), " E:", round(estim_probas,4), " D:", round(dist_probas,4)),
    color = "red")
#  g3 <- overplot_estimates(
#    graph = g2,
#    estim_quantiles = estim_values,
#    estim_labels = paste0(round(estim_probas
#                               ,4), "=", round(range_quantiles,4)),
#    color = "green")
  #g3

  summary <- data.frame(estim_probas,
                        dist_probas,
                        delta_probas,
                        estim_values)

  print(info)
  print(lambda_legend())
  print(summary)

  return(g2)

}

g_firstguess <- show_situation("First guess")
g_firstguess






message("STEP 2: OPTIMIZE LAMBDA 2")

l2_optim_fun <- function(x){
    dist_range_size <-
      qgl(p=high_proba, lambda1 = lambda1,lambda2 = x,lambda3 = lambda3,lambda4 = lambda4)
      -
      qgl(p=low_proba, lambda1 = lambda1,lambda2 = x,lambda3 = lambda3,lambda4 = lambda4)

    target_range_size <-
      high - low

    delta <- abs(dist_range_size - target_range_size)

    return(delta)
}

message("Optim function result before optimization")
l2_optim_fun(x=lambda2)

l2_optim <- optimize(l2_optim_fun, interval = c(lambda2 / 10,lambda2 * 10), tol = .000001)

# Retrieve the optimized lambda2 parameter
lambda2 <- l2_optim$minimum
message("Optim function result after optimization")
l2_optim_fun(x=lambda2)

g_l2_optimized <- show_situation("Optimized lambda2")
g_l2_optimized


# TODO: Special case, if distribution is symmetrical,
#       stop optimization here.


message("STEP 3: OPTIMIZE LAMBDA 3")

l3_optim_fun <- function(x){
  return(
    abs(
      low - qgl(p=low_proba, lambda1 = lambda1,lambda2 = lambda2,lambda3 = x,lambda4 = lambda4)
    )
  )
}

message("Optim function result before optimization")
l3_optim_fun(x=lambda3)

l3_optim <- optimize(l3_optim_fun, interval = c(-100,100), tol = .000001)

# Retrieve the optimized lambda3 parameter
lambda3 <- l3_optim$minimum
message("Optim function result after optimization")
l3_optim_fun(x=lambda3)

g_l3_optimized <- show_situation("Optimized lambda3")
g_l3_optimized






message("STEP 4: OPTIMIZE LAMBDA 4")

l4_optim_fun <- function(x){
  return(
    abs(
      high - qgl(p=low_proba, lambda1 = lambda1,lambda2 = lambda2,lambda3 = lambda3,lambda4 = x)
    )
  )
}

message("Optim function result before optimization")
l4_optim_fun(x=lambda4)

l4_optim <- optimize(l4_optim_fun, interval = c(-100,100), tol = .00000001)

# Retrieve the optimized lambda4 parameter
lambda4 <- l4_optim$minimum
message("Optim function result after optimization")
l4_optim_fun(x=lambda4)

g_l4_optimized <- show_situation("Optimized lambda4")
g_l4_optimized

#g_all <- multiplot(g_firstguess, g_l2_optimized, g_l3_optimized, g_l4_optimized, cols=1)
#g_all
