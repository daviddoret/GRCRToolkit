library(ggplot2)
library(R6)
library(rriskDistributions)
library(fitdistrplus)

f1 <- model_factor$new(name = "impact sample")
f1$estim_3_points_betapert(
  estim_min = 200,
  estim_typical = 1300,
  estim_max = 1500,
  range_size = .9)
f1$fit_distribution()
f1$get_random(8)
plot_factor(f1)

f2 <- model_factor$new(name = "norm sample", dist = "norm")
f2$estim_n_points(
  estim_quantiles = c(10,50,100,150,160),
  estim_probas = c(.1,.2,.5,.8,.9))
f2$fit_distribution()
f2$get_random(8)
plot_factor(f2)

f3 <- model_factor$new(name = "freq sample")
f3$estim_3_points_poissonpert(
  estim_min = 0,
  estim_typical = 2,
  estim_max = 5,
  range_size = .9)
f3$fit_distribution()
f3$get_random(8)
f3$probability_mass_function(c(1,2,3,4))
plot_factor(f3)



