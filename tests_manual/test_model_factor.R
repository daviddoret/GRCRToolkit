
f1 <- model_factor$new(name = "x", dist = "beta-pert")
f1$estim_3_points_betapert(
  estim_min = 200,
  estim_typical = 300,
  estim_max = 1500,
  range_size = .9)
f1$fit_dist()
f1$get_random(8)
f1$plot()

f2 <- model_factor$new(name = "y", dist = "norm")
f2$estim_n_points(
  estim_quantiles = c(10,50,100,150,160),
  estim_probas = c(.1,.2,.5,.8,.9))
f2$fit_dist()
f2$get_random(8)
f2$plot()

