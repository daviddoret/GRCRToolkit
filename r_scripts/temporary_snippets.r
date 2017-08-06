# Temp R snippets

f2 <- fair_factor$new()
f2$estimate_3_points(estimate_min = 100, estimate_typical = 800, estimate_max = 900, range_size = .9)
f2$get_dist_fit_probabilities()
f2$get_dist_fit_quantiles()
f2$fit_dist_lnorm()
#f2$range_min
#f2$range_min <- 17

library(rriskDistributions)
q <- stats::qbeta(p = c(0.025, 0.5, 0.975), shape1 = 2, shape2 = 3)
old.par <- graphics::par(mfrow = c(2, 3))
get.beta.par(q = q)
get.beta.par(q = q, scaleX = c(0.001, 0.999))
get.beta.par(q = q, fit.weights = c(10, 1, 10))
get.beta.par(q = q, fit.weights = c(1, 10, 1))
get.beta.par(q = q, fit.weights = c(100, 1, 100))
get.beta.par(q = q, fit.weights = c(1, 100, 1))
graphics::par(old.par)

library(rriskDistributions)
p <- c(0.025, 0.5, 0.6, 0.975)
q <- round(mc2d::qpert(p = p, min = 0, mode = 3, max = 10, shape = 5), digits = 2)
chosenDistr6 <- fit.perc(p = p, q = q, tolPlot = 10)
chosenDistr6

library(rriskDistributions)
#q <- stats::qlnorm(p = c(0.05, 0.5, 0.95), meanlog = 4, sdlog = 0.8)
#q
chosenDistr1 <- fit.perc()

#old.par <- graphics::par(mfrow = c(2, 3))
#get.lnorm.par(q = q)
#x <- get.norm.par(p = c(.1,.2,.8), q = c(0.05, 0.5, 0.95), fit.weights = c(1, 1, 1))
#x
fitted <- get.lnorm.par(p = c(.05, .5, .95),
                   q = c(10, 75, 100),
                   show.output=TRUE,
                   plot=TRUE,
                   tol = 0.01,
                   fit.weights = c(2, 1, 1)
                   )
fitted
fitted_meanlog <- fitted["meanlog"]
fitted_sdlog <- fitted["sdlog"]

stats::qlnorm(p = c(0.05, 0.5, 0.95),
                   meanlog = fitted_meanlog,
                   sdlog = fitted_sdlog)
stats::dlnorm(x = c(0.05, 0.5, 0.95),
              meanlog = fitted_meanlog,
              sdlog = fitted_sdlog)
stats::plnorm(q = c(0.05, 0.5, 0.95),
              meanlog = fitted_meanlog,
              sdlog = fitted_sdlog)

#help(qlnorm)
#help(get.lnorm.par)
#graphics::par(old.par)
