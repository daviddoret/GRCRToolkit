

# manual test

fg3 <- NULL
fg3 <- factor_estimate_gld_3points$new()
fg3$min_proba <- .05
fg3$mode_proba <- .5
fg3$max_proba <- .95
fg3$min_value <- 10000
fg3$mode_value <- 500000
fg3$max_value <- 2000000
fg3$lambda1 <- 0
fg3$lambda2 <- 10 #.01 #.14 # TODO: Use this dimension as an additional way to tweak the distrib
fg3$lambda3 <- -1
fg3$lambda4 <- -1
fg3$optimize_lambda1()
fg3$optimize_lambda4()
fg3$optimize_lambda3()
fg3$graph_all()
#fg3$graph_density()
#fg3$graph_quantile()
fg3

# options(digits=22)
# flat_fun <- function(x){
#   if(x >= 0) { return(Inf)  }
#   return(pgl(q = fg3$max_value, lambda1 = fg3$lambda1, lambda2 = fg3$lambda2, lambda3 = fg3$lambda3, lambda4 = x))
# }
# nlm_lambda4 <- function(x, ...){
#     return(
#       abs(
#         vapply(x, flat_fun, 0)
#         -
#         fg3$max_proba
#       ) * 1000000
#     )
# }
# optim <- nlm(nlm_lambda4, -1, ndigit = 22, print.level = 2, iterlim = 128)
# optim
# fg3$lambda4 <- optim$estimate
# fg3$get_probability(fg3$max_value)
# fg3$get_quantile(fg3$max_proba)
