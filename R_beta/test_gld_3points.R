

# manual test

fg3 <- NULL
fg3 <- factor_estimate_gld_3points$new()
fg3$min_proba <- .05
fg3$mode_proba <- .5
fg3$max_proba <- .95
fg3$min_value <- -100
fg3$mode_value <- 0
fg3$max_value <- 100
fg3$lambda1 <- 0
fg3$lambda2 <- .14
fg3$lambda3 <- -1
fg3$lambda4 <- -1
fg3
fg3$graph_all(x_start = -200, x_end = 200)
