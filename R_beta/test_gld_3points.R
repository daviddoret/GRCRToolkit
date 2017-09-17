
fg3 <- NULL
fg3 <- factor_estimate_gld_3points$new()
fg3$min_proba <- .05
fg3$max_proba <- .95
fg3$min_value <- -100
fg3$mode_value <- 0
fg3$max_value <- 100
fg3$fit_dist_location()
fg3$fit_dist_scale()
fg3$fit_dist(precision = .01)
fg3$graph_all(x_start = -200, x_end = +200)

