
fg3 <- NULL
fg3 <- factor_estimate_gld_3points$new()
fg3$min_proba <- .25
fg3$mode_proba <- .5 # GET RID OF THIS MISTAKE
fg3$max_proba <- .75
fg3$min_value <- -100
fg3$mode_value <- 0
fg3$max_value <- 100
fg3$fit_dist_step01_location()
fg3$fit_dist_step02_scale()
#fg3$optimize_lambda3()
#fg3$calculate_lambda4()
fg3$graph_all(x_start = -100, x_end = +100)
cat(fg3$get_print(),sep = "\n")




