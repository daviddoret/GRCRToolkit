
fg3 <- NULL
fg3 <- factor_estimate_gld_3points$new()

fg3$lambda1 <- 0
fg3$lambda2 <- 1
fg3$lambda3 <- -1
fg3$lambda4 <- -1

fg3$min_proba <- .5
fg3$mode_proba <- .5 # GET RID OF THIS MISTAKE
fg3$max_proba <- .90
fg3$min_value <- -100
fg3$mode_value <- -80
fg3$max_value <- 100
#fg3$fit_dist_step01_location()
#fg3$fit_dist_step02_scale()
#fg3$optimize_lambda3()
#fg3$calculate_lambda4()
fg3$fit_dist()
fg3$graph_all(x_start = -200, x_end = +200)
cat(fg3$get_print(),sep = "\n")


# Step A: Find which side must be skewed

fg3$fit_dist_step01_location()
fg3$fit_dist_step02_scale()
fg3$calculate_lambda3()
fg3$calculate_lambda4()
fg3$graph_all(x_start = -200, x_end = +200)
cat(fg3$get_print(),sep = "\n")

#fg3$lambda1 <- fg3$lambda1 - abs(fg3$dist_mode - fg3$lambda1)







