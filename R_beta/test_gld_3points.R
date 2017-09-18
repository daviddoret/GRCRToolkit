fg3 <- NULL
fg4 <- NULL
fg4 <- factor_estimate_gld_3points$new(
  estimated_range_min_value = -100,
  estimated_mode_value = 0,
  estimated_range_max_value = 100
  #estimated_range_min_proba = .05,
  #estimated_range_max_proba = .95,
  #fit_dist = TRUE,
  #simulate = TRUE
  )
#fg4$fit_dist()
#fg4$simulate()
fg4$graph_all(x_start = -150, x_end = +150)
fg4$parameter_consistency
