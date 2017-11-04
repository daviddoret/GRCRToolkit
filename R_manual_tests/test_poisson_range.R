
fg4 <- NULL
fg4 <- factor_estimate_poisson_range$new(
  estimated_range_min_value = 0,
  estimated_range_max_value = .1,
  estimated_range_size = .9,
  fit_distribution = FALSE,
  simulate = FALSE
  )
fg4$fit_distribution(verbosity = 0)
fg4$simulate()
fg4$get_simulation_sample_head(5)
fg4$get_simulation_sample_tail(5)
fg4$get_simulation_sample_random(n=10)
fg4$get_random(5)
#fg4$plot_all(x_start = -150, x_end = +150)
#fg4$check_state_consistency(output_format = "boolean")
#fg4$check_state_consistency(output_format = "report")
#fg4$estimated_mode_value > fg4$estimated_range_max_value
#fg4$estimated_mode_value <- -5000
#fg4$lambda3 <- 45
fg4$plot_density()

