
fac_01 <- NULL
fac_01 <- factor_estimate_poisson$new(
  lambda = 6,
  time_interval_friendly_name = "Year"
  )
fac_01$fit_dist(verbosity = 0)
fac_01$simulate()
print(fac_01)

fac_01$get_simulation_sample_head(5)
fac_01$get_simulation_sample_tail(5)
fac_01$get_simulation_sample_random(n=10)
fac_01$get_random(5)
fac_01$limit_min_value <- 0
fac_01$limit_max_value <- 20
#fg4$plot_all(x_start = -150, x_end = +150)
#fg4$check_state_consistency(output_format = "boolean")
#fg4$check_state_consistency(output_format = "report")
#fg4$estimated_mode_value > fg4$estimated_range_max_value
#fg4$estimated_mode_value <- -5000
#fg4$lambda3 <- 45
#fac_01$plot_density(x_start = 0, x_end = 10)
fac_01$plot_mass(x_start = 0, x_end = 10)

#g2 <- fac_01$plot_probability()
#g3 <- fac_01$plot_quantile()

