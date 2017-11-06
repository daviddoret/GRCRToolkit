
impact <- model_factor$new(
  name = "Impact",
  factor_estimate = factor_estimate_gld_3points$new(
    limit_min_value = 50,
    estimated_range_min_value = 98,
    estimated_mode_value = 100,
    estimated_range_max_value = 102,
    limit_max_value = 150,
    estimated_range_size_proba = .9)
  )

frequency <- model_factor$new(
  name = "Frequency",
  factor_estimate = factor_estimate_poisson_mode$new(
  limit_min_value = 0,
  estimated_mode_value = .5,
  limit_max_value = 12,
  time_interval_friendly_name = "events / year",
  fit_distribution = TRUE,
  simulate = TRUE)
)

risk <- model_factor$new(
  name = "Risk",
  factor_estimate = factor_estimate_composite_freqimpact$new(
    frequency_factor = frequency,
    impact_factor = impact,
    limit_min_value = 0,
    limit_max_value = 1000
  )
)

impact$get_random(30)
frequency$get_random(30)
risk$get_random(30)


risk$plot_simulation_sample(n = 10000, bins = 200)

