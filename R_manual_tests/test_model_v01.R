
risk <- model_factor$new(
  name = "Risk",
  factor_estimate = factor_estimate_composite$new()
  )


frequency <- model_factor$new(
  name = "Risk frequency",
  factor_estimate = factor_estimate_poisson_mode$new(
    estimated_mode_value = .1,
    time_interval_friendly_name = "year"
    )
  )
frequency$get_random(n = 4)

impact <- model_factor$new(
  name = "Risk impact",
  factor_estimate = factor_estimate_gld_3points$new(
      estimated_range_min_value = 10,
      estimated_mode_value = 90,
      estimated_range_max_value = 100,
      estimated_range_size_proba = .9,
      limit_min_value = 0,
      limit_max_value = 10000)
  )


