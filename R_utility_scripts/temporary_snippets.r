

impact_estimate <- factor_estimate_gld_3points$new(
  limit_min_value = 10,
  estimated_range_min_value = 100,
  estimated_mode_value = 120,
  estimated_range_max_value = 200,
  limit_max_value = 10000,
  estimated_range_size_proba = .9)

frequency_estimate <- factor_estimate_poisson_mode$new(
  limit_min_value = 0,
  estimated_mode_value = .1,
  limit_max_value = 12,
  time_interval_friendly_name = "events / year",
  fit_distribution = TRUE,
  simulate = TRUE
)
