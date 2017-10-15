
frequency <- model_factor$new(
  name = "Risk frequency",
  factor_estimate = factor_estimate_poisson_mode$new(
    estimated_mode_value = 4,
    time_interval_friendly_name = "year"
    )
  )
frequency$get_random(n = 16)

impact <- model_factor$new(
  name = "Risk impact",
  factor_estimate = factor_estimate_gld_3points$new(
      estimated_range_min_value = 10,
      estimated_mode_value = 20,
      estimated_range_max_value = 100,
      estimated_range_size_proba = .9,
      limit_min_value = 5,
      limit_max_value = 2000)
  )
impact$get_random(n = 16)

risk <- model_factor$new(
  name = "Risk",
  # dependent factor names must be unique otherwise they overwrite
  dependent_factors = c(frequency, impact),
  factor_estimate = factor_estimate_composite_freqimpact$new(
    frequency_factor = frequency,
    impact_factor = impact
  )
)
risk$get_random(n = 16, output_class = "data.frame")
risk$plot_simulation_sample(
  n = 100,
  bins = 200,
  x_scale_type = "log10")
  #,x_end = 10, bins = 200)

df <- risk$get_random(n = 100, output_class = "data.frame")
