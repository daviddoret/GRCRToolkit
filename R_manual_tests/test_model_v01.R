
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
      limit_min_value = 2,
      limit_max_value = 10000)
  )
impact$get_random(n = 16)

risk <- model_factor$new(
  name = "Risk",
  # dependent factor names must be unique otherwise they overwrite
  dependent_factors = c(frequency, impact),
  factor_estimate = factor_estimate_composite_custom$new(
    random_function = function(n = NULL, ...) {
      f <- self$get_dependent_factor("Frequency")$get_random(n = n, output_class = "data.frame", ...)
      foreach(item in f)
      {
        freq_impacts <- self$get_dependent_factor("Impact")$get_random(n = item, ...)
        df.add()
      }
      df <- new data.frame()
    }
  )
)
