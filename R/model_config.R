
model_config_defaults <- list(

  # This special parameter forces the config function
  # to output all its results to the console.
  model_config_defaults.verbose = FALSE,

  # Tolerance for distribution fitting may be started
  # from a default config value but...
  # QUESTION: it could be dynamically adapted to facilitate
  #           fitting, as long as we transparently show the
  #           difference between the resulting distribution
  #           and the original estimates.
  dist_fit.tol = 0.001,

  # Poisson-PERT distribution
  dist_fit.3_points_poissonpert.range_size = .9,

  # Beta-PERT distribution
  dist_fit.3_points_betapert.range_size = .9,
  # References:
  # - The beta-PERT distribution, RiskAmp
  #   https://www.riskamp.com/beta-pert
  dist_fit.3_points_betapert.lambda = 4,

  # non-centrality parameter for beta distributions.
  # see help(rbeta) for a detailed description.
  dist_fit.beta.ncp = NULL,
  # dist_fit.beta.ncp = 0,

  plot.estimates.xintercept.color = "dodgerblue3",
  plot.estimates.xintercept.size = 0.5,

  plot.pdf.area.color = "#00aa00",
  plot.pdf.area.fill = "#55ff55",
  plot.pdf.area.alpha = 0.1,
  plot.pdf.area.size = 1,

  plot.cdf.estim_interecept.color = "dodgerblue3",
  plot.cdf.estim_interecept.size = 0.5,
  plot.cdf.area.color = "#00aa00",
  plot.cdf.area.size = 1,

  plot.qdf.estim_interecept.color = "dodgerblue3",
  plot.qdf.estim_interecept.size = 0.5,
  plot.qdf.area.color = "#00aa00",
  plot.qdf.area.fill = "#55ff55",
  plot.qdf.area.alpha = 0.1,
  plot.qdf.area.size = 1,

  plot.mdf.bar.color = "#00aa00",
  plot.mdf.bar.fill = "#55ff55",
  plot.mdf.bar.alpha = 0.1,
  plot.mdf.bar.size = 0.5,

  model_estimate_gldpert.range_size = .9,

  # Estimation method: triangular distribution + 3 points
  triangle_3points.range = .9

)

model_config_get_option <- function(...){
  # The following tweaking look inelegant.
  # I encountered issues with the NULL terminated string produced by cat()
  # and ended up with the ugly loops
  option_args <- c(...)
  item_key <- ""
  for(i in 1 : length(option_args))
  {
    if (!is_void(option_args[i])) {
      if (i > 1) {
        item_key <- paste0(item_key, ".", sep = "") }
      item_key <- paste0(item_key, option_args[i], sep = "") } }
  item_default_value <- model_config_defaults[[item_key]]
  item_value <- getOption(item_key, default = item_default_value)
  if (is_void(item_value))
  {
    stop("No available value for the requested configuration item: ", item_key)
  }
  #else
  #{
  # TODO: Implement if verbose with recurring call,
  #       but avoiding infinite loops
  #  # message(item_key, " = ", item_value, ", default: ", item_default_value)
  #}
  return (item_value)
}

