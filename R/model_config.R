
model_config_defaults <- list(
  dist_fit.tol = 0.001,

  dist_fit.3_points_pert.range_size = .9,
  # References:
  # - The beta-PERT distribution, RiskAmp
  #   https://www.riskamp.com/beta-pert
  dist_fit.3_points_pert.lambda = 4,

  # non-centrality parameter for beta distributions.
  # see help(rbeta) for a detailed description.
  dist_fit.beta.ncp = NULL,
  # dist_fit.beta.ncp = 0,

  #TODO: Remove these default configurations, it just creates confusion.
  dist_fit.weights.beta = c(1,1,1),
  dist_fit.weights.lnorm = c(.01,1,.8),
  dist_fit.weights.norm = c(1,1,1)
)

model_config_get_option <- function(...){
  # The following tweaking look inelegant.
  # I encountered issues with the NULL terminated string produced by cat()
  # and ended up with the ugly loops
  option_args <- c(...)
  item_key <- ""
  for(i in 1 : length(option_args))
  {
    if(! is.null(option_args[i])) {
      if(i > 1) {
        item_key <- paste0(item_key, ".", sep="") }
      item_key <- paste0(item_key, option_args[i], sep="") } }
  item_default_value <- model_config_defaults[[item_key]]
  item_value <- getOption(item_key, default = item_default_value)
  if(is.null(item_value))
  {
    stop("No available value for the requested configuration item: ", item_key)
  }
  else
  {
    message(item_key, " = ", item_value, ", default: ", item_default_value)
  }
  return (item_value)
}

