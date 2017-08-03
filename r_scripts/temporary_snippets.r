# Temp R snippets

estim <- three_points_estimate(range_min=10, typical=500, range_max=900)
estim
get_range_size(estim)
set_range_size(estim, range_size = 40)
estim
get_range_size(estim)
estim
estim@range_size <- 60
get_range_size(estim)
estim

