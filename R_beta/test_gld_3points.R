

# manual test

fg3 <- NULL
fg3 <- factor_estimate_gld_3points$new()
fg3$min_proba <- .5
fg3$mode_proba <- .5
fg3$max_proba <- .95
fg3$min_value <- 100
fg3$mode_value <- 200
fg3$max_value <- 220
fg3$lambda1 <- 0
fg3$lambda2 <- .14 # TODO: Use this dimension as an additional way to tweak the distrib
fg3$lambda3 <- -1
fg3$lambda4 <- -1
fg3$fit_distribution()
fg3$graph_all(x_start = 0, x_end = 500)
fg3$simulate()
cat(fg3$get_print(),sep = "\n")

fg3$lambda2 <- .01

# OBSERVATIONS:
# Avec l3,l4 == -1, la distrib est parfaitement centrée sur son "mode".
# l2 n'influence pas la position.
# Avec l3 = -2, le mode part à droite




d1 <- fg3$simulation_data

# qplot(
#   factor_value,
#   data = d1,
#   geom = "histogram",
#   fill = I("blue"),
#   colour = I("black"),
#   alpha = .1,
#   bins = 30,
#   xlim = c(0, 600),
#   xlab = "Factor value",
#   ylab = "Count")
#
# ggplot(
#   d1,
#   aes(x = factor_value, y = "")
# ) + geom_boxplot()


