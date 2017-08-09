library(GRCRToolkit)
library(ggplot2)

f1 <- NULL
f1 <- model_factor$new(name = "x", dist = "lnorm")
f1$estim_3_points(
  estim_min = 100,
  estim_typical = 500,
  estim_max = 900,
  range_size = .95)
f1$get_dist_fit_probabilities()
f1$get_dist_fit_quantiles()
f1$get_dist_fit_weights()
f1$fit_dist()
f1$get_random(8)
f1$get_proba_density_plot()

#print(f1$dist_fitted_params)
#print(f1$dist_fitted_params["sdlog"])
#rlnorm(n=2,meanlog=6.68,sdlog=0.07)


#prob_density_fun_lnorm <- function(x) {
#  return(
#    dlnorm(
#      x,
#      meanlog = f1$dist_fitted_params["meanlog"],
#      sdlog = f1$dist_fitted_params["sdlog"],
#      log = FALSE ))}
#prob_density_fun_norm <- function(x) {
#  return(
#    dnorm(
#      x,
#      mean = f1$dist_fitted_params["mean"],
#      sd = f1$dist_fitted_params["sd"],
#      log = FALSE ))}
prob_density_fun <- f1$get_proba_density

ggplot(data.frame(x=c(f1$estim_min - (f1$estim_max - f1$estim_min) * .2,
                      f1$estim_max + (f1$estim_max - f1$estim_min) * .2)),
       aes(x)) +

  # Give a little bit of margin on the graph sides
  xlim(f1$estim_min - (f1$estim_max - f1$estim_min) * .2
       ,f1$estim_max + (f1$estim_max - f1$estim_min) * .2) +
  #ylim: let it scale automatically

  # Axis titles
  ylab("Relative likelihood")  +
  xlab("Factor value")  +

  # Limit the number of digits on the vertical axis
  scale_y_continuous(label = function(x) { round(x,3) }) +

  # Display 3 vertical bars to highlight the 3 points of the estimate
  geom_vline(xintercept = f1$get_dist_fit_quantiles()[1]) +
  geom_vline(xintercept = f1$get_dist_fit_quantiles()[2]) +
  geom_vline(xintercept = f1$get_dist_fit_quantiles()[3]) +

  # Area plot the PDF function with a neutral background
  stat_function(fun = prob_density_fun,
                colour = "#555555",
                geom = "area",
                fill = "white",
                alpha = 0.5 ) +

  # Area plot of PDF function within the estimation range with a vivid background
  stat_function(
    colour = "#00aa00",
    fun = prob_density_fun,
    geom = 'area',
    fill = 'green',
    alpha = 0.1,
    size = 1.1,
    xlim = c(f1$get_dist_fit_quantiles()[1],f1$get_dist_fit_quantiles()[3])) +

  # On top of the rest, label the 3 vertical bars
  annotate(geom = "text", x = f1$get_dist_fit_quantiles()[2], y = 0, label = "Typical", angle = 90, hjust = -1, vjust = -.2) +
  annotate(geom = "text", x = f1$get_dist_fit_quantiles()[1], y = 0, label = "Min", angle = 90, hjust = -1, vjust = -.2) +
  annotate(geom = "text", x = f1$get_dist_fit_quantiles()[3], y = 0, label = "Max", angle = 90, hjust = -1, vjust = -.2) +

  # And put a title on top of it
  ggtitle("Probability density function")

trueValues <- data.frame("p" = f1$get_dist_fit_probabilities(),
                         "q" = f1$get_dist_fit_quantiles(),
                         "my_label" = c("min","typ","max"))
trueValues

cumul_distrib_fun <- function(q) {
  return(
    plnorm(q,
           meanlog = f1$dist_fitted_params["meanlog"],
           sdlog = f1$dist_fitted_params["sdlog"],
           log = FALSE ))}

ggplot(data.frame(x=c(500, 1200)), aes(x)) +

  # Probability distribution
  stat_function(
    fun=cumul_distrib_fun,
    geom="line",
    aes(colour="density function")) +

  # Shade the estimation area
  stat_function(
    fun=cumul_distrib_fun,
    geom = 'area',
    fill = 'blue',
    alpha = 0.1,
    aes(colour="estimation range"),
    xlim = c(f1$get_dist_fit_quantiles()[1],f1$get_dist_fit_quantiles()[3])) +

  geom_point(data=trueValues, aes(x=q, y=p, label=my_label), colour="red", size=5) +

  geom_vline(xintercept = f1$get_dist_fit_quantiles()[1]) +
  geom_label(aes(f1$get_dist_fit_quantiles()[1],1,label = "Min")) +
  geom_vline(xintercept = f1$get_dist_fit_quantiles()[2]) +
  geom_label(aes(f1$get_dist_fit_quantiles()[2],1,label = "Typical")) +
  geom_vline(xintercept = f1$get_dist_fit_quantiles()[3]) +
  geom_label(aes(f1$get_dist_fit_quantiles()[3],1,label = "Max"))

