require(gld)

#' fit_gld_3points_lambda2
#'
#' This function optimizes the lambda2 (scale) parameter of a GLD distribution to further fit a 3 points estimate.
#' \cr This is an "internal" function called by fit_gld_3points.
#'
#' @inheritParams fit_gld_3points
#'
#' @return A numeric vector of size 1 representing an optimized lambda2 (scale) parameter that further fits a 3 points estimate.
#'
#' @export
fit_gld_3points_lambda2 = function(
  estimated_range_min_value,
  estimated_mode_value,
  estimated_range_max_value,
  estimated_range_size_proba,
  scaling_side = NULL,
  verbosity = NULL,
  ...) {
  # after lambda1 (location), we calculate lambda2 (PDF shape size or scale).
  # at this point, we don't consider skewness and assume shape symmetry.
  # lambda2 is like a "zoom" for the PDF,
  # but this parameter's value is inversely proportional to the shape size or scale.
  # to find an initial best match, we follow this procedure:
  # 0). "Neutralize" skewness parameters lambda3 and lambda4 setting them to -1
  # 1). Take an arbitrary PDF centered around 0 with lambda2 = e (and lambda3 & 4 set at - 1),
  # 2). Find the ratio between lambda2 and the desired quantile.
  # 3). Apply this ratio to the size / scale of the target distribution (provided in the estimation parameters).

  # scaling_side: default: "small". other possible value: "big". this parameter
  # tells us if we initialize lambda2 from the "small" side of a skewed distribution
  # or the "big" side. if the distribution is symetric, this parameter will have
  # no meaningful influence.
  if (is_void(scaling_side)) { scaling_side <- "small" }
  # OBSERVATION: when fitting the distribution from the small size, I get
  # excellent and consistent results. when we approach fitting from the big side,
  # we tend to fail to fit the distribution properly if the skewness is too large.
  # it would be great to analyse the causes of this difference in greater details.
  # it may be caused by number precision issues whereby too steep a curve would
  # require hyper precise number optimization.

  # because the 3 points estimates may be skewed,
  # we choose one of the two sides.
  # here, we decide to work with the larger side,
  # which means we will need to skew the other side
  # afterward.

  if (is_void(verbosity)) { verbosity <- 0 }

  estimated_range_min_proba <- (1 - estimated_range_size_proba) / 2
  estimated_range_max_proba <- 1 - (1 - estimated_range_size_proba) / 2

  left_value_range <- estimated_mode_value - estimated_range_min_value
  right_value_range <- estimated_range_max_value - estimated_mode_value

  # which side should we use?
  side <- NULL
  target_proba <- NULL
  if (left_value_range > right_value_range) {
    if (scaling_side == "small") {
      side <- "right"
      target_proba <- 1 - estimated_range_max_proba
    }
    else
    {
      side <- "left"
      target_proba <- estimated_range_min_proba
    }
  } else {
    if (scaling_side == "small") {
      side <- "left"
      target_proba <- estimated_range_min_proba
    }
    else
    {
      side <- "right"
      target_proba <- 1 - estimated_range_max_proba
    }
  }

  # if distribution was zero-centered
  # and we wanted its size to fit,
  # what would be the quantile value for that probability ?
  target_value <- NULL
  if (side == "left")
  {
    target_value <- -left_value_range
  } else {
    target_value <- -right_value_range
  }

  # This is where the wizardry operates, abracadabra!
  magic_value <- qgl(p = target_proba, lambda1 = 0, lambda2 = exp(1), lambda3 = -1, lambda4 = -1)
  magic_ratio <- magic_value / target_value
  magic_lambda2 = abs(exp(1) * magic_ratio)

  # TODO: Add some result quality check here.

  if (verbosity > 0) { message(paste0("lambda2: ", magic_lambda2)) }

  return(magic_lambda2)

}
