#' Update the MAP prior of the variance
#' @description This function updates MAP prior of the variance, a mixture
#' of inverse Gamma distributions, based on an estimated of the sample variance.
#' @param prior a list with elements w, shape, rate specifying the parameters
#' of the mixture of inverse Gamma distributions.
#' @param sample_var a number specifying the observed sample variance
#' @param degf a number specifying the degrees of freedom from the sample
#' variance
#' @import stats
#' @export
update_var_prior <- function(prior, sample_var, degf) {

  # check argument prior
  if (!all(c("w", "shape", "rate") %in% names(prior)))
    stop("Not all prior information defined")
  if (length(prior) != 3)
    stop("prior must contain three elements")
  list_len <- sapply(X = prior, FUN = length)
  if (length(unique(list_len)) > 1)
    stop("Elements of prior must have the same length")
  if (any((prior$w <= 0) | (prior$w > 1)))
    stop("Weights must be between 0 and 1")
  if (any((prior$rate <= 0) | (prior$shape <= 0)))
    stop("Rates and shapes must be positive ")

  # get prior information
  w_prior <- prior$w
  shape_prior <- prior$shape
  rate_prior <- prior$rate

  # update prior
  shape_post <- shape_prior + 0.5 * degf
  rate_post <- rate_prior + 0.5 * degf * sample_var
  if (length(w_prior) > 1) {
    w_post <- w_prior *  gamma(shape_post) / gamma(shape_prior) / (rate_post)^(degf/2) * (rate_prior / rate_post)^shape_prior
    w_post <- w_post / sum(w_post)
  } else {
    w_post <- 1
  }

  list(w = w_post, shape = shape_post, rate = rate_post)
}

