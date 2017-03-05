#' @title logL_gammamix
#' @description log-likelihood of a mixture of gamma distributions
#' @param x numeric vector of quantile with at least length 2
#' @param w numeric vector of mixture weights
#' @param shape numeric vector of shape parameters
#' @param rate numeric vector of rate parameters
#' @keywords internal
#' @import stats
logL_gammamix <- function(x, w, shape, rate) {

  # Check errors
  vec_lengths <- sort(c(length(w), length(shape), length(rate)))
  if (vec_lengths[1] != vec_lengths[3])
    stop("Weights, shape, and rates do not have the same length")
  if (!identical(sum(w), 1))
    stop("Weights do not sum up to 1")
  if (any(c(shape, rate) <= 0))
    stop("Shape and rate must be positive")


  mix_density <- apply(X = cbind(w, shape, rate),
                       MARGIN = 1,
                       FUN = function(y)
                       {
                         y[1]*dgamma(x = x, shape = y[2], rate = y[3])
                       })

  sum(log(rowSums(mix_density)))
}
