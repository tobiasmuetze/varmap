#' @title The mixture of inverse gamma distributions
#' @description Quantiles of a mixture of invserse gamma distributions
#' @param p vector of probabilities
#' @param w weights which sum up to one
#' @param shape shape parameter
#' @param rate rate parameter
#' @examples
#' n <- c(800000, 400000)
#' x <- c(rgamma(n[1], shape = 3, rate = 2), rgamma(n[2], shape = 5, rate = 4))
#' quantile(1/x, probs = 0.75)
#' qmixinvgamma(p = 0.75, w = n/sum(n), shape = c(3, 5), rate = c(2,4))
#' @import stats
#' @export
qmixinvgamma <- function(p, w, shape, rate) {

  if ((length(w) != length(shape)) || (length(w) != length(rate))) {
    stop("w, shape, rate must have same length")
  }
  if (abs(sum(w)- 1) >= 1e-14) {
    stop("w must sum up to 1")
  }

  cdf <- function(upper_bound) {
    out <- integrate(f = dmixinvgamma, w = w, shape = shape, rate = rate,
                     lower = 0,
                     upper = upper_bound)
    out$value
  }

  f <- function(x){ cdf(upper_bound = x) - p}

  root_upper_bound <- max(1/qgamma(p = 0.00001, shape = shape, rate = rate))
  uniroot(f = f, lower = 0, upper = root_upper_bound, extendInt = "upX")$root
}
