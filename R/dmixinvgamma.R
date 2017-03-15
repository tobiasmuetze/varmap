#' @title The mixture of inverse gamma distributions
#' @description Density of a mixture of invserse gamma distributions
#' @param x vector of quantiles
#' @param w weights which sum up to one
#' @param shape shape parameter
#' @param rate rate parameter
#' @examples
#' n <- c(8000, 4000)
#' x <- c(rgamma(n[1], shape = 3, rate = 2), rgamma(n[2], shape = 5, rate = 4))
#' hist(1/x, breaks = 1000, freq = FALSE, xlim = c(0, 4))
#' x_plot <- seq(0.001, 10, length.out = 1000)
#' y_plot <- dmixinvgamma(x_plot, w = n/sum(n), shape = c(3, 5), rate = c(2,4))
#' points(x_plot, y_plot, type = "l")
#' @import stats
#' @export
dmixinvgamma <- function(x, w, shape, rate) {

  if ((length(w) != length(shape)) || (length(w) != length(rate))) {
    stop("w, shape, rate must have same length")
  }
  if (abs(sum(w)- 1) >= .Machine$double.eps^0.5) {
    stop("w must sum up to 1")
  }
  dens <- numeric(length(x))

  # density of gamma: f; density of inv gamma: g
  # g(x) = f(1/x) / x^2
  dens <- apply(X = cbind(w, shape, rate),
                MARGIN = 1,
                FUN = function(y)
                {
                  y[1] * dgamma(x = 1/x, shape = y[2], rate = y[3]) / x^2
                })
  dens <- rowSums(dens)
  dens[x == 0] <- 0
  dens
}
