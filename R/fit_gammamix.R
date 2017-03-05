#' @title Fit gamma mixture distribution
#' @description Fits a mixture of gamma distributions to a data set using
#' the EM algorithm.
#' @details This function estimates the parameters of a mixture of Gamma
#' distributions using the EM algorithm described in Almhana et al.
#' The stopping criteria is defined through the absolute difference between the
#' log-likelihood function of two iterations.
#' @param x a numeric vector of data values.
#' @param k a number indicating the number of mixture components.
#' @param max_iter a number indicating the maximum number of iterations of
#' the EM algorithm.
#' @param tol a number determining the convergence criteria. The algorithm
#' stops if the change in the log-likelihood function is smaller than tol.
#' @return
#' \code{fit_gammamix} returns a list with the fitted parameters, the number
#' of iterations, and the value of the log-likelihood function at the fitted
#' parameters. \cr
#' \code{w}, \code{rate}, and \code{shape} are the fitted weight, rate, and shape
#' parameter, respectively. \cr
#' The value of the log-likelihood function and the number of iterations can
#' be accessed through \code{loglikeli} and \code{iter}.
#' @examples
#' set.seed(100)
#' shape <- c(10, 3)
#' rate <- c(14, 3)
#' x <- c(rgamma(290, shape = shape[1], rate = rate[1]),
#'        rgamma(710, shape = shape[2], rate = rate[2]))
#' fit_gammamix(x, k = 2, max_iter = 2500, tol = 1e-5)
#' @references Almhana J, Liu Z, Choulakian V, McGorman R.
#' A recursive algorithm for gamma mixture models.
#' IEEE International Conference on Communications 2006 (ICC 2006) 1:197-202.
#' DOI: 10.1109/ICC.2006.254727.
#' Link: http://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=4024117
#' @import stats
#' @export
fit_gammamix <- function(x, k, max_iter = 2500, tol = 1e-4) {

  if ((k != floor(k)) || (k < 2))
    stop("Number of mixture components must be integer and at least 2")
  if ((max_iter != floor(max_iter)) || (max_iter < 2))
    stop("Number of iterations must be integer and at least 2")
  if (tol <= 0)
    stop("Convercenge criteria must be larger than zero")
  # Starting values
  w <- rep(1/k, times = k)
  rate <- (1:k) * mean(x)/var(x)
  shape <- (1:k) * mean(x)^2/var(x)

  log_likehood <- numeric(max_iter)
  # EM iterations
  for(i in 1:(max_iter+1)) {

    mix_density <- apply(X = cbind(w, shape, rate),
                         MARGIN = 1,
                         FUN = function(y)
                         {
                           y[1]*dgamma(x = x, shape = y[2], rate = y[3])
                         })
    log_likehood[i] <- sum(log(rowSums(mix_density)))

    # Stopping criteria
    if (((i > 1) && (abs(log_likehood[i-1] - log_likehood[i]) < tol )) || (i > max_iter) ) {
      if (i > max_iter) {
        warning("Maximum number of iterations reached. No convergence")
      }
      return(list(w = w, rate = rate,
                  shape = shape,
                  loglikeli = log_likehood[i],
                  iter = i-1))
    }
    # Formula (11) in Almahana et al.
    p_i <-  mix_density / rowSums(mix_density)

    w_old <- w
    rate_old <- rate
    shape_old <- shape

    # Update weights: Formula (12) in Almahana et al.
    w <- colMeans(p_i)
    # Update rates: Formula (13) in Almahana et al.
    rate <- shape * colSums(p_i) / colSums(x * p_i)

    # Derivation of Q from Almahana et al.
    G <- function(shape_j_new, x, rate_j, p_ij) {
      sum((log(x) + log(rate_j) - digamma(shape_j_new)) * p_ij)
    }

    # Update shape by solving Q=0 in shape parameter
    for(j in 1:k) {
      shape[j] <- uniroot(f = G, interval = c(0.01, 10), x = x, rate_j = rate[j],
                          p_ij = p_i[,j], extendInt = "downX", tol = .Machine$double.eps^0.15)$root
    }
  }

}
