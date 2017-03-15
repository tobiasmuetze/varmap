#' @title MAP approach for variances
#' @description Performs a random effects meta-analysis for variances.
#' @details The function \code{varmeta} performs a random effect meta-analysis
#' for variance.
#' The model is as proposed by Schmidli et al., i.e. the log-variance are
#' modeled using a normal hierarchical model with normal prior of the mean
#' and a half-normal prior for the (between-trial) standard deviation.
#' @param sample_var numeric vector with the sample variances from historical
#' clinical trials
#' @param degf numeric vector with the degrees of freedom of the sample variances
#' @param tau_sd standard deviation parameter of the half-normal prior for the
#' between-study variability
#' @param mu_mean mean of the normal prior for the mean
#' @param n.iter number of iterations to monitor in the R function \code{coda.samples}
#' @param ... Additional arguments for the R functions \code{coda.samples} and
#' \code{jags.model}
#' @return An \code{mcmc.list} object with the posterior predictive for the variance
#' (\code{map_variance}), the posterior for the mean of the log variance (\code{mu}),
#' and the between-trial standard deviation of the log-variance (\code{tau})
#' @references Schmidli H, Neuenschwander B, Friede T.
#' Meta-analytic-predictive use of historical variance data for the design and
#' analysis of clinical trials.
#' Computational Statistics & Data Analysis (2016).
#' @import stats rjags
#' @export
varmeta <- function(sample_var, degf, tau_sd, mu_mean, n.iter = 10000, ...) {

  # Additional arguments
  add_args <- list(...)
  if (is.null(n.chains <- add_args$n.chains)) n.chains <- 1
  if (is.null(n.adapt <- add_args$n.adapt)) n.adapt <- 1000
  if (is.null(quiet <- add_args$quiet)) quiet <- FALSE
  if (tau_sd <= 0) stop("tau_sd must be positive")

  # meta-analytic-predictive (MAP) jags code
  modelstring_1 <- "
    model {
      for (j in 1:J) {
      s2[j] ~ dgamma(rate[j], shape[j])
      rate[j] <- 0.5 * nu[j]
      shape[j] <- 0.5 * nu[j] / sigma2[j]
      sigma2[j] <- exp(theta[j])
      theta[j] ~ dnorm(mu, inv_tau2)
    }

    tau ~ dnorm(0, 1/"
  modelstring_2 <-  ")I(0,)
    inv_tau2 <- pow(tau,-2)
    mu ~ dnorm("
  modelstring_3 <- ", 0.0001)
    theta_pred ~ dnorm(mu, inv_tau2)
    map_variance <- exp(theta_pred)
  }"

  modelstring <- paste0(modelstring_1, tau_sd, modelstring_2, mu_mean, modelstring_3)

  # MAP analysis for variances
  model_spec <- textConnection(modelstring)
  jags <- jags.model(file = model_spec,
                     data = list(s2 = sample_var, nu = degf, J = length(degf)),
                     n.chains = n.chains,
                     n.adapt = n.adapt,
                     quiet = quiet)



  out <- coda.samples(jags, variable.names = c("map_variance", "mu", "tau"), n.iter = n.iter, ...)
  out
}
