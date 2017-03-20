## ------------------------------------------------------------------------
library(varmap)
prior <- list(w = c(0.194, 0.806), shape = c(5.33, 18.19), rate = c(167.55, 691.14))
posterior <- update_var_prior(prior = prior, sample_var = 35, degf = 58)
posterior

## ------------------------------------------------------------------------
post_mean <- posterior$w %*% (posterior$rate / (posterior$shape - 1))
post_median <- qmixinvgamma(p = 0.5, w = posterior$w, shape = posterior$shape,
                            rate = posterior$rate)

