#' @title Sample size of a t-test
#' @description Sample size calculation of a one-sided two-sample t-test from
#' the non-central t-distribution. The variances are equal between the groups,
#' the sample sizes must not be equal.
#' @param variance assumed within-group variance
#' @param alloc numeric vector of sample size proportion within group
#' @param delta assumed treatment effect
#' @param sig_level level of significance
#' @param power target power
#' @return Total sample size of the one-sided two-sample t-test
#' @import stats
#' @examples
#' n_ttest(variance = 1, alloc = c(0.5, 0.5), delta = 0.5,
#'         sig_level = 0.025, power = 0.8)
#' @export
n_ttest <- function(variance, alloc, delta, sig_level, power) {

  p.body <- quote({power_ttest(variance = variance,
                               n = sum(round(n*alloc)),
                               alloc = round(n*alloc) / sum(round(n*alloc)),
                               delta = delta,
                               sig_level = sig_level)})

  n <- uniroot(function(n) eval(p.body) - power, c(6, 1e+07),
               tol = 1e-8, extendInt = "upX")$root
  n <- sum(round(n*alloc))
  n
}
