#' @title Power of a t-test
#' @description Power calculation of a one-sided two-sample t-test from the
#' non-central t-distribution. The variances are equal between the groups,
#' the sample sizes must not be equal.
#' @param variance assumed within-group variance
#' @param n total sample size
#' @param alloc numeric vector of sample size proportion within group
#' @param delta assumed treatment effect
#' @param sig_level level of significance
#' @return Power of the one-sided two-sample t-test
#' @import stats
#' @examples
#' power_ttest(variance = 1, alloc = c(0.5, 0.5),
#'             delta = 0.5, sig_level = 0.025, n = 126)
#' @export
power_ttest <- function(variance, n, alloc, delta, sig_level) {

  crit_value <- qt(1 - sig_level, df = n-2)
  ncp <- sqrt(n) * delta / sqrt(variance * sum(1/alloc))
  1 - pt(crit_value, df = n-2, ncp = ncp)
}

