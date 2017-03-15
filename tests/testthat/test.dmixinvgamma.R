context("density of mixture of inverse gamma distributions")

test_that("mixture density returns errors", {
  expect_error(dmixinvgamma(1:10, w = c(0.8, 0.3), shape = c(3, 5), rate = c(2,4)),
               "w must sum up to 1")
  expect_error(dmixinvgamma(1:10, w = c(0.2, 0.8), shape = 5, rate = c(2,4)),
               "w, shape, rate must have same length")
  expect_error(dmixinvgamma(1:10, w = c(0.2, 0.8), shape = 4:5, rate = 4),
               "w, shape, rate must have same length")
})
