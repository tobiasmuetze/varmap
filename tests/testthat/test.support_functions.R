context("log likelihood of gamma mixture")

test_that("log likelihood returns errors", {
  expect_error(logL_gammamix(x = c(1,2), w = c(0.5, 0.5), rate = 2, shape = 5),
               "Weights, shape, and rates do not have the same length")
  expect_error(logL_gammamix(x = c(1,2), w = c(0.3, 0.5), rate = 2:3, shape = 4:5),
               "Weights do not sum up to 1")
  expect_error(logL_gammamix(x = c(1,2), w = c(0.3, 0.7), rate = c(2, -1), shape = 4:5),
               "Shape and rate must be positive")
  expect_error(logL_gammamix(x = c(1,2), w = c(0.3, 0.7), rate = c(2, 3), shape = c(-4, 5)),
               "Shape and rate must be positive")
})
