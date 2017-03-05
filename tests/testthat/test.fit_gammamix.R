context("EM algorithm for gamma mixture")

test_that("fit_gammamix returns errors", {
  expect_error(fit_gammamix(x = 1:10, k = 1, max_iter = 2500, tol = 1e-4),
               "Number of mixture components must be integer and at least 2")
  expect_error(fit_gammamix(x = 1:10, k = -1, max_iter = 2500, tol = 1e-4),
               "Number of mixture components must be integer and at least 2")
  expect_error(fit_gammamix(x = 1:10, k = 2, max_iter = 0, tol = 1e-4),
               "Number of iterations must be integer and at least 2")
  expect_error(fit_gammamix(x = 1:10, k = 2, max_iter = -1, tol = 1e-4),
               "Number of iterations must be integer and at least 2")
  expect_error(fit_gammamix(x = 1:10, k = 2, max_iter = 5, tol = 0),
               "Convercenge criteria must be larger than zero")
  expect_error(fit_gammamix(x = 1:10, k = 2, max_iter = 5, tol = -1),
               "Convercenge criteria must be larger than zero")
})
