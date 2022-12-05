test_that("power calculation works (n=150)", {
  expect_equal(power_ttest(variance = 1.44,
                           alloc = c(0.5, 0.5),
                           delta = 0.5,
                           sig_level = 0.025,
                           n = 150),
               power.t.test(delta = 0.5, sd = 1.2, sig.level = 0.025,
                            type = "two.sample", alternative = "one.sided",
                            n = 75)$power)
})


test_that("power calculation works (n=274)", {
  expect_equal(power_ttest(variance = 1.44,
                           alloc = c(0.5, 0.5),
                           delta = 0.5,
                           sig_level = 0.1,
                           n = 274),
               power.t.test(delta = 0.5, sd = 1.2, sig.level = 0.1,
                            type = "two.sample", alternative = "one.sided",
                            n = 137)$power)
})
