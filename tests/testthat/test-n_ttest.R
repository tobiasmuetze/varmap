test_that("sample size calculation works (power=95%)", {
  expect_equal(n_ttest(variance = 1.44,
                       alloc = c(0.5, 0.5),
                       delta = 0.5,
                       sig_level = 0.025,
                       power = 0.95),
               2*round(power.t.test(delta = 0.5, sd = 1.2, sig.level = 0.025,
                                    type = "two.sample", alternative = "one.sided",
                                    power = 0.95)$n))
})

test_that("sample size calculation works (power=80%)", {
  expect_equal(n_ttest(variance = 1.44,
                       alloc = c(0.5, 0.5),
                       delta = 0.5,
                       sig_level = 0.025,
                       power = 0.8),
               2*round(power.t.test(delta = 0.5, sd = 1.2, sig.level = 0.025,
                                    type = "two.sample", alternative = "one.sided",
                                    power = 0.8)$n))
})

test_that("sample size calculation works (power=50%)", {
  expect_equal(n_ttest(variance = 1.44,
                       alloc = c(0.5, 0.5),
                       delta = 0.5,
                       sig_level = 0.025,
                       power = 0.5),
               2*round(power.t.test(delta = 0.5, sd = 1.2, sig.level = 0.025,
                                    type = "two.sample", alternative = "one.sided",
                                    power = 0.5)$n))
})

test_that("sample size calculation works (alpha=10%)", {
  expect_equal(n_ttest(variance = 1.44,
                       alloc = c(0.5, 0.5),
                       delta = 0.5,
                       sig_level = 0.1,
                       power = 0.9),
               2*round(power.t.test(delta = 0.5, sd = 1.2, sig.level = 0.1,
                                    type = "two.sample", alternative = "one.sided",
                                    power = 0.9)$n))
})
