context("n and weights")

test_that("supplying both 'n' and 'weights' with differing lengths warns (#39)", {

  # 'weights' wins, so a mismatched 'n' is silently discarded without this
  expect_warning(roll_sum(1:9, n = 2, weights = 1), "'n' is ignored")
  expect_warning(roll_sum(1:9, 2, 1), "'n' is ignored")
  expect_warning(roll_sumr(1:5, n = 3, weights = c(1, 1)), "'n' is ignored")
  expect_warning(roll_meanl(1:5, n = 3, weights = c(1, 1)), "'n' is ignored")
  expect_warning(roll_var(1:6, n = 9, weights = c(1, 1, 1)), "'n' is ignored")

  # the message reports both the window used and the one asked for
  expect_warning(roll_sum(1:9, n = 2, weights = 1),
                 "using 'n = 1' rather than 'n = 2'")

})

test_that("no warning when 'n' and 'weights' agree, or only one is given", {

  expect_silent(roll_sum(1:9, n = 2, weights = c(1, 1)))
  expect_silent(roll_sum(1:9, weights = c(1, 1)))
  expect_silent(roll_sum(1:9, n = 2))
  expect_silent(roll_sum(1:9))
  expect_silent(roll_meanr(1:9, n = 3, weights = c(1, 2, 3)))

})

test_that("the warning does not change the computed result (#39)", {

  expect_equal(
    suppressWarnings(roll_sum(1:9, n = 2, weights = 1)),
    roll_sum(1:9, n = 1, weights = 1)
  )

  # still the length-of-weights window, as before
  expect_equal(
    suppressWarnings(roll_sumr(1:5, n = 3, weights = c(1, 1))),
    roll_sumr(1:5, n = 2, weights = c(1, 1))
  )

})
