context("degenerate sizes")

test_that("a window wider than the data gives an empty result without fill", {

  # integer division used to round the window count back up to one when 'by'
  # exceeded the shortfall, and that window read past the data
  expect_equal(roll_sum(as.numeric(1:5), n = 7, by = 3), numeric(0))
  expect_equal(roll_sum(as.numeric(1:5), n = 7), numeric(0))

  m <- matrix(as.numeric(1:4), nrow = 2)
  expect_equal(dim(roll_sum(m, n = 5, by = 10)), c(0L, 2L))

  # with 'fill' there is one output per input, all filled
  expect_equal(roll_sum(as.numeric(1:5), n = 7, fill = NA), rep(NA_real_, 5))

})

test_that("a window of size zero or less is rejected", {

  expect_error(roll_sum(1:20, n = 0, fill = NA), "positive integer")
  expect_error(roll_sum(1:20, n = -1), "positive integer")
  expect_error(roll_sum(1:20, n = NA_integer_), "positive integer")

  # empty weights used to smuggle in a window of size zero
  expect_error(
    suppressWarnings(roll_sum(1:20, n = 5, weights = numeric(0))),
    "positive integer")

})

test_that("higher-dimensional arrays are rejected rather than flattened", {

  a <- array(as.numeric(1:8), c(2, 2, 2), dimnames = list(NULL, c("a", "b"), NULL))
  expect_error(roll_mean(a, 2), "numeric vector or matrix")

  # a one-dimensional array still rolls as a plain vector
  expect_equal(roll_sum(array(as.numeric(1:5), 5), 2), c(3, 5, 7, 9))

})
