context("Weights")

test_that("roll_* do not mutate weights vector", {
  d <- data.frame(w = c(0.2, 0.1, 0.1, 0.05, 0.05))
  roll_sum(1:25, n = length(d$w), weights = d$w)
  expect_identical(d$w, c(0.2, 0.1, 0.1, 0.05, 0.05))
})
