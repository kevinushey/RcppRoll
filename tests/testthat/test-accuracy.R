context("accuracy")

# The rolling routines carry a window's state forward rather than reading the
# window afresh at every point. That is only sound while the arithmetic holds
# its digits, so these check the cases that make it hard: windows spanning
# enormous ranges of magnitude, and windows whose spread is tiny beside their
# mean.

hard_data <- function() {
  set.seed(11)
  list(
    # eighty orders of magnitude within a single window
    extreme = rnorm(200)^100,
    # occasional values that dwarf everything around them, then depart
    spikes  = { v <- rnorm(200); i <- seq(1, 200, by = 13); v[i] <- v[i] * 1e18; v },
    # deviations twelve orders of magnitude below the values themselves
    offset  = rnorm(200, mean = 1e10, sd = 1e-4),
    # totals that cancel to nothing. 2^45 rather than a round decimal so that
    # every partial sum of a window stays a whole number below 2^53, and so is
    # held exactly -- otherwise no summation order is the right one to expect
    cancel  = rep(c(2^45, -2^45, 1, -1), length.out = 200)
  )
}

test_that("rolling sums and means hold their precision on hard data", {

  for (x in hard_data()) {
    for (n in c(2L, 5L, 20L, 50L)) {
      windows <- seq_len(length(x) - n + 1L)
      # summing from the smallest magnitude upwards loses the least
      reference <- vapply(windows, function(i) {
        w <- x[i:(i + n - 1L)]
        sum(w[order(abs(w))])
      }, numeric(1))
      expect_equal(roll_sum(x, n), reference)
      expect_equal(roll_mean(x, n), reference / n)
    }
  }

})

test_that("rolling variances hold their precision on hard data", {

  for (x in hard_data()) {
    for (n in c(2L, 5L, 20L, 50L)) {
      windows <- seq_len(length(x) - n + 1L)
      # variance is shift invariant, so centring first is the same number
      # computed in a well conditioned way
      reference <- vapply(windows, function(i) {
        w <- x[i:(i + n - 1L)]
        var(w - w[1L])
      }, numeric(1))
      expect_equal(roll_var(x, n), reference)
      expect_equal(roll_sd(x, n), sqrt(reference))
    }
  }

})

test_that("a window whose deviations overflow gives an infinite variance", {

  # squaring these passes what a double can hold, as it does for var() itself
  x <- c(1e200, -2e200, 3e200, -1.5e200, 2.5e200)
  expect_equal(roll_var(x, 3), rep(Inf, 3))
  expect_equal(roll_sd(x, 3), rep(Inf, 3))

})

test_that("infinities enter and leave a window without leaving a trace", {

  x <- c(1, Inf, 3, 4, 5, -Inf, 7, 8, 9)

  expect_equal(roll_sum(x, 2), c(Inf, Inf, 7, 9, -Inf, -Inf, 15, 17))
  expect_equal(roll_max(x, 2), c(Inf, Inf, 4, 5, 5, 7, 8, 9))
  expect_equal(roll_min(x, 2), c(1, 3, 3, 4, -Inf, -Inf, 7, 8))

  # a window holding both is a NaN, and the windows after it are not
  expect_true(is.nan(roll_sum(c(Inf, -Inf, 1, 2), 2)[1]))
  expect_equal(roll_sum(c(Inf, -Inf, 1, 2), 2)[3], 3)

})

test_that("a total that overflows recovers once the culprits leave", {

  # every value is finite, but early window totals overflow; subtracting a
  # finite value from that infinity leaves it infinite, so the windows past
  # the overflow have to be rebuilt rather than slid into
  x <- c(rep(1e307, 60), rep(1, 120))
  expect_equal(tail(roll_sum(x, 48), 3), rep(48, 3))
  expect_equal(tail(roll_mean(x, 48), 3), rep(1, 3))

  # likewise for the variance: these squared deviations are individually
  # finite, but their running sum is not
  x <- c(0, rep(1.2e154, 20), rep(1, 60))
  expect_equal(tail(roll_var(x, 12), 3), rep(0, 3))
  expect_equal(tail(roll_sd(x, 12), 3), rep(0, 3))

})

test_that("a NaN keeps its identity through a sum, an NA likewise", {

  # the incremental path counts its missing values rather than adding them
  # up, so the counting has to preserve the distinction the arithmetic did
  for (n in c(10L, 48L)) {
    x <- c(NaN, rnorm(100))
    expect_true(is.nan(roll_sum(x, n)[1]))
    expect_true(is.nan(roll_mean(x, n)[1]))

    x <- c(NA, rnorm(100))
    expect_true(is.na(roll_sum(x, n)[1]) && !is.nan(roll_sum(x, n)[1]))
    expect_true(is.na(roll_mean(x, n)[1]) && !is.nan(roll_mean(x, n)[1]))
  }

})

test_that("min and max agree with base R on the sign of a zero", {

  x <- c(0, -0, 1, -0, 0)
  expect_identical(roll_min(x, 2), vapply(1:4, function(i) min(x[i:(i+1L)]), numeric(1)))
  expect_identical(roll_max(x, 2), vapply(1:4, function(i) max(x[i:(i+1L)]), numeric(1)))

})
