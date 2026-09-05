context("var and sd")

test_that("direct variance preserves missing-value semantics at every position", {
  for (missing in c(NA_real_, NaN)) {
    for (position in seq_len(12L)) {
      x <- as.numeric(seq_len(12L))
      x[position] <- missing
      for (roll in list(roll_var, roll_sd)) {
        value <- roll(x, 12L)
        expect_true(is.na(value) && !is.nan(value))
        value <- roll(x, weights = seq_len(12L))
        expect_true(is.na(value) && !is.nan(value))
      }
      expect_equal(roll_var(x, 12L, na.rm = TRUE), var(x, na.rm = TRUE))
      expect_equal(roll_sd(x, 12L, na.rm = TRUE), sd(x, na.rm = TRUE))
    }
  }
  # An infinity before the missing value must not turn the NA answer into NaN.
  expect_identical(roll_var(c(Inf, NA_real_, 1), 3L), NA_real_)
  expect_identical(roll_sd(c(Inf, NaN, 1), weights = c(1, 2, 3)), NA_real_)
})

# frequency weights: m = sum(w * x) / sum(w), s2 = sum(w * (x - m)^2) / (sum(w) - 1)
reference_var <- function(window, weights) {
  keep <- !is.na(window)
  window <- window[keep]
  weights <- weights[keep]
  if (length(window) < 2 || !(sum(weights) > 1)) return(NA_real_)
  m <- sum(weights * window) / sum(weights)
  sum(weights * (window - m)^2) / (sum(weights) - 1)
}

# 'normalize = TRUE' scales the weights to sum to n before they reach C++
normalize <- function(weights, n) weights / sum(weights) * n

test_that("weighted roll_var uses a weighted variance, not var(x * weights) (#47)", {

  x <- c(1, 2, 3, 4, 5, 6)
  w <- c(1, 3, 1)

  expect_equal(
    roll_var(x, 3, weights = w),
    vapply(1:4, function(i) reference_var(x[i:(i + 2)], normalize(w, 3)), numeric(1))
  )

  # the old behaviour was var(window * weights), which is a different number
  expect_false(isTRUE(all.equal(
    roll_var(x, 3, weights = w)[1],
    var(x[1:3] * normalize(w, 3))
  )))

})

test_that("equal weights reduce to the unweighted variance (#47)", {

  if (!requireNamespace("zoo", quietly = TRUE))
    skip("zoo not installed")

  set.seed(99)
  x <- rnorm(30)

  for (n in c(2L, 3L, 5L, 8L)) {
    expect_equal(roll_var(x, n, weights = rep(1, n)), roll_var(x, n))
    expect_equal(roll_sd(x, n, weights = rep(1, n)), roll_sd(x, n))
    expect_equal(roll_var(x, n), as.numeric(zoo::rollapply(x, n, var)))
    expect_equal(roll_sd(x, n), as.numeric(zoo::rollapply(x, n, sd)))
  }

})

test_that("weighted roll_var keeps each weight with its own value under na.rm (#47)", {

  set.seed(101)

  for (trial in 1:50) {
    n <- sample(2:6, 1)
    x <- round(rnorm(n + sample(0:8, 1)), 3)
    x[sample(seq_along(x), sample(0:length(x), 1))] <- NA
    w <- round(runif(n, 0.2, 4), 3)

    expect_equal(
      as.numeric(roll_var(x, n, weights = w, na.rm = TRUE)),
      vapply(seq_len(length(x) - n + 1),
             function(i) reference_var(x[i:(i + n - 1)], normalize(w, n)),
             numeric(1))
    )
  }

  # a window that drops its first value must not shift the weights along with it
  y <- c(NA, 10, 20, 30)
  w <- normalize(c(1, 1, 5), 3)
  expect_equal(roll_var(y, 3, weights = c(1, 1, 5), na.rm = TRUE)[1],
               reference_var(c(10, 20), w[2:3]))

})

test_that("roll_var and roll_sd give NA for windows with fewer than two values (#47)", {

  # var() is NA for a vector of length 0 or 1; these used to be 0 or NaN
  expect_equal(roll_var(rep(NA_real_, 4), 3, na.rm = TRUE), rep(NA_real_, 2))
  expect_equal(roll_sd(rep(NA_real_, 4), 3, na.rm = TRUE), rep(NA_real_, 2))
  expect_equal(roll_var(c(NA, NA, 5, NA), 3, na.rm = TRUE), rep(NA_real_, 2))
  expect_equal(roll_var(1:5, 1), rep(NA_real_, 5))

  expect_equal(roll_var(rep(NA_real_, 4), 3, weights = c(1, 1, 1), na.rm = TRUE),
               rep(NA_real_, 2))

  # NA, not NaN
  expect_false(any(is.nan(roll_var(rep(NA_real_, 4), 3, na.rm = TRUE))))

})

test_that("unweighted roll_var matches zoo under both na.rm settings", {

  if (!requireNamespace("zoo", quietly = TRUE))
    skip("zoo not installed")

  set.seed(202)

  for (trial in 1:50) {
    len <- sample(6:30, 1)
    n <- sample(2:5, 1)
    x <- rnorm(len)
    x[sample(len, sample(0:(len %/% 2), 1))] <- NA

    expect_equal(
      roll_var(x, n, na.rm = TRUE),
      as.numeric(zoo::rollapply(x, n, function(w) {
        w <- w[!is.na(w)]
        if (length(w) < 2) NA_real_ else var(w)
      }))
    )

    expect_equal(
      roll_var(x, n, na.rm = FALSE),
      as.numeric(zoo::rollapply(x, n, function(w) if (anyNA(w)) NA_real_ else var(w)))
    )
  }

})
