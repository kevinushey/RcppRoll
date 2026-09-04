context("Weights")

test_that("roll_* do not mutate weights vector", {
  d <- data.frame(w = c(0.2, 0.1, 0.1, 0.05, 0.05))
  roll_sum(1:25, n = length(d$w), weights = d$w)
  expect_identical(d$w, c(0.2, 0.1, 0.1, 0.05, 0.05))
})

test_that("weighted roll_mean matches weighted.mean over zoo's windows (#23)", {

  if (!requireNamespace("zoo", quietly = TRUE))
    skip("zoo not installed")

  # NOTE: zoo::rollapply() builds variable-width windows when handed a vector as
  # its 'width', so it is not a direct analogue for our 'weights'. Instead, we
  # let zoo generate the windows and use weighted.mean() as the reference.
  reference <- function(x, n, weights, align, na.rm) {
    fill <- if (align == "center") NULL else NA
    zoo::rollapply(
      x, n,
      function(window) weighted.mean(window, weights, na.rm = na.rm),
      align = align, fill = fill
    )
  }

  set.seed(123)
  x <- rnorm(50)
  x[c(4, 5, 6, 7, 20, 21, 33)] <- NA

  weights <- list(c(1, 3, 3, 1), c(2, 1, 4), c(1, 2, 3, 2, 1), 5)
  for (w in weights) {
    n <- length(w)
    for (align in c("left", "center", "right")) {
      for (na.rm in c(TRUE, FALSE)) {
        expect_equal(
          roll_mean(x, n, weights = w, align = align, na.rm = na.rm,
                    fill = if (align == "center") numeric() else NA),
          as.numeric(reference(x, n, w, align, na.rm))
        )
      }
    }
  }

})

test_that("weighted roll_mean re-normalizes weights after dropping NAs (#23)", {
  tmp <- c(1, 1, 1, 1, NA, NA, NA, NA, 1, 1)
  expect_equal(
    roll_mean(tmp, 4, c(1, 3, 3, 1), na.rm = TRUE),
    c(1, 1, 1, 1, NaN, 1, 1)
  )
  # an all-NA window gives NaN, as in the unweighted case
  expect_equal(
    roll_mean(tmp, 4, c(1, 3, 3, 1), na.rm = TRUE),
    roll_mean(tmp, 4, na.rm = TRUE)
  )
})

test_that("weighted roll_median with equal weights matches zoo::rollapply", {

  if (!requireNamespace("zoo", quietly = TRUE))
    skip("zoo not installed")

  # NOTE: only for odd-sized windows of non-NA data -- a weighted median selects
  # an observation rather than interpolating between the two middle values.
  set.seed(456)
  x <- rnorm(50)

  for (n in c(1L, 3L, 5L, 7L)) {
    expect_equal(
      roll_medianr(x, n, weights = rep(1, n)),
      as.numeric(zoo::rollapply(x, n, median, align = "right", fill = NA))
    )
    expect_equal(
      roll_medianr(x, n, weights = rep(1, n)),
      roll_medianr(x, n)
    )
  }

})

test_that("weighted roll_median pairs weights with their own values", {

  if (!requireNamespace("zoo", quietly = TRUE))
    skip("zoo not installed")

  reference <- function(window, weights) {
    keep <- !is.na(window)
    window <- window[keep]
    weights <- weights[keep]
    if (!length(window)) return(NA_real_)
    ordered <- order(window)
    window <- window[ordered]
    weights <- weights[ordered]
    window[which(cumsum(weights) >= sum(weights) / 2)[1]]
  }

  set.seed(789)
  x <- rnorm(50)
  x[c(3, 11, 12, 13, 14, 30)] <- NA

  for (w in list(c(1, 2, 3, 2, 1), c(10, 1, 1), c(1, 1, 10))) {
    n <- length(w)
    for (align in c("left", "center", "right")) {
      expect_equal(
        roll_median(x, n, weights = w, align = align, na.rm = TRUE,
                    fill = if (align == "center") numeric() else NA),
        as.numeric(zoo::rollapply(
          x, n, function(window) reference(window, w),
          align = align, fill = if (align == "center") NULL else NA
        ))
      )
    }
  }

  # the weight has to follow its own value through the sort
  expect_equal(roll_median(c(1, 2, 3), 3, weights = c(1, 1, 10)), 3)
  expect_equal(roll_median(c(3, 2, 1), 3, weights = c(10, 1, 1)), 3)

})

test_that("uniform weights take the unweighted path exactly", {

  # any uniform weight vector normalizes to exactly one in real arithmetic, so
  # these agree bitwise with the unweighted call -- including window sizes
  # whose rescaling rounds away from one in floating point (e.g. 1/49 * 49)
  set.seed(321)
  x <- rnorm(200)
  x[c(5, 20, 21, 100)] <- NA

  ops <- list(roll_sum, roll_mean, roll_min, roll_max, roll_prod, roll_var, roll_sd)
  for (op in ops) {
    for (n in c(3L, 6L, 49L, 64L)) {
      for (na.rm in c(TRUE, FALSE)) {
        expect_identical(op(x, n, weights = rep(1, n), na.rm = na.rm),
                         op(x, n, na.rm = na.rm))
        expect_identical(op(x, n, weights = rep(2.5, n), na.rm = na.rm),
                         op(x, n, na.rm = na.rm))
      }
    }
  }

  # without 'normalize', only a vector of ones is the unweighted call
  expect_identical(roll_sum(x, 8, weights = rep(1, 8), normalize = FALSE),
                   roll_sum(x, 8))
  expect_equal(roll_sum(x[101:108], 8, weights = rep(2, 8), normalize = FALSE),
               2 * sum(x[101:108]))

})

test_that("uniform weights route the weighted median to its lower form", {

  # a weighted median selects an observation: on an even window, the lower of
  # the two middle values, where the unweighted median averages them
  x <- c(1, 2, 3, 4, 5)
  expect_equal(roll_median(x, 4, weights = rep(1, 4)), c(2, 3))
  expect_equal(roll_median(x, 4), c(2.5, 3.5))

  lower_median <- function(window) {
    window <- window[!is.na(window)]
    if (!length(window)) return(NA_real_)
    sort(window)[(length(window) + 1) %/% 2]
  }

  # the routed path agrees with the reference on either side of the
  # incremental crossover, NAs and even windows included
  set.seed(987)
  y <- rnorm(300)
  y[sample(300, 30)] <- NA

  for (n in c(4L, 64L)) {
    windows <- seq_len(length(y) - n + 1L)
    expect_equal(
      roll_median(y, n, weights = rep(1, n), na.rm = TRUE),
      vapply(windows, function(i) lower_median(y[i:(i + n - 1L)]), numeric(1)))
  }

  # a 'by' past the crossover takes the from-scratch path through the same form
  n <- 8L
  starts <- seq(1L, length(y) - n + 1L, by = 7L)
  expect_equal(
    roll_median(y, n, weights = rep(1, n), by = 7, na.rm = TRUE),
    vapply(starts, function(i) lower_median(y[i:(i + n - 1L)]), numeric(1)))

})

test_that("the weighted median selection matches a sort-and-scan reference", {

  reference <- function(window, weights) {
    keep <- !is.na(window)
    window <- window[keep]
    weights <- weights[keep]
    if (!length(window)) return(NA_real_)
    ordered <- order(window)
    window <- window[ordered]
    weights <- weights[ordered]
    window[which(cumsum(weights) >= sum(weights) / 2)[1]]
  }

  rolled <- function(x, w) {
    n <- length(w)
    windows <- seq_len(length(x) - n + 1L)
    vapply(windows, function(i) reference(x[i:(i + n - 1L)], w), numeric(1))
  }

  # windows large enough to take the descent through several partitions;
  # continuous weights, so no crossing lands on a knife edge
  set.seed(654)
  x <- rnorm(400)
  x[sample(400, 25)] <- NA
  w <- runif(37)

  expect_equal(
    roll_median(x, 37, weights = w, na.rm = TRUE, normalize = FALSE),
    rolled(x, w))

  # repeated values exercise the pivot's whole-run partitioning
  y <- sample(round(rnorm(400), 1))
  expect_equal(
    roll_median(y, 37, weights = w, normalize = FALSE),
    rolled(y, w))

})

test_that("negative weights keep the sorted scan's crossing", {

  # a negative weight makes the cumulative weight non-monotonic, which the
  # partition-based selection cannot follow; those windows take the scan
  expect_equal(
    roll_median(c(5, 1, 3), 3, weights = c(1, -1, 2), normalize = FALSE), 3)
  expect_equal(
    roll_median(c(1, 2, 3), 3, weights = c(3, -2, 1), normalize = FALSE), 1)

  # an all-zero weight total still has no crossing to find
  expect_equal(
    roll_median(c(1, 2, 3), 3, weights = c(0, 0, 0), normalize = FALSE),
    NA_real_)

})

test_that("weighted roll_median respects na.rm", {
  x <- c(1, 2, NA, 4, 5, 6, 7)
  expect_equal(roll_median(x, 3, weights = c(1, 2, 1), na.rm = TRUE),
               c(2, 2, 4, 5, 6))
  expect_equal(roll_median(x, 3, weights = c(1, 2, 1), na.rm = FALSE),
               c(NA, NA, NA, 5, 6))
  # an all-NA window gives NA rather than reading past the window
  expect_equal(roll_median(rep(NA_real_, 4), 2, weights = c(1, 1), na.rm = TRUE),
               rep(NA_real_, 3))
})
