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
