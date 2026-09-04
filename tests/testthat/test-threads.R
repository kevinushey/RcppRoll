context("threads")

# The drivers walk their windows in chunks, restarting the accumulator at
# every chunk boundary, whether or not those chunks then run on threads.
# These inputs are sized well past the chunk size so that a serial run
# crosses boundaries too.

window_reference <- function(x, idx, n, f) {
  vapply(idx, function(i) f(x[i:(i + n - 1L)]), numeric(1))
}

test_that("chunked window walks agree with per-window computation", {

  set.seed(1)
  x <- rnorm(5e4)
  x[sample(length(x), 500)] <- NA

  n <- 100L
  idx <- sort(sample(length(x) - n + 1L, 200L))

  # without fill, the value at position i is the window starting at i,
  # whatever the alignment
  expect_equal(roll_sum(x, n, na.rm = TRUE)[idx],
               window_reference(x, idx, n, function(w) sum(w, na.rm = TRUE)))
  expect_equal(roll_mean(x, n, na.rm = TRUE)[idx],
               window_reference(x, idx, n, function(w) mean(w, na.rm = TRUE)))
  expect_equal(roll_min(x, n, na.rm = TRUE)[idx],
               window_reference(x, idx, n, function(w) min(w, na.rm = TRUE)))
  expect_equal(roll_max(x, n, na.rm = TRUE)[idx],
               window_reference(x, idx, n, function(w) max(w, na.rm = TRUE)))
  expect_equal(roll_median(x, n, na.rm = TRUE)[idx],
               window_reference(x, idx, n, function(w) median(w, na.rm = TRUE)))
  expect_equal(roll_var(x, n, na.rm = TRUE)[idx],
               window_reference(x, idx, n, function(w) var(w, na.rm = TRUE)))
  expect_equal(roll_sd(x, n, na.rm = TRUE)[idx],
               window_reference(x, idx, n, function(w) sd(w, na.rm = TRUE)))

  # partial windows clip at the edges; check those edges along with the rest
  eidx <- c(1L, 2L, idx, length(x) - 1L, length(x))
  left <- (n - 1L) %/% 2L
  right <- n %/% 2L
  expect_equal(
    roll_sum(x, n, partial = TRUE, na.rm = TRUE)[eidx],
    vapply(eidx, function(i) {
      w <- x[max(1L, i - left):min(length(x), i + right)]
      sum(w, na.rm = TRUE)
    }, numeric(1)))

  # the weighted path walks the same chunks
  y <- rnorm(5e4)
  w <- runif(n)
  scaled <- w / sum(w) * n
  expect_equal(roll_sum(y, weights = w)[idx],
               window_reference(y, idx, n, function(v) sum(v * scaled)))

})

test_that("results do not depend on the number of threads", {

  old <- options(RcppRoll.threads = NULL)
  on.exit(options(old), add = TRUE)

  set.seed(42)
  x <- rnorm(5e4)
  x[sample(length(x), 500)] <- NA
  m <- matrix(rnorm(1e5), ncol = 2)
  weights <- runif(100)

  functions <- c("mean", "median", "min", "max", "prod", "sum", "sd", "var")

  for (f in functions) {
    roll <- get(paste("roll", f, sep = "_"), envir = asNamespace("RcppRoll"))

    results <- list()
    for (threads in c(1, 2)) {
      options(RcppRoll.threads = threads)
      results[[threads]] <- list(
        roll(x, 100),
        roll(x, 100, na.rm = TRUE),
        roll(x, 100, fill = NA),
        roll(x, 100, fill = NA, by = 7),
        roll(x, 100, partial = TRUE),
        roll(x, 100, weights = weights, na.rm = TRUE),
        roll(m, 100)
      )
    }

    expect_identical(results[[1]], results[[2]])
  }

})
