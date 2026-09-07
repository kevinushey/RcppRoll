context("common arithmetic and exceptional fallbacks")

# Center independently before forming weighted moments so the reference does
# not inherit the cancellation error a rounded raw weighted mean could cause.
centered_frequency_variance <- function(x, w) {
  keep <- !is.na(x)
  x <- x[keep]
  w <- w[keep]
  if (length(x) < 2L || sum(w) <= 1) return(NA_real_)
  shifted <- x - x[1L]
  center <- sum(w * shifted) / sum(w)
  sum(w * (shifted - center)^2) / (sum(w) - 1)
}

test_that("ordinary weighted variance retains layout and missing semantics", {
  set.seed(611)
  values <- rnorm(180)
  w <- c(0, runif(19, 0.5, 1.5))
  for (normalize in c(FALSE, TRUE)) {
    weights <- if (normalize) w / sum(w) * length(w) else w
    for (missing in c(FALSE, TRUE)) {
      x <- values
      if (missing) x[c(1, 57, 110)] <- NA_real_
      for (remove in c(FALSE, TRUE)) {
        for (by in c(1L, 3L, 31L)) {
          starts <- seq(1L, length(x) - length(w) + 1L, by = by)
          expected <- vapply(starts, function(i) {
            window <- x[i:(i + length(w) - 1L)]
            if (!remove && anyNA(window)) return(NA_real_)
            centered_frequency_variance(window, weights)
          }, numeric(1))
          expect_equal(roll_var(x, weights = w, by = by, normalize = normalize,
                                na.rm = remove), expected)
          expect_equal(roll_sd(x, weights = w, by = by, normalize = normalize,
                               na.rm = remove), sqrt(expected))
          expect_equal(unname(roll_var(cbind(x, x), weights = w, by = by,
                                       normalize = normalize, na.rm = remove)),
                       unname(cbind(expected, expected)))
          filled <- rep(NA_real_, length(x))
          filled[starts + length(w) - 1L] <- expected
          expect_equal(roll_varr(x, weights = w, by = by, normalize = normalize,
                                 na.rm = remove), filled)
        }
      }
    }
  }
})

test_that("variance bounds preserve small weighted contributions", {
  for (scale in c(1e49, 1e50, 1e51)) {
    for (remove in c(FALSE, TRUE)) {
      expect_equal(roll_var(c(0, scale), weights = c(scale, 1 / scale),
                            normalize = FALSE, na.rm = remove), 1)
      expect_equal(roll_sd(c(0, scale), weights = c(scale, 1 / scale),
                           normalize = FALSE, na.rm = remove), 1)
    }
  }
  # A tiny observation or an infinity sends the call through the scaled path.
  x <- c(1e-60, 1, 2, 3)
  w <- c(1, 2, 3, 4)
  expect_equal(roll_var(x, weights = w, normalize = FALSE),
               centered_frequency_variance(x, w))
  expect_true(is.nan(roll_var(c(Inf, 1, 2), weights = c(1, 2, 3))))
})

test_that("poorly centered wide weighted windows retain their precision", {
  n <- 1000000L
  x <- 1e15 + 1 + sin(seq_len(n + 3L)) * 10
  w <- rep(c(1, 2), length.out = n)
  for (remove in c(FALSE, TRUE)) {
    if (remove) x[2L] <- NA_real_
    expected <- vapply(1:4, function(i)
      centered_frequency_variance(x[i:(i + n - 1L)], w), numeric(1))
    expect_equal(roll_var(x, weights = w, normalize = FALSE, na.rm = remove) /
                   expected, rep(1, 4L))
    expect_equal(roll_sd(x, weights = w, normalize = FALSE, na.rm = remove) /
                   sqrt(expected), rep(1, 4L))
  }
})

test_that("mean strips recover exceptional lanes independently", {
  x <- c(rep(1.25, 20), rep(1e308, 20), rep(-1e308, 20), rep(2.5, 20))
  w <- c(1, 1.5, 0.5)
  expected <- vapply(seq_len(length(x) - 2L), function(i) {
    window <- x[i:(i + 2L)]
    scale <- max(abs(window))
    mean(window / scale * w) * scale
  }, numeric(1))
  for (remove in c(FALSE, TRUE))
    expect_equal(roll_mean(x, weights = w, na.rm = remove) / expected,
                 rep(1, length(expected)))
})

test_that("extrema strips distinguish missing products after the reduction", {
  x <- as.numeric(seq_len(90))
  x[c(4, 25, 50, 74)] <- c(NA_real_, NaN, 0, Inf)
  for (w in list(c(1, 2, 0, Inf), c(1, 2, NaN, 0), c(1, 2, NA, 0))) {
    for (f in list(roll_min, roll_max)) {
      reference <- if (identical(f, roll_min)) min else max
      for (remove in c(FALSE, TRUE)) {
        expected <- vapply(seq_len(length(x) - 3L), function(i) {
          window <- x[i:(i + 3L)]
          # NaN * NA can lose the NA payload on x86. Preserve genuine NAs
          # from either operand before multiplying, as the API requires.
          if (!remove && (any(is.na(window) & !is.nan(window)) ||
                            any(is.na(w) & !is.nan(w))))
            return(NA_real_)
          reference(window * w, na.rm = remove)
        }, numeric(1))
        actual <- f(x, weights = w, normalize = FALSE, na.rm = remove)
        expect_equal(actual, expected)
        expect_identical(is.nan(actual), is.nan(expected))
      }
    }
  }
})

test_that("finite weights preserve extrema types, overflow, and ties", {
  for (x in list(seq.int(-40L, 40L), seq(-40, 40),
                 replace(seq.int(-40L, 40L), c(4L, 29L), NA_integer_),
                 replace(seq(-40, 40), c(4L, 29L), NA_real_),
                 rep(c(-1e308, 1e308, 0), 30))) {
    w <- c(3, -2, 0.5)
    for (f in list(roll_min, roll_max)) {
      reference <- if (identical(f, roll_min)) min else max
      expected <- vapply(seq_len(length(x) - 2L), function(i)
        reference(x[i:(i + 2L)] * w), numeric(1))
      actual <- f(x, weights = w, normalize = FALSE)
      expect_equal(actual, expected)
      expect_identical(is.nan(actual), is.nan(expected))
    }
  }
  x <- rep(c(0, -0, 0, -0), 20)
  w <- c(1, 2, -1)
  products <- lapply(seq_len(length(x) - 2L), function(i) x[i:(i + 2L)] * w)
  # Min keeps the first equal value, and max keeps the last without na.rm.
  expect_identical(1 / roll_min(x, weights = w, normalize = FALSE),
                   1 / vapply(products, function(z) z[1L], numeric(1)))
  expect_identical(1 / roll_max(x, weights = w, normalize = FALSE),
                   1 / vapply(products, function(z) z[3L], numeric(1)))
})

test_that("ordinary and exceptional regions retain their window results", {
  set.seed(613)
  x <- rnorm(50000)
  special <- c(10001L, 20003L, 30007L, 40009L)
  x[special] <- c(NA_real_, NaN, 1e200, -1e200)
  w <- runif(20, 0.5, 1.5)
  starts <- sort(unique(c(sample.int(length(x) - 19L, 200L),
                          unlist(lapply(special, function(i) (i - 19L):i)))))
  old <- options(RcppRoll.threads = 1L)
  on.exit(options(old), add = TRUE)
  for (remove in c(FALSE, TRUE)) {
    for (weighted in c(FALSE, TRUE)) {
      weights <- if (weighted) w else rep(1, 20)
      expected <- vapply(starts, function(i) {
        window <- x[i:(i + 19L)]
        if (!remove && anyNA(window)) return(NA_real_)
        centered_frequency_variance(window, weights)
      }, numeric(1))
      args <- list(x = x, n = 20L, na.rm = remove, normalize = FALSE)
      if (weighted) args$weights <- w
      options(RcppRoll.threads = 1L)
      actual <- do.call(roll_var, args)
      expect_equal(actual[starts], expected)
      options(RcppRoll.threads = 2L)
      expect_identical(do.call(roll_var, args), actual)
    }
    for (f in list(roll_min, roll_max)) {
      reference <- if (identical(f, roll_min)) min else max
      expected <- vapply(starts, function(i)
        reference(x[i:(i + 19L)] * w, na.rm = remove), numeric(1))
      options(RcppRoll.threads = 1L)
      actual <- f(x, weights = w, normalize = FALSE, na.rm = remove)
      expect_equal(actual[starts], expected)
      expect_identical(is.nan(actual[starts]), is.nan(expected))
      options(RcppRoll.threads = 2L)
      expect_identical(f(x, weights = w, normalize = FALSE, na.rm = remove),
                       actual)
    }
  }
})
