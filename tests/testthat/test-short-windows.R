context("few output windows")

test_that("medians with few outputs agree with independent window selection", {
  set.seed(61)
  for (n in c(2L, 97L, 192L, 1000L)) {
    for (outputs in c(1L, 4L, 5L)) {
      x <- rnorm(n + outputs - 1L)
      reference <- vapply(seq_len(outputs), function(i)
        median(x[i:(i + n - 1L)]), numeric(1))
      expect_equal(roll_median(x, n), reference)
      expect_equal(roll_median(x, n, by = 2), reference[seq(1L, outputs, 2L)])
      expect_equal(roll_medianr(x, n), c(rep(NA_real_, n - 1L), reference))
      expect_equal(roll_medianl(x, n), c(reference, rep(NA_real_, n - 1L)))
      expect_equal(unname(roll_median(cbind(x, x), n)),
                   unname(cbind(reference, reference)))

      # Uniform weighted medians select the lower middle value, even when
      # ordinary medians average the two. Exercise both selection paths.
      lower <- vapply(seq_len(outputs), function(i)
        sort(x[i:(i + n - 1L)])[ceiling(n / 2)], numeric(1))
      expect_equal(roll_median(x, weights = rep(1, n)), lower)

      x[c(1L, length(x))] <- NA_real_
      reference <- vapply(seq_len(outputs), function(i)
        median(x[i:(i + n - 1L)], na.rm = TRUE), numeric(1))
      expect_equal(roll_median(x, n, na.rm = TRUE), reference)
    }
  }
})

test_that("few partial median windows retain alignment and skipped points", {
  x <- c(5, NA, 1, 9)
  for (align in c("left", "center", "right")) {
    n <- 7L
    left <- switch(align, left = 0L, center = 3L, right = 6L)
    reference <- vapply(seq_along(x), function(i)
      median(x[max(1L, i - left):min(length(x), i + n - left - 1L)],
             na.rm = TRUE), numeric(1))
    expect_equal(roll_median(x, n, align = align, partial = TRUE, na.rm = TRUE),
                 reference)
    reference[c(2L, 4L)] <- NA_real_
    expect_equal(roll_median(x, n, by = 2, align = align,
                            partial = TRUE, na.rm = TRUE), reference)
  }
})

test_that("few output sums and means retain compensated accumulation", {
  # A direct sum discards the ones between these cancelling endpoints.
  n <- 1000L
  for (outputs in c(1L, 4L, 16L, 17L)) {
    x <- c(1e16, rep(1, n - 2L), -1e16, rep(0, outputs - 1L))
    for (na.rm in c(FALSE, TRUE)) {
      expect_identical(roll_sum(x, n, na.rm = na.rm)[1L], n - 2)
      expect_identical(roll_mean(x, n, na.rm = na.rm)[1L], (n - 2) / n)
    }
  }
})

test_that("few output variances and products agree with independent windows", {
  set.seed(62)
  ops <- list(var = roll_var, sd = roll_sd, prod = roll_prod)
  references <- list(var = var, sd = sd, prod = prod)
  for (outputs in c(1L, 2L, 4L, 5L, 8L, 9L, 12L, 16L, 17L)) {
    for (by in c(1L, 3L)) {
      n <- 1000L
      x <- 1 + rnorm(n + (outputs - 1L) * by) * 1e-4
      for (missing in c(FALSE, TRUE)) {
        if (missing) x[c(1L, n, length(x))] <- NA_real_
        starts <- seq.int(1L, length(x) - n + 1L, by = by)
        for (name in names(ops)) {
          f <- ops[[name]]
          reference <- references[[name]]
          for (na.rm in c(FALSE, TRUE)) {
            expected <- vapply(starts, function(i)
              reference(x[i:(i + n - 1L)], na.rm = na.rm), numeric(1))
            expect_equal(f(x, n, by = by, na.rm = na.rm), expected)
            expect_equal(unname(f(cbind(x, x), n, by = by, na.rm = na.rm)),
                         unname(cbind(expected, expected)))
            # Filled output counts must count computed windows, not padding.
            for (align in c("left", "center", "right")) {
              left <- switch(align, left = 0L, center = (n - 1L) %/% 2L,
                             right = n - 1L)
              filled <- rep(NA_real_, length(x))
              filled[starts + left] <- expected
              expect_equal(f(x, n, by = by, align = align, fill = NA,
                             na.rm = na.rm), filled)
            }
          }
        }
      }
    }
  }
})

test_that("few output variance retains precision around large offsets", {
  for (n in c(1000L, 100000L)) {
    x <- 1e10 + sin(seq_len(n + 16L)) * 1e-4
    for (outputs in c(1L, 4L, 16L, 17L)) {
      y <- x[seq_len(n + outputs - 1L)]
      expected <- vapply(seq_len(outputs), function(i) {
        w <- y[i:(i + n - 1L)]
        var(w - w[1L])
      }, numeric(1))
      # Compare relative errors: an absolute tolerance would mask the entire
      # tiny variance of these observations.
      expect_equal(roll_var(y, n) / expected, rep(1, outputs))
      expect_equal(roll_sd(y, n) / sqrt(expected), rep(1, outputs))
    }
  }
})

test_that("a few wide variance windows keep a well-conditioned center", {
  n <- 1000000L
  x <- 1e15 + 1 + sin(seq_len(n + 3L)) * 10
  expected <- vapply(1:4, function(i) {
    w <- x[i:(i + n - 1L)]
    var(w - w[1L])
  }, numeric(1))
  # Ordinary summation puts the center far enough away to lose about 2e-5
  # relative accuracy even after the direct kernel's residual correction.
  for (na.rm in c(FALSE, TRUE)) {
    expect_equal(roll_var(x, n, na.rm = na.rm) / expected, rep(1, 4L))
    expect_equal(roll_sd(x, n, na.rm = na.rm) / sqrt(expected), rep(1, 4L))
    expect_equal(roll_var(x[seq_len(n)], n, na.rm = na.rm) / expected[1L], 1)
  }
})

test_that("few partial product windows retain alignment and missing identities", {
  x <- c(1.01, NA, 0.99, 1, NaN, rep(1.001, 11L))
  n <- 1000L
  for (align in c("left", "center", "right")) {
    left <- switch(align, left = 0L, center = (n - 1L) %/% 2L, right = n - 1L)
    for (by in c(1L, 3L)) {
      for (na.rm in c(FALSE, TRUE)) {
        expected <- rep(NA_real_, length(x))
        for (i in seq.int(1L, length(x), by = by)) {
          window <- x[max(1L, i - left):min(length(x), i + n - left - 1L)]
          expected[i] <- prod(window, na.rm = na.rm)
        }
        actual <- roll_prod(x, n, by = by, partial = TRUE,
                            align = align, na.rm = na.rm)
        expect_equal(actual, expected)
        expect_identical(is.nan(actual), is.nan(expected))
      }
    }
  }
})
