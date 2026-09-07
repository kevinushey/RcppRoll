context("correctness regressions")

test_that("n and by must be positive integer scalars", {

  invalid <- list(
    numeric(), c(1, 2), TRUE, 1.5, 0, -1, NA_real_, NaN, Inf,
    .Machine$integer.max + 1, "2", structure(2, class = "not-a-number")
  )

  for (value in invalid) {
    expect_error(
      do.call(roll_sum, list(x = 1:5, n = value)),
      "'n' should be a positive integer scalar"
    )
    expect_error(
      do.call(roll_sum, list(x = 1:5, n = 2, by = value)),
      "'by' should be a positive integer scalar"
    )
  }

})

test_that("logical control arguments must be non-missing scalars", {

  for (name in c("partial", "normalize", "na.rm")) {
    for (value in list(logical(), c(TRUE, FALSE), NA, 1L)) {
      args <- list(x = 1:5, n = 2)
      args[[name]] <- value
      expect_error(
        do.call(roll_sum, args),
        sprintf("'%s' should be TRUE or FALSE", name),
        fixed = TRUE
      )
    }
  }

})

test_that("explicitly empty weights are rejected", {

  expect_error(
    roll_sum(1:5, weights = numeric()),
    "'weights' should be non-empty",
    fixed = TRUE
  )
  expect_error(
    roll_sum(1:5, n = 3, weights = numeric()),
    "'weights' should be non-empty",
    fixed = TRUE
  )

})

test_that("the largest valid stride has safe filled and partial geometry", {

  stride <- .Machine$integer.max
  x <- as.numeric(1:5)

  expect_equal(
    roll_medianr(x, 3, by = stride, fill = 0),
    c(0, 0, 2, 0, 0)
  )
  expect_equal(
    roll_medianr(x, 3, by = stride, partial = TRUE),
    c(1, rep(NA_real_, 4))
  )

})

test_that("rolling products use the window's forward multiplication order", {

  n <- 96L
  x <- c(1, 0, 1e308, 2, rep(1, n - 4L), 1)
  starts <- seq_len(length(x) - n + 1L)
  reference <- vapply(
    starts,
    function(i) prod(x[i:(i + n - 1L)]),
    numeric(1)
  )

  expect_identical(reference, c(0, 0))
  expect_equal(roll_prod(x, n), reference)

  y <- c(1, 0, 1e308, 2)
  partial_reference <- vapply(
    seq_along(y),
    function(i) prod(y[i:length(y)]),
    numeric(1)
  )
  expect_equal(
    roll_prodl(y, 100, partial = TRUE),
    partial_reference
  )

})

test_that("representable location and spread statistics stay finite", {

  expect_equal(roll_mean(c(1e308, 1e308), 2), 1e308)
  expect_equal(roll_median(c(9e307, 1e308), 2), 9.5e307)

  # Exercise the incremental paths as well as the direct short-window kernels.
  large_mean <- rep(1e308, 140)
  expect_equal(roll_mean(large_mean, 128), rep(1e308, 13))
  expect_identical(
    roll_mean(rep(.Machine$double.xmax, 20), weights = 1:20),
    .Machine$double.xmax
  )

  constant <- rep(1e306, 7)
  expect_identical(roll_var(constant, 7), 0)
  expect_identical(roll_sd(constant, 7), 0)

  large_constant <- rep(1e306, 48)
  expect_identical(roll_var(large_constant, 32), rep(0, 17))
  expect_identical(roll_sd(large_constant, 32), rep(0, 17))

})

test_that("product guards recover as exceptional factors enter and leave", {

  x <- rep(c(1.001, 0.999, -1.001, -0.999), 200)
  x[200:205] <- c(0, 1e308, 2, NA, NaN, Inf)
  x[400:405] <- c(2^600, 2^600, 2^-600, 2^-600, -0.0, -Inf)
  forward <- function(window, remove) {
    if (remove) window <- window[!is.na(window)]
    if (any(is.na(window) & !is.nan(window))) return(NA_real_)
    Reduce(`*`, window, init = 1)
  }
  for (remove in c(FALSE, TRUE)) {
    for (by in c(1L, 3L)) {
      starts <- seq(1L, length(x) - 128L + 1L, by = by)
      expected <- vapply(starts, function(i)
        forward(x[i:(i + 127L)], remove), numeric(1))
      actual <- roll_prod(x, 128, by = by, na.rm = remove)
      expect_equal(actual, expected)
      expect_identical(is.nan(actual), is.nan(expected))
      zeros <- which(!is.na(expected) & expected == 0)
      expect_identical(1 / actual[zeros], 1 / expected[zeros])
    }
    expected <- vapply(seq_along(x), function(i)
      forward(x[max(1L, i - 127L):i], remove), numeric(1))
    expect_equal(roll_prodr(x, 128, partial = TRUE, na.rm = remove), expected)
  }

})

test_that("variance rescales overflowing lanes without disturbing neighbours", {

  x <- c(rep(1e306, 12), 1:20)
  for (remove in c(FALSE, TRUE)) {
    expect_equal(roll_var(x, 10, na.rm = remove)[1:3], rep(0, 3))
    expect_equal(tail(roll_var(x, 10, na.rm = remove), 11),
                 rep(var(1:10), 11))
    expect_equal(tail(roll_sd(x, 10, na.rm = remove), 11),
                 rep(sd(1:10), 11))
  }

})

test_that("normalizing finite weights is invariant to a huge common scale", {

  x <- c(1, 2, 3)
  weights <- c(1, 0.9, 0.8)
  huge_weights <- weights * 1e308

  expect_equal(
    roll_sum(x, weights = huge_weights),
    52 / 9
  )

  functions <- c(
    "roll_sum", "roll_mean", "roll_min", "roll_max",
    "roll_prod", "roll_median", "roll_var", "roll_sd"
  )
  for (name in functions) {
    roll <- get(name, envir = asNamespace("RcppRoll"))
    expect_equal(
      roll(x, weights = huge_weights, normalize = TRUE),
      roll(x, weights = weights, normalize = TRUE),
      info = name
    )
  }

})

test_that("weighted extrema handle missing products consistently", {

  for (roll in list(roll_min, roll_max)) {
    for (missing in list(NA_real_, NaN, Inf, -Inf)) {
      expect_error(
        roll(1:2, weights = c(missing, 1), normalize = TRUE),
        "finite"
      )
    }

    value <- roll(1:2, weights = c(NA_real_, 1), normalize = FALSE)
    expect_true(is.na(value) && !is.nan(value))

    value <- roll(1:2, weights = c(NaN, 1), normalize = FALSE)
    expect_true(is.nan(value))

    value <- roll(1:3, weights = c(NaN, NA_real_, 1), normalize = FALSE)
    expect_true(is.na(value) && !is.nan(value))

    expect_equal(
      roll(1:2, weights = c(NA_real_, 1), normalize = FALSE, na.rm = TRUE),
      2
    )
    expect_equal(
      roll(1:2, weights = c(NaN, 1), normalize = FALSE, na.rm = TRUE),
      2
    )

    # The inputs are individually non-missing, but their weighted product is
    # NaN and follows the same rules as an explicit NaN.
    value <- roll(0:1, weights = c(Inf, 1), normalize = FALSE)
    expect_true(is.nan(value))
    expect_equal(
      roll(0:1, weights = c(Inf, 1), normalize = FALSE, na.rm = TRUE),
      1
    )
  }

})

test_that("unweighted extrema preserve NA and NaN identity", {

  for (roll in list(roll_min, roll_max)) {
    for (n in c(2L, 112L)) {
      value <- roll(c(NaN, rep(1, 150)), n)[1L]
      expect_true(is.nan(value))

      value <- roll(c(NA_real_, NaN, rep(1, 149)), n)[1L]
      expect_true(is.na(value) && !is.nan(value))

      value <- roll(c(NaN, NA_real_, rep(1, 149)), n)[1L]
      expect_true(is.na(value) && !is.nan(value))
    }
  }

})

test_that("na.rm does not change complete unnormalized weighted means", {

  x <- 1:3
  weights <- 1:3
  expected <- mean(x * weights)

  expect_equal(
    roll_mean(x, weights = weights, normalize = FALSE, na.rm = FALSE),
    expected
  )
  expect_equal(
    roll_mean(x, weights = weights, normalize = FALSE, na.rm = TRUE),
    expected
  )

  expect_equal(
    roll_mean(c(1, NA, 3), weights = weights,
              normalize = FALSE, na.rm = TRUE),
    mean(c(1, 9))
  )

})

test_that("fill applies to short inputs and recycles to three regions", {

  expect_equal(roll_sum(1:2, 3, fill = 0), c(0, 0))

  expect_equal(
    roll_sum(1:6, 3, by = 2, align = "center", fill = c(10, 20)),
    c(10, 6, 20, 12, 10, 10)
  )

})

test_that("na_locf preserves factors and their storage", {

  x <- factor(c(NA, "a", NA, "b", NA), levels = c("a", "b"))
  expected <- factor(c(NA, "a", "a", "b", "b"), levels = levels(x))

  expect_identical(RcppRoll:::na_locf(x), expected)

})

test_that("variance and standard deviation reject negative weights", {

  for (roll in list(roll_var, roll_sd)) {
    expect_error(
      roll(1:3, weights = c(1, -0.25, 1), normalize = TRUE),
      "non-negative"
    )
    expect_error(
      roll(1:3, weights = c(1, -1, 2), normalize = FALSE),
      "non-negative"
    )
    for (missing in list(NA_real_, NaN, Inf, -Inf)) {
      expect_error(
        roll(1:3, weights = c(1, missing, 2), normalize = FALSE),
        "finite"
      )
    }
  }

})

test_that("finite frequency weights do not overflow variance intermediates", {

  weights <- c(1e308, 1e308)
  expect_equal(
    roll_var(c(1, 2), weights = weights, normalize = FALSE),
    0.25
  )
  expect_equal(
    roll_sd(c(1, 2), weights = weights, normalize = FALSE),
    0.5
  )

})

test_that("small frequency weights retain representable variance contributions", {

  # The weight ratio underflows (or is subnormal), but the weighted squared
  # deviation divided by the total weight is still approximately one.
  for (large in c(1e200, 2^600, 1e160, 2^520)) {
    for (order in list(1:2, 2:1)) {
      x <- c(0, large)[order]
      weights <- c(large, 1 / large)[order]
      for (roll in list(roll_var, roll_sd)) {
        for (remove in c(FALSE, TRUE)) {
          expect_equal(
            roll(x, weights = weights, normalize = FALSE, na.rm = remove),
            1
          )
        }
        expect_equal(
          roll(c(NA, x), weights = c(1, weights),
               normalize = FALSE, na.rm = TRUE),
          1
        )
      }
    }
  }

})

test_that("unnormalized means scale weighted products before summing", {

  for (remove in c(FALSE, TRUE)) {
    for (value in c(1, 1.5, 2, -1, -2)) {
      expect_equal(
        roll_mean(rep(value, 2), weights = rep(1e308, 2),
                  normalize = FALSE, na.rm = remove),
        value * 1e308
      )
    }
    # Overflowing positive and negative products can cancel to a finite mean.
    expect_equal(
      roll_mean(c(2, 2, -2), weights = rep(1e308, 3),
                normalize = FALSE, na.rm = remove),
      (2 / 3) * 1e308
    )
    expect_equal(
      roll_mean(rep(2, 3), weights = c(1e308, 1e308, -1e308),
                normalize = FALSE, na.rm = remove),
      (2 / 3) * 1e308
    )
  }
  expect_equal(
    roll_mean(c(NA, 1, 1), weights = rep(1e308, 3),
              normalize = FALSE, na.rm = TRUE),
    1e308
  )

})

test_that("the OpenMP thread option is a positive integer scalar", {

  old <- options(RcppRoll.threads = NULL)
  on.exit(options(old), add = TRUE)

  if (is.na(roll_threads()))
    skip("RcppRoll was compiled without OpenMP")

  invalid <- list(0, -1, 1.5, NA_real_, Inf, numeric(), c(1, 2), TRUE, "2")
  for (value in invalid) {
    options(RcppRoll.threads = value)
    expect_error(roll_threads(), "positive integer scalar")
  }

})
