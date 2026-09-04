context("partial")

test_that("partial windows compute the edges instead of filling them (#18)", {

  # the sequence and expected output from the issue -- note the reporter says
  # "right align", but their numbers are left-aligned in this package's terms
  x <- c(0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0)
  expect_equal(roll_suml(x, 3, partial = TRUE),
               c(1, 2, 2, 3, 3, 2, 2, 1, 2, 1, 1, 0))

  # unchanged without partial
  expect_equal(roll_suml(x, 3), c(1, 2, 2, 3, 3, 2, 2, 1, 2, 1, NA, NA))

})

test_that("partial windows match zoo::rollapply(partial = TRUE)", {

  if (!requireNamespace("zoo", quietly = TRUE))
    skip("zoo not installed")

  set.seed(31)
  x <- round(rnorm(14), 3)
  functions <- c("mean", "median", "min", "max", "prod", "sum", "sd", "var")

  for (f in functions) {
    RcppRoll <- get(paste("roll", f, sep = "_"), envir = asNamespace("RcppRoll"))
    for (align in c("left", "center", "right")) {
      for (n in 1:7) {
        expect_equal(
          as.numeric(RcppRoll(x, n, align = align, partial = TRUE)),
          as.numeric(zoo::rollapply(x, n, get(f), align = align, partial = TRUE))
        )
      }
    }
  }

  # a window wider than the data is still computable at every point
  y <- 1:5
  expect_equal(roll_sumr(y, 12, partial = TRUE),
               as.numeric(zoo::rollapply(y, 12, sum, align = "right", partial = TRUE)))
  expect_equal(roll_sumr(y, 12, partial = TRUE), cumsum(y))

})

test_that("an outsized 'n' computes without reserving what it cannot use", {

  # windows are clipped to the data, so a huge nominal 'n' must not size any
  # buffer -- the median accumulator once reserved 'n' doubles for this
  x <- as.numeric(1:10)
  expect_equal(roll_median(x, n = 2e8, partial = TRUE), rep(5.5, 10))
  expect_equal(roll_sum(x, n = 2e8, partial = TRUE), rep(55, 10))

})

test_that("partial output has one element per input element", {

  x <- rnorm(14)
  for (align in c("left", "center", "right"))
    for (n in c(1L, 2L, 5L, 20L))
      expect_equal(length(roll_mean(x, n, align = align, partial = TRUE)), length(x))

})

test_that("var and sd give NA on a one-element partial window", {

  # rather than the NaN that a zero-length denominator used to produce
  expect_true(is.na(roll_sdr(1:6, 3, partial = TRUE)[1]))
  expect_false(is.nan(roll_sdr(1:6, 3, partial = TRUE)[1]))
  expect_true(is.na(roll_varr(1:6, 3, partial = TRUE)[1]))

})

test_that("na.rm still applies inside a truncated window", {

  x <- c(1, NA, 3, 4, 5)
  expect_equal(roll_sumr(x, 3, partial = TRUE, na.rm = FALSE),
               c(1, NA, NA, NA, 12))
  expect_equal(roll_sumr(x, 3, partial = TRUE, na.rm = TRUE),
               c(1, 1, 4, 7, 12))

})

test_that("partial keeps every row for matrices", {

  m <- matrix(1:12, nrow = 6, dimnames = list(NULL, c("A", "B")))
  result <- roll_sum(m, 3, partial = TRUE)

  expect_equal(dim(result), dim(m))
  expect_equal(colnames(result), colnames(m))
  expect_equal(as.numeric(result[, 1]), roll_sum(m[, 1], 3, partial = TRUE))
  expect_equal(as.numeric(result[, 2]), roll_sum(m[, 2], 3, partial = TRUE))

})

test_that("partial computes at strided points when 'by' is given", {

  # points that are skipped are not computed, and 'fill' does not apply
  expect_equal(roll_sumr(1:8, 3, by = 2, partial = TRUE),
               c(1, NA, 6, NA, 12, NA, 18, NA))

})

test_that("partial rejects what it does not support (#18)", {

  expect_error(roll_sum(1:9, 3, weights = c(1, 1, 1), partial = TRUE),
               "not supported together with 'weights'")

  # only TRUE/FALSE -- zoo's numeric 'minimum observations' form is not supported
  expect_error(roll_sum(1:9, 3, partial = 2), "should be TRUE or FALSE")
  expect_error(roll_sum(1:9, 3, partial = NA), "should be TRUE or FALSE")
  expect_error(roll_sum(1:9, 3, partial = c(TRUE, TRUE)), "should be TRUE or FALSE")

  expect_warning(roll_sum(1:9, 3, partial = TRUE, fill = 0),
                 "'fill' is ignored")

  # the r/l wrappers default 'fill' themselves, so they must stay quiet
  expect_silent(roll_sumr(1:9, 3, partial = TRUE))
  expect_silent(roll_suml(1:9, 3, partial = TRUE))

})

test_that("partial = FALSE leaves existing behaviour alone", {

  x <- rnorm(12)
  for (align in c("left", "center", "right")) {
    expect_equal(roll_mean(x, 4, align = align, partial = FALSE),
                 roll_mean(x, 4, align = align))
  }
  expect_equal(roll_sumr(1:9, 3, partial = FALSE), roll_sumr(1:9, 3))
  expect_silent(roll_sum(1:9, 3, partial = FALSE))

})
