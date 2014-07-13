context("zoo")

test_that("we behave similarly to zoo::rollapply", {

  x <- rnorm(50)
  window <- 5L

  run_tests <- function(data, width, ...) {
    functions <- c("mean", "median", "min", "max", "sum")
    for (f in functions) {
      RcppRoll <- get(paste("roll", f, sep = "_"), envir = asNamespace("RcppRoll"))
      expect_equal(RcppRoll(data, width, ...), zoo::rollapply(data, width, FUN = get(f), ...))
    }
  }

  run_tests(x, window)

  window <- 50L
  run_tests(x, window)

  window <- 1L
  run_tests(x, window)

  ## test against small numbers
  x <- rnorm(1E3) ^ 100
  run_tests(x, 5L)

  ## and large numbers
  x <- rnorm(1E3, mean = 1E200, sd = 1E201)
  run_tests(x, 5L)

})
