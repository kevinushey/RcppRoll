context("zoo")

test_that("we behave similarly to zoo::rollapply", {

  x <- rnorm(50)
  window <- 5L

  run_tests <- function(data, width, ...) {
    functions <- c("mean", "median", "prod", "min", "max", "sum")
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

  ## now let's really stress it...
  args <- expand.grid(
    width = list(3L, 10L, 100L),
    fill = list(NA, c(-1, 0, 1)),
    align = list("left", "center", "right")
  )

  data <- rnorm(1E2, 100, 50)
  for (i in 1:nrow(args)) {
    run_tests(data, args$width[[i]], fill = args$fill[[i]], align = args$align[[i]])
  }

})
