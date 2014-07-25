context("zoo")

test_that("we behave similarly to zoo::rollapply", {

  library(testthat)

  functions <- c("mean", "median", "prod", "min", "max", "sum")
  x <- rnorm(50)
  window <- 5L

  run_tests <- function(data, width, ..., functions, gctorture = FALSE) {
    for (f in functions) {
      RcppRoll <- get(paste("roll", f, sep = "_"), envir = asNamespace("RcppRoll"))
      zoo <- zoo::rollapply(data, width, FUN = get(f), ...)
      if (is.matrix(zoo)) {
        dimnames(zoo) <- NULL
      }
      if (gctorture) gctorture(TRUE)
      RcppRollRes <- RcppRoll(data, width, ...)
      if (gctorture) gctorture(FALSE)
      expect_equal(RcppRollRes, zoo)
    }
  }

  run_tests(x, window, functions = functions)

  window <- 50L
  run_tests(x, window, functions = functions)

  window <- 1L
  run_tests(x, window, functions = functions)

  ## test against small numbers
  x <- rnorm(1E3) ^ 100
  run_tests(x, 5L, functions = functions)

  ## and large numbers
  x <- rnorm(1E3, mean = 1E200, sd = 1E201)
  run_tests(x, 5L, functions = functions)

  ## now let's really stress it...
  args <- expand.grid(
    width = list(3L, 10L, 100L),
    fill = list(NA, c(-1, 0, 1)),
    align = list("left", "center", "right"),
    by = c(1L, 2L, 5L),
    na.rm = c(TRUE, FALSE)
  )

  # don't use median here
  f <- setdiff(functions, 'median')
  data <- rnorm(1E2, 100, 50)
  for (i in 1:nrow(args)) {
    run_tests(data,
              args$width[[i]],
              fill = args$fill[[i]],
              align = args$align[[i]],
              na.rm = args$na.rm[[i]],
              functions = f)
  }

  data[sample(length(data), length(data) / 3)] <- NA
  for (i in 1:nrow(args)) {
    suppressWarnings(run_tests(data,
                               args$width[[i]],
                               fill = args$fill[[i]],
                               align = args$align[[i]],
                               na.rm = args$na.rm[[i]],
                               functions = f))
  }

  data <- matrix(rnorm(2E2, 100, 50), nrow = 100)
  for (i in 1:nrow(args)) {
    run_tests(data, args$width[[i]], fill = args$fill[[i]], align = args$align[[i]],
              functions = functions)
  }

})
