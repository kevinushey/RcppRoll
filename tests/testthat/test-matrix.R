context("matrix")

test_that("'by' shortens a matrix result the way it shortens a vector one", {

  # the output used to be sized as though 'by' were 1, so a strided matrix
  # result carried rows that were never computed
  m <- matrix(as.numeric(1:24), nrow = 12)

  for (by in c(1L, 2L, 5L)) {
    result <- roll_sum(m, 3, by = by)
    expect_equal(nrow(result), length(roll_sum(m[, 1], 3, by = by)))
    expect_equal(as.numeric(result[, 1]), roll_sum(m[, 1], 3, by = by))
    expect_equal(as.numeric(result[, 2]), roll_sum(m[, 2], 3, by = by))
  }

  # 'fill' and 'partial' still report one row per input row
  expect_equal(nrow(roll_sum(m, 3, by = 2, fill = NA)), nrow(m))
  expect_equal(nrow(roll_sum(m, 3, by = 2, partial = TRUE)), nrow(m))

})

test_that("'weights' sets the window size for matrices too", {

  # 'n' is left at its default here, so the result used to be sized for n = 1
  # and its last row read off the end of each column
  m <- matrix(as.numeric(1:12), nrow = 6)

  result <- roll_mean(m, weights = c(1, 1))
  expect_equal(dim(result), c(5L, 2L))
  expect_equal(as.numeric(result[, 1]), roll_mean(m[, 1], weights = c(1, 1)))
  expect_equal(as.numeric(result[, 2]), roll_mean(m[, 2], weights = c(1, 1)))

  for (f in c("mean", "median", "min", "max", "prod", "sum", "sd", "var")) {
    roll <- get(paste("roll", f, sep = "_"), envir = asNamespace("RcppRoll"))
    result <- roll(m, weights = c(1, 3, 1))
    expect_equal(dim(result), c(4L, 2L))
    expect_equal(as.numeric(result[, 1]), roll(m[, 1], weights = c(1, 3, 1)))
  }

})

test_that("matrix columns are rolled independently of one another", {

  set.seed(41)
  m <- matrix(rnorm(60), nrow = 20, dimnames = list(NULL, c("a", "b", "c")))
  m[c(3, 25, 48)] <- NA

  for (f in c("mean", "median", "min", "max", "sum", "sd", "var")) {
    roll <- get(paste("roll", f, sep = "_"), envir = asNamespace("RcppRoll"))
    for (na.rm in c(TRUE, FALSE)) {
      result <- roll(m, 4, na.rm = na.rm)
      expect_equal(colnames(result), colnames(m))
      for (j in 1:3)
        expect_equal(as.numeric(result[, j]), roll(m[, j], 4, na.rm = na.rm))
    }
  }

})
