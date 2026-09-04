context("conditions")

# The wrappers share one argument-checking helper, so its errors and warnings
# have to be reported against the roll_* call the user actually made rather
# than against the helper.

test_that("errors name the roll_* function they came from", {

  condition <- tryCatch(roll_sum(1:9, 3, partial = 2), error = function(e) e)
  expect_s3_class(condition, "error")
  expect_identical(as.character(conditionCall(condition)[[1L]]), "roll_sum")

  condition <- tryCatch(
    roll_medianr(1:9, 3, weights = c(1, 1, 1), partial = TRUE),
    error = function(e) e)
  expect_identical(as.character(conditionCall(condition)[[1L]]), "roll_medianr")

})

test_that("warnings name the roll_* function they came from", {

  condition <- tryCatch(roll_sum(1:9, n = 2, weights = 1), warning = function(w) w)
  expect_s3_class(condition, "warning")
  expect_identical(as.character(conditionCall(condition)[[1L]]), "roll_sum")

  condition <- tryCatch(
    roll_varl(1:9, 3, partial = TRUE, fill = 0), warning = function(w) w)
  expect_identical(as.character(conditionCall(condition)[[1L]]), "roll_varl")

})

test_that("the checking helper is not exported", {

  expect_false("checkRollArgs" %in% getNamespaceExports("RcppRoll"))

})
