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
