library("microbenchmark")
library("RcppRoll")
library("Rcpp")
library("zoo")

x <- rnorm(1E5)
y <- matrix(rnorm(1E6), ncol = 1E3)

rolling_mean <- rollit(final_trans = "x/N")
rolling_mean2 <- rollit(final_trans = "x/N", inline = FALSE)

close_enough <- function(x, y) {
  all.equal(x, y)
}

microbenchmark(
  roll_mean(x, 11),
  rolling_mean(x, 11),
  rolling_mean2(x, 11),
  roll_median(x, 11),
  zoo::rollmean(x, 11),
  zoo::rollmedian(x, 11),
  zoo::rollapply(x, 11, mean),
  times = 5
)

close_enough(c(roll_mean(y, 10)), c(zoo::rollmean(y, 10)))

microbenchmark(
  roll_mean(y, 10),
  roll_median(y, 10),
  roll_prod(y, 10),
  roll_sd(y, 10),
  roll_sum(y, 10),
  roll_var(y, 10),
  times = 10
)

microbenchmark(roll_mean(y, nrow(y)),
               colMeans(y),
               roll_mean(y, ncol(y), FALSE),
               rowMeans(y))

## by groups

dat <- data.frame(x = rnorm(1E3),
                  y = rep(1:100, each = 1E1))

microbenchmark(with(dat, tapply(x, y, roll_mean, 10)),
               with(dat, tapply(x, y, zoo::rollmean, 10)),
               times = 25)

f <- function(x) {
  return(sqrt(x))
}

x <- rnorm(1E5)
microbenchmark(roll_var(x, 100),
               rollapply(x, 100, var),
               times = 1)
