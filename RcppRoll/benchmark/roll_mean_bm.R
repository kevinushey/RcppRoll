library("microbenchmark")
library("RcppRoll")
library("zoo")

x <- rnorm(1E5)
y <- matrix( rnorm(1E5), ncol=1E3 )

close_enough <- function(x, y) {
  all.equal(x, y, tolerance = 2*.Machine$double.eps)
}

microbenchmark(
  roll_mean(x, 10),
  roll_median(x, 10),
  zoo::rollmean(x, 10),
  zoo::rollapply(x, 10, mean),
  roll_apply(x, 10, mean),
  times=1
  )

close_enough( c(roll_mean(y, 10)), c(zoo::rollmean(y, 10)) )

microbenchmark(
  roll_mean(y, 10),
  roll_median(y, 10),
  zoo::rollmean(y, 10),
  zoo::rollapply(y, 10, mean),
  times=1
)

close_enough( c(roll_mean(y, 10)), c(zoo::rollmean(y, 10)) )

microbenchmark(
  roll_mean(y, 10),
  roll_median(y, 10),
  roll_prod(y, 10),
  roll_sd(y, 10),
  roll_stddev(y, 10),
  roll_sum(y, 10),
  roll_var(y, 10),
  times=10
  )

microbenchmark(
  roll_mean(y, nrow(y)),
  colMeans(y)
  )

## by groups

dat <- data.frame(
  x = rnorm(1E3),
  y = rep(1:100, each=1E1)
  )

microbenchmark(
  with( dat, tapply( x, y, roll_mean, 10 ) ),
  with( dat, tapply( x, y, zoo::rollmean, 10 ) ),
  times=5
  )

f <- function(x) {
  return( sqrt(x) )
}

microbenchmark(
  roll_apply(x, 10, f),
  rollapply(x, 10, f),
  times=1
  )