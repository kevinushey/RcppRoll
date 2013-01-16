library("testthat")
library("RcppRoll")

close_enough <- function(x, y) {
  stopifnot( all.equal(x, y) )
}

x <- matrix( 1:16, nrow=4 )

close_enough( apply(x, 2, mean), as.numeric(roll_mean(x, 4)) )

close_enough( apply(x, 2, median), as.numeric(roll_median(x, 4)) )

rolling_mean <- rollit(final_trans="x/N")

close_enough( apply(x, 2, mean), as.numeric( rolling_mean(x, 4) ) )
close_enough( apply(x, 1, mean), as.numeric( rolling_mean(x, 4, FALSE) ) )

y <- rnorm(1E6)
close_enough( median(y), roll_median(y, length(y)) )
close_enough( sd(y), roll_sd(y, length(y)) )
close_enough( var(y), roll_var(y, length(y)) )
close_enough( prod(y), roll_prod(y, length(y)) )

x <- 1:3
close_enough( rolling_mean(x, 3, weights=c(1,0,1)), (1+3)/2 )
rolling_mean(x, 3, weights=c(1,0,1))
rolling_mean(x, 2, weights=c(0.5,2), normalize=FALSE) == c( (0.5*1 + 2*2)/2, (0.5*2 + 2*3)/2)

rolling_mean(x, 3, by=2)

x <- 1:5
rolling_prod <- rollit(combine="*")
close_enough( rolling_prod(x, 5), prod(x) )
close_enough( rolling_prod(x, 5, weights=c(1,0,1,0,1), normalize=FALSE), prod(1, 3, 5) )

x <- matrix( rnorm(1E5), ncol=1E3 )

library(zoo)
library(microbenchmark)

microbenchmark(
  roll_mean(x, 10),
  rolling_mean(x, 10),
  zoo::rollmean(x, 10),
  times=10
  )

all.equal( as.vector(roll_mean(x, 100)), as.vector(zoo::rollmean(x, 100)) )