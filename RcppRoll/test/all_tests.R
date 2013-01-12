library("testthat")
library("RcppRoll")

close_enough <- function(x, y) {
  all.equal(x, y)
}

x <- matrix( 1:16, nrow=4 )

close_enough( apply(x, 2, mean), as.numeric(roll_mean(x, 4)) )

close_enough( apply(x, 2, median), as.numeric(roll_median(x, 4)) )

rolling_mean <- rollit("x", const_vars=list(n="x.size()"), final_trans="x/n")

close_enough( apply(x, 2, mean), as.numeric( rolling_mean(x, 4) ) )
close_enough( apply(x, 1, mean), as.numeric( rolling_mean(x, 4, FALSE) ) )

y <- rnorm(1E6)
close_enough( median(y), roll_median(y, length(y)) )
close_enough( sd(y), roll_sd(y, length(y)) )
close_enough( var(y), roll_var(y, length(y)) )
close_enough( prod(y), roll_prod(y, length(y)) )