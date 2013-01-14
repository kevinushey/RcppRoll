library("testthat")
library("RcppRoll")

close_enough <- function(x, y) {
  stopifnot( all.equal(x, y) )
}

x <- matrix( 1:16, nrow=4 )

close_enough( apply(x, 2, mean), as.numeric(roll_mean(x, 4)) )

close_enough( apply(x, 2, median), as.numeric(roll_median(x, 4)) )

rolling_mean <- rollit("x", const_vars=list(n="LENGTH(x)"), final_trans="x/n")

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

fun <- "
double out = 0;
for( int i=1; i < x.size(); i++ ) {
  out += (x[i] - x[i-1]) / x[i-1];
}
return out;
"

rolling_fun <- rollit_raw( fun )
rolling_fun( 1:5, 5)
rolling_fun( 1:5, 3, weights=c(1,0,1), normalize=FALSE )

rolling_mean <- rollit("mean(x)", vector=TRUE)
